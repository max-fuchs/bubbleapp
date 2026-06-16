library(shiny)
library(shinyWidgets)
library(dplyr)
library(bslib)
library(ggplot2)
library(stringr)
library(readxl)
library(scales)
library(grid)
library(igraph)
library(ggraph)
library(tidyr)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "superhero"),
  titlePanel("g:Profiler Results Plotter - by MF"),
  tabsetPanel(
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          12,
          h4("Instructions"),
          p(
            "Go to ",
            tags$a(
              href = "https://biit.cs.ut.ee/gprofiler/gost",
              target = "_blank",
              "https://biit.cs.ut.ee/gprofiler/gost"
            ),
            " and perform functional analysis on your significant DEGs. Then download results as a .csv or .xlsx file and input at Plot Settings Tab."
          )
        )
      )
    ),
    tabPanel(
      "Plot Settings",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload CSV or XLSX File", accept = c(".csv", ".xlsx")),
          uiOutput("sheet_selector"),
          uiOutput("term_selector"),
          numericInput("wrap_width", "Wrap Width:", value = 30, min = 1, max = 100),
          numericInput("y_axis_text_size", "Term name text size", value = 12),
          selectInput(
            "y_axis_text_face",
            "Y-Axis Text Face",
            choices = c("plain", "italic", "bold", "bold.italic")
          ),
          numericInput("top_n", "Top N Terms to Display:", value = 15, min = 1),
          selectInput(
            "sort_direction",
            "Sort by Gene Ratio:",
            choices = c("Descending" = "desc", "Ascending" = "asc")
          ),
          selectInput("plot_type", "Plot Type:", choices = c("Bubble Plot", "Bar Plot")),
          conditionalPanel(
            condition = "input.plot_type == 'Bar Plot'",
            selectInput(
              "bar_plot_metric",
              "Metric for Bar Plot:",
              choices = c("Intersection Size", "Gene Ratio")
            )
          ),
          checkboxInput("facet_by_source", "Compartmentalize plot by source", value = FALSE),
          selectInput(
            "source_strip_position",
            "Source strip position",
            choices = c("Right" = "right", "Left" = "left"),
            selected = "right"
          ),
          numericInput("facet_text_size", "Facet label size", value = 12, min = 6, max = 30),
          selectInput(
            "gg_theme",
            "Select ggplot2 Theme",
            choices = c(
              "Default" = "default",
              "Minimal" = "theme_minimal",
              "Classic" = "theme_classic",
              "Light" = "theme_light",
              "Dark" = "theme_dark",
              "BW" = "theme_bw",
              "Void" = "theme_void"
            )
          ),
          numericInput("legend_title_size", "Legend title size", value = 12, min = 6, max = 30),
          numericInput("legend_text_size", "Legend text size", value = 10, min = 6, max = 30),
          numericInput("legend_key_size", "Legend key size (mm)", value = 6, min = 2, max = 20),
          radioButtons("paper_size", "Paper Size (PDF):", choices = c("A4", "A5")),
          radioButtons("orientation", "Orientation (PDF):", choices = c("Portrait", "Landscape")),
          colorPickr("low_color", "Select Low Color:", selected = "#1E90FF"),
          colorPickr("high_color", "Select High Color:", selected = "#8B0000"),
          downloadButton("downloadPlot", "Download Plot as PDF"),
          numericInput("svg_width", "SVG Width (cm):", value = 14, min = 5),
          numericInput("svg_height", "SVG Height (cm):", value = 10, min = 5),
          downloadButton("downloadSVG", "Download Plot as SVG")
        ),
        mainPanel(
          plotOutput("genePlot", height = "900px")
        )
      )
    ),
    tabPanel(
      "Network Plot",
      sidebarLayout(
        sidebarPanel(
          h4("DESeq2 Results"),
          fileInput("deg_file", "Upload DESeq2 XLSX File", accept = c(".xlsx")),
          uiOutput("deg_sheet_selector"),
          uiOutput("deg_col_selectors"),
          hr(),
          h4("Term Selection"),
          uiOutput("network_source_filter"),
          uiOutput("network_term_selector"),
          hr(),
          h4("Appearance"),
          numericInput("net_label_size", "Label size", value = 3, min = 1, max = 10, step = 0.5),
          numericInput("net_pathway_node_size", "Pathway node size", value = 10, min = 3, max = 30),
          numericInput("net_gene_node_size", "Gene node size", value = 4, min = 1, max = 15),
          colorPickr("net_low_color", "Low fold change color:", selected = "#2166AC"),
          colorPickr("net_high_color", "High fold change color:", selected = "#B2182B"),
          colorPickr("net_mid_color", "Mid fold change color:", selected = "#F7F7F7"),
          numericInput("net_fc_limit", "Fold change color limit (±):", value = 3, min = 0.5, max = 10, step = 0.5),
          hr(),
          h4("Export"),
          radioButtons("net_paper_size", "Paper Size (PDF):", choices = c("A4", "A5")),
          radioButtons("net_orientation", "Orientation (PDF):", choices = c("Portrait", "Landscape")),
          downloadButton("downloadNetPDF", "Download as PDF"),
          numericInput("net_svg_width", "SVG Width (cm):", value = 20, min = 5),
          numericInput("net_svg_height", "SVG Height (cm):", value = 16, min = 5),
          downloadButton("downloadNetSVG", "Download as SVG")
        ),
        mainPanel(
          plotOutput("networkPlot", height = "900px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      req(input$selected_sheet)
      read_xlsx(input$file$datapath, sheet = input$selected_sheet)
    } else {
      validate("Unsupported file type")
    }
  })
  
  output$sheet_selector <- renderUI({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext != "xlsx") return(NULL)
    sheets <- excel_sheets(input$file$datapath)
    selectInput("selected_sheet", "Select Sheet:", choices = sheets)
  })
  
  output$term_selector <- renderUI({
    req(data())
    terms <- unique(str_to_sentence(data()$term_name))
    pickerInput(
      "selected_terms",
      "Select Terms to Display (optional):",
      choices = sort(terms),
      multiple = TRUE,
      selected = terms,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  genePlot <- reactive({
    req(data())
    
    res <- data()
    
    if ("adjusted_p_value" %in% names(res)) {
      res$p_value_col <- res$adjusted_p_value
      p_label <- "Adjusted p-value"
    } else if ("p_value" %in% names(res)) {
      res$p_value_col <- res$p_value
      p_label <- "p-value"
    } else {
      stop("The data must contain either 'adjusted_p_value' or 'p_value' column.")
    }
    
    if (input$facet_by_source && !"source" %in% names(res)) {
      stop("Column 'source' not found in uploaded file.")
    }
    
    res$term_name <- str_to_sentence(res$term_name)
    res$GeneRatio <- res$intersection_size / res$term_size
    
    if (!is.null(input$selected_terms) && length(input$selected_terms) > 0) {
      res <- res %>% filter(term_name %in% input$selected_terms)
    }
    
    if (input$facet_by_source && "source" %in% names(res)) {
      res <- res %>%
        group_by(source) %>%
        {
          if (input$sort_direction == "asc") {
            arrange(., GeneRatio, .by_group = TRUE)
          } else {
            arrange(., desc(GeneRatio), .by_group = TRUE)
          }
        } %>%
        slice_head(n = input$top_n) %>%
        ungroup()
    } else {
      res <- if (input$sort_direction == "asc") {
        res %>% arrange(GeneRatio) %>% head(input$top_n)
      } else {
        res %>% arrange(desc(GeneRatio)) %>% head(input$top_n)
      }
    }
    
    res$term_name_wrapped <- str_wrap(res$term_name, width = input$wrap_width)
    
    if (input$facet_by_source && "source" %in% names(res)) {
      res <- res %>%
        group_by(source) %>%
        mutate(term_name_wrapped = factor(term_name_wrapped, levels = rev(unique(term_name_wrapped)))) %>%
        ungroup()
    } else {
      res$term_name_wrapped <- factor(res$term_name_wrapped, levels = rev(unique(res$term_name_wrapped)))
    }
    
    p_breaks <- pretty(res$p_value_col, n = 5)
    p_breaks <- p_breaks[
      p_breaks >= min(res$p_value_col, na.rm = TRUE) &
        p_breaks <= max(res$p_value_col, na.rm = TRUE)
    ]
    
    if (length(p_breaks) < 2) {
      p_breaks <- unique(signif(seq(min(res$p_value_col, na.rm = TRUE), max(res$p_value_col, na.rm = TRUE), length.out = 4), 2))
    }
    
    theme_choice <- switch(
      input$gg_theme,
      "theme_minimal" = theme_minimal(),
      "theme_classic" = theme_classic(),
      "theme_light" = theme_light(),
      "theme_dark" = theme_dark(),
      "theme_bw" = theme_bw(),
      "theme_void" = theme_void(),
      theme_gray()
    )
    
    if (input$plot_type == "Bubble Plot") {
      p <- ggplot(res, aes(x = GeneRatio, y = term_name_wrapped)) +
        geom_point(aes(color = p_value_col, size = intersection_size)) +
        ylab("Term Name") +
        xlab("Gene Ratio") +
        scale_color_gradient(
          name = p_label,
          low = input$low_color,
          high = input$high_color,
          breaks = p_breaks,
          labels = label_scientific(digits = 2)
        ) +
        scale_size_continuous(
          name = "Intersection size",
          range = c(3, 10)
        ) +
        guides(
          size = guide_legend(
            title.position = "top",
            override.aes = list(alpha = 1)
          ),
          color = guide_colorbar(
            title.position = "top",
            barheight = unit(40, "mm"),
            barwidth = unit(input$legend_key_size, "mm")
          )
        )
    } else {
      metric <- if (input$bar_plot_metric == "Intersection Size") "intersection_size" else "GeneRatio"
      
      p <- ggplot(res, aes(x = .data[[metric]], y = term_name_wrapped, fill = p_value_col)) +
        geom_col() +
        ylab("Term Name") +
        xlab(if (metric == "intersection_size") "Intersection Size" else "Gene Ratio") +
        scale_fill_gradient(
          name = p_label,
          low = input$low_color,
          high = input$high_color,
          breaks = p_breaks,
          labels = label_scientific(digits = 2)
        ) +
        guides(
          fill = guide_colorbar(
            title.position = "top",
            barheight = unit(40, "mm"),
            barwidth = unit(input$legend_key_size, "mm")
          )
        )
    }
    
    p <- p +
      theme_choice +
      theme(
        axis.text.y = element_text(
          size = input$y_axis_text_size,
          face = input$y_axis_text_face,
          color = "black"
        ),
        axis.title.y = element_text(size = 16, face = "bold", color = "grey20"),
        axis.title.x = element_text(size = 16, face = "bold", color = "grey20"),
        strip.text = element_text(size = input$facet_text_size, face = "bold"),
        strip.text.y = element_text(size = input$facet_text_size, face = "bold"),
        strip.text.y.left = element_text(size = input$facet_text_size, face = "bold"),
        strip.background = element_rect(fill = "grey90", color = "black"),
        legend.title = element_text(size = input$legend_title_size, face = "bold"),
        legend.text = element_text(size = input$legend_text_size),
        legend.key.size = unit(input$legend_key_size, "mm")
      )
    
    if (input$facet_by_source && "source" %in% names(res)) {
      if (input$source_strip_position == "left") {
        p <- p +
          facet_grid(source ~ ., scales = "free_y", space = "free_y", switch = "y") +
          theme(strip.placement = "outside")
      } else {
        p <- p +
          facet_grid(source ~ ., scales = "free_y", space = "free_y")
      }
    }
    
    p
  })
  
  output$genePlot <- renderPlot({
    genePlot()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("GeneRatio_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf_width <- if (input$paper_size == "A4") 8.27 else 5.83
      pdf_height <- if (input$paper_size == "A4") 11.69 else 8.27
      if (input$orientation == "Landscape") {
        temp <- pdf_width
        pdf_width <- pdf_height
        pdf_height <- temp
      }
      pdf(file, width = pdf_width, height = pdf_height)
      print(genePlot())
      dev.off()
    }
  )
  
  output$downloadSVG <- downloadHandler(
    filename = function() {
      paste("GeneRatio_Plot", Sys.Date(), ".svg", sep = "")
    },
    content = function(file) {
      svg(file, width = input$svg_width / 2.54, height = input$svg_height / 2.54)
      print(genePlot())
      dev.off()
    }
  )
  
  deg_raw <- reactive({
    req(input$deg_file)
    ext <- tools::file_ext(input$deg_file$datapath)
    validate(need(ext == "xlsx", "Please upload an XLSX file."))
    req(input$deg_selected_sheet)
    read_xlsx(input$deg_file$datapath, sheet = input$deg_selected_sheet)
  })
  
  deg_data <- reactive({
    req(deg_raw())
    df <- deg_raw()
    gene_col <- if ("external_gene_name" %in% names(df)) {
      "external_gene_name"
    } else {
      req(input$deg_gene_col)
      input$deg_gene_col
    }
    fc_col <- if ("log2FoldChange" %in% names(df)) {
      "log2FoldChange"
    } else {
      req(input$deg_fc_col)
      input$deg_fc_col
    }
    df %>%
      rename(external_gene_name = all_of(gene_col), log2FoldChange = all_of(fc_col))
  })
  
  output$deg_sheet_selector <- renderUI({
    req(input$deg_file)
    sheets <- excel_sheets(input$deg_file$datapath)
    selectInput("deg_selected_sheet", "Select Sheet:", choices = sheets)
  })
  
  output$deg_col_selectors <- renderUI({
    req(deg_raw())
    df <- deg_raw()
    cols <- names(df)
    ui_elements <- list()
    if (!"external_gene_name" %in% cols) {
      ui_elements[[length(ui_elements) + 1]] <- selectInput(
        "deg_gene_col",
        "Select gene name column:",
        choices = cols,
        selected = cols[1]
      )
    }
    if (!"log2FoldChange" %in% cols) {
      ui_elements[[length(ui_elements) + 1]] <- selectInput(
        "deg_fc_col",
        "Select fold change column:",
        choices = cols,
        selected = cols[1]
      )
    }
    if (length(ui_elements) == 0) return(NULL)
    tagList(
      tags$hr(),
      tags$p(tags$b("Column mapping required:"), style = "color: #c0392b;"),
      ui_elements
    )
  })
  
  output$network_source_filter <- renderUI({
    req(data())
    sources <- sort(unique(data()$source))
    pickerInput(
      "net_source_filter",
      "Filter by source:",
      choices = sources,
      selected = sources,
      multiple = TRUE,
      options = list(`actions-box` = TRUE)
    )
  })
  
  output$network_term_selector <- renderUI({
    req(data(), input$net_source_filter)
    terms <- data() %>%
      filter(source %in% input$net_source_filter) %>%
      arrange(adjusted_p_value) %>%
      pull(term_name) %>%
      str_to_sentence() %>%
      unique()
    pickerInput(
      "net_selected_terms",
      "Select terms (max 10):",
      choices = terms,
      multiple = TRUE,
      selected = head(terms, 5),
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `max-options` = 10,
        `max-options-text` = "Max 10 terms"
      )
    )
  })
  
  build_network_plot <- reactive({
    req(data(), deg_data(), input$net_selected_terms)
    validate(need(length(input$net_selected_terms) >= 1, "Please select at least one term."))
    
    gprofiler <- data()
    degs <- deg_data()
    
    gprofiler$term_name <- str_to_sentence(gprofiler$term_name)
    
    selected <- gprofiler %>%
      filter(term_name %in% input$net_selected_terms)
    
    validate(need(nrow(selected) > 0, "No matching terms found."))
    validate(need("intersections" %in% names(selected), "Column 'intersections' not found. Please upload the g:Profiler file with intersections enabled."))
    
    edges <- selected %>%
      rowwise() %>%
      mutate(gene = list(trimws(strsplit(intersections, ",")[[1]]))) %>%
      ungroup() %>%
      tidyr::unnest(gene) %>%
      select(term_name, gene, intersection_size, adjusted_p_value)
    
    pathway_nodes <- selected %>%
      select(name = term_name, intersection_size, adjusted_p_value) %>%
      distinct() %>%
      mutate(node_type = "pathway", log2FoldChange = NA_real_)
    
    gene_nodes <- edges %>%
      select(name = gene) %>%
      distinct() %>%
      left_join(
        degs %>% select(name = external_gene_name, log2FoldChange),
        by = "name"
      ) %>%
      mutate(
        node_type = "gene",
        intersection_size = NA_real_,
        adjusted_p_value = NA_real_
      )
    
    nodes <- bind_rows(pathway_nodes, gene_nodes)
    
    edge_list <- edges %>%
      select(from = term_name, to = gene)
    
    g <- graph_from_data_frame(d = edge_list, vertices = nodes, directed = FALSE)
    
    fc_limit <- input$net_fc_limit
    
    p <- ggraph(g, layout = "fr") +
      geom_edge_link(color = "grey70", alpha = 0.6, linewidth = 0.4) +
      geom_node_point(
        data = function(x) filter(x, node_type == "gene"),
        aes(color = log2FoldChange),
        size = input$net_gene_node_size
      ) +
      geom_node_point(
        data = function(x) filter(x, node_type == "pathway"),
        aes(size = intersection_size),
        color = "#E8A838",
        shape = 21,
        fill = "#E8A838",
        stroke = 1.2
      ) +
      geom_node_text(
        aes(label = name),
        size = input$net_label_size,
        repel = TRUE,
        max.overlaps = Inf,
        fontface = ifelse(
          igraph::V(g)$node_type == "pathway", "bold", "plain"
        )
      ) +
      scale_color_gradient2(
        name = "log2 Fold Change",
        low = input$net_low_color,
        mid = input$net_mid_color,
        high = input$net_high_color,
        midpoint = 0,
        limits = c(-fc_limit, fc_limit),
        oob = squish,
        na.value = "grey80"
      ) +
      scale_size_continuous(
        name = "Intersection size",
        range = c(input$net_pathway_node_size * 0.6, input$net_pathway_node_size * 1.4)
      ) +
      theme_void() +
      theme(
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 9)
      )
    
    p
  })
  
  output$networkPlot <- renderPlot({
    build_network_plot()
  })
  
  output$downloadNetPDF <- downloadHandler(
    filename = function() {
      paste("Network_Plot", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf_width <- if (input$net_paper_size == "A4") 8.27 else 5.83
      pdf_height <- if (input$net_paper_size == "A4") 11.69 else 8.27
      if (input$net_orientation == "Landscape") {
        temp <- pdf_width
        pdf_width <- pdf_height
        pdf_height <- temp
      }
      pdf(file, width = pdf_width, height = pdf_height)
      print(build_network_plot())
      dev.off()
    }
  )
  
  output$downloadNetSVG <- downloadHandler(
    filename = function() {
      paste("Network_Plot", Sys.Date(), ".svg", sep = "")
    },
    content = function(file) {
      svg(file, width = input$net_svg_width / 2.54, height = input$net_svg_height / 2.54)
      print(build_network_plot())
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
