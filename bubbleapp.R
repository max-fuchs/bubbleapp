library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(stringr)
library(readxl)
library(scales)
library(grid)

ui <- fluidPage(
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
}

shinyApp(ui = ui, server = server)
