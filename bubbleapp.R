library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(stringr)

# UI
ui <- fluidPage(
  titlePanel("g:Profiler Results Plotter - by MF"),
  tabsetPanel(
    tabPanel("Introduction",
             fluidRow(
               column(12, 
                      h4("Instructions"),
                      p("Go to ", 
                        tags$a(href = "https://biit.cs.ut.ee/gprofiler/gost", target = "_blank", 
                               "https://biit.cs.ut.ee/gprofiler/gost"), 
                        " and perform functional analysis on your significant DEGs. Then download results as a .csv file and input at Plot Settings Tab.")
               )
             )
    ),
    tabPanel("Plot Settings",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV File", accept = ".csv"),
                 numericInput("wrap_width", "Wrap Width:", value = 30, min = 1, max = 100),
                 numericInput("y_axis_text_size","Term name text size", value = 12),
                 selectInput("y_axis_text_face", "Y-Axis Text Face",
                             choices = c("plain","italic","bold","bold.italic")),
                 numericInput("top_n", "Top N Terms to Display:", value = 15, min = 1),
                 selectInput("plot_type", "Plot Type:", choices = c("Bubble Plot", "Bar Plot")), # Auswahl des Plot-Typs
                 conditionalPanel(
                   condition = "input.plot_type == 'Bar Plot'",
                   selectInput("bar_plot_metric", "Metric for Bar Plot:", 
                               choices = c("Intersection Size", "Gene Ratio")) # Auswahl der Metrik
                 ),
                 radioButtons("paper_size", "Paper Size:", choices = c("A4", "A5")),
                 radioButtons("orientation", "Orientation:", choices = c("Portrait", "Landscape")),
                 colorPickr("low_color", "Select Low Color:", selected = "#1E90FF"),  
                 colorPickr("high_color", "Select High Color:", selected = "#8B0000"), 
                 downloadButton("downloadPlot", "Download Plot as PDF")
               ),
               mainPanel(
                 plotOutput("genePlot")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive expression to read the uploaded CSV file
  data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    read.csv(input$file$datapath)
  })
  
  # Generate the plot
  genePlot <- reactive({
    req(data())
    res <- data()
    
    # Check for the p-value column name
    if ("adjusted_p_value" %in% names(res)) {
      res$p_value_col <- res$adjusted_p_value
    } else if ("p_value" %in% names(res)) {
      res$p_value_col <- res$p_value
    } else {
      stop("The data must contain either 'adjusted_p_value' or 'p_value' column.")
    }
    
    # Data manipulation
    res$term_name <- str_to_sentence(res$term_name)
    res$GeneRatio <- res$intersection_size / res$term_size
    res$term_name <- str_wrap(res$term_name, width = input$wrap_width)
    res <- res %>% arrange(desc(GeneRatio)) %>% head(input$top_n)
    res$term_name <- factor(res$term_name, levels = unique(as.character(res$term_name)))
    
    # Plot based on selected type
    if (input$plot_type == "Bubble Plot") {
      ggplot(res) +
        geom_point(mapping = aes(x = GeneRatio, y = term_name, color = p_value_col, size = intersection_size)) +
        ylab("Term Name") +
        labs(size = "Intersection size", color = "Adjusted p-value") +
        scale_color_gradient(low = input$low_color, high = input$high_color) +
        scale_size_continuous(range = c(3, 10)) +
        theme(
          axis.text.y = element_text(size = input$y_axis_text_size,
                                     face = input$y_axis_text_face,
                                     color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "grey20"),
          axis.title.x = element_text(size = 16, face = "bold", color = "grey20")
        )
    } else {  # Bar Plot
      metric <- if (input$bar_plot_metric == "Intersection Size") {
        "intersection_size"
      } else {
        "GeneRatio"
      }
      
      ggplot(res) +
        geom_bar(mapping = aes_string(x = metric, y = "term_name", fill = "p_value_col"), 
                 stat = "identity") +
        ylab("Term Name") +
        xlab(if (metric == "intersection_size") "Intersection Size" else "Gene Ratio") +
        labs(fill = "Adjusted p-value") +
        scale_fill_gradient(low = input$low_color, high = input$high_color) +
        theme(
          axis.text.y = element_text(size = input$y_axis_text_size,
                                     face = input$y_axis_text_face,
                                     color = "black"),
          axis.title.y = element_text(size = 16, face = "bold", color = "grey20"),
          axis.title.x = element_text(size = 16, face = "bold", color = "grey20")
        )
    }
  })
  
  # Render the plot in the UI
  output$genePlot <- renderPlot({
    genePlot()
  })
  
  # Download the plot as PDF
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
}

# Run the application
shinyApp(ui = ui, server = server)
