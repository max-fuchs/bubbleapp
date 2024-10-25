# g:Profiler Results Plotter

This Shiny app helps visualize g:Profiler functional analysis results as bubble plots

## Instructions

1. Go to [g:Profiler](https://biit.cs.ut.ee/gprofiler/gost) and perform functional analysis on your DEGs.
2. Download the results as a `.csv` file.
3. Upload the file in this app to generate plots.

## Requirements

- R (version >= 4.0)
- Shiny package
- shinyWidgets package
- ggplot2, dplyr, stringr packages

## Running the App

1. Clone the repository: `git clone https://github.com/your-username/bubbleapp.git`
2. Open R or RStudio.
3. Run the following command to launch the app:

   ```r
   shiny::runApp("path/to/bubbleapp.R")
