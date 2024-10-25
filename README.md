# g:Profiler Results Plotter

This Shiny app helps visualize g:Profiler functional analysis results as bubble plots

## Instructions

1. Go to [g:Profiler](https://biit.cs.ut.ee/gprofiler/gost) and perform functional analysis on your DEGs.
2. Download the results as a `.csv` file.
3. Upload the file in this app to generate plots.

## Requirements

- [R (version >= 4.0)](https://www.r-project.org/)
- Shiny package
- shinyWidgets package
- ggplot2, dplyr, stringr packages

## Installation of Required Packages

To ensure all required packages are installed, you can run the following R code:

```r
# Install BiocManager if it’s not already installed
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Set Bioconductor version
BiocManager::install(version = "3.19")

# List of required packages
packages <- c("shiny", "shinyWidgets", "dplyr", "ggplot2", "stringr")

# Check each package and install any that are missing
packages_to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(packages_to_install) > 0) {
    BiocManager::install(packages_to_install)
}

# Note: Packages already installed will be skipped


## Running the App

1. Clone the repository: `git clone https://github.com/your-username/bubbleapp.git` or download bubbleapp.R
2. Open R or RStudio.
3. Run the following command to launch the app:

   ```r
   shiny::runApp("path/to/bubbleapp.R")
