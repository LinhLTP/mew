# Load required packages using pacman
pacman::p_load(
  # File management and data import
  here,        # For relative file paths
  rio,         # For importing/exporting data
  
  # Libraries for handling SPSS data
  haven,       # Reading and writing SPSS, Stata, and SAS data
  sjlabelled,  # Working with labelled data
  labelled,    # Handling variable labels
  sjPlot,      # Visualisation of labelled data
  
  # Data processing libraries
  dplyr,       # Data manipulation
  janitor,     # Data cleaning and tabulation
  lubridate,   # Handling dates
  matchmaker,  # Dictionary-based cleaning
  epikit,      # For the age_categories() function
  tidyverse,   # Comprehensive data management and visualisation toolkit
  datawizard,  # Useful for descriptive statistics (e.g., freq)
  
  # Additional utilities
  surveytoolbox,  # Survey data tools
  pbkrtest,       # Tools for linear mixed models
  foreign,        # Reading data from other formats (e.g., SPSS, Stata)
  gmodels,        # Various statistical models
  AMR,            # Epidemiology tools (e.g., age grouping)
  skimr,          # Quick data summaries
  apyramid        # Age pyramids for demographic data
)
