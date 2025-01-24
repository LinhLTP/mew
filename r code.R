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

# Data Import
# Display SPSS variable view and clean column names
sjPlot::view_df(mydata)  # View variable metadata
df <- mydata %>% janitor::clean_names()


# Data Anonymous

## Step 1: Convert SPSS data to factors with value labels
df[cols] <- lapply(df[cols], haven::as_factor).       # without the comma is a *list* subset
sapply(df[cols], class)                               # check the class

df2 <- df
df2[, cols] <- lapply(df2[, cols], haven::as_factor)  # with the comma is a *matrix* subset 

## Step 2: Threshold - convert factors to numeric values
df[cols] <- lapply(df[cols], function(x) as.numeric(levels(x)[x]))
sapply(df[cols], class)

summaries <- vector(mode = "list", ncol(threshold)).  # generate summaries for each column in `threshold`
for (i in 1:ncol(threshold)) {
  sm <- summary(threshold[[i]])
  summaries[[i]] <- sm
}

outcome <- vector("list", 7)                          # histogram        
for (i in seq_along(threshold)) {
  print(i)
  var_name     <- names(threshold[i])
  title        <- paste0("Histogram of ", var_name, " values:") 
  x_lab        <- var_name
  outcome[[i]] <- hist(threshold[[i]], breaks = 60,
                       main = title, xlab = x_lab)
}

out <- vector("list", 7)                             # frequency 
for (i in seq_along(threshold)) {
  freq <- data_tabulate(threshold[[i]]) 
  out[[i]] <- freq
}
out

## Step 3: Anonymise data
df <- df %>%   
  mutate(
    a3_1 = case_when(
      a3 >= 3 & a3 <= 6    ~ "3-6",
      a3 >= 7 & a3 <= 10   ~ "7-10",
      .default             = "11-12",                # true ~ "11-12"
      is.na(a3)            ~ "No answer"
    ), .after = a3
  ) 

df <- df %>% select(-c(a3)) %>% rename('a3' = 'a3_1')

data_tabulate(df$a3)                               # frequency of a3


