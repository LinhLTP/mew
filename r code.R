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


# Calculation 
df_h5 <- haven::as_factor(df_h5) 

## S1: combine three income values into one value of annual_income 
df_h5 <- df_h5 %>% 
  unite(
    col = 'annual_income',
    c('h5a', 'h5b', 'h5c'),
    sep = " ",
    remove = FALSE,
    na.rm = TRUE
  )

## S2: Average weekly income
df_h5 <- df_h5 %>% 
  separate(h5a, into = c("w_from", "w_to"), sep = "-", extra = "merge") # remove string, text 

df_h5 <- df_h5 %>%
  mutate(
    w_from = as.numeric(gsub(",|and more", "", as.character(w_from))),
    w_to = as.numeric(gsub(",", "", as.character(w_to)))
  )


df_h5 <- df_h5 %>%                                                 # calculation annual income from weekly income 
  mutate(
    w_from_annual = (df_h5$w_from * 52), .after = w_to) %>%        # 52 week per year    
  mutate(
    w_to_annual = (df_h5$w_to * 52), .after = w_from_annual) 
  %>% mutate(
    w_from_annual = as.character(w_from_annual),
    w_to_annual = as.character(w_to_annual)
  )


df_h5 <- df_h5 %>%                                                 # Unite annual income columns into a single range
  unite(
    col = 'annual_income (w)',
    c('w_from_annual', 'w_to_annual'),
    sep = '-',
    remove = TRUE,
    na.rm = FALSE
  )


df_h5 <- df_h5 %>%                                                # Clean up and handle special cases in "annual_income (w)" 
  mutate(
    `annual_income (w)` = gsub("NA-NA", "", `annual_income (w)`),
    `annual_income (w)` = str_replace(`annual_income (w)`, "54236-NA", "54236 and more")
  ) %>%
  select(-w_from, -w_to)                                         # Remove intermediate columns

df_h5

