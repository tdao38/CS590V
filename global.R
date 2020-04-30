source('utils.R', echo = TRUE)

options(scipen = 999)

packages(c("shinydashboard", "readr", "tidyverse", "ggplot2", "ggrepel",
           "GGally", "plotly", "shiny", "scales", "shinyWidgets", "shinythemes",
           "shinyjs", "janitor", "DT","leaflet","sp","rgdal", "sf", "htmltools",
           "RColorBrewer", "shinycssloaders"))

# Load data
school <- read_csv('data/school_new.csv')
census <- read_csv('data/census_new.csv')
income <- read_csv('data/income.csv')
race <- read_csv('data/race.csv')
job_type <- read_csv('data/job_type.csv')
school_type <- read_csv('data/school_type.csv')
school_income <-read_csv('data/school_income.csv')
states <- readOGR(dsn = "data/cb_2016_us_state_500k.shp", encoding = "UTF-8", verbose = FALSE)

# CLeaning
#school_income$display_name <- gsub("--", " ", school_income$display_name)
school_income$variable <- factor(school_income$variable, levels = c("Mid Career 10th Percentile Salary", "Mid Career 25th Percentile Salary", "Mid Career Median Salary", "Mid Career 75th Percentile Salary", "Mid Career 90th Percentile Salary"))
school_income$value <-as.numeric(school_income$value)
school_income <- school_income %>%
  arrange(variable,value)

levels(school_income$variable) <- list('10th' = "Mid Career 10th Percentile Salary",
                                       '25th' = "Mid Career 25th Percentile Salary",
                                       'Median' = "Mid Career Median Salary",
                                       '75th' = "Mid Career 75th Percentile Salary",
                                       '90th' = "Mid Career 90th Percentile Salary")

school_median <- school_income %>% filter(variable == 'Median') %>% select(display_name, value)
colnames(school_median)[2] <- "median_value"
school_income <- school_income %>% 
  left_join(school_median, by = 'display_name')
