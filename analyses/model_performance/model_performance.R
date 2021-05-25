library(dplyr)
library(tidyr)
library(readr)
library(analysis)

environment <- read_csv("../data/environment/environment.csv")
cpue <- read_csv("../data/cpue/cpue.csv")
yukon <- left_join(environment, cpue, by = "year")

# Candidate models
models <- c("mdj ~ amatc",
            "mdj ~ msstc",
            "mdj ~ pice",
            "mdj ~ amatc + msstc",
            "mdj ~ amatc + pice",
            "mdj ~ msstc + pice",
            "mdj ~ amatc + msstc + pice")

# Set up selection
window <- 15 # Go back this many years
hindcast_years <- seq(tail(yukon$year, 1) - (window-1), tail(yukon$year, 1))


create_performance_table(yukon, models, hindcast_years)
