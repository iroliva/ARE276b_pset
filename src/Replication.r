# ARE276b Problem Set
# Replication code for Chay and Greenstone 2005.

# Ignacio Oliva

# Packages
library(haven)    # package for reading dta
library(stargazer) # package for exporting tables
library(tidyverse)
library(dplyr)
library(rstatix)  # package for statistical tests and adding significance
library(ivreg)  #package for IV regression
library(ggplot2)
library(boot)

# Import data
data_path <- "./data/poll7080.dta"
data <- read_dta(data_path)

# Functions
source("./src/functions.R")

#0. Set up

no_controls <- c("") 

main_controls <- c("ddens", "dmnfcg", "dwhite", "dfeml", "dage65", "dhs",
                    "dcoll", "durban", "dunemp", "dincome", "dpoverty",
                    "downer", "dplumb", "drevenue", "dtaxprop", 
                    "deduc", "dhghwy", "dwelfr", "dhlth", "vacant70",
                    "vacant80", "vacrnt70", "bltold80")

polinomial_controls <- c("popincm", "manwhite", "manage65", "whtage",
                      "urban2", "femal2", "age2", "mnfcg2", "plumb3",
                      "pcteduc2", "pcteduc3", "pcthghw2", "pcthlth2")

all_controls <- c(main_controls, polinomial_controls)

iterations <- list(no_controls, main_controls, all_controls)

# Q1. Estimate the relationship between changes in air pollution and housing prices:

## Regressions

q1_reg <- my_lm_wrapper("dlhouse", "I(dgtsp / 100)", iterations, data)


## Table with results
stargazer(q1_reg[[1]], q1_reg[[2]], q1_reg[[3]], type = "latex",
          keep = "dgtsp",
          dep.var.labels = "1970-80 (First Differences)",
          covariate.labels = "Mean TSPs (1/100)",
          add.lines = list(c("Main effects", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes")),
          label = "t:q1",
          title = "Relationship between changes in air pollution and housing prices",
          out = "Table_q1.tex",
          font.size = "scriptsize")

## Evidence on the negative relationship between TSP and economic shocks.

data <- data %>%
    mutate(dgtsp_above_median = ifelse(dgtsp > median(dgtsp, na.rm = TRUE), 0, 1))

economic_shocks <- c("dincome", "dunemp", "dmnfcg", "ddens",
                     "I(durban * 10)", "blt1080")

title <- "Differences in sample means defined by TSP levels"
label <- "t:q1_evidence"
out <- "table_balance_q1.tex"  
column_name <- "First Difference 1980 1970"

Balance_table_q1 <- make_balance_table("dgtsp_above_median", economic_shocks, data, 
                column_name, title, label, out)

# Q2. Use tsp7576 as an instrument.

## Balance table

title_q2 <- "Differences in sample means by attainment status"
label_q2 <- "t:q2"
out_q2   <- "table_balance_q2.tex"
column_name_q2 <- "TSPS non attainment in 1975 or 1976"

Balance_table_q2 <- make_balance_table("tsp7576", economic_shocks, data,
    column_name_q2, title_q2, label_q2, out_q2)


# Q3. Revise instrument assumptions

## 3.1 First stage relationship between regulation and air pollution changes.

first_stage_reg     <- my_lm_wrapper("dgtsp", "tsp7576", iterations, data)

## Second stage

second_stage_reg    <- my_lm_wrapper("dlhouse", "tsp7576", iterations, data)

## 2SLS regressions

### Define variables
Y <- "dlhouse"
Endog<- "I(dgtsp/100)"
Ins <- "tsp7576"
Ins75 <- "tsp75"

q3_iv_ <- my_iv_wrapper(Y, Endog, Ins, iterations, data)

q3_iv75 <- my_iv_wrapper(Y, Endog, Ins75, iterations, data)

## Table with first stage and second stage results
stargazer(first_stage_reg[[1]], first_stage_reg[[2]], first_stage_reg[[3]],
          type = "latex",
          keep = c("tsp7576"),
          dep.var.labels = "A. Mean TSPs Changes",
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_firststage",
          title = "First stage relationship between regulation and air pollution changes",
          font.size = "scriptsize",
          out = "Table_first_stage.tex")

stargazer(second_stage_reg[[1]], second_stage_reg[[2]], second_stage_reg[[3]],
          type = "latex",
          keep = c("tsp7576"),
          dep.var.labels = c("B. Log Housing Changes"),
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_secondstage",
          title = "Reduced form relationship between regulation and housing prices",
          font.size = "scriptsize",
          out = "Table_second_stage.tex")

##Tables with iv estimates
stargazer(q3_iv[[1]], q3_iv[[2]],
          type = "latex",
          keep = "dgtsp",
          title = "IV Results using TSP7576 as instrumental variable",
          dep.var.labels = c("TSPs Nonattainment in 1975 or 1976"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_iv",
          font.size = "scriptsize",
          out = "Table_IV.tex")

stargazer(q3_iv75[[1]], q3_iv75[[2]],
          type = "latex",
          keep = "dgtsp",
          title = "IV Results using TSP75 as instrumental variable",
          dep.var.labels = c("TSPs Nonattainment in 1975"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          font.size = "scriptsize",
          label = "t:q3_iv75",
          out = "Table_IV_75.tex")

# Q4: RDD design

## RDD design

data_rd <- data %>% filter(!(mtspgm74 < 75 & tsp75 == 1))

q4_rd <- my_iv_wrapper(Y, Endog, Ins75, iterations, data_rd %>%
                    filter(mtspgm74 >= 50 & mtspgm74 <= 100))

## Matching

q4_matching     <- my_iv_wrapper(Y, Endog, Ins75, iterations, data %>%
                    filter(mtspgm74 >= 50 & mtspgm74 <= 75))

##Make tables

stargazer(q4_rd[[1]], q4_rd[[2]],
          type = "latex",
          keep = "dgtsp",
          title = "RD of the Effect of 1970–80 Changes in TSPs Pollution on Changes in Log Housing Values",
          dep.var.labels = c("B. Regression Discontinuity II TSPs Nonattainment in 1975"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q4_rd",
          font.size = "scriptsize",
          out = "Table_q4_rd.tex")

stargazer(q4_matching[[1]], q4_matching[[2]],
          type = "latex",
          keep = "dgtsp",
          title = "Bad day/matching of the Effect of 1970–80 Changes in TSPs Pollution on Changes in Log Housing Values",
          dep.var.labels = c("C. Bad Day/Matching TSPs Nonattainment in 1975"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q4_rd_badday",
          font.size = "scriptsize",
          out = "Table_q4_rd_badday.tex")

## Replicate figures 4 and 5

fig_4 <- make_figure(data, "mtspgm74", "dgtsp", c(-25,5), "1970–80 Change in Mean TSPs", "Geometric Mean TSPs in 1974", "Figure_4.pdf")

fig_5 <- make_figure(data, "mtspgm74", "dlhouse", c(0.2,0.35), "1970–80 change in log housing values", "Geometric Mean TSPs in 1974", "Figure_5.pdf")

# Q6: Garen - type control function and bootstrap

Bootstrap_results <- my_bootstrap_table(iterations, data)
