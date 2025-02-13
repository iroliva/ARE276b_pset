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
source("functions.R")

my_lm <- function(y, var, controls, database){
    formula <- reformulate(termlabels = paste(c(var, controls)), response = y)
    output <- lm(formula, data = database)
    return (output)
}

my_lm_wrapper <- function(y, var, iterations, database) {
  lm_results <- list()

  for (i in seq_along(iterations)) {
    if (length(iterations[[i]]) == 1) {
      formula <- as.formula(paste(y, "~", var))
      lm_results[[i]] <- lm(formula, data = database)
    } else {
      lm_results[[i]] <- my_lm(y, var, iterations[[i]], database)
    }
  }
  return(lm_results)
}

my_ivreg <- function(y, controls, endog_var, instrument, data){
     formula <- as.formula(paste(y, "~", paste(controls, collapse = " + "), "|", paste(endog_var, collapse = " + "), "|", paste(instrument, collapse = " + ")))
   iv_results <- ivreg(formula, data = data)

   return(iv_results)
}

my_iv_wrapper <- function(y, endog_var, instrument, iterations, database) {
  iv_results <- list()

  for (i in seq_along(iterations)) {
    if (length(iterations[[i]]) == 1) {
      formula <- as.formula(paste(y, "~", endog_var, "|", instrument))
      iv_results[[i]] <- ivreg(formula, data = database)
    } else {
      iv_results[[i]] <- my_ivreg(y, iterations[[i]], endog_var, instrument, database)
    }
  }

  return(iv_results)
}

make_balance_table <- function(var, controls, database, column_name, table_title,
                            table_label, table_out) {
  results <- list()

  for (x in controls) {
    formula <- as.formula(paste(var, "~", x))
    coefs <- lm(formula, data = database)
    results[[x]] <- summary(coefs)
  }

  coefficients <- sapply(results, function(y) y$coefficients[2, 1])
  standard_errors <- sapply(results, function(y) y$coefficients[2, 2])
  p_values <- sapply(results, function(y) y$coefficients[2, 4])

  table_data <- data.frame(
    Variable = names(results),
    Coefficient = coefficients,
    Serror = standard_errors,
    p_value = p_values,
    Stars = ifelse(p_values < 0.01, "***", 
                   ifelse(p_values < 0.05, "**", 
                          ifelse(p_values < 0.1, "*", "")))
  )

Export_table <- data.frame(matrix(ncol = 2, nrow = 2 * nrow(table_data)))
colnames(Export_table) <- c("", column_name)

for(i in 1:nrow(table_data)){
        Export_table[2*i-1, 1] <- table_data$Variable[i]
        Export_table[2*i, 1]   <- ""
        Export_table[2 * i - 1, 2] <- paste0(
            round(table_data$Coefficient[i], 3), 
            table_data$Stars[i]
        )
        Export_table[2*i, 2] <- paste0("(", round(table_data$Serror[i], 3), ")")
}

stargazer(Export_table, type = "latex", summary = FALSE, rownames = FALSE,
          title = table_title,
          label = table_label,
          dep.var.caption = column_name,
          font.size = "small",
          out = table_out)

  return(Export_table)
}

make_figure <- function(data, x_var, y_var, y_axis_limits, y_label, x_label, file_name){

    my_figure <- ggplot(data %>% filter(mtspgm74 >= 25 & mtspgm74 <= 125), aes_string(x = x_var, y = y_var, color = "factor(tsp75)")) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", formula = "y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6)", se = FALSE) +
    theme_bw() +
    xlab(x_label) +
    geom_vline(xintercept = 75, linetype = "dashed", color = "black") +
    ylab(y_label) +
    scale_color_manual(values = c("red", "blue"), labels = c("Attainment in 1975", "Non attainment in 1975"), name = "") +
    coord_cartesian(ylim = y_axis_limits) +
    theme(aspect.ratio = 1/2, legend.position = "top")
  
    # Save the plot as a PDF
    pdf(file_name, width = 8, height = 6)
    print(my_figure)
    dev.off()
    
    return(my_figure)
}

my_bootstrap_table <- function(controls, data){

results_bootstrap <- list()

garen_type <- function(data, indices, controls){
    data <- data[indices, ]
    step1 <- lm(dgtsp ~ tsp7576, data = data)


    if(length(controls) == 1){
    fml <-  as.formula("dlhouse ~ I(dgtsp / 100) + I(resid(step1) / 100) + I(dgtsp*resid(step1) / 10000)")
    }

    else{
    fml <-  as.formula(paste(
        "dlhouse ~ I(dgtsp / 100) + I(resid(step1) / 100) + I(dgtsp*resid(step1) / 10000) +", 
        paste(controls, collapse = " + ")
    ))
    }
    step2 <- lm(fml, data = data)
    
    return(coef(step2))
}

for (i in 1:length(iterations)) {
  con <- iterations[[i]]
  boot_results <- boot(data = data, statistic = garen_type, R = 1000, controls = con)
  results_bootstrap[[i]] <- boot_results
}

bootstrap_table <- data.frame(matrix(ncol = length(iterations), nrow = 8))
colnames(bootstrap_table) <- c("(1)", "(2)", "(3)")
rownames(bootstrap_table) <- c("Mean TSPs (1/100)", "", "vi (first-stage residual) (1/100)", " ", "vi * mean TSPs (1/10,000)", "  ", "Main effects", "Main and Polinomials")

bootstrap_table <- as.matrix(bootstrap_table)

for (j in 1:length(iterations)) {
  bootstrap_table[1, j] <- round(results_bootstrap[[j]]$t0[2], 3)
  bootstrap_table[3, j] <- round(results_bootstrap[[j]]$t0[3], 3)
  bootstrap_table[5, j] <- round(results_bootstrap[[j]]$t0[4], 3)
  bootstrap_table[2, j] <- paste0("(", round(sd(results_bootstrap[[j]]$t[, 2]), 3), ")")
  bootstrap_table[4, j] <- paste0("(", round(sd(results_bootstrap[[j]]$t[, 3]), 3), ")")
  bootstrap_table[6, j] <- paste0("(", round(sd(results_bootstrap[[j]]$t[, 4]), 3), ")")
}

bootstrap_table[7, ] <- c("No", "Yes", "Yes")
bootstrap_table[8, ] <- c("No", "No", "Yes")

stargazer(bootstrap_table, type = "latex", summary = FALSE,
          title = "Control Function Estimates of the Capitalization of 1970–80 Changes in TSPs Pollution, with Correction for Selectivity Bias Due to Random Coefficients",
            label = "t:bootstrap_results",
          font.size = "scriptsize",
          out = "Table_bootstrap_results.tex")

return(bootstrap_table)

}

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
