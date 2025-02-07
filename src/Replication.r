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

# Import data
data_path <- "./data/poll7080.dta"
data <- read_dta(data_path)

#0. Set up

main_controls <- c("ddens", "dmnfcg", "dwhite", "dfeml", "dage65", "dhs",
                    "dcoll", "durban", "dunemp", "dincome", "dpoverty",
                    "downer", "dplumb", "drevenue", "dtaxprop", "depend",
                    "deduc", "dhghwy", "dwelfr", "dhlth", "vacant70",
                    "vacant80", "vacrnt70", "blt1080", "blt2080", "bltold80")

polinomial_controls <- c("popwhite", "popage65", "pophs", "popcoll", "popincm",
                         "manwhite", "manage65", "manhs", "mancoll", "manincm",
                         "whtage", "whths", "whtcoll", "whtincm", "incage",
                         "inchs", "inccoll", "pop2", "pop3", "urban2",
                         "urban3", "white2", "white3", "femal2", "femal3",
                         "age2", "age3", "hs2", "hs3", "coll2", "coll3",
                         "unemp2", "unemp3", "mnfcg2", "mnfcg3", "income2",
                         "income3", "poverty2", "poverty3", "vacant2",
                         "vacant3", "owner2", "owner3", "plumb2", "plumb3",
                         "revenue2", "revenue3", "taxprop2", "taxprop3",
                         "epend2", "epend3", "pcteduc2", "pcteduc3", "pcthghw2",
                         "pcthghw3", "pctwelf2", "pctwelf3", "pcthlth2",
                         "pcthlth3", "built102", "built103", "built202",
                         "built203", "builold2", "builold3")

all_controls <- c(main_controls, polinomial_controls)

my_lm <- function(y, var, controls, database){
    formula <- reformulate(termlabels = paste(c(var, controls)), response = y)
    output <- lm(formula, data = database)
    return (output)
}

# 1. Estimate the relationship between changes in air pollution and housing prices:

## Regressions
model1 <- lm(dlhouse ~ I(dgtsp / 100), data = data)

model1_main <- my_lm("dlhouse", "I(dgtsp / 100)", main_controls, data)

model1_all <- my_lm("dlhouse", "I(dgtsp / 100)", all_controls, data)


## Table with results
stargazer(model1, model1_main, model1_all, type = "text",
          keep = "dgtsp",
          title = "Regression Results",
          dep.var.labels = "1970-80 (First Differences)",
          covariate.labels = "Mean TSPs (1/100)",
          add.lines = list(c("Main effects", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes")),
          out = "Table_q1.tex")


# 2. Use tsp7576 as an instrument.




## Replication Table 2, column 4
economic_vars <- c("dlhouse", "dgtsp", "dincome", "pop7080", "dunemp", "dmnfcg",
                "ddens", "I(durban * 10)", "I(dpoverty * 10)",
                "I(dwhite * 10)", "blt1080", "downer", "I(dplumb * 100)",
                "drevenue", "dtaxprop", "deduc") 
            # Loop to regress each variable in the list on tsp7576
            regression_results <- list()

            for (var in economic_vars) {
                formula <- as.formula(paste(var, "~ tsp7576"))
                model <- lm(formula, data = data)
                regression_results[[var]] <- summary(model)
            }

            # Print summaries of the regression results
            for (var in names(regression_results)) {
                cat("Regression results for", var, ":\n")
                print(regression_results[[var]])
                cat("\n")}

# 2.1 Assumptions for being a valid instrument

# Uncorrelation

# 2.2 Evidence: Replicate balance table, column 4
# Calculate means for each variable based on tsp75
variables <- c("dlhouse", "dgtsp", "dincome", "pop7080", "dunemp", "dmnfcg",
            "ddens", "I(durban * 10)", "I(dpoverty * 10)",
            "I(dwhite * 10)", "blt1080", "downer", "I(dplumb * 100)",
             "drevenue", "dtaxprop", "deduc")

mean_differences <- data %>%
    select(all_of(variables), tsp7576) %>%
    pivot_longer(cols = -tsp7576, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable, levels = variables)) %>%
    group_by(variable) %>%
    t_test(value ~ tsp7576) %>%
    add_significance()

mean_differences <- mean_differences %>%
    mutate(statistic = -statistic)

mean_differences

# Create a table showing the results of the t-tests

# 3. Revise instrument assumptions

# 3.1 First stage relationship between regulation and air pollution changes.

first_stage         <- lm(dgtsp ~ tsp7576, data = data)

first_stage_main    <- my_lm("dgtsp", "tsp7576", main_controls, data)

first_stage_all     <- my_lm("dgtsp", "tsp7576", all_controls, data)

second_stage        <- lm(dlhouse ~ tsp7576, data = data)

second_stage_main   <- my_lm("dlhouse", "tsp7576", main_controls, data)

second_stage_all    <- my_lm("dlhouse", "tsp7576", all_controls, data)

iv      <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp7576, data = data)

iv_main <- ivreg(
    dlhouse ~ ddens + dmnfcg + dwhite + dfeml + dage65 + dhs +
    dcoll + durban + dunemp + dincome + dpoverty + downer + dplumb + drevenue +
    dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + vacant70 + vacant80 +
    vacrnt70 + blt1080 + blt2080 + bltold80 | I(dgtsp / 100) | tsp7576,
    data = data
)


iv_all <- ivreg(
dlhouse ~ ddens + dmnfcg + dwhite + dfeml + dage65 + dhs +
dcoll + durban + dunemp + dincome + dpoverty + downer + dplumb + drevenue +
dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + vacant70 + vacant80 +
vacrnt70 + blt1080 + blt2080 + bltold80 + popwhite + popage65 + pophs +
popcoll + popincm + manwhite + manage65 + manhs + mancoll + manincm +
whtage + whths + whtcoll + whtincm + incage + inchs + inccoll + pop2 +
pop3 + urban2 + urban3 + white2 + white3 + femal2 + femal3 + age2 + age3 +
hs2 + hs3 + coll2 + coll3 + unemp2 + unemp3 + mnfcg2 + mnfcg3 + income2 +
income3 + poverty2 + poverty3 + vacant2 + vacant3 + owner2 + owner3 +
plumb2 + plumb3 + revenue2 + revenue3 + taxprop2 + taxprop3 + epend2 +
epend3 + pcteduc2 + pcteduc3 + pcthghw2 + pcthghw3 + pctwelf2 + pctwelf3 +
pcthlth2 + pcthlth3 + built102 + built103 + built202 + built203 + builold2 +
builold3 | I(dgtsp / 100) | tsp7576, data = data
)

iv75      <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp75, data = data)

iv75_main <- ivreg(
    dlhouse ~ ddens + dmnfcg + dwhite + dfeml + dage65 + dhs +
    dcoll + durban + dunemp + dincome + dpoverty + downer + dplumb + drevenue +
    dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + vacant70 + vacant80 +
    vacrnt70 + blt1080 + blt2080 + bltold80 | I(dgtsp / 100) | tsp75,
    data = data
)


iv75_all <- ivreg(
dlhouse ~ ddens + dmnfcg + dwhite + dfeml + dage65 + dhs +
dcoll + durban + dunemp + dincome + dpoverty + downer + dplumb + drevenue +
dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + vacant70 + vacant80 +
vacrnt70 + blt1080 + blt2080 + bltold80 + popwhite + popage65 + pophs +
popcoll + popincm + manwhite + manage65 + manhs + mancoll + manincm +
whtage + whths + whtcoll + whtincm + incage + inchs + inccoll + pop2 +
pop3 + urban2 + urban3 + white2 + white3 + femal2 + femal3 + age2 + age3 +
hs2 + hs3 + coll2 + coll3 + unemp2 + unemp3 + mnfcg2 + mnfcg3 + income2 +
income3 + poverty2 + poverty3 + vacant2 + vacant3 + owner2 + owner3 +
plumb2 + plumb3 + revenue2 + revenue3 + taxprop2 + taxprop3 + epend2 +
epend3 + pcteduc2 + pcteduc3 + pcthghw2 + pcthghw3 + pctwelf2 + pctwelf3 +
pcthlth2 + pcthlth3 + built102 + built103 + built202 + built203 + builold2 +
builold3 | I(dgtsp / 100) | tsp75, data = data
)

# Table with first stage and second stage results
stargazer(first_stage, first_stage_main, first_stage_all,
          type = "latex",
          keep = c("tsp7576"),
          title = "First Stage Results",
          dep.var.labels = "A. Mean TSPs Changes",
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          out = "Table_first_stage.tex")


stargazer(second_stage, second_stage_main, second_stage_all,
          type = "latex",
          keep = c("tsp7576"),
          title = "Second Stage Results",
          dep.var.labels = c("B. Log Housing Changes"),
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          out = "Table_second_stage.tex")


stargazer(iv, iv_main,
          type = "text",
          keep = "dgtsp",
          title = "IV Results",
          dep.var.labels = c("TSPs Nonattainment in 1975 or 1976"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          out = "Table_IV.tex")

stargazer(iv75, iv75_main,
          type = "text",
          keep = "dgtsp",
          title = "IV Results",
          dep.var.labels = c("TSPs Nonattainment in 1975"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          out = "Table_IV_75.tex")

# Q4
# Replicate figure 4

# Q4
# Replicate figure 4

# Create the plot


# RD regressions

data_rd <- data 

data_rd <- data_rd %>% 
    filter(mtspgm74 >= 50 & mtspgm74 <= 100) %>%
    filter(!(tsp75 == 1 & mtspgm74 < 75))

test<- ivreg(dlhouse ~ I(mtspgm74^2) + I(mtspgm75^2) + I(mtspgm75^2)*I(mtspgm75^2) |  I(dgtsp / 100) | tsp7576, data = data_rd)

summary(test)

test <- ivreg(dlhouse ~ I(mtspgm74^2) + I(mtspgm75^2) | I(dgtsp / 100) + tsp7576, data = data_rd)


iv75_main <- ivreg(
    dlhouse ~ ddens + dmnfcg + dwhite + dfeml + dage65 + dhs +
    dcoll + durban + dunemp + dincome + dpoverty + downer + dplumb + drevenue +
    dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + vacant70 + vacant80 +
    vacrnt70 + blt1080 + blt2080 + bltold80 | I(dgtsp / 100) | tsp75,
    data = data
)



# Print the summary of the RDD model
summary(rdd_model)

# Plot the RDD results
plot(rdd_model)




    


