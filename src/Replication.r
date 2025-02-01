#ARE276b Problem Set
#Replication code for Chay and Greenstone 2005.

#Ignacio Oliva

#Packages
library(haven)             # package for reading dta
library(stargazer)         # package for exporting tables
library(tidyverse)
library(rstatix)          # package for statistical tests and adding significance

## Import data
data_path <- "./data/poll7080.dta"
data <- read_dta(data_path)

## 1. Estimate the relationship between changes in air pollution and housing prices:

### 1.1 Not adjusting for any control variable.
model11 <- lm(dlhouse ~ I(dgtsp / 100), data = data)
summary(model11)

### 1.2 Adjusting for the main effects of the control variables listed on the previous page

model12 <- lm(dlhouse ~ I(dgtsp / 100) + tsp75 + tsp7576 + mtspgm74 + mtspgm75, data = data)
summary(model12)

### 1.3 adjusting for the main effects, polynomials and interactions of the control variables included in the data set

model13 <- lm(dlhouse ~ I(dgtsp / 100) + tsp75 + tsp7576 + mtspgm74 + mtspgm75 + 
    ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll + durban + dunemp + 
    dincome + dpoverty + vacant70 + vacant80 + vacrnt70 + downer + dplumb +
    drevenue + dtaxprop + depend + deduc + dhghwy + dwelfr + dhlth + blt1080 +
    blt2080 + bltold80, data = data)
summary(model13)

### Answers
stargazer(model11, model12, model13, type = "latex",
          title = "Regression Results",
          dep.var.labels = "Change in Housing Prices",
          covariate.labels = c("Change in Air Pollution", "TSP 1975", "TSP 1975-76", "Mean TSP Growth 1974", "Mean TSP Growth 1975",
                               "Change in Density", "Change in Manufacturing", "Change in White Population", "Change in Female Labor Force",
                               "Change in Age 65+", "Change in High School Graduates", "Change in College Graduates", "Change in Urban Population",
                               "Change in Unemployment", "Change in Income", "Change in Poverty", "Vacant Housing 1970", "Vacant Housing 1980",
                               "Vacant Rental Housing 1970", "Change in Home Ownership", "Change in Plumbing", "Change in Revenue",
                               "Change in Property Tax", "Change in Dependents", "Change in Education Spending", "Change in Highway Spending",
                               "Change in Welfare Spending", "Change in Health Spending", "Built 1970-1980", "Built 1980-1990", "Built Before 1980"),
          out = "Table_q1.tex")

## 2. Use tsp7576 as an instrument.

### 2.1 Assumptions for being a valid instrument

#### Uncorrelation 


### 2.2 Evidence: Replicate balance table, column 4
# Calculate means for each variable based on tsp75
variables <- c("dgtsp", "dlhouse", "mtspgm74", "mtspgm75", "ddens", "dmnfcg", "dwhite", "dfeml", 
               "dage65", "dhs", "dcoll", "durban", "dunemp", "dincome", "dpoverty", "vacant70", "vacant80", 
               "vacrnt70", "downer", "dplumb", "drevenue", "dtaxprop", "depend", "deduc", "dhghwy", "dwelfr", 
               "dhlth", "blt1080", "blt2080", "bltold80")

mean_differences <- data %>%
    select(all_of(variables), tsp7576) %>%
    pivot_longer(cols = -tsp7576, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable, levels = variables)) %>%
    group_by(variable) %>%
    t_test(value ~ tsp7576) %>%
    add_significance()
   
mean_differences <- mean_differences %>%
        mutate(statistic = -statistic)


# Create a table showing the results of the t-tests


## 3. Revise instrument assumptions

### 3.1 First stage relationship between regulation and air pollution changes.

model31 <- lm(dgtsp ~ tsp7576 + 0, data = data)
summary(model31)

### 3.2 Relation between regulation and housing price changes, using 3 same specifications in 1.
model32 <- lm(dlhouse ~ tsp7576, data = data)
summary(model32)
