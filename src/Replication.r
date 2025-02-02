# ARE276b Problem Set
# Replication code for Chay and Greenstone 2005.

# Ignacio Oliva

# Packages
library(haven)    # package for reading dta
library(stargazer) # package for exporting tables
library(tidyverse)
library(rstatix)  # package for statistical tests and adding significance
library(ivreg)
# Import data
data_path <- "./data/poll7080.dta"
data <- read_dta(data_path)

# 1. Estimate the relationship between changes in air pollution and housing prices:

# 1.1 Not adjusting for any control variable.
model11 <- lm(dlhouse ~ I(dgtsp / 100), data = data)
summary(model11)

# 1.2 Adjusting for the main effects of the control variables listed on the previous page
model12 <- lm(dlhouse ~ I(dgtsp / 100) + tsp75 + tsp7576 + mtspgm74 + mtspgm75,
              data = data)
summary(model12)

# 1.3 Adjusting for the main effects, polynomials and interactions of the control variables
model13 <- lm(dlhouse ~ I(dgtsp / 100) + tsp75 + tsp7576 + mtspgm74 + mtspgm75 +
              ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll + durban +
              dunemp + dincome + dpoverty + vacant70 + vacant80 + vacrnt70 +
              downer + dplumb + drevenue + dtaxprop + depend + deduc + dhghwy +
              dwelfr + dhlth + blt1080 + blt2080 + bltold80, data = data)
summary(model13)

# Answers
stargazer(model11, model12, model13, type = "latex",
          title = "Regression Results",
          dep.var.labels = "Change in Housing Prices",
          covariate.labels = c("Change in Air Pollution", "TSP 1975", "TSP 1975-76",
                               "Mean TSP Growth 1974", "Mean TSP Growth 1975",
                               "Change in Density", "Change in Manufacturing",
                               "Change in White Population", "Change in Female Labor Force",
                               "Change in Age 65+", "Change in High School Graduates",
                               "Change in College Graduates", "Change in Urban Population",
                               "Change in Unemployment", "Change in Income",
                               "Change in Poverty", "Vacant Housing 1970",
                               "Vacant Housing 1980", "Vacant Rental Housing 1970",
                               "Change in Home Ownership", "Change in Plumbing",
                               "Change in Revenue", "Change in Property Tax",
                               "Change in Dependents", "Change in Education Spending",
                               "Change in Highway Spending", "Change in Welfare Spending",
                               "Change in Health Spending", "Built 1970-1980",
                               "Built 1980-1990", "Built Before 1980"),
          out = "Table_q1.tex")

# 2. Use tsp7576 as an instrument.

# 2.1 Assumptions for being a valid instrument

# Uncorrelation

# 2.2 Evidence: Replicate balance table, column 4
# Calculate means for each variable based on tsp75
variables <- c("dgtsp", "dlhouse", "mtspgm74", "mtspgm75", "ddens", "dmnfcg",
               "dwhite", "dfeml", "dage65", "dhs", "dcoll", "durban", "dunemp",
               "dincome", "dpoverty", "vacant70", "vacant80", "vacrnt70",
               "downer", "dplumb", "drevenue", "dtaxprop", "depend", "deduc",
               "dhghwy", "dwelfr", "dhlth", "blt1080", "blt2080", "bltold80")

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

# 3. Revise instrument assumptions

# 3.1 First stage relationship between regulation and air pollution changes.
model31a <- lm(dgtsp ~ tsp7576, data = data)
summary(model31a)

model131b <- lm(dgtsp ~ tsp7576 + tsp75 + mtspgm74 + mtspgm75, data = data)

model131c <- lm(dgtsp ~ tsp7576 + tsp7576 + tsp75 + mtspgm74 + mtspgm75 +
                ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll +
                durban + dunemp + dincome + dpoverty + vacant70 + vacant80 +
                vacrnt70 + downer + dplumb + drevenue + dtaxprop + depend +
                deduc + dhghwy + dwelfr + dhlth + blt1080 + blt2080 + bltold80,
                data = data)

stargazer(model31a, model131b, model131c, type = "latex",
          title = "First Stage Regression Results",
          dep.var.labels = "Change in Air Pollution",
          covariate.labels = c("TSP 1975-76", "TSP 1975", "Mean TSP Growth 1974",
                               "Mean TSP Growth 1975", "Change in Density",
                               "Change in Manufacturing", "Change in White Population",
                               "Change in Female Labor Force", "Change in Age 65+",
                               "Change in High School Graduates", "Change in College Graduates",
                               "Change in Urban Population", "Change in Unemployment",
                               "Change in Income", "Change in Poverty", "Vacant Housing 1970",
                               "Vacant Housing 1980", "Vacant Rental Housing 1970",
                               "Change in Home Ownership", "Change in Plumbing",
                               "Change in Revenue", "Change in Property Tax",
                               "Change in Dependents", "Change in Education Spending",
                               "Change in Highway Spending", "Change in Welfare Spending",
                               "Change in Health Spending", "Built 1970-1980",
                               "Built 1980-1990", "Built Before 1980"),
          out = "Table_q3_1.tex")

# 3.2 Relation between regulation and housing price changes, using 3 same specifications in 1.
model32a <- lm(dlhouse ~ tsp7576, data = data)
summary(model32a)

model132b <- lm(dlhouse ~ tsp7576 + tsp75 + mtspgm74 + mtspgm75, data = data)

model132c <- lm(dlhouse ~ tsp7576 + tsp7576 + tsp75 + mtspgm74 + mtspgm75 +
                ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll +
                durban + dunemp + dincome + dpoverty + vacant70 + vacant80 +
                vacrnt70 + downer + dplumb + drevenue + dtaxprop + depend +
                deduc + dhghwy + dwelfr + dhlth + blt1080 + blt2080 + bltold80,
                data = data)

stargazer(model32a, model132b, model132c, type = "latex",
          title = "Second Stage Regression Results",
          dep.var.labels = "Change in Housing Prices",
          covariate.labels = c("TSP 1975-76", "TSP 1975", "Mean TSP Growth 1974",
                               "Mean TSP Growth 1975", "Change in Density",
                               "Change in Manufacturing", "Change in White Population",
                               "Change in Female Labor Force", "Change in Age 65+",
                               "Change in High School Graduates", "Change in College Graduates",
                               "Change in Urban Population", "Change in Unemployment",
                               "Change in Income", "Change in Poverty", "Vacant Housing 1970",
                               "Vacant Housing 1980", "Vacant Rental Housing 1970",
                               "Change in Home Ownership", "Change in Plumbing",
                               "Change in Revenue", "Change in Property Tax",
                               "Change in Dependents", "Change in Education Spending",
                               "Change in Highway Spending", "Change in Welfare Spending",
                               "Change in Health Spending", "Built 1970-1980",
                               "Built 1980-1990", "Built Before 1980"),
          out = "Table_q3_2.tex")

# 3.3 2SLS using two different instruments

# Using tsp7576 as an instrument
model331a <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp7576, data = data)
summary(model331a)

model331b <- ivreg(dlhouse ~ tsp75 + mtspgm74 + mtspgm75 | I(dgtsp / 100) | tsp7576  , data = data)
summary(model331b)

model331c <- ivreg(dlhouse ~ tsp75 + mtspgm74 + mtspgm75 +
                ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll +
                durban + dunemp + dincome + dpoverty + vacant70 + vacant80 +
                vacrnt70 + downer + dplumb + drevenue + dtaxprop + depend +
                deduc + dhghwy + dwelfr + dhlth + blt1080 + blt2080 + bltold80 |
                I(dgtsp / 100) | tsp7576, data = data)
summary(model331c)

# Using tsp75 as an instrument
model332a <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp75, data = data)
summary(model332a)

model332b <- ivreg(dlhouse ~ tsp7576 + mtspgm74 + mtspgm75 | I(dgtsp / 100) | tsp75, data = data)
summary(model332b)

model332c <- ivreg(dlhouse ~ tsp7576 + mtspgm74 + mtspgm75 +
                ddens + dmnfcg + dwhite + dfeml + dage65 + dhs + dcoll +
                durban + dunemp + dincome + dpoverty + vacant70 + vacant80 +
                vacrnt70 + downer + dplumb + drevenue + dtaxprop + depend +
                deduc + dhghwy + dwelfr + dhlth + blt1080 + blt2080 + bltold80 |
                I(dgtsp / 100) | tsp75, data = data)
summary(model332c)

stargazer(model331a, model331b, model331c, model332a, model332b, model332c, type = "latex",
          title = "2SLS Regression Results",
          dep.var.labels = "Change in Housing Prices",
          covariate.labels = c("Change in Air Pollution", "TSP 1975", "Mean TSP Growth 1974",
                               "Mean TSP Growth 1975", "Change in Density",
                               "Change in Manufacturing", "Change in White Population",
                               "Change in Female Labor Force", "Change in Age 65+",
                               "Change in High School Graduates", "Change in College Graduates",
                               "Change in Urban Population", "Change in Unemployment",
                               "Change in Income", "Change in Poverty", "Vacant Housing 1970",
                               "Vacant Housing 1980", "Vacant Rental Housing 1970",
                               "Change in Home Ownership", "Change in Plumbing",
                               "Change in Revenue", "Change in Property Tax",
                               "Change in Dependents", "Change in Education Spending",
                               "Change in Highway Spending", "Change in Welfare Spending",
                               "Change in Health Spending", "Built 1970-1980",
                               "Built 1980-1990", "Built Before 1980"),
          out = "Table_q3_3.tex")

# Q4
# Q4: Time series plot of the mean of mtspgm72 to mtspgm80, differentiating by tsp75

# Create a new data frame with the means for each year and tsp75 group
mean_data <- data %>%
    pivot_longer(cols = starts_with("mtspgm"), names_to = "year", values_to = "value") %>%
    mutate(year = as.numeric(str_extract(year, "\\d+"))) %>%
    group_by(year, tsp75) %>%
    summarize(mean_value = mean(value, na.rm = TRUE)) %>%
    filter(year <= 80) %>%
    filter(year > 69) %>%
    filter(!is.na(tsp75))%>%
    ungroup()
    

# Plot the time series
fig4_tsp <- ggplot(mean_data, aes(x = year, y = mean_value, color = factor(tsp75)))+
    geom_line() +
    geom_point() +
    labs(title = "Mean TSP Growth (1972-1980) by TSP 1975 Group",
             x = "Year",
             y = "Annual Average TSP",
             color = "TSP 1975") +
    theme_minimal() +
    geom_vline(xintercept = 74, linetype = "dashed", color = "black") +
    scale_y_continuous(limits = c(45, 95))

    # Save the plot
    ggsave("Figure_q4_tsp.jpg", plot = fig4, width = 8, height = 6, dpi = 300)