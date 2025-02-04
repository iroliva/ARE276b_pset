# ARE276b Problem Set
# Replication code for Chay and Greenstone 2005.

# Ignacio Oliva

# Packages
library(haven)    # package for reading dta
library(stargazer) # package for exporting tables
library(tidyverse)
library(rstatix)  # package for statistical tests and adding significance
library(ivreg)  #package for IV regression
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
                        # Extract coefficients and standard errors from regression results
                        coef_list <- lapply(regression_results, function(x) x$coefficients["tsp7576", ])

                        # Create a data frame to store the results
                        results_df <- data.frame(
                            Variable = names(coef_list),
                            Estimate = sapply(coef_list, function(x) x["Estimate"]),
                            Std_Error = sapply(coef_list, function(x) x["Std. Error"]),
                            t_value = sapply(coef_list, function(x) x["t value"]),
                            p_value = sapply(coef_list, function(x) x["Pr(>|t|)"])
                        )

                        # Create a stargazer table with the results
                        stargazer(results_df, type = "text",
                                            title = "Regression Results for tsp7576",
                                            out = "Regression_Results_tsp7576.tex")








B1 <- lm(dgtsp ~ tsp7576, data = data)
B2 <- lm(dlhouse ~ tsp7576, data = data)
B3 <- lm(dunemp ~ tsp7576, data = data)

# Collect the estimates of tsp7576 in a table
tsp7576_estimates <- data.frame(
    Variable = c("dgtsp", "dincome", "dunemp"),
    Estimate = c(coef(B1)["tsp7576"], coef(B2)["tsp7576"], coef(B3)["tsp7576"]),
    Std_Error = c(summary(B1)$coefficients["tsp7576", "Std. Error"],
                  summary(B2)$coefficients["tsp7576", "Std. Error"],
                  summary(B3)$coefficients["tsp7576", "Std. Error"]),
    t_value = c(summary(B1)$coefficients["tsp7576", "t value"],
                summary(B2)$coefficients["tsp7576", "t value"],
                summary(B3)$coefficients["tsp7576", "t value"]),
    p_value = c(summary(B1)$coefficients["tsp7576", "Pr(>|t|)"],
                summary(B2)$coefficients["tsp7576", "Pr(>|t|)"],
                summary(B3)$coefficients["tsp7576", "Pr(>|t|)"])
)

print(tsp7576_estimates)


summary(AAA)




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