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
          out = table_out)

  return(Export_table)
}





# Q1. Estimate the relationship between changes in air pollution and housing prices:

## Regressions
model1 <- lm(dlhouse ~ I(dgtsp / 100), data = data)

model1_main <- my_lm("dlhouse", "I(dgtsp / 100)", main_controls, data)

model1_all <- my_lm("dlhouse", "I(dgtsp / 100)", all_controls, data)


## Table with results
stargazer(model1, model1_main, model1_all, type = "latex",
          keep = "dgtsp",
          title = "Changes in housing prices",
          dep.var.labels = "1970-80 (First Differences)",
          covariate.labels = "Mean TSPs (1/100)",
          add.lines = list(c("Main effects", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes")),
          label = "t:q1"
          title = "Relationship between changes in air pollution and housing prices",
          out = "Table_q1.tex")

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
whtage + whths  + whtincm + incage + inchs + inccoll +
urban2 + white2 + femal2 + age2 +
hs2 + coll2 + unemp2 + mnfcg2 + income2  | I(dgtsp / 100) | tsp7576, data = data
)
summary(iv_all)

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
          dep.var.labels = "A. Mean TSPs Changes",
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_firststage",
          title = "First stage relationship between regulation and air pollution changes",
          out = "Table_first_stage.tex")


stargazer(second_stage, second_stage_main, second_stage_all,
          type = "latex",
          keep = c("tsp7576"),
          dep.var.labels = c("B. Log Housing Changes"),
          covariate.labels = c("TSP nonattainment in 1975 or 1976"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_secondstage",
          title = "Reduced form relationship between regulation and housing prices",
          out = "Table_second_stage.tex")


stargazer(iv, iv_main,
          type = "latex",
          keep = "dgtsp",
          title = "IV Results using TSP7576 as instrumental variable",
          dep.var.labels = c("TSPs Nonattainment in 1975 or 1976"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_iv",
          out = "Table_IV.tex")

stargazer(iv75, iv75_main,
          type = "latex",
          keep = "dgtsp",
          title = "IV Results using TSP75 as instrumental variable",
          dep.var.labels = c("TSPs Nonattainment in 1975"),
          covariate.labels = c("Mean TSPs (1/100)"),
          add.lines = list(c("Main effects", "No", "Yes", "Yes", "No", "Yes", "Yes"),
                           c("Main and Polinomials", "No", "No", "Yes", "No", "No", "Yes")),
          label = "t:q3_iv75",
          out = "Table_IV_75.tex")

# Q4
# Replicate figure 4
data_clean <- data %>% 
    filter(!is.na(dlhouse) & !is.na(mtspgm74))
data_tp75_yes <- data_clean %>% 
        filter(tsp75 == 1)%>%
        filter(tsp74 <= 125)
data_tp75_no <- data_clean %>% filter(tsp75 != 1)

dlhouse_yes <- data_clean %>%
        filter(tsp75 == 0) %>%
        filter(mtspgm74 <= 125)

dlhouse_no <- data_clean %>%
    filter(tsp75 == 1) %>%
    filter(mtspgm74 <= 125)

KKK <- ksmooth(dlhouse_no$mtspgm74, dlhouse_no$dgtsp, "box", bandwidth = 1)


plot(KKK,  type = "l", col = "blue", lwd = 2,
    xlab = "Mean TSPs (1974)", ylab = "Log Housing Price Changes (1970-1980)",
    main = "Smoothed Relationship between TSPs and Housing Price Changes")

# Create the plot


# RD regressions



data_rd <- data 

data_rd <- data %>% filter(!(mtspgm74 < 75 & tsp75 == 1))
A  <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp75, data = data_rd%>% filter(mtspgm74 >= 50 & mtspgm74 <= 100))
summary(A)



A <- ivreg(dlhouse ~ I(dgtsp / 100) | tsp75, data = data_rd %>% filter(mtspgm74 >= 50 & mtspgm74 <= 100))
summary(A)
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
