# Functions

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