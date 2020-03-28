# Setup ----
#' Clear the environment and load the plm and the stargazer packages.
# Introduction ----


## Example Call of `lm` with Eurostat Data ----
eur_data <- read.csv2("data/eurostat_data.csv")

m1 <-
  formula(unemp_workagepop_t ~ gdp_gr + inv_per_empl + immigration_t,
          subset = year == 2014)
model <- lm(formula = m1,
            data = eur_data)


## Output of `lm` ----

#' Lets look up our coefficients $\beta$, 
#' fitted values $\hat{y}$ and OLS residuals $\varepsilon$

model$coefficients

model$fitted.values[1:7] # first 7 fitted values

model$residuals[1:7] # first 7 residuals

#' We can visualise the results very simply with `hist` or `plot`:
hist(model$residuals, breaks = 30)

hist(model$fitted.values, breaks = 30)

## Output of `lm` with the `summary()` function ----
summary(model)


## Display and Export Tables with `stargazer()`----

stargazer::stargazer(model, type = "text")

### Export Stargazer Output to File ----

stargazer::stargazer(model,
                     type = "html",
                     out = "model.html")

## Compare different Models ----

model2 <- lm(unemp_workagepop_t ~ gdp_gr,
             data = eur_data)

stargazer::stargazer(model, model2,
                     type = "html")
