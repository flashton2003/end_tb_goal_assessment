library(tidyverse)
#library(ggplot2)
library(knitr)
library(tibble)
library(ggpmisc)
library(gridExtra)

source("000_source-functions.R")

master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)

## get_r2 calculates the r2 for a linear regression of inc vs year
## it outputs a one row tibble with columns country name and r2
get_r2_and_coef <- function(country_name){
  
  # throw error if country_name not in master
  # source function
  throwWarning(country_name)

  one_country <- master %>% filter(country == country_name)
  linear_model <- lm(e_inc_100k ~ year, data = one_country)
  
  #print(summary(linear_model))
  ## this tryCatch function will run summary on the linear model
  ## and if summary returns a warning about the essentially perfect fit so summary
  ## unreliable, then r_sq is NaN
  ## also want to get the co-efficent which tells us about slope of line
  r_sq <- tryCatch(
    {
      r_sq <- summary(linear_model)$r.squared
    },
    warning = function(w){
      return(NaN)
    }
    )
  coefficient <- tryCatch(
    {
      coef <- summary(linear_model)$coefficients[2]
    },
    warning = function(w){
      return(NaN)
    }
  )
  
  # shorten country name to fit in graph/use vernacular names
  # source function
  country_name <- shorten_country_name(country_name)
  
  o <- tibble(country = country_name, r2 = r_sq, coef = coefficient)
  return(o)
}

# testing:
get_r2_and_coef("Bangladesh") # return NaN and NaN
get_r2_and_coef("Republic of Korea") # return values with "South Korea"
get_r2_and_coef("Switzerland") # return warning

plot_trend <- function(country_name, r2_res){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  # edit country name to be the vernacular name/shorter form
  country_name <- shorten_country_name(country_name)
  
  ## extract the r2 for that country from the r2_res tibble
  r_sq <- r2_res %>% filter(country == country_name)
  r_sq <- format(round(r_sq$r2, 2), nsmall = 2)
  my.formula <- y ~ x
  
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
    geom_smooth(method = "lm", se = TRUE, formula = my.formula, fill = "blue") +
    geom_point() +
    annotate("text", label = paste("R-squared = ", r_sq), x = Inf, y = Inf, vjust = 1, hjust = 1) +
    geom_ribbon(data = one_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    ggtitle(country_name) +
    xlab("Year") +
    ylab("Incidence per 100k")
  
  g 
}

plot_trend_no_ci <- function(country_name, r2_res){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  # edit country name to be the vernacular name/shorter form
  country_name <- shorten_country_name(country_name)
  
  ## extract the r2 for that country from the r2_res tibble
  r_sq <- r2_res %>% filter(country == country_name)
  r_sq <- format(round(r_sq$r2, 2), nsmall = 2)
  my.formula <- y ~ x
  
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
    geom_smooth(method = "lm", se = TRUE, formula = my.formula, fill = "blue") +
    geom_point() +
    annotate("text", label = paste("R-squared = ", r_sq), x = Inf, y = Inf, vjust = 1, hjust = 1) +
    ggtitle(country_name) +
    xlab("Year") +
    ylab("Incidence per 100k")
  
  g 
}

## here, we run get_r2_and_coef on everything in all_countries
## this outputs a list of tibbles, which we then merge with
## bind_rows
r2_res <- bind_rows(lapply(all_countries, get_r2_and_coef))
## kable from knitr library
kable(r2_res)
write_tsv(r2_res, '2020.05.06.40_countries_r2.tsv')

## generate plots for everything
all_plots <- lapply(all_countries, plot_trend, r2_res = r2_res)
do.call(grid.arrange, all_plots)

## generate plots with no CI to have a closer look at trend
## as the CIs are so broad that they mask the signal.

plot_trend_no_ci("Congo", r2_res)
plot_trend_no_ci("Democratic Republic of the Congo", r2_res)
plot_trend_no_ci("Guinea-Bissau", r2_res)
plot_trend_no_ci("Pakistan", r2_res)
plot_trend_no_ci("Philippines", r2_res)
plot_trend_no_ci("Sierra Leone", r2_res)
plot_trend_no_ci("Myanmar", r2_res)

plot_trend_no_ci("Liberia", r2_res)
plot_trend_no_ci("Angola", r2_res)
plot_trend_no_ci("Mozambique", r2_res)

trAngola <- plot_trend("Angola", r2_res = r2_res)
trBrazil <- plot_trend("Brazil", r2_res = r2_res)

# testing
grid.arrange(trAngola, trBrazil)
eg <- c("Angola","Brazil")
lp_eg <- lapply(eg, plot_trend, r2_res = r2_res)
do.call(grid.arrange, lp_eg)

## generate plots for countries which will take analysis forward for
hq_countries <- c("Angola","Brazil","Botswana","Cambodia","Cameroon","Chad","China", "Congo", "Democratic Republic of the Congo", "Eswatini","Ethiopia","Ghana", "Guinea-Bissau", "India","Indonesia","Kenya", "Laos","Lesotho","Liberia","Malawi", "Mozambique","Myanmar","Namibia", "Pakistan", "Philippines", "Republic of Korea", "Russian Federation", "Sierra Leone", "South Africa","Thailand", "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

recently_linear <- c('Republic of Korea', 'Congo', 'Russian Federation', 'South Africa', 'Myanmar', 'Kenya', 'United Republic of Tanzania', 'Eswatini', 'Lesotho', 'Namibia', 'Malawi')

hq_plots <- lapply(recently_linear, plot_trend, r2_res = r2_res)
do.call(grid.arrange, hq_plots)

###### generating figures

# Figure CWLD (14 countries)
linear_decrease_plots <- lapply(linear_decrease, plot_trend, r2_res = r2_res)
do.call(grid.arrange, linear_decrease_plots)

# Figure CWII (4 countries)
increasing_plots <- lapply(increasing, plot_trend, r2_res = r2_res)
do.call(grid.arrange, increasing_plots)

# Figure CWFI (4 countries)
flat_plots <- lapply(flat, plot_trend, r2_res = r2_res)
do.call(grid.arrange, flat_plots)

# Figure CWPITN (11 countries)
peak_in_00s_plots <- lapply(peak_in_00s, plot_trend, r2_res = r2_res)
do.call(grid.arrange, peak_in_00s_plots)

# Figure TIFOC (8 countries)
other_countries_plots <- lapply(other, plot_trend, r2_res = r2_res)
do.call(grid.arrange, other_countries_plots)

# Figure TIFOCNCI (8 countries)
other_countries_plots_no_ci <- lapply(other, plot_trend_no_ci, r2_res = r2_res)
do.call(grid.arrange, other_countries_plots_no_ci)