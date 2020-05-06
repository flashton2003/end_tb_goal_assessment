library(tidyverse)
#library(ggplot2)
library(tibble)
library(ggpmisc)
library(gridExtra)

master <- read.csv("who_ALL.csv")
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon",
                   "Central African Republic","Chad","China","Congo",
                   "Democratic People's Republic of Korea",
                   "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana",
                   "Guinea-Bissau","India","Indonesia","Kenya",
                   "Laos","Lesotho","Liberia","Malawi",
                   "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan",
                   "Papua New Guinea","Philippines","Republic of Korea",
                   "Russian Federation","Sierra Leone","South Africa","Thailand",
                   "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")


## get_r2 calculates the r2 for a linear regression of inc vs year
## it outputs a one row tibble with columns country name and r2
get_r2 <- function(country_name){
 
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
  linear_model <- lm(e_inc_100k ~ year, data = one_country)
  ## this tryCatch function will run summary on the linear model
  ## and if summary returns a warning about the essentially perfect fit so summary
  ## unreliable, then r_sq is NaN
  r_sq <- tryCatch(
    {
      r_sq <- summary(linear_model)$r.squared
    },
    warning = function(w){
      return(NaN)
    }
    )
    
  o <- tibble(country_name = country_name, r2 = r_sq)
  return(o)
}

get_r2("Angola")

## here, we run get_r2 on everything in all_countries
## this outputs a list of tibbles, which we then merge with
## bind_rows
r2_res <- bind_rows(lapply(all_countries, get_r2))


plot_trend <- function(country_name){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  my.formula <- y ~ x
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
  geom_smooth(method = "lm", se = TRUE, formula = my.formula, fill = "blue") +
  stat_poly_eq(formula = my.formula, aes(label = ..rr.label..), parse = TRUE) +
  geom_point() +
  geom_ribbon(data = one_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
  ggtitle(country_name) +
  xlab("Year") +
  ylab("Incidence per 100,000 people")
  g 
}

## generate plots for everything in 
all_plots <- lapply(all_countries, plot_trend)

do.call(grid.arrange, all_plots)


