library(tidyverse)
library(gapminder)
library(dplyr)
library(ggfortify)
library(ggplot2)
library(gridExtra)

source("000_source-functions.R")

# in addition to country name, also input year_start
# year_start is the year in which incidence started decreasing linearly

predict_tb_inc <- function(year_start, fit){
  # insert predicted values to df
  predicted_tb_inc <- data.frame(year = seq(year_start, 2035, 0.01), predict_value = 0)
  predicted_tb_inc$predict_value <- as.numeric(predict(fit, predicted_tb_inc, type = "response"))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  predicted_tb_inc[] <- lapply(predicted_tb_inc, function(x) ifelse(x<10, 10, x))
  return(predicted_tb_inc)
}

calc_target <- function(df_country, one_country){
  # model the target decline with End TB goal with benchmarks to 2035
  num_2015 <- as.integer(df_country$e_inc_100k[one_country$year == 2015])
  
  df_target <- data.frame("year" = c(2015, 2020, 2025, 2030, 2035), 
                          "num" = c(num_2015, 0.80*num_2015, 
                                    0.50*num_2015, 0.20*num_2015, 0.10*num_2015))
  
  # minimum incidence set to 10 per 100,000
  df_target[] <- lapply(df_target, function(x) ifelse(x<10, 10, x))
  
  # nonlinear, polynomial regression for target decline
  fit.target <- lm(num ~ poly(year, 5, raw = TRUE), data = df_target)
  o <- list(df_target = df_target, fit.target = fit.target)
  return(o)
}

get_one_country_df <- function(df_country){
  to_2035 <- data.frame(year=2019:2035, e_inc_100k= replicate(17, "NA"))
  # need to do this as doesn't like the rbind with e_inc_100k_lo, e_inc_100k_hi
  tmp_df_country <- df_country %>% select(year, e_inc_100k)
  one_country <- rbind(tmp_df_country, to_2035)
  return(one_country)
}

calc_ci_ratios <- function(df_country){
  ## add new columns with the ratio of the estimate to the high and low bounds
  df_country_ci <- df_country %>% mutate(lb_ratio = e_inc_100k_lo / e_inc_100k) %>% mutate(hb_ratio = e_inc_100k_hi / e_inc_100k)
  ## take averge of the last 5 years to obtain ratios 
  lo <- mean(tail(df_country_ci$lb_ratio, 5))
  hi <- mean(tail(df_country_ci$hb_ratio, 5))
  ## df with low and high bound ratios
  ci_ratios <- data.frame("lo" = lo, "hi" = hi)
  return(ci_ratios)
}
  
add_ci_to_predicted <- function(predicted_tb_inc, df_country){
  ci_ratios <- calc_ci_ratios(df_country)
  low_bound_ratio <- as.numeric(ci_ratios$lo)
  high_bound_ratio <- as.numeric(ci_ratios$hi)
  ## only want values for the period for which we rely on projections i.e. > 2018
  ## so, filter, add the projected CIs
  tmp_predicted_tb_inc <- predicted_tb_inc %>% filter(year > 2018)
  tmp_predicted_tb_inc <- tmp_predicted_tb_inc %>% mutate(proj_ci_lo = predict_value * low_bound_ratio) %>% mutate(proj_ci_hi = predict_value * high_bound_ratio)
  
  ## and then combine back with predicted_tb_inc using a join
  predicted_tb_inc <- left_join(predicted_tb_inc, tmp_predicted_tb_inc)
  #print(df_country)
  print(tail(predicted_tb_inc))
  return(predicted_tb_inc)
}

# project TB incidence through 2035
model_main <- function(country_name){
  
  # throw warning if input country is not one of the 40 of interest
  throwWarning(country_name)
  
  # the start year of each of the 40 countries specified in source function
  # as vector "years" and "names"
  
  # select country, year, TB incidence, population
  df_country <- master %>% filter(country == country_name) %>% 
    select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
  year_start <- years[country_name]
  
  # row begins at year_start
  row <- as.numeric(rownames(df_country[df_country$year == year_start,]))
  
  # fit model from year_start to 2017
  fit <- lm(e_inc_100k ~ year, data = df_country[row:nrow(df_country),])
  
  # predict tb inc to 2035 based on lm
  ## @Jaeyoon - seem to predict to 2035 twice? once inside this function, and once for pred1
  ## do we need both of these, or can we only do this once?
  predicted_tb_inc <- predict_tb_inc(year_start, fit)
  
  ## add a new function which adds error bars on prediction
  ## it needs to take in 1) predicted_tb_inc 2) tb_inc and confidence intervals
  ## for the last 5 years, get the ratio of the estimate to hte high and hte low CI, take teh average of this over 5 years
  ## then, make new columns, predicting the bounds based on the average.
  predicted_tb_inc <- add_ci_to_predicted(predicted_tb_inc, df_country)
  
  ## get df for one country
  one_country <- get_one_country_df(df_country)
  
  ## calculate the targets
  target_output  <- calc_target(df_country, one_country)
  df_target <- target_output$df_target
  print(df_target)
  fit.target <- target_output$fit.target
  
  # add predicted incidence based on linear regression model, through year 2035
  pred1 <- one_country %>% 
    mutate(pred_num_100k = predict(fit, newdata = one_country, type = "response"))
  pred1[] <- lapply(pred1, function(x) ifelse(x<10, 10, x))
  
  #new.df <- data.frame(year = pred1$year)
  
  # add target TB incidence for each year
  # keep in mind: only relevant for 2015 and onwards
  pred2 <- pred1 %>% 
    mutate(target_100k = predict(fit.target, pred1))
  
  # population
  pop_country <- pop %>% filter(country == country_name)
  
  pred3 <- pred2 %>% 
    mutate(pop = pop_country$population)
  
  # calculate the number of cases each year under real data model
  # this is the per 100,000 rate multiplied by the population
  pred_num_cases_df <- pred3 %>% mutate(pred_cases = (as.numeric(pred3$pred_num_100k)/100000)*as.numeric(pred3$pop))
  
  # do the same for the target model
  target_num_cases_df <- pred_num_cases_df %>% 
    mutate(target_num_cases_dfses = (pred_num_cases_df$target_100k/100000)*as.numeric(pred_num_cases_df$pop))
  
  # take the difference to find number of extra cases
  with_diff_df <- target_num_cases_df %>% mutate(diff = as.numeric(target_num_cases_df$pred_cases) - as.numeric(target_num_cases_df$target_num_cases))
  
  year_diffs <- with_diff_df %>% select(year, diff) %>% filter(year > 2019)
  
  # calculate the extra number of cases from 2020 to 2035:
  extra_cases <- year_diffs %>% 
    summarize(extra_cases = as.integer(sum(diff)))
  ## this is a hack to get the number of extra cases out of the script
  ## if each part of the script were a function, it would be easy to gather up these data
  ## for all the coutnries and write one file
  write_tsv(data.frame(c(country_name, extra_cases)), paste('extra_cases', country_name))
  # return(list(trend, tot_extra_num_cases_2020_2035))
  
  # max incidence value
  max_inc_100k = max(df_country$e_inc_100k, na.rm = TRUE)
  # min incidence value
  min_inc_100k = min(df_country$e_inc_100k, na.rm = TRUE)
  # difference
  range_inc = max_inc_100k - min_inc_100k  
  country_name <- shorten_country_name(country_name)
  
  # country name to be in vernacular form/shortened to fit in graph
  shorten_country_name(country_name)
  
  # graph modified linear model, include the dataframe showing
  # extra number of cases in the graph
  trend <- ggplot() + geom_point(data = df_country, aes(x = year, y = e_inc_100k)) +
    geom_point(data = df_target, aes(x = year, y = num), colour = "red", size = 2) +
    geom_line(aes(x = year, y = predict_value), data = predicted_tb_inc, colour = "blue", size = 1.2) +
    geom_ribbon(data = df_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    geom_ribbon(data = predicted_tb_inc, aes(x = year, ymin=proj_ci_lo, ymax = proj_ci_hi), fill = "blue", alpha = 0.3) +
    xlab("Year") + ylab("Incidence \n per 100k people") + 
    ggtitle(country_name) + 
    annotation_custom(tableGrob(extra_cases, cols = NULL, rows = NULL, theme = ttheme_minimal(base_size = 8)), xmin = 2022, ymin = (max_inc_100k - (0.2*range_inc))) + 
    theme(legend.position = "none", axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10))
  
  # previous y-axis was: ylab("Incidence of TB \n (per 100,000)")
  
  trend
  
}

# test
model_main("Cambodia")
model_main("Republic of Korea")
model_main("Nigeria")

# process master
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
master <- master %>% filter(country %in% all_countries)

## check -- should be 40 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master$country)) == 40)

## Model for all countries
all_countries_projection <- lapply(all_countries, model_main)
# generate figures in 2 pages (20 graphs/page) with the specified # rows and columbs
all <- marrangeGrob(all_countries_projection, nrow = 4, ncol = 5)
all
