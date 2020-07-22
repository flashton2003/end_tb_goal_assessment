library(tidyverse)
library(gapminder)
library(dplyr)
library(ggfortify)
library(ggplot2)
library(gridExtra)

# in addition to country name, also input year_start
# year_start is the year in which incidence started decreasing linearly
# country names are too long to fit in graph title
shorten_country_name <- function(country_name){
  if (country_name == "Democratic Republic of the Congo"){
    country_name = "DR Congo"
  }
  if (country_name == "Central African Republic"){
    country_name = "Central Af. Republic"
  }
  if (country_name == "United Republic of Tanzania"){
    country_name = "Tanzania"
  }
  if (country_name == "Russian Federation"){
    country_name = "Russia"
  }
  return(country_name)
}

read_in_pop <- function(pop_handle){
  # process population for each of the 40 countries for years 2000-2035
  # store in dataframe pop_df
  pop <- read.csv(pop_handle) %>% filter(country %in% all_countries)
  keycol <- 'year'
  valuecol <- 'population'
  gathercols <- colnames(pop)[2:37]
  pop <- gather_(pop, keycol, valuecol, gathercols)
  # get rid of the "X" (change X2001 to 2001)
  pop$year <- as.numeric(substr(pop$year, 2, 5))
  return(pop)
}

predict_tb_inc <- function(year_start, fit){
  # insert predicted values to df
  predicted_tb_inc <- data.frame(year = seq(year_start, 2035, 0.01), predict_value = 0)
    predicted_tb_inc$predict_value <- as.numeric(predict(fit, predicted_tb_inc, type = "response"))
  
  # set 10 as minimum for TB incidence
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

add_ci_to_predicted <- function(predicted_tb_inc, df_country){
  ## add new columns with the ratio of the estimate to the high and low bounds
  df_country <- df_country %>% mutate(lb_ratio = e_inc_100k_lo / e_inc_100k) %>% mutate(hb_ratio = e_inc_100k_hi / e_inc_100k)
  ## take averge of the last 5 years  
  low_bound_ratio <- mean(tail(df_country$lb_ratio, 5))
  high_bound_ratio <- mean(tail(df_country$hb_ratio, 5))
  
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
 
 
model_main <- function(country_name){
  ## need to get the year from the name, as mapply not working
  ## not ideal to have this specified within the function
  years <- c(2011, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2010, 2000, 2012, 2015, 2000, 2000, 2005, 2000, 2000, 2010, 2000, 2007, 2013, 2015, 2012, 2009, 2011, 2000, 2012, 2000, 2016, 2011, 2009, 2009, 2016, 2000, 2012, 2006, 2000, 2000, 2000)
  names(years) <- c("Angola", "Bangladesh", "Botswana", "Brazil", "Cambodia", "Cameroon", "Central African Republic", "Chad", "China", "Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Eswatini", "Ethiopia", "Ghana", "Guinea-Bissau", "India", "Indonesia", "Kenya", "Laos", "Lesotho", "Liberia", "Malawi", "Mozambique", "Myanmar", "Namibia", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "Republic of Korea", "Russian Federation", "Sierra Leone", "South Africa", "Thailand", "Uganda", "United Republic of Tanzania", "Vietnam", "Zambia", "Zimbabwe")
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
 
  ## add a new function which adds teh error bars on prediction
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
  pop <- read_in_pop("population_ALL.csv")
  pop_country <- pop %>% filter(country == country_name)
  
  pred3 <- pred2 %>% 
    mutate(pop = pop_country$population)
  
  # calculate the number of cases each year under real data model
  # this is the per 100000 rate multiplied by the population
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
  
  # graph modified linear model, include the dataframe showing
  # extra number of cases in the graph
  trend <- ggplot() + geom_point(data = df_country, aes(x = year, y = e_inc_100k)) +
    geom_point(data = df_target, aes(x = year, y = num), colour = "#7CAE00") +
    geom_line(aes(x = year, y = predict_value), data = predicted_tb_inc, color = "#F8766D") +
    geom_ribbon(data = df_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    geom_ribbon(data = predicted_tb_inc, aes(x = year, ymin=proj_ci_lo, ymax = proj_ci_hi, fill = "#F8766D"), alpha = 0.3) +
    xlab("Year") + ylab("Incidence of TB \n (per 100,000)") + 
    ggtitle(country_name) + 
    annotation_custom(tableGrob(extra_cases, rows = NULL), xmin = 2025, ymin = (max_inc_100k - (0.3*range_inc))) + 
    theme(legend.position = "none") 
  
  trend
  
}

model_main("Cambodia")



# processing the data
# store only the data of 40 countries of interest into master
master <- read.csv("who_ALL.csv")
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon", "Central African Republic","Chad","China","Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana", "Guinea-Bissau","India","Indonesia","Kenya", "Laos","Lesotho","Liberia","Malawi", "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan", "Papua New Guinea","Philippines","Republic of Korea", "Russian Federation","Sierra Leone","South Africa","Thailand", "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

master <- master %>% filter(country %in% all_countries)

## check -- should be 40 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master$country)) == 40)

#population <- read_in_pop("population_ALL.csv")

## Model for all countries

## tried to vectorise model function with mapply, but can't
## get grid.arrange working on the output
## got it working with lapply instead, but not ideal as the start dates
## had to go inside the function.

all_countries_projection <- lapply(all_countries, model_main)
do.call(grid.arrange, all_countries_projection)

