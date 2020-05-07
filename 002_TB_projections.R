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

model <- function(country_name){
  ## need to get the year from the name, as mapply not working
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
  
  # insert predicted values to df
  regression <- data.frame(year = seq(year_start, 2035, 0.01), predict_value = 0)
  regression$predict_value <- as.numeric(predict(fit, regression, type = "response"))
  
  # set 10 as minimum for TB incidence
  # replace all predicted values less than 10 with 10
  regression[] <- lapply(regression, function(x) ifelse(x<10, 10, x))
  
  # need to do this as doesn't like the rbind with e_inc_100k_lo, e_inc_100k_hi
  to_2035 <- data.frame(year=2019:2035, e_inc_100k= replicate(17, "NA"))
  tmp_df_country <- df_country %>% select(year, e_inc_100k)
  one_country <- rbind(tmp_df_country, to_2035)
  
  # add predicted incidence based on linear regression model, through year 2035
  pred1 <- one_country %>% 
    mutate(pred_num_100k = predict(fit, newdata = one_country, type = "response"))
  pred1[] <- lapply(pred1, function(x) ifelse(x<10, 10, x))
  
  # model the target decline with End TB goal with benchmarks to 2035
  num_2015 <- as.integer(df_country$e_inc_100k[one_country$year == 2015])
  
  df_target <- data.frame("year" = c(2015, 2020, 2025, 2030, 2035), 
                          "num" = c(num_2015, 0.80*num_2015, 
                                    0.50*num_2015, 0.20*num_2015, 0.10*num_2015))
  
  # nonlinear, polynomial regression for target decline
  fit.target <- lm(num ~ poly(year, 5, raw = TRUE), data = df_target)
  
  new.df <- data.frame(year = pred1$year)
  
  # add target TB incidence for each year
  # keep in mind: only relevant for 2015 and onwards
  pred2 <- pred1 %>% 
    mutate(target_100k = predict(fit.target, pred1))
  
  # population
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
    geom_line(aes(x = year, y = predict_value), data = regression, color = "#F8766D") +
    geom_ribbon(data = df_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
    xlab("Year") + ylab("Incidence of TB \n (per 100,000)") + 
    ggtitle(country_name) + 
    annotation_custom(tableGrob(extra_cases, rows = NULL), xmin = 2025, ymin = (max_inc_100k - (0.3*range_inc)))
  
  trend
  
}





# processing the data
# store only the data of 40 countries of interest into master
master <- read.csv("who_ALL.csv")
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon", "Central African Republic","Chad","China","Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana", "Guinea-Bissau","India","Indonesia","Kenya", "Laos","Lesotho","Liberia","Malawi", "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan", "Papua New Guinea","Philippines","Republic of Korea", "Russian Federation","Sierra Leone","South Africa","Thailand", "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

master <- master %>% filter(country %in% all_countries)

## check -- should be 40 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master$country)) == 40)

# process population for each of the 40 countries for years 2000-2035
# store in dataframe pop_df
pop <- read.csv("population_ALL.csv") %>% filter(country %in% all_countries)
keycol <- 'year'
valuecol <- 'population'
gathercols <- colnames(pop)[2:37]
pop <- gather_(pop, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
pop$year <- as.numeric(substr(pop$year, 2, 5))

## Model for all countries

## tried to vectorise model function with mapply, but can't
## get grid.arrange working on the output
## got it working with lapply instead, but not ideal as the start dates
## had to go inside the function.

grid.arrange(trAngola)



all_countries_projection <- lapply(all_countries, model)
model("Malawi")

do.call(grid.arrange, all_countries_projection)


##

trAngola <- model("Angola")
trZimbabwe <- model("Zimbabwe")
trBangladesh
