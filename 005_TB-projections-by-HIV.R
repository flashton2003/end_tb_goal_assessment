source("000_source-functions.R")

# select country, total TB incidence, and HIV+ TB incidence by year
master <- master %>% select(country, year, e_inc_100k, e_inc_tbhiv_100k) 
# change column names for clarity
master <- master %>% dplyr::rename(total_inc = e_inc_100k, hiv_inc = e_inc_tbhiv_100k)

# master.hiv with only the 15 countries of interest
master.hiv <- master %>% filter(country %in% hiv_15)

## check that there are 15 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master.hiv$country)) == 15)


# display df for an individual country only through 2035
get_one_country_df <- function(country_name){
  
  df_onecountry <- master %>% filter(country == country_name) %>% select(year, total_inc, hiv_inc)
  
  # add column for HIV negative (nohiv) TB incidence
  df_onecountry <- df_onecountry %>% mutate(nohiv_inc = total_inc - hiv_inc)
  
  # extend overall, HIV+, HIV- TB incidence to 2035
  # to_2035 <- data.frame(year = 2019:2035, 
  #                       total_inc = replicate(17, "NA"), 
  #                       hiv_inc = replicate(17, "NA"),
  #                       nohiv_inc = replicate(17, "NA"))
  # one_country <- rbind(df_onecountry, to_2035)
  
  one_country <- df_onecountry
  
  # returns df with 4 columns (year, total TB, HIV+ TB, HIV- TB) and 36 rows (2000-2035)
  return(one_country)
}

## TEST: get_one_country_df("Angola")

# display tb incidence from 2000 to 2035 by hiv status
# from 2000 to 2018 is WHO data; from 2019 to 2035 is predicted data
# predicted data is calculated from fit from year_start to 2018
predict_inc <- function(country_name){
  
  # get df of just that country (2000-2018 WHO reported data)
  df_actual <- get_one_country_df(country_name)
  
  # extract start year for projection, same year as for overall projection (in 002)
  year_start <- years[country_name]
  
  
  # fit models from year_start to 2018
  # row is the row number corresponding to year start
  row <- as.numeric(rownames(df_actual[df_actual$year == year_start,]))
  # end is the row number corresponding to year 2018
  end <- 19
  
  # for total TB incidence
  fit_total <- lm(total_inc ~ year, data = df_actual[row:end,])
  
  # for HIV+ TB incidence
  fit_hiv <- lm(hiv_inc ~ year, data = df_actual[row:end,])
  
  # for HIV- (nohiv) TB incidence
  fit_nohiv <- lm(nohiv_inc ~ year, data = df_actual[row:end,])
  
  
  # make empty df with same columns from years 2019 to 2035
  df_preds <- data.frame(year = 2019:2035, total_inc = 0, hiv_inc = 0, nohiv_inc = 0)
  
  # populate df_preds from 2019 to 2035
  df_preds$total_inc <- as.numeric(predict(fit_total, df_preds, type = "response"))
  df_preds$hiv_inc <- as.numeric(predict(fit_hiv, df_preds, type = "response"))
  df_preds$nohiv_inc <- as.numeric(predict(fit_nohiv, df_preds, type = "response"))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  df_preds[] <- lapply(df_preds, function(x) ifelse(x<10, 10, x))
  
  
  # bind df_country (2000-2018) together with df_preds (2019-2035)
  df_country <- rbind(df_actual, df_preds)
  
  
  # insert predicted values from 2019 to 2035 to df_pred_inc
  # df_pred_inc <- data.frame(year = 2019:2035, predict_value = 0)
  # predicted_tb_inc$predict_value <- as.numeric(predict(fit, predicted_tb_inc, type = "response"))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  # df[] <- lapply(df, function(x) ifelse(x<10, 10, x))
  # predicted_tb_inc[] <- lapply(predicted_tb_inc, function(x) ifelse(x<10, 10, x))
  
  return(df_country)
}

### validate to make sure predict_by_hiv numbers match with 002 YES!
  ### angola <- predict_inc("Angola")
  ### angola[nrow(angola), ] # returns 284, consistent with Table 2

  ### malawi <- predict_by_hiv("Malawi")
  ### malawi[nrow(malawi), ] # returns 48, consistent with Table 2


  # 2020-09-04 next steps:
  # using combination of rbind and cbind get columns A-D in toy example table
  # return this. then feed in as input to another function that uses POPULATION data 
  # to then get ACTUAL NUMBER OF CASES, columns E-G (will have to mutate to add these columns)
  # this NUMBER OF CASES function should return final table

### validate to make sure predict_by_hiv numbers match with 002
  ### angola <- predict_by_hiv("Angola")
  ### angola[nrow(angola), ] # returns 284, consistent with Table 2

  ### malawi <- predict_by_hiv("Malawi")
  ### malawi[nrow(malawi), ] # returns 48, consistent with Table 2

