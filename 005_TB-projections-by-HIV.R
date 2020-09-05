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
  to_2035 <- data.frame(year = 2019:2035, 
                        total_inc = replicate(17, "NA"), 
                        hiv_inc = replicate(17, "NA"),
                        nohiv_inc = replicate(17, "NA"))
  one_country <- rbind(df_onecountry, to_2035)
  
  # returns df with 4 columns (year, total TB, HIV+ TB, HIV- TB) and 36 rows (2000-2035)
  return(one_country)
}

## TEST: get_one_country_df("Angola")

# predict tb incidence projecting to 2035
predict_incidence <- function(year_start, fit){
  
  # insert predicted values to df
  predicted_tb_inc <- data.frame(year = year_start:2035, predict_value = 0)
  predicted_tb_inc$predict_value <- as.numeric(predict(fit, predicted_tb_inc, type = "response"))
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  predicted_tb_inc[] <- lapply(predicted_tb_inc, function(x) ifelse(x<10, 10, x))
  
  return(predicted_tb_inc)
}


predict_by_hiv <- function(country_name){
  
  # get df of just that country
  df_country <- get_one_country_df(country_name)
  
  # extract start year for projection, same year as for overall projection (in 002)
  year_start <- years[country_name]
  
  # rows begin at year_start
  row <- as.numeric(rownames(df_country[df_country$year == year_start,]))
  
  # fit models from year_start to 2018
  ## end is the row number corresponding to year 2018
  end <- 19
  ## for total TB incidence
  fit_total <- lm(total_inc ~ year, data = df_country[row:19,])
  ## for HIV+ TB incidence
  fit_hiv <- lm(hiv_inc ~ year, data = df_country[row:19,])
  ## for HIV- (nohiv) TB incidence
  fit_nohiv <- lm(nohiv_inc ~ year, data = df_country[row:19,])
  
  # predict incidence
  pred_total <- predict_incidence(year_start, fit_total) %>% dplyr::rename(pred_total = predict_value)
  pred_hiv <- predict_incidence(year_start, fit_hiv) %>% dplyr::rename(pred_hiv = predict_value)
  pred_nohiv <- predict_incidence(year_start, fit_nohiv) %>% dplyr::rename(pred_nohiv = predict_value)
  
  # 2020-09-04 next steps:
  # using combination of rbind and cbind get columns A-D in toy example table
  # return this. then feed in as input to another function that uses POPULATION data 
  # to then get ACTUAL NUMBER OF CASES, columns E-G (will have to mutate to add these columns)
  # this NUMBER OF CASES function should return final table
  
  
  ### validate to make sure numbers match with 002
    ### return(pred_total)
}

### validate to make sure numbers match with 002
  ### angola <- predict_by_hiv("Angola")
  ### angola[nrow(angola), ] # returns 284, consistent with Table 2

  ### malawi <- predict_by_hiv("Malawi")
  ### malawi[nrow(malawi), ] # returns 48, consistent with Table 2

