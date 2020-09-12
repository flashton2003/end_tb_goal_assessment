# obtain TB incidence (per 100k) and TB number of cases by HIV status

# call source file
source("000_source-functions.R")

## needed for the {country_name} in the output tsv file name
library(glue)
## and for writing the tsv
library(readr)

######################### Data Processing #########################
######### WHO incidence data + WorldBank population data ##########

# select country, total TB incidence, and HIV+ TB incidence, with CIs by year
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi, e_inc_tbhiv_100k,  e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi) 
# change column names for clarity
master <- master %>% 
  dplyr::rename(total_inc = e_inc_100k, total_inc_lo = e_inc_100k_lo, total_inc_hi = e_inc_100k_hi,
                hiv_inc = e_inc_tbhiv_100k, hiv_inc_lo = e_inc_tbhiv_100k_lo, hiv_inc_hi = e_inc_tbhiv_100k_hi)

# master.hiv with only the 15 countries of interest
master.hiv <- master %>% filter(country %in% hiv_15)

# population data to be used for calculating number of cases
# pop.hiv with only the 15 countries of interest
pop.hiv <- pop %>% filter(country %in% hiv_15)

## check that there are 15 countries
## stopifnot will error if the expression is not true
stopifnot(length(unique(master.hiv$country)) == 15)
stopifnot(length(unique(pop.hiv$country)) == 15)


######################### TB INCIDENCE #########################

calc_ci_ratios <- function(df_country){
  ## add new columns with the ratio of the estimate to the high and low bounds
  df_country_ci <- df_country %>% mutate(hiv_lb_ratio = hiv_inc_lo / hiv_inc) %>% mutate(hiv_hb_ratio = hiv_inc_hi / hiv_inc)
  df_country_ci <- df_country_ci %>% mutate(nohiv_lb_ratio = nohiv_inc_lo / nohiv_inc) %>% mutate(nohiv_hb_ratio = nohiv_inc_hi / nohiv_inc)
  ## take averge of the last 5 years to obtain ratios 
  lo_tbhiv <- mean(tail(df_country_ci$hiv_lb_ratio, 5))
  hi_tbhiv <- mean(tail(df_country_ci$hiv_hb_ratio, 5))
  lo_nohiv_tb <- mean(tail(df_country_ci$nohiv_lb_ratio, 5))
  hi_nohiv_tb <- mean(tail(df_country_ci$nohiv_hb_ratio, 5))
  ## df with low and high bound ratios
  ci_ratios <- data.frame("lo_tbhiv" = lo_tbhiv, "hi_tbhiv" = hi_tbhiv, "lo_nohiv_tb" = lo_nohiv_tb, "hi_nohiv_tb" = hi_nohiv_tb)
  return(ci_ratios)
}

add_ci_to_predicted <- function(predicted_tb_inc, df_country){
  ci_ratios <- calc_ci_ratios(df_country)
  low_tbhiv_bound_ratio <- as.numeric(ci_ratios$lo_tbhiv)
  high_tbhiv_bound_ratio <- as.numeric(ci_ratios$hi_tbhiv)
  low_no_hiv_tb_bound_ratio <- as.numeric(ci_ratios$lo_nohiv_tb)
  high_no_hiv_tb_bound_ratio <- as.numeric(ci_ratios$hi_nohiv_tb)
  ## only want values for the period for which we rely on projections i.e. > 2018
  ## so, filter, add the projected CIs
  tmp_predicted_tb_inc <- predicted_tb_inc %>% filter(year > 2018)
  tmp_predicted_tb_inc <- tmp_predicted_tb_inc %>% 
    mutate(proj_tbhiv_ci_lo = hiv_inc * low_tbhiv_bound_ratio) %>% 
    mutate(proj_tbhiv_ci_hi = hiv_inc * high_tbhiv_bound_ratio) %>% 
    mutate(proj_no_hiv_tb_ci_lo = nohiv_inc * low_no_hiv_tb_bound_ratio) %>% 
    mutate(proj_no_hiv_tb_ci_hi = nohiv_inc * high_no_hiv_tb_bound_ratio)
  
  ## then combine back with predicted_tb_inc using a join
  predicted_tb_inc <- left_join(predicted_tb_inc, tmp_predicted_tb_inc)
  
  return(predicted_tb_inc)
}

# display df for an individual country only through 2035
get_one_country_df <- function(country_name){
  df_onecountry <- master.hiv %>% 
    filter(country == country_name) %>% 
    select(year, total_inc, total_inc_lo, total_inc_hi, hiv_inc, hiv_inc_lo, hiv_inc_hi)
  # add HIV negative (nohiv) TB incidence including low and high bound
  df_onecountry <- df_onecountry %>% 
    mutate(nohiv_inc = total_inc - hiv_inc) %>% 
    mutate(nohiv_inc_lo = total_inc_lo - hiv_inc_lo) %>% 
    mutate(nohiv_inc_hi = total_inc_hi - hiv_inc_hi)
  # returns df with 10 cols and 36 rows (2000-2035)
  return(df_onecountry)
}


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
  
  # create empty df with same columns from years 2019 to 2035
  df_preds <- data.frame(year = 2019:2035, total_inc = 0, hiv_inc = 0, nohiv_inc = 0)
  
  # populate df_preds from 2019 to 2035
  df_preds$total_inc <- as.numeric(predict(fit_total, df_preds, type = "response"))
  df_preds$hiv_inc <- as.numeric(predict(fit_hiv, df_preds, type = "response"))
  df_preds$nohiv_inc <- as.numeric(predict(fit_nohiv, df_preds, type = "response"))
  
  # add confidence intervals
  df_preds <- add_ci_to_predicted(df_preds, df_actual)
  
  # minimum incidence set to 10 per 100,000
  # replace all predicted values less than 10 with 10
  df_preds[] <- lapply(df_preds, function(x) ifelse(x<10, 10, x))
  
  # bind df_actual (2000-2018) together with df_preds (2019-2035)
  # View(df_actual)
  # View(df_preds)
  df_preds <- df_preds %>% 
    dplyr::rename(hiv_inc_lo = proj_tbhiv_ci_lo, hiv_inc_hi = proj_tbhiv_ci_hi, 
                  nohiv_inc_lo = proj_no_hiv_tb_ci_lo, nohiv_inc_hi = proj_no_hiv_tb_ci_hi)
  df_actual <- df_actual %>% select(-c(total_inc_lo, total_inc_hi))
  df_incidence <- rbind(df_actual, df_preds)
  
  return(df_incidence)
}

### validate to make sure predict_by_hiv numbers match with 002 YES!
  ### angola <- predict_inc("Angola")
  ### angola[nrow(angola), ] # returns 284, consistent with Table 2

  ### malawi <- predict_by_hiv("Malawi")
  ### malawi[nrow(malawi), ] # returns 48, consistent with Table 2

######################### TB NUMBER OF CASES #########################
################### columns E-G in toy file from PA ####################

# now that we know incidence of TB by HIV status 2000-2035, 
# we want the actual NUMBER OF TB CASES by HIV status 2000-2035.
# to do this, use population data
predict_numb <- function(country_name){
  
  # get incidence predictions of that country from predict_inc, 2000-2035
  inc_country <- predict_inc(country_name)
  
  # get population of that country, 2000-2035 (worldbank data)
  pop_country <- pop.hiv %>% filter(country == country_name) %>% select(year, population)
  
  # create new empty df 2000-2035
  df_number <- data.frame(year = 2000:2035, total_numb = 0, hiv_numb = 0, nohiv_numb = 0)
  
  df_number <- df_number %>%
    mutate(
      
      total_numb = as.numeric(inc_country$total_inc/100000)*as.numeric(pop_country$population),
      hiv_numb = as.numeric(inc_country$hiv_inc/100000)*as.numeric(pop_country$population),
      nohiv_numb = as.numeric(inc_country$nohiv_inc/100000)*as.numeric(pop_country$population),
      hiv_plus_nohiv = hiv_numb + nohiv_numb
      
  ) #%>% 
    # don't want two columns for "year" in final, so select all but year
    #select(total_numb, hiv_numb, nohiv_numb)

  return(df_number)
  
}


######################### CALCULATE EXCESS NUM CASES #########################

### Read in target

target <- read.delim("002_output_target_numbers.tsv", sep = "\t")

# want excess number of cases from 2020 to 2035
# num is the predicted number of cases (with predict_numb)
count_excess <- function(a_country_name, target, numb){
  country_target <- target %>% 
    filter(country_name == a_country_name) %>% 
    filter(year > 2019)
  numb <- numb %>% filter(year > 2019)
  # View(numb)
  # View(country_target)
  total_numb_predicted <- sum(numb$hiv_plus_nohiv)
  total_numb_target <- sum(country_target$target_num_cases_dfses)
  # View(total_numb_predicted)
  # View(total_numb_target)
  output <- data.frame(country_name = a_country_name, total_numb_predicted = total_numb_predicted, total_numb_target = total_numb_target, excess = total_numb_predicted - total_numb_target)
  write_tsv(output, glue('/Users/flashton/Dropbox/mtb/end_tb_goal_assessment/results/2020.09.06/{a_country_name}.excess_number_cases.tsv'))
}


######################################## 2020.09.12 JC ########################################
############## GENERATE GRAPH LIKE FIG. 3, NOW INCLUDING PROJECTIONS THRU 2035 ##############

tb_by_hiv_2035 <- function(country_name){

  full_inc_df <- predict_inc(country_name)
  
  p <- ggplot(full_inc_df, aes(year, y = value, colour = Incidence)) +
    geom_point(aes(y = total_inc, col = "Total")) +
    geom_point(aes(y = hiv_inc, col = "HIV infected")) +
    geom_point(aes(y = nohiv_inc, col = "HIV uninfected")) +
    #xlab("Year") + ylab("Incidence \nper 100k people") + 
    theme(axis.title = element_blank()) +
    ggtitle(country_name) + 
    # make legend horizontal in one row
    guides(colour = guide_legend(ncol = 1)) +
    # modify legend title
    labs(colour = "Population") +
    ylim(0, NA)
}
  
  
# without legend
tb_by_hiv_2035_no_legend <- function(country_name){
  tb_by_hiv_2035(country_name) + theme(legend.position = "none")
}

# obtain common legend
trAngola <- tb_by_hiv_2035("Angola")
legend <- get_legend(trAngola)

# generate graphs for the 15 countries of interest using lapply
g <- lapply(hiv_15, tb_by_hiv_2035_no_legend)
g15 <- do.call(grid.arrange, g)
plot <- plot_grid(g15, vjust = 1, scale = 1, ncol = 1, align = 'v', axis = 't')

# axes labels
y.grob <- textGrob("Incidence per 100k people", gp = gpar(col="black", fontsize=15), rot = 90)
x.grob <- textGrob("Year", gp = gpar(fontface="bold", col="black", fontsize=15))

# create tiff file
tiff(filename = "005_tb_hiv_2000_2035.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(arrangeGrob(plot, left = y.grob, bottom = x.grob))
dev.off()

# generate the common legend
tiff(filename = "005_tb_hiv_2000_2035_legend.tiff", width = 6.75, height = 8, units = "in", res = 300)
grid.arrange(legend)
dev.off()

################### COMBINE INCIDENCE AND NUMBER, 2000-2035 ###################
####################### ALL COLUMNS in toy file from PA #######################

table_main("Botswana")

# now, combine incidence and number into one table
# this generates ONE table that looks like the toy example sent by PA
table_main <- function(country_name){
  inc <- predict_inc(country_name)
  numb <- predict_numb(country_name)
  count_excess(country_name, target, numb)
  numb <- numb %>% select(total_numb, hiv_numb, nohiv_numb, hiv_plus_nohiv)
  country_df <- cbind(inc, numb)
  country_df <- country_df %>% add_column(country_name = country_name, .before = 0)
  graph_hiv_nonhiv_projections(country_df = country_df)
  write_tsv(country_df, glue('/Users/flashton/Dropbox/mtb/end_tb_goal_assessment/results/2020.09.06/{country_name}.hiv_and_non_hiv.tsv'))
  return(country_df)
}

lapply(hiv_15, table_main)

# just for Angola
table_main("Angola")
