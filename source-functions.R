library(gridExtra)
library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)
# to have only one legend for the entire plot grid
library(cowplot)

# master is all existing WHO data from 2000 to 2018 by country
master <- read.csv("who_ALL.csv")
# all_countries is a list of the 40 countries of interest
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon",
                   "Central African Republic","Chad","China","Congo",
                   "Democratic People's Republic of Korea",
                   "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana",
                   "Guinea-Bissau","India","Indonesia","Kenya",
                   "Lao People's Democratic Republic","Lesotho","Liberia","Malawi",
                   "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan",
                   "Papua New Guinea","Philippines","Republic of Korea",
                   "Russian Federation","Sierra Leone","South Africa","Thailand",
                   "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

# throw warning if input country is not in the 40 countries of interest 
throwWarning <- function(country_name)
{
  if (country_name %in% all_countries == FALSE) warning('The input country is not among the 40 countries of interest')
 
}
