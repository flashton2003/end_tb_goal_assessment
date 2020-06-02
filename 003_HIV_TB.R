library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

source("000_source-functions.R")

# arv: HIV antiretroviral coverage (in %) by country from 2000 to 2018
# prev: HIV prevalence (in %) by country from 2000 to 2018

# calculating prevalence of uncontrolled HIV (%, unc_hiv) per year

prev_uncontrolled_hiv <- function(country_name){

  country_arv <- arv %>% filter(country == country_name)
  hiv_prevalence <- (prev %>% filter(country == country_name))$hiv_prevalence
  
  # cbind is column bind, the data frame gets fatter, binds with row name as key, needs the same dimensions and row names
  # rbind is row bind, the data frame gets taller, binds with col name as key, needs same dimensions
  arv.prev <- cbind(country_arv, hiv_prevalence)
  
  arv.prev <- arv.prev %>% mutate(unc_hiv = ((100 - hiv_coverage)/100) * hiv_prevalence)
  
  # code that will show the legend:
      # p <- ggplot(arv.prev, aes(year, y = value, color = variable)) +
        # geom_point(aes(y = hiv_prevalence, col = "HIV Prevalence (%)")) +
        # geom_point(aes(y = unc_hiv, col = "Uncontrolled HIV (%)")) +
        # xlab("Year") + ylab("% of Total Population") + ggtitle(country_name)
  
  # will not display legend:
  p <- ggplot(arv.prev, aes(year, y = value, color = variable)) +
  geom_point(aes(y = hiv_prevalence, col = "HIV Prevalence (%)"), show.legend = FALSE) +
  geom_point(aes(y = unc_hiv, col = "Uncontrolled HIV (%)"), show.legend = FALSE) +
  xlab("Year") + ylab("% of Total Population") + ggtitle(country_name)
  
  p + theme(legend.position = "none")
  
  p

}


### generate graphs for the 13 countries in hpauh

g <- lapply(hpauh, prev_uncontrolled_hiv)
g1 <- lapply(g, )
do.call(grid.arrange, g)
