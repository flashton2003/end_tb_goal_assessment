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
  
  # throw warning if country_name not in master
  throwWarning(country_name)

  country_arv <- arv %>% filter(country == country_name)
  hiv_prevalence <- (prev %>% filter(country == country_name))$hiv_prevalence
  
  # cbind is column bind, the data frame gets fatter, binds with row name as key, needs the same dimensions and row names
  # rbind is row bind, the data frame gets taller, binds with col name as key, needs same dimensions
  arv.prev <- cbind(country_arv, hiv_prevalence)
  
  arv.prev <- arv.prev %>% mutate(unc_hiv = ((100 - hiv_coverage)/100) * hiv_prevalence)
  
  # shorten country name to fit in graph/use vernacular names
  country_name <- shorten_country_name(country_name)
  
  # display graph with the legend:
  p <- ggplot(arv.prev, aes(year, y = value, colour = variable)) +
      geom_point(aes(y = hiv_prevalence, col = "Total")) +
      geom_point(aes(y = unc_hiv, col = "Uncontrolled")) +
      xlab("Year") + ylab("% Population") + ggtitle(country_name) +
      # make legend horizontal in one row
      guides(colour = guide_legend(nrow = 1)) +
      # modify legend title
      labs(colour = "HIV Prevalence (%)")
  
  p

}

# without legend
prev_uncontrolled_hiv_no_legend <- function(country_name){
  prev_uncontrolled_hiv(country_name) + theme(legend.position = "none")
}

# obtain common legend
trAngola <- prev_uncontrolled_hiv("Angola")
legend <- get_legend(trAngola)

# generate graphs for the 13 countries of interest using lapply
g <- lapply(hiv_13, prev_uncontrolled_hiv_no_legend)
g13 <- do.call(grid.arrange, g)

# now add the common legend to bottom
g_legend <- plot_grid(g13, legend, rel_heights = c(1, .1), ncol = 1, 
                   align = 'v', axis = 'c')
g_legend
