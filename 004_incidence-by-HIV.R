library(gridExtra)
library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

source("000_source-functions.R")

# Plot TB incidence, TB incidence HIV negative, and TB incidence HIV positive on one graph for each country with high HIV burden

# process TB incidence data
master <- master %>% select(country, year, e_inc_100k, e_tbhiv_prct, e_inc_tbhiv_100k)

tb_by_hiv <- function(country_name){
  
  # throw warning if country_name not in master
  throwWarning(country_name)
  
  # calculate HIV negative TB incidence using known values
  # add calculated HIV negative TB to master sheet
  master <- master %>% 
    filter(country == country_name) %>% 
    mutate(tb_hiv_neg = e_inc_100k - e_inc_tbhiv_100k)
  
  # shorten country name to fit in graph/use vernacular names
  country_name <- shorten_country_name(country_name)
  
  # this code gives the legend for each graph:
  p <- ggplot(master, aes(year, y = value, colour = Incidence)) +
    geom_point(aes(y = e_inc_100k, col = "Total")) +
    geom_point(aes(y = e_inc_tbhiv_100k, col = "HIV infected")) +
    geom_point(aes(y = tb_hiv_neg, col = "HIV uninfected")) +
    xlab("Year") + ylab("TB Incidence \n (per 100k)") + ggtitle(country_name) + 
    # make legend horizontal in one row
    guides(colour = guide_legend(nrow = 1)) +
    # modify legend title
    labs(colour = "TB Incidence")
}

# without legend
tb_by_hiv_no_legend <- function(country_name){
  tb_by_hiv(country_name) + theme(legend.position = "none")
}

# obtain common legend
trAngola <- tb_by_hiv("Angola")
legend <- get_legend(trAngola)

# generate graphs for the 13 countries of interest using lapply
g <- lapply(hiv_13, tb_by_hiv_no_legend)
g13 <- do.call(grid.arrange, g)

# now add the common legend to bottom
g_legend <- plot_grid(g13, legend, rel_heights = c(1, .1), ncol = 1, 
                      align = 'v', axis = 'c')
g_legend