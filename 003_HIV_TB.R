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

<<<<<<< HEAD
trAngola <- prev_uncontrolled_hiv('Angola')
trBangladesh <- prev_uncontrolled_hiv('Bangladesh')
trBotswana <- prev_uncontrolled_hiv('Botswana')
trBrazil <- prev_uncontrolled_hiv('Brazil')
trCambodia <- prev_uncontrolled_hiv('Cambodia')
trCameroon <- prev_uncontrolled_hiv('Cameroon')
trCentral_African_Republic <- prev_uncontrolled_hiv('Central African Republic')
trChad <- prev_uncontrolled_hiv('Chad')
trChina <- prev_uncontrolled_hiv('China')
trCongo <- prev_uncontrolled_hiv('Congo')
trDPRK <- prev_uncontrolled_hiv("Democratic People's Republic of Korea")
trDRC <- prev_uncontrolled_hiv("Democratic Republic of the Congo")
trEswatini <- prev_uncontrolled_hiv('Eswatini')
trEthiopia <- prev_uncontrolled_hiv('Ethiopia')
trGhana <- prev_uncontrolled_hiv('Ghana')
trGB <- prev_uncontrolled_hiv('Guinea-Bissau')
trIndia <- prev_uncontrolled_hiv('India')
trIndonesia <- prev_uncontrolled_hiv('Indonesia')
trKenya <- prev_uncontrolled_hiv('Kenya')
trLaos <- prev_uncontrolled_hiv('Laos')
trLesotho <- prev_uncontrolled_hiv('Lesotho')
trLiberia <- prev_uncontrolled_hiv('Liberia')
trMalawi <- prev_uncontrolled_hiv('Malawi')
trMozambique <- prev_uncontrolled_hiv('Mozambique')
trMyanmar <- prev_uncontrolled_hiv('Myanmar')
trNamibia <- prev_uncontrolled_hiv('Namibia')
trNigeria <- prev_uncontrolled_hiv('Nigeria')
trPakistan <- prev_uncontrolled_hiv('Pakistan')
trPNG <- prev_uncontrolled_hiv('Papua New Guinea')
trPhilippines <- prev_uncontrolled_hiv('Philippines')
trRepublicofKorea <- prev_uncontrolled_hiv('Republic of Korea')
trRussia <- prev_uncontrolled_hiv('Russian Federation')
trSierraLeone <- prev_uncontrolled_hiv('Sierra Leone')
trSouthAfrica <- prev_uncontrolled_hiv('South Africa')
trThailand <- prev_uncontrolled_hiv('Thailand')
trTanzania <- prev_uncontrolled_hiv('Tanzania')
trUganda <- prev_uncontrolled_hiv('Uganda')
trVietnam <- prev_uncontrolled_hiv('Vietnam')
trZambia <- prev_uncontrolled_hiv('Zambia')
trZimbabwe <- prev_uncontrolled_hiv('Zimbabwe')

trUganda
### generate the graphs!

# bell curve
grid.arrange(trAngola, trCameroon, trCongo, trEswatini, trKenya, trLesotho, trMalawi, trNamibia, trSierraLeone, trSouthAfrica, trTanzania, trZimbabwe)

# flat
grid.arrange(trBangladesh, trNigeria, trPNG, trGB)
=======
# without legend
prev_uncontrolled_hiv_no_legend <- function(country_name){
  prev_uncontrolled_hiv(country_name) + theme(legend.position = "none")
}
>>>>>>> b481efc23a98d28fd4f0bf0484a5d19c7e9f5e24

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
