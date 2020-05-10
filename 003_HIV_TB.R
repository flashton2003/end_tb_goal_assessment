library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)

# store the HIV coverage data in dataframe arv (antiretroviral therapy)
master.arv <- read.csv("hiv_arv_coverage.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_coverage'
gathercols <- colnames(master.arv)[2:20]
arv <- gather_(master.arv, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
arv$year <- as.numeric(substr(arv$year, 2, 5))

# store the HIV prevalence data in dataframe prev
master.prev <- read.csv("hiv_prev_ALL.csv")
# tidy format
keycol <- 'year'
valuecol <- 'hiv_prevalence'
gathercols <- colnames(master.prev)[2:20]
prev <- gather_(master.prev, keycol, valuecol, gathercols)
# get rid of the "X" (change X2001 to 2001)
prev$year <- as.numeric(substr(prev$year, 2, 5))

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


### generate the graphs!

# bell curve
grid.arrange(trAngola, trCongo, trKenya, trEswatini, trLesotho, trNamibia, trSierraLeone, trSouthAfrica)

# flat
grid.arrange(trBangladesh, trNigeria, trPNG, trGB)

# increase
grid.arrange(trLiberia, trMozambique)

# other
grid.arrange(trBrazil, trCentral_African_Republic, trDRC, trPakistan, trPhilippines, trTanzania, trMalawi, trUganda)

# normal linear decrease
grid.arrange(trBotswana, trCambodia, trCameroon, trChad, trEthiopia, trGhana, trIndonesia, trLaos, trMyanmar, trThailand, trVietnam, trZambia, trZimbabwe)
