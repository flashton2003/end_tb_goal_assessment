library(ggplot2)
library(ggpmisc)
library(gridExtra)

master <- read.csv("who_ALL.csv")
master <- master %>% select(country, year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi)
all_countries <- c("Angola","Bangladesh","Brazil","Botswana","Cambodia","Cameroon",
                   "Central African Republic","Chad","China","Congo",
                   "Democratic People's Republic of Korea",
                   "Democratic Republic of the Congo","Eswatini","Ethiopia","Ghana",
                   "Guinea-Bissau","India","Indonesia","Kenya",
                   "Laos","Lesotho","Liberia","Malawi",
                   "Mozambique","Myanmar","Namibia", "Nigeria", "Pakistan",
                   "Papua New Guinea","Philippines","Republic of Korea",
                   "Russian Federation","Sierra Leone","South Africa","Thailand",
                   "Uganda","United Republic of Tanzania","Vietnam", "Zambia","Zimbabwe")

## what is the correlation like between year and inciddence?
inc_year_cor <- function(country_name){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k) 
  res <- cor(one_country$year, one_country$e_inc_100k)
  res
}

inc_year_cor("Nigeria")



trend <- function(country_name){
  one_country <- master %>% filter(country == country_name) %>% select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) 
  my.formula <- y ~ x
  g <- ggplot(data = one_country, mapping = aes(x = year, y = e_inc_100k)) + 
  geom_smooth(method = "lm", se = TRUE, formula = my.formula, fill = "blue") +
  stat_poly_eq(formula = my.formula, aes(label = ..rr.label..), parse = TRUE) +
  geom_point() +
  geom_ribbon(data = one_country, aes(x = year, ymin=e_inc_100k_lo, ymax = e_inc_100k_hi), alpha = 0.3) +
  ggtitle(country_name) +
  xlab("Year") +
  ylab("Incidence per 100,000 people")
  g 
}


grid.arrange(trAngola)
#inc_year_cor("Angola")

trAngola<- trend("Angola")
trBangladesh<- trend("Bangladesh")
trBrazil<- trend("Brazil")
trBotswana<- trend("Botswana")
trCambodia<- trend("Cambodia")
trCameroon<- trend("Cameroon")
trCentralAfricanRepublic<- trend("Central African Republic")
trChad<- trend("Chad")
trChina<- trend("China")
trCongo<- trend("Congo")
trDemocraticPeoplesRepublicofKorea<- trend("Democratic People's Republic of Korea")
trDemocraticRepublicoftheCongo<- trend("Democratic Republic of the Congo")
trEswatini<- trend("Eswatini")
trEthiopia<- trend("Ethiopia")
trGhana<- trend("Ghana")
trGuineaBissau<- trend("Guinea-Bissau")
trIndia<- trend("India")
trIndonesia<- trend("Indonesia")
trKenya<- trend("Kenya")
trLaos<- trend("Laos")
trLesotho<- trend("Lesotho")
trLiberia<- trend("Liberia")
trMalawi<- trend("Malawi")
trMozambique<- trend("Mozambique")
trMyanmar<- trend("Myanmar")
trNamibia<- trend("Namibia")
trNigeria<- trend("Nigeria")
trPakistan<- trend("Pakistan")
trPapuaNewGuinea<- trend("Papua New Guinea")
trPhilippines<- trend("Philippines")
trRepublicofKorea<- trend("Republic of Korea")
trRussianFederation<- trend("Russian Federation")
trSierraLeone<- trend("Sierra Leone")
trSouthAfrica<- trend("South Africa")
trThailand<- trend("Thailand")
trUganda<- trend("Uganda")
trUnitedRepublicofTanzania<- trend("United Republic of Tanzania")
trVietnam<- trend("Vietnam")
trZambia<- trend("Zambia")
trZimbabwe<- trend("Zimbabwe")

grid.arrange(trAngola, trBangladesh, trBrazil, trBotswana, trCambodia, trCameroon, trCentralAfricanRepublic, trChad, trChina, trCongo, trDemocraticPeoplesRepublicofKorea, trDemocraticRepublicoftheCongo, trEswatini, trEthiopia, trGhana, trGuineaBissau, trIndia, trIndonesia, trKenya, trLaos, trLesotho, trLiberia, trMalawi, trMozambique, trMyanmar, trNamibia, trNigeria, trPakistan, trPapuaNewGuinea, trPhilippines, trRepublicofKorea, trRussianFederation, trSierraLeone, trSouthAfrica, trThailand, trUganda, trUnitedRepublicofTanzania, trVietnam, trZambia, trZimbabwe)
