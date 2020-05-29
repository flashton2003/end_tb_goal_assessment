library(gridExtra)
library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)
# to have only one legend for the entire plot grid
library(cowplot)

# Plot TB incidence, TB incidence HIV negative, and TB incidence HIV positive on one graph for each country with high HIV burden

# process TB incidence data into master
master <- read.csv("who_ALL.csv")
master <- master %>% select(country, year, e_inc_100k, e_tbhiv_prct, e_inc_tbhiv_100k)
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
master <- master %>% filter(country %in% all_countries)

inc_tb_by_hiv <- function(country_name){
  
  # calculate HIV negative TB incidence using known values
  # add calculated HIV negative TB to master sheet
  master <- master %>% 
    filter(country == country_name) %>% 
    mutate(tb_hiv_neg = e_inc_100k - e_inc_tbhiv_100k)
  
  # this code gives the legend for each graph:
  p <- ggplot(master, aes(year, y = value, colour = Incidence)) +
    geom_point(aes(y = e_inc_100k, col = "Overall")) +
    geom_point(aes(y = e_inc_tbhiv_100k, col = "HIV infected")) +
    geom_point(aes(y = tb_hiv_neg, col = "HIV uninfected")) +
    xlab("Year") + ylab("TB Incidence per 100k") + ggtitle(country_name) + 
    # make legend horizontal in one row
    guides(colour = guide_legend(nrow = 1)) +
    # modify legend title
    labs(colour = "TB incidence")
  
  # the following code removes the legend:
  #ggplot(master, aes(year, y = value, color = variable)) +
  # geom_point(aes(y = e_inc_100k, col = "Overall"), show.legend = FALSE) +
  # geom_point(aes(y = e_inc_tbhiv_100k, col = "HIV infected"), show.legend = FALSE) +
  # geom_point(aes(y = tb_hiv_neg, col = "HIV uninfected"), show.legend = FALSE) +
  # xlab("Year") + ylab("TB Incidence per 100k") + ggtitle(country_name)
}

# HIV high burden countries, per WHO
# countries_hiv <- 
# c("Angola", "Botswana", "Brazil", "Cameroon", "Chad", "China", "Central African Republic", "Congo", "Democratic Republic of the Congo", "Eswatini",
#  "Ethiopia", "Ghana", "Guinea-Bissau", "India", "Indonesia", "Kenya", "Lesotho", "Liberia", "Malawi", "Mozambique", "Myanmar", "Namibia", "Nigeria", 
#  "Papua New Guinea", "Russian Federation", "Sierra Leone", "South Africa", "United Republic of Tanzania", "Thailand", "Uganda",
#  "Zambia", "Zimbabwe")

trAngola <- inc_tb_by_hiv('Angola')
trCongo <- inc_tb_by_hiv('Congo')
trEswatini <- inc_tb_by_hiv('Eswatini')
trKenya <- inc_tb_by_hiv('Kenya')
trLesotho <- inc_tb_by_hiv('Lesotho')
trNamibia <- inc_tb_by_hiv('Namibia')
trSierraLeone <- inc_tb_by_hiv('Sierra Leone')
trSouthAfrica <- inc_tb_by_hiv('South Africa')

# bell curve
# grid.arrange(trAngola, trCongo, trKenya, trEswatini, trLesotho, trNamibia, trSierraLeone, trSouthAfrica)

# for the 8 bell curve countries
bellcurve <- plot_grid( trAngola + theme(legend.position="none"),
                        trCongo + theme(legend.position="none"),
                        trKenya + theme(legend.position="none"),
                        trEswatini + theme(legend.position="none"),
                        trLesotho + theme(legend.position="none"),
                        trNamibia + theme(legend.position="none"),
                        trSierraLeone + theme(legend.position="none"),
                        trSouthAfrica + theme(legend.position="none")
                        # align = 'vh',
                        # hjust = -1,
                        # nrow = 1
)

# extract legend to display just 1 legend for entire plot grid
legend <- get_legend(trAngola)
p <- plot_grid( bellcurve, legend, rel_heights = c(1, .1), ncol = 1, 
                align = 'v', axis = 'c')
p

# flat
# grid.arrange(trNigeria,trPNG, trGNB)

# increase
# grid.arrange(trLiberia, trMozambique)

# other
# grid.arrange(trBrazil, trCentral_African_Republic, trDRC, trRussia, trTanzania, trMalawi, trUganda)

# normal linear decrease
# grid.arrange(trBotswana, trCameroon, trChad, trChina, trEthiopia, trGhana, trIndia, trIndonesia, trMyanmar, trThailand, trZambia, trZimbabwe)