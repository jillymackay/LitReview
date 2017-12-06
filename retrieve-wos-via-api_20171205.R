#  Have a crack at accessing WoS using API

#  Uses https://github.com/juba/rwos

# devtools::install_github("juba/rwos")

# Need to be VPN'ed or at UoE etc

library(tidyverse)
library(rwos)
library(wesanderson)


sid <- wos_authenticate()

res <- wos_search(sid, "TO=lecture capture")

pubs <- wos_retrieve_all(res) %>% 
  mutate(year = parse_number(year))

# plot something with Jill's code 
ByYear <- ggplot (data = pubs, aes(x = year, fill=..count..)) + 
  geom_histogram(binwidth = 1) + 
  labs (title = "Lecture Recoding Publications by Year", x = "Publication Year") + 
  theme_bw() + scale_fill_gradientn(colors = wes_palette("Darjeeling"))  + 
  theme(axis.text.x = element_text(angle = 90), panel.grid = element_blank()) + 
  scale_x_continuous(breaks = seq(1990, 2017, 1))
ByYear


