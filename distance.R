library(dplyr)
library(fuzzyjoin)
library(formattable)

#US Geographical data
states <- data.frame(state = state.name, lat = state.center$y,lon = state.center$x)

x <- states %>%
     rename(state1 = state) %>%
	 geo_inner_join(states, max_dist = 200, distance_col = "distance") %>%
	 filter(state1 != state) %>%
	 arrange(distance) %>%
	 formattable(list(distance = color_bar("orange")), align = 'l')
	 
