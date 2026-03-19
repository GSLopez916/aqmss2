install.packages("sf")
install.packages("spData")

library(sf)
library(spData) 
library(tidyr)
library(dplyr) 
library(ggplot2) 

#1.1a
data(world)
class(world)
names(world)
#The geometry column is sticky and its is stored automatically so spatial attributes travels with the data without any extra effort.

#1.1b
st_crs(world)
#WGS84 is the global standard coordinate system used by GPS and most web mapping tools. Coordinates are expressed in decimal degrees of longitude and latitude
#making it great for global datasets where a common datum is needed is across all regions


#1.1c
unique(st_geometry_type(world))
#MULTIPOLYGON is a collection of one or more polygons treated as a single geo feature
#countries require multiple polygons when their territory is not a single contiguous land mass
#EX, the US has Alaska and Hawaii as separate polygons.

#1.1d
pdf("world_gdp_base.pdf") 
plot(world["gdpPercap"]) 
dev.off()

plot(world["gdpPercap"], main = "GDP per capita by country")
#The map shows a sharp global inequality pattern. The global north is as the wealthiest regions and the global south appears 
#to be on the lower end of wealth. 


#1.2.a
africa = filter(world, continent == "Africa") 
nrow(africa)

plot(africa["gdpPercap"], main = "GDP per capita-- Africa")
#the dataset contains 51 counties in africa. The Un reconizes 54 soverign African states, so this count is slightly 
# so that this count is below expectations and likely reflects missing data or the exclusion of very small territories
#from the spdata world polygon dataset

#1.2.b
world = world %>% 
  mutate(pop_millions = pop / 1e6) 
gdp_by_continent = world %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE)) 

print(st_drop_geometry(gdp_by_continent))

africa_sorted = africa %>% 
  arrange(desc(gdpPercap)) %>% 
  select(name_long, gdpPercap) 
print(head(st_drop_geometry(africa_sorted), 5))

#when summarirse() is called on a grouped sf object, it unions the geometries within 
#each group and retains the resulting geometry column. to obtain a plain data frame without spatial info
#use st drop b4 or after the summary step. this allows to aviods carrying unneeded geometry through purely tab analyses

#1.2.c
#The 5 african countries with the highest GDP per cap, in this data are shown above, Guinea ranks high due to its oil
#rev to a small population. Libya are oil dependent economies, and Botswana benefits from diamond exports and relatively strong 
#institutions, and the 5th position is typically taken by a North African economy. 


#1.3.a
ggplot(world) + 
  geom_sf(aes(fill = gdpPercap)) + 
  scale_fill_viridis_c(option = "plasma", na.value = "grey80", 
                       name = "GDP per capita") + 
  theme_void() + 
  labs(title = "GDP per capita by country")
# The pattern mirrors with the base R map showed. West Eu, NA and Oceania stand out as the wealthiest
# East Asia shows a gradient from high to middle and sub Africa and South Asia concentrate the lowest values with a copuple
#exceptions

#1.3.b
ggplot(africa) + 
  geom_sf(aes(fill = gdpPercap)) + 
  scale_fill_viridis_c(option = "magma", na.value = "grey80", 
                       name = "GDP per capita") + 
  theme_void() + 
  labs(title = "GDP per capita-- Africa")

ggsave("africa_gdp.pdf", width = 7, height = 6)

#Within Africa there is much variation. North Africa tend to be wealthier countries. While Southern 
#africa countries, besides the oil rich countries, have the lowest values, this reflects low diversification and stuructural poverty.

#1.3.c
ggplot(africa) + 
  geom_sf(aes(fill = gdpPercap), 
          color = "white", linewidth = 0.3) + 
  scale_fill_viridis_c(option = "magma", na.value = "grey80", 
                       name = "GDP per capita") + 
  theme_void() + 
  labs(title = "GDP per capita-- Africa (with borders)")

ggsave("africa_gdp_borders.pdf", width = 7, height = 6)

#Adding white country borders improves readability for smaller countries. The thin white lines show each country
#without competing visually, making it easier to identify specific countries of interest and to compare states. 












