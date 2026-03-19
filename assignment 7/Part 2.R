Part 2

library(sf)
library(spData) 
library(tidyr)
library(dplyr) 
library(ggplot2) 


conflict_raw <- read.csv("https://github.com/franvillamil/AQM2/blob/master/datasets/spatial/conflict_events.csv")
#2.1a

url <- "https://raw.githubusercontent.com/franvillamil/AQM2/master/datasets/spatial/conflict_events.csv"
conflict_raw <- read.csv(url)

names(conflict_raw)

conflict_sf <- st_as_sf(conflict_raw, 
                        coords = c("longitude", "latitude"), 
                        crs = 4326)

class(conflict_sf)
st_crs(conflict_sf)

#ST_as_sf converts the data frome into a spatial object by adding geometry colum. 
#the cords specify tells which colums in the data frame contain the longitude and latitude values to build 
#those points. crs = 4326 means the CRS to WGS84 which is the standard for locations using raw decimal degrees

#2.1b
nrow(conflict_sf)

table(conflict_sf$event_type)
#there are 68,354 events in the data set. the event type most common is state based

#2.1c

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_sf(data = conflict_sf, aes(color = event_type), alpha = 0.5, size = 0.5) +
  theme_minimal() +
  labs(title = "Global Conflict Events by Type",
       color = "Event Type")

ggsave("global_conflict_map.pdf", width = 10, height = 6)

#the conflcits are mostly concentrated in clusters with the continent of Africa. The events
#tend to be around central and east africa. 


#2.2a
st_crs(conflict_sf) == st_crs(world)
events_joined <- st_join(conflict_sf, world)
nrow(events_joined)
nrow(conflict_sf)

#St_join is determining which country polygon each even point falls within because it identifies which country polygon from the world 
# dataset physically contains each point in the conflict_sf. Checking both objects share the same CRS is important because spatial joining needs coordinates to be the same
# to align correctly. if not it will be wrong

#2.2b
sum(is.na(events_joined$name_long))
sum(is.na(events_joined$name_long)) / nrow(events_joined)
#The fraction of events with no matching country is about 2.3% of the data. 2 possible reasons is bc
#the coordinates are located in the ocean or on small islands. or 2 small coordinate errors make the cordinates 
#fall outside the offical country borders.

#2.2c
country_summary <- events_joined %>%
  filter(!is.na(name_long)) %>%
  st_drop_geometry() %>%
  group_by(name_long) %>%
  summarise(n_events = n(),
            total_fatalities = sum(fatalities)) %>%
  arrange(desc(n_events))
head(country_summary, 10)
#The results are consistent with the knowlegde of armed conflicts in africa
#The Rwanda genocide comes to mind and also the history of Ethiopia have had many conflicts in their history.

#2.3a
country_summary_df <- country_summary %>%
  st_drop_geometry()
world_counts <- world %>%
  left_join(country_summary_df, by = "name_long")
world_counts$n_events[is.na(world_counts$n_events)] <- 0
world_counts$total_fatalities[is.na(world_counts$total_fatalities)] <- 0
nrow(world_counts)
nrow(world)
#the 177 matches exactly between the worldcounts and org. world data, confirming the left_join worked correctly.

#2.3b
africa_final <- world_counts %>%
  filter(continent == "Africa")


ggplot(africa_final) +
  geom_sf(aes(fill = n_events)) +
  scale_fill_viridis_c(option = "magma", name = "Event Count") +
  theme_minimal() +
  labs(title = "Conflict Intensity in Africa",
       subtitle = "Number of events per country")

ggsave("africa_conflict_map.pdf", width = 8, height = 6)

#The map reveals a clear geographic concentration of conflict. This is a more effective than the point map for showing national trends and comparing
#the total scale of conflict across different states

#2.3c
ggplot(africa_final) +
  geom_sf(aes(fill = log1p(n_events))) +

  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Log(events+1)") +
  theme_minimal() +
  labs(title = "Log-Transformed Conflict Intensity in Africa",
       subtitle = "Using log1p(n_events) to visualize relative differences")

ggsave("conflict_map.pdf", width = 8, height = 6)

#The log transformation is useful because the raw counts are highly skewed
# with a few countries having high values that wash out the rest of the map. What is reveals is the differences among countries with fewer events that apperared the same
#in the raw map. It is reveals more nuance. 


#2.4b
nigeria_events <- events_joined %>% 
  filter(name_long == "Nigeria")
#2.4c
abuja <- data.frame(name = "Abuja", lon = 7.48, lat = 9.07) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
#2.4d
nigeria_utm <- st_transform(nigeria_events, 32632)
abuja_utm <- st_transform(abuja, 32632)
#2.4e
nigeria_utm$dist_capital_km <- as.numeric(st_distance(nigeria_utm, abuja_utm)) / 1000
#2.4f
mod1 <- lm(log1p(fatalities) ~ log1p(dist_capital_km), data = nigeria_utm)
mod2 <- lm(log1p(fatalities) ~ log1p(dist_capital_km) * event_type, data = nigeria_utm)
summary(mod1)
summary(mod2)
#2.4g
#The results show that mod1 from the capital alone is not a significant predictor of deaths. Mod2 reveals that the effect of
#distance depends on the type of event. 

#2.5a
#One limitation of the spatial join approach used in this assignment is that if a point falls just outside of the polygon due to the 
#coordinates, it may be assigned to the wrong coutnry. In order to handle this better, a researcher could use a buffer around the polygons to capture all nearby points
#and would work around this issue. Assigning the points to the nearest country would be the best way to work around this issue.

#2.5b
#The difference between st_join and left_join is the latter uses geographic coordinates to match rows based on the actual location. The left_join
# is an attribute join that uses key variables to merge the 2 data sets regareless of the location
# Honestly, I would prefer st_join to understand the importance of geographic location and its relationships
#but, I would use left_join to merge external data to an existing map once the gep link is established. 














