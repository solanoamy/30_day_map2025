library(tidyverse)
library(dplyr)
library(fuzzyjoin)
library(ggfun)
library(sf)
library(systemfonts)

`%notin%` <- Negate(`%in%`)

setwd("~/Documents/GitHub/30_day_map2025")

# load in ward data/geography
#london_wards <- read_csv("data/london_wards.csv")
england_wards <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2022_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
new_wards <- read_csv("data/new_wards_data.csv") # UK ONS residents per square km


real_london <- left_join(new_wards, england_wards, by=c("ward_code"="WD22CD")) %>%
  dplyr::select(ward_code, ward_name, observation, LAD22CD, geometry) %>%
  st_as_sf()

epl_venues <- data.frame(
  team = c("Arsenal","Chelsea","Tottenham", "West Ham",
           "Fulham","Crystal Palace","Brentford FC", "Wembley"),
  stadium = c("Emirates","Stamford Bridge","Tottenham Hotspur Stadium","London Stadium",
              "Craven Cottage","Selhurst Park","Gtech Community Stadium", "Wembley Stadium"),
  lat = c(51.554867,51.481834,51.604252,51.538811,
          51.47495041898478,51.398338,51.490715,51.555973),
  lon = c(-0.109112,-0.191390,-0.067007,-0.017136,
          -0.22161852883585098,-0.086084,-0.289048,-0.279672)
)

venues.sf <-
  st_as_sf(epl_venues, coords=c("lon","lat"), crs=st_crs(real_london))

venues.sf <-
  st_join(venues.sf, real_london) %>%
  dplyr::select(team, stadium, ward_name, observation, geometry) %>%
  # the 2022 ward boundaries file is weird around the river so doesn't include the ward that craven cottage is in
  # just gonna manually put it the info in para no batallar
  # proof that it's the same ward: https://www.lbhf.gov.uk/councillors-and-democracy/about-hammersmith-fulham-council/ward-profiles/palace-hurlingham-2024
  mutate(ward_name = case_when(team=="Fulham" ~ "Palace & Hurlingham",
                               .default = as.character(ward_name)),
         observation = case_when(team=="Fulham" ~ 7090.8,
                                 .default = as.numeric(observation)))

ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=real_london, aes(fill=observation), color = "whitesmoke") +
  geom_sf(data=venues.sf, size = 2) +
  scale_fill_gradient(low="#f5f5f5",high="#870c01",
                      name="Persons per sq km",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(panel.background = element_rect(fill="#f7f7f7"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
2




