library(tidyverse)
library(dplyr)
library(fuzzyjoin)
library(ggfun)
library(sf)
library(systemfonts)
library(ggrepel)
library(ggtext)

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

#<span style = 'font-size:2pt'></span>
#<span style = 'font-size:10pt'></span>


venues.sf$label <- c("<span style = 'font-size:6pt'>**Emirates**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/5/53/Arsenal_FC.svg/1021px-Arsenal_FC.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 16,088 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Stamford Bridge**</span> <img src='https://upload.wikimedia.org/wikipedia/sco/thumb/c/cc/Chelsea_FC.svg/480px-Chelsea_FC.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density 16,509 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Tottenham Hotspur Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/b/b4/Tottenham_Hotspur.svg/584px-Tottenham_Hotspur.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 6,966 / sq km</span>",
                     "<span style = 'font-size:6pt'>**London Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/West_Ham_United_FC_logo.svg/1079px-West_Ham_United_FC_logo.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 6,518 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Craven Cottage**</span> <img src='https://cdn.freebiesupply.com/logos/large/2x/fulham-fc-1-logo-png-transparent.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 7,090 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Selhurst Park**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Crystal_Palace_FC_logo_%282022%29.svg/962px-Crystal_Palace_FC_logo_%282022%29.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 7,681 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Gtech Community Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/2/2a/Brentford_FC_crest.svg/1200px-Brentford_FC_crest.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 10,303 / sq km</span>",
                     "<span style = 'font-size:6pt'>**Wembley Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/b/be/Flag_of_England.svg/330px-Flag_of_England.svg.png'
                     height='9' /><br><span style = 'font-size:4pt'>Population density: 7,574 / sq km</span>")


ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=real_london, aes(fill=observation), color = "whitesmoke") +
  geom_sf(data=venues.sf, size = 2) +
  scale_fill_gradient(low="#f5f5f5",high="#870c01",
                      name="Persons per sq km",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  # make ggrepel text transparent so i can have the lines leading to the point with the rich text features
  ggrepel::geom_text_repel(lineheight = .1,
                            family = "SF Pro Text",
                           # split teams into two sections (those on top and on bottom) to reposition the labels
                            data=venues.sf %>%
                              filter(team %in% c("Arsenal","Tottenham","Wembley","West Ham")),
                            aes(label=team, geometry=geometry),
                            color = "transparent",
                            segment.colour="black",
                            stat = "sf_coordinates",
                            position = position_nudge_repel(
                              x = c(0, 0.18, 0.18, -0.05),
                              y = c(0.13, 0.12, -0.1, 0.1)
                            ))+
  geom_richtext(lineheight = .1,
                family = "SF Pro Text",
                data=venues.sf %>%
                  filter(team %in% c("Arsenal","Tottenham","Wembley","West Ham")),
                aes(label=label, geometry=geometry),
                stat = "sf_coordinates",
                position = position_nudge_repel(
                  x = c(0, 0.18, 0.18, -0.05),
                  y = c(0.13, 0.12, -0.1, 0.1)
                ))+
  ggrepel::geom_text_repel(lineheight = .1,
                            family = "SF Pro Text",
                            data=venues.sf %>%
                              filter(team %notin% c("Arsenal","Tottenham","Wembley","West Ham")),
                            aes(label=team, geometry=geometry),
                            stat = "sf_coordinates",
                            color = "transparent",
                            segment.colour="black",
                            position = position_nudge_repel(
                              x = c(0.1, -0.2, 0.2, -0.15),
                              y = c(-0.1, -0.1, -0.1, 0)
                            ))+
  geom_richtext(lineheight = .1,
               family = "SF Pro Text",
               data=venues.sf %>%
                 filter(team %notin% c("Arsenal","Tottenham","Wembley","West Ham")),
               aes(label=label, geometry=geometry),
               stat = "sf_coordinates",
               position = position_nudge_repel(
                 x = c(0.1, -0.2, 0.2, -0.15),
                 y = c(-0.1, -0.1, -0.1, 0)
               ))+
  theme(panel.background = element_rect(fill="#f7f7f7"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())



ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=venues.sf, size = 2) +
  geom_richtext(lineheight = .1,
                family = "SF Pro Text",
                data=venues.sf,
                aes(label=label, geometry=geometry),
                stat = "sf_coordinates")

ggplot(venues.sf, aes(label=label, geometry=geometry))+
  geom_richtext(stat = "sf_coordinates") +
  geom_sf()+
  ggrepel::geom_text_repel()





