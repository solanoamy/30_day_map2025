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


venues.sf$label <- c("<span style = 'font-size:8pt'>Emirates</span> <img src='https://static.vecteezy.com/system/resources/thumbnails/015/863/617/small/arsenal-logo-on-transparent-background-free-vector.jpg'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 16,088 / sq km</span>",
                     "<span style = 'font-size:8pt'>Stamford Bridge</span> <img src='https://upload.wikimedia.org/wikipedia/sco/thumb/c/cc/Chelsea_FC.svg/480px-Chelsea_FC.svg.png'
                     width='10' /><br><span style = 'font-size:4pt'>Population density 16,509 / sq km</span>",
                     "<span style = 'font-size:8pt'>Tottenham Hotspur Stadium</span> <img src='https://logodownload.org/wp-content/uploads/2018/11/tottenham-logo-escudo.png'
                     height='7' /><br><span style = 'font-size:4pt'>Population density: 6,966 / sq km</span>",
                     "<span style = 'font-size:8pt'>London Stadium</span> <img src='https://1000logos.net/wp-content/uploads/2018/07/West-Ham-logo.jpg'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 6,518 / sq km</span>",
                     "<span style = 'font-size:8pt'>Craven Cottage</span> <img src='https://cdn.freebiesupply.com/logos/large/2x/fulham-fc-1-logo-png-transparent.png'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 7,090 / sq km</span>",
                     "<span style = 'font-size:8pt'>Selhurst Park</span> <img src='https://1000logos.net/wp-content/uploads/2023/04/Crystal-Palace-Logo-2013.png'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 7,681 / sq km</span>",
                     "<span style = 'font-size:8pt'>Gtech Community Stadium</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/2/2a/Brentford_FC_crest.svg/1200px-Brentford_FC_crest.svg.png'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 10,303 / sq km</span>",
                     "<span style = 'font-size:8pt'>Wembley Stadium</span> <img src='https://www.nicepng.com/png/full/904-9044001_wembley-logo-wembley-stadium.png'
                     width='10' /><br><span style = 'font-size:4pt'>Population density: 7,574 / sq km</span>")

ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=real_london, aes(fill=observation), color = "whitesmoke") +
  geom_sf(data=venues.sf, size = 2) +
  scale_fill_gradient(low="#f5f5f5",high="#870c01",
                      name="Persons per sq km",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  ggrepel::geom_label_repel(data=venues.sf,
                            aes(label=stadium, geometry=geometry),
                            stat = "sf_coordinates",
                            min.segment.length = 0) +
  theme(panel.background = element_rect(fill="#f7f7f7"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=real_london, aes(fill=observation), color = "whitesmoke") +
  geom_sf(data=venues.sf, size = 2) +
  scale_fill_gradient(low="#f5f5f5",high="#870c01",
                      name="Persons per sq km",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_richtext(data=venues.sf, aes(label=label, geometry=geometry), stat = "sf_coordinates") +
  theme(panel.background = element_rect(fill="#f7f7f7"))



ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=1, color="#360f0f")+
  geom_sf(data=venues.sf)+
  geom_richtext(family = "SF Pro Text",
                data=venues.sf,
                aes(label=label, geometry=geometry),
                stat = "sf_coordinates")




