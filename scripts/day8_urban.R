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


venues.sf$label <- c("<span style = 'font-size:9pt'>**Emirates**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/5/53/Arsenal_FC.svg/1021px-Arsenal_FC.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **16,088 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Stamford Bridge**</span> <img src='https://upload.wikimedia.org/wikipedia/sco/thumb/c/cc/Chelsea_FC.svg/480px-Chelsea_FC.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **16,509 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Tottenham Hotspur Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/b/b4/Tottenham_Hotspur.svg/584px-Tottenham_Hotspur.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **6,966 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**London Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/West_Ham_United_FC_logo.svg/1079px-West_Ham_United_FC_logo.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **6,518 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Craven Cottage**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/e/eb/Fulham_FC_%28shield%29.svg/901px-Fulham_FC_%28shield%29.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **7,090 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Selhurst Park**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Crystal_Palace_FC_logo_%282022%29.svg/962px-Crystal_Palace_FC_logo_%282022%29.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **7,681 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Gtech Community Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/2/2a/Brentford_FC_crest.svg/1200px-Brentford_FC_crest.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **10,303 / sq km**</span>",
                     "<span style = 'font-size:9pt'>**Wembley Stadium**</span> <img src='https://upload.wikimedia.org/wikipedia/en/thumb/b/be/Flag_of_England.svg/330px-Flag_of_England.svg.png'
                     height='9' /><br><span style = 'font-size:5pt'>Population density: **7,574 / sq km**</span>")

# create column in real_london for aesthetics highlighting the wards with PL stadiums
real_london <- real_london %>%
  mutate(is_pl = if_else(ward_name %in% venues.sf$ward_name, "y","n"))

# taking only the PL wards for labeling. drawing the line to the center of the ward rather than the stadium
pl_wards <- real_london %>%
  filter(is_pl=="y") 

pl_wards <- pl_wards[ order(match(pl_wards$ward_name, venues.sf$ward_name)), ]


# arsenal, chelsea, spurs, west ham, fulham, palace, brentford, wembley

ggplot()+
  geom_sf(data=st_union(real_london), fill = "transparent", lwd=0.5, color="#360f0f")+
  geom_sf(data=real_london, aes(fill=observation, color=is_pl), lwd=0.8) +
  #geom_sf(data=venues.sf, size = 1.2) +
  scale_fill_gradient(low="#f2f1e6",high="#992f0c",
                      name="Persons per sq km",
                      labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_color_manual(values=c("transparent","black"), guide="none")+
  # make ggrepel text transparent so i can have the lines leading to the point with the rich text features
  ggrepel::geom_text_repel(
    data=pl_wards,
    aes(label=ward_name, geometry=geometry),
    color = "transparent",
    segment.colour="black",
    stat = "sf_coordinates",
    position = position_nudge_repel(
      x = c(0, 0.15, 0.18, 0.23, -0.23, 0.12, -0.15, -0.05),
      y = c(0.13, -0.02, 0.05, -0.03,-0.08, -0.05, 0, 0.1)
    ))+
  geom_richtext(lineheight = .1,
                family = "SF Pro Text",
                data=venues.sf,
                aes(label=label, geometry=geometry),
                stat = "sf_coordinates",
                position = position_nudge_repel(
                  x = c(0, 0.15, 0.18, 0.2, -0.2, 0.1, -0.15, -0.05),
                  y = c(0.13, -0.02, 0.05, -0.03,-0.08, -0.05, 0, 0.1)
                ))+
  labs(title="Population density around London's Premier League Stadiums",
       caption="Source: Office for National Statistics licensed under the Open Government Licence v.3.0")+
  theme(panel.background = element_rect(fill="#f7f5f0"),
        plot.title = element_text(family = "SF Pro Display"),
        legend.title = element_text(family = "Lucida Grande"),
        legend.text = element_text(family="SF Pro Text"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

