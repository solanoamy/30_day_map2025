library(tidyverse)
library(dplyr)
library(ggpubr)
library(sf)
library(tmap)
library(systemfonts)

`%notin%` <- Negate(`%in%`)

setwd("~/Documents/GitHub/30_day_map2025")

# i created a dataset of septa trips using my septa key
# some trips where i used other payment are missing
# load septa csv
amy_trips <- read.csv("data/amy_septa_trips2425.csv") %>%
  dplyr::select(entry_stop, mode, route) %>%
  # have to rename trolley lines to new metro versions
  mutate(route = case_when(route == 10 ~ "T1",
                           route == 13 ~ "T3",
                           route == 34 ~ "T2",
                           route == 36 ~ "T5",
                           .default = as.character(route)),
         # i am going to manually rename the rail stations too since they don't match up with my data
         # and not working with that many to where i feel the need to automate
         entry_stop = gsub(" Station - MFL", "", entry_stop),
         entry_stop = case_when(entry_stop == "40th St Trolley Portal" ~ "40th St Portal",
                                entry_stop == "33rd St Trolley Station" ~ "33rd St",
                                entry_stop == "8th St" ~ "8th-Market",
                                entry_stop == "69th St Transportation Center - MFL EB" ~ "69th St Transit Center",
                                entry_stop == "15th St" ~ "15th St/City Hall",
                                entry_stop == "30th Street Station/ZONE CCP" ~ "30th Street Station",
                                entry_stop == "Jefferson Station/ZONE CCP" ~ "Jefferson",
                                entry_stop == "Jenkintown Wyncote/ZONE 3" ~ "Jenkintown-Wyncote",
                                entry_stop == "Penn Medicine Station/ZONE CCP" ~ "Penn Medicine",
                                .default = as.character(entry_stop)))

# will use this same script for points and lines
# so load in septa GIS data, including transit stops and routes for bus and rail from open data portal

septa_stops <- st_read("https://hub.arcgis.com/api/v3/datasets/b227f3ddbe3e47b4bcc7b7c65ef2cef6_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")
# regional rail is separate
rr_stops <- st_read("https://services2.arcgis.com/9U43PSoL47wawX5S/arcgis/rest/services/SEPTA_-_Regional_Rail_Stations/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")


# narrow trips data into unique stations before transforming to spatial data
amy_stations <- amy_trips %>% group_by(entry_stop, mode, route) %>% summarise(total_trips=n())

# separate chart maintaining the route as well
amy_routes <- amy_trips %>% group_by(mode, route) %>% summarise(n=n())

# assign geometries to stations and create sf object
stations.sf <- left_join(amy_stations %>% filter(mode!="Regional Rail"),
                         septa_stops %>%
                           dplyr::select(LineAbbr, StopName, geometry),
                         by=c("entry_stop"="StopName", "route"="LineAbbr"),
                         multiple = "first") %>%
  rbind(left_join(amy_stations %>% filter(mode=="Regional Rail"), rr_stops %>%
                    dplyr::select(Station_Na), by=c("entry_stop"="Station_Na"))) %>%
  st_as_sf() %>%
  group_by(entry_stop, mode) %>% summarize(total_trips=sum(total_trips))


# change bbox so i can put the legend in the map
bbox_new <- st_bbox(stations.sf) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.2 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.8 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.2 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top

bbox_new <- bbox_new %>%
  st_as_sfc() # 

#tmap_mode("view")
tmap_mode("plot")

# add basemap
tm_basemap("CartoDB.PositronNoLabels") +
  tm_shape(stations.sf, is.main = TRUE, bbox = bbox_new) +
  # plot points, bigger for more trips and fill based on mode
  tm_symbols(size="total_trips",
             size.legend = tm_legend(
               title = "Trips",
               position = c("right","bottom"),
               width = 7,
               bg.color = "whitesmoke",
               frame.r = 4,
               frame.lwd = 0.7,
               show = TRUE
             ),
             # make circles a little bigger
             size.scale = tm_scale(values.scale = 2),
             fill = "mode",
             col = "black",
             fill.scale = tm_scale_categorical(
               n = 4, values = c("#f78c1f","#4377bc","red","#49b048")
               ),
             fill_alpha = 0.85,
             fill.legend = tm_legend(
               title = "Mode",
               position = c("right","bottom"),
               width = 7,
               bg.color = "whitesmoke",
               frame.r = 4,
               frame.lwd = 0.7,
               show = TRUE
             )) +
  tm_credits("Data source: SEPTA", fontface = "bold",
             position = c("left","bottom")) +
  tm_scalebar(position = c("left","bottom"))+
  # one year later and my tm_title still won't let me change fontface
  tm_title("Where I Ride", 
           size = 1.5,
           color = "black",
           fontface = "italic",
           fontfamily = "sans",
           bg = TRUE,
           bg.color = "whitesmoke",
           position = c("left","top"))

# now for lines
# read in regional rail
rr_lines <- st_read("https://opendata.arcgis.com/api/v3/datasets/7ebff6bc356d4fa28d4a7e4147d03b32_0/downloads/data?format=geojson&spatialRefId=4326") %>%
  # then i'm just gonna narrow this down knowing which ones i took
  filter(Route_Name %in% c("Airport","Lansdale/Doylestown",
                           "Warminster", "West Trenton")) %>%
  # rename columns in order to to the rbind
  dplyr::select(Route_Name) %>%
  rename("route"="Route_Name") %>%
  mutate(mode = "Regional Rail")
  
  
# read in geojson of septa lines
septa_lines <- st_read("https://hub.arcgis.com/api/v3/datasets/4f827cdbf84d4a53983cf43b8d9fd4df_0/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1") %>%
  # rename the trolley lines so i can merge to routes dataset, and only keep relevant columns
  dplyr::select(LineAbbr, Shape__Length) %>%
  mutate(LineAbbr = case_when(LineAbbr == "T1 (10)" ~ "T1",
                              LineAbbr == "T2 (34)" ~ "T2",
                              LineAbbr == "T3 (13)" ~ "T3",
                              LineAbbr == "T5 (36)" ~ "T5",
                              .default = as.character(LineAbbr)))

amy_lines <- left_join(amy_routes, septa_lines, by=c("route"="LineAbbr"), multiple = "first") %>%
  dplyr::select(mode, route, geometry) %>%
  filter(route != "Junction Route") %>%
  rbind(rr_lines) %>%
  st_as_sf()

# using dvrpc counties map to add more visual dimension/context since i don't plan to add a basemap
dvrpc_counties <- st_read("https://arcgis.dvrpc.org/portal/rest/services/boundaries/countyboundaries/FeatureServer/0/query?where=%22dvrpc_reg%22+LIKE+%27%25Yes%25%27&fullText=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&defaultSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=xyFootprint&resultOffset=&resultRecordCount=&returnTrueCurves=false&returnExceededLimitFeatures=false&quantizationParameters=&returnCentroid=false&returnEnvelope=false&timeReferenceUnknownClient=false&maxRecordCountFactor=&sqlFormat=none&resultType=&featureEncoding=esriDefault&datumTransformation=&cacheHint=false&f=geojson")

#philly only
philly_outline <- st_read("https://arcgis.dvrpc.org/portal/rest/services/boundaries/countyboundaries/FeatureServer/0/query?where=%22Co_Name%22+LIKE+%27%25Philadelphia%25%27&fullText=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&defaultSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&gdbVersion=&historicMoment=&returnDistinctValues=false&returnIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&multipatchOption=xyFootprint&resultOffset=&resultRecordCount=&returnTrueCurves=false&returnExceededLimitFeatures=false&quantizationParameters=&returnCentroid=false&returnEnvelope=false&timeReferenceUnknownClient=false&maxRecordCountFactor=&sqlFormat=none&resultType=&featureEncoding=esriDefault&datumTransformation=&cacheHint=false&f=geojson")
philly_hydro <- st_read("https://hub.arcgis.com/api/v3/datasets/2b10034796f34c81a0eb44c676d86729_1/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1")

# take dvrpc counties, crop them to the philly outline to create polygons
# i only went as far as jenkintown so having all of montco and bucks in there is too much
cropped_counties <- st_crop(dvrpc_counties, philly_outline)

# crop the transit line data to what i want to show on the map (not the whole line)
cropped_lines <- st_crop(amy_lines, cropped_counties)

# crop hydrology layer to the map shape
cropped_water <- st_crop(philly_hydro, cropped_counties)

# deciding to use both layers in the same map because the other one looks too empty
ggplot() +
  geom_sf(data=cropped_counties, fill="#1a1e24",color="#2a384f")+
  geom_sf(data=cropped_water, fill="#30394a", color="transparent")+
  geom_sf(data=cropped_lines, aes(color=mode), lwd=0.6)+
  geom_sf(data=stations.sf, aes(fill=mode),
          shape = 21, color="#c9cfd1", size=1.2, stroke = 0.1)+
  scale_color_manual(values = c("#593b31","#69758a","#437bba","#048200"),
                     name=NULL)+
  scale_fill_manual(values = c("#593b31","#69758a","#437bba","#048200")
                    )+
  guides(
    color  = guide_legend(position = "inside",
                          #direction = "horizontal",
                          # thicker legend lines
                          override.aes = list(lwd = 3) 
                          ),
    fill = "none"
    )+
  # reduce space between map, plot, and background
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  labs(title="How I Roll",
       subtitle="SEPTA Lines I rode: August 2024-July 2025",
       caption="Data sources: SEPTA and DVRPC")+
  #theme_void()+
  theme(legend.position.inside = c(0.85, 0.15),
        legend.background = element_rect(fill = "#c9cfd1"),
        legend.key = element_rect(fill="#c9cfd1"),
        #legend.margin = margin(t=0.2,b=0.2, l=0.5, r=1),
        text = element_text(family="SF Pro Text"),
        plot.title = element_text(face="bold",
                                  family = "Optima",
                                  color="#69758a",
                                  size=30,
                                  vjust = 0.2),
        plot.subtitle = element_text(face="bold",
                                     color="#c9cfd1",
                                     vjust = -0.2,
                                     size=9),
        plot.caption = element_text(color="#95999e",
                                    face = "italic",
                                    size= 6),
        plot.title.position = "plot",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="#1a1e24"),
        plot.background = element_rect(fill="#1a1e24"))


  
  
  
  
  
  



                           