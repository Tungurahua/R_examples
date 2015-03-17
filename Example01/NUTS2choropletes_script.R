setwd("H:/Rprojects/R_examples")

## @knitr libraries
library(maptools) # Dealing with spatial data
library(ggplot2)  # Hadley Wickham plotting package
library(ggmap)    # Mapping with ggplot
library(RJSDMX)   # Query Eurostat REST-interface
library(grid)     # Needed for unit() function
library(rgdal)    # Provides driver to read .shp files

## @knitr get_data
# Get the data formatted as list from Eurostat
tsList <- getTimeSeries('EUROSTAT',
                       'tgs00010.A.PC.T.*',
                       start = "2013",
                       end="2013")

## @knitr convert_data
# Convert the list into a dataframe
tsDf <- sdmxdf(tsList,meta = T)


## @knitr subset_data
# Subset to exclude Turkey
tsDfsub <-  subset(tsDf, 
                   !grepl(c("TR"),GEO))


## @knitr download_shp
# Get the shapefile from Eurostat
download.file(paste0("http://ec.europa.eu/eurostat/cache/",
                     "GISCO/geodatafiles/NUTS_2010_60M_SH.zip"),
              destfile = "data/NUTS_2010_60M_SH.zip")

# Unzip the data in a temporary location
unzip("data/NUTS_2010_60M_SH.zip",
      exdir="data/NUTS_2010_60M_SH")




## @knitr Read_shp
# Read administrative boundaries 
# and set appropriate Transformation
eurMap <- readOGR(paste0("data/NUTS_2010_60M_SH/NUTS_2010_60M_SH/",
                         "NUTS_2010_60M_SH/data"),
                  "NUTS_RG_60M_2010")

# Set appropriate projection
eurMap <- spTransform(eurMap, 
                      CRS("+proj=longlat +datum=WGS84")) 


## @knitr convert_shp
# Convert into a dataframe suitable for plotting
eurMapDf <- fortify(eurMap, region="NUTS_ID")


## @knitr merge_data
# merge map and data
tsMapDf <- merge(eurMapDf, tsDfsub, 
                 by.x="id", by.y="GEO")

## @knitr order_data
# put data in correct order for plotting
tsMapDf <- tsMapDf[order(tsMapDf$order),] 


## @knitr first_plot
# inverse order (to have visible borders)
map <- ggplot(data=tsMapDf,
              aes(x=long, y=lat, group=group))
map <- map + geom_polygon(aes(fill=OBS)) 
map


## @knitr put_EU_in_focus
#limit data to main Europe
europe.limits <- geocode(c("Cape Fligely, Rudolf Island,
                           Franz Josef Land, Russia", 
                           "Gavdos, Greece", "Faja Grande,Azores", 
                           "Severny Island, Novaya Zemlya, Russia"))

# apply the limits to our dataset
tsMapDf <- subset(tsMapDf, 
                       long > min(europe.limits$lon) &
                       long < max(europe.limits$lon) & 
                       lat > min(europe.limits$lat) & 
                       lat < max(europe.limits$lat))

# and re-read the plot with the new data
map <- map %+% tsMapDf 
map



## @knitr bin_data
map <- ggplot(data=tsMapDf,
              aes(x=long, 
                  y=lat, 
                  group=group)) 
map <- map + geom_polygon(aes(fill=cut(OBS, 
                                       breaks=c(0,10,20,30,100)))) 
map <- map + theme(legend.title=element_blank())
map


## @knitr set_colors
map <- map + scale_fill_brewer(palette = "OrRd",
                               name="",
                               labels=c("0-10%",
                                        "10-20%",
                                        "20-30%",
                                        ">30%"),
                               guide="legend")
map


## @knitr add_borders
# Borders on NUTS0 level in black
rindex <- grep(eurMapDf$id,pattern=c("^[[:alpha:]]{2}$"))
eurMapDf_NUTS0 <- eurMapDf[rindex,]

eurMapDf_NUTS0 <- subset(eurMapDf_NUTS0, 
                       long > min(europe.limits$lon) &
                         long < max(europe.limits$lon) & 
                         lat > min(europe.limits$lat) & 
                         lat < max(europe.limits$lat))

map <- map + geom_path(data=eurMapDf_NUTS0, color='black', size=0.01)
map


## @knitr change_projection
map <- map + coord_map(projection = "mollweide")
map


## @knitr window_dressing
map <- map + theme_bw()
map <- map + theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.key.size = unit(10,"pt"),
    text=element_text(size=8),
    legend.position=c(0.1, 0.2))


## @knitr lastTouch
# Get the shapefile and unzip it
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip", destfile = "data/TM_WORLD_BORDERS-0.3.zip")
#unzip("data/TM_WORLD_BORDERS-0.3.zip", exdir="data/TM_WORLD_BORDERS-0.3")

TM_WORLD_BORDERS3 <- readOGR("data/TM_WORLD_BORDERS-0.3",
                  "TM_WORLD_BORDERS-0.3")
TM_WORLD_BORDERS3  <- spTransform(TM_WORLD_BORDERS3 , 
                      CRS("+proj=longlat +datum=WGS84")) 
TMdf <- fortify(TM_WORLD_BORDERS3,region = "ISO2")

## Wir brauchen Bosnien BA, Serbien RS, Makedonien ME, Albanine AL und Türkei 
TMdf <- subset(TMdf, 
                  long > min(europe.limits$lon) &
                    long < max(europe.limits$lon) & 
                    lat > min(europe.limits$lat) & 
                    lat < max(europe.limits$lat))

TMdf2 <- TMdf[which(TMdf$id %in% c("BA","RS","ME","AL","TR")),]

map <- map + geom_polygon(data = TMdf2,aes(x=long, 
                                          y=lat, 
                                          group=group),fill="gray95")
map <- map + geom_path(data=TMdf2, color='black', size=0.01)



## @knitr final_plot
map 



