## @knitr setting_scene
library(rgeos)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)
library(raster)
library(ggmap)
library(directlabels)


## @knitr read_csv
# Read the *.csv with the names
#kommunen <- read.csv("resources/kommunen.csv")


## @knitr get_adm_data
# Get administrative boundaries for Germany
gadm1 <- getData('GADM', country='DEU', level=1,path = "data/") 
gadm3 <- getData('GADM', country='DEU', level=3,path = "data/")


## @knitr select_data
# Subset to those named in "kommunen" for which Antwort=TRUE
#gadm3 <- subset(gadm3, NAME_3 %in% kommunen[which(kommunen$Antwort==TRUE),]$gadm3)

# take polygon centers for labelling
# getting geocodes would be more exact
#NKInames <- as.data.frame(coordinates(gadm3))
#NKInames$label <- gadm3@data$NAME_3 
#names(NKInames) <- c("x","y","label")

# Make the labels from the "Kommunen" table available
#NKInames <- merge(NKInames,kommunen[,2:5],by.x = "label",by.y= "gadm3", )
#head(NKInames)


labelsTRUE <- c("flensburg, germany"="Flensburg", 
                "steinfurt, germany"="Steinfurt", 
                "herten, germany"="Herten", 
                "göttingen, germany"="Göttingen", 
                "frankfurt, germany"="Frankfurt", 
                "nalbach, germany"="Nalbach", 
                "neumarkt in der oberpfalz, germany"="Neumarkt", 
                "kempten, germany"="Kempten", 
                "hanover, germany"="Hannover")


labelsFALSE <- c("sankt ingbert, germany"="St. Ingbert", 
                 "rheine, germany"="Rheine", 
                 "osnabrück, germany"="Osnabrück", 
                 "enkenbach-alsenborn, germany"="Enkenbach-Alsenborn", 
                 "marburg-biedenkopf, germany"="Marburg-Biedenkopf",
                 "heidelberg, germany"="Heidelberg", 
                 "bensheim, germany"="Bensheim", 
                 "rostock, germany"="Rostock", 
                 "burbach, germany"="Burbach")

labels <- c(labelsFALSE,labelsTRUE)

NKInamesTRUE <- geocode(labelsTRUE,output=c("latlona"))
NKInamesTRUE$answer <- TRUE
NKInamesFALSE <- geocode(labelsFALSE,output=c("latlona"))
NKInamesFALSE$answer <- FALSE

NKInames <- rbind(NKInamesFALSE,NKInamesTRUE)

# Fortify the dataframes to use with ggplot
adm1 <- fortify(gadm1)
adm3 <- fortify(gadm3)


## @knitr prepare_plotting
# Get theme from devtools
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")


## @knitr plotting
# Do the plotting
gg <- ggplot(data=NKInames,aes(x=lon, y=lat,
                 color=answer))
gg <- gg + geom_map(data=adm1, map=adm1,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="#ffffff", fill="#bbbbbb", size=0.25)
gg <- gg + geom_point()
gg <- gg + geom_text(aes(label=labels),
                     size=3,
                     vjust=-0.5,
                     hjust=0.01) 

gg <- gg + theme_map()
gg <- gg + coord_map()
gg <- gg + labs(x="", y="", title="") 
gg <- gg + theme(legend.position="none")
gg <- gg + scale_color_manual(values=c("red","black"))
gg

## @knitr saving

# Save the figure in different file formats
ggsave("Example03/NKImap2.pdf",width = 7,height=7)
ggsave("Example03/NKImap2.png",width = 7,height=7)
if(.Platform$OS.type == "windows")ggsave("Example03/NKImap2.wmf",width = 4,height=5)
