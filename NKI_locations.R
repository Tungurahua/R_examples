library(rgeos)
library(rgdal) # needs gdal > 1.11.0
library(ggplot2)
library(raster)
library(ggmap)

# Read the *.csv with the names
kommunen <- read.csv("resources/kommunen.csv",encoding="UTF8")


# Get administrative boundaries for Germany
gadm1 <- getData('GADM', country='DEU', level=1)
gadm3 <- getData('GADM', country='DEU', level=3)
# Subset to those named in "kommunen" for which Antwort=TRUE
gadm3 <- subset(gadm3, NAME_3 %in% kommunen[which(kommunen$Antwort=="TRUE"),]$gadm3)

# Of the subset gadm3 take polygon centers for labelling
# getting geocodes would be more exact
NKInames <- as.data.frame(coordinates(gadm3))
NKInames$label <- gadm3@data$NAME_3 
names(NKInames) <- c("x","y","label")

# Make the labels from the "Kommunen" table available
NKInames <- merge(NKInames,kommunen[,2:5],by.x = "label",by.y= "gadm3", )
head(NKInames)

# Fortify to use with ggplot
adm1 <- fortify(gadm1)
adm3 <- fortify(gadm3)

# Get theme from devtools
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df")


# Do the plotting
gg <- ggplot()
gg <- gg + theme_map()
gg <- gg + geom_map(data=adm1, map=adm1,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="#ffffff", fill="#bbbbbb", size=0.25)
gg <- gg + geom_point(data=NKInames, aes(x=x, y=y))
gg <- gg + geom_text(data=NKInames, aes(label=lsNames, x=x, y=y), size=3,vjust=-0.5,hjust=0.01)
gg <- gg + coord_map()
gg <- gg + labs(x="", y="", title="") #+ theme(plot.title=element_text(vjust=-3))
gg

ggsave("NKImap2.pdf",width = 6)
ggsave("NKImap2.png",width=6)
