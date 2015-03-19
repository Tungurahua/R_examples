
## @knitr globals, include=FALSE}
library("knitr")
opts_chunk$set(message='FALSE',results='hide')
opts_knit$set(root.dir=normalizePath('../'))

## @knitr  librariesx
library(ggplot2)  # Hadley Wickhams plotting package
library(RJSDMX)   # Provides Access to Eurostats REST-interface
library(dplyr)    # Aids manipulation of data
library(scales)   # Needed to display percentage scales

## @knitr  layout_libs
library(xtable)

## @knitr idString,eval=FALSE}
idString = "ef_ov_lusum.A.TOTAL.D+E+F+G+H.HA.TOTAL.*"


## @knitr  FullREST
idString = "[DataID].  # insert data-code here
            [FREQ].    # Frequency of data A -> Annual
            [TERRTYPO].# All agricultural holdings -> TOTAL
            [VARIABLE].# see discussion below
            [UNIT].    # Unit of data HA -> hectar
            [AGRAREA]. # All types of territory -> TOTAL
            [GEO]      # Countries *-> select all"

## @knitr getDatax
# Define idString
idString = "ef_ov_lusum.A.TOTAL.D+E+F+G+H.HA.TOTAL.*"

# Retrieve data from Eurostat
listSdmx <- getSDMX(provider = "EUROSTAT",
                    id = idString,
                    start = "2007",
                    end="2007")

# Re-format into a dataframe
dfSdmx <- sdmxdf(listSdmx, meta = T)

## @knitr headsdmx
# Display the first 6 lines of dfSdmx, suppressing
# columns 1 and 4
options(width = 286)
head(dfSdmx[-c(1,4)])


## @knitr  getCodesVARIABLE
options(width = 83)
# get dictionary for VARIABLE
VARIABLE_Codes <- getCodes(provider = "EUROSTAT",
           flow = "ef_ov_lusum",
           dimension = "VARIABLE")

# convert it into a dataframe
VARIABLE_Codes <- data.frame(VARIABLE=names(VARIABLE_Codes),
                             labels=unlist(VARIABLE_Codes),
                             row.names=NULL)


## @knitr headVARdic
# Display the first 6 lines of VARIABLE_Codes
head(VARIABLE_Codes)

## @knitr dplyr, warning=FALSE,message=FALSE,tidy=FALSE}
# use dplyr for the following lines
dfSdmx <- 
  dfSdmx %>%
  inner_join(VARIABLE_Codes)%>%
  select(GEO, VARIABLE,labels, OBS) %>%
  filter(!GEO %in% c("CH","FX")) %>%
  group_by(GEO) %>%
  mutate(PC=OBS/sum(OBS))

# Re-order GEO to display countries in alphabetical order
dfSdmx$GEO <-  factor(dfSdmx$GEO, levels = rev(levels(dfSdmx$GEO)))

# Re-name some VARIABLE levels for smaller legend
levels(dfSdmx$labels)[24] <- "Permanent grassland and meadows"
levels(dfSdmx$labels)[16] <- "Other (incl. forests)"
levels(dfSdmx$GEO)[17] <- "FR"

## @knitr  firstplotx, fig.cap="An initial version of the plot.",fig.width=5,fig.height=5,warning=FALSE,tidy=FALSE,fig.margin=T}
gg <- ggplot(data=dfSdmx, aes(x=GEO,y=PC,fill=labels,order=labels)  )
gg <- gg + geom_bar(stat="identity",width=0.5) 
gg <- gg + coord_flip() 
gg <- gg + theme(legend.position="bottom") 
gg

## @knitr  themex
theme_qual <- function(base_size=11, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, 
           base_family=base_family) %+replace%
    theme(axis.title=element_blank(),
      legend.position = "bottom",
      legend.title=element_blank())
}

## @knitr  windowdressingx
gg <- gg + theme_qual()
gg <- gg + scale_fill_brewer(type = "qual")
gg <- gg + scale_y_continuous(labels = percent)
gg <- gg + guides(fill = guide_legend(keywidth = 0.8, 
                                      keyheight = 0.8,
                                      nrow = 2,
                                      byrow = TRUE))  
gg

## @knitr  betterplot
dfSdmx$GEO <-  factor(dfSdmx$GEO, levels = rev(levels(dfSdmx$GEO)))

gg <- gg %+% dfSdmx
gg <- gg + coord_cartesian()+facet_wrap(facets = "labels",nrow = 5)
gg <- gg + scale_fill_brewer(type = "qual",guide=FALSE)
gg <- gg +theme(axis.text.x=element_text(angle = 45))
gg
