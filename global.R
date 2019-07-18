
rm(list=ls())

library(shiny)
library(shinythemes)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggmap)
library(grid)
library(leaflet)
library(stringr)
library(scales)
library(rgdal)
#library(raster)
library(dbplyr)
library(DBI)
library(RPostgres)
library(config)
library(shinycssloaders)
#detach(package:raster)
#detach(package:stats)
library(dplyr)
library(rsconnect)
library(curl)
library(shinyAce)
library(fst)
library(DT)
library(googlesheets)

# Increase max size
options(shiny.maxRequestSize=1000*1024^2)



Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    legend.position = "top",
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    # axis.text.x=element_blank(),
    axis.text.y=element_text(size=14),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=14, face="bold.italic"))


# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)


colors <- c("#d94c45","#78807a","#bd6539","#d67cc9","#d99445","#4eb570","#faef52")
#4eb570 green
#d94c45 red
#bd6539 brown
#d67cc9 pink
#78807a grey
#faef52 yellow
#d99445 orange

#runApp()

# Set up account info to connect to shinyapps
#rsconnect::setAccountInfo(name='brookegibbons',token='6D757FE28C61F12274F1E16C30783B95',
#                          secret='zu/Mcxyo0ZN3qX/QTvc/SYoeRxuoY9LdPSDIET09') # connect to shinyapps

#deployApp()
