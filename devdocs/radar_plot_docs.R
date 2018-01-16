## install 'webshot' package

# Radial plot -------------------------------------------------------------

#ggplot2
#http://www.r-graph-gallery.com/142-basic-radar-chart/
#http://www.r-graph-gallery.com/143-spider-chart-with-saveral-individuals/  
#http://www.r-graph-gallery.com/142-basic-radar-chart/  
  
   
#interactive
#ver ejemplo
#https://github.com/MangoTheCat/radarchart
#https://rpubs.com/dww-/189473
#https://moderndata.plot.ly/radar-charts-in-r-using-plotly/

library(devtools)
install_github("wch/webshot")
webshot::install_phantomjs()

## load packages
library(leaflet)
library(htmlwidgets)
library(webshot)

## create map
m <- leaflet() %>% addTiles()

## save html to png
saveWidget(m, "temp.html", selfcontained = FALSE)
webshot("temp.html", file = "Rplot.png",
        cliprect = "viewport")