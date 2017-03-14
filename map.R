install.packages("devtools")
install.packages(c("maps", "mapdata"))
install.packages("kriging")
install.packages(c("gstat", "sp","maptools"))
devtools::install_github("dkahle/ggmap")
library(devtools)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(mapdata)
library(ggmap)
library(kriging)
library(gstat)
library(sp)
library(maptools)
#get county data for FL
counties <- map_data("county")
fl_county <- subset(counties, region == "florida")
states <- map_data("state")
fl_df <- subset(states, region == "florida")
#plot base map (florida) and then add county boundaries
fl_base <- ggplot(data = fl_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
fl_base + theme_nothing() + 
  geom_polygon(data = fl_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top
#aggregate rows in the same county
newskimmer <- aggregate(Found~County, skimmer, sum)
colnames(newskimmer)[1] <- "subregion"
newskimmer$subregion <- tolower(newskimmer$subregion)
newskimmer
#join newskimmer and fl_county
flcopa <- inner_join(fl_county, newskimmer, by = "subregion")
flcopa
#plot
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
final <- fl_base + 
  geom_polygon(data = flcopa, aes(fill = Found), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes
#data transformation to make the distribution comparable
final + scale_fill_gradient(trans = "log10")
