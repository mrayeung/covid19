install.packages("readxl")
install.packages("httr")
install.packages("tidyverse")
install.packages("rvest")
install.packages("magrittr")
install.packages("ggmap")
install.packages("stringr")
install.packages("dplyr")
install.packages("rworldmap")
install.packages("maps")
install.packages("viridis")
install.packages("mapproj")
library(mapproj)
library(readxl)
library(httr)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(dplyr)
library(ggplot2)
library(rworldmap)
library(RColorBrewer)
library(maptools)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”

data <- read_excel(tf)

print(data)
glimpse(data)
ndata <- data %>% group_by (region=data$'Countries and territories') %>% summarise (value = sum(Cases))
glimpse(ndata)

world <- map_data("world")
glimpse(world)
worlddata <- inner_join(world, ndata, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldplot <- ggplot(data = worlddata, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = value)) +
  scale_fill_distiller(palette ="RdBu", direction = -1) + # or direction=1
  ggtitle("Covid19 Cases by Country") +
  plain

worldplot


