require(pacman)
pacman::p_load(raster, sf, openxlsx, ggplot2, ggrepel)
library(ggplot2)
library(dplyr)
library(sf)
library(tmap)
library(spData)
library(RColorBrewer)
library(colorspace)
Pais_Elev     <- getData('alt', country='Peru')
#Pais_Elev2    = get_elev_raster(Pais, z=10)
plot(Pais_Elev )

slope = terrain(Pais_Elev  , opt = "slope") 
aspect = terrain(Pais_Elev  , opt = "aspect")
hill = hillShade(slope , aspect, angle = 40, direction = 270)

Peru_Depa     <- getData('GADM', country='Peru', level=1) %>%
  st_as_sf()
Data              <- read.xlsx("Excel/Departa.xlsx", sheet="Hoja1") 
Data$Variab = sample(13467:97545,546, replace=T) 

data_del_mundo <- Peru_Depa %>% left_join(Data, by="NAME_1") 
data_del_mund  <- st_transform(data_del_mundo   ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

#data_del_mundo <- Distritos_per %>% filter(year %in% seq(2000, 2022, 4))

colores1 <- sequential_hcl(100, palette = "YlOrRd", rev = T)
world_anim= 
  tm_shape(Peru_Depa,ylim=c(-18.3518, -0.03747),xlim=c( -81.3307,  -68.65311)) +
  tm_borders()+
  qtm(data_del_mund  , "Variab", palette = colores1 ) +
  tm_facets(along = "Year", drop.units = TRUE)+
  tm_shape(Peru_Depa)+
  tm_borders("white",lwd=1)+
  tm_text("NAME_1",size = .6, col="black",shadow=TRUE,fontfamily = "Kefa", 
          fontface = "bold",
          bg.color="white", bg.alpha=.35)+
  tm_compass(type = "arrow",size = 2) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_credits("Peru:  Departamental \n     Animation map", position = c(.008, .07), col = "black", fontface="bold", size=1.6, fontfamily = "serif")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 1.5, position = c(0.40, 0.005))+
  tm_credits("Gorky Florez Castillo", position = c(0.6, .005), col = "Black", fontface="bold")+
  tm_xlab("Longitude")+
  tm_ylab("Latitude")+
  tm_grid(col = 'gray', alpha = 0.5)
  

tmap_animation(world_anim, filename = "world_anim.gif", delay = 50)
