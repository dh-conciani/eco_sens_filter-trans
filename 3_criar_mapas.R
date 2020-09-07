## carregar bibliotecas
library (dplyr)  
library (ggplot2)
library (raster)
library (rgdal)
library (rgeos)
library (reshape2)
library (sf)
library (adehabitatHR)
library (ggsn)


## carregar limite de porto feliz (com buffer)
sp_municipio <- st_as_sf(readOGR(dsn = "./vetores", layer = "sp_porto_feliz_buffer"))

## carregar o shapefile de estradas de porto feliz, obtido no OSM (open street maps)
vec_estradas <- st_as_sf(readOGR (dsn= "./vetores", layer = "sp_porto_feliz_estradas"))

## carregar o shapefile de estradas de porto feliz, obtido no OSM (open street maps)
vec_AP <- st_as_sf(readOGR (dsn= "./vetores", layer = "AP"))

## carregar o relevo de porto feliz (produto ALOS)
r_relevo <- raster ("./raster/ALOS_AW3D30_portofeliz.tif")

## carregar a densidade de transições
r_densidade <- raster ("./raster/areas_quentes.tif")


## Plotar o mapa de estradas 
#x11()
ggplot(sp_municipio) + 
  geom_sf(fill=NA, color= "black", size= 1) +
  geom_sf(data=vec_estradas, color= "red",) + 
  xlab (NULL) + ylab (NULL) + ggtitle ("Estradas urbanas e rurais, Porto Feliz, SP - 2020") + 
  labs (fill= "Classe de cobertura") + theme_bw()


## Plotar o mapa de relevo
#x11()
ggplot(sp_municipio) + 
  geom_raster(data= as.data.frame(rasterToPoints(r_relevo)), aes(x= x, y= y, fill= ALOS_AW3D30_portofeliz)) +
  scale_fill_gradientn(colors= c("#8E44AD", "#3498DB", "#16A085", "#2ECC71", "#F1C40F", "#F7DC6F", "red")) +
  geom_sf(fill=NA, color= "black", size= 1) +
  xlab (NULL) + ylab (NULL) + ggtitle ("Relevo, Porto Feliz, SP") + 
  labs (fill= "Relevo (m)") + theme_bw() 


## Plotar o mapa de declividade
#x11()
ggplot(sp_municipio) + 
  geom_raster(data= as.data.frame(rasterToPoints(terrain (r_relevo, opt = "slope", unit = "degrees"))), aes(x= x, y= y, fill= slope)) +
  scale_fill_gradientn(colors= c("#8E44AD", "#3498DB", "#16A085", "#2ECC71", "#F1C40F", "#F7DC6F", "red")) +
  geom_sf(fill=NA, color= "black", size= 1) +
  xlab (NULL) + ylab (NULL) + ggtitle ("Declividade, Porto Feliz, SP") + 
  labs (fill= "Declividade (graus)") + theme_bw() 


## Plotar o mapa de áreas de transição, longes no minimo 500m de cidades e distantes no máximo 350 metros de estradas
#x11()
ggplot(sp_municipio) + 
  geom_sf(fill=NA, color= "black", size= 1) +
  geom_sf(data=vec_AP, color= "darkgreen") +
  xlab (NULL) + ylab (NULL) + ggtitle ("Transição Floresta/Pasto-Mosaico, Porto Feliz, SP") + 
  theme_bw() 


## Plotar densidade de transições que atendem os critérios
x11()
ggplot(sp_municipio) + 
  geom_raster(data= as.data.frame(rasterToPoints(r_densidade)), aes(x= x, y= y, fill= areas_quentes), alpha = 0.6) +
  scale_fill_gradientn(colors= c("darkgreen", "green", "yellow", "orange", "red", "purple")) +
  geom_sf(fill=NA, color= "black", size= 1) +
  xlab (NULL) + ylab (NULL) + ggtitle ("Densidade de transições, Porto Feliz, SP") + 
  labs (fill= "Densidade") + theme_bw() 

