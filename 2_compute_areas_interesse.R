## carregar bibliotecas
library (dplyr)  
library (ggplot2)
library (raster)
library (rgdal)
library (rgeos)
library (reshape2)
library (sf)
library (adehabitatHR)


## carregar mapbiomas de porto feliz
r_mapbiomas <- raster ("./raster/2019_mapbiomas_portofeliz.tif")

## carregar limite de porto feliz (com buffer)
sp_municipio <- st_as_sf(readOGR(dsn = "./vetores", layer = "sp_porto_feliz_buffer"))

## carregar o shapefile de estradas de porto feliz, obtido no OSM (open street maps)
vec_estradas <- readOGR (dsn= "./vetores", layer = "sp_porto_feliz_estradas")

## Plotar o mapa de estradas 
ggplot(st_as_sf(vec_estradas)) + 
  geom_sf(fill=NA, color= "red", size= 0.5) +
  xlab (NULL) + ylab (NULL) + ggtitle ("Estradas urbanas e rurais, Porto Feliz, SP - 2020") + 
  labs (fill= "Classe de cobertura") + theme_bw() 

## transformar o mapbiomas de raster para vetor
## essa operação pode levar alguns minutos ou algumas horas, dependendo do computador
## por garantia, já deixei o arquivo salvo na pasta, não precisa processar essa linha =)
#vec_mapbiomas <- rasterToPolygons(r_mapbiomas, fun=NULL, n=4, na.rm= TRUE, digits=12, dissolve= TRUE)
## exportar shapefile
##writeOGR(vec_mapbiomas, dsn = "./vetores", layer = "vec_mapbiomas", driver = "ESRI Shapefile")

## carregar shapefile mapbiomas
vec_mapbiomas <- readOGR(dsn = "./vetores", layer = "vec_mapbiomas")

## informar que pastagem e mosaico de agricultura com pastagem será tratado como mesma coisa (criamos o codigo 99)
vec_mapbiomas@data[vec_mapbiomas@data == 15] <- 99
vec_mapbiomas@data[vec_mapbiomas@data == 21] <- 99

## criar um buffer 60 metros ao redor das florestas 
## vai dar um aviso dizendo que o objeto espera coordenadas planares, não tem problema, fizemos a conversão ali embaixo de metros para graus
buffer_floresta <- gBuffer (subset(vec_mapbiomas, X2019__ == 3), width = 0.0003)

## igual para pastos - essa função pode demorar um pouco
buffer_pasto <- gBuffer (subset(vec_mapbiomas, X2019__ == 99), width = 0.0003)

## estimar as intesercções entre pasto e floresta
interseccoes <- gIntersection (buffer_floresta, buffer_pasto)
#writeOGR (as(interseccoes, "SpatialPolygonsDataFrame" ), dsn = "./vetores", layer = "interseccoes", driver = 'ESRI Shapefile')

## Como estamos usando 30m flroesta  +30m pasto  = 60 metros de buffer, em pequenos fragmentos ou matas ciliares
## as nossas intersccoes (areas de transição pasto-floresta) podem atravessar a mata e cair em outras classes
## não queremos que isso aconteça, pois sabemos que a transição que queremos só acontece em pasto
## vamos usar todas as demais classes do mapbiomas para filtrar nossas interseccoes

## vamos chamar tudo que não é pasto de uma mesma coisa, nessa caso de 100
vec_mapbiomas@data[vec_mapbiomas@data != 99] <- 100

## aplicar o filtro: apagar todas interseccoes que ocorrem fora de pasto
filtro_interseccoes <- gDifference (interseccoes, subset(vec_mapbiomas, X2019__ == 100))  
#writeOGR (as(filtro_interseccoes, "SpatialPolygonsDataFrame" ), dsn = "./vetores", layer = "filtro_interseccoes", driver = 'ESRI Shapefile')
## Se quiser, desmarque os comentários do writeOGR, exporte os produtos e compara com a imagem Google do ARCGis e as classes mapbiomas

## Vamos chamar a partir de agora as interessecções de áreas potenciais, vamos filtrar mais um pouco:
## Que tal excluir tudo que estiver a menos de 500m de áreas urbanas?
## Vamos precisar recarregar o vetor de classes para recupérar as classes originais (nós alteramos os códigos de classe ali atras kk)
vec_mapbiomas <- readOGR(dsn = "./vetores", layer = "vec_mapbiomas")

## criar um buffer de 500m ao redor das áreas urbanas ## código 24
buffer_urbano <- gBuffer (subset(vec_mapbiomas, X2019__ == 24), width = 0.005)

## filtrar as areas potenciais novamente, excluindo tudo q estiver nesse raio
AP <- gDifference (filtro_interseccoes, buffer_urbano)  

## carregar o relevo de porto feliz (produto ALOS)
r_relevo <- raster ("./raster/ALOS_AW3D30_portofeliz.tif")

## converter relevo em uma matriz
df_relevo <- as.data.frame(rasterToPoints(r_relevo))
colnames(df_relevo)[3] <- "Relevo" ## Infomar coluna de classes

## Estimar declividade a partir do relevo (em graus)
r_slope <- terrain (r_relevo, opt = "slope", unit = "degrees")

## converter relevo em uma matriz
df_slope <- as.data.frame(rasterToPoints(r_slope))
colnames(df_slope)[3] <- "Declividade" ## Infomar coluna de classes

## Plotar a declividade
# ggplot(sp_municipio) + 
#   geom_raster(data= df_slope, aes(x= x, y= y, fill= Declividade)) +
#   scale_fill_gradientn(colors= c("#8E44AD", "#3498DB", "#16A085", "#2ECC71", "#F1C40F", "#F7DC6F", "red")) +
#   geom_sf(fill=NA, color= "black", size= 1) +
#   xlab (NULL) + ylab (NULL) + ggtitle ("Declividade, Porto Feliz, SP") + 
#   labs (fill= "Declividade (graus)") + theme_bw() 

## Toda área potencial que tiver mais que 30 graus de declividade será excluida
## Para isso, vamos criar um raster onde declividade entre 0-25 será 0 e declividade a partir de 25 será 1
slope25 <- reclassify (r_slope, rcl = cbind(c(0,  25.000001),
                                            c(25, 100),
                                            c(0,  1)))

## Converter slope maior 25 para poligonos 
vec_slope25 <- rasterToPolygons(slope25, fun=function(x){x==1}, n=4, na.rm= TRUE, digits=12, dissolve= TRUE)

## Apagar áreas com alta declividade das área potenciais
AP <- gDifference (AP, vec_slope25) 

## Criar uma buffer de 300m ao redor das estradas, vamos filtrar e selcionar areas com posibilidade de acesso
buffer_estrada <- gBuffer (vec_estradas, width = 0.003)

## Selecionar apenas as áreas potênciais que estiverem dentro do buffer de estradas
AP <- gIntersection (AP, buffer_estrada)

## Plot apenas para entender
## Essas são nossas interfaces flroesta-pasto
plot (interseccoes, axes=T)
## Aqui as áreas potenciais, filtradas para não cair em alta declividade e para cairem proxímas a estradas
plot (AP, axes = T)
lines (buffer_estrada, col="red")

## Esse já é um produto legal, com base nele da pra visualizar em uma imagem Google e começar a selecionar as áreas :P 
## Vamos exportar esse produto
#writeOGR (as(AP, "SpatialPolygonsDataFrame" ), dsn = "./vetores", layer = "AP", driver = 'ESRI Shapefile')

## Transformar as areas potenciais em raster
r_AP <- rasterize(x= AP, y= r_mapbiomas)

## Ler as APs como uma matriz de pontos
df_AP <- rasterToPoints(r_AP)

## Cada ponto representa um pixel de 30 x 30, vc tem tudo isso de area potencial :O
plot (df_AP, pch = "+")

## Vamos estimar um raster que vai nos apontar onde existe maior concentração de pontos
kde.output <- kernelUD(xy= SpatialPoints(df_AP[,1:2], proj4string=crs(r_AP)), 
                       h="href", 
                       grid = 800)


## recortar para porto feliz
pontos_quentes <- crop (raster(kde.output), sp_municipio)
pontos_quentes <- mask (pontos_quentes, sp_municipio)

## apenas conferir se gerou
plot (pontos_quentes)

## Esse é nosso produto final, vamos exportar :P 
writeRaster(pontos_quentes, filename="./raster/areas_quentes.tif")


