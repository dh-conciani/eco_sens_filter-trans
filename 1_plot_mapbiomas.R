## carregar bibliotecas
library (dplyr)  
library (ggplot2)
library (raster)
library (rgdal)
library (reshape2)
library (sf)

## carregar mapbiomas de porto feliz
r_mapbiomas <- raster ("./raster/2019_mapbiomas_portofeliz.tif")

## carregar limite de porto feliz (com buffer)
sp_municipio <- st_as_sf(readOGR(dsn = "./vetores", layer = "sp_porto_feliz_buffer"))

## Transformar o raster Mapbiomas em uma matriz
df_mapbiomas <- as.data.frame(rasterToPoints(r_mapbiomas))
colnames(df_mapbiomas)[3] <- "Classe" ## Infomar coluna de classes

## Informar que o valor 128 é NA
df_mapbiomas <- na_if(df_mapbiomas, 128)

## Plotar classificação do MapBiomas
#x11()
ggplot(sp_municipio) + 
  geom_raster(data= df_mapbiomas, aes(x= x, y= y, fill= as.factor(Classe))) +
  scale_fill_manual(values=c('darkgreen','green2', '#FFD966', '#C27BA0', '#FFEFC3', '#af2a2a', '#FF99FF', '#0000FF', '#f3b4f1', '#c59ff4', '#e787f8', NA),
                    labels=c('Formação florestal', 'Floresta plantada', 'Pastagem', 'Cana', 'Mosaico agric/pastagem', 'Cidade', 'Solo nu', 'Água', 'Lavoura perene', 'Soja', 'Outras agriculturas','')) +
  geom_sf(fill=NA, color= "black", size= 1) +
  xlab (NULL) + ylab (NULL) + ggtitle ("Uso e cobetura do solo, Porto Feliz, SP - 2019") + 
  labs (fill= "Classe de cobertura") + theme_bw() 


## Calcular tabela de contingência (contagem de pixel por classe)
tab_freq <- melt(table(df_mapbiomas$Classe))
tab_freq$Classe <- c('Formação florestal', 'Floresta plantada', 'Pastagem', 'Cana', 'Mosaico agric/pastagem', 'Cidade', 'Solo nu', 'Água', 'Lavoura perene', 'Soja', 'Outras agriculturas', NA)
colnames(tab_freq)[3] <- "Classe"

## Calcular área de cada classe a partir da contagem de pixels
## n de pixel x ( 30 x 30 [tamanho do pixel]) dividido pelo fator 10 000 (hectares)
tab_freq$area_hectares <- tab_freq$value * (30 * 30) / 10000
## Converter área em porcentagem relativa
tab_freq$porcentagem <- tab_freq$area_hectares / sum(tab_freq$area_hectares) * 100

## Plotar distribuição de classes
x11()
ggplot(na.omit(tab_freq), aes(x= reorder(Classe, area_hectares), y= porcentagem)) +
  geom_bar(stat="identity", fill='gray35') +
  xlab (NULL) + ylab ("% de cobertura") + ggtitle ("Distribuição do uso e sobertura do solo, Porto Feliz, SP - 2019") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust=1)) + coord_flip()
