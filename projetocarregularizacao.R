#####################################
#### 1 Limpar Console e Memória #####
#####################################

gc(TRUE) #garbage colector da RAM
rm(list = ls()) #limpar memoria das variáveis globais 
dev.off() # limpar os plots
cat("\014") #limpar console

################################################################################
##### 2 Carregar bibliotecas, arquivos externos e definir pasta de trabalho ####
################################################################################

list.of.packages <- c("colorRamps","ggplot2","zoo","RColorBrewer", "ggrepel", "sf", "rgeos","ggforce",
                      "rworldmap", "rworldxtra","scales","openair","raster","rgdal","rasterVis",
                      "ggspatial","reshape2", "cowplot", "googleway", "networkD3","tidyverse") # lista de pacotes utilizados

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # checar se há algum pacote novo

if(length(new.packages)) install.packages(new.packages) # instala os pacotes novos

lapply(list.of.packages, require, character.only = TRUE) # carrega os pacotes necessários

setwd("G:/My Drive/Pesquisa/Coordenador de Projetos de Pesquisa 2019/SHAPE_2512309/") #define a pasta de trabalho

####################################
####   3 Importanto os dados    ####
####################################

shpdir <- list.files(getwd(),pattern = "*.shp",full.names = T) #pega o diretório de cada shape

shplist<- lapply(shpdir, function(x) readOGR(x,use_iconv = TRUE, encoding = "LATIN1")) # lista com os shapefiles

shplistnames<- gsub("G:/My Drive/Pesquisa/Coordenador de Projetos de Pesquisa 2019/SHAPE_2512309/","",shpdir) #cria uma variável com os nomes dos shapes

names(shplist)<- shplistnames # da os nomes dos shapes para a lista de shapes

classcodes<-read.table("classcodes.csv",header = T,sep = ";") # ler os códigos e classes do mapbiomas

caatingaraster<- raster("G:\\My Drive\\Pesquisa\\Coordenador de Projetos Pesquisa 2018\\Dados Caatinga\\CAATINGA.tif") # carrega banda específica


####################################
#### 4 Pré tratamento dos dados ####
####################################

### 4.1 Definindo as cores para cada classe

colfunc<- colorRampPalette(c("#174B02", "#44D808")) # criar rampa de cores verde escuro para verde claro
floresta<- colfunc(11) # definir cores para flores
colfunc<- colorRampPalette(c("#787603", "#E0DD08")) # criar rampa de cores amarelo escuro para amarelo claro
agricultura<- colfunc(6) #defiir cores para a agricultura
semvegetacao<- "#FA6713"
praia<- "#C70039"
infra<- "#747572"
rocha<- "#B9C0AD"
mineracao<- "#663302"
semvegetacao<- "#FA6713"
corposdagua<- "#0F2CB0"
rioslagooceano<- "#16BFDE"
aquicultura<- "#6116DE"
outra<- "#000000"
classcodes$colors<- c(floresta, agricultura, semvegetacao, praia, 
                      infra, rocha, mineracao, semvegetacao, corposdagua, rioslagooceano, aquicultura, outra) 

### 4.2 Cortando as classes pelos shapes

shpclasscrp <- lapply(shplist,function(x) crop(caatingaraster,x))

shpclassmask<- mapply(function(x,y) mask(x, y), shpclasscrp,shplist)

### 4.3 Calculando a área de cada uso

classareas <- lapply(shpclassmask,area) # cria um raster e calcula área para cada pixel (diferente por está em angular, WGS84)

classareaszones <- mapply(function (x,y) zonal(x, y, 'sum'),classareas,shpclassmask) # soma todos os pixels pelas classes/zonas

zonemerged<- lapply(classareaszones,function(x) merge(x,classcodes,by.x = "zone", by.y = "NEW.ID"))
                

#########################
#### 5 Gerando mapas ####
#########################

### 5.1 Transformar os arquivos para plotagem no ggplot

shpclassmaskDFSpatial <- as(shpclassmask$AREA_IMOVEL.shp, "SpatialPixelsDataFrame") # transformar para tabela de dados espaciais
shpclassmaskDFSpatial <- as.data.frame(shpclassmaskDFSpatial, xy = TRUE) # transformar para data frame 
colnames(shpclassmaskDFSpatial) <- c("classe", "x", "y") # dar nomes as colunas

### 5.2 Gerar os mapas

ggplot() +  
  geom_raster(data=shpclassmaskDFSpatial, aes(x=x, y=y, fill=as.factor(classe)))+
  geom_sf(data = st_as_sf(shplist$AREA_IMOVEL.shp),colour = "black", fill = NA) +
  labs(title="Uso do Solo na Área do Imóvel", 
       subtitle="Ano: 2018",caption = "Fonte dos Dados: Autor/MAPBIOMAS", 
       y="Latitude",x = "Longitude") +
  scale_fill_manual("Classes",labels = zonemerged$AREA_IMOVEL.shp$'COLEÇÃO.3...PORTUGUÊS' ,values = zonemerged$AREA_IMOVEL.shp$colors) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl") +
  coord_sf() +
  #annotate(geom = "table", x = -38, y = -7.7, label = list(zonemerged),
   #        vjust = 1, hjust = 0) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5), #legend.position = c(.1, .5),#
        panel.background = element_rect(fill = "aliceblue"), text = element_text(size=12),
        plot.title = element_text(hjust = 0, vjust = 0, face = "bold"))
