install.packages('tidyverse')
remove.packages("rlang")
install.packages("rlang")
library(tidyverse)
install.packages("rlang")
library(rlang)
install.packages("sf", dependencies = TRUE)
library(sf)

remove(list=ls())

#Definindo o diretório onde os arquivos estão armazenados
gitOnde <- "https://github.com/FlorestaR/dados/blob/main/1_AMAZON"
gitNome <- "shapes.zip"
gitArqv <- file.path(gitOnde, gitNome) %>% paste0("?raw=true")

#Criando diretório temporário e descompactando pasta zipada
tmpd <- tempdir(check = TRUE)                  # diretório temporário
zipf <- file.path(tmpd, "shapes.zip")            # arquivo temporário

if(!file.exists(zipf))      # garante download de dados binários (wb)
  download.file(gitArqv, mode="wb", destfile = zipf) 

unzip(zipf, exdir = tmpd)   # shape é unziped no diretório temporário
unlink(zipf)                                # deleta o arquivo zipado

# leitura das camadas 
dadosArv <- paste0(tmpd, "/shapes/Arvores.shp")     %>% read_sf()
#dadosUca <- paste0(tmpd, "/shapes/UCAs.shp")        %>% read_sf()
dadosUts <- paste0(tmpd, "/shapes/UTs.shp")         %>% read_sf()
dadosApp <- paste0(tmpd, "/shapes/APP.shp")         %>% read_sf()
#dadosHid <- paste0(tmpd, "/shapes/Hidrografia.shp") %>% read_sf()
#dadosEst <- paste0(tmpd, "/shapes/Estrada.shp")     %>% read_sf()
#dadosRam <- paste0(tmpd, "/shapes/Ramal.shp")       %>% read_sf()
#dadosPat <- paste0(tmpd, "/shapes/Patios.shp")      %>% read_sf()

#filtrando meus registros
registros <- dadosArv %>%                
  #filter(sp == "Cedrela odorata") %>% 
  #filter(destino == "PAB" & sp == "Cedrela odorata") %>%
  filter(destino == "PAB" | destino =="CF") %>% 
  select(codARV,altura,dap,volume,codUT)

#Plotando mapa com os dados .shp importados
ggplot() +      # plot das UTs, APPs e castanheiras (col por altura)
  geom_sf(data = dadosUts, colour = "red", fill=NA) +
  geom_sf(data = dadosApp, colour = "lightblue") +
  geom_sf(data = registros, aes(colour = altura), size = 1) +
  scale_color_distiller(palette = "Greens", trans = "reverse") + 
  coord_sf(datum=st_crs(29190)) +     # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000))

#Gerando a média, a soma dos volumes e nº de espécies por UT
#mediaVolUts <- aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, data = registros, mean)#Média

# Criar um dataframe com todas as codUT
todos_codUTs <- data.frame(unique(dadosUts$UT))
colnames(todos_codUTs) <- c("codUT")

#Gerando a média, a soma dos volumes e nº de espécies por UT
#mediaVolUts <- aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, data = registros, mean)#Média
mediaVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, data = registros, mean), by = "codUT", all.x = TRUE)#media
somaVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, data = registros, sum), by = "codUT", all.x = TRUE)#Soma
contagemVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, data = registros, FUN = length), by = "codUT", all.x = TRUE)#Qtd
#contagemVolUts <- aggregate(codARV ~ codUT, data = registros, FUN = length)#Qtd

#Unindo todos os valores em um dataframe apenas e renomenado as colunas
resumoUts <- merge(merge(mediaVolUts, somaVolUts, by = "codUT"), contagemVolUts, by = "codUT")
colnames(resumoUts) <- c("codUT", "mediaVolume", "somaVolume", "numeroIndividuos")
resumoUts[is.na(resumoUts)] <- 0

# Dividir o shp dadosAPP em feições por Uts -> APP por Uts
poligono_recortado <- st_intersection(dadosApp, dadosUts)
appUts <- st_cast(poligono_recortado, "MULTIPOLYGON")
print(appUts)

#Calcular a área da app nas Uts
area <- round(st_area(appUts)/10000,3)
print(area)
#Removendo a unidade de medida do texto e transformando em número
appUts$AreaAPP <- as.numeric(gsub("[m^]", "", as.character(area)))

#Agrupando os valores de área da app pelas Uts e renomenado as colunas
areaAppUts <- aggregate(AreaAPP ~ UT, data = appUts, sum)
colnames(areaAppUts) <- c("codUT", "areaAPP")

#Enviando os dados da tabela areaAppUts para o shp resumoUts através d ID
resumoUts <- merge(resumoUts, areaAppUts, by = "codUT")

#Inserindo o valor bruto de área das Uts 
resumoUts$area <- 100.00

#calculando a coluna de área liquida area com app - area da app
resumoUts$areaSApp <- resumoUts$area - resumoUts$areaAPP

head(resumoUts)

#Selecionando apenas as colunas que interessa
resumoUts <- resumoUts %>% 
  select(codUT,mediaVolume,somaVolume,numeroIndividuos,areaAPP,area,areaSApp) 

#Criando as colunas com as informações de volume/ha e individuos/ha
resumoUts$volumeHectare <- resumoUts$somaVolume / resumoUts$areaSApp
resumoUts$individuosHectare <- resumoUts$numeroIndividuos / resumoUts$areaSApp

#Gerando os gráficos
GraficoVolume<-ggplot()+
  ggtitle("produção/areaAPP")+
  geom_point(data = resumoUts, 
             aes(x= areaAPP, y = volumeHectare))+
  #geom_point(data = dados_outliers,aes(x= idadeOld, y = vol),col='red')+
  #geom_point(data = dados_limpos,aes(x= idadeOld, y = vol),col='darkgreen')+
  theme_bw()+
  xlab("Área da APP") + ylab("Produtividade por ha")
plot(GraficoVolume)

GraficoIndividuos<-ggplot()+
  ggtitle("Ind/areaAPP")+
  geom_point(data = resumoUts, 
             aes(x= areaAPP, y = individuosHectare))+
  #geom_point(data = dados_outliers,aes(x= idadeOld, y = vol),col='red')+
  #geom_point(data = dados_limpos,aes(x= idadeOld, y = vol),col='darkgreen')+
  theme_bw()+
  xlab("Área da APP") + ylab("Nº de indivíduos (nº/ha)")
plot(GraficoIndividuos)

GraficoIndVol<-ggplot()+
  ggtitle("Ind/produção")+
  geom_point(data = resumoUts, 
             aes(x= volumeHectare, y = individuosHectare))+
  #geom_point(data = dados_outliers,aes(x= idadeOld, y = vol),col='red')+
  #geom_point(data = dados_limpos,aes(x= idadeOld, y = vol),col='darkgreen')+
  theme_bw()+
  xlab("Produtividade por ha") + ylab("Nº de indivíduos (nº/ha)")
plot(GraficoIndVol)

colnames(resumoUts) <- c("UT", "mediaVolume", "somaVolume", "numeroIndividuos", "areaAPP", "area", "areaSApp", "volumeHectare", "individuosHectare")

dadosUts <- merge(dadosUts, resumoUts, by = "UT")

MapaVolUt <- ggplot() +  
  ggtitle("Produção/ha")+
  geom_sf(data = dadosUts, aes(fill = volumeHectare), colour = "red") +
  scale_fill_gradient(low = "blue", high = "orange") +
  geom_sf_text(data = dadosUts, aes(label = UT), size = 3) +
  geom_sf(data = dadosApp, colour = "red", fill='white', alpha = 0.5) +
  coord_sf(datum=st_crs(29190)) +     # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000))+
  labs(fill = "Produção/ha")
plot(MapaVolUt)

MapaIndlUt <- ggplot() + 
  ggtitle("Individuos/ha")+
  geom_sf(data = dadosUts, aes(fill = individuosHectare), colour = "red") +
  scale_fill_gradient(low = "blue", high = "orange",) +
  geom_sf_text(data = dadosUts, aes(label = UT), size = 3) +
  geom_sf(data = dadosApp, colour = "red", fill='white', alpha = 0.5) +
  coord_sf(datum=st_crs(29190)) +     # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000))+
  labs(fill = "Indivíduos/ha")
plot(MapaIndlUt)

library(gridExtra)
grid.arrange(GraficoVolume, GraficoIndividuos,GraficoIndVol, ncol = 3,nrow = 1)
grid.arrange(MapaIndlUt, MapaVolUt, ncol = 2,nrow = 1)

# Criar a nova coluna com base na condição
dadosUts$catAreaApp <- ifelse(dadosUts$areaAPP > 15, 2, ifelse(dadosUts$areaAPP > 7, 1, 0))

# Visualizar o resultado
print(dadosUts)

modeloAnovaVol <- aov(mediaVolume ~ catAreaApp, data = dadosUts)

# Resumir os resultados da ANOVA
resultadoAnovaVol <- summary(modeloAnovaVol)

resultadoVolTexto <- capture.output(print(resultadoAnovaVol))

# Visualizar o resultado
print(resultadoVolTexto)

modeloAnovaInd <- aov(individuosHectare ~ catAreaApp, data = dadosUts)

# Resumir os resultados da ANOVA
resultadoAnovaInd <- summary(modeloAnovaInd)

resultadoIndTexto <- capture.output(print(resultadoAnovaInd))

# Visualizar o resultado
print(resultadoIndTexto)

# Converter a variável catAreaApp para o tipo de dados factor
dadosUts$catAreaApp <- factor(dadosUts$catAreaApp)

# Definir cores para cada valor da variável catAreaApp
cores <- c("orange", "blue", "pink")
nomes_cores <- setNames(cores, levels(dadosUts$catAreaApp))

# Plotar o gráfico com cores por valor
MapaClasseAPP <- ggplot() + 
  ggtitle("Classe area/APP") +
  geom_sf(data = dadosUts, aes(fill = catAreaApp), colour = "red") +
  geom_sf_text(data = dadosUts, aes(label = UT), size = 3) +
  geom_sf(data = dadosApp, colour = "red", fill = 'white', alpha = 0.5) +
  coord_sf(datum = st_crs(29190)) +  # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000)) +
  scale_fill_manual(values = nomes_cores, labels = c("Baixa influência", "Média influência", "Alta influência")) +
  labs(fill = "Influência de APP")  # Altera o título da legenda

# Visualizar o gráfico
print(MapaClasseAPP)

