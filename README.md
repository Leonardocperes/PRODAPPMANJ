# PRODAPPMANJ
 
---
title: "Influência da APP na produção de madeira na floresta Amazônica"

author: "Leonardo C. Peres"

date: "`r Sys.Date()`"

description: Análise exploratória e estatística de dados da floresta Amazônica

output: pdf_document

always_allow_html: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
rm(list=ls(all=TRUE))                                 # Limpa memória
gc()

if(!require(remotes))
  install.packages('remotes', dependencies = TRUE)
library(remotes)

if(!require(klippy))
  remotes::install_github("rlesur/klippy")
library(klippy)

if(!require(rio))
  install.packages('rio', dependencies = TRUE)
if(!require(rlang))
  install.packages('rlang', dependencies = TRUE)
if(!require(tidyverse))
  install.packages('tidyverse', dependencies = TRUE)
if(!require(sf))
  install.packages('sf', dependencies = TRUE)

library(tidyverse)
library(rlang)
library(sf)
library(gridExtra)
```

```{r klippy, echo=FALSE, include=TRUE}
klippy::klippy()
```

------------------------------------------------------------------------

# Apresentação

  Neste projeto o objetivo foi utilizar a linguagem ***R*** para análisar a influência da presença de APP na produção em m³ de madeira e da quantidade de indivíduos presentes em diferêntes Unidades de Trabalho (UT) de manejo florestal realizado na floresta Amazônica.

------------------------------------------------------------------------

# Hipotese

-   **H0** A presença de APP nas UTs exerce influência na produção de madeira e quantidade de indivíduos;
-   **H1** A presença de APP nas UTs não exerce influência significativa na produção de madeira e quantidade de indivíduos;

------------------------------------------------------------------------

# Etapas

  Para realização do projeto o mesmo foi dividido em três etapas:

-   **Análise exploratória** Estudo dos dados e possibilidades de trabalhos
-   **Modelagem** Análise estatísticas para responder a hipótese
-   **Rmarkdow** Apresentação do projeto e dos códigos utilizados nas etapas anteriores

------------------------------------------------------------------------

# Dados

  Os dados utilizados são fornecidos pelo projeto **Floresta*R*** disponibilizados em um diretório publico do `GitHub`

  O projeto **Floresta*R*** reúne professores, estudantes e profissionais dedicados à redação da série **Floresta*R*** de livros sobre o uso do ***R*** como ferramenta de análise de dados florestais.

  Para mais informações a respeito do projeto acesse:
[github.com/LuizEstraviz/FlorestaR_dados](https://github.com/LuizEstraviz/FlorestaR_dados)

------------------------------------------------------------------------

# 1. Exploratória

  Nesta etapa do projeto, o objetivo foi verificar qual a relação existente entre a presença de APP na UT e a produção madeireira assim como o número de indivíduos presente.

  Para isso foi criado um data-frame `shapefile` contendo as 10 UTs, e suas correspondentes: (quantidade de indivíduos por hectare, volume total por hectare, volume médio, área de APP dentro da UT e área da UT). Com isso foram plotados gráficos afim de representar as relações existentes entre a área de APP

------------------------------------------------------------------------

## Abrindo os dados

```{r openData, message=FALSE, echo=TRUE}

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
dadosUts <- paste0(tmpd, "/shapes/UTs.shp")         %>% read_sf()
dadosApp <- paste0(tmpd, "/shapes/APP.shp")         %>% read_sf()

```
------------------------------------------------------------------------

## Gerando o `dataFrame` com os valores de volume e número de indivíduos por UT

```{r dfUt, message=FALSE, echo=TRUE}
#filtrando meus registros
registros <- dadosArv %>%
  filter(destino == "PAB" | destino =="CF") %>% 
  select(codARV,altura,dap,volume,codUT)

# Criar um dataframe com todas as codUT
todos_codUTs <- data.frame(unique(dadosUts$UT))
colnames(todos_codUTs) <- c("codUT")

#Gerando a média, a soma dos volumes e nº de espécies por UT
mediaVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, 
  data = registros, mean), by = "codUT", all.x = TRUE)#media
somaVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, 
  data = registros, sum), by = "codUT", all.x = TRUE)#Soma
contagemVolUts <- merge(todos_codUTs, aggregate(ifelse(is.na(volume), 0, volume) ~ codUT, 
  data = registros, FUN = length), by = "codUT", all.x = TRUE)#Qtd

#Unindo todos os valores em um dataframe apenas e renomenado as colunas
resumoUts <- merge(merge(mediaVolUts, somaVolUts, by = "codUT"), contagemVolUts, by = 
  "codUT")
colnames(resumoUts) <- c("codUT", "mediaVolume", "somaVolume", "numeroIndividuos")
resumoUts[is.na(resumoUts)] <- 0
```
------------------------------------------------------------------------

## Calculando a área de APP existente dentro de cada UT

```{r areaAPP, message=FALSE, echo=TRUE}

# Dividir o shp dadosAPP em feições por Uts -> APP por Uts
poligono_recortado <- st_intersection(dadosApp, dadosUts)
appUts <- st_cast(poligono_recortado, "MULTIPOLYGON")

#Calcular a área da app nas Uts
area <- round(st_area(appUts)/10000,3)

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

#Selecionando apenas as colunas que interessa
resumoUts <- resumoUts %>% 
  select(codUT,mediaVolume,somaVolume,numeroIndividuos,areaAPP,area,areaSApp) 

#Criando as colunas com as informações de volume/ha e individuos/ha
resumoUts$volumeHectare <- resumoUts$somaVolume / resumoUts$areaSApp
resumoUts$individuosHectare <- resumoUts$numeroIndividuos / resumoUts$areaSApp

head(resumoUts)

```

------------------------------------------------------------------------

## Plotagem dos gráficos de dispersão

```{r plotDisp, message=FALSE, echo=TRUE}

GraficoVolume<-ggplot()+
  ggtitle("produção/areaAPP")+
  geom_point(data = resumoUts, 
             aes(x= areaAPP, y = volumeHectare))+
  theme_bw()+
  xlab("Área da APP") + ylab("Produção m³/ha")

GraficoIndividuos<-ggplot()+
  ggtitle("Ind/areaAPP")+
  geom_point(data = resumoUts, 
             aes(x= areaAPP, y = individuosHectare))+
  theme_bw()+
  xlab("Área da APP") + ylab("Nº de indivíduos (nº/ha)")

GraficoIndVol<-ggplot()+
  ggtitle("Ind/produção")+
  geom_point(data = resumoUts, 
             aes(x= volumeHectare, y = individuosHectare))+
  theme_bw()+
  xlab("Produção m³/ha") + ylab("Nº de indivíduos (nº/ha)")

grid.arrange(GraficoVolume, GraficoIndividuos,GraficoIndVol, ncol = 3,nrow = 1)

```

  Ao analisar os gráficos percebe-se que não existe relação entre os agentes avaliados (APP X Produção) e (APP X N° de indivíduos). Sendo assim foram gerados dois mapas para entender melhor sobre a dinâmica dos parâmetros avaliados e sua UT correspondente
  
------------------------------------------------------------------------

## Mapas de localização dos blocos e seus respectivos valores de volume m³ por ha e quantidade de indivíduos

```{r plotMaps, message=FALSE, echo=TRUE}

colnames(resumoUts) <- c("UT", "mediaVolume", "somaVolume", "numeroIndividuos",
  "areaAPP", "area", "areaSApp", "volumeHectare", "individuosHectare")

dadosUts <- merge(dadosUts, resumoUts, by = "UT")

MapaVolUt <- ggplot() +  
  ggtitle("Produção m³/ha")+
  geom_sf(data = dadosUts, aes(fill = volumeHectare), colour = "red") +
  scale_fill_gradient(low = "blue", high = "orange") +
  geom_sf_text(data = dadosUts, aes(label = UT), size = 3) +
  geom_sf(data = dadosApp, colour = "red", fill='white', alpha = 0.5) +
  coord_sf(datum=st_crs(29190)) +     # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000))+
  labs(fill = "Produção m³/ha")

MapaIndlUt <- ggplot() + 
  ggtitle("Individuos/ha")+
  geom_sf(data = dadosUts, aes(fill = individuosHectare), colour = "red") +
  scale_fill_gradient(low = "blue", high = "orange",) +
  geom_sf_text(data = dadosUts, aes(label = UT), size = 3) +
  geom_sf(data = dadosApp, colour = "red", fill='white', alpha = 0.5) +
  coord_sf(datum=st_crs(29190)) +     # Especifica sistema de coord.
  scale_x_continuous(breaks = seq(from = 218500, to = 223000, by = 1000))+
  labs(fill = "Indivíduos/ha")

grid.arrange(MapaIndlUt, MapaVolUt, ncol = 2,nrow = 1)

```

# 2. Modelagem

  Diante dos resultados obtidos durante a etapa exploratória dos dados, optou-se por agrupar as UTs em 3 classes diferentes de acordo com a área de APP dentro de cada uma delas. Foram consideradas UTs com alta influência de APP aquelas que apresentaram área de APP maior que 15 hectares, média influência aquelas que a área da APP se encontrasse entre 7 e 15 hectares e as demais foram consideradas com baixa influência.
  
------------------------------------------------------------------------

## Criação da coluna categorizando as UTs de acordo com a área de APP dentro delas

```{r createCat, message=FALSE, echo=TRUE}

# Criar a nova coluna com base na condição
dadosUts$catAreaApp <- ifelse(dadosUts$areaAPP > 15, 2, ifelse(dadosUts$areaAPP > 7, 1, 
  0))

print(dadosUts)
```
  
------------------------------------------------------------------------

## Geração do mapa com as UTs classificadas

```{r plotMap, message=FALSE, echo=TRUE}

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
  scale_fill_manual(values = nomes_cores, labels = c("Baixa influência",
    "Média influência", "Alta influência")) +
  labs(fill = "Influência de APP")  # Altera o título da legenda

# Visualizar o gráfico
print(MapaClasseAPP)
```

------------------------------------------------------------------------

## ANOVA

  Em seguida, realizou-se a ANOVA para verificar a existência de diferencia significativa entre as categorias criadas em relação a produção de madeira (Tabela 1) e ao número de indivíduos (Tabela 2), considerando um nível de significância de 0,05.
  
------------------------------------------------------------------------
  
### ANOVA Volume

```{r ANOVAv, message=FALSE, echo=TRUE}

modeloAnovaVol <- aov(mediaVolume ~ catAreaApp, data = dadosUts)

# Resumir os resultados da ANOVA
resultadoAnovaVol <- summary(modeloAnovaVol)

resultadoVolTexto <- capture.output(print(resultadoAnovaVol))

# Visualizar o resultado
print(resultadoVolTexto)

```

### ANOVA nº indivíduos

```{r ANOVAi, message=FALSE, echo=TRUE}
modeloAnovaInd <- aov(individuosHectare ~ catAreaApp, data = dadosUts)

# Resumir os resultados da ANOVA
resultadoAnovaInd <- summary(modeloAnovaInd)

resultadoIndTexto <- capture.output(print(resultadoAnovaInd))

# Visualizar o resultado
print(resultadoIndTexto)

```

  Ambas as ANOVAS indicam que os dois parâmetros testados não apresentam diferencia significativa entre eles e a presença de APP nas UTs, pois o Pr(>f) possuem valores maiores que 0,05.
  
------------------------------------------------------------------------

# 3. Conclusão

De acordo com os resultados apresentados neste trabalho, a presença da APP nas unidades de trabalhos não são suficientes para gerarem alguma influência sobre a produção de madeira e a quantidade de indivíduos presentes, em um contexto geral considerando todos os indivíduos com DAP maior que 40.

Uma possibilidade seria replicar a mesma sequência lógica que gerou este resultado, porém filtrando o data-frame por espécies, com o objetivo de localizar espécies com maior aptidão a regiões próximas a APPs e outras com menor.

------------------------------------------------------------------------

**RStudioCloud - **[PRODAPPMANJ](https://posit.cloud/content/5925743)

