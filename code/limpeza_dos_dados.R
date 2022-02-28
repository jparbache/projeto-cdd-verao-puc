library(dplyr)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(readr)
library(OpenLand)
library(readxl)

#Nomeando o dataset que será limpo
LULC_raw <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/input/LULC-unidades-de-conservacao-federal.csv")


#Limpando colunas desnecessárias
LULC_clean = LULC_raw %>%
  select(area:class_name)

#Arrumando a coluna de ano
LULC_clean$year <- gsub("^.{0,15}", "", LULC_clean$band)

#Removendo a coluna "band"
LULC_clean = LULC_clean %>%
  select(-band)

#Convertendo ano para valores numéricos
LULC_clean$year = as.numeric(LULC_clean$year)

#Reordenando as colunas para fazer mais sentido
LULC_clean = LULC_clean %>%
  select(year, class_name, class, area)


#Agregando as categorias (para diminuir o total de classes)
agg_class = read.csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/agg_class.csv") #arquivo de legenda obtido do site do mapbiomas
LULC_agregado = dplyr::full_join(agg_class, LULC_clean, by = "class", na.rm = TRUE)
LULC_agregado = LULC_agregado%>% filter(!is.na(area))  #retirando os NAs

#Ordenando pelo ano
LULC_agregado = LULC_agregado %>%
  arrange(year)

#Agrupando e somando as linhas redundantes referentes a mesma classe superior
LULC_ACF = LULC_agregado %>%
  group_by(Aggregated_class, year) %>%
  summarise(sum(area))

#Renomeando os nomes das colunas
LULC_ACF= LULC_ACF %>%
  rename(area = `sum(area)`) %>%
  rename(aggregate_class = Aggregated_class)



#Salvando o dataframe limpo  e pronto para a analise
write.csv(LULC_ACF, "C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/LULC_ACF.csv", row.names = FALSE)
write.csv(LULC_Clean, "C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/LULC_CLEAN.csv", row.names = FALSE)


