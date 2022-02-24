###########GRÁFICOS FINAIS

library(ggplot2)
library(viridis)
library(hrbrthemes)
library(readr)
library(OpenLand)
library(dplyr)
library(readxl)

####GRÁFICOS DA SÉRIE DE TEMPO LULC

##DADOS DESAGREGADOS
LULC_desagregado <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/LULC_Clean.csv")


#SERIE - PASTO
pasto = LULC_desagregado %>% filter(class_name == "Pasture")

ggplot(pasto, aes(y=area, x=year)) + 
  geom_bar(position="stack", stat="identity", fill = "#FFD966") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Área de Pastagem em Unidades de Conservação Federal", 
        subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Área (Km²)")


#SERIE SOJA
soja = LULC_desagregado %>% filter(class_name == "Soy Beans")

ggplot( soja, aes(y=area, x=year)) + 
  geom_bar(position="stack", stat="identity", fill = "#e075ad") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Área de Soja em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)")

#SERIE MINERAÇÃO
mineiracao = LULC_desagregado %>% filter(class_name == "Mining")

ggplot( mineiracao, aes( y=area, x=year)) + 
  geom_bar(position="stack", stat="identity", fill = "#af2a2a") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Mineiração em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)") 

##DADOS AGREGADOS
LULC_ACF <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/LULC_ACF.csv")
LULC_ACF = LULC_ACF %>% rename( area = 'sum(area)')

#PARA TODOS OS DADOS JUNTOS
LULC_ACF$log_area = log(LULC_ACF$area)
leg_traduzida <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/leg_traduzida.csv")
LULC_ACF = dplyr::left_join(LULC_ACF, leg_traduzida, by = "Aggregated_class")


#estatisticas
floresta_apenas = LULC_ACF %>% filter(Classe == "Floresta")
mean(floresta_apenas$area)
332715.9/381420.3
fnnf = LULC_ACF %>% filter(Classe == "Formação Natural não Florestal")
mean(fnnf$area)
36567.15/381420.3 + 332715.9/381420.3


#log
ggplot( LULC_ACF, aes(fill= Classe, y= log_area, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Uso da Terra em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("log(Area (Km²))") 

#nível
ggplot( LULC_ACF, aes(fill= Classe, y= area, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Uso da Terra em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)") 


#AGRICULTURA
agricultura = LULC_ACF %>% filter(Aggregated_class == "Agriculture")

  
ggplot(agricultura, aes(fill= Aggregated_class, y= area, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Área de Agricultura em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)")

#ÀGUA
agua  = LULC_ACF %>% filter(Aggregated_class == "Water")

ggplot(agua, aes(fill= Aggregated_class, y= area, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Área de Água em Unidades de Conservação Federal", subtitle = "Período: 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Area (Km²)")

#VARIAÇÃO DAS CLASSES AGREGADAS

#calculando a variacao nos valores das classes agregadas  de uso da terra entre 2020 e 1985
variacao = LULC_ACF %>%
  filter(year == c(1985, 2020))
variacao = variacao %>%
  group_by(Classe) %>%
  summarise(variation = area[year == 2020] - area[year == 1985])
variacao = variacao %>% filter(Classe != "Não observado")

ggplot(variacao, aes(fill= Classe, y=variation, x= Classe)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  labs( title = "Variação do Uso da Terra por Classe", subtitle = "Variação em Unidades de Conservação Federal no Período 1985-2020", caption = "Fonte: Mapbiomas") +
  theme_ipsum() +
  xlab("") +
  ylab("Variação (Km²)") +
  geom_text(aes(label= round(variation)), vjust=-0.5, size=3.0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


####GRÁFICOS PARA OS DADOS DE TRANSIÇÃO

matriz_transicoes <- read_excel("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/matriz_transicoes.xlsx")

#tirando o Log para suavizar
matriz_transicoes_log = matriz_transicoes
matriz_transicoes_log$km2 = log(matriz_transicoes_log$km2)

agg_legend_table <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/agg_legend_table.csv")

agg_legend_table$categoryName <- factor(c("Agricultura", "Floresta", "Formação natural não-florestal", "Não observado", "Área não-vegetada", "Pasto", 
                                          "Água"),
                                        levels = c("Floresta", "Formação natural não-florestal", "Pasto", "Agricultura", "Área não-vegetada", "Água", 
                                                   "Não observado"))
agg_legend_table = agg_legend_table %>% rename(categoryValue = "agg_categoryValue") %>% rename(color = "Color")


#grafico de corda
chordDiagramLand(dataset = matriz_transicoes,
                 legendtable = agg_legend_table, legposition = c(-1.7, 1), legtitle = "Fluxo de Transição 1985-2020
                Àrea em log" )

chordDiagramLand(dataset = matriz_transicoes_log,
                 legendtable = agg_legend_table, legposition = c(-1.7, 1), legtitle = "Fluxo de Transição 1985-2020
                Àrea em log" )

#graficos sankey
sankeyLand(dataset = matriz_transicoes,
           legendtable = agg_legend_table)

sankeyLand(dataset = matriz_transicoes_log,
           legendtable = agg_legend_table)
