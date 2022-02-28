#Carregando os dados crus de transicao
library(readxl)
library(readr)

LULC_transitions <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/input/LULC-transitions-c.csv")

#Carregando dados da legenda das classes
library(readr)
legend_table <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/legend_table.csv")

###Juntando as bases
library(dplyr)
#Selecionando apenas as colunas desejadas para o bind
transitions_from = legend_table %>%
  select(Class, categoryValue)
#Renomenado a coluna que fara a chave
transitions_from= transitions_from %>%
  rename(From = Class)
#Dando o join e limpando os NAs
LULC_tr_from = dplyr::full_join(transitions_from, LULC_transitions, by = "From", na.rm = TRUE)
LULC_tr_from = LULC_tr_from%>% filter(!is.na(km2))  #retirando os NAs
#Limpando as colunas adicionais e renomeando algumas outras
LULC_tr_from = LULC_tr_from %>%
  select(-From)
LULC_tr_from = LULC_tr_from %>%
  rename(From = categoryValue)

#Selecionando apenas as colunas desejadas para o bind
transitions_to = legend_table %>%
  select(Class, categoryValue)
#Renomenado a coluna que fara a chave
transitions_to= transitions_to %>%
  rename(To = Class)
#Dando o join e limpando os NAs
LULC_tr_to = dplyr::full_join(transitions_to, LULC_tr_from, by = "To", na.rm = TRUE)
LULC_tr_to = LULC_tr_to%>% filter(!is.na(km2))  #retirando os NAs
#Limpando as colunas adicionais e renomeando algumas outras
LULC_tr_to = LULC_tr_to %>%
  select(-To)
LULC_tr_to = LULC_tr_to %>%
  rename(To = categoryValue)

###Testando o grafico de chord
library(OpenLand)
#Arrumando a legenda
legenda = legend_table %>%
  select(categoryValue:Color)
legenda = legenda %>% rename(color = Color)
legenda$categoryName <- factor(c("Fo", "FF","SF", "Mg",
                                 "FP","Nf","WL","GL",
                                 "OF","Pa","Ag","TC",
                                 "SC","Mo","Nv","BD",
                                 "UA","ON","Wa","No",
                                 "RO","Mn","Aq","Sa",
                                 "RL","PC","SB","Rc",
                                 "OT","MC","Cf","Ct",
                                 "OP","WR"),
                                    levels = c("Ag", "Ct", "Cf", "FP", "Mo", "MC",
                                               "OP", "OT", "PC", "Rc", "SB", "SC",
                                               "TC", "Fo", "FF", "Mg", "SF", "WR",
                                               "GL", "Nf", "OF", "RO", "Sa", "WL",
                                               "No", "BD", "Mn", "Nv", "ON", "UA",
                                               "Pa", "Aq", "RL", "Wa"))

chordDiagramLand(dataset = LULC_tr_to,
                 legendtable = legenda)

sankeyLand(dataset =  LULC_tr_to,
           legendtable = legenda)

#"Ag", "Ct", "Cf", "FP", "Mo", "MC",
#"OP", "OT", "PC", "Rc", "SB", "SC",
#"TC", "Fo", "FF", "Mg", "SF", "WR",
#"GL", "Nf", "OF", "RO", "Sa", "WL",
#"No", "BD", "Mn", "Nv", "ON", "UA",
#"Pa", "Aq", "RL", "Wa"

#"Ag", "Ct", "Cf", "FP", "Mo", "MC",
#"OP", "OT", "PC", "Rc", "SB", "SC",
#"TC", "Fo", "FF", "Mg", "SF", "WR",
#"GL", "Nf", "OF", "RO", "Sa", "WL",
#"No", "BD", "Mn", "Nv", "ON", "UA",
#"Pa", "Aq", "RL", "Wa"

#"FP","Nf","WL","GL",
#"OF","Pa","Ag","TC",
#"SC","Mo","Nv","BD",
#"UA","ON","Wa","No",
#"RO","Mn","Aq","Sa",
#"RL","PC","SB","Rc",
#"OT","MC","Cf","Ct",
#"OP","WR"

###Organizando para os dados agregados

#Selecionando apenas as colunas desejadas para o bind
agregado_from = legend_table %>%
  select(Aggregated_class, categoryValue)
#Renomenado a coluna que fara a chave
agregado_from = agregado_from %>%
  rename(From = categoryValue)
#Dando o join e limpando os NAs
LULC_agg_from = dplyr::full_join(agregado_from, LULC_tr_to, by = "From", na.rm = TRUE)
LULC_agg_from = LULC_agg_from%>% filter(!is.na(km2))  #retirando os NAs
#Retirando a coluna com codigos antigos
LULC_agg_from = LULC_agg_from %>% select(-From)
#Pegando o codigo de classe das categorias agregadas
chave_agg_from = legend_table %>% select(Aggregated_class, agg_categoryValue)
chave_agg_from = distinct(chave_agg_from)
LULC_agg_from = dplyr::left_join(LULC_agg_from, chave_agg_from, by = "Aggregated_class", na.rm = TRUE)
LULC_agg_from = LULC_agg_from %>% select(-Aggregated_class)
LULC_agg_from = LULC_agg_from %>% rename(From = agg_categoryValue)

#Selecionando apenas as colunas desejadas para o bind
agregado_to = legend_table %>%
  select(Aggregated_class, categoryValue)
#Renomenado a coluna que fara a chave
agregado_to = agregado_to %>%
  rename(To = categoryValue)
#Dando o join e limpando os NAs
LULC_agg_to = dplyr::full_join(agregado_to, LULC_agg_from, by = "To", na.rm = TRUE)
LULC_agg_to = LULC_agg_to%>% filter(!is.na(km2))  #retirando os NAs
#Retirando a coluna com codigos antigos
LULC_agg_to = LULC_agg_to %>% select(-To)
#Pegando o codigo de classe das categorias agregadas
chave_agg_from = legend_table %>% select(Aggregated_class, agg_categoryValue)
chave_agg_from = distinct(chave_agg_from)
LULC_agg_to = dplyr::left_join(LULC_agg_to, chave_agg_from, by = "Aggregated_class", na.rm = TRUE)
LULC_agg_to = LULC_agg_to %>% select(-Aggregated_class)
LULC_agg_to = LULC_agg_to %>% rename(To = agg_categoryValue)

#Agora e necessario dar um groupby para reduzir o numero de linhas
transitions_agregado = LULC_agg_to %>%
  group_by(From, To) %>%
  summarise(sum(km2))
#Adicionando as colunas restantes
transitions_agregado$QtPixel = transitions_agregado$`sum(km2)`*1108
transitions_agregado$yearFrom = 1985
transitions_agregado$yearTo = 2020
transitions_agregado$Interval = 35
transitions_agregado$Period = '1985-2020'
transitions_agregado = transitions_agregado %>% rename(km2 = 'sum(km2)')

agg_legend_table <- read_csv("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/agg_legend_table.csv")
agg_legend_table = agg_legend_table %>%
  rename(categoryValue = agg_categoryValue,
         categoryName = agg_categoryName,
         color = Color)
agg_legend_table = agg_legend_table %>% select(-Aggregated_class)
transitions_agregado = transitions_agregado %>% select(Period, From, To , km2, QtPixel, Interval, yearFrom, yearTo)
writexl::write_xlsx(transitions_agregado,  "C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/transitions_agregado.xlsx")

###Fazendo o grafico
matriz_transicoes <- read_excel("C:/Users/JoaoArbache/Desktop/verao/CienciadeDados/projeto-cdd-verao-puc/output/matriz_transicoes.xlsx")

#tirando o Log para suavizar
matriz_transicoes_log = matriz_transicoes
matriz_transicoes_log$km2 = log(matriz_transicoes_log$km2)


agg_legend_table$categoryName <- factor(c("Agricultura", "Floresta", "Formação natural não-florestal", "Não observado", "Área não-vegetada", "Pasto", 
                                      "Água"),
                                    levels = c("Floresta", "Formação natural não-florestal", "Pasto", "Agricultura", "Área não-vegetada", "Água", 
                                               "Não observado"))
#gráficos legais
chordDiagramLand(dataset = matriz_transicoes,
                 legendtable = agg_legend_table, legposition = c(-1.7, 1), legtitle = "Fluxo de Transição 1985-2020
                Àrea em log" )

sankeyLand(dataset = matriz_transicoes,
           legendtable = agg_legend_table)

sankeyLand(dataset = matriz_transicoes_log,
           legendtable = agg_legend_table)


#grafico sem floresta
matriz_sem_floresta = matriz_transicoes %>% 
  filter(From!="1") %>%
  filter(To!="1")
leg_sf = agg_legend_table %>%
  filter(categoryValue !="1")
leg_sf$categoryName <- factor(c("Ag", "Nf", "No", "Nv", "Pa", 
                                "Wa"),
                              levels = c("Nf", "Pa", "Ag", "Nv", "Wa", 
                                         "No"))
chordDiagramLand(dataset = matriz_sem_floresta,
                 legendtable = leg_sf)

sankeyLand(dataset = matriz_sem_floresta,
           legendtable = leg_sf)
