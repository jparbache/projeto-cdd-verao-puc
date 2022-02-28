FROM rocker/tidyverse:4.0.0

RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('viridis')"
RUN R -e "install.packages('hrbrthemes')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('OpenLand')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('readxl')"

COPY /input /input
COPY /input/LULC-unidades-de-conservacao-federal.csv /input/LULC-unidades-de-conservacao-federal.csv
COPY /input/LULC-transitions-c.csv /input/LULC-transitions-c.csv

COPY /output /output
COPY /output/agg_class.csv /output/agg_class.csv
COPY /output/agg_legend_table.csv /output/agg_legend_table.csv
COPY /output/leg_traduzida.csv /output/leg_traduzida.csv
COPY /output/legend_table.csv /output/legend_table.csv
COPY /output/LULC_ACF.csv /output/LULC_ACF.csv
COPY /output/matriz_transicoes.xlsx /output/matriz_transicoes.xlsx
COPY /output/transitions_agregado.xlsx /output/transitions_agregado.xlsx
COPY /output/LULC_Clean.csv /output/LULC_Clean.csv

COPY /code /code
COPY /code/limpeza_dos_dados.R /code/limpeza_dos_dados.R
COPY /code/limpeza_dados_transition.R /code/limpeza_dados_transition.R
COPY /code/codigos_graficos_projeto.R /code/codigos_graficos_projeto.R


CMD Rscript /code/limpeza_dos_dados.R
CMD Rscript /code/limpeza_dados_transition.R
CMD Rscript /code/codigos_graficos_projeto.R
