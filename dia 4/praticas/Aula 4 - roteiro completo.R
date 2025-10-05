#' --------------------------------------------------
#' Aula 4 - PNADC em R
#' --------------------------------------------------


# Importacao de biblioteca ------------------------------------------------

install.packages("PNADcIBGE")
install.packages("survey")

library(PNADcIBGE)
library(survey)


# Variáveis que utilizaremos
# UF
# VD4002 - Condicao de ocupacao (VD4002 == 1, ocupada)
# VD4017 - Rendimento mensal efetivo do trabalho principal
# V2009 - Idade

# Importação da base de dados ---------------------------------------------

# Importação da base de dados 2024 concentrada na visita 1

pnad_2024_v1 <- get_pnadc(
  year = 2024,
  interview = 1,
  labels = FALSE
)

# Criacao de variaveis ----------------------------------------------------

# Potencial isento

pnad_2024_v1$variables <- transform(
  pnad_2024_v1$variables,
  renda_isencao = ifelse(
    test = VD4002 == "1" & VD4017 <= 5000,
    yes = "Potencial_Isencao",
    no = ifelse(test = VD4002 == "1", yes = "Sem_isencao", no = NA_character_)
  )
)

# Variavel de soma

pnad_2024_v1$variables <- transform(
  pnad_2024_v1$variables,
  valor = 1
)

## Criando uma base um pouco menos para o nosso publico

class(pnad_2024_v1$variables$VD4002)

pnad_ocupados <- subset(pnad_2024_v1, VD4002 == "1")

# remover base de dados completos

rm(pnad_2024_v1)

# limpar um pouco do espaco da memoria
invisible(gc())

# Analises ----------------------------------------------------------------

# 1 - Numero de pessoas ocupadas

tabela_1 <- svytotal(
  ~valor,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_1 # visualizando tabela

# 2 - Numero de pessoas potencialmente isentas do IRPF em 2024

tabela_2 <- svytotal(
  ~renda_isencao,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_2 # visualizando tabela

# 3 - Proporcao de pessoas potencialemnte isentas do IRPF em 2024

tabela_3 <- svymean(
  ~renda_isencao,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_3 # visualizando tabela

# 4 - Rendimento médio do trabalho principal por grupo de potencialmente isento

# Funcao 'svyby':
# Para utilizá-la são necessários os seguintes argumentos:
#  - A variável da qual se deseja calcular a quantidade;
#  - A variável que define os domínios;
#  - O objeto do plano amostral;
#  - A função utilizada para calcular a quantidade de interesse (svytotal, svymean, svyratio, svyquantile, …).

tabela_4 <- svyby(
  formula =~ VD4017,
  by =~ renda_isencao,
  FUN = svymean,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_4 # visualizando tabela

# 5 - Proporcao de pessoas potencialemnte isentas do IRPF em 2024 por UF

tabela_5 <- svyby(
  formula =~ renda_isencao,
  by =~ UF,
  FUN = svymean,
  design = pnad_ocupados,
  na.rm = TRUE
)

tabela_5 # visualizando tabela

# Avaliando nossas estimativas principais

confint(tabela_5)
cv(object=tabela_5) * 100


# Extra! ------------------------------------------------------------------
###
# Vamos plotar um mapa com essas estimativas
###

# bibliotecas necessarias
library(sf) # para trabalhar com dados espaciais em R. Se nao tiver instalado, "install.packages("sf")" antes
library(tidyverse) # se nao tiver instalado, rodar "install.packages("tidyverse")" antes
library(brazilmaps) # se nao tiver instalado, rodar "install.packages("brazilmaps")" antes

# importando camadas espaciais usando o pacote "brazilmaps"
uf_map <- get_brmap(
  geo = "State",
  class = "sf"
)

# tratando as estimativas como um banco de dados

tabela_mapa <- as_tibble(tabela_5)

# fazendo pequenas alteracoes na tabela

tabela_mapa <- select(tabela_mapa, c(UF,"Potencial_Isentos" = renda_isencaoPotencial_Isencao)) # selecionando variaveis

tabela_mapa <- mutate(tabela_mapa, Potencial_Isentos = round(Potencial_Isentos * 100,2)) # transformacao de variavel em %

tabela_mapa <- mutate(tabela_mapa, UF = as.numeric(UF)) # transformacao de variavel em classe numerica

##
# juntando dados espaciais e estimativas usando bind_cols()
##

# ordenando estados para terem a mesma sequencia

uf_map <- arrange(uf_map, State)
tabela_mapa <- arrange(tabela_mapa, UF)

estimativas_espaciais <- left_join(
  x = uf_map,
  y = tabela_mapa,
  by = c("State" = "UF"),
  keep = FALSE
)

estimativas_espaciais |>
  ggplot() +
  geom_sf(aes(fill = Potencial_Isentos),
          colour = "black", size = 0.1) +
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.6) +
  scale_fill_viridis_c(option = 2,direction = -1) +
  guides(fill = guide_colourbar(title = "%")) +
  labs(
    title = "Potencial parcela (%) de isentos do IRPFM em 2024 se a lei estivesse em vigor, por UF.",
    caption = "Fonte: IBGE/PNADC, acumulado da primeira visita."
  ) +
  # tira sistema cartesiano
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.caption = element_text(size = 8),
    legend.title = element_text(face = "bold", size = 9, hjust = 0, vjust = .5),
    legend.text = element_text(size = 8, hjust = 0, vjust = .5),
    legend.position = "bottom",
    axis.title = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 8, color = "#636363", hjust = .5, vjust = .5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 9, color = "#636363", hjust = .9, vjust = .5),
    panel.grid = element_line(color = "#f0f0f0",linewidth = .01),
    # panel.grid = element_line(colour = "grey"),
    panel.background = element_blank())


# Outras analises possiveis -----------------------------------------------

# Distribuicao etaria, genero, raca

# Por setor de atividade (agricultura, servico publico?)

# Por posicao na ocupacao (sao mais empregados, conta propria, etc?)

# ...
