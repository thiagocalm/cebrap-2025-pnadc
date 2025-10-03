#'----------------------------------------
#'Roteiro trabalhos iniciais da PNADC em R
#'----------------------------------------

# instalando os pacotes necessários
install.packages("PNADcIBGE")
install.packages("survey")
install.packages("writexl")

# carregar os pacotes necessários
library(PNADcIBGE)
library(survey)
library(writexl)

# Importacao dos dados ----------------------------------------------------

# Baixando o banco de dados trimestral do 3º trimestre de 2016

pnad_2016_3t <- get_pnadc(
  year = 2016,
  quarter = 3,
  design = TRUE,
  vars = c("UF", "V2007","VD4009","VD4019")
)

class(pnad_2016_3t) # verificando a classe do objeto criado

# Algumas analises diretas da PNADC ---------------------------------------

# Frequência de Tipo de vínculo do trabalho
vinculo_16_3t <- svytotal(~VD4009,design = pnad_2016_3t, na.rm = TRUE)

# Resultado da frequência
vinculo_16_3t

# intervalo de confiança
confint(vinculo_16_3t)

# coeficiente de variação
cv(vinculo_16_3t)*100

# Fazendo tabela com o intervalo de confiança
vinculo_16_3t_ic <- data.frame(vinculo_16_3t, confint(vinculo_16_3t), cv(vinculo_16_3t)*100)

# imprimindo tabela com o intervalo de confiança
vinculo_16_3t_ic

# Exportacao --------------------------------------------------------------

# Exportacao em csv
write.csv2(vinculo_16_3t_ic,"C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/vinculo.csv")

# Produzindo uma tabela tirando a primeira coluna do índice e inserindo-a como dados da tabela
vinculo_16_3t_final <- data.frame(row.names(vinculo_16_3t_ic), vinculo_16_3t, confint(vinculo_16_3t), cv(vinculo_16_3t)*100)

# exportando para excel
write_xlsx(vinculo_16_3t_final, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/vinculo.xlsx")

###################################################################################################################
# Repetindo as etapas anteriores com os dados já baixados no computador ---------------------------------

# 1 - Atribuindo um Working diretory (precisa ser a pasta onde os arquivos que você baixou do site do IBGE estão)
# (etapa opcional, mas uma boa pratica)
setwd("C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/")

# 2 - Abrindo o banco do terceiro trimestre de 2016 que baixei no site do IBGE
Pnad_3trim_2016 <- read_pnadc(microdata = "PNADC_032016.txt", input_txt = "input_PNADC_trimestral.txt")

class(Pnad_3trim_2016)

# 3 - Atribuindo os labels ao banco de 2016 (opcional)
Pnad_3trim_2016 <- pnadc_labeller(data_pnadc = Pnad_3trim_2016, dictionary.file = "dicionario_PNADC_microdados_trimestral.xls")

class(Pnad_3trim_2016)

# 4 - Atribuindo o plano amostral ao banco de dados
Pnad_3trim_2016 <- pnadc_design(Pnad_3trim_2016)

class(Pnad_3trim_2016)

# 5 - Gerando alguns resultados

# Frequência de Tipo de vínculo do trabalho
vinculo_16_3t_v2 <- svytotal(~VD4009, Pnad_3trim_2016, na.rm = T)

# Fazendo tabela com o intervalo de confiança e coeficiente de variacao
vinculo_16_3t_ic_v2 <- data.frame(vinculo_16_3t, confint(vinculo_16_3t), cv(vinculo_16_3t)*100)

# Produzindo uma tabela tirando a primeira coluna do índice e inserindo-a como dados da tabela
vinculo_16_3t_final_v2 <- data.frame(row.names(vinculo_16_3t_ic_v2), vinculo_16_3t_v2, confint(vinculo_16_3t_v2), cv(vinculo_16_3t_v2)*100)

# exportando para excel
write_xlsx(vinculo_16_3t_final_v2, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/vinculo_v2.xlsx")
