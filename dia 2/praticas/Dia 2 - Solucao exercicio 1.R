#--------------------------------
#  Dia 3
#  Exercicio 1 - Resolução
#--------------------------------

# Configuração do diretorio de trabalho ------------------------

setwd("C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/") # configura o nosso NOVO diretório de trabalho

getwd() # verificação se foi feita a mudança de diretório

trabalhadores <- read.csv("Trabalhadores.txt") # importação da base de dados

# comandos para entender a estrutura do banco de dados

View(trabalhadores) # visualizar base de dados como um todo
head(trabalhadores) # visualizar as primeiras linhas
tail(trabalhadores) # visualizar ultimas linhas
str(trabalhadores) # estrutura das variaveis
class(trabalhadores) # classe da base dados
class(trabalhadores$Nome) # classe da variável Nome na base de dados
class(trabalhadores$Idade) # classe da variável Idade na base de dados


# 1 - Media salarial dos trabalhadores ------------------------------------

# Criando coluna para a media dos salarios

trabalhadores$soma_salarios <- (trabalhadores$Salario_atual + trabalhadores$Salario_anterior)

# Criando variável para a média dos salarios

trabalhadores$media_salarios <- trabalhadores$soma_salarios/2

class(trabalhadores$media_salarios)

# Visualizar a media salarial dos trabalhadores
trabalhadores$media_salarios


# 2 - Coluna dizendo qual salario é maior ---------------------------------

trabalhadores$salario_categorico <- ifelse(
  trabalhadores$Salario_atual > trabalhadores$Salario_anterior, yes = "MAIOR", no = "MENOR"
)

# Visualizar a variavel de salario categorica criada
trabalhadores$salario_categorico

table(trabalhadores$salario_categorico)

# 3 - Qual a media de idade de todos os trabalhadores? --------------------

# Calculando media de idade de todos os trabalhadores

mean(trabalhadores$Idade, na.rm = T)
