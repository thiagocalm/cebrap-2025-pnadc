
library(PNADcIBGE)
library(survey)

# Baixando os 4 bancos trimestrais de 2021
P_22_t1 <- get_pnadc(year = 2022, quarter = 1, design = TRUE, vars = c("VD4009", "VD4002"))
P_22_t2 <- get_pnadc(year = 2022, quarter = 2, design = TRUE, vars = c("VD4009", "VD4002"))
P_22_t3 <- get_pnadc(year = 2022, quarter = 3, design = TRUE, vars = c("VD4009", "VD4002"))
P_22_t4 <- get_pnadc(year = 2022, quarter = 4, design = TRUE, vars = c("VD4009", "VD4002"))

# Vínculo dos empregados (VD4009)
Vinculo_t1 <- svytotal(~VD4009, P_22_t1, na.rm = T)
Vinculo_t2 <- svytotal(~VD4009, P_22_t2, na.rm = T)
Vinculo_t3 <- svytotal(~VD4009, P_22_t3, na.rm = T)
Vinculo_t4 <- svytotal(~VD4009, P_22_t4, na.rm = T)


Vinculo_t1
Vinculo_t2
Vinculo_t3
Vinculo_t4

# Salvando em csv para abrir no excel
write.csv2(Vinculo_t1, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/Vinculo_t1.csv")
write.csv2(Vinculo_t2, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/Vinculo_t2.csv")
write.csv2(Vinculo_t3, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/Vinculo_t3.csv")
write.csv2(Vinculo_t4, "C:/Users/User/Desktop/Aulas/Cebrap/Práticas em R/Vinculo_t4.csv")

# Condição de ocupação das pessoas (VD4009)
Condicao_t1 <- svytotal(~VD4002, P_22_t1, na.rm = T)
Condicao_t2 <- svytotal(~VD4002, P_22_t2, na.rm = T)
Condicao_t3 <- svytotal(~VD4002, P_22_t3, na.rm = T)
Condicao_t4 <- svytotal(~VD4002, P_22_t4, na.rm = T)

Condicao_t1
Condicao_t2
Condicao_t3
Condicao_t4

confint(Condicao_t1)
confint(Condicao_t2)
confint(Condicao_t3)
confint(Condicao_t4)
