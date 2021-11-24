#########################################################
## GRÁFICO DE COLUNAS NO GGPLOT                        ##
## EVITANDO QUE CATEGORIAS DO EIXO X SE SOBREPONHAM    ##
## INSERINDO TEXTO/LABEL NA FIGURA - FREQ. E %         ##
## DADOS HIPOTETICOS                                   ##
## NATALIA PAIVA - IESC UFRJ                           ##
#########################################################
 

library(ggplot2) # grafico
library(scales) # para usar comando percent
library(dplyr) # manipular dados; usar %>%


# criando "base de dados" ficticia

tipo <- sample(c("Caso novo", "Pós Óbito", "Recidiva",
                 "Reingresso após abandono", "Transferência",
                 "Não sabe"), size = 3000, replace = TRUE,
                  prob = c(0.30, 0.10, 0.25,0.15,0.05, 0.15))

sexo <- sample(c("Feminino", "Masculino"), size = 3000, replace = TRUE,  prob = c(0.40, 0.60))

dados <- data.frame(sexo, tipo)

dados[1:10,]


# 1 - Grafico de colunas para Tipo de entrada

ggplot(dados, aes(x= tipo)) +  
  geom_bar(fill = "navyblue")+ # cor azul escura
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "red", size= 3.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor vermelha e tamanho 3.5
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Tipo de entrada", y= "Nº de casos notificados",
       title= "Figura 1: Número de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_minimal()+ # minimal com linhas de grades suaves
  theme(text = element_text(size=10))  # tamanho das letras = 10



# 2- Grafico de colunas para Tipo de entrada

ggplot(dados, aes(x= tipo)) +  
  geom_bar(fill = "lightcoral")+ # cor coral
  geom_text(aes(label = paste0(..count.., " (", percent(..count../sum(..count..)),")")), 
            stat = "count", vjust = - 0.75, colour = "black", size= 2.75)+ # inserindo freq. absoluta e (%), posicao= - 0.75 (negativo = acima da coluna), cor preta e tamanho 2.75
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Tipo de entrada", y= "Nº de casos notificados",
       title= "Figura 2: Distribuição de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_classic()+ # tipo classico - bem clean!
  theme(text = element_text(size=12))  # tamanho das letras = 12


# 3- Grafico de colunas para Tipo de entrada (eixo Y em %)

# criando dataframe auxiliar p/ gerar %
tipo.aux <- dados %>% group_by(tipo) %>% tally() %>% mutate(porcent = n/sum(n)*100) %>% print()

ggplot(tipo.aux, aes(x= tipo, y = porcent)) +  
  geom_bar(stat= "identity", fill= "violetred2")+ # cor violeta
  geom_text(label= paste0(round(tipo.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
    nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  labs(x = "Tipo de entrada", y= "Porcentagem",
       title= "Figura 3: Distribuição de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_bw()+ # tema bw
  theme(axis.text.x = element_text(color = "blue", size = 10, angle = 30, hjust= 1),
        axis.text.y = element_text(color = "blue", size = 10),  
        axis.title.x = element_text(color = "green", size = 14),
        axis.title.y = element_text(color = "orange", size = 14),
        title = element_text(size= 8)) # como mexer nos textos dos eixos e titulo


# 4- Grafico de colunas para Tipo de entrada (eixo Y em % e ordem decrescente)

# criando dataframe auxiliar p/ gerar % 
# ops! se ja tiver criado tipo.aux nao precisa criar de novo
tipo.aux <- dados %>% group_by(tipo) %>% tally() %>% mutate(porcent = n/sum(n)*100) %>% print()

ggplot(tipo.aux, aes(x= reorder(tipo, - porcent), y = porcent)) +   # reorder ; negativo = decrescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tipo.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Tipo de entrada", y= "Nº de casos notificados",
       title= "Figura 4: Distribuição de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_bw() # tema bw


# 5- Grafico de colunas para Tipo de entrada (eixo Y em % e ordem crescente)

# criando dataframe auxiliar p/ gerar % 
# ops! se ja tiver criado tipo.aux nao precisa criar de novo
tipo.aux <- dados %>% group_by(tipo) %>% tally() %>% mutate(porcent = n/sum(n)*100) %>% print()

ggplot(tipo.aux, aes(x= reorder(tipo, + porcent), y = porcent)) +   # reorder ; positivo = crescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  geom_text(label= paste0(round(tipo.aux$porcent, 1), "%"), # inserindo porcent arredondado p/ 1 casa e o simbolo %
            nudge_x = 0, nudge_y = 0.85, size= 3) + # no eixo x = meio da barra, no eixo y = 0.85 acima da coluna, tamanho 3
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + # inserindo eixo Y em %
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Tipo de entrada", y= "Nº de casos notificados",
       title= "Figura 5: Distribuição de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_light() # tema light



# 6- Grafico de colunas para Tipo de entrada (ordem crescente e eixo Y freq absoluta)

# criando dataframe auxiliar p/ gerar % 
# ops! se ja tiver criado tipo.aux nao precisa criar de novo
tipo.aux <- dados %>% group_by(tipo) %>% tally() %>% mutate(porcent = n/sum(n)*100) %>% print()

ggplot(tipo.aux, aes(x= reorder(tipo,  n), y = n)) +   # reorder ; positivo = crescente
  geom_bar(stat= "identity", fill= "turquoise4")+ # cor turquesa
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  geom_text(label= tipo.aux$n, # inserindo freq absoluta
            nudge_y = 25, size= 3) +  # precisa mexer no nudge_y para aletrar posicao do texto
  labs(x = "Tipo de entrada", y= "Nº de casos notificados",
       title= "Figura 6: Número de casos de Tuberculose segundo Tipo de entrada",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_light() # tema light


# 7- Grafico de colunas para Tipo de entrada segundo sexo

# Nota que voltamos para base de dados original

ggplot(dados, aes(x= tipo)) +   
  geom_bar(aes(fill= sexo)) + # colunas empilhadas
  scale_fill_brewer(palette = "Pastel1") + #Pastel2, Dark1, Set2...
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  labs(x = "Tipo de entrada", y= "Nº de casos notificados", fill = "Sexo do paciente", #fill= titulo da legenda
       title= "Figura 7: Casos de Tuberculose - Tipo de entrada segundo sexo",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_minimal() + # tema minimal
  theme(legend.position = "bottom") # legenda embaixo


# veja mais cores brewer
library(RColorBrewer)
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=FALSE)



# 8- Grafico de colunas para Tipo de entrada segundo sexo

# Nota que voltamos para base de dados original

ggplot(dados, aes(x= tipo, group = sexo,  fill= sexo)) +   # reorder ; positivo = crescente
  geom_bar(aes(y= ..count..), stat="count", position=position_dodge()) + # position = "dodge" colunas lado a lado
  scale_fill_brewer(palette = "Set2", direction = -1) + # se remover ", direction = -1", Fem fica verde e Masc laranja
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) + # evita que categorias do eixo x se sobreponham
  geom_text(aes(label= ..count.., 
                y=..count..), stat="count", vjust=-.5, size = 3, position=position_dodge(.9)) +
  labs(x = "Tipo de entrada", y= "Nº de casos notificados", fill = "Sexo", #fill= titulo da legenda
       title= "Figura 8: Casos de Tuberculose - Tipo de entrada segundo sexo",
       subtitle = "MRJ, 2020",
       caption= "Fonte: Dados hipotéticos")+
  theme_minimal() + # tema minimal
  theme(legend.position = "top") # legenda no topo


