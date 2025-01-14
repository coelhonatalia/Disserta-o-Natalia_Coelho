#instalando & abrindo os pacotes
if(require(ffbase) == F) install.packages("ffbase", dependencies = TRUE); require(ffbase)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)



#definindo o direit�rio
setwd("C:/Users/natal/OneDrive/Documentos/CP-Mestrado/Disserta��o")

#carregando a base de dados
Resolu��es <- read_excel("dados_1.xlsx", sheet = 1)
relat�rios <- read_excel("dados_1.xlsx", sheet = 2)
Agenda <- read_excel("dados_1.xlsx", sheet = 4)


#################### FILTRAGEM ############################

#Filtrar base de dados pela categoria AGENTES
ONU_Res <- subset(Resolu��es, Agentes == "ONU")
Estados_Res <- subset(Resolu��es, Agentes == "Estados")
Todos_Res <- subset(Resolu��es, Agentes == "todos")
Tropas_Res <- subset(Resolu��es, Agentes == "tropas")
Outros_Res <- subset(Resolu��es, Agentes == "outros")

#Filtrar base de dados pela categoria N�VEL
Internacional_Res <- subset(Resolu��es, N�vel == "internacional")
Nacional_Res <- subset(Resolu��es, N�vel == "nacional")
Local_Res <- subset(Resolu��es, N�vel == "local")

#Filtrar base de dados pela categoria ATIVIDADE FIM em Resolu��es
Res_Preven��o_Prote��o <- subset(Resolu��es, Atividades_Fim == "Preven��o e Prote��o")
Res_Combate <- subset(Resolu��es, Atividades_Fim == "Combate � impunidade")
Res_Assist�ncia <- subset(Resolu��es, Atividades_Fim == "Assist�ncia �s v�timas")


#Filtrar base de dados pela categoria ATIVIDADE FIM em relat�rios
Rel_Preven��o_Prote��o <- subset(relat�rios, Atividades_Fim == "Preven��o e Prote��o")
Rel_Combate <- subset(relat�rios, Atividades_Fim == "Combate � impunidade")
Rel_Assist�ncia <- subset(relat�rios, Atividades_Fim == "Assist�ncia �s v�timas")

#Filtrar base de dados pela categoria AGENTES
ONU_Rel <- subset(Resolu��es, Agentes == "ONU")
table(ONU_Rel$N�vel)


#Filtrar base de dados pela categoria ATIVIDADE FIM em Agenda
A_Preven��o_Prote��o <- subset(relat�rios, Atividades_Fim == "Preven��o e Prote��o")
A_Combate <- subset(Agenda, Atividades_Fim == "Combate � impunidade")
A_Assist�ncia <- subset(relat�rios, Atividades_Fim == "Assist�ncia �s v�timas")




################ FREQU�NCIA EM RESOLU��ES #################

# Frequ�ncia de outros Agentes
table(Outros_Res$AGENTESM)


# Frequ�ncia de Agentes
table(Resolu��es$Agentes)

#Frequ�ncia Relativa de Agentes
Tab = table(Resolu��es$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequ�ncia de N�vel
table(Resolu��es$N�vel)

#Frequ�ncia Relativa de N�vel
Tab = table(Resolu��es$N�vel)
Total = sum(Tab)
Tab/Total * 100 


#Frequ�ncia Absoluta de Atividades_Tipo
table(Resolu��es$Atividades_Tipo)

#Frequ�ncia Relativa de Atividades_Tipo
Tab = table(Resolu��es$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequ�ncia de Atividades_Fim
table(Resolu��es$Atividades_Fim)

#Frequ�ncia Relativa de Atividades_Fim
Tab = table(Resolu��es$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 


################ FREQU�NCIA EM RELAT�RIOS #################

# Frequ�ncia de outros Agentes
table(Outros_Res$AGENTESM)


# Frequ�ncia de Agentes
table(relat�rios$Agentes)

#Frequ�ncia Relativa de Agentes
Tab = table(relat�rios$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequ�ncia de N�vel
table(relat�rios$N�vel)

#Frequ�ncia Relativa de N�vel
Tab = table(relat�rios$N�vel)
Total = sum(Tab)
Tab/Total * 100 


#Frequ�ncia Absoluta de Atividades_Tipo
table(relat�rios$Atividades_Tipo)

#Frequ�ncia Relativa de Atividades_Tipo
Tab = table(relat�rios$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequ�ncia de Atividades_Fim
table(relat�rios$Atividades_Fim)

#Frequ�ncia Relativa de Atividades_Fim
Tab = table(relat�rios$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 

# Frequ�ncia de combate em Combate � Impunidade
table(Rel_Combate$Atividades_Tipo)


################ FREQU�NCIA EM AGENDA #################

# Frequ�ncia de combate
table(A_Combate$Atividades_Tipo)


# Frequ�ncia de Agentes
table(Agenda$Agentes)

#Frequ�ncia Relativa de Agentes
Tab = table(Agenda$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequ�ncia de N�vel
table(Agenda$N�vel)

#Frequ�ncia Relativa de N�vel
Tab = table(Agenda$N�vel)
Total = sum(Tab)
Tab/Total * 100 


#Frequ�ncia Absoluta de Atividades_Tipo
table(Agenda$Atividades_Tipo)

#Frequ�ncia Relativa de Atividades_Tipo
Tab = table(Agenda$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequ�ncia de Atividades_Fim
table(Agenda$Atividades_Fim)

#Frequ�ncia Relativa de Atividades_Fim
Tab = table(Agenda$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 




################## GR�FICOS RESOLU��ES #########################
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")
y

# Gr�fico 1
ggplot(Resolu��es, aes(x = factor(Atividades_Tipo))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Tipo)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "percentagem", fill = "") +
   theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico 2
ggplot(Resolu��es, aes(x = factor(Atividades_Fim))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Fim)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 3
ggplot(data = Resolu��es, aes(x = Atividades_Tipo, fill = Atividades_Fim)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Atividades_Fim") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 4
ggplot(data = Resolu��es, aes(x = Atividades_Tipo, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico 5
ggplot(data = Resolu��es, aes(x = Atividades_Fim, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico 6
ggplot(data = Resolu��es, aes(x = Atividades_Tipo, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 7
ggplot(data = Resolu��es, aes(x = Atividades_Fim, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 8
ggplot(data = Resolu��es, aes(x = N�vel, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


################## GR�FICOS RELAT�RIOS #########################

# Gr�fico 9
# Cor adicional
library(RColorBrewer)
# Definindo n�mero de cores
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

ggplot(relat�rios, aes(x = factor(Atividades_Tipo))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Tipo)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = mycolors)

# Gr�fico 10
ggplot(relat�rios, aes(x = factor(Atividades_Fim))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Fim)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 11
ggplot(data = relat�rios, aes(x = Atividades_Tipo, fill = Atividades_Fim)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Atividades_Fim") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 12
ggplot(data = relat�rios, aes(x = Atividades_Tipo, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


# Gr�fico 13
ggplot(data = relat�rios, aes(x = Atividades_Fim, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico 14
ggplot(data = relat�rios, aes(x = Atividades_Tipo, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 15
ggplot(data = relat�rios, aes(x = Atividades_Fim, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 16
ggplot(data = relat�rios, aes(x = N�vel, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

############# GR�FICOS AGENDA ####################

# Gr�fico 17
# Cor adicional
library(RColorBrewer)
# Definindo n�mero de cores
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

ggplot(Agenda, aes(x = factor(Atividades_Tipo))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Tipo)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = mycolors)


# Gr�fico 18
ggplot(Agenda, aes(x = factor(Atividades_Fim))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Fim)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 19
ggplot(data = Agenda, aes(x = Atividades_Tipo, fill = Atividades_Fim)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Atividades_Fim") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 20
ggplot(data = Agenda, aes(x = Atividades_Tipo, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


# Gr�fico 21
ggplot(data = Agenda, aes(x = Atividades_Fim, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico 22
ggplot(data = Agenda, aes(x = Atividades_Tipo, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 23
ggplot(data = Agenda, aes(x = Atividades_Fim, fill = N�vel)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "N�vel") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gr�fico 24
ggplot(data = Agenda, aes(x = N�vel, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


################# AP�NDICE #######################

# Frequ�ncia dos agentes contidos na categoria ONU
table(ONU_Res$AGENTESM)

# Frequ�ncia dos agentes contidos na categoria outros
table(Outros_Res$AGENTESM)


# Gr�fico A1
ggplot(Resolu��es, aes(x = factor(Agentes))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Agentes)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Agentes", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico A2
ggplot(Resolu��es, aes(x = factor(N�vel))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=N�vel)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico A3
ggplot(relat�rios, aes(x = factor(Agentes))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Agentes)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Agentes", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico A4
ggplot(relat�rios, aes(x = factor(N�vel))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=N�vel)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico A5
ggplot(Agenda, aes(x = factor(Agentes))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Agentes)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Agentes", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gr�fico A6
ggplot(Agenda, aes(x = factor(N�vel))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=N�vel)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "N�vel", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Frequ�ncia das Atividades_Tipo para Agentes do Estado
table(Estados_Res$Atividades_Tipo)
table(ONU_Res$Atividades_Fim)
