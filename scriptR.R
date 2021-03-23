#instalando & abrindo os pacotes
if(require(ffbase) == F) install.packages("ffbase", dependencies = TRUE); require(ffbase)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)



#definindo o direitório
setwd("C:/Users/natal/OneDrive/Documentos/CP-Mestrado/Dissertação")

#carregando a base de dados
Resoluções <- read_excel("dados_1.xlsx", sheet = 1)
relatórios <- read_excel("dados_1.xlsx", sheet = 2)
Agenda <- read_excel("dados_1.xlsx", sheet = 4)


#################### FILTRAGEM ############################

#Filtrar base de dados pela categoria AGENTES
ONU_Res <- subset(Resoluções, Agentes == "ONU")
Estados_Res <- subset(Resoluções, Agentes == "Estados")
Todos_Res <- subset(Resoluções, Agentes == "todos")
Tropas_Res <- subset(Resoluções, Agentes == "tropas")
Outros_Res <- subset(Resoluções, Agentes == "outros")

#Filtrar base de dados pela categoria NÍVEL
Internacional_Res <- subset(Resoluções, Nível == "internacional")
Nacional_Res <- subset(Resoluções, Nível == "nacional")
Local_Res <- subset(Resoluções, Nível == "local")

#Filtrar base de dados pela categoria ATIVIDADE FIM em Resoluções
Res_Prevenção_Proteção <- subset(Resoluções, Atividades_Fim == "Prevenção e Proteção")
Res_Combate <- subset(Resoluções, Atividades_Fim == "Combate à impunidade")
Res_Assistência <- subset(Resoluções, Atividades_Fim == "Assistência às vítimas")


#Filtrar base de dados pela categoria ATIVIDADE FIM em relatórios
Rel_Prevenção_Proteção <- subset(relatórios, Atividades_Fim == "Prevenção e Proteção")
Rel_Combate <- subset(relatórios, Atividades_Fim == "Combate à impunidade")
Rel_Assistência <- subset(relatórios, Atividades_Fim == "Assistência às vítimas")

#Filtrar base de dados pela categoria AGENTES
ONU_Rel <- subset(Resoluções, Agentes == "ONU")
table(ONU_Rel$Nível)


#Filtrar base de dados pela categoria ATIVIDADE FIM em Agenda
A_Prevenção_Proteção <- subset(relatórios, Atividades_Fim == "Prevenção e Proteção")
A_Combate <- subset(Agenda, Atividades_Fim == "Combate à impunidade")
A_Assistência <- subset(relatórios, Atividades_Fim == "Assistência às vítimas")




################ FREQUÊNCIA EM RESOLUÇÕES #################

# Frequência de outros Agentes
table(Outros_Res$AGENTESM)


# Frequência de Agentes
table(Resoluções$Agentes)

#Frequência Relativa de Agentes
Tab = table(Resoluções$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequência de Nível
table(Resoluções$Nível)

#Frequência Relativa de Nível
Tab = table(Resoluções$Nível)
Total = sum(Tab)
Tab/Total * 100 


#Frequência Absoluta de Atividades_Tipo
table(Resoluções$Atividades_Tipo)

#Frequência Relativa de Atividades_Tipo
Tab = table(Resoluções$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequência de Atividades_Fim
table(Resoluções$Atividades_Fim)

#Frequência Relativa de Atividades_Fim
Tab = table(Resoluções$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 


################ FREQUÊNCIA EM RELATÓRIOS #################

# Frequência de outros Agentes
table(Outros_Res$AGENTESM)


# Frequência de Agentes
table(relatórios$Agentes)

#Frequência Relativa de Agentes
Tab = table(relatórios$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequência de Nível
table(relatórios$Nível)

#Frequência Relativa de Nível
Tab = table(relatórios$Nível)
Total = sum(Tab)
Tab/Total * 100 


#Frequência Absoluta de Atividades_Tipo
table(relatórios$Atividades_Tipo)

#Frequência Relativa de Atividades_Tipo
Tab = table(relatórios$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequência de Atividades_Fim
table(relatórios$Atividades_Fim)

#Frequência Relativa de Atividades_Fim
Tab = table(relatórios$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 

# Frequência de combate em Combate à Impunidade
table(Rel_Combate$Atividades_Tipo)


################ FREQUÊNCIA EM AGENDA #################

# Frequência de combate
table(A_Combate$Atividades_Tipo)


# Frequência de Agentes
table(Agenda$Agentes)

#Frequência Relativa de Agentes
Tab = table(Agenda$Agentes)
Total = sum(Tab)
Tab/Total * 100 

# Frequência de Nível
table(Agenda$Nível)

#Frequência Relativa de Nível
Tab = table(Agenda$Nível)
Total = sum(Tab)
Tab/Total * 100 


#Frequência Absoluta de Atividades_Tipo
table(Agenda$Atividades_Tipo)

#Frequência Relativa de Atividades_Tipo
Tab = table(Agenda$Atividades_Tipo)
Total = sum(Tab)
Tab/Total * 100 

#Frequência de Atividades_Fim
table(Agenda$Atividades_Fim)

#Frequência Relativa de Atividades_Fim
Tab = table(Agenda$Atividades_Fim)
Total = sum(Tab)
Tab/Total * 100 




################## GRÁFICOS RESOLUÇÕES #########################
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")
y

# Gráfico 1
ggplot(Resoluções, aes(x = factor(Atividades_Tipo))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Tipo)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "percentagem", fill = "") +
   theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 2
ggplot(Resoluções, aes(x = factor(Atividades_Fim))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Fim)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 3
ggplot(data = Resoluções, aes(x = Atividades_Tipo, fill = Atividades_Fim)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Atividades_Fim") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 4
ggplot(data = Resoluções, aes(x = Atividades_Tipo, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 5
ggplot(data = Resoluções, aes(x = Atividades_Fim, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 6
ggplot(data = Resoluções, aes(x = Atividades_Tipo, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 7
ggplot(data = Resoluções, aes(x = Atividades_Fim, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 8
ggplot(data = Resoluções, aes(x = Nível, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


################## GRÁFICOS RELATÓRIOS #########################

# Gráfico 9
# Cor adicional
library(RColorBrewer)
# Definindo número de cores
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(8, "Set3"))(nb.cols)

ggplot(relatórios, aes(x = factor(Atividades_Tipo))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Tipo)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_manual(values = mycolors)

# Gráfico 10
ggplot(relatórios, aes(x = factor(Atividades_Fim))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Atividades_Fim)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 11
ggplot(data = relatórios, aes(x = Atividades_Tipo, fill = Atividades_Fim)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Atividades_Fim") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 12
ggplot(data = relatórios, aes(x = Atividades_Tipo, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


# Gráfico 13
ggplot(data = relatórios, aes(x = Atividades_Fim, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico 14
ggplot(data = relatórios, aes(x = Atividades_Tipo, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 15
ggplot(data = relatórios, aes(x = Atividades_Fim, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 16
ggplot(data = relatórios, aes(x = Nível, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

############# GRÁFICOS AGENDA ####################

# Gráfico 17
# Cor adicional
library(RColorBrewer)
# Definindo número de cores
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


# Gráfico 18
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

# Gráfico 19
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

# Gráfico 20
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


# Gráfico 21
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

# Gráfico 22
ggplot(data = Agenda, aes(x = Atividades_Tipo, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Tipo", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 23
ggplot(data = Agenda, aes(x = Atividades_Fim, fill = Nível)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Atividades_Fim", y = "contagem", fill = "Nível") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set2")

# Gráfico 24
ggplot(data = Agenda, aes(x = Nível, fill = Agentes)) +
  geom_bar() +  
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "contagem", fill = "Agentes") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")


################# APÊNDICE #######################

# Frequência dos agentes contidos na categoria ONU
table(ONU_Res$AGENTESM)

# Frequência dos agentes contidos na categoria outros
table(Outros_Res$AGENTESM)


# Gráfico A1
ggplot(Resoluções, aes(x = factor(Agentes))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Agentes)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Agentes", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico A2
ggplot(Resoluções, aes(x = factor(Nível))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Nível)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico A3
ggplot(relatórios, aes(x = factor(Agentes))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Agentes)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Agentes", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico A4
ggplot(relatórios, aes(x = factor(Nível))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Nível)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Gráfico A5
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

# Gráfico A6
ggplot(Agenda, aes(x = factor(Nível))) +  
  geom_bar(aes(y = (..count..)/sum(..count..), fill=Nível)) +
  theme(axis.text.x=element_text(angle = +90, hjust = 1)) +
  theme(text=element_text(family="Times New Roman", size=10)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Nível", y = "percentagem", fill = "") +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(fill = "white", colour = "grey10")) +
  theme(panel.grid.major = element_line(colour = "gray", linetype = "solid")) +
  scale_fill_brewer(palette = "Set3")

# Frequência das Atividades_Tipo para Agentes do Estado
table(Estados_Res$Atividades_Tipo)
table(ONU_Res$Atividades_Fim)
