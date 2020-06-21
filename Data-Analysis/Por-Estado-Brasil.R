#https://covid.saude.gov.br/
#https://www.google.com/covid19/mobility/

require('gganimate')
require('ggplot2')
require('magick')


#BD =  read.csv("Data/HIST_PAINEL_COVIDBR_19mai2020.csv", header = TRUE, sep=",")
#BD =  read.csv("Data/HIST_PAINEL_COVIDBR_21mai2020.csv", header = TRUE, sep=",")

BD =  read.csv("Data/HIST_PAINEL_COVIDBR_20jun2020.csv", header = TRUE, sep=",")


#BBBD = BD[!is.na(BD$emAcompanhamentoNovos),c(8,10,11,12,13,14,15,16)]
#BBBD$data <- as.Date(as.character(as.Date(BBBD$data)), format="%d-%m-%y")
#barplot(BBBD$emAcompanhamentoNovos/1000~BBBD$data, ylab="", main="Casos de covid em acompanhamento (por mil) no Brasil", ylim=c(0,500), xlab="")


BD$Dens=NA
BD$Dens[BD$estado=="SP"] <- 184.99
BD$Dens[BD$estado=="MG"] <- 36.09
BD$Dens[BD$estado=="RJ"] <- 394.62
BD$Dens[BD$estado=="BA"] <- 26.34
BD$Dens[BD$estado=="PR"] <- 57.37
BD$Dens[BD$estado=="RS"] <- 40.39
BD$Dens[BD$estado=="PE"] <- 97.45
BD$Dens[BD$estado=="CE"] <- 61.33
BD$Dens[BD$estado=="PA"] <- 6.91
BD$Dens[BD$estado=="SC"] <- 74.84
BD$Dens[BD$estado=="MA"] <- 21.46
BD$Dens[BD$estado=="GO"] <- 20.63
BD$Dens[BD$estado=="AM"] <- 2.66
BD$Dens[BD$estado=="ES"] <- 87.22
BD$Dens[BD$estado=="PB"] <- 71.16
BD$Dens[BD$estado=="RN"] <- 66.41
BD$Dens[BD$estado=="MT"] <- 3.86
BD$Dens[BD$estado=="AL"] <- 119.86
BD$Dens[BD$estado=="PI"] <- 13.01
BD$Dens[BD$estado=="DF"] <- 523.41
BD$Dens[BD$estado=="MS"] <- 7.78
BD$Dens[BD$estado=="SE"] <- 104.83
BD$Dens[BD$estado=="RO"] <- 7.47
BD$Dens[BD$estado=="TO"] <- 5.66
BD$Dens[BD$estado=="AC"] <- 5.37
BD$Dens[BD$estado=="AP"] <- 5.94
BD$Dens[BD$estado=="RR"] <- 2.7


BD$Pop  = NA

BD$Pop[BD$estado=="SP"] <- 45919049
BD$Pop[BD$estado=="MG"] <- 21168791
BD$Pop[BD$estado=="RJ"] <- 17264943
BD$Pop[BD$estado=="BA"] <- 14873064
BD$Pop[BD$estado=="PR"] <- 11433957
BD$Pop[BD$estado=="RS"] <- 11377239
BD$Pop[BD$estado=="PE"] <- 9557071
BD$Pop[BD$estado=="CE"] <- 9132078
BD$Pop[BD$estado=="PA"] <- 8602865
BD$Pop[BD$estado=="SC"] <- 7164788
BD$Pop[BD$estado=="MA"] <- 7075181
BD$Pop[BD$estado=="GO"] <- 7018354
BD$Pop[BD$estado=="AM"] <- 4144597
BD$Pop[BD$estado=="ES"] <- 4018650
BD$Pop[BD$estado=="PB"] <- 4018127
BD$Pop[BD$estado=="RN"] <- 3506853
BD$Pop[BD$estado=="MT"] <- 3484466
BD$Pop[BD$estado=="AL"] <- 3337357
BD$Pop[BD$estado=="PI"] <- 3273227
BD$Pop[BD$estado=="DF"] <- 3015268
BD$Pop[BD$estado=="MS"] <- 2778986
BD$Pop[BD$estado=="SE"] <- 2298696
BD$Pop[BD$estado=="RO"] <- 1777225
BD$Pop[BD$estado=="TO"] <- 1572866
BD$Pop[BD$estado=="AC"] <- 881935
BD$Pop[BD$estado=="AP"] <- 845731
BD$Pop[BD$estado=="RR"] <- 605761

BD$obitosAcumulados1 = BD$obitosAcumulado/BD$Pop*1000000
BD$obitosAcumulados2 = BD$obitosAcumulado/BD$Dens

## Gráfico para o Paolo
##
#BD2 =  na.omit(BD[,c(1:3,7,11,12,9)])
#BD2[,2] <- factor(BD2[,2])
#BD2 <- BD2[BD2[,4]>0,]
#BD20 <- data.frame()
#for(j in levels(BD2[,2])){
#	aux <- BD2[BD2[,2] == j,]
#	BD20 <- rbind(BD20, cbind(id=1:dim(aux)[1],aux))
#}
#
#aw = function(s) tapply(BD20[BD20[,1]==s,7],BD20[BD20[,1]==s,8], sum)
#
#q = sapply(1:32,aw)
#w = q[2,]/q[1,]
#
#plot(w~c(1:32))



BD0 <- BD[,c(1:3,8,10,13,18,19,20)]

BD0$data<-as.Date(BD0$data)
BD0$data<-as.Date(as.character(BD0$data), format="%d-%m-%y")

BD0 <- BD0[BD0[,6]>0,]
BD0 <- BD0[BD0[,3]== "",]
BD0 <- BD0[BD0[,2]!= "",]

BD0 <- BD0[BD0[,5]!= "",]

BD0 <- na.omit(BD0)

BD0[,2] <- factor(BD0[,2])
BD1 <- data.frame()
for(j in levels(BD0[,2])){
	aux <- BD0[BD0[,2] == j,]
	BD1 <- rbind(BD1,cbind(id=1:dim(aux)[1],aux))
}

BD1 = BD1[as.numeric(BD1[,6]) >10,]


a = ggplot(BD1, aes(x =id, y=obitosAcumulado, color = regiao, group= estado)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste("Óbitos segundo a data da divulgação."), x = 'Dias desde o primeiro morto \n Dados: https://covid.saude.gov.br/', y = '')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
					view_follow()

a1 = ggplot(BD1, aes(x =id, y=log(obitosAcumulado,base=10), color = regiao, group= estado)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
		transition_reveal(along=id) +
			labs(title=paste("Escala log"), x = 'Dias desde o primeiro morto', y = '')+ 	
				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+
					view_follow()

a2 = ggplot(BD1, aes(x =id, y=obitosAcumulados1, color = regiao, group= estado)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour = regiao), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste("Por milhão de habitantes"), x = 'Dias desde o primeiro morto \n (Elaborado por: AGPatriota)', y = '')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
					view_follow()

#a3 = ggplot(BD1, aes(x =id, y=log(obitosAcumulados1,base=10), color = regiao, group= estado)) +geom_path() + 
#	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
#		transition_reveal(along=id) +
#			labs(title=paste(""), x = '(Elaborado por: AGPatriota)', y = 'Log do número de mortos por covid (1Mi hab)')+ 	
#				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+
#					view_follow()

a.gif =  animate(a, width = 320, height = 300)
a.gif = image_read(a.gif)
a1.gif =  animate(a1, width = 320, height = 300)
a1.gif = image_read(a1.gif)
a2.gif =  animate(a2, width = 320, height = 300)
a2.gif = image_read(a2.gif)
#a3.gif =  animate(a3, width = 280, height = 240)
#a3.gif = image_read(a3.gif)

gc()
gc()

#Appending two gifs
#new_gif <- image_append(image_join(image_append(c(a.gif[1], a1.gif[1])),image_append(c(a2.gif[1], a3.gif[1]))), stack = TRUE)
new_gif <- image_append(c(a.gif[1], a1.gif[1],a2.gif[1]))


for(k in 2:100){
#  combined <- image_append(image_join(image_append(c(a.gif[k], a1.gif[k])),image_append(c(a2.gif[k], a3.gif[k]))),  stack = TRUE)
  combined <- image_append(c(a.gif[k], a1.gif[k], a2.gif[k]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif , paste("Gifs/Evolucao-mortos-estado.gif", sep=""))
gc()
gc()


###################################
#Estado de São Paulo
###################################

BD2 = BD1[BD1$estado=="SP",]
BD2 = BD2[as.numeric(BD2[,6]) >10,]

#Movimentação 16/03
#Quarentena em SP 24/03
#máscaras em SP 07/05 Uso obrigatório 
#11/05 Rodízio ampliado

BD2$obitosNovos = c(BD2$obitosAcumulado[1],diff(BD2$obitosAcumulado))
week0 = weekdays(BD2$data)
col0 = ifelse(week0 == "domingo" | week0 == "segunda", 4, 19)
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 1)}

jpeg("Figs/Dados-Divul-SP.jpg", width=800, height=550)
ss = plot(BD2$obitosNovos~BD2$data, lwd = 7, type="l", col="slateblue2", lty=1, ylab="", main="Óbitos diários segundo data da divulgação (Estado de São Paulo)", xlab="Dias", ylim=c(0,400),cex=1.6,cex.main=1.8, cex.lab=1.6)
 points(BD2$obitosNovos~BD2$data,pch=col0, cex=1.5, lwd=2)

abline(v = BD2$data[BD2$data=="2020-03-17"], lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-03-24"], lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-05-07"],lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-05-11"],lwd=4, col="gray50")
mtext("decreto",1, at= BD2$data[BD2$data=="2020-03-17"], cex=0.8 )
mtext("quaren",1, at= BD2$data[BD2$data=="2020-03-24"] , cex=0.8)
mtext("másc",1, at= BD2$data[BD2$data=="2020-05-07"] -1, cex=0.8)
mtext("rod_amp",1, at= BD2$data[BD2$data=="2020-05-11"]+1 , cex=0.8)
points((BD2$obitosNovos~BD2$data),type="l", lty=2, lwd = 0.4)

points(ma(BD2$obitosNovos)~BD2$data,type="l", lty=3, lwd=4, col="gray10")
legend(BD2$data[3],380,c("Domingos e segundas","Outros dias da semana", "Média móvel de 7 dias"), pch=c(4,19,NA),  lwd = c(2), cex=1.5, lty=c(1,1,2))
mtext("Elaboração: AGPatriota", 1, at= BD2$data[3], line=3)
dev.off()


#jpeg("Figs/Log-Dados-SP.jpg", width=900, height=550)
#plot(log(BD2$obitosAcumulado,base=10)~BD2$data, col=col0, pch=19, ylab="logarítmo de mortes acumuladas", main="Mortes acumuladas divulgadas por Covid (Estado de São Paulo)", xlab="Dias",cex.main=2, cex.lab=1.5, ylim=c(0,9.5))
#abline(v = BD2$data[BD2$data=="2020-03-17"], lwd=4, col="gray50")
#abline(v = BD2$data[BD2$data=="2020-03-24"], lwd=4, col="gray50")
#abline(v = BD2$data[BD2$data=="2020-05-07"],lwd=4, col="gray50")
#abline(v = BD2$data[BD2$data=="2020-05-11"],lwd=4, col="gray50")
#mtext("decreto",1, at= BD2$data[BD2$data=="2020-03-17"], cex=0.8 )
#mtext("quaren",1, at= BD2$data[BD2$data=="2020-03-24"] , cex=0.8)
#mtext("másc",1, at= BD2$data[BD2$data=="2020-05-07"] -1, cex=0.8)
#mtext("rod_amp",1, at= BD2$data[BD2$data=="2020-05-11"]+1 , cex=0.8)
#legend(BD2$data[1],9.5,c("Fins de semana, feriados e segundas","Dias de semana normais exceto segunda"), pch=19, col=c("tomato", "black"), cex=1.5)
#mtext("Elaboração: AGPatriota", 1, at= BD2$data[1], line=3)
#dev.off()

###################################
#Brasil
###################################

BD2 = BD[,c(1:3,8,10,13,18,19,20)]
BD2 = BD2[BD2$estado=="",]
BD2 = BD2[BD2[,6] >10,]

BD2$data<-as.Date(BD2$data)
BD2$data<-as.Date(as.character(BD2$data), format="%d-%m-%y")

#Movimentação 16/03
#Quarentena em SP 24/03
#máscaras em SP 07/05 Uso obrigatório 
#11/05 Rodízio ampliado

BD2$obitosNovos = c(BD2$obitosAcumulado[1],diff(BD2$obitosAcumulado))
week0 = weekdays(BD2$data)
col0 = ifelse(week0 == "domingo" | week0 == "segunda", 4, 19)
ma <- function(x, n = 7){filter(x, rep(1 / n, n), sides = 1)}

jpeg("Figs/Dados-Divul-Brasil.jpg", width=800, height=550)
ss = plot(BD2$obitosNovos~BD2$data, lwd = 7, type="l", col="slateblue2", lty=1, ylab="", main="Óbitos diários segundo data da divulgação (Brasil)", xlab="Dias", ylim=c(0,1800),cex=1.6,cex.main=1.8, cex.lab=1.6, xlim=c(range( BD2$data)[1]-5, range( BD2$data)[2]))
 points(BD2$obitosNovos~BD2$data,pch=col0, cex=1.5, lwd=2)

abline(v = as.Date("2020-03-17"), lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-03-24"], lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-05-07"],lwd=4, col="gray50")
abline(v = BD2$data[BD2$data=="2020-05-11"],lwd=4, col="gray50")
mtext("decreto",1, at= as.Date("2020-03-17"), cex=0.8 )
mtext("quaren",1, at= BD2$data[BD2$data=="2020-03-24"] , cex=0.8)
mtext("másc",1, at= BD2$data[BD2$data=="2020-05-07"] -1, cex=0.8)
mtext("rod_amp",1, at= BD2$data[BD2$data=="2020-05-11"]+1 , cex=0.8)
points((BD2$obitosNovos~BD2$data),type="l", lty=2, lwd = 0.4)

points(ma(BD2$obitosNovos)~BD2$data,type="l", lty=3, lwd=4, col="gray10")
legend(BD2$data[3]-5,1800,c("Domingos e segundas","Outros dias da semana", "Média móvel de 7 dias"), pch=c(4,19,NA),  lwd = c(2), cex=1.5, lty=c(1,1,2))
mtext("Elaboração: AGPatriota", 1, at= BD2$data[3], line=3)
dev.off()


##################################
##################################
##################################
# Mobilidade Google
##################################
##################################
##################################

BDD <- read.csv(url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"), header=TRUE)
BDD$date <- as.Date(BDD$date, format="%Y-%m-%d")
BDD = BDD[BDD[,2]=="Brazil",-c(1,2,4,5)]
BDD = BDD[BDD[,1]!="",]
BDD[,1] = factor(BDD[,1])

BDD$region = NA

Norte = c("State of Acre","State of Amapá","State of Amazonas","State of Tocantins","State of Roraima","State of Rondônia","State of Pará")

Nordeste = c("State of Alagoas","State of Maranhão","State of Bahia","State of Ceará","State of Sergipe","State of Rio Grande do Norte","State of Piauí","State of Paraíba","State of Pernambuco")

Centro.oeste = c("Federal District","State of Goiás","State of Mato Grosso","State of Mato Grosso do Sul")

Sudeste = c("State of São Paulo","State of Espírito Santo","State of Minas Gerais","State of Rio de Janeiro")

Sul = c("State of Santa Catarina","State of Paraná","State of Rio Grande do Sul")

BDD$region[BDD[,1] %in% Norte]= "Norte"
BDD$region[BDD[,1] %in% Sul] = "Sul"
BDD$region[BDD[,1] %in% Sudeste] = "Sudeste"
BDD$region[BDD[,1] %in% Nordeste] = "Nordeste"
BDD$region[BDD[,1] %in% Centro.oeste] = "Centro-Oeste"


############
#Graficos
###########

jpeg("Figs/Tempo-Residencia.jpg", width=1000, height=600)
ggplot(BDD, aes(x =date, y=residential_percent_change_from_baseline, color = region, group= sub_region_1)) +geom_line(size=1) + 
#	 geom_point(alpha=0.7) +
			labs(title=paste("Mudança percentual no tempo de permanência na residência por Estado.\nQuanto maior mais tempo em casa. \nZero indica a movimentação usual.  \n \nDados: https://www.google.com/covid19/mobility/ "), x = 'Data', y = '%', caption="Elaboração: AGPatriota") +theme(legend.title = element_blank(),legend.text=element_text(size = 18))+ theme(text =element_text(size=21),plot.title = element_text(size = 21),panel.background = element_rect(fill = "snow2"))+scale_color_manual(values=c("orange", "red3","green","blue3", "purple"))# + geom_text(aes(label =  sub_region_1, colour = region), hjust=1.5, size = 3.5, fontface = "bold")
dev.off()

