#https://covid.saude.gov.br/
#https://www.google.com/covid19/mobility/

require('gganimate')
require('ggplot2')
require('magick')


#BD =  read.csv("Data/HIST_PAINEL_COVIDBR_19mai2020.csv", header = TRUE, sep=",")
#BD =  read.csv("Data/HIST_PAINEL_COVIDBR_21mai2020.csv", header = TRUE, sep=",")

BD =  read.csv("Data/HIST_PAINEL_COVIDBR_04jun2020.csv", header = TRUE, sep=",")




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
BD0 <- BD0[BD0[,6]>0,]
BD0 <- BD0[BD0[,3]== "",]
BD0 <- BD0[BD0[,2]!= "",]
BD0 <- na.omit(BD0)

BD0[,2] <- factor(BD0[,2])
BD1 <- data.frame()
for(j in levels(BD0[,2])){
	aux <- BD0[BD0[,2] == j,]
	BD1 <- rbind(BD1,cbind(id=1:dim(aux)[1],aux))
}




a = ggplot(BD1, aes(x =id, y=obitosAcumulado, color = regiao, group= estado)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste("Evolução do número de mortos por Estado."), x = 'Dias desde o primeiro morto', y = 'Número de mortos por covid')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
					view_follow()

a1 = ggplot(BD1, aes(x =id, y=log(obitosAcumulado), color = regiao, group= estado)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
		transition_reveal(along=id) +
			labs(title=paste("Dados: https://covid.saude.gov.br/"), x = 'Dias desde o primeiro morto', y = 'Log do número de mortos por covid')+ 	
				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+
					view_follow()

a2 = ggplot(BD1, aes(x =id, y=obitosAcumulados1, color = regiao, group= estado)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour = regiao), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste(""), x = 'Dias desde o primeiro morto', y = 'Número total de mortos por covid (1Mi hab)')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
					view_follow()

a3 = ggplot(BD1, aes(x =id, y=log(obitosAcumulados1), color = regiao, group= estado)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = estado, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
		transition_reveal(along=id) +
			labs(title=paste(""), x = '(Elaborado por: AGPatriota)', y = 'Log do número de mortos por covid (1Mi hab)')+ 	
				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+
					view_follow()

a.gif =  animate(a, width = 280, height = 240)
a.gif = image_read(a.gif)
a1.gif =  animate(a1, width = 280, height = 240)
a1.gif = image_read(a1.gif)
a2.gif =  animate(a2, width = 280, height = 240)
a2.gif = image_read(a2.gif)
a3.gif =  animate(a3, width = 280, height = 240)
a3.gif = image_read(a3.gif)


#Appending two gifs
new_gif <- image_append(image_join(image_append(c(a.gif[1], a1.gif[1])),image_append(c(a2.gif[1], a3.gif[1]))), stack = TRUE)


for(k in 2:100){
  combined <- image_append(image_join(image_append(c(a.gif[k], a1.gif[k])),image_append(c(a2.gif[k], a3.gif[k]))),  stack = TRUE)
  new_gif <- c(new_gif, combined)
}

image_write(new_gif , paste("Gifs/Evolucao-mortos-estado.gif", sep=""))




BD2 = BD1[BD1$estado=="SP",]
BD2 = BD2[BD2[,6] != "",]

BD2$data = as.Date(BD2$data, format="%Y-%m-%d")
BD2$obitosNovos = c(BD2$obitosAcumulado[1],diff(BD2$obitosAcumulado))
week0 = weekdays(BD2$data)
col0 = ifelse(week0 == "sábado" | week0 == "domingo", "gray70", "black")
col0[BD2$data == "2020-05-01" |BD2$data =="2020-02-24" | BD2$data == "2020-04-10" |BD2$data =="2020-04-21"] = "gray70"
col0[week0 == "segunda"] = "gray70"
col0[BD2$data == "2020-04-22"] = "gray70"

jpeg("Figs/Dados-SP.jpg", width=1400, height=550)
ss = barplot(BD2$obitosNovos~BD2$data, col=col0, pch=19, ylab="Mortes por dia", main="Mortes diárias divulgadas por Covid  (Estado de São Paulo)", xlab="Dias", ylim=c(0,350),cex.main=2, cex.lab=1.5)
legend(ss[1],350,c("Fins de semana, feriados e segundas","Dias de semana normais exceto segunda"), pch=19, col=c("gray70", "black"), cex=1.5)
text(ss,BD2$obitosNovos+5, BD2$obitosNovos, cex=1, font=2)
mtext("Elaboração: AGPatriota", 1, at= ss[1], line=3)
dev.off()


jpeg("Figs/Log-Dados-SP.jpg", width=900, height=550)
plot(log(BD2$obitosAcumulado)~BD2$data, col=col0, pch=19, ylab="logarítmo de mortes acumuladas", main="Mortes acumuladas divulgadas por Covid (Estado de São Paulo)", xlab="Dias",cex.main=2, cex.lab=1.5, ylim=c(0,9.5))
legend(BD2$data[1],9.5,c("Fins de semana, feriados e segundas","Dias de semana normais exceto segunda"), pch=19, col=c("gray70", "black"), cex=1.5)
mtext("Elaboração: AGPatriota", 1, at= BD2$data[1], line=3)
dev.off()


##################################
##################################
##################################
# Mobilidade Google
##################################
##################################
##################################

BDD <- read.csv(url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"), header=TRUE)
BDD[,5] <- as.Date(BDD[,5], format="%Y-%m-%d")
BDD = BDD[BDD[,2]=="Brazil",-c(1,2,4)]
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


#########################3
#Tentativas abaixo
##########################

#Tentar pensar em uma medida de mobilidade com o tempo. Menos tempo até atingir imobilidade máxima

#############################
#Velocidade de mortos por dia
#############################

#SP
X11()
y.SP=diff(BD1[BD1[,3]=="SP",5])/BD1[BD1[,3]=="SP",5][-length(BD1[BD1[,3]=="SP",5])]
x.SP = BD1[BD1[,3]=="SP",1][-1]
#Amazonas
y.AM=diff(BD1[BD1[,3]=="AM",5])/BD1[BD1[,3]=="AM",5][-length(BD1[BD1[,3]=="AM",5])]
x.AM = BD1[BD1[,3]=="AM",1][-1]

sub = TRUE#-c(1:10)
plot(y.SP[sub]~x.SP[sub], type="l", lwd=3, ylim=range(y.SP[sub],y.AM[sub]))
points(y.AM[sub]~x.AM[sub], pch=19, type="l", col="tomato3", lwd=3)

y=list()
x = list()
for(j in levels(BD1[,3])){
y[[j]] = diff(BD1[BD1[,3]==j,5])/BD1[BD1[,3]==j,5][-length(BD1[BD1[,3]==j,5])]*100
x[[j]] = BD1[BD1[,3]==j,1][-1]
}
plot(y$SP~x$SP, type="l", lwd=3, ylim=range(y), xlab="Dias desde a primeira morte", main="Crescimento diário (%) de mortes por Covid-19 ", ylab="Percentagem")
for(j in levels(BD1[,3])){
points(y[[j]]~x[[j]], lty=2, type="l", col="gray50", lwd=1)
}
points(y$SP~x$SP, type="l",lty=1, lwd=3)
points(y$AM~x$AM, type="l",lty=1, lwd=3, col="green")
points(y$CE~x$CE, type="l",lty=1, lwd=3, col="tomato3")
mtext("Elaboração: AGPatriota", 1, at= 2, line=3)




#Mobilidade
#São Paulo
BD.aux = BDD[BDD[,1]=="State of São Paulo",c(2,8)]
names(BD.aux) = c("data", "mobilidade")
plot(BD.aux$mobilidade~BD.aux$data, ylim=c(-10,30))
abline(v= BD1[BD1[,3]=="SP",c(4)][1])
BD.aux = merge(BD.aux,BD1[BD1[,3]=="SP",c(4,5)])
BD.aux = cbind(1:dim(BD.aux)[1], BD.aux)
lm(obitosAcumulados~mobilidade + I(mobilidade^2),data = BD.aux)
par(mfrow=c(2,1))
plot(BD.aux$obitosAcumulados~BD.aux$data)
plot(BD.aux$mobilidade~BD.aux$data)


#Amazonas
BD.aux = BDD[BDD[,1]=="State of Amazonas",c(2,8)]
names(BD.aux) = c("data", "mobilidade")
plot(BD.aux$mobilidade~BD.aux$data, ylim=c(-10,30))
abline(v= BD1[BD1[,3]=="AM",c(4)][1])

BD.aux = merge(BD.aux,BD1[BD1[,3]=="AM",c(4,5)])
BD.aux = cbind(1:dim(BD.aux)[1], BD.aux)
lm(obitosAcumulados~mobilidade + I(mobilidade^2),data = BD.aux)
par(mfrow=c(2,1))
plot(BD.aux$obitosAcumulados~BD.aux$data)
plot(BD.aux$mobilidade~BD.aux$data)


