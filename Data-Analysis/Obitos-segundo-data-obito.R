#https://noticias.uol.com.br/saude/ultimas-noticias/redacao/2020/04/12/brasil-ja-pode-ter-cerca-de-2-mil-mortes-por-covid-19-entenda.htm
#https://cidadeverde.com/coronavirus/105909/brasil-bate-recorde-e-registra-204-novas-mortes-por-coronavirus-em-24-horas
#https://www.saude.gov.br/boletins-epidemiologicos
#http://plataforma.saude.gov.br/coronavirus/dados-abertos/

#Data:
Obitos.11=c(1,3,2,5,7,9,11,13,27,23,25,29,29,39,46,46,54,53,54,63,71,71,70,53,53,32,31,13, NA, NA, NA )
Obitos.14=c(1,3,2,5,7,9,11,13,27,25,25,31,30,41,46,51,59,57,67,71,80,86,73,71,70,50,58,54,57,37,5)
Obitos.17=c(1,3,3,5,8,11,12,13,29,26,27,32,35,51,48,57,70,68,79,87,98,104,93,99,101,79,87,86,101,96,70)
Obitos.20=c(1,3,3,5,7,11,12,14,29,27,26,33,36,51,49,59,73,73,82,94,103,111,104,106,105,90,103,98,114,113,93)
Transp =c(NA, 6,2,7,9,10,19,29,27,45,41,58,55,67,64,95,87,113,116,143,143,185,153,160,167,142,167,193)
Dias = as.Date(0:30, origin = "15-03-20", "%d-%m-%y")

BD = read.csv("Data/INFLUD-16-06-2020-Revisado.csv", header=TRUE, sep=";",na.strings = c("-"))

#BD= BD[!is.na(BD$CLASSI_FIN),]
BD = BD[BD$ID_PAIS=="BRASIL",]
BD$ID_Pais = factor(BD$ID_PAIS)
BD= BD[which(BD$CLASSI_FIN==5),]
BD = BD[which(BD$PCR_SARS2==1),]
#BD = BD[!is.na(BD$EVOLUCAO),]
BD = BD[which(BD$EVOLUCAO == 2),]
auxx = BD$DT_EVOLUCA=="0"

#data.frame(BD$DT_NOTIFIC,BD$DT_EVOLUCA,BD$DT_ENCERRA,BD$DT_DIGITA )
BD$DT_DIGITA = as.Date(BD$DT_DIGITA, format="%d/%m/%y")
BD$DT_EVOLUCA = as.Date(BD$DT_EVOLUCA, format="%d/%m/%y")
BD$DT_ENCERRA = as.Date(BD$DT_ENCERRA, format="%d/%m/%y")
BD$DT_SIN_PRI = as.Date(BD$DT_SIN_PRI, format="%d/%m/%y")
BD$DT_NOTIFIC = as.Date(BD$DT_NOTIFIC, format="%d/%m/%y")

#write.table(BD, file="Data/Dados-completos-OPENSUS/17-06-OPENSUS1.dat")


#Por sexo
xF = as.numeric(table(BD$DT_EVOLUCA[BD$CS_SEXO=="F"]))
xM = as.numeric(table(BD$DT_EVOLUCA[BD$CS_SEXO=="M"]))
time = as.Date(names(table(BD$DT_EVOLUCA)))

BD.SP = BD[BD$SG_UF_NOT=="SP",]
xF.SP = as.numeric(table(BD.SP$DT_EVOLUCA[BD.SP$CS_SEXO=="F"]))
xM.SP = as.numeric(table(BD.SP$DT_EVOLUCA[BD.SP$CS_SEXO=="M"]))
time.SP = as.Date(names(table(BD.SP$DT_EVOLUCA)))

jpeg("Figs/Obitos-Por-Sexo.jpg", width = 894, height = 600)
par(mfrow=c(1,2))
plot(xF~time[-1], xlab="", ylab="", lwd = 5, type="l", col="tomato", lty=1,ylim=range(xF,xM), main ="Óbitos por COVID-19 segundo a data do óbito e sexo\n Brasil")
points(xM~time[-1], xlab="", ylab="", lwd = 5, type="l", col="slateblue", lty=1,)
legend(time[3], range(xF,xM)[2], c("Masculino", "Feminino"), lty=1, col=c("slateblue", "tomato"),lwd=10)
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = time[25],line=3)
mtext("Elaboração: AGPatriota",1,at = time[10],line=4)
plot(xF.SP~time.SP[-1], xlab="", ylab="", lwd = 5, type="l", col="tomato", lty=1,ylim=range(xF,xM), main ="Óbitos por COVID-19 segundo a data do óbito e sexo\n Estado de São Paulo")
points(xM.SP~time.SP[-1], xlab="", ylab="", lwd = 5, type="l", col="slateblue", lty=1,)
legend(time.SP[3], range(xF,xM)[2], c("Masculino", "Feminino"), lty=1, col=c("slateblue", "tomato"),lwd=10)
dev.off()

#Por idade
qq=quantile(BD$NU_IDADE_N, probs=c(0.1, 0.9))
col0 = ifelse(as.numeric(names(table(BD$NU_IDADE_N))) > qq[1] , "tomato" , "blue4")
jpeg("Figs/Obitos-Por-Idade.jpg", width = 894, height = 600)
plot(table(BD$NU_IDADE_N), main="Óbitos por COVID-19 segundo a idade (Brasil)", ylab="", xlab="Idade", col=col0, lwd=6)
text(18,100,"10%", font=2)
text(103,100,"90%", font=2)
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = 11.5,line=3)
mtext("Elaboração: AGPatriota",1,at = 3,line=4)
dev.off()


jpeg("Figs/Obitos-Por-Idade-Sexo.jpg", width = 894, height = 600)
par(mfrow=c(1,2))
qq=quantile(BD$NU_IDADE_N[BD$CS_SEXO=="F"], probs=c(0.1, 0.9))
col0 = ifelse(as.numeric(names(table(BD$NU_IDADE_N[BD$CS_SEXO=="F"]))) > qq[1] , "tomato" , "blue4")
plot(table(BD$NU_IDADE_N[BD$CS_SEXO=="F"]), main="Óbitos por COVID-19 segundo a idade (Brasil)\n Sexo Feminino", ylab="", xlab="", col=col0, lwd=6,ylim=c(1,600), xlim=c(0,115))
text(18,100,paste("10%",sep=""), font=2)
text(18,80,paste("(",qq[1], " anos)",sep=""), font=2)
text(103,100,paste("90%",sep=""), font=2)
qq=quantile(BD$NU_IDADE_N[BD$CS_SEXO=="M"], probs=c(0.1, 0.9))
col0 = ifelse(as.numeric(names(table(BD$NU_IDADE_N[BD$CS_SEXO=="M"]))) > qq[1] , "tomato" , "blue4")
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = 27,line=2)
mtext("Elaboração: AGPatriota",1,at = 7,line=3)
plot(table(BD$NU_IDADE_N[BD$CS_SEXO=="M"]), main="Óbitos por COVID-19 segundo a idade (Brasil)\n  Sexo Masculino", ylab="", xlab="Idade", col=col0, lwd=6,ylim=c(1,600), xlim=c(0,115))
text(18,100,paste("10%",sep=""), font=2)
text(18,80,paste("(",qq[1], " anos)",sep=""), font=2)
text(103,100,paste("90%",sep=""), font=2)
dev.off()


#Óbitos totais
jpeg("Figs/Obitos-Brasil.jpg", width = 894, height = 600)
time = as.Date(names(table(BD$DT_EVOLUCA)))
week0 = weekdays(time)
col0 = ifelse(week0 == "domingo" | week0 == "segunda", 4, 19)
x = as.numeric(table(BD$DT_EVOLUCA))
plot(x~ time, lwd = 7, type="l", col="slateblue2", lty=1, xlab="", ylab="", main ="Óbitos por COVID-19 segundo a data do óbito (Brasil)", ylim=c(0,1800))
points(x~ time,pch=col0, cex=1.5, lwd=2)
#points(Obitos.11[1:28]~Dias[1:28], lwd = 7, type="l")
#points(Obitos.14[1:28]~Dias[1:28], lwd = 7, type="l", col="tomato")
#points(Obitos.17[1:28]~Dias[1:28], lwd = 7, type="l", col="turquoise2")
#points(Obitos.20[1:28]~Dias[1:28], lwd = 7, type="l", col="royalblue2")

abline(v = as.Date("2020-04-11"), lwd=4, lty=2)

abline(v = as.Date("2020-03-17"), lwd=4, col="gray50")
abline(v = as.Date("2020-03-24"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-07"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-11"), lwd=4, col="gray50")
mtext("decreto",1, at= as.Date("2020-03-17"), cex=0.8 )
mtext("quaren",1, at= as.Date("2020-03-24") , cex=0.8)
mtext("másc",1, at= as.Date("2020-05-07") -1, cex=0.8)
mtext("rod_amp",1, at= as.Date("2020-05-11")+1 , cex=0.8)
legend(time[3],1800,c("Domingos e segundas","Outros dias da semana", "Média móvel de 7 dias"), pch=c(4,19,NA),  lwd = c(2), cex=1.5, lty=c(1,1,2))
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = time[9],line=3)
mtext("Elaboração: AGPatriota",1,at = time[2],line=4)
#	mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = Dias[9],line=3)
#mtext("Elaboração: AGPatriota",1,at = Dias[2],line=4)
dev.off()


#Óbitos totais
jpeg("Figs/Obitos-Totais.jpg", width = 894, height = 600)
time = as.Date(names(table(BD$DT_EVOLUCA)))
week0 = weekdays(time)
col0 = ifelse(week0 == "domingo" | week0 == "segunda", 4, 19)
x = as.numeric(table(BD$DT_EVOLUCA))
plot(x~ time, lwd = 7, type="l", col="slateblue2", lty=1, xlab="", ylab="", main ="Óbitos por COVID-19 segundo a data do óbito (Brasil)")
points(Obitos.11[1:28]~Dias[1:28], lwd = 7, type="l")
points(Obitos.14[1:28]~Dias[1:28], lwd = 7, type="l", col="tomato")
points(Obitos.17[1:28]~Dias[1:28], lwd = 7, type="l", col="turquoise2")
points(Obitos.20[1:28]~Dias[1:28], lwd = 7, type="l", col="royalblue2")

abline(v = as.Date("2020-04-11"), lwd=4, lty=2)

abline(v = as.Date("2020-03-17"), lwd=4, col="gray50")
abline(v = as.Date("2020-03-24"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-07"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-11"), lwd=4, col="gray50")
mtext("decreto",1, at= as.Date("2020-03-17"), cex=0.8 )
mtext("quaren",1, at= as.Date("2020-03-24") , cex=0.8)
mtext("másc",1, at= as.Date("2020-05-07") -1, cex=0.8)
mtext("rod_amp",1, at= as.Date("2020-05-11")+1 , cex=0.8)
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = Dias[9],line=3)
mtext("Elaboração: AGPatriota",1,at = Dias[2],line=4)
dev.off()



jpeg("Figs/Obitos111417.jpg", width = 894, height = 600)
#bb = barplot(cbind(Obitos.11, Obitos.14, Obitos.17, Obitos.20)~c(1:length(Obitos.14)), beside = TRUE, main="Número de mortos na data do óbito",col=c("gray80", "gray60", "gray30", "black"), xlab="Dias (15/03 a 11/04)", ylim=c(0,max(c(Obitos.11, Obitos.14, Obitos.17, Obitos.20), na.rm=T)+4),border=NA,axes = F,names.arg=NA,axisnames=FALSE)
plot(Obitos.11[1:28]~Dias[1:28], lwd = 7, type="l", ylim=c(0,250), main="Óbitos por COVID-19 segundo data do óbito (Brasil: 15/03 a 11/04)", xlab="", ylab="", axes=F)
box()
axis(2)
axis(1, Dias[1:28], Dias[1:28])
points(Obitos.14[1:28]~Dias[1:28], lwd = 7, type="l", col="tomato")
points(Obitos.17[1:28]~Dias[1:28], lwd = 7, type="l", col="turquoise2")
points(Obitos.20[1:28]~Dias[1:28], lwd = 7, type="l", col="royalblue2")
#points(Transp[1:28]~Dias[1:28], lwd = 3, type="l", col="blue2", lty=4)
points(table(BD$DT_EVOLUCA)~as.Date(names(table(BD$DT_EVOLUCA))), lwd = 7, type="l", col="slateblue2")
legend(Dias[1],200, c("Dados divulgados em 11/04/20","Dados divulgados em 14/04/20","Dados divulgados em 17/04/20","Dados divulgados em 20/04/20", "Open DataSUS acessado em 16/06/2020"), col=c("black", "tomato", "turquoise2", "royalblue2","slateblue2"), lwd=c(10,10,10,10,10),lty=c(1,1,1,1,1))
#abline(v= Dias[9])
text(Dias[9],0, "19 dias", font=2)
#abline(v= Dias[16])
text(Dias[16],0, "12 dias", font=2)
abline(v = as.Date("2020-04-11"), lwd=4, lty=2)

text(Dias[17:28], Obitos.11[17:28]-5.5, Obitos.11[17:28], col="gray30", font=2)
text(Dias[17:28], Obitos.14[17:sum(!is.na(Obitos.11))], Obitos.14[17:sum(!is.na(Obitos.11))], col="gray40", font=2)
text(Dias[17:28], Obitos.17[17:sum(!is.na(Obitos.11))], Obitos.17[17:sum(!is.na(Obitos.11))], col="black", font=2)
text(Dias[17:28], Obitos.20[17:sum(!is.na(Obitos.11))]+5.5, Obitos.20[17:sum(!is.na(Obitos.11))], col="black", font=2)
#text(Dias[17:28], Transp[17:sum(!is.na(Obitos.11))]-7.5, Transp[17:sum(!is.na(Obitos.11))], col="black", font=2)
text(Dias[17:28], table(BD$DT_EVOLUCA)[17:sum(!is.na(Obitos.11))]+15,  table(BD$DT_EVOLUCA)[17:sum(!is.na(Obitos.11))], col="black", font=2)

	mtext("Fonte: Boletins epidemiológicos. Ministério da Saúde e OpenDataSUS",1,at = Dias[6],line=3)
mtext("Elaboração: AGPatriota",1,at = Dias[2],line=4)
dev.off()




BD.SP = BD[BD$SG_UF_NOT=="SP",]
jpeg("Figs/Obitos-SP.jpg", width = 894, height = 600)
time = as.Date(names(table(BD.SP$DT_EVOLUCA)))
week0 = weekdays(time)
col0 = ifelse(week0 == "domingo" | week0 == "segunda", 4, 19)
#col0[time == "2020-05-01" |time =="2020-02-24" |time == "2020-04-10" |time =="2020-04-21"] = 16
#col0[week0 == "segunda"] = 16
#col0[time == "2020-04-22"] = 16
#col0[!col0==19]    <-15
#col0[week0 == "sábado"] = 15

x = as.numeric(table(BD.SP$DT_EVOLUCA))
plot(x~ time, lwd = 7, type="l", col="slateblue2", lty=1, xlab="", ylab="", main ="Óbitos por COVID-19 segundo a data do óbito (SP)",ylim=c(0,400))
points(x~ time,pch=col0, cex=1.5, lwd=2)
#abline(v = as.Date("2020-04-11"), lwd=4, lty=2)
abline(v = as.Date("2020-03-17"), lwd=4, col="gray50")
abline(v = as.Date("2020-03-24"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-07"), lwd=4, col="gray50")
abline(v = as.Date("2020-05-11"), lwd=4, col="gray50")
mtext("decreto",1, at= as.Date("2020-03-17"), cex=0.8 )
mtext("quaren",1, at= as.Date("2020-03-24") , cex=0.8)
mtext("másc",1, at= as.Date("2020-05-07") -1, cex=0.8)
mtext("rod_amp",1, at= as.Date("2020-05-11")+1 , cex=0.8)
legend(BD2$data[3],380,c("Domingos e segundas","Outros dias da semana", "Média móvel de 7 dias"), pch=c(4,19,NA),  lwd = c(2), cex=1.5, lty=c(1,1,2))
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at = Dias[9],line=3)
mtext("Elaboração: AGPatriota",1,at = Dias[2],line=4)
dev.off()

#Fazer gráficos por idado e outros interessantes

#Por escolaridade
jpeg("Figs/Internado-Hospital.jpg", width = 894, height = 600)

IBGE = c(7, 35.3,7.8, 31.9, 18.1)
x0 = (table(BD$DT_EVOLUCA[BD$CS_ESCOL_N==0]))/IBGE[1]
x1 = (table(BD$DT_EVOLUCA[BD$CS_ESCOL_N==1| BD$CS_ESCOL_N==3| BD$CS_ESCOL_N==2| BD$CS_ESCOL_N==4]))/(100-IBGE[1])

jpeg("Figs/Escolaridade.jpg", width = 894, height = 600)
xx = data.frame(Obitos = c(x0,x1), Esco = c(rep("Analfabeto", length(x0)),rep("Alfabetizado", length(x1))), Data =c( as.Date(names(x0)), as.Date(names(x1))))
ggplot(xx, aes(y = Obitos, x = Data, color=Esco))  + geom_line(size =2)+	labs(title=paste("Óbitos (por míl hab) segundo a data de óbito e alfabetização (Brasil)."), x = 'Data', y = '') + theme(legend.title = element_blank())+ theme(text =element_text(size=13))
dev.off()





#Internado no Hospital
x0 = (table(BD$DT_EVOLUCA[BD$HOSPITAL ==1]))
x1 = (table(BD$DT_EVOLUCA[BD$HOSPITAL ==2]))
xx = data.frame(Obitos = c(x0,x1), Internado = c(rep("Foi internado", length(x0)),rep("Não foi internado", length(x1))), Data =c( as.Date(names(x0)), as.Date(names(x1)) ))
jpeg("Figs/Internado-Hospital.jpg", width = 894, height = 600)
ggplot(xx, aes(y = Obitos, x = Data, color=Internado)) + geom_line(size =2)+	labs(title=paste("Óbitos segundo a data de óbito e hospitalização (Brasil)."), x = 'Data', y = '') + theme(legend.title = element_blank())+ theme(text =element_text(size=13))
dev.off()


#Internado na UTI
x0 = (table(BD$DT_EVOLUCA[BD$UTI ==1]))
x1 = (table(BD$DT_EVOLUCA[BD$UTI ==2]))
xx = data.frame(Obitos = c(x0,x1), UTI = c(rep("Foi para a UTI", length(x0)),rep("Não foi para UTI", length(x1))), Data =c( as.Date(names(x0)), as.Date(names(x1)) ))

jpeg("Figs/Internado-UTI.jpg", width = 894, height = 600)

ggplot(xx, aes(y = Obitos, x = Data, color=UTI)) + geom_line(size =2)+	labs(title=paste("Óbitos segundo a data de óbito e internação UTI (Brasil)."), x = 'Data', y = '') + theme(legend.title = element_blank())+ theme(text =element_text(size=13))

dev.off()


#Tempo médio dos primeiros sintomas até o óbito
quan1 = function(x) quantile(x, 0.05)
quan2 = function(x) quantile(x, 0.95)

dd = tapply(BD$DT_EVOLUCA-BD$DT_SIN_PRI,BD$DT_EVOLUCA, mean)
dd1 = tapply(BD$DT_EVOLUCA-BD$DT_SIN_PRI,BD$DT_EVOLUCA, quan1)
dd2 = tapply(BD$DT_EVOLUCA-BD$DT_SIN_PRI,BD$DT_EVOLUCA, quan2)
jpeg("Figs/Tempo-Sintomas-Obito.jpg", width = 694, height = 600)
plot(BD$DT_EVOLUCA-BD$DT_SIN_PRI~BD$DT_EVOLUCA, cex=0.2, xlab="Data de óbito", main="Tempo decorrido dos primeiros sintomas ao óbito",ylab="")
points(dd~as.Date(names(dd)), type="l", lwd=3, col="red")
points(dd1~as.Date(names(dd)), type="l", col="red")
points(dd2~as.Date(names(dd)), type="l", col="red")
polygon(c(as.Date(names(dd)),rev(as.Date(names(dd)))),c(dd1,rev(dd2)),col="gray60", density=60)
points(dd~as.Date(names(dd)), type="l", lwd=3, col="red")
points(dd1~as.Date(names(dd)), type="l", col="red")
points(dd2~as.Date(names(dd)), type="l", col="red")
mtext("Fonte: Ministério da Saúde e OpenDataSUS",1,at =as.Date("2020-03-17")+9,line=3)
mtext("Elaboração: AGPatriota",1,at = as.Date("2020-03-17"),line=4)
dev.off()



plot(BD$DT_ENCERRA-BD$DT_EVOLUCA~BD$DT_ENCERRA)


plot(BD$DT_DIGITA-BD$DT_NOTIFIC~BD$DT_DIGITA, main="Tempo decorrido da notificação até a anotação\n (deveria ser sempre positivo)", xlab="Data da anotação", ylab="", cex=.5)

plot(BD$DT_DIGITA-BD$DT_EVOLUCA~BD$DT_DIGITA, main="Tempo decorrido da morte até a anotação\n (deveria ser sempre positivo)", xlab="Data da anotação", ylab="", cex=.5)
4386

plot(BD$DT_EVOLUCA-BD$DT_NOTIFIC ~ BD$DT_NOTIFIC)
plot(BD$DT_NOTIFIC-BD$DT_SIN_PRI ~ BD$DT_NOTIFIC)


plot(BD$DT_EVOLUCA-BD$DT_SIN_PRI~BD$DT_EVOLUCA)



BD.NEG =  BD[which(BD$DT_EVOLUCA-BD$DT_DIGITA>0),]
BD[2,]
