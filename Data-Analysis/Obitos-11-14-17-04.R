#https://noticias.uol.com.br/saude/ultimas-noticias/redacao/2020/04/12/brasil-ja-pode-ter-cerca-de-2-mil-mortes-por-covid-19-entenda.htm
#https://cidadeverde.com/coronavirus/105909/brasil-bate-recorde-e-registra-204-novas-mortes-por-coronavirus-em-24-horas
#https://www.saude.gov.br/boletins-epidemiologicos


#Data:
Obitos.11=c(1,3,2,5,7,9,11,13,27,23,25,29,29,39,46,46,54,53,54,63,71,71,70,53,53,32,31,13, NA, NA, NA )
Obitos.14=c(1,3,2,5,7,9,11,13,27,25,25,31,30,41,46,51,59,57,67,71,80,86,73,71,70,50,58,54,57,37,5)
Obitos.17=c(1,3,3,5,8,11,12,13,29,26,27,32,35,51,48,57,70,68,79,87,98,104,93,99,101,79,87,86,101,96,70)
Obitos.20=c(1,3,3,5,7,11,12,14,29,27,26,33,36,51,49,59,73,73,82,94,103,111,104,106,105,90,103,98,114,113,93)

jpeg("Figs/Obitos.11.14.17.jpg", width = 1384, height = 500)
Dias = as.Date(0:30, origin = "15-03-20", "%d-%m-%y")
bb = barplot(cbind(Obitos.11, Obitos.14, Obitos.17, Obitos.20)~c(1:length(Obitos.14)), beside = TRUE, main="Número de mortos na data do óbito",col=c("gray80", "gray60", "gray30", "black"), xlab="Dias (15/03 a 11/04)", ylim=c(0,max(c(Obitos.11, Obitos.14, Obitos.17, Obitos.20), na.rm=T)+4))
legend(1,100, c("Dados divulgados em 11/04/20","Dados divulgados em 14/04/20","Dados divulgados em 17/04/20","Dados divulgados em 20/04/20"), col=c("gray80", "gray60", "gray30", "black"), lwd=10)
text(bb[1,17:28]-0.6, Obitos.11[17:28]+2.5, Obitos.11[17:28], col="gray30", font=2)
text(bb[2,17:28]-0.4, Obitos.14[17:sum(!is.na(Obitos.11))]+2.5, Obitos.14[17:sum(!is.na(Obitos.11))], col="gray40", font=2)
text(bb[3,17:28]-0.4, Obitos.17[17:sum(!is.na(Obitos.11))]+2.5, Obitos.17[17:sum(!is.na(Obitos.11))], col="black", font=2)
text(bb[4,17:28], Obitos.20[17:sum(!is.na(Obitos.11))]+2.5, Obitos.20[17:sum(!is.na(Obitos.11))], col="black", font=2)
mtext("Fonte: Boletins epidemiológicos. Ministério da Saúde",1,at = bb[1,3],line=3)
mtext("Elaboração: agpatriota",1,at = bb[1,1],line=4)
dev.off()


