#https://noticias.uol.com.br/saude/ultimas-noticias/redacao/2020/04/12/brasil-ja-pode-ter-cerca-de-2-mil-mortes-por-covid-19-entenda.htm
#https://cidadeverde.com/coronavirus/105909/brasil-bate-recorde-e-registra-204-novas-mortes-por-coronavirus-em-24-horas
#https://www.saude.gov.br/images/pdf/2020/April/18/2020-04-17---BE11---Boletim-do-COE-21h.pdf

#Data:
Obitos.11=c(1,3,2,5,7,9,11,13,27,23,25,29,29,39,46,46,54,53,54,63,71,71,70,53,53,32,31,13, NA, NA, NA )
Obitos.14=c(1,3,2,5,7,9,11,13,27,25,25,31,30,41,46,51,59,57,67,71,80,86,73,71,70,50,58,54,57,37,5)
Obitos.17=c(1,3,3,5,8,11,12,13,29,26,27,32,35,51,48,57,70,68,79,87,98,104,93,99,101,79,87,86,101,96,70)


jpeg("Figs/Obitos.11.14.17.jpg", width = 1284, height = 500)
Dias = as.Date(0:30, origin = "15-03-20", "%d-%m-%y")
bb = barplot(cbind(Obitos.11, Obitos.14, Obitos.17)~c(1:length(Obitos.14)), beside = TRUE, main="Comparação do número de mortos na data do óbito",col=c("gray30", "gray70", "black"), xlab="Dias (15/03 a 11/04)", ylim=c(0,max(c(Obitos.11, Obitos.14, Obitos.17), na.rm=T)+4))
legend(1,100, c("Dados divulgados em 11/04/20","Dados divulgados em 14/04/20","Dados divulgados em 17/04/20"), col=c("gray30", "gray70", "black"), lwd=10)
text(bb[1,17:28]-0.3, Obitos.11[17:28]+1.5, Obitos.11[17:28], col="gray30", font=2)
text(bb[2,17:28], Obitos.14[17:sum(!is.na(Obitos.11))]+1.5, Obitos.14[17:sum(!is.na(Obitos.11))], col="gray40", font=2)
text(bb[3,17:28]+0.3, Obitos.17[17:sum(!is.na(Obitos.11))]+1.5, Obitos.17[17:sum(!is.na(Obitos.11))], col="black", font=2)
mtext("Fonte: Boletins epidemiológicos. Ministério da Saúde",1,at = bb[1,3],line=3)
mtext("Elaboração: agpatriota",1,at = bb[1,1],line=4)
dev.off()

