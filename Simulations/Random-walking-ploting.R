#Scenario 1 in https://www.ime.usp.br/~patriota/Covid19.html
#dist0=1.2


BD = read.table(url("https://www.ime.usp.br/~patriota/Covid19-Sim/Sim-Data.dat"))



jpeg("Figs/andancas.jpg", width=800, height=800)

par(mfrow=c(3,3))
for(i in 1:9){
	plot(BD[BD[,2]==i,c(4,5)], type="l", axes=FALSE, ylab="", xlab="", lty=2)
	points(BD[BD[,2]==i,c(4,5)][c(1,500),], col=c("green4", "red3"), pch=19, cex=2)
	box()
	if(i ==8){
		mtext("Elaboração: agpatriota", 1, 0,line=1.2)
		mtext("1a simulação em https://www.ime.usp.br/~patriota/Covid19.html", 1, 0,line=2.5)
	}
	if(i ==2){
		mtext("Ponto verde: início do passeio \n Ponto vermelho: fim do passeio", 3, 0,line=1)
	}
}
dev.off()



