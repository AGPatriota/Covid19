#https://covid.saude.gov.br/
#https://www.google.com/covid19/mobility/

require('gganimate')
require('ggplot2')
require('magick')
#require('readxl')
#BD = read_xlsx("~/Dropbox/Articles/Artigos_Pendentes/COVID19/algo/Covid19/Data-Analysis/Data/DT_PAINEL_COVIDBR_20200511.xlsx")

#BD =  read.csv("Data/DT_PAINEL_COVIDBR_20200510.csv", header = TRUE, sep=",")
#BD =  read.csv("Data/DT1_PAINEL_COVIDBR_20200512.csv", header = TRUE, sep=",")

BD<- list()

BD[[1]] =  read.csv("Data/HIST_PAINEL_COVIDBR_20jun2020.csv", header = TRUE, sep=",")


for(i in 1){
BD[[i]]$Dens=NA
BD[[i]]$Dens[BD[[i]]$estado=="SP"] <- 184.99
BD[[i]]$Dens[BD[[i]]$estado=="MG"] <- 36.09
BD[[i]]$Dens[BD[[i]]$estado=="RJ"] <- 394.62
BD[[i]]$Dens[BD[[i]]$estado=="BA"] <- 26.34
BD[[i]]$Dens[BD[[i]]$estado=="PR"] <- 57.37
BD[[i]]$Dens[BD[[i]]$estado=="RS"] <- 40.39
BD[[i]]$Dens[BD[[i]]$estado=="PE"] <- 97.45
BD[[i]]$Dens[BD[[i]]$estado=="CE"] <- 61.33
BD[[i]]$Dens[BD[[i]]$estado=="PA"] <- 6.91
BD[[i]]$Dens[BD[[i]]$estado=="SC"] <- 74.84
BD[[i]]$Dens[BD[[i]]$estado=="MA"] <- 21.46
BD[[i]]$Dens[BD[[i]]$estado=="GO"] <- 20.63
BD[[i]]$Dens[BD[[i]]$estado=="AM"] <- 2.66
BD[[i]]$Dens[BD[[i]]$estado=="ES"] <- 87.22
BD[[i]]$Dens[BD[[i]]$estado=="PB"] <- 71.16
BD[[i]]$Dens[BD[[i]]$estado=="RN"] <- 66.41
BD[[i]]$Dens[BD[[i]]$estado=="MT"] <- 3.86
BD[[i]]$Dens[BD[[i]]$estado=="AL"] <- 119.86
BD[[i]]$Dens[BD[[i]]$estado=="PI"] <- 13.01
BD[[i]]$Dens[BD[[i]]$estado=="DF"] <- 523.41
BD[[i]]$Dens[BD[[i]]$estado=="MS"] <- 7.78
BD[[i]]$Dens[BD[[i]]$estado=="SE"] <- 104.83
BD[[i]]$Dens[BD[[i]]$estado=="RO"] <- 7.47
BD[[i]]$Dens[BD[[i]]$estado=="TO"] <- 5.66
BD[[i]]$Dens[BD[[i]]$estado=="AC"] <- 5.37
BD[[i]]$Dens[BD[[i]]$estado=="AP"] <- 5.94
BD[[i]]$Dens[BD[[i]]$estado=="RR"] <- 2.7
#Problemas de conversão
BD[[i]]$populacaoTCU2019 <- as.numeric(as.character(BD[[i]]$populacaoTCU2019))
BD[[i]]$obitosAcumulados1 = BD[[i]]$obitosAcumulado/BD[[i]]$populacaoTCU2019*1000000
BD[[i]]$obitosAcumulados2 = BD[[i]]$obitosAcumulado/BD[[i]]$Dens
}

BD0 = list()

for(i in 1){
BD0[[i]] <- BD[[i]][,c(1:3,8,10,13,18,19,5)]
BD0[[i]]$data<-as.Date(BD0[[i]]$data)
BD0[[i]] <- BD0[[i]][BD0[[i]][,6]>99,]
BD0[[i]] <- BD0[[i]][BD0[[i]][,3]!= "",]
}


BBD0 = BD0[[1]]



BD1 <- data.frame()
for(j in as.numeric(names(table(BBD0[,9])))){
	aux <- BBD0[BBD0[,9] == j,]
	BD1 <- rbind(BD1, cbind(id=1:dim(aux)[1],aux))
}

BD1<- BD1[BD1[,7]!=611,]

gc()
gc()
a = ggplot(BD1, aes(x =id, y=obitosAcumulado, color = regiao, group= municipio)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>800),aes(label =municipio, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste("Óbitos segundo a data de divulgação por Município."), x = 'Dias desde o centésimo morto\n Dados: https://covid.saude.gov.br/', y = '')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
					view_follow()

a1 = ggplot(BD1, aes(x =id, y=log(obitosAcumulado), color = regiao, group= municipio)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>800),aes(label = municipio, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
		transition_reveal(along=id) +
			labs(title=paste("Escala Log"), x = 'Dias desde o centésimo morto\n (Elaborado por: AGPatriota)', y = '')+ 	
				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+
					view_follow()

#a2 = ggplot(BD1, aes(x =id, y=obitosAcumulados1, color = regiao, group= municipio)) +geom_path() +  
#	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>800),aes(label = municipio, colour = regiao), hjust=1.5, size = 3.5, fontface = "bold")+
#	 	transition_reveal(along=id) +
#			labs(title=paste("Por milhão de habitantes"), x = 'Dias desde o primeiro morto \n (Elaborado por: AGPatriota)', y = '')+
#				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=11),plot.title = element_text(size = 11))+
#					view_follow()

#a3 = ggplot(BD1, aes(x =id, y=log(obitosAcumulados1), color = regiao, group= municipio)) +geom_path() + 
#	geom_point(alpha=0.7)+geom_text(data=subset(BD1, BD1[,7]>200),aes(label = municipio, colour =regiao), hjust=1.5, size = 3.5, fontface = "bold")+
#		transition_reveal(along=id) +
#			labs(title=paste(""), x = '(Elaborado por: AGPatriota)', y = 'Log do número de mortos por covid (1Mi hab)')+ 	
#				theme(text =element_text(size=11),plot.title = element_text(size = 11))+theme(legend.position = "none")+				view_follow()

a.gif =  animate(a, width = 320, height = 300)
a.gif = image_read(a.gif)
a1.gif =  animate(a1, width = 320, height = 300)
a1.gif = image_read(a1.gif)
#a2.gif =  animate(a2, width = 320, height = 300)
#a2.gif = image_read(a2.gif)
#a3.gif =  animate(a3, width = 280, height = 240)
#a3.gif = image_read(a3.gif)
gc()
gc()


#Appending two gifs
#new_gif <- image_append(image_join(image_append(c(a.gif[1], a1.gif[1])),image_append(c(a2.gif[1], a3.gif[1]))), stack = TRUE)
new_gif <- image_append(c(a.gif[1], a1.gif[1]))


for(k in 2:100){
#  combined <- image_append(image_join(image_append(c(a.gif[k], a1.gif[k])),image_append(c(a2.gif[k], a3.gif[k]))),  stack = TRUE)
combined <- image_append(c(a.gif[k], a1.gif[k]))

  new_gif <- c(new_gif, combined)
}

image_write(new_gif , paste("Gifs/Evolucao-mortos-municipio.gif", sep=""))
gc()
gc()
   

