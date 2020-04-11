#Reading the data

#Continents by https://www.kaggle.com/statchaitya/country-to-continent
BD.Cont =read.csv("continent2country.csv", header = TRUE)

#Cases
BD.cases.or =  read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"), header = TRUE)

#Deaths
BD.deaths.or <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"), header = TRUE)

#Recoveries
BD.rec.or <- read.csv(url("https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_recovered_global.csv&filename=time_series_covid19_recovered_global.csv"),  header = TRUE)

#required libraries
require('ggplot2')
require('plotly')
require('gganimate')
require('magick')


#Selecting the countries: Brazil  Germany Italy   Sweden  US 
countries <- c(29,121, 138,206,226)

#If you ndo not want to select countries, uncomment below. However, the counting deaths will not be correct for some countries because of the Province.State column
#countries = TRUE
BD.cases  <- BD.cases.or[countries,]
BD.deaths <- BD.deaths.or[countries,]
BD.rec    <- BD.rec.or[countries,]

#Transform character to Date
dias.c <- as.Date(substr(names(BD.cases[,-c(1:4)]),2,10), "%m.%d.%y")
dias.d <- as.Date(substr(names(BD.deaths[,-c(1:4)]),2,10), "%m.%d.%y")
dias.r <- as.Date(substr(names(BD.rec[,-c(1:4)]),2,10), "%m.%d.%y")

#inserting Continents in the Data Frame

Cont.c <- character(dim(BD.cases.or)[1])
Cont.d <- character(dim(BD.deaths.or)[1])
Cont.r <- character(dim(BD.rec.or)[1])

for(i in 1:dim(BD.Cont)[1]){
	Cont.c[BD.cases.or[,2] == as.character(BD.Cont[i,1])]  <- as.character(BD.Cont[i,2])
	Cont.d[BD.deaths.or[,2] == as.character(BD.Cont[i,1])] <- as.character(BD.Cont[i,2])
	Cont.r[BD.rec.or[,2] == as.character(BD.Cont[i,1])]    <- as.character(BD.Cont[i,2])
	}

#Data frame with continentes
BD.cases0  <- cbind(Cont.c,BD.cases.or)
BD.deaths0 <- cbind(Cont.d,BD.deaths.or)
BD.rec0    <- cbind(Cont.r,BD.rec.or)

dias.c0 <- as.Date(substr(names(BD.cases0[,-c(1:5)]),2,10), "%m.%d.%y")
dias.d0 <- as.Date(substr(names(BD.deaths0[,-c(1:5)]),2,10), "%m.%d.%y")
dias.r0 <- as.Date(substr(names(BD.rec0[,-c(1:5)]),2,10), "%m.%d.%y")

#Creating a more reader friendly data frame
BD.c  <- data.frame(Country =rep(BD.cases[,2],dim(BD.cases)[2]-4), Cases = c(as.matrix(BD.cases[,-c(1:4)])), Days = rep(dias.c,each = dim(BD.cases)[1]))
BD.d  <- data.frame(Country =rep(BD.deaths[,2],dim(BD.deaths)[2]-4), Deaths = c(as.matrix(BD.deaths[,-c(1:4)])), Days = rep(dias.d,each = dim(BD.deaths)[1]))
BD.r  <- data.frame(Country =rep(BD.rec[,2],dim(BD.rec)[2]-4), Recoveries = c(as.matrix(BD.rec[,-c(1:4)])), Days = rep(dias.r,each = dim(BD.rec)[1]))

BD.c0 <- data.frame(Continent =rep(BD.cases0[,1],dim(BD.cases0)[2]-5), Country =rep(BD.cases0[,3],dim(BD.cases0)[2]-5), Cases = c(as.matrix(BD.cases0[,-c(1:5)])), Days = rep(dias.c0,each = dim(BD.cases0)[1]))
BD.d0 <- data.frame(Continent =rep(BD.deaths0[,1],dim(BD.deaths0)[2]-5),Country =rep(BD.deaths0[,3],dim(BD.deaths0)[2]-5), Deaths = c(as.matrix(BD.deaths0[,-c(1:5)])), Days = rep(dias.d0,each = dim(BD.deaths0)[1]))
BD.r0 <- data.frame(Continent =rep(BD.rec0[,1],dim(BD.rec0)[2]-5),Country =rep(BD.rec0[,3],dim(BD.rec0)[2]-5), Recoveries = c(as.matrix(BD.rec0[,-c(1:5)])), Days = rep(dias.r0,each = dim(BD.rec0)[1]))


#Selecting Deaths > 0
BD.d        <-  BD.d[BD.d[,2]>0,]
BD.d0       <- BD.d0[BD.d0[,3]>0,]

#Translating to Portugues
names(BD.d) <- c("Países", "Mortos", "Dias")
names(BD.d0)<- c("Continentes", "Países", "Mortos", "Dias")
BD.d[,1]    <- factor(BD.d[,1])
BD.d0[,1]   <- factor(BD.d0[,1])
BD.d0[,2]   <- factor(BD.d0[,2])

#Translating to Portugues
levels(BD.d[,1]) <- c("Brasil", "Alemanha", "Itália", "Suécia", "Estados Unidos")

#Creating a new data frame with a counting since the first death
BD1 <- data.frame()
for(j in levels(BD.d[,1])){
	aux <- BD.d[BD.d[,1] == j,]
	BD1 <- rbind(BD1, cbind(id=1:dim(aux)[1],aux))
}

BD2 <- data.frame()
for(j in levels(BD.d0[,1])[-1]){
	aux	<- BD.d0[BD.d0[,1] == j,]
	aux.d	<- tapply(aux[,3], aux[,4], sum)
	aux.d	<- data.frame(id = 1:length(aux.d),Cont = rep(j, length(aux.d)),aux.d, as.Date(names(aux.d)))
	BD2 <- rbind(BD2, aux.d)
}
names(BD2)<- c("id",names(BD.d0)[-2])




#Plots for the selected countries with Portugues title, labels and legend

a = ggplot(BD1, aes(x =id, y=Mortos, color = Países, group= Países)) +geom_path() + 
	 geom_point(alpha=0.7)+geom_text(aes(label = Países, colour = Países), hjust=1.5, size = 3.5, fontface = "bold")+
	 	transition_reveal(along=id) +
			labs(title=paste("Evolução do número de mortos desde o primeiro morto."), x = 'Dias desde o primeiro morto', y = 'Número de mortos')+
				theme(legend.position = "none")+theme(legend.title = element_blank())+ theme(text =element_text(size=13))+
					view_follow()

a1 = ggplot(BD1, aes(x =id, y=log(Mortos), color = Países, group= Países)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(aes(label = Países, colour = Países), hjust=1.5, size = 3.5, fontface = "bold")+
		transition_reveal(along=id) +
			labs(title=paste("Dados: https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"), x = 'Dias desde o primeiro morto (Elaborado por: AGPatriota)', y = 'Log do número de mortos')+ 	
				theme(text =element_text(size=13),plot.title = element_text(size = 11))+theme(legend.position = "none")+
					view_follow()

a.gif =  animate(a, width = 440, height = 340)
a.gif = image_read(a.gif)
a1.gif =  animate(a1, width = 440, height = 340)
a1.gif = image_read(a1.gif)


#Appending two gifs
new_gif <- image_append(c(a.gif[1],a1.gif[1]))
for(k in 2:100){
  combined <- image_append(c(a.gif[k], a1.gif[k]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif , paste("Evolucao-mortos.gif", sep=""))


#Plots for the Continents with Portugues title, labels and legend
b = ggplot(BD2, aes(x =id, y=Mortos, color = Continentes, group= Continentes)) +geom_path() + 
	geom_point(alpha=0.7)+geom_text(aes(label = Continentes, colour = Continentes), hjust=1.5, size = 3.5, fontface = "bold")+	
		transition_reveal(along=id) +
			labs(title=paste("Evolução do número de mortos desde o primeiro morto.\n Dados:\n https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases"), x = 'Dias desde o primeiro morto', y = 'Número de mortos', caption="(Elaborado por: AGPatriota)")+
				theme(legend.position = "none")+view_follow()

b.gif =  animate(b, width = 440, height = 340)
b.gif = image_read(b.gif)

image_write(b.gif, paste("Evolucao-mortos-Continente.gif", sep=""))




