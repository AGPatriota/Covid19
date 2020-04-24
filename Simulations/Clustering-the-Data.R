require('cluster')
require('ggplot2')
require('gganimate')
require('magick')

BD = read.table(url("https://www.ime.usp.br/~patriota/Covid19-Sim/Sim-Data.dat"))

 gc()
Km = kmeans(BD[,c(4,5)], centers=10)
BD$Cluster =  Km$cluster

plot.clusters= TRUE

if(plot.clusters){
	a= ggplot(BD, aes(x=V2 ,y=V3, color=Cluster ))+xlim(range(BD[,c(4,5)])[1],range(BD[,c(4,5)])[2])+  ylim(range(BD[,c(4,5)])[1],range(BD[,c(4,5)])[2]*1.65)+geom_point(alpha=0.7,size=0.01)+ labs(title = 'Step: {frame_time}', x = '', y = '', caption="Simulated data (AGPatriota)")+theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.background = element_rect(fill = "white"),axis.ticks=element_blank())+  transition_time(as.integer(Gen))
	a= animate(a, width = 340, height = 240,nframes = 120)
	a = image_read(a)
	image_write(a, "Gifs/Covid19-1/Cluster-Sim1.gif")
}




aux = function(s,w) table(BD[BD[,1]==s & BD[,7]==w,6])
DATA = function(t) t(sapply(1:t,aux))
DD = list()
for(j in 1:max(BD$Cluster)){
	aux0 = function(s) aux(s,w=j)
	DATA = function(t) t(sapply(1:t,aux0))
	DD[[j]] =  DATA(500)
print(j)}


for(j in 1:10){
	BD2 = data.frame(Gen = rep(1:500, 4),Cases = c(DD[[j]]), Type= c(rep("Infectados", 500), rep("Mortos", 500), rep("Recuperados", 500), rep("Susceptíveis", 500)))
	col2 = c(NA,NA,NA,NA)
	co2[levels(BD2$Type)=="Recuperados"] = "palegreen3"
	col2[levels(BD2$Type)=="Mortos"] = "black"
	col2[levels(BD2$Type)=="Susceptíveis"] = "royalblue3"
	col2[levels(BD2$Type)=="Infectados"] = "tomato2"
	a =ggplot(BD2, aes(x=Gen, y = Cases, color = Type)) + geom_line()+scale_color_manual(values=col2)+	labs(title = paste("Cluster: ", j, sep=""))  +	transition_reveal(along=Gen) + ease_aes('linear')
	a= animate(a, width = 340, height = 240,nframes = 120)
	a = image_read(a)
	image_write(a, paste("Gifs/Covid19-1/Cluster-Sim1-",j,".gif", sep=""))
}



require('deSolve')
SEIR0 = function(t,state,parameters){
     with(as.list(c(state, parameters)), {
	dS = -beta*S*I/(S +  I + R + M)
	dI = beta*S*I/(S +  I + R+M)-gamma*I - mu*I
	dR = gamma*I 
	dM = mu*I 
        list(c(dS, dI, dR, dM))
       })
}
#Iniciando do dia 28
d0=1
parameters0 <- c(beta=0.1,mu =0.001, gamma=0.004)

state0=list()
for(j in 1:max(BD$Cluster)){
	state0[[j]]      <- c(S =  as.numeric(max(DD[[j]][d0,])),  I = as.numeric(DD[[j]][d0,2]), R=as.numeric(DD[[j]][d0,3]),M = as.numeric(DD[[j]][d0,4]))
}


times      <- seq(d0, 500, 1)
if(d0>1){ sub <- -c(1:(d0-1))}
if(d0==1){sub <- TRUE}


