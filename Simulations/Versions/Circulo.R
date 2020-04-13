################################################
################################################
#Simulando dados
################################################
################################################
require('ggplot2')
require('plotly')
require('gganimate')
require('magick')
require('cowplot')
library('tweenr')
library("ggpubr")

Pop  = 5000
T    = 500
a    = 50
r00  = 15
#set.seed(123)
aa = seq(-5,5,length = 4)
A = matrix(runif(Pop*2, -a,a), Pop,2)

Bounderies = function(A, r0){
	A = A[A[,1]^2+A[,2]^2<r0^2, ]
	return(A)
}

#área central
Central = Bounderies(A,r00)


A = data.frame(Bounderies(A,a))
Pop = dim(A)[1]
A1         = A
#Parameters
alpha0     = 0.5
Infec      = 1
dist0      = 2.8
mu         = 0.008
rec        = 0.020
prob.inf   = 0.1
rever.susc = 0.0001
rever.dea  = 0.0001
prop.Travel= 0.5


#Viajantes
aux.Travel = A1[,1]^2+A1[,2]^2>= r00^2
aux.Travel[aux.Travel] = rbinom(1:sum(aux.Travel), size=1, prob=prop.Travel)==1


col0       = rep("blue", Pop)
col0[sample(1:Pop, Infec)]<-"red3"
BD = data.frame(Gen =1,Pop0= 1:Pop,A1, Type= col0)
theta = runif(Pop,0,2*pi)
Cont = numeric(Pop)
Cont[col0=="red3"] = 1
Traveled = FALSE

for(i in 2:T){
death.ind <- col0=="black"
aux.Travel <- (aux.Travel * !death.ind)==1
theta =   (1-alpha0)*theta + alpha0*runif(Pop,0,2*pi)
aux  = ifelse(rbinom(Pop,1, prob=0.5)==0, 1, -1)*ifelse(death.ind,0,1)
A1 = A1 + aux*0.1*cbind(cos(theta),sin(theta))
#plot(A1)
#points(A1[aux.Travel,],pch=19, col="red")
#OK

Ind1 = A1[,1]^2+A1[,2]^2>=a^2
A1[Ind1,] =  a* A1[Ind1,] / sqrt(rowSums(A1[Ind1,]^2))

if(sum(col0=="red3")==0) stop("There is no infected")
if(sum(col0=="blue")==0) stop("There is no normal")
Normal = A1[which(col0=="blue"),]
Infect = A1[which(col0=="red3"),]
distancia = function(i) sqrt((Infect[i,1]-Normal[,1])^2+(Infect[i,2]-Normal[,2])^2) < dist0
teste = sapply(1:dim(Infect)[1], distancia)

if(sum(teste)>0) {
	col00 = col0[which(col0=="blue")][rowSums(rbind(teste==TRUE))>0]
	#susceptible infectado?
	col0[which(col0=="blue")][rowSums(rbind(teste==TRUE))>0] = ifelse(rbinom(length(col00),1,prob=prob.inf)==1, "red3", "blue")
}

if(sum(col0=="red3")>0) {
	col00 = col0[which(col0=="red3")]
	#infectado recupera?
	col0[which(col0=="red3")] = ifelse(rbinom(length(col00),1,prob=rec)==1, "springgreen3","red3")
}
if(sum(col0=="red3")>0) {
	col00 = col0[which(col0=="red3")]
	#infectado morre?
	col0[which(col0=="red3")] = ifelse(rbinom(length(col00),1,prob=mu)==1, "black","red3")
}
if(sum(col0=="springgreen3")>0) {
	col00 = col0[which(col0=="springgreen3")]
	#recuperado se torna suceptivel?
	col0[which(col0=="springgreen3")] = ifelse(rbinom(length(col00),1,prob=rever.susc)==1, "blue","springgreen3")
}
if(sum(col0=="springgreen3")>0) {
	col00 = col0[which(col0=="springgreen3")]
	#recuperado morre?
	col0[which(col0=="springgreen3")] = ifelse(rbinom(length(col00),1,prob=rever.dea)==1, "black","springgreen3")
}
BD1 = data.frame(Gen =i,Pop0= 1:Pop,A1,  Type= col0)
BD = rbind(BD,BD1)
}

levels(BD$Type)[levels(BD$Type)=="red3"]="Infected"
levels(BD$Type)[levels(BD$Type)=="blue"]="Susceptible"
levels(BD$Type)[levels(BD$Type)=="black"]="Deaths"
levels(BD$Type)[levels(BD$Type)=="springgreen3"]="Recovered"



i<- i-1
BD2 =  t(sapply(1:i, function(j) table(BD[BD$Gen==j,]$Type)))
BD2 = data.frame(Gen=c(rep(1:i,dim(BD2)[2])),Cases = c(BD2), Type =c(rep(colnames(BD2)[1],i), rep(colnames(BD2)[2], i),rep(colnames(BD2)[3],i), rep(colnames(BD2)[4], i)))


BD2[,2] = BD2[,2]/Pop*100 
#scale_color_manual(values=c("tomato4","#69b3a2", "Black", 

a= ggplot(BD2,aes(x=Gen, y=Cases, color=Type)) +geom_path() + geom_point(alpha=0.7)+ labs(title = 'Proportion of Cases (%)', x = 'steps', y = '')+scale_color_manual(values=c("Black", "red3","springgreen3","blue"),labels =expression(paste("Deaths:         ", frac(dD,dt) == mu * I),paste("Infected:        ",frac(dI,dt) == -gamma* I - mu*I),paste("Recovered:   ",frac(dR,dt) == gamma* I),paste("Susceptible: ",frac(dS,dt) == -r* beta*S*frac(I,N))))+ 	theme(legend.text.align = 0) +
	transition_reveal(id=Gen, along=Gen) +ease_aes('linear')

#a2<- ggplot(BD2,aes(x=Gen)) +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_blank(), axis.text = element_blank(),axis.ticks=element_blank()) + geom_text(aes(2.5,75, label=(paste(expression("y = x "^-2*"")))),parse = TRUE)+labs(title = 'Differential Equations', x = '', y = '') +theme(plot.margin = unit(c(1,1,1,1), "cm")) + transition_reveal(id=Gen, along=Gen) +ease_aes('linear')




col2 = c(NA,NA,NA,NA)
col2[levels(BD$Type)=="Recovered"] = "springgreen3"
col2[levels(BD$Type)=="Deaths"] = "black"
col2[levels(BD$Type)=="Susceptible"] = "blue"
col2[levels(BD$Type)=="Infected"] = "red3"

b = ggplot(BD,aes(X1,X2, color = Type)) + geom_point(alpha=0.7,size=0.5)+ labs(title = 'Step: {frame_time}', x = '', y = '', caption="Simulated data (AGPatriota)")+theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.background = element_rect(fill = "white"),axis.ticks=element_blank()) + theme(legend.position="none") +transition_time(as.integer(Gen)) +scale_color_manual(values=col2)  +
	ease_aes('linear')

g = ifelse(i < 100, i, 150)
a_gif <- animate(a, width = 340, height = 240,nframes = g)
#a2_gif <- animate(a2, width = 240, height = 240,nframes = g)
b_gif <- animate(b, width = 240, height = 240,nframes=g)


a_mgif <- image_read(a_gif)
#a2_mgif <- image_read(a2_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(k in 2:g){
  combined <- image_append(c(a_mgif[k], b_mgif[k]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, "teste.gif")


