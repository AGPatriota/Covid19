#https://raw.githubusercontent.com/belisards/coronabr/master/dados/corona_brasil.csv
#https://www.youtube.com/watch?v=k6nLfCbAzgo


BD = read.csv("corona_brasil.csv", sep=",")

BD$date <- as.Date(BD$date)

TT = tapply(BD$cases, BD$date, sum)
TT.D = tapply(BD$deaths, BD$date, sum, na.rm=TRUE)
plot(TT~as.Date(names(table(BD$date))), type="l", ylab="Número de casos", xlab="", main="COVID 19 (Brasil)", lwd=5)
points(TT.D~as.Date(names(table(BD$date))), type="l",lwd=5, col="red4")

mtext("Elaborado por: agpatriota", 3)
mtext("Fonte dos dados: https://github.com/belisards/coronabr",3,.8)
abline(h=0, lty=2)


##################################3
#Solving ODE equations
###################################

require('deSolve')
#  Lorenz <- function(t, state, parameters) {
#       with(as.list(c(state, parameters)), {
#         dX <-  a * X + Y * Z
#         dY <-  b * (Y - Z)
#         dZ <- -X * Y + c * Y - Z
#         list(c(dX, dY, dZ))
#       })
#     }
#parameters <- c(a = -8/3, b = -10, c = 28)
#state      <- c(X = 1, Y = 1, Z = 1)
#times      <- seq(0, 100, by = 0.01)
#out <- ode(y = state, times = times, func = Lorenz, parms = parameters)
#plot(out)



SEIR = function(t,state,parameters){
     with(as.list(c(state, parameters)), {
	dS = Lambda - r*beta*S*I/(S + E + I + R) -mu*S
	dE = r*beta*S*I/(S + E + I + R) - eps*E
	dI = eps*E-gamma*I-mu*I
	dR = gamma*I-mu*R
        list(c(dS, dE, dI, dR))
       })
}     

parameters <- c(Lambda = 00, r = 3, beta= 0.7,mu =0.05, eps= 0.4,gamma=0.7)
state      <- c(S = 200000, E = 100, I = 2000, R=5)
times      <- seq(0, 356, length(1000))
out <- ode(y = state, times = times, func = SEIR, parms = parameters)
plot(out[,2]~out[,1], type="l", ylim = range(out[-1]), xlab="", lwd=2)
#points(out[,3]~out[,1], type="l", col="red4", lwd=2)
points(out[,4]~out[,1], type="l", col="blue3", lwd=2)
#points(out[,5]~out[,1], type="l", col="tomato3",lwd=2)
R0 = parameters[2]*parameters[3]*parameters[5]/((parameters[6]+parameters[4])*(parameters[5]+parameters[4]))
legend(round(3*length(times)/4),round(range(out[-1])[2]*0.9), c("Susceptible", "Infectious"), col=c("black", "blue3"), lty=1, lwd =2)


##SEIR Mathematical Model
#dSdt = Lambda - r*beta*S*I/N -mu*S
#dEdt = r*beta*S*I/N - eps*E
#dIdt = eps*E-gamma*I-mu*I
#dRdt = gamma*I-mu*R
#N = (S + E + I + R)

################################################
################################################
#Simulando dados
################################################
################################################

Pop = 700
T   = 500
a   = 5
#set.seed(123)
aa = seq(-5,5,length = 4)
A = matrix(runif(Pop*2, -a,a), Pop,2)
Bounderies = function(A1, a1, a2, a3, a4){
	A1[,1] = ifelse(A1[,1]< a1 , a1, A1[,1])
	A1[,1] = ifelse(A1[,1]> a2 , a2, A1[,1])
	A1[,2] = ifelse(A1[,2]< a3 , a3, A1[,2])
	A1[,2] = ifelse(A1[,2]> a4 , a4, A1[,2])
	return(A1)
}


aux1 = which(A[,1]<aa[2] & A[,2]< aa[2])
aux2 = which(A[,1]>aa[2] & A[,1]< aa[3] & A[,2]> aa[2] & A[,2]< aa[3])
aux3 = which(A[,1]>aa[3] & A[,1]< aa[4] & A[,2]> aa[3] & A[,2]< aa[4])
aux4 = which(A[,1]<aa[2] & A[,2]> aa[3])
aux5 = which(A[,1]>aa[3] & A[,2]< aa[2])

A = A[c(aux1, aux2, aux3, aux4, aux5),]
aux1 = which(A[,1]<aa[2] & A[,2]< aa[2])
aux2 = which(A[,1]>aa[2] & A[,1]< aa[3] & A[,2]> aa[2] & A[,2]< aa[3])
aux3 = which(A[,1]>aa[3] & A[,1]< aa[4] & A[,2]> aa[3] & A[,2]< aa[4])
aux4 = which(A[,1]<aa[2] & A[,2]> aa[3])
aux5 = which(A[,1]>aa[3] & A[,2]< aa[2])

Pop = dim(A)[1]
require('ggplot2')
require('plotly')
require('gganimate')
require('magick')
require('cowplot')
library(tweenr)
A = data.frame(A)
A1         = A
#Parameters
alpha0     = 0.5
Infec      = 0.05*Pop
dist0      = 0.05
mu         = 0.002
rec        = 0.008
prob.inf   = 0.8
rever.susc = 0.0001
rever.dea  = 0.0001
prop.Travel= 0.9

#Viajantes
aux.Travel = c(aux1,aux3,aux4,aux5)
aux.Travel = aux.Travel[rbinom(1:length(aux.Travel), size=1, prob=prop.Travel)==1]

col0       = rep("blue", Pop)
col0[sample(1:Pop, Infec)]<-"red3"
BD = data.frame(Gen =1,Pop0= 1:Pop,A1, Type= col0)
theta = runif(Pop,0,2*pi)
Cont = numeric(Pop)
Cont[col0=="red3"] = 1
Traveled = FALSE
for(i in 2:T){
death.ind <- col0=="black"
aux.Travel <- aux.Travel[!death.ind[aux.Travel]]

theta =   (1-alpha0)*theta + alpha0*runif(Pop,0,2*pi)
aux  = ifelse(rbinom(Pop,1, prob=0.5)==0, 1, -1)*ifelse(death.ind,0,1)
A1 = A1 + aux*0.1*cbind(cos(theta),sin(theta))
plot(A1)
#applying the bounderies
if(!Traveled){
	A1[aux1,] = Bounderies(A1[aux1,],a1= aa[1],a2=aa[2],a3=aa[1],a4=aa[2])
	A1[aux2,] = Bounderies(A1[aux2,],a1= aa[2],a2=aa[3],a3=aa[2],a4=aa[3])
	A1[aux3,] = Bounderies(A1[aux3,],a1= aa[3],a2=aa[4],a3=aa[3],a4=aa[4])
	A1[aux4,] = Bounderies(A1[aux4,],a1= aa[1],a2=aa[2],a3=aa[3],a4=aa[4])
	A1[aux5,] = Bounderies(A1[aux5,],a1= aa[3],a2=aa[4],a3=aa[1],a4=aa[2])
}
if(!Traveled){
	A1[aux1[!(aux1 %in%aux.Travel)],] = Bounderies(A1[aux1[!(aux1 %in%aux.Travel)],],a1= aa[1],a2=aa[2],a3=aa[1],a4=aa[2])
	A1[aux2,]                         = Bounderies(A1[aux2,],a1= aa[2],a2=aa[3],a3=aa[2],a4=aa[3])
	A1[aux3[!(aux3 %in%aux.Travel)],] = Bounderies(A1[aux3[!(aux3 %in%aux.Travel)],],a1= aa[3],a2=aa[4],a3=aa[3],a4=aa[4])
	A1[aux4[!(aux4 %in%aux.Travel)],] = Bounderies(A1[aux4[!(aux4 %in%aux.Travel)],],a1= aa[1],a2=aa[2],a3=aa[3],a4=aa[4])
	A1[aux5[!(aux5 %in%aux.Travel)],] = Bounderies(A1[aux5[!(aux5 %in%aux.Travel)],],a1= aa[3],a2=aa[4],a3=aa[1],a4=aa[2])
}

#Voltando do Centro:  aa[2] aa[3]
#plot(A1)
#points(A1[aux1,],pch=19, col="red4")

if(Traveled & i%%10==4){
	A1[!death.ind,][aux.Travel %in% aux1[aux1 %in%aux.Travel],] <- BD[BD$Gen== 4,c(3,4)][!death.ind,][aux.Travel %in%  aux1[aux1 %in%aux.Travel],]
	A1[!death.ind,][aux.Travel %in% aux3[aux3 %in%aux.Travel],] <- BD[BD$Gen== 4,c(3,4)][!death.ind,][aux.Travel %in%  aux3[aux3 %in%aux.Travel],]
	A1[!death.ind,][aux.Travel %in% aux4[aux4 %in%aux.Travel],] <- BD[BD$Gen== 4,c(3,4)][!death.ind,][aux.Travel %in%  aux4[aux4 %in%aux.Travel],]
	A1[!death.ind,][aux.Travel %in% aux5[aux5 %in%aux.Travel],] <- BD[BD$Gen== 4,c(3,4)][!death.ind,][aux.Travel %in%  aux5[aux5 %in%aux.Travel],]
Traveled <- FALSE
}

#Indo para o Centro
if(i%%10==0){
	A1[aux1[aux1 %in%aux.Travel],] <- A1[aux1[aux1 %in%aux.Travel],]+(aa[2]-aa[1])
	A1[aux3[aux3 %in%aux.Travel],] <- A1[aux3[aux3 %in%aux.Travel],]-(aa[3]-aa[2])
	A1[aux4[aux4 %in%aux.Travel],] <- cbind(A1[aux4[aux4 %in%aux.Travel],1]+(aa[2]-aa[1]),A1[aux4[aux4 %in%aux.Travel],2] - (aa[3]-aa[2]))
	A1[aux5[aux5 %in%aux.Travel],] <- cbind(A1[aux5[aux5 %in%aux.Travel],1]-(aa[4]-aa[3]),A1[aux5[aux5 %in%aux.Travel],2] + (aa[2]-aa[1]))
Traveled <- TRUE
}

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




BD2 =  t(sapply(1:i, function(j) table(BD[BD$Gen==j,]$Type)))
BD2 = data.frame(Gen=c(rep(1:i,dim(BD2)[2])),Cases = c(BD2), Type =c(rep(colnames(BD2)[1],i), rep(colnames(BD2)[2], i),rep(colnames(BD2)[3],i), rep(colnames(BD2)[4], i)))

BD2[,2] = BD2[,2]/Pop 
#scale_color_manual(values=c("tomato4","#69b3a2", "Black", 

a= ggplot(BD2,aes(x=Gen, y=Cases, color=Type)) +geom_path() + geom_point(alpha=0.7)+ labs(title = 'Proportion of Cases', x = 'steps', y = '')	+ #annotate('text', x  = i-4, y=Pop*0.8,label = paste("dS/dt == - ",) ) +
	transition_reveal(id=Gen, along=Gen) +scale_color_manual(values=c("Black", "red3","springgreen3","blue"))+ease_aes('linear') 

col2 = c(NA,NA,NA,NA)
col2[levels(BD$Type)=="Recovered"] = "springgreen3"
col2[levels(BD$Type)=="Deaths"] = "black"
col2[levels(BD$Type)=="Susceptible"] = "blue"
col2[levels(BD$Type)=="Infected"] = "red3"

b = ggplot(BD,aes(X1,X2, color = Type)) + geom_point(alpha=0.7,size=1)+ labs(title = 'Step: {frame_time}', x = '', y = '')+theme(axis.text.x=element_blank(), axis.text.y=element_blank()) + transition_time(as.integer(Gen)) +scale_color_manual(values=col2)  +
	ease_aes('linear')

g = ifelse(i < 100, i, 150)
a_gif <- animate(a, width = 240, height = 240,nframes = g)
b_gif <- animate(b, width = 240, height = 240,nframes=g)


a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(k in 2:g){
  combined <- image_append(c(a_mgif[k], b_mgif[k]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, "teste.gif")



