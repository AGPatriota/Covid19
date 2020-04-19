#Reading the required packages
require('ggplot2')
require('plotly')
require('gganimate')
require('magick')



#Initializing  the counter
kk=0

#Minimum distance to avoid being infected "dist0" varies in (1.2,1.7,2.2,2.7)

for(dist0 in c(1.2,1.7,2.2,2.7)){
	kk=kk+1

	#Scenario Parameters

	#Population size
	Pop  = 400

	#Number os steps
	T    = 500
	
	#Uniform distribution Parameter
	a    = 20

	#Number of clusters
	NN   = 20

	#Auxiliary parameters
	r00 = a
	espaco = a*0.2

	#Fixing the random seed
	set.seed(123)

	aa = seq(-5,5,length = 4)

	A = list()
	Bounderies = function(A, r0, x1, x2){
		A = cbind(A[,1]+x1,A[,2]+x2)
		A = A[(A[,1]-x1)^2+(A[,2]-x2)^2<r0^2, ]
		return(A)
	}
	for(q in 1:NN){
		A[[q]] = matrix(runif(Pop*2, -a,a), Pop,2)
	}

	#Creating the scenario
	B=numeric()
	for(q in 1:NN){
		A[[q]]<- Bounderies(A[[q]], r0=r00, (q-1)*sin(q-1)*espaco , (q-1)*cos(q-1)*espaco)
		B = rbind(B,cbind(q,A[[q]]))
	}

	#See the generated data
	#plot(B[,-1], cex = 0.5)

	#Updating the total population size. It depends on the number of clusters

	Pop = dim(B)[1]
	B1         = B

	#Parameters
	#steps until death
	pp         = 10

	#Number of initial infected
	Infec      =round(0.003*sum(B1[,1]==1))


	#Probability of dying of an infected after 10 steps
	mu         = 0.0035/pp

	#Probability of recovering of an infected after 10 steps
	rec        = 0.08/pp

	#Probability of transmission(or infection) from an infected if he is close
	prob.inf   = 0.6

	#Probability of a re
	rever.susc = 0.0001/pp
	rever.dea  = 0.0001/pp


	step0      = 0.4
	Interv     = FALSE
	inter.prop = 0.01

	#Proportion of the population that is in lockdown	
	Pop.locked = 0.0


	n.death=3


	#All are susceptible
	col0       = rep("royalblue3", Pop)

	#Except some: Infec
	col0[B1[,1]==1][sample(1:sum(B1[,1]==1), Infec)]<-"tomato2"

	#Creating first step Data set.
	BD = data.frame(Gen =1,Pop0= 1:Pop,B1, Type= col0)

	#Angles to build the random walking 
	theta = runif(Pop,0,2*pi)
	select.inter = 1

	#Starting T=500 steps
	for(i in 2:T){
		death.ind <- col0=="black"
		if(sum(death.ind)>n.death & !Interv){
			Interv = TRUE
			select.inter = ifelse(!death.ind ,1,0)
			select.inter[select.inter==1] <- select.inter[select.inter==1]*rbinom(sum(select.inter==1),1,1-Pop.locked)
	}
		alpha0=0.5
		theta =   (1-alpha0)*theta + alpha0*runif(Pop,0,2*pi)
		aux  = ifelse(rbinom(Pop,1, prob=0.5)==0, 1, -1)*ifelse(death.ind,0,1)*select.inter
		B1[,-1] = B1[,-1] + aux*step0*cbind(cos(theta),sin(theta))


		Normal = rbind(B1[which(col0=="royalblue3"),])
		Infect = rbind(B1[which(col0=="tomato2"),])

		distancia = function(i) sqrt((Infect[i,2]-Normal[,2])^2+(Infect[i,3]-Normal[,3])^2) < dist0
		if(dim(Infect)[1]==0 |dim(Normal)[1]==0){
			teste <- FALSE}
		else{
			teste = sapply(1:dim(Infect)[1], distancia)
		}

		if(sum(teste)>0) {
			col00 = col0[which(col0=="royalblue3")][rowSums(rbind(teste==TRUE))>0]
			#susceptible infectado?
			col0[which(col0=="royalblue3")][rowSums(rbind(teste==TRUE))>0] = ifelse(rbinom(length(col00),1,prob=prob.inf)==1, "tomato2", "royalblue3")
		}

		if(sum(col0=="tomato2")>0) {
			col00 = col0[which(col0=="tomato2")]
			#infectado recupera?
			col0[which(col0=="tomato2")] = ifelse(rbinom(length(col00),1,prob=rec)==1, "palegreen3","tomato2")
		}
		if(sum(col0=="tomato2")>0) {
			col00 = col0[which(col0=="tomato2")]
			#infectado morre?
			col0[which(col0=="tomato2")] = ifelse(rbinom(length(col00),1,prob=mu)==1, "black","tomato2")
		}
		if(sum(col0=="")>0) {
			col00 = col0[which(col0=="palegreen3")]
			#recuperado se torna suceptivel?
			col0[which(col0=="palegreen3")] = ifelse(rbinom(length(col00),1,prob=rever.susc)==1, "royalblue3","palegreen3")
		}
		if(sum(col0=="palegreen3")>0) {
			col00 = col0[which(col0=="palegreen3")]
			#recuperado morre?
			col0[which(col0=="palegreen3")] = ifelse(rbinom(length(col00),1,prob=rever.dea)==1, "black","palegreen3")
		}

		#Updating the data frame for the new generation
		BD1 = data.frame(Gen =i,Pop0= 1:Pop,B1,  Type= col0)
		BD = rbind(BD,BD1)
}

<<<<<<< HEAD
=======


>>>>>>> 6f8691321855970c3591ad3f6d6226b9ae64a6f4
levels(BD$Type)[levels(BD$Type)=="tomato2"]="Infected"
levels(BD$Type)[levels(BD$Type)=="royalblue3"]="Susceptible"
levels(BD$Type)[levels(BD$Type)=="black"]="Deaths"
levels(BD$Type)[levels(BD$Type)=="palegreen3"]="Recovered"

<<<<<<< HEAD

=======
>>>>>>> 6f8691321855970c3591ad3f6d6226b9ae64a6f4
i<- i-1
BD2 =  t(sapply(1:i, function(j) table(BD[BD$Gen==j,]$Type)))
BD2 = data.frame(Gen=c(rep(1:i,dim(BD2)[2])),Cases = c(BD2), Type =c(rep(colnames(BD2)[1],i), rep(colnames(BD2)[2], i),rep(colnames(BD2)[3],i), rep(colnames(BD2)[4], i)))
BD2[,2] = BD2[,2]/Pop*100 
 
a= ggplot(BD2,aes(x=Gen, y=Cases, color=Type)) +geom_path() + geom_point(alpha=0.7)+ 
	labs(title = 'Proportion of Cases (%)', x = 'steps', y = '')+
		scale_color_manual(values=c("Black", "tomato2","palegreen3","royalblue3"),labels =expression(paste("Deaths:         ", frac(dD,dt) == mu * I),paste("Infected:        ",frac(dI,dt) == r* beta*S*frac(I,N)-gamma* I - mu*I),paste("Recovered:   ",frac(dR,dt) == gamma* I),paste("Susceptible: ",frac(dS,dt) == -r* beta*S*frac(I,N))))+ 	
			theme(legend.text.align = 0) +	transition_reveal(along=Gen) +ease_aes('linear')


col2 = c(NA,NA,NA,NA)
col2[levels(BD$Type)=="Recovered"] = "palegreen3"
col2[levels(BD$Type)=="Deaths"] = "black"
col2[levels(BD$Type)=="Susceptible"] = "royalblue3"
col2[levels(BD$Type)=="Infected"] = "tomato2"

w=3
b = ggplot(BD,aes(V2,V3, color = Type)) +xlim(range(BD[,c(4,5)])[1],range(BD[,c(4,5)])[2])+  ylim(range(BD[,c(4,5)])[1],range(BD[,c(4,5)])[2]*1.65)+geom_point(alpha=0.7,size=0.01)+ labs(title = 'Step: {frame_time}', x = '', y = '', caption="Simulated data (AGPatriota)")+theme(axis.text.x=element_blank(), axis.text.y=element_blank(), panel.background = element_rect(fill = "white"),axis.ticks=element_blank())+ xlab(paste("After ", n.death, " deaths, lockdown of ", Pop.locked*100, "%", sep="")) + theme(legend.position="none") + 
	annotate("text",x=mean(range(BD[,c(4,5)])),y= range(BD[,c(4,5)])[2]*1.65, label= paste("N: ", Pop, ";  Number of initial infected: ",Infec , sep=""),size=w) +
		annotate("text",x=mean(range(BD[,c(4,5)])),y= range(BD[,c(4,5)])[2]*1.5, label= paste("P(Transmission| infected around): ", prob.inf*100, "%", sep=""),size=w) +
		       	annotate("text",x=mean(range(BD[,c(4,5)])),y= range(BD[,c(4,5)])[2]*1.35, label= paste("P(Death in ", pp, " steps): ", mu*pp*100, "%", sep="" ),size=w) +
			    	annotate("text",x=mean(range(BD[,c(4,5)])),y= range(BD[,c(4,5)])[2]*1.2, label= paste("P(Recovery in ", pp, " steps): ", rec*pp*100, "%", sep="" ),size=w)+ 
					annotate("text",x=mean(range(BD[,c(4,5)])),y= range(BD[,c(4,5)])[2]*1.05, label= paste("Minimum distance to avoid infection: (",dist0,")", sep="" ),size=w)+ 
						geom_segment(x=range(BD[,c(4,5)])[2]*0.86,y=range(BD[,c(4,5)])[2]*1.02, xend=range(BD[,c(4,5)])[2]*0.86+dist0, yend = range(BD[,c(4,5)])[2]*1.02, lineend="butt",col="black") + transition_time(as.integer(Gen)) +scale_color_manual(values=col2)  + ease_aes('linear')


g = ifelse(i < 100, i, 150)
a_gif <- animate(a, width = 340, height = 240,nframes = g)
b_gif <- animate(b, width = 240, height = 240,nframes=g)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(k in 2:g){
  combined <- image_append(c(a_mgif[k], b_mgif[k]))
  new_gif <- c(new_gif, combined)
}

print(kk)
image_write(new_gif, paste("Gifs/Covid19-",kk,".gif", sep=""))
}

