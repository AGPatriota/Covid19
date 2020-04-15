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
	dS = Lambda - r*beta*S*I/(S + E + I + R) - mu0*S
	dE = r*beta*S*I/(S + E + I + R) - eps*E - mu0*E
	dI = eps*E-gamma*I-mu*I
	dR = gamma*I - mu0*R
        list(c(dS, dE, dI, dR))
       })
}     

parameters <- c(Lambda = 0, r = 0.9, beta= 0.12,mu =0.01, eps= 0.5,gamma=0.04, mu0=0.0001)
state      <- c(S = 0.9, E = 0.1, I = 0.001, R=0.0)
times      <- seq(0, 500, length(1000))
out <- ode(y = state, times = times, func = SEIR, parms = parameters)

jpeg("Gifs/Dif-Eq.jpg")
plot(out[,2]~out[,1], type="l", ylim = range(out[,-1]), xlab="", lwd=2, col="blue3", ylab="Cases")
#points(out[,3]~out[,1], type="l", col="blue3", lwd=2)
points(out[,4]~out[,1], type="l", col="red4", lwd=2)
points(out[,5]~out[,1], type="l",lwd=2)
R0 = parameters[2]*parameters[3]*parameters[5]/((parameters[6]+parameters[4])*(parameters[5]+parameters[4]))
legend(round(2.8*length(times)/4),range(out[,-1])[2], c("Susceptible", "Infected", "Removed"), col=c("blue3","red4", "black" ), lty=1, lwd =2)
dev.off()



##SEIR Mathematical Model
#dSdt = Lambda - r*beta*S*I/N -mu*S
#dEdt = r*beta*S*I/N - eps*E
#dIdt = eps*E-gamma*I-mu*I
#dRdt = gamma*I-mu*R
#N = (S + E + I + R)
