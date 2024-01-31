
#------------------------------ SML Procedure -------------------------------#
rm(list=ls())
if (Sys.info()["user"] == "alecr") {
  projdir <- "C:/Users/alecr/OneDrive/Documents/TSE_2023-24/Empirical IO/Project/"
}

sml_data <- readRDS(paste0(projdir, "sml_data.rds"))

# Number of markets
M = n_distinct(sml_data$market)
# Number of players (airlines)
N = n_distinct(sml_data$airline)
#Number of Simulations (starting small)
S=50

#Number of cluster to make it in parallel
Nclust = 8

####Libraries
library(MASS)
library(data.table)
library(nloptr)
library(parallel)
library(dplyr)
options(digits = 5)

##### Simulation of error terms for SML N*M*S  
set.seed(31497)
uim = matrix(rnorm(N*M*S, mean = 0, sd = 1), N*M, S)
u0m = matrix(rnorm(M*S, mean = 0, sd = 1), M, S) #### %*% matrix(rep(1,N),N,1)

#### Explanatory variables

# Need to sort sml_data by market first

sml_data <- sml_data %>% arrange(market)

pop = sml_data$pop
City2 = sml_data$City2
dist = sml_data$dist
dist2 = dist^2
income = sml_data$income
slot = sml_data$slot
hub = sml_data$hub
Ncompet = sml_data$nentrants
Nthreats = sml_data$nentrythreats

Y = as.matrix(sml_data$I)
X = as.matrix(rbind(rep(1,N*M),pop[1:(N*M)],income[1:(N*M)],dist[1:(N*M)],
                    dist2[1:(N*M)],City2[1:(N*M)],slot[1:(N*M)],hub[1:(N*M)],
                    Ncompet[1:(N*M)],Nthreats[1:(N*M)]))
matexpl = t(X[,1:(N*M)])
matN = Ncompet[seq(1,N*M,N)]
matNT = Nthreats[seq(1,N*M,N)]

mydata=data.frame(cbind(Y,matexpl))
colnames(mydata)=c("Y","K","pop","income","dist","dist2","City2","slot","hub","Ncompet","Nthreats")

#### Nb var = col(matexpl) + 2 for the correlation of the term
nvar=ncol(matexpl)+2

####Initial value
myprobit <- glm(Y~ pop+dist+dist2+income+slot+City2+hub+Ncompet+Nthreats, family = binomial(link = "probit"), 
                data = mydata)

## model summary
summary(myprobit)  
stargazer(myprobit) # latex table

coefinit=c(coef(myprobit),1, -0.8)
# Some coefs from probit seem unintuitive, so will replace with values closer to
# Gayle & Wu estimates
coefinit[2] = 4.5 # expect population to have positive effect
coefinit[5] = 6.5 # expect income to have positive effect
coefinit[9] = -1.5 # expect Ncompet to have negative effect

#### Procedure to compute N at the equilibrium
Calc_N<- function(s,matexpl,coef,uim,u0m){
  Calc_N_m<- function(m,s,matexpl,coef,uim,u0m){
    
    #Compute profits : 
    ind1=(m-1)*N+1
    rho=(exp(coef[nvar])-exp(-coef[nvar]))/(exp(coef[nvar])+exp(-coef[nvar])) #Reparametrization to get it between -1 and 1
    profits = matexpl[ind1:(ind1+N-1),]%*%coef[1:(nvar-2)]+rho*rep(u0m[m,s],N)+sqrt(1-rho^2)*uim[ind1:(ind1+N-1),s]
    
    #Threshold 
    delta=coef[nvar-1]
    threshold=delta*log(seq(1,N,1))
    
    ### Neq
    above<-function(i){sum(profits>threshold[i])}
    pentry=sapply(seq(1,N,1),above)-seq(1,N,1) # Try to add predicted entry for each firm???
    nfirm=sum((sapply(seq(1,N,1),above)-seq(1,N,1))>0)
    
    return(c(1*(nfirm==matN[m])))
    # return(c(1*ifelse(pentry=matN[m], nfirm, 0)))
  } 
  return(c(sapply(seq(1,M,1), Calc_N_m,s=s,matexpl=matexpl,coef=coef,uim=uim,u0m=u0m)))
}

#### Procedure to compute the log-likelihood

loglik<-function(theta){
  # Compute average number of entrants
  cl<-makeCluster(Nclust) # Function NPred will be called 100 times parallelly
  clusterExport(cl=cl, varlist=c("matexpl", "uim", "u0m", "matN","M","N","nvar"))
  Matrice_N <- rowMeans(data.frame(matrix(unlist(parLapply(cl,1:S,Calc_N,matexpl=matexpl,coef=theta,uim=uim,u0m=u0m)),
                                          nrow=M, byrow=F))) # Want to max the number of 1s in the matrix
  stopCluster(cl)
  Matrice_N[Matrice_N==0]=1E-100
  loglik=-sum(log(Matrice_N)) # More 1s -> lower value (so want to minimize log likelihood in this case)
  return(loglik)
}

start_time <- Sys.time()
loglik(coefinit)
end_time <- Sys.time()
end_time - start_time
# S=100 -> 8 secs (-> 100 iterations = 15 mins)

start_time <- Sys.time()
ml.res = nloptr(coefinit,loglik,opts=list("algorithm"="NLOPT_LN_COBYLA",print_level=5,"xtol_rel"=1.0e-4,maxeval=10000))
# optim(coefinit, loglik, control=list(trace=T,maxit=100, REPORT=1),method="BFGS") # trace=T, report=100
end_time <- Sys.time()
end_time - start_time 
