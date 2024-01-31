
#------------------------------ SML Procedure -------------------------------#
# Following Berry (1992)

#-------------------------- Load Libraries & Data ---------------------------#

#### Libraries
library(MASS)
library(data.table)
library(nloptr)
library(parallel)
library(dplyr)
options(digits = 5)

#### Data
rm(list=ls())
if (Sys.info()["user"] == "alecr") {
  projdir <- "C:/Users/alecr/OneDrive/Documents/TSE_2023-24/Empirical IO/Project/"
}

sml_data <- readRDS(paste0(projdir, "sml_data.rds"))

### Theory
# Firm profits: \pi_ik(N) = X_i*\Beta + Z_ik*\Gamma - \Delta*Log(N) + \sigma*u_ik + \rho*u_i0
# X_i: market characteristics (distance, income, pop, slot)
# Z_ik: firm-specific characteristics (hub, City2)
# N: number of competing firms
# u_ik: firm-market error term
# u_i0: market-specific error term observed by all firms
# 
# Assumption: \sigma = sqrt(1-\rho^2) to normalize total variance to 1
# 
# Goal: construct log-likelihood function

#-------------------------- Set Parameters for SML ---------------------------#

# Number of markets
M = n_distinct(sml_data$market)
# Number of airlines
N = n_distinct(sml_data$airline)
#Number of Simulations (starting small)
S=1000

#Number of cluster to make it in parallel
Nclust = 8

#### Explanatory variables

# Need to sort sml_data by market first
sml_data <- sml_data %>% arrange(market) 

market = sml_data$market
pop = sml_data$pop
City1 = sml_data$City1
City2 = sml_data$City2
dist = sml_data$dist
dist2 = sml_data$dist2
income = sml_data$income
slot = sml_data$slot
hub = sml_data$hub
Ncompet = sml_data$nentrants
Nthreats = sml_data$nentrythreats 

Y = as.matrix(sml_data$I)
X = as.matrix(rbind(rep(1,N*M),pop[1:(N*M)],income[1:(N*M)],dist[1:(N*M)],
                    dist2[1:(N*M)], City2[1:(N*M)],slot[1:(N*M)],hub[1:(N*M)]))
matexpl = t(X[,1:(N*M)])

matthreats=Nthreats[seq(1,N*M,N)]
matN=Ncompet[seq(1,N*M,N)]

mydata=data.frame(cbind(Y,matexpl,lmatthreats,lmatN, matN))
colnames(mydata)=c("Y","const","pop","income","dist","dist2", "City2","slot","hub")

#### Nb var = col(matexpl) + 3 for the threat and entry coefs and rho correlation of the terms
nvar=ncol(matexpl)+3


## Initial values
myprobit <- glm(Y ~ pop+income+dist+dist2+City2+slot+hub, family = binomial(link = "probit"), 
                 data = mydata)

## model summary
summary(myprobit)
stargazer(myprobit) # latex table

# For SML, use results from trial run with S=100
coefinit <- c(-0.067, 0.2, 0.015, 0.73, -0.326, 0.13, 0.24, 1.6, 0.614, 0.27, 0.68)

##### Simulation of error terms for SML N*M*S  
set.seed(31497)
uim = matrix(rnorm(N*M*S, mean = 0, sd = 1), N*M, S)
u0m = matrix(rnorm(M*S, mean = 0, sd = 1), M, S)

#### Procedure to calculate predicted N in each market
Calc_N<- function(s,matexpl,coef,uim,u0m){
  Calc_N_m<- function(m,s,matexpl,coef,uim,u0m){
    
    #Compute profits : 
    ind1=(m-1)*N+1
    rho=(exp(coef[nvar])-exp(-coef[nvar]))/(exp(coef[nvar])+exp(-coef[nvar])) #Reparametrization to get it between -1 and 1
    profits = matexpl[ind1:(ind1+N-1),]%*%coef[1:(nvar-3)]+rho*rep(u0m[m,s],N)+sqrt(1-rho^2)*uim[ind1:(ind1+N-1),s]
    
    #Threshold 
    delta_a=coef[nvar-1] # Delta for actual entrants
    entrants <- seq(1,N,1) # Sequence from 1 to N number of possible entrants
    
    delta_e=coef[nvar-2] # Delta for entry threats
    Sum_City2 = sum(matexpl[ind1:(ind1+N-1),6]) # Sum of City 2 for market m
    threats <- seq(Sum_City2-1, Sum_City2-N,-1) 
    # Sequence for number of threats, decreasing as no. of entrants increases
    threats[threats < 0] <- 0 # Set zero/negatives to 1 since log(1) = 0
    
    threshold=delta_a*log(entrants) + delta_e*log(threats+1) 
    # Add 1 to threats b/c log(1) = 0
    # Want negative effect of 1 threat but 0 effect for 1 entrant since that entrant = monopolist
    
    # Estimate number of firms for each entrant/threat combo
    above<-function(i){sum(profits>threshold[i])}
    nfirm=sum((sapply(seq(1,N,1),above)-seq(1,N,1))>=0)
    
    return(c(1*(nfirm==matN[m]))) # Check whether prediction matches observed value
    # return(c(nfirm))
  } 
  return(c(sapply(seq(1,M,1), Calc_N_m,s=s,matexpl=matexpl,coef=coef,uim=uim,u0m=u0m)))
}

#### Procedure to compute the log-likelihood

loglik<-function(theta){
  # Compute average number of entrants
  cl<-makeCluster(Nclust) # Function NPred will be called 100 times parallelly
  clusterExport(cl=cl, varlist=c("matexpl", "uim", "u0m","matN", "M","N","nvar"))
  Matrice_N <- rowMeans(data.frame(matrix(unlist(parLapply(cl,1:S,Calc_N,matexpl=matexpl,coef=theta,uim=uim,u0m=u0m)),
                                          nrow=M, byrow=F))) # Want to max the number of 1s in the matrix
  stopCluster(cl)
  # Compare predicted N to real N
  diff = abs(Matrice_N - matN) + 1 # Add 1 so that log always increasing in diff
  Matrice_N[Matrice_N==0]=1E-100
  loglik=-sum(log(Matrice_N)) # More 1s -> lower value (so want to minimize log likelihood in this case)
  return(loglik)
}

start_time <- Sys.time()
loglik(coefinit)
end_time <- Sys.time()
end_time - start_time
# S=100 -> 6.5 secs (-> 100 iterations =  11 mins) -> 25-35 to finish
# S=500 -> 40 seconds (-> 100 iterations = 66 mins)
# S=1000 -> 53 secs (-> 100 iterations = 140 mins = 2.5 hrs) --> 11.5 hours!! to finish (569 iterations)

start_time <- Sys.time()
ml.res = nloptr(coefinit,loglik,opts=list("algorithm"="NLOPT_LN_COBYLA",print_level=1,"xtol_rel"=1.0e-4,maxeval=1000))
# optim(coefinit, loglik, control=list(trace=T,maxit=100, REPORT=1),method="BFGS") # trace=T, report=100
end_time <- Sys.time()
end_time - start_time 

SML_results <- ml.res$solution
names <- c("Constant", "Population", "Income", "Distance", "Distance^2", "City2", 
           "Slot", "Hub", "No. Entry Threats", "No. Entrants", "Rho")
names(SML_results) <- names
SML_results["No. Entrants"] <- SML_results["No. Entrants"]*-1 # Multiply by negative one b/c this would be sign in profit eq'n
SML_results["No. Entry Threats"] <- SML_results["No. Entry Threats"]*-1 # Multiply by negative one b/c this would be sign in profit eq'n

saveRDS(ml.res, paste0(projdir, "SML_results.RData"))

