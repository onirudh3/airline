
# Libraries and data ------------------------------------------------------

library(MASS)
library(data.table)
library(nloptr)
library(parallel)
library(dplyr)
options(digits = 5)

# Data
matdata <- read.csv("entry_data.csv", header = T, sep = ",", dec = ".")

# Number of markets
M = 1176

# Number of players
N = 12

# Number of Simulations
S = 1000

# Number of cluster to make it in parallel
Nclust = 40

## Simulation of error terms for SML N * M * S
set.seed(8769)
uim = matrix(rnorm(N * M * S, mean = 0, sd = 1), N * M, S)
u0m = matrix(rnorm(M * S, mean = 0, sd = 1), M, S) # %*% matrix(rep(1, N), N, 1)


# Explanatory variables
# Of course, you adapt your list here !!!!!
pop = sqrt(as.numeric(matdata$popdep) * as.numeric(matdata$poparr) / 1E6 / 1E6)
City2 = matdata$City2
dist = matdata$direct_distance / 1000
dist2 = dist ^ 2
wealth = (matdata$gdppercapdep + matdata$gdppercaparr) / 2000
Ncompet = matdata$nentrythreats
Y = as.matrix(matdata$incumbent)
X = as.matrix(rbind(rep(1, N * M), pop[1:(N * M)], wealth[1:(N * M)], dist[1:(N * M)],
                    dist2[1:(N * M)], City2[1:(N * M)]))
matexpl = t(X[, 1:(N * M)])
matN = Ncompet[seq(1, N * M, N)]

mydata = data.frame(cbind(Y, matexpl))
colnames(mydata) = c("Y", "K", "pop", "wealth", "dist", "dist2", "City2")

# Nb var = col(matexpl) + 2 for the correlation of the term
nvar = ncol(matexpl) + 2

# Initial value
myprobit <- glm(Y ~ pop + dist + dist2 + City2,
                family = binomial(link = "probit"),
                data = mydata)

## Model summary
summary(myprobit)

coefinit = c(coef(myprobit), 1, 0)


# Procedure to compute N at the equilibrium
Calc_N <- function(s, matexpl, coef, uim, u0m){
  Calc_N_m <- function(m, s, matexpl, coef, uim, u0m){

    # Compute profits :
    ind1 = (m - 1) * N + 1
    rho = (exp(coef[nvar]) - exp(-coef[nvar])) / (exp(coef[nvar]) + exp(-coef[nvar])) #Reparameterization to get it between -1 and 1
    profits = matexpl[ind1:(ind1 + N - 1), ] %*% coef[1:(nvar - 2)] + rho * rep(u0m[m, s], N) + sqrt(1 - rho ^ 2) * uim[ind1:(ind1 + N - 1), s]

    # Threshold
    delta = coef[nvar - 1]
    threshold = delta * log(seq(1, N, 1))

    ### Neq
    above <- function(i){sum(profits > threshold[i])}
    nfirm = sum((sapply(seq(1, N, 1), above) - seq(1, N, 1)) > 0)

    return(c(1 * (nfirm == matN[m])))
  }

  return(c(sapply(seq(1, M, 1), Calc_N_m, s = s, matexpl = matexpl, coef = coef,
                  uim = uim, u0m = u0m)))
}


# Procedure to compute the log-likelihood

loglik <- function(theta){

  # Compute average number of entrants
  cl <- makeCluster(Nclust) # Function NPred will be called 100 times parallely
  clusterExport(cl = cl, varlist = c("matexpl", "uim", "u0m", "matN", "M", "N", "nvar"))
  Matrice_N <- rowMeans(data.frame(matrix(unlist(parLapply(cl, 1:S, Calc_N, matexpl = matexpl,
                                                           coef = theta, uim = uim, u0m = u0m)),
                                          nrow = M, byrow = F)))
  stopCluster(cl)
  Matrice_N[Matrice_N == 0] = 1E - 100
  loglik = -sum(log(Matrice_N))
  return(loglik)
}
