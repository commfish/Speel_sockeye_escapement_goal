#State Space Model Function

#analysis----
#State Space Model Function
mod=function() {
  for (c in A+a.min:C) {
    log.R[c] ~ dnorm(log.R.mean2[c],tau.R)
    R[c] <- exp(log.R[c])
    log.R.mean1[c] <- log(S[c-a.max]) + lnalpha - beta * S[c-a.max] #eq.1
    log.resid[c] <- log(R[c]) - log.R.mean1[c]
  }
  log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.O #eq.2
  for (c in A+a.min+1:C) {
    log.R.mean2[c] <- log.R.mean1[c] + phi * log.resid[c-1]
  }
  
  lnalpha ~ dnorm(0,1.0E-6)%_%I(0,)
  beta ~ dnorm(0,1.0E-6)%_%I(0,)              
  phi ~ dnorm(0,1.0E-6)%_%I(-1,1)
  mean.log.RO ~ dnorm(0,1.0E-6)
  tau.RO ~ dgamma(0.1,0.1)  
  log.resid.O ~ dnorm(0,tau.red)
  tau.R ~ dgamma(0.001,0.001) 
  
  sigma.R <- 1 / sqrt(tau.R)
  alpha <- exp(lnalpha)
  sigma.RO <- 1 / sqrt(tau.RO)
  tau.red <- tau.R * (1-phi*phi)
  lnalpha.c <- lnalpha + (sigma.R * sigma.R / 2 / (1-phi*phi) ) #eq.22
  S.max <- 1 / beta #eq.20
  
  
# THE FIRST SEVERAL COHORTS ORIGINATE FROM UNMONITORED SPAWNING EVENTS
# DRAW THESE RETURNS FROM A COMMON LOGNORMAL DISTRIBUTION #eq. 17-21
  R.O<-exp(mean.log.RO)
  for (c in 1:a.max) { 
    log.R[c] ~ dnorm(mean.log.RO,tau.RO) 
    R[c] <- exp(log.R[c]) 
  }
# CORRECTION FOR LOGNORMAL SKEWNESS
  alpha.c <- min(exp(lnalpha.c),1.0E4)
  positive.lna.c <- step(lnalpha.c)
  lnalpha.c.nonneg <- lnalpha.c * positive.lna.c
  S.eq.c <- lnalpha.c.nonneg * S.max
  peterman.approx.c <- (0.5 - 0.65*pow(lnalpha.c.nonneg,1.27) / (8.7 +pow(lnalpha.c.nonneg,1.27)))
  U.msy.c <- lnalpha.c.nonneg * peterman.approx.c #peterman approximation
  S.msy.c <- U.msy.c / beta
  U.max.c <- 1 - 1 / exp(lnalpha.c.nonneg)
  
# GENERATE Y+A-1 = 33 MATURITY SCHEDULES, ONE PER BROOD YEAR (eq.4-6)
  D.scale ~ dunif(0,1)#uninformative
  D.sum <- 1 / (D.scale * D.scale)
  pi.2p ~ dbeta(1,1)#uninformative
  pi.1 ~ dbeta(1,1)#uninformative; Eq.6
  pi[1] <- pi.1
  pi[2] <- pi.2p * (1 - pi[1])
  pi[3] <- 1 - pi[1] - pi[2]
  
  for (a in 1:A) {
    gamma[a] <- D * pi[a]#eq.6
    for (c in 1:C) {                                                    
      g[c,a] ~ dgamma(gamma[a],0.1)#eq.5
      p[c,a] <- g[c,a]/sum(g[c,])#eq.4
    }
  }
  
# CALCULATE THE NUMBERS AT AGE MATRIX
  for(a in 1:A){
    for(y in a:(Y+(a-1))){
      N.ya[y-(a-1),(A+1-a)]<-R[y]*p[y,(A+1-a)] #eq.3
    }
  }
  
# MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N #eq.13
  for (y in 1:Y) {
    N[y] <- sum(N.ya[y,1:A])
    for (a in 1:A) {
      q[y,a] <- N.ya[y,a] / N[y]
    }
    n[y] <- sum(x[y,1:A])
    x[y,1:A] ~ dmulti(q[y,],n[y])
  }
  
  
# HARVESTS BELOW (No harvest above)#eq.13
  B.scale ~ dunif(0,1)
  mu ~ dbeta(0.1,0.1)
  B.sum<-1/B.scale/B.scale
  B[1]<-mu*B.sum
  B[2]<-B.sum-B[1]
  for (y in 1:Y) {
    mu.HB[y] ~ dbeta(B[1],B[2])
    H.B[y] <- mu.HB[y] * N[y]
    log.HB[y] <- log(H.B[y])
    tau.log.hb[y] <- 1 / log(cv.hb[y]*cv.hb[y]+1)  
    h.b[y] ~ dlnorm(log.HB[y],tau.log.hb[y])     
    W[y]<-max(N[y]-H.B[y],1)#eq. 8
    log.W[y] <- log(W[y])
    tau.log.w[y] <- 1 / log(cv.w[y]*cv.w[y]+1)  
    w[y]~dlnorm(log.W[y],tau.log.w[y])
    S[y] <- W[y]#eq.9
  }
}