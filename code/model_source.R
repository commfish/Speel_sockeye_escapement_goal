#State Space Model Function

#analysis----
#State Space Model Function
mod=function() {
  for (y in (A+a.min):(Y+A-1)) {
    log.R[y] ~ dnorm(log.R.mean2[y],tau.R)
    R[y] <- exp(log.R[y])
    log.R.mean1[y] <- log(S[y-a.max]) + lnalpha - beta * S[y-a.max] 
    log.resid[y] <- log(R[y])-log.R.mean1[y]
  }
  log.R.mean2[A+a.min] <- log.R.mean1[A+a.min] + phi * log.resid.0 
  for (y in (A+a.min+1):(Y+A-1)) {
    log.R.mean2[y] <- log.R.mean1[y] + phi * log.resid[y-1]
  }
  
  lnalpha ~ dnorm(0,1.0E-6)%_%I(0,)
  beta ~ dnorm(0,1.0E-6)%_%I(0,)              
  phi ~ dnorm(0,1.0E-6)%_%I(-1,1)
  mean.log.R0 ~ dnorm(0,1.0E-6)
  tau.R0 ~ dgamma(0.1,0.1)  
  log.resid.0 ~ dnorm(0,tau.red)
  tau.R ~ dgamma(0.001,0.001) 
  
  sigma.R <- 1 / sqrt(tau.R)
  alpha <- exp(lnalpha)
  sigma.R0 <- 1 / sqrt(tau.R0)#uninformative; informative (not shown) prior had large effect on parameter itself 
  #and initial R values, but not on key model quantities
  tau.red <- tau.R * (1-phi*phi)
  lnalpha.c <- lnalpha + (sigma.R * sigma.R / 2 / (1-phi*phi) ) 
  
  #THE FIRST SEVERAL COHORTS ORIGINATE FROM UNMONITORED SPAWNING EVENTS
  #DRAW THESE RETURNS FROM A COMMON LOGNORMAL DISTRIBUTION 
  R.0<-exp(mean.log.R0)
  for (y in 1:a.max) { 
    log.R[y] ~ dnorm(mean.log.R0,tau.R0) 
    R[y] <- exp(log.R[y]) 
  }
  #REFERENCE POINTS (WITH CORRECTION FOR LOGNORMAL SKEWNESS)
  S.max <- 1 / beta 
  alpha.c <- min(exp(lnalpha.c),1.0E4)
  S.eq.c <- lnalpha.c * S.max #Eq.21
  U.msy.c <- lnalpha.c * (0.5-0.07*lnalpha.c)
  S.msy.c <- S.eq.c *(0.5-0.07*lnalpha.c)  
  
  positive.lna.c <- step(lnalpha.c)
  lnalpha.c.nonneg <- lnalpha.c * positive.lna.c
  S.eq.c2 <- lnalpha.c.nonneg * S.max 
  peterman.approx.c <- (0.5 - 0.65*pow(lnalpha.c.nonneg,1.27) / (8.7 +pow(lnalpha.c.nonneg,1.27)))
  U.msy.c2 <- lnalpha.c.nonneg * peterman.approx.c 
  S.msy.c2 <- U.msy.c2 / beta  
  U.max.c2 <- 1 - 1 / exp(lnalpha.c.nonneg) 
  
  #GENERATE Y+A-1 = 42 MATURITY SCHEDULES, ONE PER BROOD YEAR USING THE DIRICHLET DISTRIB. (Eq.4-6)
  # "pi" (central tendency of "p"), and "D.scale" (dispersion of "p")
  D.scale ~ dunif(0,1)#uninformative
  D.sum <- 1 / (D.scale * D.scale)
  pi.2p ~ dbeta(1,1)#uninformative
  pi.1 ~ dbeta(1,1)#uninformative; Eq.6
  pi[1] <- pi.1
  pi[2] <- pi.2p * (1 - pi[1])
  pi[3] <- 1 - pi[1] - pi[2]
  
  for (a in 1:A) {
    gamma[a] <- D.sum * pi[a]
    for (y in 1:(Y+A-1)) {                                                    
      g[y,a] ~ dgamma(gamma[a],0.01)
      p[y,a] <- g[y,a]/sum(g[y,])
    }
  }
  
  #CALCULATE THE NUMBERS AT AGE MATRIX (Number returning to spawn at age in year y); Eq.3
  #Product of the total return from brood year y-a and the prop. mature from cohort y-a returning at age a; 
  for(a in 1:A){
    for(y in a:(Y+(a-1))){
      N.ya[y-(a-1),(A+1-a)]<-p[y,(A+1-a)]*R[y]
    }
  }
  
  #MULTINOMIAL SCALE SAMPLING ON TOTAL ANNUAL RETURN N; Eq.13
  for (y in 1:Y) {
    N[y] <- sum(N.ya[y,1:A])
    for (a in 1:A) {
      q[y,a] <- N.ya[y,a] / N[y]
    }
  } 
  for (t in 1:Y){  
    x[t,1:A] ~ dmulti(q[t,],n.a[t])
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