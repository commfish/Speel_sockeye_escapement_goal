# this code gets sourced from this file, 
# and creates the "inits" object, which is the used in the main script

# inital values 
B.scale = 0.3759
D.scale = 0.22
beta = 1.743E-4
lnalpha = 2.039
log.resid.0 = 0.1399
mean.log.R0 = 9.458
mu = 0.4819
phi = -0.04894
tau.R = 9.986
tau.R0 = 0.5552
pi = c(0.3232,NA,NA)
pi.2p = 0.9674

g.inits	=	structure(.Data	=	c(
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1,	
    1,1,1),	
    .Dim	=	c(31,3))
  

mu.HB = c(
  0.5727,0.7415,0.3437,0.4287,0.499,
  0.4154,0.3911,0.2358,0.6217,0.5142,
  0.5369,0.228,0.5579,0.4417,0.3731,
  0.1244,0.1509,0.5799,0.5217,0.567,
  0.6105,0.5038,0.5227,0.6594,0.5728,
  0.789,0.5331,0.5872,0.4138)

log.R = c(
  6.952,10.11,10.75,9.788,9.068,
  10.01,8.771,9.773,9.618,10.01,
  9.923,10.13,9.084,9.089,9.993,
  9.018,9.205,9.579,9.611,9.778,
  9.498,9.915,9.666,9.654,9.349,
  9.182,9.006,8.977,9.418,9.035,
  9.378)

inits1=list(
  B.scale=B.scale.init,
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.RO=mean.log.RO.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.RO=tau.RO.init)# pass the initials to JAGS

inits=list(inits1,inits1,inits1)