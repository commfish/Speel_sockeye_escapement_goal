# this code gets sourced from this file, 
# and creates the "inits" object, which is the used in the main script

# inital values 
B.scale.init = 0.3759
D.scale.init = 0.19
beta.init = 1.055E-5
lnalpha.init = 1.6
log.resid.0.init = 0.15
mean.log.R0.init = 11.9
phi.init = 0.5
pi.1.init = 0.06
pi.2p.init = 0.4
tau.R.init = 2.5
tau.R0.init = 76
log.R.inits = c(
  11.5,11.74,11.74,11.81,12.01,
  11.93,11.89,12.0,12.13,12.38,
  12.15,11.33,11.68,12.03,12.7,
  11.86,12.12,12.35,12.07,12.14,
  11.89,11.31,12.46,12.96,12.39,
  10.58,11.86,11.86,11.28,11.84,
  10.75,10.84,10.57,11.87,12.5,
  11.14,11.44,12.26,11.95,12,12)
g.inits = structure(.Data = c(
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
  .Dim = c(41,3))


inits1=list(
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.RO=mean.log.RO.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.R0=tau.R0.init)# pass the initials to JAGS

inits=list(inits1,inits1,inits1)