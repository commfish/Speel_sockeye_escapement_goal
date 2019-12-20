# this code gets sourced from this file, 
# and creates the "inits" object, which is the used in the main script

# inital values 
B.scale.init = 0.3759
D.scale.init = 0.22
beta.init = 1.743E-4
lnalpha.init = 2.039
log.resid.0.init = 0.1399
mean.log.R0.init = 9.458
phi.init = -0.04894
mu.init = 0.4819
pi.1.init = 0.06
pi.2p.init = 0.4
tau.R.init = 9.986
tau.R0.init = 0.5552
log.R.inits = c(
  6.952,10.11,10.75,9.788,9.068,
  10.01,8.771,9.773,9.618,10.01,
  9.923,10.13,9.084,9.089,9.993,
  9.018,9.205,9.579,9.611,9.778,
  9.498,9.915,9.666,9.654,9.349,
  9.182,9.006,8.977,9.418,9.035,
  9.378,.498,9.915,9.666,9.654,9.349,
  9.182,9.006,8.977,9.418)
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
  1,1,1),
  .Dim = c(37,3))


inits1=list(
  mu=mu.init,
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.R0=mean.log.R0.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.R0=tau.R0.init)# pass the initials to JAGS

# inital values 
B.scale.init = 0.5
D.scale.init = 0.20
beta.init = 2E-4
lnalpha.init = 3.039
log.resid.0.init = 0.15
mean.log.R0.init = 10
phi.init = -0.06
mu.init = 0.8
pi.1.init = 0.06
pi.2p.init = 0.4
tau.R.init = 10
tau.R0.init = 0.60
log.R.inits = c(
  7,10.11,10.75,9.788,9.068,
  10.01,8.771,9.773,9.618,10.,
  9.923,10.13,9.084,9.089,9.993,
  9.018,9.205,9.579,9.611,9.778,
  9.498,9.915,9.666,9.654,9,
  9.182,9.006,8.977,9.418,9.035,
  9.378,.498,9.915,10,9.654,9.349,
  9,9.006,8.977,9.418)
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
  1,1,1),
  .Dim = c(37,3))


inits2=list(
  mu=mu.init,
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.R0=mean.log.R0.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.R0=tau.R0.init)# pass the initials to JAGS

# inital values 
B.scale.init = 0.6
D.scale.init = 0.23
beta.init = 3E-4
lnalpha.init = 4.039
log.resid.0.init = 0.18
mean.log.R0.init = 12
phi.init = 0.50
mu.init = 0.9
pi.1.init = 0.07
pi.2p.init = 0.5
tau.R.init = 11
tau.R0.init = 0.70
log.R.inits = c(
  7,10.11,10.75,9.788,9.068,
  10.01,8.771,9.773,10,10.,
  9.923,10.13,9.084,9.089,10,
  10,9.205,9.579,10,10,
  9.498,9.915,9.666,9.654,9,
  9.182,9.006,8.977,9.418,9.035,
  9,.498,9.915,10,9.654,9,
  9,9.006,8.977,9) #1977 to 2015
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
  1,1,1),
  .Dim = c(37,3)) #1983 to 2019


inits3=list(
  mu=mu.init,
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.R0=mean.log.R0.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.R0=tau.R0.init)# pass the initials to JAGS

inits=list(inits1,inits2,inits3)

# inital values 
B.scale.init = 0.5
D.scale.init = 0.20
beta.init = 2E-4
lnalpha.init = 3.039
log.resid.0.init = 0.15
mean.log.R0.init = 10
phi.init = -0.06
mu.init = 0.8
pi.1.init = 0.06
pi.2p.init = 0.4
tau.R.init = 10
tau.R0.init = 0.60
log.R.inits = c(
  7,10.11,10.75,9.788,9.068,
  10.01,8.771,9.773,9.618,10.,
  9.923,10.13,9.084,9.089,9.993,
  9.018,9.205,9.579,9.611,9.778,
  9.498,9.915,9.666,9.654,9,
  9.182,9.006,8.977,9.035,
  9.378,.498,9.915,10,9.654,9.349) #1980-2015
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
  1,1,1),
  .Dim = c(34,3)) #1986 to 2019


inits4=list(
  mu=mu.init,
  D.scale=D.scale.init,
  beta=beta.init,
  lnalpha=lnalpha.init,
  log.resid.0=log.resid.0.init,
  mean.log.R0=mean.log.R0.init,
  phi=phi.init,
  pi.1=pi.1.init,
  pi.2p=pi.2p.init,
  tau.R=tau.R.init,
  tau.R0=tau.R0.init)# pass the initials to JAGS

inits=list(inits4,inits4,inits4)