#Taku sockeye state space model
#authors: Sara E Miller & Gottfried Pestal
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: July 2019
#must download program JAGS for this script to work

# warning: some of these packages mask commands, so need to specify the package when calling the fn
# runjags::extract vs. tidyr::extract
# coda::traceplot vs. R2jags::traceplot
library(coda)
library(tidyverse)
library(R2jags)
library(rjags)
library(runjags)
library(R2OpenBUGS)
library(mcmcplots)
library(gsl)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(gdata)
library(ggplot2)
library(mcmcplots)
library(scales)
library(cowplot)
install.packages("devtools")
devtools::install_github("ben-williams/FNGr")
library(FNGr)
library(extrafont)
# STEP 1: CHOOSE SETTINGS----

# if test runs then do sensitivity tests with explore, and final run with full
# "explore" version takes ~10min with the current settings.
out.label <-  "rjags_base_case" 
package.use <- "rjags"  #"rjags"  or "R2jags"
jags.settings <- "test"  # "test" or "explore" or full" 
sensitivity.analysis <- 0 #0 1 is yes and 0 is no

# source the model file (this reads in a function called "mod")
# then write the model to a text file to be called by JAGS if using rjags version
# if used R2Jags, can just use the "mod" object directly
# if you get a dmulti error, then the age comps are not whole numbers
source("code/model_source.R") 
print(mod)
model_file_loc=paste("code/","Speel_sockeye.txt", sep="") # where to write the model file
write.model(mod, model_file_loc)

# load custom functions
source('code/functions.R')

# create output folder for model results
out.path <- paste0("output/", out.label)
if(!exists(out.path)){dir.create(out.path)}

if(jags.settings == "test"){
  n.adapt.use <- 100 ; n.iter.use <- 500;  n.burnin.use <- 100;   thin.use = 10
  by.use <- 10 # this is just for the progress bar
}

if(jags.settings == "explore"){
  n.adapt.use <- 10000 ; n.iter.use <- 10000; n.burnin.use <- 30000 ;   thin.use = 10
  by.use <- 100 # this is just for the progress bar
}


if(jags.settings == "full"){
  n.adapt.use <- 10000  ; n.iter.use <- 1000000    #1,000,000 per chain; 3 chains; thin by 1000
  n.burnin.use <- 10000  # consider increasing this?
  thin.use = 1000; by.use <- 1000 # this is just for the progress bar 
}

# STEP 2: READ IN DATA, MODEL, AND INITIAL VALUES----
# generates the object "dat"
source("code/model_data.R")

# generate initial values
source("code/model_inits.R")

# STEP 3: RUN THE MODEL AND PROCESS THE OUTPUT----
# This step does the actual MCMC sampling. All subsequent steps

# start the timer
start.jags <- proc.time()
#rjags
if(package.use == "rjags" & sensitivity.analysis == 0){
  parameters <- c('alpha','beta', 'lnalpha','lnalpha.c','phi',
                  'sigma.R','log.resid.0', 'mean.log.R0','log.resid',
                  'S','R','N','pi','h.b','N.ya','mu.HB',
                  'p','q', 'S.max','D.sum','sigma.R0',
                  'S.eq.c', 'U.msy.c', 'S.msy.c', 'B.sum')
  
  jmod <- rjags::jags.model(file='code/Speel_sockeye.txt', data=dat, n.chains=3, inits=inits, n.adapt=n.adapt.use) 
  stats::update(jmod, n.iter=n.iter.use, by=by.use, progress.bar='text', DIC=T, n.burnin=n.burnin.use) # this modifies the original object, function returns NULL
  post <- rjags::coda.samples(jmod, parameters, n.iter=n.iter.use, thin=thin.use, n.burnin=n.burnin.use)

end.jags <- proc.time()   # store time for MCMC
# post = original output from rjags::coda.samples
# post.arr = reformatted version of post
post.arr <- as.array(post) # convert to an accessible obj

# run the script that generates all the outputs 
#source("state_space_model/code/2_GENERATE_OUTPUTS.R")
}

