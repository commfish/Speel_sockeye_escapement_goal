# THIS SCRIPT IS RUN AFTER 1_RUN_MODELS.R and 2_GENERATE_OUTPUTS.r
# i and z act as ways to change range of escapement based on stock size

# input values below based on stats output
LowerB <- 4000  #lower bound of recommended escapement goal range
UpperB <- 9000 #upper bound of recommended escapement goal range
SMSY <- 6276  #Lambert W from lambert file
UMSY <- 0.55  #median from staquants file
SMAX <- 11557  #median from staquants file
SEQ <- 15444 #median from staquants file
lnalpha.c <-  1.3328 #median from staquants file
beta <- 8.65258E-05  #median from staquants file

# load----
library(tidyverse)
library(cowplot)
library(ggplot2)
library(FNGr)
library(gsl)
library(FField)
library(scales)
library(dplyr)
library(extrafont)
library("devtools")
library(zoo)
# font_import()

windowsFonts(Times=windowsFont("TT Arial"))
source('code/functions.r')

if(!dir.exists(file.path("output", "rjags_base_case", "processed"))){dir.create(file.path("output", "rjags_base_case", "processed"))}

# data----
# loadfonts(device="win") #only need to do this once; takes awhile to run!
coda <- read.csv("output/rjags_base_case/coda.csv") 
coda  %>%
  mutate(S.eq.c = lnalpha.c/beta, 
                S.msy.c = (1-lambert_W0(exp(1-lnalpha.c)))/beta, #Lambert W
					      R.msy.c = S.msy.c*exp(lnalpha.c-beta*S.msy.c), 
					      MSY.c = R.msy.c-S.msy.c, 
					      Rmax = exp(lnalpha)*(1/beta)*exp(-1)) -> coda

# analysis----
# create function for probability profiles and figures
theme_sleek <- function(base_size = 12, base_family = "Arial") {
  theme_light(base_size = 12, base_family = "Arial") +theme_bw()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)
    )
}
theme_set(theme_sleek())

profile(i=10, z=50, xa.start=0, xa.end=8000,lnalpha.c, beta) #can change i,z, xa.start, xa.end

# read in data----
Speel_sockeye <- read.csv("data/Speel_sockeye.csv") 
p_q_Nya<- read.csv("output/rjags_base_case/p_q_Nya.csv") 
CI <- read.csv("output/rjags_base_case/processed/CI.csv") 
QM <- read.csv("output/rjags_base_case/processed/QM.csv")
coda <- read.csv("output/rjags_base_case/coda.csv") 
parameters <- read.csv("output/rjags_base_case/parameters.csv") 

parameters %>%
  mutate (year = as.numeric(year),
  S97.5. = as.numeric(S97.5.),
  S50. = as.numeric(S50.)) -> parameters
data <- merge(parameters, Speel_sockeye, by=c("year"), all=TRUE)
xaxis = tickr(data, year, 4)

# POINT ESTIMATES----
out.file <- paste0("output/rjags_base_case/processed/point_est.png")

# escapement
maxY<-max(parameters$S97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=(S50.))) +
  geom_line(size=0.75)+ geom_point (size=2)+ylab("Escapement (S)") + xlab("") +
  geom_ribbon(aes(ymin=(parameters$S2.5.), ymax=(parameters$S97.5.)), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 30000, 5000), limits = c(0, 30000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") +
  geom_line(aes(y=SMSY), colour="grey70", size=1, linetype=2)+
  annotate("text",x = 1977, y= 30000, label="(a)", family="Arial" ,size=6) -> plot1

# terminal run abundance   
maxY<-max(parameters$N97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=N50.))+geom_line(size=0.75) + 
  geom_point (size=2) + xlab("Year") +
  ylab("Terminal Run Abundance (N)") +annotate("text",x = 1977, y = 50000, label="(c)", family="Arial" ,size=6) +
  geom_ribbon(aes(ymin=N2.5., ymax=N97.5.), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 50000, 10000), limits = c(0, 50000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot2

# harvest rates
ggplot(parameters, aes(x=year, y=mu.HB50.))+geom_line(size=0.75)+geom_point (size=2)+ ylab("Harvest rate") +
  xlab("") +
  geom_ribbon(aes(ymin=mu.HB2.5., ymax=mu.HB97.5.), alpha=0.15) +
  coord_cartesian(ylim=c(0,1)) + annotate("text",x = 1977, y=1.00, label="(b)", family="Arial", size=6) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  geom_line(aes(y=UMSY), colour="grey70", size=1, linetype=2) + theme(legend.position = "none") -> plot3

cowplot::plot_grid(plot1,plot3, plot2,  align = "v", nrow = 3, ncol=1) 
ggsave(out.file, dpi = 500, height = 8, width = 9, units = "in")

# RETURNS----
# returns
out.file <- paste0("output/rjags_base_case/processed/returns_est.png")
maxY<-max(parameters$R97.5., na.rm=TRUE)*1.5
ggplot(parameters, aes(x=year, y=(R50.))) + geom_line(size=0.75) + 
  geom_point(size=2)+ylab("Returns (R)") + xlab("") + annotate("text",x = 1977, y= 90000, label="(a)", family="Arial" ,size=6) +
  geom_ribbon(aes(ymin=R2.5., ymax=R97.5.), alpha=0.20) +
  scale_y_continuous(labels = comma,breaks = seq(0, 90000, 10000), limits = c(0, 90000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot1

# Ricker productivity residuals
maxY<-max(parameters$log.resid97.5., na.rm=TRUE)+0.5
minY<-min(parameters$log.resid2.5., na.rm=TRUE)-0.5
ggplot(parameters, aes(x=year, y=log.resid50.))+#geom_line(size=0.75)+
  geom_point (size=2) + 
  ylab("Productivity Residuals")+xlab("Brood Year") + annotate("text",x = 1977, y=1.5, label="(b)", family="Arial" ,size=6) +
  geom_ribbon(aes(ymin=parameters$log.resid2.5., ymax=parameters$log.resid97.5.), alpha=0.20) +
  geom_line(aes(y=0), colour="black", size=0.5, lty=2) +geom_line(aes(y=rollmean(log.resid50., 5, na.pad=TRUE, align ="right"))) +
  scale_y_continuous(breaks = seq(-2.5, 2, 0.50), limits = c(-2.5, 1.5)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(legend.position = "none") -> plot2
cowplot::plot_grid(plot1,plot2,  align = "v", nrow = 2, ncol=1) 
ggsave(out.file, dpi = 500, height = 8, width = 9, units = "in")

# PROPORTIONS----
# mean age at maturity (p), age composition (q), terminal run by age proportions (Nya)
out.file <- paste0("output/rjags_base_case/processed/props.png")

p_q_Nya %>%
  mutate(age_comp = as.numeric(age_comp),
          year = as.numeric(year),
          Age = factor(age, ordered = TRUE, 
                              levels = c( "Ages 2-4", "Age 5", "Ages 6-8"),
                              labels = c("Ages 2-4", "Age 5", "Ages 6-8"))) %>%
  mutate(age_comp = ifelse(Age == 'Ages 2-4', NA, age_comp)) -> data

ggplot(data,aes(x=year, y=p, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=TRUE)) + scale_fill_grey(start=0.1, end=0.8) +
  theme(legend.title=element_blank(),legend.position=c(0.90,0.86)) +
  ylab("Age-at-Maturity Proportions") + xlab("") + annotate("text",x = 1977, y=1.00, label= "(a)", family="Arial" ,size=6) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1977, 2022)) -> plot1  

ggplot(data,aes(x=year, y=q, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=FALSE)) +
  scale_fill_grey(start=0.1, end=0.8) + annotate("text",x = 1977, y=1.00, label="(b)", family="Arial" ,size=6) +
  ylab("Age Composition Proportions") + xlab("") +
  theme(legend.title=element_blank(), legend.position="none") + geom_point(aes(x=year, y=age_comp), position='stack') +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1977, 2022)) -> plot2

ggplot(data,aes(x=year, y=Nya, fill=as.factor(Age))) +
  geom_area(position=position_stack(reverse=TRUE)) + scale_fill_grey(start=0.1, end=0.8) +
  ylab("Terminal Run by Age")+xlab("Year") + annotate("text",x = 1977, y=30000, label="(c)", family="Arial" ,size=6) +
  guides(fill = guide_legend(reverse=FALSE)) + 
  theme(legend.title=element_blank(), legend.position="none") +
  scale_y_continuous(labels = comma,breaks = seq(0, 30000, 5000), limits = c(0, 30000)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1977, 2022)) -> plot3
cowplot::plot_grid(plot1,plot2,plot3, align = "v", nrow = 3, ncol=1) 
ggsave(out.file, dpi = 500, height = 11, width = 8, units = "in")

# HORSETAIL PLOTS----
num <- nrow(QM)
QM %>%
  dplyr::select(c(Escapement)) -> x
coda %>%
  dplyr::select(c(lnalpha.c, beta)) %>%
  filter(row_number()==1:50) %>%
  slice(rep(1:n(), each = num)) %>%
  cbind(., x) %>%#lnalpha.c, beta, and S
 mutate(Recruitment = Escapement*exp(lnalpha.c-beta*Escapement),
           variable = rep(1:50,each=num)) -> dataset

QM %>%
  dplyr::select(c(Escapement)) %>%
  mutate (lnalpha.c = lnalpha.c,
          beta = beta,
          Recruitment = Escapement*exp(lnalpha.c-beta*Escapement),
          variable = 51) %>%
  rbind(., dataset) %>%
  mutate(year = 'NA',
         R2.5. = 0,
         R97.5. = 0,
         S2.5. = 0,
         S97.5. = 0) -> dataset

parameters %>%
  filter (year %in% c(1983:2015)) %>%
  dplyr::select(c(year, S50., R50., R2.5., R97.5., S2.5., S97.5.)) %>%
  mutate(lnalpha.c = 'NA',
         beta = 'NA',
         Escapement = S50.,
         Recruitment = R50.,
         variable = 52) %>%
  dplyr::select(year, lnalpha.c, beta, Escapement, Recruitment, variable, R2.5., R97.5., S2.5., S97.5.) %>%
  rbind(., dataset) -> dataset

dataset %>%
  filter (variable %in% c(52)) %>%
  mutate(Escapement1 = Escapement) %>%
  dplyr::select(-c(lnalpha.c, beta, Escapement)) %>%
  mutate(Escapement = 'NA',
         Median = 'NA',
         q95 = 'NA',
         q90 ='NA',
         q10 ='NA',
         q5 = 'NA') -> dataset
CI %>%
  mutate(year = 'NA',
         R2.5.= 'NA',
         R97.5. = 'NA',
         S2.5. ='NA',
         S97.5. ='NA',
         variable = 51,
         Recruitment = 'NA',
         Escapement1 ='NA') %>%
  rbind(., dataset) %>%
  mutate_if(is.character, as.numeric) -> dataset1

x.fact <- 100/max(dataset1$Escapement1) 
y.fact <- 100/max(dataset1$Recruitment)
coords <- FFieldPtRep(coords = cbind(dataset1$Escapement1 * x.fact, dataset1$Recruitment * y.fact), rep.fact = 40)
x.t <- coords$x/x.fact 
y.t <- coords$y/y.fact

out.file <- paste0("output/rjags_base_case/processed/horsetail.png")
 
ggplot(data=dataset1, aes(x=Escapement, y=Median, group=variable)) + 
  geom_line(size=1, lty=2, group=51) +
  geom_ribbon(aes(ymin = q5, ymax = q95, group=51), alpha=.08) +
  geom_ribbon(aes(ymin = q10, ymax = q90, group=51), alpha=.08) +
  xlab("Spawners (S)") +
  ylab("Recruits (R)") +
  scale_y_continuous(labels = comma,breaks = seq(0, 50000, 5000), limits = c(0, 50000)) +
  scale_x_continuous(labels = comma,breaks = seq(0, 50000, 5000), limits = c(0, 50000)) +
  geom_line(aes(x=Escapement, y=Escapement, group=51),linetype="solid", size=1) +
  geom_point(data=dataset1, aes(x=x.t, y=y.t, group=52),pch=16, size=1) +
  geom_errorbar(data=dataset1, aes(x=Escapement1, ymax=R97.5., ymin=R2.5., group=52), width=0.2,linetype = 1, colour="grey70") +
  geom_point(data=dataset1, aes(x=Escapement1, y=Recruitment, group=52),pch=16, size=1) +
  geom_errorbarh(data=dataset1, aes(x=Escapement1, y=Recruitment, xmax=S97.5., xmin=S2.5.),na.rm=T,  linetype = 1, colour="grey70") +
  geom_text(size=3, data=dataset1, aes(x=Escapement1, y=Recruitment, group=52, label=year,family="Times", 
                                     hjust = -0.1, vjust= -0.4)) 
ggsave(out.file, dpi = 500, height = 6, width = 8, units = "in")

# ESCAPEMENT ESTIMATES
parameters <- read.csv("output/rjags_base_case/parameters.csv") 
parameters %>%
  filter (year %in% c(1983:2019)) -> parameter_set

  out.file <- paste0("output/rjags_base_case/processed/hist_esc.png")

ggplot(parameter_set, aes(x=year, y=S50.)) + 
  annotate("rect", ymin = 4000, ymax = 9000, xmin = 1977, xmax = 2020,
           fill = "grey90", alpha = 0.9) +
  geom_line(size=0.75) + geom_point (size=2) + ylab("Escapement (S)") + xlab("Year") +
  scale_y_continuous(labels = comma,breaks = seq(0, 25000, 5000), limits = c(0, 25000)) + 
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1977, 2020)) +                    
  theme(legend.position = "topright") +
  #geom_hline(yintercept = SMAX, colour="grey40", size=1, linetype=4) +
  #geom_hline(yintercept = SMSY, colour="grey40", size=1, linetype=2) +
  geom_errorbar(aes(ymin=S2.5., ymax=S97.5.),size=0.5, linetype = "solid", colour="grey40", width=0.02)
  #geom_hline(yintercept=SEQ, colour="grey50", size=1, linetype=1)

ggsave(out.file, dpi = 500, height = 6, width = 8, units = "in")

# ln(R/S) versus year
parameters <- read.csv("output/rjags_base_case/parameters.csv") 
parameters %>%
  filter (year %in% c(1983:2015)) %>%
  mutate(lnRS = log(R50./S50.))-> parameters

  out.file <- paste0("output/rjags_base_case/processed/lnRS.png")
ggplot(parameters, aes(x=year, y=lnRS))+
  geom_point(size=4, color = "black", shape = 18) +
  xlab("Brood Year") + 
  ylab("ln(R/S)") +
  scale_y_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels, limits = c(1980, 2020))+
  geom_label((aes(label=year)))+ 
  geom_smooth(method="gam", formula=y~s(x, k=10),colour="black", 
              method.args=list(family="gaussian", se=TRUE)) 
ggsave(out.file, dpi = 500, height = 6, width = 8, units = "in")

# ln(R/S) versus S
parameters <- read.csv("output/rjags_base_case/parameters.csv") 
parameters %>%
  filter (year %in% c(1983:2015)) %>%
  mutate(lnRS = log(R50./S50.),
         S = (S50.))-> parameters
ggplot(parameters, aes(x=S, y=lnRS))+
  geom_point(size=4, color = "black", shape = 18) +
  scale_y_continuous(breaks = seq(-2, 2, 0.5), limits = c(-2, 2)) +
  scale_x_continuous(labels = comma,breaks = seq(0, 20000, 5000), limits = c(0, 20000)) + 
  xlab("Spawners (S)") + 
  ylab("ln(R/S)") +
  geom_label((aes(label=year)))+ 
  geom_smooth(method="gam", formula=y~s(x, k=10),colour="black", 
              method.args=list(family="gaussian", se=TRUE)) 
ggsave("output/rjags_base_case/processed/lnRS_S.png", dpi = 500, height = 6, width = 8, units = "in")


