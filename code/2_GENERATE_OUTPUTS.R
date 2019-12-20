# THIS SCRIPT IS SOURCED FROM INSIDE 1_RUN_MODEL.R
#(if package.use = "rjags") 

# - post.samp and post 
      # these are the mcmc.list objects created by coda.samples
      # you can access individual variables like this: post[,"var.name"]
# - post.arr 
# - coda
# create coda file
#Gelman statistic
#Brooks and Gelman (1997) have suggested, if Rc<1.2 for all model parameters, 
#one can be fairly confident that convergence has been reached. Otherwise, longer chains or other means for improving the convergence may be needed
#Brooks, S. P., and A. Gelman. 1997. General Methods for Monitoring Convergence of Iterative Simulations. 
#Journal of Computational and Graphical Statistics 7: 434–455.
loadfonts(device="win")
windowsFonts(Times=windowsFont("Arial"))

parameters <- c('alpha','beta', 'alpha.c', 'lnalpha','lnalpha.c','phi',
                'sigma.R','log.resid.0', 'mean.log.R0','log.resid',
                'S','R','N','pi','H.B','N.ya','mu.HB',
                'p','q', 'S.max','D.sum','sigma.R0',
                'S.eq.c', 'U.msy.c', 'S.msy.c', 'B.sum')

# Numerical summary of each parameter (mean, median, quantiles of posteriers)----
summary<-summary(post)  
stats<-summary$statistics;  colnames(stats)
quants<-summary$quantiles;  colnames(quants)
statsquants <- cbind(stats,quants) 
statsquants %>% 
  as.data.frame() %>% 
  dplyr::select(Mean, SD, 'Time-series SE', '2.5%', '50%', '97.5%') %>%
  dplyr::rename(time_series_se = 'Time-series SE') %>%
  rownames_to_column('variable') %>%
  mutate (mc_error = time_series_se/SD,
  converge = ifelse(mc_error < 0.05, "true", "false")) %>%
  write.csv(., file= paste0(out.path,"/statsquants.csv"))    

# Gelman Diagnostics----
gel <- as.data.frame(gelman.diag(post, multivariate=F)[[1]])
poor.threshold = 1.2 #values less than 1.2 are generally considered converged
gel %>%
  rownames_to_column('variable') %>%
  rename(point_estimate = "Point est.") %>%
  mutate (converge = ifelse(point_estimate < poor.threshold, "true", "false")) %>%
  write.csv(., file= paste0(out.path,"/gelman.csv") )   

# Geweke Diagnostics----
#Examine convergence of the Markov chains using the Geweke's convergence diagnostic
#a convergence diagnostic for Markov chains based on a test for equality 
#of the means of the ﬁrst and last part of a Markov chain (by default the ﬁrst 10% and the last 50%).
#As for a cut-off you can compare to the standard normal critical values z α/2 where α=0.05
#null hypothesis is rejected if Z large (used to determine the burn-in period)
# Geweke diagnostic
l <- geweke.diag(post)
df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),stringsAsFactors=FALSE)
df <- t(df) %>%
  as.data.frame() %>%
  rownames_to_column('variable1') -> df
colnames(df) <- c("variable1", "chain1", "chain2", "chain3")
names <-read.csv(file = paste0(out.path,"/statsquants.csv"))
names  %>%
  dplyr::select(variable) %>% 
  add_row(variable = "frac1") %>% #0.1 and 0.5 default
  add_row(variable = "frac2")  -> names
x <- cbind(names, df)
x  %>% 
  dplyr::select(-variable1) %>%
  write.csv(., paste0(out.path,"/geweke.csv")) 

pdf("output/rjags_base_case/geweke.pdf",height=10, width=8,onefile=T)
geweke.plot(post)
dev.off()

# 90th and 95th percentiles of output quants----
parameters=c('alpha')
x <- post.arr[,parameters,]
alpha<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
alpha <- data.frame(alpha)

parameters=c('alpha.c')
x <- post.arr[,parameters,]
alpha.c<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
alpha.c <- data.frame(alpha.c)

parameters=c('lnalpha')
x <- post.arr[,parameters,]
lnalpha<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
lnalpha <- data.frame(lnalpha)

parameters=c('lnalpha.c')
x <- post.arr[,parameters,]
lnalpha.c<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
lnalpha.c <- data.frame(lnalpha.c)

parameters=c('beta')
x <- post.arr[,parameters,]
beta<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
beta <- data.frame(beta)

parameters=c('phi')
x <- post.arr[,parameters,]
phi<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
phi <- data.frame(phi)

parameters=c('sigma.R')
x <- post.arr[,parameters,]
sigma.R<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
sigma.R <- data.frame(sigma.R)

parameters=c('S.eq.c')
x <- post.arr[,parameters,]
S.eq.c<-quantile(x, probs=c(0,0.025, 0.05, 0.5, 0.95, 0.975, 1))
S.eq.c <- data.frame(S.eq.c)

parameters=c('S.max')
x <- post.arr[,parameters,]
S.max<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
S.max <- data.frame(S.max)

parameters=c('D.sum')
x <- post.arr[,parameters,]
D.sum<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
D.sum <- data.frame(D.sum)

parameters=c('B.sum')
x <- post.arr[,parameters,]
B.sum<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
B.sum <- data.frame(B.sum)

parameters=c('pi[1]')
x <- post.arr[,parameters,]
pi<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
pi1 <- data.frame(pi)

parameters=c('pi[2]')
x <- post.arr[,parameters,]
pi<-quantile(x, probs=c(0,0.025, 0.05, 0.5, 0.95, 0.975, 1))
pi2 <- data.frame(pi)

parameters=c('pi[3]')
x <- post.arr[,parameters,]
pi<-quantile(x, probs=c(0, 0.025, 0.05, 0.5, 0.95, 0.975, 1))
pi3 <- data.frame(pi)

step1 <- cbind(alpha, alpha.c)
step2 <- cbind(step1, lnalpha)
step3 <- cbind(step2, lnalpha.c)
step4 <- cbind(step3, beta)
step5 <- cbind(step4, phi)
step6 <- cbind(step5, sigma.R)
step7 <- cbind(step6, S.eq.c)
step8 <- cbind(step7, S.max)
step9 <- cbind(step8, D.sum)
step10 <- cbind(step9, B.sum)
step11 <- cbind(step10, pi1)
step12 <- cbind(step11, pi2)
step13 <- cbind(step12, pi3)
step13 %>% 
  t()%>%
write.csv(., file= paste0(out.path,"/percentiles.csv")) 

# p_q_Nya.csv file----
rawdat<-as.data.frame(read.csv("data/Speel_sockeye.csv",header=T) ) #get age_comp data
rawdat  %>% 
  dplyr::select (c(year, x4, x5, x6)) %>%
  mutate(age4 = x4/(x4+x5+x6),
         age5 = x5/(x4+x5+x6),
         age6 = x6/(x4+x5+x6)) %>%
  dplyr::select (c(year, age4, age5, age6)) %>%
  gather(key="age", value="age_comp", -year) %>%
  mutate(age = ifelse(age == 'age4', "Ages 2-4",
                      ifelse(age== 'age5', "Age 5","Ages 6-8"))) -> age_data
summary <- summary(post)# extract p
stats <- summary$statistics;  colnames(stats)
stats <- stats[,c(1)] #select columns of interest
data.frame(stats) %>% 
  rownames_to_column('variable') -> stats
stats %>% 
  filter(str_detect(variable, "p")) %>% 
  filter(!str_detect(variable, "alpha")) %>%
  filter(!str_detect(variable, "lnalpha")) %>% 
  filter(!str_detect(variable, "lnalpha.c")) %>%
  filter(!str_detect(variable, "phi")) %>%
  filter(!str_detect(variable, "pi")) %>%
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "p"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = c(1977:2015, 1977:2015, 1977:2015),
  age = rep(c("Ages 2-4", "Age 5", "Ages 6-8"), each =39))
p_data <- cbind(df, new.df)

stats %>% 
  filter(str_detect(variable, "q")) %>% 
  filter(!str_detect(variable, "S.eq.c")) %>%
  filter(!str_detect(variable, " S.eq.c2")) %>%
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "q"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = c(1983:2019, 1983:2019, 1983:2019),
  age = rep(c("Ages 2-4", "Age 5", "Ages 6-8"), each = 37))
q_data <- cbind(df, new.df)

stats %>% 
  filter(str_detect(variable, "N.ya")) %>% 
  rename_at(vars(starts_with("stats")), 
            funs(str_replace(., "stats", "Nya"))) %>%
  dplyr::select(-c(variable))-> df
Nya_data <- cbind(df, new.df)
step1 <- merge(age_data, p_data, by=c("year", "age"), all=TRUE)
step2 <- merge(step1, q_data, by=c("year", "age"), all=TRUE)
p_q_Nya <- merge(step2, Nya_data, by=c("year", "age"), all=TRUE)
p_q_Nya <- p_q_Nya[order(p_q_Nya$age, p_q_Nya$year),]
p_q_Nya %>% mutate(age = as.character(age))
write.csv(p_q_Nya, file= paste0(out.path,"/p_q_Nya.csv")) 

# parameters file----
data.frame(quants) %>% 
  rownames_to_column('variable') -> quants
quants %>%
  filter(str_detect(variable, "h.b")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "h.b"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1983:2019),1))
df1 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "mu.HB")) %>% 
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "mu.HB"))) %>%
  dplyr::select(-c(variable))-> df
df2 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "S")) %>% 
  filter(!str_detect(variable, "S.eq.c")) %>%
  filter(!str_detect(variable, " S.eq.c2")) %>%
  filter(!str_detect(variable, "S.max")) %>%
  filter(!str_detect(variable, "S.msy.c")) %>%
  filter(!str_detect(variable, "S.msy.c2")) %>%
  filter(!str_detect(variable, "lnRS")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "S"))) %>%
  dplyr::select(-c(variable))-> df
df3 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "N")) %>% 
  filter(!str_detect(variable, "N.ya")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "N"))) %>%
  dplyr::select(-c(variable))-> df
df4 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "log.resid")) %>% 
  filter(!str_detect(variable, "log.resid.0")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "log.resid"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1983:2015),1))
df5 <- cbind(df, new.df)

quants %>%
  filter(str_detect(variable, "R")) %>%
  filter(!str_detect(variable, "mean.log.R0")) %>%
  filter(!str_detect(variable, "sigma.R")) %>%
  filter(!str_detect(variable, "sigma.R0")) %>%
  filter(!str_detect(variable, "lnRS")) %>%
  rename_at(vars(starts_with("X")), 
            funs(str_replace(., "X", "R"))) %>%
  dplyr::select(-c(variable))-> df
new.df <- data.frame(
  year = rep(c(1977:2015),1))
df6 <- cbind(df, new.df)

step1 <- merge(df1, df2, by=c("year"), all=TRUE)
step2 <- merge(step1, df3, by=c("year"), all=TRUE)
step3 <- merge(step2, df4, by=c("year"), all=TRUE)
step4 <- merge(step3, df5, by=c("year"), all=TRUE)
step5 <- merge(step4, df6, by=c("year"), all=TRUE)
write.csv(step5, file= paste0(out.path,"/parameters.csv")) 

# lambert calc----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- as.data.frame(post.arr[,parameters,])
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  dplyr::rename(beta = beta.1,
         lnalpha = lnalpha.1,
         lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  dplyr::rename(beta = beta.2,
         lnalpha = lnalpha.2,
         lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  dplyr::rename(beta = beta.3,
         lnalpha = lnalpha.3,
         lnalpha.c = lnalpha.c.3) -> coda3
coda<-rbind(coda1,coda2,coda3)
write.csv(coda, file= paste0(out.path,"/coda.csv") ,row.names=FALSE)  
coda %>% 
  mutate(Smsy_lambert = (1-lambert_W0(exp(1-lnalpha.c)))/beta,
         Umsy_lambert = (1-lambert_W0(exp(1-lnalpha.c))))  %>%
  as.data.frame() %>%
  dplyr::select(Smsy_lambert,Umsy_lambert) -> coda
coda %>% 
  apply(., 2, sd) %>%
  as.data.frame()%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column('variable') %>%
  mutate(variable = ifelse(variable == '.', "sd", "sd"))-> sd
coda %>% 
  apply(., 2, mean) %>%
  as.data.frame() %>%
  t()%>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  mutate(variable = ifelse(variable == '.', "mean", "mean"))-> mean
rbind(sd,mean) -> x1
q1<-apply(coda,2,quantile,probs=c(0,0.025,0.05,0.5,0.95,0.975,1))
q1 %>%
  as.data.frame() %>%
  rownames_to_column('variable') -> x2
rbind(x1,x2) %>%
  t() %>%
write.csv(., file= paste0(out.path,"/quantiles_lambert.csv"))    

# lambert density plot----
parameters=c("lnalpha", "beta", "lnalpha.c")
x <- as.data.frame(post.arr[,parameters,])
coda1 <- x[,1:3]
coda2 <- x[,4:6]
coda3 <- x[,7:9]
coda1 %>% 
  dplyr::rename(beta = beta.1,
                lnalpha = lnalpha.1,
                lnalpha.c = lnalpha.c.1)-> coda1
coda2 %>% 
  dplyr::rename(beta = beta.2,
                lnalpha = lnalpha.2,
                lnalpha.c = lnalpha.c.2) -> coda2
coda3 %>% 
  dplyr::rename(beta = beta.3,
                lnalpha = lnalpha.3,
                lnalpha.c = lnalpha.c.3) -> coda3
coda<-rbind(coda1,coda2,coda3)
coda %>% 
  mutate(Smsy = (1-lambert_W0(exp(1-lnalpha.c)))/beta,
         Umsy = (1-lambert_W0(exp(1-lnalpha.c)))) %>%
  as.data.frame() -> coda
coda %>% 
  dplyr::select(Smsy) -> Smsy
coda %>% 
  dplyr::select(Umsy) -> Umsy

# density plot
out.file <- paste0("output/rjags_base_case/density.png")
options(scipen=999)
ggplot(Smsy, aes(x=Smsy, fill=Smsy, color = Smsy)) +
  geom_density(fill ="#999999", alpha=0.5)+ annotate("text",x = 0, y=0.0006, label="a)", family="Arial" ,size=6) +
  scale_color_manual(values=c("#999999"))+
  scale_fill_manual(values=c("#999999"))+
  #geom_vline(xintercept = 43857,linetype = "longdash" ) +
  labs(x="Smsy",y="Density") + theme_set(theme_bw(base_size=14,base_family=
                                             'Arial')+
                                    theme(panel.grid.major = element_blank(),
                                          panel.grid.minor = element_blank())) + theme(legend.position="none")+ 
 scale_x_continuous(labels = comma,breaks = seq(0, 25000, 5000), limits = c(0, 25000)) -> plot1

ggplot(Umsy, aes(x=Umsy, fill=Umsy)) +
  geom_density(fill ="#999999", alpha=0.5)+ annotate("text",x = 0, y=5, label="b)", family="Arial" ,size=6) +
  scale_color_manual(values=c("#999999"))+
  scale_fill_manual(values=c("#999999"))+#geom_vline(xintercept = 0.75,linetype = "longdash" ) +
  labs(x="Umsy",y="Density") + theme_set(theme_bw(base_size=14,base_family=
                                                      'Arial')+
                                             theme(panel.grid.major = element_blank(),
                                                   panel.grid.minor = element_blank())) + theme(legend.position="none")+ 
  scale_x_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) -> plot2

cowplot::plot_grid(plot1,plot2,  align = "v", nrow = 2, ncol=1) 
ggsave(out.file, dpi = 500, height = 6, width = 8, units = "in")

#trace and density plots----
parameters <- c('alpha', 'lnalpha', 'lnalpha.c','beta','phi','sigma.R','S.eq.c','S.max','D.sum', 'pi', 'alpha.c', 'B.sum')
png("output/rjags_base_case/density_other.png",res=500, width=8, height=9, units ="in")
denplot(post, parms = c(parameters), style="plain", greek=TRUE, col="gray30", collapse =T)
dev.off()
dev.off()

# autocorrelation plots----
windows(record=T)
pdf("output/rjags_base_case/autocorr.pdf",height=6, width=8,onefile=T,useDingbats=F)
autocorr.plot(post, lag.max=1)
dev.off()
dev.off()
autocorr.summary<-autocorr.diag(post)
autocorr.summary<-data.frame(autocorr.summary)
write.csv(autocorr.summary, file= paste0(out.path,"/autocorr.csv")) 
