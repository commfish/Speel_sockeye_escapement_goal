# geom_vline(xintercept=6276); need to update with SMSY value
profile <-function(i,z,xa.start, xa.end,lnalpha.c, beta){ 
  xa = seq(xa.start, xa.end, by=i) 
  x =(xa+i)*z
  # empty dataframes
  dat <- data.frame(S0=rep(1, length(coda[,1])))
  dat1 <- data.frame(S0=rep(0, length(coda[,1])))
  dat2 <- data.frame(S0=rep(0, length(coda[,1])))
  dat3 <- data.frame(S0=rep(1, length(coda[,1])))
  dat4 <- data.frame(S0=rep(0, length(coda[,1])))
  dat5 <- data.frame(S0=rep(0, length(coda[,1])))
  dat6 <- data.frame(S0=rep(1, length(coda[,1])))
  dat7 <- data.frame(S0=rep(0, length(coda[,1])))
  dat8 <- data.frame(S0=rep(0, length(coda[,1])))
  dat9 <- data.frame(S0=rep(0, length(coda[,1])))
  dat10 <- data.frame(S0=rep(0, length(coda[,1])))
  for (i in 1:length(xa)){
    dat[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.7*coda$MSY.c), 0, ifelse(dat[,i]==0, 0,1))
    dat1[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.7*coda$MSY.c), 1,0)
    dat2[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.7*coda$Rmax), 1,0)
    dat3[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.8*coda$MSY.c), 0, ifelse(dat3[,i]==0, 0,1))
    dat4[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.8*coda$MSY.c), 1,0)
    dat5[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.8*coda$Rmax), 1,0)
    dat6[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.9*coda$MSY.c), 0, ifelse(dat6[,i]==0, 0,1))
    dat7[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i])-x[i])>(0.9*coda$MSY.c), 1,0)
    dat8[,i+1] = ifelse((x[i] * exp(coda$lnalpha.c-coda$beta*x[i]))>(0.9*coda$Rmax), 1,0)
    dat9[,i+1] = x[i]*exp(coda$lnalpha.c-coda$beta*x[i])-x[i] #expected yield
    dat10[,i+1] = x[i]*exp(coda$lnalpha.c-coda$beta*x[i]) # CI around S
  }
  # Overfishing estimate ----
  f.over <- function(x){
    x %>% 
      dplyr::filter(complete.cases(.)) %>% 
      dplyr::summarise_all(funs(mean)) %>% 
      tidyr::gather() %>% 
      dplyr::select(value)
  }
  
  of_0.7 <- f.over(dat)
  of_0.8 <- f.over(dat3)
  of_0.9 <- f.over(dat6)
  
  # Optimal yield estimate ----
  oy_0.7 <- f.over(dat1)
  oy_0.8 <- f.over(dat4)
  oy_0.9 <- f.over(dat7)
  
  # Optimal recruitment ----
  or_0.7 <- f.over(dat2)
  or_0.8 <- f.over(dat5)
  or_0.9 <- f.over(dat8)
  
  # Bind dataframes together
  Y <- cbind(of_0.7,oy_0.7,or_0.7,of_0.8,oy_0.8,or_0.8,of_0.9,oy_0.9,or_0.9, c(0, x))
  names(Y) <- c('of_0.7','oy_0.7','or_0.7','of_0.8','oy_0.8','or_0.8','of_0.9','oy_0.9',
                'or_0.9','Escapement')
  
  # Quantiles and Medians ----
  dat9 %>%
    summarise_all(funs(median = median, 
                       q95=quantile(., 0.95, na.rm=T), 
                       q90=quantile(., 0.90, na.rm=T),
                       q10=quantile(., 0.10, na.rm=T),
                       q5=quantile(., 0.05, na.rm=T))) -> mq
  names(mq) <- c(rep(('Median'),length(x)+1), 
                 rep(('q95'),length(x)+1), 
                 rep(('q90'),length(x)+1), 
                 rep(('q10'),length(x)+1), 
                 rep(('q5'),length(x)+1))
  
  qm <- data.frame(measure = names(mq), value = as.numeric(mq[1,]), Escapement=rep(c(0,x), length(unique(names(mq)))))
  qm <- spread(qm, measure, value)
  qm <- qm[c("q95", "q90", "Median","q10", "q5", "Escapement")]
  Y <- Y[c("oy_0.9", "oy_0.8", "or_0.9","or_0.8", "of_0.9", "of_0.8", "oy_0.7","or_0.7","of_0.7","Escapement")]
  write.csv(qm,("output/rjags_base_case/processed/QM.csv"), row.names=FALSE)
  write.csv(Y,("output/rjags_base_case/processed/Y.csv"), row.names=FALSE)
  
# confidence intervals ----
  dat10 %>%
     summarise_all(funs(median = median, 
                        q95=quantile(., 0.95, na.rm=T), 
                        q90=quantile(., 0.90, na.rm=T),
                        q10=quantile(., 0.10, na.rm=T),
                        q5=quantile(., 0.05, na.rm=T))) -> mq
  names(mq) <- c(rep(('Median'),length(x)+1), 
                  rep(('q95'),length(x)+1), 
                  rep(('q90'),length(x)+1), 
                  rep(('q10'),length(x)+1), 
                  rep(('q5'),length(x)+1))
  
  CI <- data.frame(measure = names(mq), value = as.numeric(mq[1,]), Escapement=rep(c(0,x), length(unique(names(mq)))))
  CI <- spread(CI, measure, value)
  CI <- CI[c("q95", "q90", "Median","q10", "q5", "Escapement")]
  write.csv(CI,("output/rjags_base_case/processed/CI.csv"), row.names=FALSE)
  
read.csv("output/rjags_base_case/processed/Y.csv") -> Y
  Y %>% 
    dplyr::select(Escapement, oy_0.9, oy_0.8, oy_0.7) %>% 
    gather(key="variable", value="value", -Escapement) %>% 
    mutate(sra = "Yield Profile",
           max_pct =ifelse(grepl("oy_0.7",variable), 
                           0.7,
                    ifelse(grepl("oy_0.8",variable),0.8, 0.9 )))-> my1
  
  Y %>% 
    dplyr::select(Escapement, of_0.9, of_0.8, of_0.7) %>% 
    gather(key="variable", value="value", -Escapement) %>% 
    mutate(sra = "Overfishing Profile",
           max_pct =ifelse(grepl("of_0.7",variable), 
                           0.7,
                           ifelse(grepl("of_0.8",variable),0.8, 0.9)))-> my2
  
  Y %>% 
    dplyr::select(Escapement, or_0.9, or_0.8, or_0.7) %>% 
    gather(key="variable", value="value", -Escapement) %>% 
    mutate(sra = "Recruitment Profile",
           max_pct =ifelse(grepl("or_0.7",variable), 
                           0.7,
                           ifelse(grepl("or_0.8",variable),0.8, 0.9 )))-> my3
  
  
  my4<-rbind(my1, my2, my3)
  my4 %>%
    dplyr::select(Escapement, variable, value, sra, max_pct) %>%
    mutate(Escapement = as.numeric(Escapement),
           Probability = as.numeric(value),
           max_pct = as.factor(max_pct)) -> my4
  my4 %>%
    filter(sra == "Yield Profile") -> fig_data1
  
  my4 %>%
    filter(sra == "Overfishing Profile") -> fig_data2
  
  my4 %>%
    filter(sra == "Recruitment Profile") -> fig_data3
    
# PROFILES----
  ggplot(fig_data1, aes(x = Escapement, y = Probability, linetype = max_pct)) + 
    annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
             inherit.aes = FALSE, fill = "grey90", alpha = 0.9) +
    geom_line() + theme(legend.position= "none") +
    scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000),expand=c(0,0))+
    scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
    scale_linetype_discrete(name = "Percent of Max.") + xlab('Escapement (S)')+
    facet_grid(sra ~ .) +geom_vline(xintercept=6276  , lwd=1.25,colour="grey30") -> plot1
  
  ggplot(fig_data2, aes(x = Escapement, y = Probability, linetype = max_pct)) +
    annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
             inherit.aes = FALSE, fill = "grey90", alpha = 0.9) +
    theme(legend.position=c(0.90,0.85), legend.title = element_blank()) +
    geom_line() + xlab('Escapement (S)') +
    scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000),expand=c(0,0))+
    scale_linetype_discrete(name = "Percent of Max.") + 
    facet_grid(sra ~ .) +geom_vline(xintercept=6276 , lwd=1.25,colour="grey30")-> plot2
  
  ggplot(fig_data3, aes(x = Escapement, y = Probability, linetype = max_pct)) +
    annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
             inherit.aes = FALSE, fill = "grey90", alpha = 0.9) +
  theme(legend.position= "none") +
    geom_line() + xlab('Escapement (S)') +  
    scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000),expand=c(0,0))+
    scale_linetype_discrete(name = "Percent of Max.") +
    facet_grid(sra ~ .) +geom_vline(xintercept=6276  , lwd=1.25,colour="grey30") -> plot3
  cowplot::plot_grid(plot2,plot3,plot1, align = "v", nrow = 3, ncol=1) 
  ggsave("output/rjags_base_case/processed/profiles.png", dpi = 500, height = 8, width = 9, units = "in")
  
# EXPECTED SUSTAINED YIELD----
out.file <- paste0("output/rjags_base_case/processed/expect_yield.png")
ggplot(qm, aes(Escapement, Median))+geom_line(size=1)+
  geom_ribbon(aes(ymin = q5, ymax = q95), alpha=.15)+
  geom_ribbon(aes(ymin = q10, ymax = q90), alpha=.15)+ xlab("Escapement (S)")+
  ylab("Expected Yield")+scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma,breaks = seq(0, 20000, 5000), limits = c(0,20000))+
  scale_y_continuous(labels = comma,breaks = seq(-20000, 20000, 5000), limits = c(-20000,20000))+
  geom_vline(xintercept = LowerB,linetype = "longdash" )+geom_vline(xintercept = UpperB ,linetype = "longdash")+
  geom_vline(xintercept = SMSY,linetype = 1 )
ggsave(out.file, dpi = 500, height = 4, width = 6, units = "in")

# SEAK plot 
ggplot(fig_data1, aes(x = Escapement, y = Probability, linetype = max_pct)) + ggtitle("(c) Yield Profile") + 
  annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
           inherit.aes = FALSE, fill = "grey80", alpha = 0.9) +
  geom_line() +    theme(plot.title = element_text(size = 12, face = "bold"),
                         strip.text.y = element_text(size=0),legend.position= "none") +
  scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000))+
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1))+
  scale_linetype_discrete(name = "Percent of Max.") + xlab('Escapement (S)')+
  facet_grid(sra ~ .)  -> plot1

ggplot(fig_data2, aes(x = Escapement, y = Probability, linetype = max_pct)) + 
  annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
           inherit.aes = FALSE, fill = "grey80", alpha = 0.9) + ggtitle("(a) Overfishing Profile") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size=0),legend.position=c(0.92,0.86), legend.title = element_blank()) +
  geom_line() + xlab('Escapement (S)') +
  scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000))+
  scale_linetype_discrete(name = "Percent of Max.") + 
  facet_grid(sra ~ .) -> plot2

ggplot(fig_data3, aes(x = Escapement, y = Probability, linetype = max_pct)) + 
  annotate("rect", xmin = 4000, xmax = 9000, ymin = 0, ymax = 1,
           inherit.aes = FALSE, fill = "grey80", alpha = 0.9) + ggtitle("(b) Recruitment Profile") + 
  geom_line() + xlab('Escapement (S)') +   theme(plot.title = element_text(size = 12, face = "bold"),
                                                 strip.text.y = element_text(size=0),legend.position= "none") +
  scale_x_continuous(labels = comma, breaks = seq(0, 14000, 2000), limits = c(0, 14000))+
  scale_linetype_discrete(name = "Percent of Max.") +
  facet_grid(sra ~ .)  -> plot3
cowplot::plot_grid(plot2,plot3,plot1, align = "v", nrow = 3, ncol=1) 
ggsave("output/rjags_base_case/processed/SEAK_report_english.png", dpi = 500, height = 7, width = 6, units = "in")}



