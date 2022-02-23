#=====================================================================================================================#
#                   CODE FOR FIGURE 5: PLOTTING INTERVENTION PROGRAMMES TERMINATING AFTER 5 YEARS                     #
#=====================================================================================================================#
library(dplyr)


# load EPICYST 
devtools::load_all()

#============================================================================================================================#
#               Set - up model run                                                                                           #
#============================================================================================================================#

# Set up key baseline parameters
psi_input <- 1/1.5

a <- 1/(15*12)
b <-1/12 - a
c <- 1/b
slgep <- c/12 # year for epicyst input


S1<-Set_up(psi = psi_input, Slaughter_age_min = 5,
           PCPrev_true = 0.333975, CPrev_true = 0.0611, TPrev_true= 0.03055,
           LEP = 15, SlgEP = slgep, number_age_classes_pig = 150,
           PPS = 1000, HPS = 12000)#gives an endemic prec of (PCC = 0.25, HTT= 0.03, HCC= 0.06)


tau_baseline <- S1[[1]]$tau

S2 <- set_up(psi = psi_input, Slaughter_age_min = 5,
                PCPrev_true = 0.333975, CPrev_true = 0.0611, TPrev_true= 0.03055,
                LEP = 15, SlgEP = slgep, number_age_classes_pig = 150,
                PPS = 1000, HPS = 12000,
                tau_input = tau_baseline)#gives an endemic prec of (PCC = 0.25, HTT= 0.03, HCC= 0.06)


#============================================================================================================================#
#                       Run different interventions for Figure 5 & store output & final PCC prevalence value                 #


#================================#
#     1) Pig MDA                 #

i1 <- run_model(time = 11, burn_in = 100, intervention='Pig_MDA', intervention_time=1, 
                intervention_frequency = 3, 
                pig_MDA_cov = 0.75, age_target_pig_MDA = c(4:150),
                num_intervention_rounds = 21, Pig_MDA_prop_noimmunity = 1.0,
                params = S2[[1]], Initial_states = S2[[2]]) 

Pig_MDA_run <- i1 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_MDA_run$intervention <- rep(as.factor("Pig MDA"))

last_row <- Pig_MDA_run[3961,]
PCC_prev_pigMDA <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigMDA


#================================#
#     2) Pig vaccine             #

i2 <- run_model(time = 11, burn_in = 50, intervention = 'Pig_vaccine', intervention_time = 1, 
                intervention_frequency = 3, pig_vaccine_ds1_cov =  0.75, pig_vaccine_ds2_cov =  0.75,
                num_intervention_rounds = 21, age_target_pig_vaccine = c(4:150),
                params = S2[[1]], initial_states = S2[[2]]) 

Pig_vaccine_run <- i2 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_vaccine_run$intervention <- rep(as.factor("Pig vaccine"))

last_row <-Pig_vaccine_run[3961,]
PCC_prev_pigvaccine <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigvaccine



#================================#
#  3) Pig MDA (age-restricted)   #

i3 <- run_model(time = 11, burn_in = 100, intervention = 'Pig_MDA', intervention_time = 1, 
                intervention_frequency = 3, pig_MDA_cov = 0.75, age_target_pig_MDA = c(4:7),
                num_intervention_rounds = 21, Pig_MDA_prop_noimmunity = 1.0,
                params = S2[[1]], initial_states = S2[[2]]) 

Pig_minMDA_run <- i3 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_minMDA_run$intervention <- rep(as.factor("'Restricted' pig MDA"))

last_row <- Pig_minMDA_run[3961,]
PCC_prev_pigminMDA <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigminMDA

#================================#
#    4) Pig MDA & vaccine        #

i4 <- run_model(time = 11, burn_in = 50, intervention = c('Pig_MDA','Pig_vaccine'), intervention_time = 1, 
                intervention_frequency = 3, pig_vaccine_ds1_cov =  0.75, pig_vaccine_ds2_cov =  0.75,
                pig_MDA_cov = 0.75, age_target_pig_MDA = c(4:150), age_target_pig_vaccine = c(4:150),
                Pig_MDA_prop_noimmunity = 1.0,
                num_intervention_rounds = 21,
                Params = S2[[1]], initial_states = S2[[2]]) 

Pig_MDAvaccine_run <- i4 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_MDAvaccine_run$intervention <- rep(as.factor("Pig MDA & vaccine"))

last_row <-Pig_MDAvaccine_run[3961,]
PCC_prev_pigMDAvaccine <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigMDAvaccine

#=================================================#
#    5) Pig MDA (age-restricted) & vaccine        #


i5 <- run_model(time=11, burn_in=50, intervention = c('Pig_MDA','Pig_vaccine'), intervention_time = 1, 
                intervention_frequency = 3, pig_vaccine_ds1_cov =  0.75, pig_vaccine_ds2_cov =  0.75,
                pig_MDA_cov = 0.75, age_target_pig_MDA = c(4:7), Pig_MDA_prop_noimmunity = 1.0,
                age_target_pig_vaccine = c(4:150),
                num_intervention_rounds = 21,
                params = S2[[1]], initial_states = S2[[2]]) 

Pig_minMDAvaccine_run <- i5 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_minMDAvaccine_run$intervention <- rep(as.factor("'Restricted' Pig MDA & vaccine"))

last_row <-Pig_minMDAvaccine_run[3961,]
PCC_prev_pigminMDAvaccine <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigminMDAvaccine

#=======================================#
#     6) Pig min MDA & min vaccine      #

i6 <- run_model(time = 11, burn_in = 50, intervention = c('Pig_MDA','Pig_vaccine'), intervention_time = 1, 
                intervention_frequency = 3, pig_vaccine_ds1_cov =  0.75, pig_vaccine_ds2_cov =  0.75,
                pig_MDA_cov = 0.75, age_target_pig_MDA = c(4:7), Pig_MDA_prop_noimmunity = 1.0,
                age_target_pig_vaccine = c(4:7),
                num_intervention_rounds = 21,
                params = S2[[1]], initial_states = S2[[2]]) # run model with intervention freq [i]

Pig_minMDAminvaccine_run <- i6 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_minMDAminvaccine_run$intervention <- rep(as.factor("'Restricted' Pig MDA & 'restricted' vaccine"))

last_row <-Pig_minMDAminvaccine_run[3961,]
PCC_prev_pigminMDAminvaccine <- last_row$Pig_Cysticercosis_prev
PCC_prev_pigminMDAminvaccine

#====================================#
#     7) Pig - CYSTINET              #

i7 <- run_model(time = 11, burn_in = 50, intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                intervention_time_stage1 = 1, intervention_frequency_stage1 = 3, num_intervention_rounds_stage1 = 1,
                intervention_time_stage2 = 1.25, intervention_frequency_stage2 = 3,
                pig_MDA_cov_stage1 = 0.75,  pig_MDA_cov_stage2 = 0.75,
                pig_vaccine_ds1_cov_stage2 = 0.75, pig_vaccine_ds2_cov_stage2 = 0.75,
                pig_vaccine_ds1_cov_stage1 = 0.75, pig_vaccine_ds2_cov_stage1 = 0.75,
                age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:7),
                Pig_MDA_prop_noimmunity = 1.0,
                age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7),
                num_intervention_rounds_stage2 =  20,
                params = S2[[1]], initial_states = S2[[2]]) # run model with intervention freq [i]

Pig_cystinet_run <- i7 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Pig_cystinet_run$intervention <- rep(as.factor("Pig-directed interventions
(CYSTINET-AFRICA, 2019)"))

last_row <-Pig_cystinet_run[3961,]
PCC_prev_cystinet <- last_row$Pig_Cysticercosis_prev
PCC_prev_cystinet


#================================#
#     8) Human MDA               #

i8 <- run_model(time = 11, burn_in = 50, intervention = 'Human_MDA_pzq', intervention_time = 1, intervention_frequency = 3,
                human_MDApzq_cov = 0.75, age_target_human_MDA = c(2:7),
                num_intervention_rounds = 21,
                params = S2[[1]], initial_states = S2[[2]]) 

Human_MDA_run <- i8 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Human_MDA_run$intervention <- rep(as.factor("Human MDA (praziquantel)"))

last_row <-Human_MDA_run[3961,]
PCC_prev_humanMDA <- last_row$Pig_Cysticercosis_prev
PCC_prev_humanMDA

#================================#
#    9) Human MDA (SAC)          #

i9 <- run_model(time = 11, burn_in = 50, intervention = 'Human_MDA_pzq', intervention_time = 1, intervention_frequency = 3,
                human_MDApzq_cov = 0.75, age_target_human_MDA = c(2:3),
                num_intervention_rounds = 21,
                params = S2[[1]], initial_states = S2[[2]]) 

Human_MDAsac_run <- i9 %>%
  select(t, Pig_Cysticercosis_prev, Human_Taeniasis_prev, Human_Cysticercosis_prev)

Human_MDAsac_run$intervention <- rep(as.factor("Human MDA SAC (praziquantel)"))

last_row <-Human_MDAsac_run[3961,]
PCC_prev_humanMDAsac <- last_row$Pig_Cysticercosis_prev
PCC_prev_humanMDAsac

#=======================================#
#      Combine different interventions  #

Combined_intervention_runs <- rbind(Pig_MDA_run, Pig_minMDA_run, Pig_vaccine_run, Pig_MDAvaccine_run,
                                    Pig_minMDAvaccine_run, Pig_minMDAminvaccine_run,
                                    Pig_cystinet_run, Human_MDA_run, Human_MDASAC_run)

dat_text <- data.frame(
  label = c(PCC_prev_pigMDA, PCC_prev_pigminMDA, PCC_prev_pigvaccine,
            PCC_prev_pigMDAvaccine,PCC_prev_pigminMDAvaccine, PCC_prev_pigminMDAminvaccine,
            PCC_prev_cystinet, PCC_prev_humanMDA, PCC_prev_humanMDASAC),
  #label2 = as.character(label),
  intervention   = c("Pig MDA", "'Restricted' pig MDA", "Pig vaccine", "Pig MDA & vaccine",
                     "'Restricted' Pig MDA & vaccine",  "'Restricted' Pig MDA & 'restricted' vaccine",
                     "Pig-directed interventions
(CYSTINET-AFRICA, 2019)", "Human MDA (praziquantel)", "Human MDA SAC (praziquantel)")
)

dat_text$pcc_prev <- round(dat_text$label, digits = 3)

# ===========#
# if loading #

# Combined_intervention_runs <- Combined_intervention_runs_5mnthMINslgt
# dat_text <- Combined_intervention_runs_5mnthMINslgt_TEXT
# 
# Combined_intervention_runs <- Combined_intervention_runs_24mnthMINslgt
# dat_text <- Combined_intervention_runs_24mnthMINslgt_TEXT


Combined_intervention_runs$intervention<- factor(Combined_intervention_runs$intervention, levels(Combined_intervention_runs$intervention)[c(7,1,9,8,3,2,6,4,5)]) # reorder indicator levels
levels(Combined_intervention_runs$intervention)[2] <- "Restricted pig MDA"
levels(Combined_intervention_runs$intervention)[5] <- "Restricted Pig MDA & vaccine"
levels(Combined_intervention_runs$intervention)[6] <- "Restricted Pig MDA & restricted vaccine" 
#Combined_restrictages2$Indicator <- factor(Combined_restrictages2$Indicator, levels(Combined_restrictages2$Indicator)[c(3,2,1)]) # reorder indicator levels

dat_text$intervention<- factor(dat_text$intervention, levels(dat_text$intervention)[c(7,1,9,8,3,2,6,4,5)]) # reorder indicator levels
levels(dat_text$intervention)[2] <- "Restricted pig MDA"
levels(dat_text$intervention)[5] <- "Restricted Pig MDA & vaccine"
levels(dat_text$intervention)[6] <- "Restricted Pig MDA & restricted vaccine" 

ggplot()+
  geom_hline(yintercept=0.01, linetype="dashed", 
             color = "red", size=1.1, alpha=0.4)+
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "blue", size=1.1, alpha=0.4)+
  geom_hline(yintercept=0.1, linetype="dashed", 
             color = "purple", size=1.1, alpha=0.4)+
  geom_line(data = Combined_intervention_runs, aes(x=t/12,y=Pig_Cysticercosis_prev), size=1.02)+
    scale_x_continuous(breaks=seq(12/12, 132/12, 12/12), limits=c(0, 132/12))+
  scale_y_continuous(breaks= seq(0, 0.3, 0.05), limits=c(0,0.3))+
  facet_wrap(~intervention, ncol=2)+
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 9.5),
        axis.text.y = element_text(size = 9.5),
        plot.title = element_text(size = 18, hjust=0.5),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        legend.title=element_text(size=14, face= "bold"), 
        legend.text=element_text(size=12))+
  labs(x="Time (years)", y="Pig cysticercosis prevalence")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 10, y = 0.3, label = pcc_prev))

ggplot()+
  geom_hline(yintercept=0.01, linetype="dashed", 
             color = "red", size=1.1, alpha=0.4)+
  geom_hline(yintercept=0.05, linetype="dashed", 
             color = "blue", size=1.1, alpha=0.4)+
  geom_hline(yintercept=0.1, linetype="dashed", 
             color = "purple", size=1.1, alpha=0.4)+
  geom_line(data = Combined_intervention_runs, aes(x=t/12,y=Pig_Cysticercosis_prev), size=1.02)+
  scale_x_continuous(breaks=seq(12/12, 132/12, 12/12), limits=c(0, 132/12))+
  scale_y_continuous(breaks= seq(0, 0.6, 0.1), limits=c(0,0.6))+
  facet_wrap(~intervention, ncol=2)+
  theme_minimal() +
  #theme(aspect.ratio = 0.5)+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 9.5),
        axis.text.y = element_text(size = 9.5),
        plot.title = element_text(size = 18, hjust=0.5),
        axis.title.x = element_text(size = 12, face= "bold"),
        axis.title.y = element_text(size = 12, face= "bold"),
        #axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "right",
        legend.title=element_text(size=14, face= "bold"), 
        legend.text=element_text(size=12))+
  labs(x="Time (years)", y="Pig cysticercosis prevalence")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  geom_text(
    data    = dat_text,
    mapping = aes(x = 10, y = 0.55, label = pcc_prev))

# SAVE PDF as 8.27 x 5.845 (in) as 1/2 A4 

# setwd(" ")

write.csv(Combined_intervention_runs, "Combined_intervention_runs_5mnthMINslgt.csv")
write.table(Combined_intervention_runs, "Combined_intervention_runs_5mnthMINslgt.txt", sep="\t")

write.csv(dat_text, "Combined_intervention_runs_5mnthMINslgt_TEXT.csv")
write.table(dat_text, "Combined_intervention_runs_5mnthMINslgt_TEXT.txt", sep="\t")

write.csv(Combined_intervention_runs, "Combined_intervention_runs_24mnthMINslgt.csv")
write.table(Combined_intervention_runs, "Combined_intervention_runs_24mnthMINslgt.txt", sep="\t")

write.csv(dat_text, "Combined_intervention_runs_24mnthMINslgt_TEXT.csv")
write.table(dat_text, "Combined_intervention_runs_24mnthMINslgt_TEXT.txt", sep="\t")
