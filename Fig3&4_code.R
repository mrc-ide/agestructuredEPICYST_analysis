#===============================================================================================================================================================#
#                                            Code used for Figure 3: processing and plotting                                                                    #
#===============================================================================================================================================================#
#==================#
#     LIBRARIES    #
library(dplyr) 
library(ggplot2)
#install.packages("viridis")
library(viridis)
#====================================================================================#
# Load-in looped simulations (frequency x covrage x min slaughter age combinations)  #

Int_run_out1 # pig MDA (all eligible ages)
Int_run_out2 # pig vaccine (all eligible ages)
Int_run_out3 # pig MDA (age-restricted) 
Int_run_out4 # pig MDA & vaccine (all eligible ages) 
Int_run_out5 # pig MDA (age-restricted) & vaccine (all eligible ages)
Int_run_out6 # pig MDA & vaccine (age-restricted) 
Int_run_out7 # Human MDA (all eligible ages) 
Int_run_out8 # Human MDA (only school-age children) 
Int_run_out9 # Pig interventions proposed at CYSTINET

# =================================================== #
#        Pig MDA (all eligible)                       #

process_model_runs_func <- function(data, specify_intervention) {
  
  data <- data %>% filter(Freq == 3) # subset for 3 months
  
  # Convert months to years and minus 1 year (to account for intervention starting at 1 yr)
  
  # time to reach 1 % PCC prev # 
  data$time_to_threshold_pcc1_yrs <- (data$time_to_threshold_pcc1 - 12)/12
  data$time_to_threshold_pcc1_yrs_rounded <- round(data$time_to_threshold_pcc1_yrs)
  
  # time to reach 5 % PCC prev # 
  data$time_to_threshold_pcc5_yrs <- (data$time_to_threshold_pcc5 - 12)/12
  data$time_to_threshold_pcc5_yrs_rounded <- round(data$time_to_threshold_pcc5_yrs)
  
  # time to reach 10 % PCC prev # 
  data$time_to_threshold_pcc10_yrs <- (data$time_to_threshold_pcc10 - 12)/12
  data$time_to_threshold_pcc10_yrs_rounded <- round(data$time_to_threshold_pcc10_yrs)

  # time to reach HTT time to elim # 
  data$time_to_elim_htt_yrs <- (data$time_to_elim_htt - 12)/12
  data$time_to_elim_htt_yrs_rounded <- round(data$time_to_elim_htt_yrs)

  if(specify_intervention == "Pig_MDA"){
  # make factor lable for intervention
  data$Intervention <- rep(as.factor("Pig MDA only
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_vaccine"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig vaccine only
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_MDAvaccine"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig MDA & vaccine 
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_MDAage"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig MDA 
only to min 
slaughter age
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_MDAage_vaccine"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig 'restricted' MDA 
& vaccine 
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_MDAage_vaccineage"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig 'restricted' MDA 
& 'restricted' vaccine 
Frequency = 3 months"))
  }
  if(specify_intervention == "Pig_CYSTINET"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Pig MDA & vaccine
1 round to all pigs,
subsequent rounds only 
to pigs < 6 months
(CYSTINET-AFRICA),
Frequency = 3 months"))
  }
  if(specify_intervention == "Human_MDA"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Human MDA
(Praziquantel) 
Frequency = 3 months"))
  }
  if(specify_intervention == "Human_MDAsac"){
    # make factor lable for intervention
    data$Intervention <- rep(as.factor("Human MDA
(Praziquantel)
only to school-age
children (5-15 years),
Frequency = 3 months"))
  }
  
  # select specific columns and make dataframe for each prevalence target #
  # 1% prevalence target
  
  Target_1pccprev <- data[c(1,2,3,4,8,12,19,20)]
  names(Target_1pccprev) <- c("Cov","Min_slaughter_age","Freq","ID","Time_months","Time_years","Time_years_rnd",
                            "Intervention")
  Target_1pccprev$Indicator <- rep(as.factor("PCC target prevalence (<1%)"))
  
  # 5% prevalence target
  Target_5pccprev <- data[c(1,2,3,4,9,13,14,20)]
  names(Target_5pccprev) <- c("Cov","Min_slaughter_age","Freq","ID","Time_months","Time_years","Time_years_rnd",
                            "Intervention")
  Target_5pccprev$Indicator <- rep(as.factor("PCC target prevalence (<5%)"))
  
  # 10 % prevalence target
  Target_10pccprev <- data[c(1,2,3,4,10,15,16,20)]
  names(Target_10pccprev) <- c("Cov","Min_slaughter_age","Freq","ID","Time_months","Time_years","Time_years_rnd",
                             "Intervention")
  Target_10pccprev$Indicator <- rep(as.factor("PCC target prevalence (<10%)"))
  
  # combine prevalence target dataframes 
  Combined_targets <- rbind(Target_1pccprev, Target_5pccprev, Target_10pccprev)
  Combined_targets <- Combined_targets  %>% filter(Cov >= 0.5) # select coverages >= 50% to plot in heat maps

return(Combined_targets)

}

# select and run function to prepare for each intervention for heat maps

Combined_pigMDA <- process_model_runs_func(data = Int_run_out1, specify_intervention = "Pig_MDA")
# Combined_pigvaccine <- process_model_runs_func(data = Int_run_out2, specify_intervention = "Pig_vaccine")
# Combined_pigMDAage <- process_model_runs_func(data = Int_run_out3, specify_intervention = "Pig_MDAage")
# Combined_pigMDAvaccine <- process_model_runs_func(data = Int_run_out4, specify_intervention = "Pig_MDAvaccine")
# Combined_pigMDAage_vaccine <- process_model_runs_func(data = Int_run_out5, specify_intervention = "Pig_MDAage_vaccine")
# Combined_pigMDAage_vaccineage <- process_model_runs_func(data = Int_run_out6, specify_intervention = "Pig_MDAage_vaccineage")
# Combined_humanMDA <- process_model_runs_func(data = Int_run_out7, specify_intervention = "Human_MDA")
# Combined_humanMDAsac <- process_model_runs_func(data = Int_run_out8, specify_intervention = "Human_MDAsac")
# Combined_pigCYSTINET <- process_model_runs_func(data = Int_run_out9, specify_intervention = "Pig_CYSTINET")


#===========================================================================================================================#
#             HEATMAPS: PLOTTING ACROSS INTERVENTIONS (& % RUN COMPLETED)                                                   #
#===========================================================================================================================#

#============================================================================#
#  Figure 3: Heatmaps for interventions applied to all eligible age groups   #

#================================#
#   LOAD (if required)           #

# Combined_pigMDA
# Combined_pigvaccine
# Combined_pigMDAmin
# Combined_pigMDAvaccine
# Combined_humanMDAPZQ

# remove extraneous X column (if needed) #

# Combined_pigMDA <- subset (Combined_pigMDA_3mnth, select = -c(X))
# Combined_pigvaccine_3mnth <- subset (Combined_pigvaccine_3mnth, select = -c(X))
# Combined_pigMDAvaccine_3mnth <- subset (Combined_pigMDAvaccine_3mnth, select = -c(X))

# ribind dataframes together : interventions applied to all ages # 
Combined_allages <- rbind(Combined_pigMDA, Combined_pigvaccine, Combined_pigMDAvaccine, Combined_humanMDA)

Combined_allages <- Combined_allages  %>% filter(Cov >= 0.5) # select coverages >= 50% to plot in heat maps

Combined_allages <- Combined_allages[Combined_allages$Indicator!="HTT 'elimination' (<1 case)",] #remove all rows with this

Combined_allages$Indicator <- factor(Combined_allages$Indicator) # remove factor level next

Combined_allages$Indicator <- factor(Combined_allages$Indicator, levels(Combined_allages$Indicator)[c(1,3,2)]) # reorder indicator levels

Combined_allages$Intervention <- as.factor(Combined_allages$Intervention)

Combined_allages$Intervention <- factor(Combined_allages$Intervention, levels(Combined_allages$Intervention)[c(3, 4, 2, 1)]) # reorder indicator levels

# heatmap plot code to use for figure 3
Figure3 <-
  ggplot(Combined_allages, aes(as.factor(Min_slaughter_age), as.factor(Cov))) + 
  geom_tile(aes(fill = Time_years))+
  scale_fill_viridis("Time to reach target (years)", na.value = 'gray73', direction=-1, breaks=c(1,2,3,4,5,6,7,8,9),limits=c(0,10))+
  scale_x_discrete(breaks=c("5","10","15","20"))+
  scale_y_discrete(breaks=c("0.5","0.6","0.7","0.8","0.9","1.0"))+
  facet_grid(Intervention~Indicator)+
  theme_bw()+
  theme(
    plot.background=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    strip.text.x = element_text(size=10, face="bold"),
    strip.text.y = element_text(size=10, face="bold", angle=360),
    legend.position="bottom",
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=12, angle=-1))+
  labs(x="Minimum age from which pigs slaughtered (months)", y="Coverage (%)")

# ggsave("Figure3.tiff", width = 210, height = 148, units = 'mm', dpi = 300)

write.csv(Combined_allages, "Combined_allages.csv")
write.table(Combined_allages, "Combined_allages.txt", sep="\t")

# Alternative heat map plot code to use:

# Figure3 <-
#   ggplot(Combined_allages,aes(Min_slaughter_age, Cov)) + 
#   geom_tile(aes(fill = Time_years))+
#   scale_fill_viridis("Time to reach target (years)", na.value = 'gray73', direction=-1, breaks=c(1,2,3,4,5,6,7,8,9),limits=c(0,10))+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
#   facet_grid(Intervention~Indicator)+
#   theme_bw()+
#   theme(
#     plot.background=element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
#     strip.background = element_blank(),
#     panel.border = element_rect(colour = "black"),
#     strip.text.x = element_text(size=10, face="bold"),
#     strip.text.y = element_text(size=10, face="bold", angle=360),
#     legend.position="bottom",
#     axis.title.x = element_text(size=14, face="bold"),
#     axis.title.y = element_text(size=14, face="bold"),
#     legend.title=element_text(size=14), 
#     legend.text=element_text(size=12, angle=-1))+
#   labs(x="Minimum age from which pigs slaughtered (months)", y="Coverage (%)")

#============================================================================#
#  Figure 4: Heatmaps for interventions applied to specific age groups       #

#================================#
#   LOAD (if required)           #

Combined_pigMDAage
Combined_pigMDAage_vaccine
Combined_pigMDAage_vaccineage
Combined_humanMDAsac
Combined_pigCYSTINET


# remove extraneous X column (if needed) #

# Combined_pigMDA <- subset (Combined_pigMDA_3mnth, select = -c(X))
# Combined_pigvaccine_3mnth <- subset (Combined_pigvaccine_3mnth, select = -c(X))
# Combined_pigMDAvaccine_3mnth <- subset (Combined_pigMDAvaccine_3mnth, select = -c(X))

# ribind dataframes together : interventions applied to specific ages # 
Combined_specificages <- rbind(Combined_pigMDAage, Combined_pigMDAage_vaccine, Combined_pigMDAage_vaccineage, Combined_humanMDAsac, Combined_pigCYSTINET)

Combined_specificages <- Combined_specificages  %>% filter(Cov >= 0.5) # select coverages >= 50% to plot in heat maps

Combined_specificages <- Combined_specificages[Combined_specificages$Indicator!="HTT 'elimination' (<1 case)",] #remove all rows with this

Combined_specificages$Indicator <- factor(Combined_specificages$Indicator) # remove factor level next

Combined_specificages$Indicator <- factor(Combined_specificages$Indicator, levels(Combined_specificages$Indicator)[c(1,3,2)]) # reorder indicator levels

Combined_specificages$Intervention <- as.factor(Combined_specificages$Intervention)

Combined_specificages$Intervention <- factor(Combined_specificages$Intervention, levels(Combined_specificages$Intervention)[c(3, 4, 2, 1, 5)]) # check & reorder indicator levels

# heatmap plot code to use for figure 4
Figure4 <-
  ggplot(Combined_specificages, aes(as.factor(Min_slaughter_age), as.factor(Cov))) + 
  geom_tile(aes(fill = Time_years))+
  scale_fill_viridis("Time to reach target (years)", na.value = 'gray73', direction=-1, breaks=c(1,2,3,4,5,6,7,8,9),limits=c(0,10))+
  scale_x_discrete(breaks=c("5","10","15","20"))+
  scale_y_discrete(breaks=c("0.5","0.6","0.7","0.8","0.9","1.0"))+
  facet_grid(Intervention~Indicator)+
  theme_bw()+
  theme(
    plot.background=element_blank(),
    panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black"),
    strip.text.x = element_text(size=10, face="bold"),
    strip.text.y = element_text(size=10, face="bold", angle=360),
    legend.position="bottom",
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=12, angle=-1))+
  labs(x="Minimum age from which pigs slaughtered (months)", y="Coverage (%)")

# ggsave("Figure4.tiff", width = 210, height = 148, units = 'mm', dpi = 300)

write.csv(Combined_specificages, "Combined_specificages.csv")
write.table(Combined_specificages, "Combined_specificages.txt", sep="\t")

# Alternative heat map plot code to use:

# Figure4 <-
#   ggplot(Combined_specificages,aes(Min_slaughter_age, Cov)) + 
#   geom_tile(aes(fill = Time_years))+
#   scale_fill_viridis("Time to reach target (years)", na.value = 'gray73', direction=-1, breaks=c(1,2,3,4,5,6,7,8,9),limits=c(0,10))+
#   scale_x_continuous(expand=c(0,0))+
#   scale_y_continuous(expand=c(0,0))+
#   facet_grid(Intervention~Indicator)+
#   theme_bw()+
#   theme(
#     plot.background=element_blank(),
#     panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"),
#     strip.background = element_blank(),
#     panel.border = element_rect(colour = "black"),
#     strip.text.x = element_text(size=10, face="bold"),
#     strip.text.y = element_text(size=10, face="bold", angle=360),
#     legend.position="bottom",
#     axis.title.x = element_text(size=14, face="bold"),
#     axis.title.y = element_text(size=14, face="bold"),
#     legend.title=element_text(size=14), 
#     legend.text=element_text(size=12, angle=-1))+
#   labs(x="Minimum age from which pigs slaughtered (months)", y="Coverage (%)")
