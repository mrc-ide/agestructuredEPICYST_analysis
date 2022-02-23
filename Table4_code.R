#============================================================================================================================#
#                 CODE FOR TABLE 4: MINIMUM FRACTION/ COVERAGE X FREQUENCY TABLES                                            #
#============================================================================================================================#
#==================#
#     LIBRARIES    #
library(dplyr)
# library(ggplot2)
# #install.packages("viridis")
# library(viridis)

#=========================================================================#
# load (from Simulation_loops_prevalenceTrgts_script.R outputs & rename   #

Pig_MDA <- Int_run_out1 # pig MDA
Pig_Vacc <- Int_run_out2
Pig_MDAmin <- Int_run_out3
Pig_MDAvacc <- Int_run_out4
Pig_MDAminVacc <- Int_run_out5
Pig_MDAminVaccmin <- Int_run_out6
Human_MDA <- Int_run_out7
Human_MDASAC <- Int_run_out8
Pig_Cystinet <- Int_run_out9

# Human_MDA <- Human_MDA_updated

# Pig_MDA <- Int_run_out_pigMDA2_freq34612_151219 # pig MDA
# Pig_Vacc <- Int_run_out_pigvacc2_freq34612_181219 
# Pig_MDAmin <- Int_run_out2_pigMDAmin_freq34612_290120
# Pig_MDAvacc <- Int_run_out_pigMDAvacc2_freq34612_231219
# Pig_MDAminVacc <- Int_run_out2_pigMDAminvacc_freq34612_310120
# Pig_MDAminVaccmin <- Int_run_out_pigMDAminvaccmin2_freq34612_020220
# Pig_Cystinet <- Int_run_out2_pigMDAvaccine_CYSTINET_freq34612_280119
# #Human_MDA_master <- Int_run_out2_humanMDA_freq34612_060120
# Human_MDASAC<- Int_run_out2_humanMDASAC_freq34612_080120

# modify dataframes ready to combine #
Pig_MDA$intervention <- rep(as.factor("Pig MDA"))
Pig_Vacc$intervention <- rep(as.factor("Pig vaccine"))
Pig_MDAmin$intervention <- rep(as.factor("Restricted pig MDA"))
Pig_MDAvacc$intervention <- rep(as.factor("Pig MDA & vaccine"))
Pig_MDAminVacc$intervention <- rep(as.factor("Restricted pig MDA & vaccine"))
Pig_MDAminVaccmin$intervention <- rep(as.factor("Restricted pig MDA & restricted vaccine"))
Pig_Cystinet$intervention <- rep(as.factor("CYSTINET"))
Human_MDA$intervention <- rep(as.factor("Human MDA"))
Human_MDASAC$intervention <- rep(as.factor("Human MDA SAC"))


# Human_MDA <- Human_MDA[c(1,2,3,4,5,7,8,9,10,11,12,22)]

#===================================#
# Clean and prep combined dataframe #

Master_dfa <- rbind(Pig_MDA, Pig_Vacc, Pig_MDAvacc,
                    Pig_Cystinet, Human_MDA, Human_MDASAC)

Master_dfb <- rbind(Pig_MDAmin, Pig_MDAminVacc, Pig_MDAminVaccmin)

Master_dfb <- Master_dfb[,-6] # remove extra column in min interventions dataframes

Master_df <- rbind(Master_dfa, Master_dfb)

Master_df2 <- Master_df %>% filter(Min_slaughter_age %in% c(6,12,24))

Master_df2$Min_slaughter_age_factor <- as.factor(ifelse(Master_df2$Min_slaughter_age==6, "6 months",
                                                        ifelse(Master_df2$Min_slaughter_age==12, "12 months","24 months")))

Master_df2$Min_slaughter_age_factor <- factor(Master_df2$Min_slaughter_age_factor, levels = c("6 months",
                                                                                              "12 months",
                                                                                              "24 months"))

levels(Master_df2$Min_slaughter_age_factor)

Master_df2$Freq_factor <- as.factor(ifelse(Master_df2$Freq==3, "3 months",
                                           ifelse(Master_df2$Freq==4, "4 months",
                                                  ifelse(Master_df2$Freq==6, "6 months","12 months"))))


Master_df2$Freq_factor  <- factor(Master_df2$Freq_factor, levels = c("3 months","4 months","6 months","12 months"))

levels(Master_df2$Freq_factor)

Freq_out <- c(3,4,6,12) # vector of frequencies to select out


#=====================================#
#  Subset based on prevalence targets #

#=========================================================================================================================== #
# 1% pcc threshold - this prevalence threshold is shown in table 4 (can be modified to include 5%, 10% PCC prev thresholds)  #

package_dataframe_prevtargets_func <- function(Master_df2, specify_intervention, Freq_out){
  
  if(specify_intervention == "Pig_MDA") {
  Dataframe1 <- Master_df2  %>% filter(intervention == "Pig MDA" & Min_slaughter_age == 6)
  Dataframe2 <- Master_df2  %>% filter(intervention == "Pig MDA" & Min_slaughter_age == 12)
  Dataframe3 <- Master_df2  %>% filter(intervention == "Pig MDA" & Min_slaughter_age == 24)
  
  }
  
  if(specify_intervention == "Pig_vaccine") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Pig vaccine" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Pig vaccine" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Pig vaccine" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Pig_MDAvaccine") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Pig MDA & vaccine" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Pig MDA & vaccine" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Pig MDA & vaccine" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Pig_MDAage") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Restricted pig MDA" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Restricted pig MDA" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Restricted pig MDA" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Pig_MDAage_vaccine") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & vaccine" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & vaccine" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & vaccine" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Pig_MDAage_vaccineage") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & restricted vaccine" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & restricted vaccine" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Restricted pig MDA & restricted vaccine" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Pig_cystinet") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "CYSTINET" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "CYSTINET" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "CYSTINET" & Min_slaughter_age == 24)
  }

  if(specify_intervention == "Human_MDA") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Human MDA" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Human MDA" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Human MDA" & Min_slaughter_age == 24)
  }
  
  if(specify_intervention == "Human_MDAsac") {
    Dataframe1 <- Master_df2  %>% filter(intervention == "Human MDA SAC" & Min_slaughter_age == 6)
    Dataframe2 <- Master_df2  %>% filter(intervention == "Human MDA SAC" & Min_slaughter_age == 12)
    Dataframe3 <- Master_df2  %>% filter(intervention == "Human MDA SAC" & Min_slaughter_age == 24)
  }

# ======================== #
# PCC 1% prevalence target #

# 6 mnth min slgt age for 3,4,6,12 freq #
MinSlgt6mo_PCC1_df1 <- Dataframe1 %>% group_by(Freq_factor) %>% top_n(1,time_to_threshold_pcc1)
MinSlgt6mo_PCC1_df2 <- MinSlgt6mo_PCC1_df1 %>% group_by(Freq_factor) %>% slice_min(order_by = Cov, n = 1)
MinSlgt6mo_PCC1_df3  <- MinSlgt6mo_PCC1_df2$Cov
MinSlgt6mo_PCC1_nas  <- rep_len(NA, length.out=(length(Freq_out) - length(MinSlgt6mo_PCC1_df3)))
MinSlgt6mo_PCC1_df4 <- c(MinSlgt6mo_PCC1_df3, MinSlgt6mo_PCC1_nas)
MinSlgt6mo_PCC1_dfout <- MinSlgt6mo_PCC1_df2$time_to_threshold_pcc1
MinSlgt6mo_PCC1_dfout2 <- c(MinSlgt6mo_PCC1_dfout, MinSlgt6mo_PCC1_nas)

# 12 mnth min slgt age for 3,4,6,12 freq #
MinSlgt12mo_PCC1_df1 <- Dataframe2 %>% group_by(Freq_factor) %>% top_n(1,time_to_threshold_pcc1)
MinSlgt12mo_PCC1_df2 <- MinSlgt12mo_PCC1_df1 %>% group_by(Freq_factor) %>% slice_min(order_by = Cov, n = 1)
MinSlgt12mo_PCC1_df3  <- MinSlgt12mo_PCC1_df2$Cov
MinSlgt12mo_PCC1_nas  <- rep_len(NA, length.out=(length(Freq_out) - length(MinSlgt12mo_PCC1_df3)))
MinSlgt12mo_PCC1_df4 <- c(MinSlgt12mo_PCC1_df3, MinSlgt12mo_PCC1_nas)
MinSlgt12mo_PCC1_dfout <- MinSlgt12mo_PCC1_df2$time_to_threshold_pcc1
MinSlgt12mo_PCC1_dfout2 <- c(MinSlgt12mo_PCC1_dfout, MinSlgt12mo_PCC1_nas)

# 24 mnth min slgt age for 3,4,6,12 freq #
MinSlgt24mo_PCC1_df1 <- Dataframe3 %>% group_by(Freq_factor) %>% top_n(1,time_to_threshold_pcc1)
MinSlgt24mo_PCC1_df2 <- MinSlgt24mo_PCC1_df1 %>% group_by(Freq_factor) %>% slice_min(order_by = Cov, n = 1)
MinSlgt24mo_PCC1_df3  <- MinSlgt24mo_PCC1_df2$Cov
MinSlgt24mo_PCC1_nas  <- rep_len(NA, length.out=(length(Freq_out) - length(MinSlgt24mo_PCC1_df3)))
MinSlgt24mo_PCC1_df4 <- c(MinSlgt24mo_PCC1_df3, MinSlgt24mo_PCC1_nas)
MinSlgt24mo_PCC1_dfout <- MinSlgt24mo_PCC1_df2$time_to_threshold_pcc1
MinSlgt24mo_PCC1_dfout2 <- c(MinSlgt24mo_PCC1_dfout, MinSlgt24mo_PCC1_nas)

return(list(MinSlgt6mo_PCC1_df4, MinSlgt12mo_PCC1_df4, MinSlgt24mo_PCC1_df4))

}

# ================================================================================================= #
# run function and return dataframe of min fractions/coverage by min slaughter age x intervention   #


# pig MDA #
pig_MDA_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_MDA", Freq_out = Freq_out)
pigMDA_minslgt6mo <- pig_MDA_out[[1]]
pigMDA_minslgt12mo <- pig_MDA_out[[2]]
pigMDA_minslgt24mo <- pig_MDA_out[[3]]

# pig vaccine #
pig_vacc_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_vaccine", Freq_out = Freq_out)
pigvacc_minslgt6mo <- pig_vacc_out[[1]]
pigvacc_minslgt12mo <- pig_vacc_out[[2]]
pigvacc_minslgt24mo <- pig_vacc_out[[3]]

# pig MDA & vaccine #
pig_MDAvacc_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_MDAvaccine", Freq_out = Freq_out)
pigMDAvacc_minslgt6mo <- pig_MDAvacc_out[[1]]
pigMDAvacc_minslgt12mo <- pig_MDAvacc_out[[2]]
pigMDAvacc_minslgt24mo <- pig_MDAvacc_out[[3]]

# pig MDA (age) #
pig_MDAage_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_MDAage", Freq_out = Freq_out)
pigMDAage_minslgt6mo <- pig_MDAage_out[[1]]
pigMDAage_minslgt12mo <- pig_MDAage_out[[2]]
pigMDAage_minslgt24mo <- pig_MDAage_out[[3]]

# pig MDA (age) + vaccine #
pig_MDAage_vaccine_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_MDAage_vaccine", Freq_out = Freq_out)
pigMDAagevaccine_minslgt6mo <- pig_MDAage_vaccine_out[[1]]
pigMDAagevaccine_minslgt12mo <- pig_MDAage_vaccine_out[[2]]
pigMDAagevaccine_minslgt24mo <- pig_MDAage_vaccine_out[[3]]

# pig MDA (age) + vaccine (age)#
pig_MDAage_vaccineage_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_MDAage_vaccineage", Freq_out = Freq_out)
pigMDAagevaccineage_minslgt6mo <- pig_MDAage_vaccineage_out[[1]]
pigMDAagevaccineage_minslgt12mo <- pig_MDAage_vaccineage_out[[2]]
pigMDAagevaccineage_minslgt24mo <- pig_MDAage_vaccineage_out[[3]]

# pig CystiNet #
pig_cystinet_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Pig_cystinet", Freq_out = Freq_out)
pigcystinet_minslgt6mo <- pig_cystinet_out[[1]]
pigcystinet_minslgt12mo <- pig_cystinet_out[[2]]
pigcystinet_minslgt24mo <- pig_cystinet_out[[3]]

# Human MDA #
Human_MDA_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Human_MDA", Freq_out = Freq_out)
HumanMDA_minslgt6mo <- Human_MDA_out[[1]]
HumanMDA_minslgt12mo <- Human_MDA_out[[2]]
HumanMDA_minslgt24mo <- Human_MDA_out[[3]]

# Human MDA (SAC) #
Human_MDAsac_out <- package_dataframe_prevtargets_func(Master_df2 = Master_df2, specify_intervention = "Human_MDAsac", Freq_out = Freq_out)
HumanMDAsac_minslgt6mo <- Human_MDAsac_out[[1]]
HumanMDAsac_minslgt12mo <- Human_MDAsac_out[[2]]
HumanMDAsac_minslgt24mo <- Human_MDAsac_out[[3]]

#============================================#
#        Re-combine into final table         #
#============================================#

## 6 months min slgt age ~ 1% PCC prev target ##
min.fraction.data.out.6mnthslgt <- matrix(c(pigMDA_minslgt6mo, pigMDAage_minslgt6mo , 
                                            pigvacc_minslgt6mo, pigMDAvacc_minslgt6mo,
                                            pigMDAagevaccine_minslgt6mo, pigMDAagevaccineage_minslgt6mo,
                                            pigcystinet_minslgt6mo,
                                            HumanMDA_minslgt6mo, HumanMDAsac_minslgt6mo), nrow=9, byrow = TRUE)

colnames(min.fraction.data.out.6mnthslgt)<-c(3,4,6,12)
rownames(min.fraction.data.out.6mnthslgt)<-c("Pig MDA", "'Restricted' Pig MDA",
                                             "Pig vaccine",
                                             "Pig MDA and vaccine", "'Restricted' pig MDA and vaccine",
                                             "'Restricted' pig MDA and restricted vaccine", "CYSTINET",
                                             "Human MDA (praziquantel)", "Human MDA SAC (praziquantel)")
min.fraction.data.out.6mnthslgt2 <- as.table(min.fraction.data.out.6mnthslgt)
min.fraction.data.out.6mnthslgt2

## 12 months min slgt age ~ 1% PCC prev target ##
min.fraction.data.out.12mnthslgt <- matrix(c(pigMDA_minslgt12mo, pigMDAage_minslgt12mo , 
                                             pigvacc_minslgt12mo, pigMDAvacc_minslgt12mo,
                                             pigMDAagevaccine_minslgt12mo, pigMDAagevaccineage_minslgt12mo,
                                             pigcystinet_minslgt12mo,
                                             HumanMDA_minslgt12mo, HumanMDAsac_minslgt12mo), nrow=9, byrow = TRUE)

colnames(min.fraction.data.out.12mnthslgt)<-c(3,4,6,12)
rownames(min.fraction.data.out.12mnthslgt)<-c("Pig MDA", "'Restricted' Pig MDA",
                                              "Pig vaccine",
                                              "Pig MDA and vaccine", "'Restricted' pig MDA and vaccine",
                                              "'Restricted' pig MDA and restricted vaccine", "CYSTINET",
                                              "Human MDA (praziquantel)", "Human MDA SAC (praziquantel)")
min.fraction.data.out.12mnthslgt2 <- as.table(min.fraction.data.out.12mnthslgt)
min.fraction.data.out.12mnthslgt2

## 24 months min slgt age ~ 1% PCC prev target ##
min.fraction.data.out.24mnthslgt <- matrix(c(pigMDA_minslgt24mo, pigMDAage_minslgt24mo , 
                                             pigvacc_minslgt24mo, pigMDAvacc_minslgt24mo,
                                             pigMDAagevaccine_minslgt24mo, pigMDAagevaccineage_minslgt24mo,
                                             pigcystinet_minslgt24mo,
                                             HumanMDA_minslgt24mo, HumanMDAsac_minslgt24mo), nrow=9, byrow = TRUE)

colnames(min.fraction.data.out.24mnthslgt)<-c(3,4,6,12)
rownames(min.fraction.data.out.24mnthslgt)<-c("Pig MDA", "'Restricted' Pig MDA",
                                              "Pig vaccine",
                                              "Pig MDA and vaccine", "'Restricted' pig MDA and vaccine",
                                              "'Restricted' pig MDA and restricted vaccine", "CYSTINET",
                                              "Human MDA (praziquantel)", "Human MDA SAC (praziquantel)")
min.fraction.data.out.24mnthslgt2 <- as.table(min.fraction.data.out.24mnthslgt)
min.fraction.data.out.24mnthslgt2 

setwd("")

# write.csv(Master_df, "MASTER_minfraction_030220.csv")
# write.table(Master_df, "MASTER_minfraction_030220.txt", sep="\t")
# 
# write.csv(min.fraction.data.out.6mnthslgt2, "minfraction_6mnthslgt_1pcc.csv")
# write.table(min.fraction.data.out.6mnthslgt2, "minfraction_6mnthslgt_1pcc.txt", sep="\t")
# 
# write.csv(min.fraction.data.out.12mnthslgt2, "minfraction_12mnthslgt_1pcc.csv")
# write.table(min.fraction.data.out.12mnthslgt2, "minfraction_12mnthslgt_1pcc.txt", sep="\t")
# 
# write.csv(min.fraction.data.out.24mnthslgt2, "minfraction_24mnthslgt_1pcc.csv")
# write.table(min.fraction.data.out.24mnthslgt2, "minfraction_24mnthslgt_1pcc.txt", sep="\t")