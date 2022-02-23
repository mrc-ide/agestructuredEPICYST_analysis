# ========================================================================================================================================================= #
# SCRIPT FOR RUNNING SIMULATION LOOPS (different intervention frequency, coverage and min slaughter age) to measure time to attain PCC prevalence targets   #

# ================== #
devtools::load_all() 

#=========================================================================================================================#
#                       Set-up dataframe for simulations                                                                  #
#=========================================================================================================================#

#===========================================================================================================#
# Vector for different combinations of frequency, coverage, minimum slaughter age & other fixed parameters  #

Coverage <- seq(from=0.495,to=1.0,by=0.005) # coverage range to test 
Min_slaughter_age <- c(5:24) # minimum slaughter age range to test 
Freq <- c(3) # if only want to test frequency of 3 months
Freq <- c(3,4,6,12) # 3, 4, 6, 12 month frequency to test 

# =================================================================#
# make dataframe with all combinations of parameters               #
test <- as.data.frame(expand.grid(Coverage, Min_slaughter_age, Freq))
test$ID <- seq_along(test[,1])
colnames(test)[which(names(test) == "Var1")] <- "Cov"
colnames(test)[which(names(test) == "Var2")] <- "Min_slaughter_age"
colnames(test)[which(names(test) == "Var3")] <- "Freq"

# ================================================================#
#    Set up other parameters for baseline model                   #

# set-up life expectancy once pigs reach slaughter age  & natural life expectancy # 
a <- 1/(15*12)
b <- 1/12 - a
c <- 1/b
slgep <- c/12 # year for epicyst input
slg_vec <- rep(slgep,8160) # vector to add to dataframe

lep_vec <- rep(15, 8160)


psi_input <- 1/1.5 # rate of progression from pre-patent to infected pigs

test$slgep <- slg_vec #add to table to specify for each model run 
test$lep <- lep_vec


# extract baseline tau to speciy for model run (solved at endemic eq given other input parameters and input prevalences which give 25% PCC, 6% HCC and 3% HTT) # 
S1 <- Set_up(psi = psi_input, Slaughter_age_min = 5,
           PCPrev_true = 0.333975, CPrev_true = 0.0611, TPrev_true = 0.03055,
           LEP = 15, SlgEP = slgep, number_age_classes_pig = 150,
           PPS = 1000, HPS = 12000)# gives an endemic prec of (PCC = 0.25, HTT= 0.03, HCC= 0.06)

tau_baseline <- S1[[1]]$tau# 

test$MDA_to_age <- test$Min_slaughter_age+1 # upper age limit for pig MDA to non-slaughter age pigs

#======================================================================================================================#
#        Run intervention simulations with different frequency, coverage and min slaughter age combinations            #                                                                                          #
# =====================================================================================================================#
require(dplyr)

pb <- winProgressBar(title = "Intervention simulation progress bar", label = "0% done", min  =0, max = 100, initial = 0)

loop_run_func <- function(test, specify_intervention){
  for (i in 1:nrow(test)){
    
    Sys.sleep(0.1)
    
    info <- sprintf("%d%% done", round((i / nrow(test)) * 100))
    
    setWinProgressBar(pb, i/(nrow(test)) * 100, label = info)
    
    S1 <- Set_up(Slaughter_age_min = test$Min_slaughter_age[i], LEP = test$lep[i], SlgEP = test$slgep[i], 
                 number_age_classes_pig = 150, 
                 PCPrev_true = 0.3064, CPrev_true = 0.06225, TPrev_true = 0.03125, HPS = 12000, PPS = 1000,
                 tau_input = tau_baseline, psi = psi_input) 
    
    if(specify_intervention == "Pig_MDA") {
    
    I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = 'Pig_MDA', Intervention_time = 1, 
                      Intervention_frequency = test$Freq[i], pig_MDA_cov = test$Cov[i], age_target_pig_MDA = c(4:150),
                      Pig_MDA_prop_noimmunity = 1.0,
                      Params = S1[[1]], Initial_states = S1[[2]]) # run model with intervention freq [i]
    }
    
    if(specify_intervention == "Pig_vaccine") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = 'Pig_vaccine', Intervention_time = 1, 
                        Intervention_frequency = test$Freq[i], pig_vaccine_ds1_cov  = test$Cov[i], pig_vaccine_ds2_cov = test$Cov[i],
                        age_target_pig_vaccine = c(4:150),
                        Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Pig_MDA_age") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = 'Pig_MDA', Intervention_time=1, 
                        Intervention_frequency = test$Freq[i], pig_MDA_cov = test$Cov[i], 
                        age_target_pig_MDA = c(4:test$MDA_to_age[i]),
                        Pig_MDA_prop_noimmunity = 1.0,
                        Params=S1[[1]], Initial_states=S1[[2]])
    }

    if(specify_intervention == "Pig_MDA_vaccine") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = c('Pig_MDA','Pig_vaccine'), Intervention_time = 1, 
                        Intervention_frequency = test$Freq[i], 
                        pig_MDA_cov = test$Cov[i], 
                        age_target_pig_MDA = c(4:150), Pig_MDA_prop_noimmunity = 1.0,
                        pig_vaccine_ds1_cov  = test$Cov[i], pig_vaccine_ds2_cov =test$Cov[i], 
                        age_target_pig_vaccine = c(4:150),
                        Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Pig_MDAage_vaccine") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = c('Pig_MDA','Pig_vaccine'), Intervention_time = 1, 
                         Intervention_frequency = test$Freq[i], 
                         pig_MDA_cov = test$Cov[i], 
                         age_target_pig_MDA = c(4:test$MDA_to_age[i]), Pig_MDA_prop_noimmunity = 1.0,
                         pig_vaccine_ds1_cov  = test$Cov[i], pig_vaccine_ds2_cov = test$Cov[i], 
                         age_target_pig_vaccine = c(4:150),
                         Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Pig_MDAage_vaccineage") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention=c('Pig_MDA','Pig_vaccine'), Intervention_time = 1, 
                         Intervention_frequency = test$Freq[i], 
                         pig_MDA_cov = test$Cov[i], 
                         age_target_pig_MDA = c(4:test$MDA_to_age[i]), Pig_MDA_prop_noimmunity = 1.0,
                         pig_vaccine_ds1_cov  = test$Cov[i], pig_vaccine_ds2_cov = test$Cov[i], 
                         age_target_pig_vaccine = c(4:test$MDA_to_age[i]),
                         Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Human_MDA") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention ='Human_MDA_pzq', Intervention_time = 1, 
                        Intervention_frequency = test$Freq[i], 
                        human_MDApzq_cov = test$Cov[i], 
                        age_target_human_MDA = c(2:7),
                        Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Human_MDAsac") {
      
      I_run <- Run_model(Time = 11, Burn_in = 50, Intervention = 'Human_MDA_pzq', Intervention_time = 1, 
                        Intervention_frequency = test$Freq[i], 
                        human_MDApzq_cov = test$Cov[i], 
                        age_target_human_MDA = c(2:3),
                        Params = S1[[1]], Initial_states = S1[[2]])
    }
    
    if(specify_intervention == "Pig_Cystinet") {
      
      I_run <- run_model(Time=11, Burn_in=50, Intervention_stage1 = c('Pig_MDA','Pig_vaccine'), 
                         Intervention_stage2 = c('Pig_MDA','Pig_vaccine'),
                         Intervention_time_stage1 =1, Intervention_frequency_stage1 = 3, Num_intervention_rounds_stage1 = 1,
                         Intervention_time_stage2 =1.25, Intervention_frequency_stage2 = test$Freq[i],
                         pig_MDA_cov_stage1 = test$Cov[i],  pig_MDA_cov_stage2 = test$Cov[i],
                         pig_vaccine_ds1_cov_stage2 = test$Cov[i], pig_vaccine_ds2_cov_stage2 = test$Cov[i],pig_vaccine_ds1_cov_stage1 = test$Cov[i], pig_vaccine_ds2_cov_stage1 = test$Cov[i],
                         age_target_pig_MDA_stage1 = c(4:150), age_target_pig_MDA_stage2 = c(4:7), Pig_MDA_prop_noimmunity = 1.0,
                         age_target_pig_vaccine_stage1 = c(4:150), age_target_pig_vaccine_stage2 = c(4:7),
                         Params=S1[[1]], Initial_states=S1[[2]])
      }
    
    
    I_run$t_round <- round(I_run$t, digit=2) # round time
    
    # set up the vector of times (measurments before each new intervention round) dependent on freq of intervention # 
    if(test$Freq[i]==2){
      freq_vec   <- seq(13.97, 312, 2)
    } else if (test$Freq[i]==3){
      freq_vec   <- seq(14.97, 312, 3)
    } else if (test$Freq[i]==4){
      freq_vec   <- seq(15.97, 312, 4)
    }  else if (test$Freq[i]==5){
      freq_vec   <- seq(16.97, 312, 5)
    }  else if (test$Freq[i]==6){
      freq_vec   <- seq(17.97, 312, 6)  
    } else if (test$Freq[i]==7){
      freq_vec   <- seq(18.97, 312, 7)  
    } else if (test$Freq[i]==8){
      freq_vec   <- seq(19.97, 312, 8)  
    } else if (test$Freq[i]==9){
      freq_vec   <- seq(20.97, 312, 9)  
    } else if (test$Freq[i]==10){
      freq_vec   <- seq(21.97, 312, 10)
    } else if (test$Freq[i]==11){
      freq_vec   <- seq(22.97, 312, 11)
    } else if (test$Freq[i]==12){
      freq_vec   <- seq(23.97, 312, 12)
    } else if (test$Freq[i]==13){
      freq_vec   <- seq(24.97, 312, 13)
    } else if (test$Freq[i]==14){
      freq_vec   <- seq(25.97, 312, 14)
    } else if (test$Freq[i]==15){
      freq_vec   <- seq(26.97, 312, 15)
    } else if (test$Freq[i]==16){
      freq_vec   <- seq(27.97, 312, 16)
    } else if (test$Freq[i]==17){
      freq_vec   <- seq(28.97, 312, 17)
    } else if (test$Freq[i]==18){
      freq_vec   <- seq(29.97, 312, 18)
    } else if (test$Freq[i]==19){
      freq_vec   <- seq(30.97, 312, 19)
    } else if (test$Freq[i]==20){
      freq_vec   <- seq(31.97, 312, 20) 
    } else if (test$Freq[i]==21){
      freq_vec   <- seq(32.97, 312, 21)
    } else if (test$Freq[i]==22){
      freq_vec   <- seq(33.97, 312, 22)
    } else if (test$Freq[i]==23){
      freq_vec   <- seq(34.97, 312, 23)
    } else {
      freq_vec   <- seq(35.97, 312, 24) 
    }
    
    
    I1_cleaned <- I_run[I_run$t_round %in% freq_vec, ] # extract all rows where 3 (or x ) months post-intervention round depending on frquency
    
    
    #====================================================================#
    #       find time to reach each prevalence value (each run) & store  #
    
    #===============#
    ##### PCC #######
    #===============#
    I1_subset_pcc1 <- subset(I1_cleaned, Pig_Cysticercosis_prev3 <= 0.01) # 1% prevalence target
    I1_subset_pcc2 <- subset(I1_cleaned, Pig_Cysticercosis_prev3 <= 0.05) # 5% prevalence target
    I1_subset_pcc3 <- subset(I1_cleaned, Pig_Cysticercosis_prev3 <= 0.1) # 10% prevalence target
    
    #=====================#
    #   PCC prev < 1%     #
    
    head_res_pcc1 <- c()
    ######################
    if(dim(I1_subset_pcc1)[1] == 0) {
      
      head_res_pcc1 <- NA # if 0 observations where <1% 
      
    } else {
      
      head_res_pcc1 <- head(I1_subset_pcc1,1)# select first instance where this occurs (pig cysticercosis < 1 %)
    }
    
    ############################
    if(dim(I1_subset_pcc1)[1] == 0) {
      
      test$time_to_threshold_pcc1[i] <- NA # if 0 observations where <1% 
      
    } else {
      
      test$time_to_threshold_pcc1[i] <- head_res_pcc1$t[[1]] # select first instance where this occurs (pig cysticercosis < 1 %)
      
    }
    
    #=====================#
    #   PCC prev < 5%     #
    
    head_res_pcc2 <- c()
    ######################
    if(dim(I1_subset_pcc2)[1] == 0) {
      
      head_res_pcc2 <- NA # if 0 observations where <1% 
      
    } else {
      
      head_res_pcc2 <- head(I1_subset_pcc2,1)# select first instance where this occurs (pig cysticercosis < 1 %)
    }
    
    ############################
    if(dim(I1_subset_pcc2)[1] == 0) {
      
      test$time_to_threshold_pcc5[i] <- NA # if 0 observations where <1% 
      
    } else {
      
      test$time_to_threshold_pcc5[i] <- head_res_pcc2$t[[1]] # select first instance where this occurs (pig cysticercosis < 1 %)
      
    }
    
    #=====================#
    #   PCC prev < 10%     #
    
    head_res_pcc3 <- c()
    ######################
    if(dim(I1_subset_pcc3)[1] == 0) {
      
      head_res_pcc3 <- NA # if 0 observations where <1% 
      
    } else {
      
      head_res_pcc3 <- head(I1_subset_pcc3,1)# select first instance where this occurs (pig cysticercosis < 1 %)
    }
    
    ############################
    if(dim(I1_subset_pcc3)[1] == 0) {
      
      test$time_to_threshold_pcc10[i] <- NA # if 0 observations where <1% 
      
    } else {
      
      test$time_to_threshold_pcc10[i] <- head_res_pcc3$t[[1]] # select first instance where this occurs (pig cysticercosis < 1 %)
      
    }
    
    #===============#
    ##### HTT #######
    #===============#
    
    I1_subset_htt_avg_elim <- subset(I1_cleaned, Humans_Taeniasis <= 1)
    
    
    head_res_htt_elim <- c()
    ######################
    if(dim(I1_subset_htt_avg_elim)[1] == 0) {
      
      head_res_htt_elim <- NA # if 0 observations where <1% 
      
    } else {
      
      head_res_htt_elim <- head(I1_subset_htt_avg_elim,1) # select first instance where this occurs (pig cysticercosis < 1 %)
    }
    
    ############################
    if(dim(I1_subset_htt_avg_elim)[1] == 0) {
      
      test$time_to_elim_htt[i] <- NA # if 0 observations where <1% 
      
    } else {
      
      test$time_to_elim_htt[i] <- head_res_htt_elim$t[[1]] # select first instance where this occurs (pig cysticercosis < 1 %)
      
    }
    
    
  }
  return(test) 
}

#==========================================================================================================#
#   Run loop - note this will take 24 - 72 hours if run all combinations (~ 8,000 rows in test dataframe)  #


start_time <- Sys.time() # this will time the run


# select one of the runs below (and hash the rest) #
Int_run_out1 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_MDA")) # supress warnings when run
#Int_run_out2 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_vaccine")) 
#Int_run_out3 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_MDAage"))
#Int_run_out4 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_MDA_vaccine"))
#Int_run_out5 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_MDAage_vaccine"))
#Int_run_out6 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_MDAage_vaccineage"))
#Int_run_out7 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Human_MDA"))
#Int_run_out8 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Human_MDAsac"))
#Int_run_out9 <- suppressWarnings(loop_run_func(test = test, specify_intervention = "Pig_Cystinet"))

close(pb) #Closing the Progress Bar

end_time <- Sys.time() 

start_time - end_time # 2.214848 days difference

