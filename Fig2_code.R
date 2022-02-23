#======================================================================================================================#
#                  PLOT 2 : MIN AGE OF SLAUGHTER ~ ENDEMIC PREVALENCE                                                  #
#======================================================================================================================#

library(epicyst)
require(ggplot2)
require(dplyr)

#==================================#
#     Set up baseline parameters   #

a<-1/(15*12)
b<-1/12 - a
c <- 1/b
slgep <- c/12 # year for epicyst input

slg_vec <- rep(slgep,36)
slg_vec2 <- c(1, slg_vec)
test$slgep <- slg_vec2 

lep_vec <- rep(15, 36)
lep_vec2 <- c(1, lep_vec)
test$lep <- lep_vec2


S1<-Set_up(psi=psi_input, Slaughter_age_min = 5,
           PCPrev_true = 0.333975, CPrev_true = 0.0611, TPrev_true= 0.03055,
           LEP = 15, SlgEP=slgep, number_age_classes_pig = 150,
           PPS=1000, HPS=12000) #gives an endemic prec of (PCC = 0.25, HTT= 0.03, HCC= 0.06)

tau_baseline <- S1[[1]]$tau # find tau 
theta_baseline <- S1[[1]]$theta # find theta
beta_baseline <- S1[[1]]$beta # find beta

#==========================================================================#
#  Set up dataframe to test min slaughter age ~ endemic prevalence output  #

Min_slaughter_age <- c(0:36) # vector of min slaughter age to record endemic prev over

test <- as.data.frame(Min_slaughter_age)
test$ID <- seq_along(test[,1])
colnames(test)[which(names(test) == "Var1")] <- "Min_slaughter_age"
test <- test[,c(which(colnames(test)=="ID"),which(colnames(test)!="ID"))] # make ID first col

# ======== #
# run loop #

pb <- winProgressBar(title="Endemic prev for min slgt age run", label="0% done", min=0, max=100, initial=0)

loop_run_func2 <- function(test){
  for (i in 1:nrow(test)){
    
    Sys.sleep(0.1)
    info <- sprintf("%d%% done", round((i/nrow(test))*100))
    setWinProgressBar(pb, i/(nrow(test))*100, label=info)
    #print(i)
    
    S1 <- set_up(slaughter_age_min = test$Min_slaughter_age[i], LEP = test$lep[i], SlgEP = test$slgep[i], number_age_classes_pig = 150,
               PCPrev = 0.333975, CPrev = 0.0611, TPrev = 0.03055, HPS = 12000, PPS = 1000, 
               tau_input = tau_baseline, theta_input = theta_baseline, beta_input = beta_baseline,
               psi = psi_input)
    
    m1 <- run_model(time = 11, burn_in = 50, params = S1[[1]], initial_states = S1[[2]]) 
    
    final_row <- m1[3961,]
    
    #=============================#
    #   extract endemic prev      #
    
    ######################
    test$pcc_prev[i] <- tail(m1$Pig_Cysticercosis_prev,1)
    test$hcc_prev[i] <- tail(m1$Human_Cysticercosis_prev,1)
    test$htt_prev[i] <- tail(m1$Human_Taeniasis_prev,1)
    
    #===============================================#
    #   transmission parameters to track/check      #
    
    # test$beta[i] <- S1[[1]]$beta
    # test$theta[i] <- S1[[1]]$theta
    # test$tau[i] <- S1[[1]]$tau
  }
  return(test) 
}

endemicprev_run_out <- loop_run_func2(test = test) 

close(pb) # closing the Progress Bar

# =================================================================#
# extract endemic prevalences (for each min slaughter age) to plot #

pcc_vars <- c("ID", "Min_slaughter_age", "pcc_prev")
hcc_vars <- c("ID", "Min_slaughter_age", "hcc_prev")
htt_vars <- c("ID", "Min_slaughter_age", "htt_prev")

subdata1 <- endemicprev_run_out[pcc_vars]
subdata2 <- endemicprev_run_out[hcc_vars]
subdata3 <- endemicprev_run_out[htt_vars]

subdata1$indicator <- rep(as.factor("pcc prev"))
subdata2$indicator <- rep(as.factor("hcc prev"))
subdata3$indicator <- rep(as.factor("htt prev"))

colnames(subdata1)[colnames(subdata1)=="pcc_prev"] <- "prev"
colnames(subdata2)[colnames(subdata2)=="hcc_prev"] <- "prev"
colnames(subdata3)[colnames(subdata3)=="htt_prev"] <- "prev"

data <- rbind(subdata1, subdata2, subdata3)

#=================#
#  make plot 2    #

Figure_2 <- ggplot() +
  geom_line(data=data, aes(x=Min_slaughter_age, y=prev, colour=indicator), size=1.3)+
  scale_x_continuous(breaks=seq(0, 36, 2), limits=c(0, 36))+
  scale_y_continuous(breaks= seq(0, 1, 0.1), limits=c(0,1))+
  geom_vline(xintercept=5, linetype="dashed", 
             color = "purple", size=1.25, alpha=0.25)+
  labs(x="Minimum age from which pigs slaughtered (months)", y="Endemic equilibrium 
prevalence")+
  theme_bw() +
  theme(aspect.ratio = 0.5)+
  theme(panel.border = element_blank(), 
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_text(size = 18, hjust=0.5),
        axis.title.x = element_text(size = 18, face= "bold"),
        axis.title.y = element_text(size = 18, angle = 90, face= "bold"),
        legend.position = "bottom",
        legend.title=element_text(size=16), 
        legend.text=element_text(size=14))
