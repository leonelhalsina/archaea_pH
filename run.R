do_things <- function(i){
  #rm(list = ls())
  library(phytools)
  library(secsse)
  library(ape)
  library(DDD)
  source("controlFunction.R")
  
  cat("entering model", i, "\n")
  ############################# 
  all_models <- list()
  
  
  all_models[[1]]   <-"ETD_speciation1_Mod1_a"
  all_models[[2]]   <-"ETD_speciation1_Mod1_b"
  all_models[[3]]   <-"ETD_speciation1_Mod1_c"
  all_models[[4]]  <-"ETD_speciation1_Mod2_a"
  all_models[[5]]  <-"ETD_speciation1_Mod2_b"
  all_models[[6]]  <-"ETD_speciation1_Mod2_c"
  all_models[[7]]  <-"ETD_speciation1_Mod3_a"
  all_models[[8]]  <-"ETD_speciation1_Mod3_b"
  all_models[[9]]  <-"ETD_speciation1_Mod3_c"
  all_models[[10]]  <-"ETD_speciation1_Mod4_a"
  all_models[[11]]  <-"ETD_speciation1_Mod4_b"
  all_models[[12]]  <-"ETD_speciation1_Mod4_c"
  all_models[[13]]  <-"ETD_speciation1_Mod5_a"
  all_models[[14]]  <-"ETD_speciation1_Mod5_b"
  all_models[[15]]  <-"ETD_speciation1_Mod5_c"
  all_models[[16]]  <-"ETD_speciation1_Mod6_a"
  all_models[[17]]  <-"ETD_speciation1_Mod6_b"
  all_models[[18]]  <-"ETD_speciation1_Mod6_c"
  all_models[[19]]  <-"ETD_speciation1_Mod7_a"
  all_models[[20]]  <-"ETD_speciation1_Mod7_b"
  all_models[[21]]  <-"ETD_speciation1_Mod7_c"
  
  all_models[[22]]   <-"ETD_speciation2_Mod1_a"
  all_models[[23]]   <-"ETD_speciation2_Mod1_b"
  all_models[[24]]   <-"ETD_speciation2_Mod1_c"
  all_models[[25]]  <-"ETD_speciation2_Mod2_a"
  all_models[[26]]  <-"ETD_speciation2_Mod2_b"
  all_models[[27]]  <-"ETD_speciation2_Mod2_c"
  all_models[[28]]  <-"ETD_speciation2_Mod3_a"
  all_models[[29]]  <-"ETD_speciation2_Mod3_b"
  all_models[[30]]  <-"ETD_speciation2_Mod3_c"
  all_models[[31]]  <-"ETD_speciation2_Mod4_a"
  all_models[[32]]  <-"ETD_speciation2_Mod4_b"
  all_models[[33]]  <-"ETD_speciation2_Mod4_c"
  all_models[[34]]  <-"ETD_speciation2_Mod5_a"
  all_models[[35]]  <-"ETD_speciation2_Mod5_b"
  all_models[[36]]  <-"ETD_speciation2_Mod5_c"
  all_models[[37]]  <-"ETD_speciation2_Mod6_a"
  all_models[[38]]  <-"ETD_speciation2_Mod6_b"
  all_models[[39]]  <-"ETD_speciation2_Mod6_c"
  all_models[[40]]  <-"ETD_speciation2_Mod7_a"
  all_models[[41]]  <-"ETD_speciation2_Mod7_b"
  all_models[[42]]  <-"ETD_speciation2_Mod7_c"
  
  all_models[[43]]   <-"ETD_speciation3_Mod1_a"
  all_models[[44]]   <-"ETD_speciation3_Mod1_b"
  all_models[[45]]   <-"ETD_speciation3_Mod1_c"
  all_models[[46]]  <-"ETD_speciation3_Mod2_a"
  all_models[[47]]  <-"ETD_speciation3_Mod2_b"
  all_models[[48]]  <-"ETD_speciation3_Mod2_c"
  all_models[[49]]  <-"ETD_speciation3_Mod3_a"
  all_models[[50]]  <-"ETD_speciation3_Mod3_b"
  all_models[[51]]  <-"ETD_speciation3_Mod3_c"
  all_models[[52]]  <-"ETD_speciation3_Mod4_a"
  all_models[[53]]  <-"ETD_speciation3_Mod4_b"
  all_models[[54]]  <-"ETD_speciation3_Mod4_c"
  all_models[[55]]  <-"ETD_speciation3_Mod5_a"
  all_models[[56]]  <-"ETD_speciation3_Mod5_b"
  all_models[[57]]  <-"ETD_speciation3_Mod5_c"
  all_models[[58]]  <-"ETD_speciation3_Mod6_a"
  all_models[[59]]  <-"ETD_speciation3_Mod6_b"
  all_models[[60]]  <-"ETD_speciation3_Mod6_c"
  all_models[[61]]  <-"ETD_speciation3_Mod7_a"
  all_models[[62]]  <-"ETD_speciation3_Mod7_b"
  all_models[[63]]  <-"ETD_speciation3_Mod7_c"
  
  
  all_models[[64]]   <-"CTD_speciation1_Mod1_a"
  all_models[[65]]   <-"CTD_speciation1_Mod1_b"
  all_models[[66]]   <-"CTD_speciation1_Mod1_c"
  all_models[[67]]  <-"CTD_speciation1_Mod2_a"
  all_models[[68]]  <-"CTD_speciation1_Mod2_b"
  all_models[[69]]  <-"CTD_speciation1_Mod2_c"
  all_models[[70]]  <-"CTD_speciation1_Mod3_a"
  all_models[[71]]  <-"CTD_speciation1_Mod3_b"
  all_models[[72]]  <-"CTD_speciation1_Mod3_c"
  all_models[[73]]  <-"CTD_speciation1_Mod4_a"
  all_models[[74]]  <-"CTD_speciation1_Mod4_b"
  all_models[[75]]  <-"CTD_speciation1_Mod4_c"
  all_models[[76]]  <-"CTD_speciation1_Mod5_a"
  all_models[[77]]  <-"CTD_speciation1_Mod5_b"
  all_models[[78]]  <-"CTD_speciation1_Mod5_c"
  all_models[[79]]  <-"CTD_speciation1_Mod6_a"
  all_models[[80]]  <-"CTD_speciation1_Mod6_b"
  all_models[[81]]  <-"CTD_speciation1_Mod6_c"
  all_models[[82]]  <-"CTD_speciation1_Mod7_a"
  all_models[[83]]  <-"CTD_speciation1_Mod7_b"
  all_models[[84]]  <-"CTD_speciation1_Mod7_c"
  
  all_models[[85]]   <-"CTD_speciation2_Mod1_a"
  all_models[[86]]   <-"CTD_speciation2_Mod1_b"
  all_models[[87]]   <-"CTD_speciation2_Mod1_c"
  all_models[[88]]  <-"CTD_speciation2_Mod2_a"
  all_models[[89]]  <-"CTD_speciation2_Mod2_b"
  all_models[[90]]  <-"CTD_speciation2_Mod2_c"
  all_models[[91]]  <-"CTD_speciation2_Mod3_a"
  all_models[[92]]  <-"CTD_speciation2_Mod3_b"
  all_models[[93]]  <-"CTD_speciation2_Mod3_c"
  all_models[[94]]  <-"CTD_speciation2_Mod4_a"
  all_models[[95]]  <-"CTD_speciation2_Mod4_b"
  all_models[[96]]  <-"CTD_speciation2_Mod4_c"
  all_models[[97]]  <-"CTD_speciation2_Mod5_a"
  all_models[[98]]  <-"CTD_speciation2_Mod5_b"
  all_models[[99]]  <-"CTD_speciation2_Mod5_c"
  all_models[[100]]  <-"CTD_speciation2_Mod6_a"
  all_models[[101]]  <-"CTD_speciation2_Mod6_b"
  all_models[[102]]  <-"CTD_speciation2_Mod6_c"
  all_models[[103]]  <-"CTD_speciation2_Mod7_a"
  all_models[[104]]  <-"CTD_speciation2_Mod7_b"
  all_models[[105]]  <-"CTD_speciation2_Mod7_c"
  
  all_models[[106]]   <-"CTD_speciation3_Mod1_a"
  all_models[[107]]   <-"CTD_speciation3_Mod1_b"
  all_models[[108]]   <-"CTD_speciation3_Mod1_c"
  all_models[[109]]  <-"CTD_speciation3_Mod2_a"
  all_models[[110]]  <-"CTD_speciation3_Mod2_b"
  all_models[[111]]  <-"CTD_speciation3_Mod2_c"
  all_models[[112]]  <-"CTD_speciation3_Mod3_a"
  all_models[[113]]  <-"CTD_speciation3_Mod3_b"
  all_models[[114]]  <-"CTD_speciation3_Mod3_c"
  all_models[[115]]  <-"CTD_speciation3_Mod4_a"
  all_models[[116]]  <-"CTD_speciation3_Mod4_b"
  all_models[[117]]  <-"CTD_speciation3_Mod4_c"
  all_models[[118]]  <-"CTD_speciation3_Mod5_a"
  all_models[[119]]  <-"CTD_speciation3_Mod5_b"
  all_models[[120]]  <-"CTD_speciation3_Mod5_c"
  all_models[[121]]  <-"CTD_speciation3_Mod6_a"
  all_models[[122]]  <-"CTD_speciation3_Mod6_b"
  all_models[[123]]  <-"CTD_speciation3_Mod6_c"
  all_models[[124]]  <-"CTD_speciation3_Mod7_a"
  all_models[[125]]  <-"CTD_speciation3_Mod7_b"
  all_models[[126]]  <-"CTD_speciation3_Mod7_c"
  
  
  #all_models[[127]]  <-"CR_speciation1_Mod2_c"
  sampling_fraction <- c(1,1,1,1)
  running_this_model <- all_models[[i]]
  
  phylotree <- read.nexus("RidgeRace370_rate.nwk")
  
  scale <- 950
  phylotree$edge.length <-
    phylotree$edge.length/max(nodeHeights(phylotree)[,2]) * scale
  
  phylotree <- force.ultrametric(phylotree)
  phylotree$edge.length[which(phylotree$edge.length==0)] <- 0.0000000001
  
  
  
  intGuessLamba <- 0.1
  intGuessMu <- 0.05
  
  trait_data <- read.csv("data_analysis.csv")
  trait_data <- trait_data[,c(1,5)]
  cat("running your model:",running_this_model,"___\n")       
  output_model <- do_mlsearch(phylotree,
                              trait_data,
                              running_this_model = running_this_model,
                              sampling_fraction,
                              intGuessLamba,
                              intGuessMu)
  
  saveRDS(output_model,
          file = paste0(running_this_model,".RDS"))   
}
#sub<-readRDS(paste0(running_this_model,".RDS"))

