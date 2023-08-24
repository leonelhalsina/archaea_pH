do_mlsearch <- function(phylotree,
                        trait_data,
                        running_this_model,
                        sampling_fraction,
                        intGuessLamba,
                        intGuessMu){
  
  
  
  trait_depend <- strsplit(running_this_model,"_")[[1]][1]
  type_speciation <- strsplit(running_this_model,"_")[[1]][2]
  transition <- strsplit(running_this_model,"_")[[1]][3]
  starting_point <- strsplit(running_this_model,"_")[[1]][4]
  
  extinction_dependence <- FALSE # TRUE or FALSE
  
  
  
  
  traits <- sortingtraits(trait_data, phylotree)
  num_concealed_states <- 4
  
  num_traits<-length(sort(unique(traits)))
  
  if(type_speciation=="speciation1"){
    
    
    idparslist <- id_paramPos(traits, num_concealed_states)
    if(trait_depend=="ETD"){
      to_lambdas<-rep(1:num_traits,num_concealed_states)
    } 
    
    if(trait_depend=="CTD"){
      to_lambdas<-NULL
      for(i in 1:num_traits){
        to_lambdas<-c(to_lambdas,rep(i,num_concealed_states))
      }
      
    }
    
    if(trait_depend=="CR"){
      to_lambdas<-rep(1,num_concealed_states*num_traits)
    } 
    
    idparslist[[1]][] <- to_lambdas
    idparslist[[2]][] <- max(idparslist[[1]]) + 1
    first_idpar_Q<-max(idparslist[[2]]) + 1
    if(extinction_dependence==TRUE){
      idparslist[[2]][] <- to_lambdas
      idparslist[[1]][] <- max(idparslist[[2]]) + 1 
      first_idpar_Q<-max(idparslist[[1]]) + 1
    }
  }
  
  
  
  
  
  
  
  
  
  
  if(type_speciation == "speciation2"){ # pH change and speciation events
    idparslist <- cla_id_paramPos(traits, num_concealed_states)
    if(trait_depend=="CR"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][2,1] <- 1
      full_lambdas[[2]][1,2] <- 1
      full_lambdas[[3]][4,3] <- 1
      full_lambdas[[4]][3,4] <- 1
      
      full_lambdas[[5]][6,5] <- 1
      full_lambdas[[6]][5,6] <- 1
      full_lambdas[[7]][8,7] <- 1
      full_lambdas[[8]][7,8] <- 1
      
      full_lambdas[[9]][10,9] <- 1
      full_lambdas[[10]][9,10] <- 1
      full_lambdas[[11]][12,11] <- 1
      full_lambdas[[12]][11,12] <- 1
      
      full_lambdas[[13]][14,13] <- 1
      full_lambdas[[14]][13,14] <- 1
      full_lambdas[[15]][16,15] <- 1
      full_lambdas[[16]][15,16] <- 1
      
    }
    
    if(trait_depend=="ETD"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][2,1] <- 1
      full_lambdas[[2]][1,2] <- 2
      full_lambdas[[3]][4,3] <- 3
      full_lambdas[[4]][3,4] <- 4
      
      full_lambdas[[5]][6,5] <- 1
      full_lambdas[[6]][5,6] <- 2
      full_lambdas[[7]][8,7] <- 3
      full_lambdas[[8]][7,8] <- 4
      
      full_lambdas[[9]][10,9] <- 1
      full_lambdas[[10]][9,10] <- 2
      full_lambdas[[11]][12,11] <- 3
      full_lambdas[[12]][11,12] <- 4
      
      full_lambdas[[13]][14,13] <- 1
      full_lambdas[[14]][13,14] <- 2
      full_lambdas[[15]][16,15] <- 3
      full_lambdas[[16]][15,16] <- 4
      
    }
    if(trait_depend=="CTD"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][2,1] <- 1
      full_lambdas[[2]][1,2] <- 1
      full_lambdas[[3]][4,3] <- 1
      full_lambdas[[4]][3,4] <- 1
      
      full_lambdas[[5]][6,5] <- 2
      full_lambdas[[6]][5,6] <- 2
      full_lambdas[[7]][8,7] <- 2
      full_lambdas[[8]][7,8] <- 2
      
      full_lambdas[[9]][10,9] <- 3
      full_lambdas[[10]][9,10] <- 3
      full_lambdas[[11]][12,11] <- 3
      full_lambdas[[12]][11,12] <- 3
      
      full_lambdas[[13]][14,13] <- 4
      full_lambdas[[14]][13,14] <- 4
      full_lambdas[[15]][16,15] <- 4
      full_lambdas[[16]][15,16] <- 4
      
    }
    idparslist[[1]]<-full_lambdas
    idparslist[[2]][]<-max(unlist(idparslist[[1]])) + 1 
    first_idpar_Q<-max(unlist(idparslist[[1]])) + 2 
    
  }
  
  if(type_speciation == "speciation3"){ # geographic extand change and speciation events
    idparslist <- cla_id_paramPos(traits, num_concealed_states)
    if(trait_depend=="CR"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][3,1] <- 1
      full_lambdas[[2]][4,2] <- 1
      full_lambdas[[3]][1,3] <- 1
      full_lambdas[[4]][2,4] <- 1
      
      full_lambdas[[5]][7,5] <- 1
      full_lambdas[[6]][8,6] <- 1
      full_lambdas[[7]][5,7] <- 1
      full_lambdas[[8]][6,8] <- 1
      
      full_lambdas[[9]][11,9] <- 1
      full_lambdas[[10]][12,10] <- 1
      full_lambdas[[11]][9,11] <- 1
      full_lambdas[[12]][10,12] <- 1
      
      full_lambdas[[13]][15,13] <- 1
      full_lambdas[[14]][16,14] <- 1
      full_lambdas[[15]][13,15] <- 1
      full_lambdas[[16]][14,16] <- 1
      
    }
    
    if(trait_depend=="ETD"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][3,1] <- 1
      full_lambdas[[2]][4,2] <- 2
      full_lambdas[[3]][1,3] <- 3
      full_lambdas[[4]][2,4] <- 4
      
      full_lambdas[[5]][7,5] <- 1
      full_lambdas[[6]][8,6] <- 2
      full_lambdas[[7]][5,7] <- 3
      full_lambdas[[8]][6,8] <- 4
      
      full_lambdas[[9]][11,9] <- 1
      full_lambdas[[10]][12,10] <- 2
      full_lambdas[[11]][9,11] <- 3
      full_lambdas[[12]][10,12] <- 4
      
      full_lambdas[[13]][15,13] <- 1
      full_lambdas[[14]][16,14] <- 2
      full_lambdas[[15]][13,15] <- 3
      full_lambdas[[16]][14,16] <- 4
      
    }
    if(trait_depend=="CTD"){
      lambd_and_modeSpe <- cla_id_paramPos(traits,num_concealed_states)
      lambd_and_modeSpe[[1]][1,] <- rep(0,16)
      
      full_lambdas<-prepare_full_lambdas(traits,num_concealed_states,lambd_and_modeSpe[[1]])
      full_lambdas[[1]][3,1] <- 1
      full_lambdas[[2]][4,2] <- 1
      full_lambdas[[3]][1,3] <- 1
      full_lambdas[[4]][2,4] <- 1
      
      full_lambdas[[5]][7,5] <- 2
      full_lambdas[[6]][8,6] <- 2
      full_lambdas[[7]][5,7] <- 2
      full_lambdas[[8]][6,8] <- 2
      
      full_lambdas[[9]][11,9] <- 3
      full_lambdas[[10]][12,10] <- 3
      full_lambdas[[11]][9,11] <- 3
      full_lambdas[[12]][10,12] <- 3
      
      full_lambdas[[13]][15,13] <- 4
      full_lambdas[[14]][16,14] <- 4
      full_lambdas[[15]][13,15] <- 4
      full_lambdas[[16]][14,16] <- 4
      
    }
    idparslist[[1]]<-full_lambdas
    idparslist[[2]][]<-max(unlist(idparslist[[1]])) + 1 
    first_idpar_Q<-max(unlist(idparslist[[1]])) + 2 
    
  }
  
  ####
  if(transition=="Mod1"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- first_idpar_Q 
    masterBlock [2,1] <- first_idpar_Q 
    masterBlock [3,4] <- first_idpar_Q 
    masterBlock [4,3] <- first_idpar_Q 
    masterBlock [1,3] <- first_idpar_Q + 1
    masterBlock [3,1] <- first_idpar_Q + 1
    masterBlock [2,4] <- first_idpar_Q + 1
    masterBlock [4,2] <- first_idpar_Q + 1
    
    masterBlock [1,4] <- 0
    masterBlock [4,1] <- 0
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    diag(masterBlock)<-NA
  }  
  
  if(transition=="Mod2"){ ## nulo 1 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T) 
    diag(masterBlock)<-NA
  }
  
  
  
  if(transition=="Mod3"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- 0
    masterBlock [2,1] <- 0
    masterBlock [3,4] <- first_idpar_Q 
    masterBlock [4,3] <- first_idpar_Q 
    masterBlock [1,3] <- first_idpar_Q + 1
    masterBlock [3,1] <- first_idpar_Q + 1
    masterBlock [2,4] <- first_idpar_Q + 1
    masterBlock [4,2] <- first_idpar_Q + 1
    
    masterBlock [1,4] <- 0
    masterBlock [4,1] <- 0
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    diag(masterBlock)<-NA
  }
  if(transition=="Mod4"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- first_idpar_Q 
    masterBlock [2,1] <- first_idpar_Q 
    masterBlock [3,4] <- first_idpar_Q 
    masterBlock [4,3] <- first_idpar_Q 
    masterBlock [1,3] <- 0
    masterBlock [3,1] <- 0
    masterBlock [2,4] <- first_idpar_Q + 1
    masterBlock [4,2] <- first_idpar_Q + 1
    
    masterBlock [1,4] <- 0
    masterBlock [4,1] <- 0
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    diag(masterBlock)<-NA
  }
  
  if(transition=="Mod5"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- first_idpar_Q 
    masterBlock [2,1] <-  first_idpar_Q + 1
    masterBlock [3,4] <- first_idpar_Q
    masterBlock [4,3] <- first_idpar_Q + 1
    masterBlock [1,3] <- first_idpar_Q + 2
    masterBlock [3,1] <- first_idpar_Q + 3
    masterBlock [2,4] <- first_idpar_Q + 2
    masterBlock [4,2] <- first_idpar_Q + 3
    
    masterBlock [1,4] <- 0
    masterBlock [4,1] <- 0
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    diag(masterBlock)<-NA
  }
  if(transition=="Mod6"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- first_idpar_Q 
    masterBlock [2,1] <-  first_idpar_Q + 1
    masterBlock [3,4] <- first_idpar_Q
    masterBlock [4,3] <- first_idpar_Q + 1
    masterBlock [1,3] <- first_idpar_Q + 2
    masterBlock [3,1] <- first_idpar_Q + 3
    masterBlock [2,4] <- first_idpar_Q + 2
    masterBlock [4,2] <- first_idpar_Q + 3
    
    masterBlock [1,4] <- first_idpar_Q
    masterBlock [4,1] <- first_idpar_Q + 1
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    
    
    diag(masterBlock)<-NA
  }
  
  if(transition=="Mod7"){ ## nulo 2 - hoja calc
    masterBlock<-matrix(first_idpar_Q,ncol=4,nrow=4,byrow=T)
    masterBlock [1,2] <- first_idpar_Q 
    masterBlock [2,1] <- first_idpar_Q 
    masterBlock [3,4] <- first_idpar_Q 
    masterBlock [4,3] <- first_idpar_Q 
    masterBlock [1,3] <- first_idpar_Q + 1
    masterBlock [3,1] <- first_idpar_Q + 1
    masterBlock [2,4] <- first_idpar_Q + 1
    masterBlock [4,2] <- first_idpar_Q + 1
    
    masterBlock [1,4] <- first_idpar_Q + 2
    masterBlock [4,1] <- first_idpar_Q + 2
    masterBlock [2,3] <- 0
    masterBlock [3,2] <- 0
    diag(masterBlock)<-NA
  }  
  idparslist[[3]]<-q_doubletrans(traits,masterBlock,diff.conceal=FALSE)
  
  
  if(type_speciation == "speciation1"){
    idparsopt<-sort(unique(as.vector(idparslist[[1]])))
  } else {
    idparsopt<-sort(unique(as.vector(unlist(idparslist[[1]]))))
  }
  
  idparsopt <- idparsopt[idparsopt > 0]
  total_lambdas_opt<-length(idparsopt)
  idparsopt<-c(idparsopt,sort(unique(as.vector(idparslist[[2]]))))
  total_mus_opt<- length(idparsopt) - total_lambdas_opt
  allq<-sort(unique(as.vector(idparslist[[3]])))
  idparsopt<-c(idparsopt,allq[allq > 0])
  total_qs_opt<- length(allq[allq > 0])
  
  cat(" -----Now a bd model to find the starting point----- \n")
  # startingpoint<-bd_ML(brts = ape::branching.times(phylotree))
  # intGuessLamba <- (startingpoint$lambda0)
  # intGuessMu <- (startingpoint$mu0)
  intGuessLamba <- 0.1
  intGuessMu <- 0.05
  cat(" \n")
  cat("------ Now, the secsse inference will start----- \n")
  
  
  if(starting_point=="a"){
    intGuessLamba <-intGuessLamba
    intGuessMu<-intGuessMu
  }
  
  if(starting_point=="b"){
    intGuessLamba <-intGuessLamba * 2
    intGuessMu<-intGuessMu * 2
  }
  if(starting_point=="c"){
    intGuessLamba <-intGuessLamba * 0.5
    intGuessMu<-intGuessMu * 0.5
  }
  
  
  idparsopt
  initparsopt<-c(rep(intGuessLamba,total_lambdas_opt),
                 rep(intGuessMu,total_mus_opt),
                 rep(intGuessLamba/5,total_qs_opt))
  
  
  
  if(length(initparsopt)!=length(idparsopt)){
    stop("something happened when setting parameters")
  }
  
  cat("you are optimizing",total_lambdas_opt,"lambdas,",total_mus_opt,"mus and ",total_qs_opt,"qs. \n")
  idparsfix<-0
  parsfix<-0
  
  methode <- "ode45"
  optimmethod <- "subplex"
  run_parallel <- FALSE
  cond <-"proper_cond"
  root_state_weight <- "proper_weights"
  use_fortran <- TRUE
  
  tol <- c(1e-04, 1e-05, 1e-07)
  maxiter = 1000 * round((1.25)^length(idparsopt))
  
  
  # if(type_speciation=="speciation5"){
  #   idparslist[[1]] <- full_lambdas
  # }
  
  
  
  if(type_speciation=="speciation1"){
    model_output <- secsse_ml(phy=phylotree,
                              traits,
                              num_concealed_states, 
                              idparslist, 
                              idparsopt,
                              initparsopt,
                              idparsfix, 
                              parsfix, 
                              cond,
                              root_state_weight,
                              sampling_fraction,
                              tol,
                              maxiter, use_fortran,
                              methode ,
                              optimmethod ,
                              num_cycles = Inf,
                              run_parallel, 
                              loglik_penalty = 0)
    total_pars <-length(idparsopt) 
    names(total_pars)<-"total_free_parameters"
    
  } else {
    model_output <- cla_secsse_ml(phy=phylotree,
                                  traits,
                                  num_concealed_states, 
                                  idparslist, 
                                  idparsopt,
                                  initparsopt,
                                  idparsfix, 
                                  parsfix, 
                                  cond,
                                  root_state_weight,
                                  sampling_fraction,
                                  tol,
                                  maxiter, use_fortran,
                                  methode ,
                                  optimmethod ,
                                  num_cycles = Inf,
                                  run_parallel, 
                                  loglik_penalty = 0)
    total_pars <-length(idparsopt) 
    names(total_pars)<-"total_free_parameters"
  }
  return(list(model_output=model_output,total_pars))
}

