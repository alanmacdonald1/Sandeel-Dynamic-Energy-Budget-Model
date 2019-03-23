 
  
  ################################################
  #~~~~~~~~~~ Parameterisation here ~~~~~~~~~~~~#
  ################################################
  
  
  #~~~~~~~~~~ SIMULATED ANNEALING ~~~~~~~~~~~~~~#
  
  no.para = Number.of.fitted.parameters
  Identity = diag(no.para)
  ui = NULL
  for (i in 1:no.para)
  {
    ui = rbind(ui , rbind(Identity[i,],-1 * Identity[i,]))
  }
  rm(Identity,i)
  ########################
  
  # Choose conditions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # THIS FORCES aL > aS
  ui = rbind(ui,c(1 ,-1, rep(0,26)))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  ######### ci ###########
  
 Bounds = as.matrix(data.frame(FittedParameters$LowerBound,-FittedParameters$UpperBound))
  ci =  as.vector(t(Bounds))
  ci = c(ci,0)
 
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~  GENERALISED SIMULATED ANNEALING ~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # THIS IS MORE EFFECTIVE THAN OPTIMs INBUILT SANN FUNCTION
  


  # lower bound
  lb <- FittedParameters$LowerBound
  
  
  
  # upper bound
  ub <- FittedParameters$UpperBound
  
  
  rm(Bounds)
  
  ###############################################
  ###############################################
  
  # Compute model error...
  Parameterise= function(XXX)
  {
    {
      RunModel =  .C("MODEL_PARAMETERISATION",
                     as.double(XXX),
                     as.double(TT_FEED),
                     as.double(TT_OV),
                     as.double(ACT.MET),
                     as.double(ACT.CONS),
                     as.integer(JulianDayV),
                     as.double(ABUNDANCE_DAILY),
                     as.integer(Age),
                     as.double(FL),
                     as.double(FS),
                     as.double(DF_R),
                     as.double(DF_S),
                     as.double(DF_G),
                     as.double(Rdw),
                     as.double(Er),
                     as.integer(DF_STATE),
                     as.integer(DF_DATE_ADDED),
                     as.integer(DF_DATE_FINISH) ,
                     as.double(DF_LENGTH),
                     as.double(DF_WEIGHT),
                     # WEIGHT, LENGTH AT SURVEY
                     as.integer(WL_DATE_CHECK),
                     as.double(MEAN_WL_LENGTH),
                     as.double(MEAN_WL_WEIGHT),
                     as.double(MEAN_WL_RS),
                     as.integer(MEAN_WL_DATE_ADDED),
                     as.integer(MEAN_WL_DRUN),
                     as.integer(MEAN_WL_AGE),
                     # ABUNDANCE AT SURVEY
                     as.integer(ABUNDANCE_DATE_CHECK),
                     as.double(ABUNDANCE_FIT),
                     as.integer(ABUNDANCE_DATE_ADDED),
                     as.integer(ABUNDANCE_DRUN),
                     as.integer(ABUNDANCE_AGE),
                     result = double(length(1)))
      
      # SAVE TO FILE
      if( ((proc.time() - ptm)[3]/60)/60  > 9){
      write.table(t(c(RunModel[["result"]],XXX)),
                  file="/Users/Alan/Desktop/DEBpara.txt",
                  append=TRUE,eol="\r",sep=",",
       col.names = FALSE,row.names = FALSE)
      }
      
    }
    RunModel[["result"]]
  }
  

  
  
  ################################################
  #~~~~~~~~~~ Parameterisation here ~~~~~~~~~~~~#
  ################################################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIMUALTED ANNEALING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  SANNr = function(XXX)
  {
    RunSANN <- GenSA( par = XXX, lower = lb, upper = ub, fn = Parameterise, control = list(maxit = 1e8,temperature = 7) )
    print(RunSANN)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ NELDER-MEAD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  
  NMr = function(XXX)
  {
    RunNM <- constrOptim( XXX ,f = Parameterise,ui=ui,
                          ci=ci,method="Nelder-Mead",
                          control= list(maxit = 1e8,hessian=T,trace=T,reltol=1e-60,tmax=1,temperature=100
                          ))
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ALTERNATIVE SIMUALTED ANNEALING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  AltSANN = function(XXX)
  {
    
    
    RunSANN2<- constrOptim(XXX ,Parameterise,ui=ui,
                           ci=ci,method="SANN",
                           control= list(maxit = 1e8,hessian=T,trace=T,reltol=1e-60,tmax=10,temperature=10
                           ))
    print(RunSANN2)
    
  }
  
  ## An then adding the hybrid
  # set.seed(2)
  library(pso)
  # 
   # o3 <- psoptim(par = X,fn = Parameterise,lower = lb,upper = ub,
   #               control=list(abstol=1e-8,trace=1,REPORT=1,
   #                              trace.stats=TRUE))
   # set.seed(2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RANDOM WALK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  RANDr = function(iter = 1e8)
  {
    for(i in 1:iter)
    {
      XX = X
      # random parameters
      for(para in 1:length(XX))
      {
        XX[para] = runif(1,ci[2*para-1],-ci[2*para])
      }
      Parameterise(XXX = XX)
      
    }
  }
  
  rm(ABUNDANCEFIT, AbundanceFittingData,ABUNDANCEFITTINGDATA, ALLFITTINGDATA,
     DF,FittedParameters,FittingData,FittingData2)
  
   
