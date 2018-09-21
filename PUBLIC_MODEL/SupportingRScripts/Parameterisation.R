 
  
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
      RunModel =  .C("MODEL_ERROR",
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
                     as.integer(TimeToSave[,1]),
                     result = double(length(1)))
      

      
    }
    RunModel[["result"]]
  }
  
  
  #Temp = round(runif(1,1,20))
  #TMAX = round(runif(1,1,20))
  #print("Running...")
  #print(paste("Temp set at",Temp,"and tmax set at",TMAX))
  
  
  ################################################
  #~~~~~~~~~~ Parameterisation here ~~~~~~~~~~~~#
  ################################################
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIMUALTED ANNEALING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  SANNr = function()
  {
    RunSANN <- GenSA( par = XXX, lower = lb, upper = ub, fn = A, control = list(maxit = 1e8,temperature = Temp  ) )
    print(RunSANN)
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ NELDER-MEAD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  
  NMr = function()
  {
    RunNM <- constrOptim(XXX ,A,ui=ui,
                         ci=ci,method="Nelder-Mead",
                         control= list(maxit = 1e8,hessian=T,trace=T,reltol=1e-60,tmax=1,temperature=100
                         ))
  }
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ALTERNATIVE SIMUALTED ANNEALING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  AltSANN = function()
  {
    
    
    RunSANN2<- constrOptim(XXX ,A,ui=ui,
                           ci=ci,method="SANN",
                           control= list(maxit = 1e8,hessian=T,trace=T,reltol=1e-60,tmax=TMAX,temperature=Temp
                           ))
    print(RunSANN2)
    
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ RANDOM WALK ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  
  RANDr = function(iter = 1e8)
  {
    for(i in 1:iter)
    {
      XX = X
      # random parameters
      for(para in c(1:6,8:14,27,28))
      {
        #XX[para] = runif(1,ci[2*para-1],-ci[2*para])
        
        if(X[para]>0) XX[para] = runif(1, X[para] -0.021*X[para], X[para] + 0.0251*X[para]) else
          XX[para] = runif(1, X[para] +0.0251*X[para], X[para] - 0.0251*X[para]) 
        
      }
      A(XX)
      
    }
  }
  
  
  rm(ABUNDANCEFIT, AbundanceFittingData,ABUNDANCEFITTINGDATA, ALLFITTINGDATA,
     DF,FittedParameters,FittingData,FittingData2)
  
   
