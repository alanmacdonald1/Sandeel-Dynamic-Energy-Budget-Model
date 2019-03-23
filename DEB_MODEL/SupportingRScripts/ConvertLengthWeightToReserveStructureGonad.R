#~~ Reserve dynamics model


# Initial conditions

DF_Sdryweight = alpha*DF_LENGTH^Length_structure_exponent

DF_Rdryweight= ((1-gamma)*DF_WEIGHT - DF_Sdryweight) / (1-a*b)

DF_R = DF_Rdryweight*(Ef*a + Ep*(1-a)+ Ep*ashx - Ep*gamma*ashy +(Ep*ashy*a*b*(DF_Rdryweight+DF_Sdryweight))/DF_WEIGHT)

ReserveEnergyDensity = (Ef*a + Ep*(1-a)+ Ep*ashx - Ep*gamma*ashy +(Ep*ashy*a*b*(DF_Rdryweight+DF_Sdryweight))/DF_WEIGHT)

DF_S= DF_Sdryweight*( Ep + Ep*ashx - gamma*Ep*ashy)

DF_G = rep(0,length(DF_R))

Rdw = rep( ((1-a*b) / (1-gamma) ) ,length(DF_R))

Er = rep(mean(ReserveEnergyDensity),length(DF_R))

# Initial conditions

if(Parameterisation!="F" | Parameterisation =="T"){

DF_Sdryweight_MEAN = alpha*MEAN_WL_LENGTH^Length_structure_exponent

DF_Rdryweight_MEAN = ((1-gamma)*MEAN_WL_WEIGHT - DF_Sdryweight_MEAN) / (1-a*b)

ReserveEnergy_MEAN = DF_Rdryweight_MEAN*(Ef*a + Ep*(1-a)+ Ep*ashx - Ep*gamma*ashy +(Ep*ashy*a*b*(DF_Rdryweight_MEAN+DF_Sdryweight_MEAN))/MEAN_WL_WEIGHT)

StructuralEnergy_MEAN= DF_Sdryweight_MEAN*( Ep + Ep*ashx - gamma*Ep*ashy)

MEAN_WL_RS = ReserveEnergy_MEAN/StructuralEnergy_MEAN

rm(StructuralEnergy_MEAN,ReserveEnergy_MEAN)
rm(DF_Rdryweight,DF_Rdryweight_MEAN,DF_Sdryweight_MEAN) 
}

# remove some stuff

rm(a,alpha,ashx,ashy,b)  
rm(DF_Sdryweight)
rm(Ef,Ep,gamma,hrs.daylight,Length_structure_exponent)
rm(ReserveEnergyDensity)

 
 