/*
#=======================================#
   FUNCTION TO MINIMISE MODEL ERROR
#=======================================#
*/

#include <R.h>
#include <Rmath.h>


#include<stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>

#include "CONSTANTS.h"
#include "FUNCTIONS.h"
	 

double MODEL_RUN(double X[NoParameters] ,
		   double TT_FEED[NoDays] ,
		   double TT_OV[NoDays] ,
		   double ACT_MET[NoDays],
		   double ACT_CONS[NoDays],
		   int JulianDayV[NoDays],
		   double ABUNDANCE_DAILY[IndNum],
		   int Age[IndNum],
		   double FL[NoDays] ,
		   double FS[NoDays],
		   double DF_R[IndNum] ,
		   double DF_S[IndNum],
		   double DF_G[IndNum],
		   double Rdw2[IndNum],
		   double Er2[IndNum], 
		   int DF_STATE[IndNum] ,
		   int DF_DATE_ADDED[IndNum],
		   int DF_DATE_FINISH[IndNum],
		   double DF_LENGTH[IndNum] ,
		   double DF_WEIGHT[IndNum],
		   int TimeToSave[NumberOfDaysToSave],
		   double *result)
{
  
  
  

/*~~~~~~~~~~~ SET MODEL ERROR TO ZERO (LEAST SQUARE ERROR) ... ~~~~~~~~~~~*/
      


if(X[0] <= X[1]){*result = *result + 1E10 ; }

if( X[0] > X[1])  // parameter constraints
{



double aL = X[0], aS = X[1] , tau = X[2], OVTHRESH1 = X[3],OVTHRESH2 = X[4], sigma1 = X[5], sigma2 =  X[6], rho0 = X[7] , rhow = X[8], S1 = X[9], S2 = X[10], OVTHRESH3 = X[11] , G1 = X[12] , G2 = X[13], MULTIPLIER1 = X[14] , MULTIPLIER2= X[15] ,  MULTIPLIER3 = X[16] ,  MULTIPLIER4 = X[17] , MULTIPLIER5 = X[18] , MULTIPLIER6 = X[19] , mu1 = X[20] , mu2 = X[21] , mu3 = X[22] , mu4 = X[23] , mu5 = X[24] , mu6 = X[25] , B = X[26] , Eb = X[27] ;


  
/*~~~~~~~~~~~ ENVIRONMENTAL DATA VECTORS ~~~~~~~~~~~~*/
   
 
double CopepodEnergyDensityV[NoDays]; // This is  (1/E)
    for( int i = 0; i < NoDays; i++)
        CopepodEnergyDensityV[i] =   (pow(FL[i],2)*aL + pow(FS[i],2)*aS +B) /(Elargecop* pow(FL[i],2)*aL + Esmallcop*pow(FS[i],2)*aS +Eb*B)    ;
    
double ConsumptionTerm1V[NoDays];
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm1V[i] =  ACT_CONS[i]*(Epsilon1 + Epsilon2*TT_FEED[i])*(maxI*CopepodEnergyDensityV[i])* pow(Q10_U , TT_FEED[i] / 10 ) *( aL*pow(FL[i],2) + aS*pow(FS[i],2) +B) ;
    
double ConsumptionTerm2V[NoDays]; // This is (U0/E)*Q10
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm2V[i] =  (maxI*CopepodEnergyDensityV[i]);
    
double ConsumptionTerm3V[NoDays]; // This is (aL Fl + aS Fs)
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm3V[i] =    ( (aL*pow(FL[i],2))/Elargecop + (aS*pow(FS[i],2))/Esmallcop  + B/Eb ) ;
    
double METABOLISM_Q10_FEEDV[NoDays];
 for( int i = 0; i < NoDays; i++) 
	 METABOLISM_Q10_FEEDV[i] =  ACT_MET[i]*M_FEED*pow(Q10_MF , TT_FEED[i] / 10) ;

double METABOLISM_Q10_OVV[NoDays];
 for( int i = 0; i < NoDays; i++) 
	 METABOLISM_Q10_OVV[i] = M_OV*pow(Q10_MO , TT_OV[i] / 10 )  ;


/*
#=======================================#
  	       MAIN LOOP
#=======================================#
*/
    
      
static double NET_A[IndNum*NoDays];
static double    dS[IndNum*NoDays];


int JulianDay  ;

double ConsumptionTerm1;
double ConsumptionTerm2;
double ConsumptionTerm3;

    
double METABOLISM_Q10_FEED  ;
double METABOLISM_Q10_OV;




    
double dgdt ;
double drdt ;
double NewRdry ;
    

static const char filename97[] = "Individuals.txt";
FILE *file97 = fopen ( filename97, "w" );




/* initialize time */
struct tm InitialTime = { .tm_year=StartYear-1900, .tm_mon=StartMonth-1, .tm_mday=StartDay };
struct tm Time = { .tm_year=StartYear-1900, .tm_mon=StartMonth-1, .tm_mday=StartDay };


bool DateSave = false;




       
for( int t = 0; t < NoDays; t++)
{
     
  
  
  
  JulianDay =  JulianDayV[t];
  
  ConsumptionTerm1  = ConsumptionTerm1V[t] ;
  ConsumptionTerm2 =  ConsumptionTerm2V[t] ;
  ConsumptionTerm3 =  ConsumptionTerm3V[t] ;

    
  METABOLISM_Q10_FEED = METABOLISM_Q10_FEEDV[t];
  METABOLISM_Q10_OV = METABOLISM_Q10_OVV[t];

    
  
  for( int z = 0; z < NumberOfDaysToSave ; z++)
{
  if(t== TimeToSave[z])
  {
    /* modify date */
    Time =  InitialTime;
    Time.tm_mday =  InitialTime.tm_mday + t;
    mktime(&Time);
    DateSave =true ; 
    break;
  }
}

  
  
  for( int i = 0; i < IndNum; i++)
  {
    if(t >= DF_DATE_ADDED[i])
    {
      if( DF_STATE[i])
      {
	
	
	DF_STATE[i] = !(  ( (DF_R[i] / (DF_S[i]+DF_G[i])) > (OVTHRESH1 - JulianDay*OVTHRESH2)/pow(DF_S[i],OVTHRESH3)) && (JulianDay > 120)) ;  // overwintering entry

      }
      else
      {
                   
         DF_STATE[i] =  (JulianDay == OverwinteringExit) ;  // overwintering exit

      }
      
    }
    
    if( (t >= DF_DATE_ADDED[i])*(JulianDay ==360)*(DF_STATE[i]) )  //forcing overwinter by late december (day 360)
    {
     DF_STATE[i] = 0;  
    } 
    
         
      
      NET_A[i+t*IndNum] = (DF_R[i] > 0) * ((DF_STATE[i]) * (      (LAMBDA_F(DF_R[i] / (DF_S[i]+DF_G[i]), tau)*ConsumptionTerm1*DF_S[i]) / (LAMBDA_F(DF_R[i] / (DF_S[i]+DF_G[i]), tau)*ConsumptionTerm2*pow(DF_S[i],0.33)+ConsumptionTerm3)      )  -
                                           (  pow(DF_WEIGHT[i],rrr) ) * ((!DF_STATE[i]) * METABOLISM_Q10_OV  + (DF_STATE[i]) * METABOLISM_Q10_FEED )) ;


		  
					  // Allocation towards structure
      dS[i+t*IndNum] =  (NET_A[i+t*IndNum] > 0) * (NET_A[i+t*IndNum] * kappa(  DF_R[i]/(DF_S[i]+DF_G[i]),  DF_S[i],  rho0 ,   rhow ,   S1,S2)) ;

     
      DF_S[i] =  DF_S[i] + dS[i+t*IndNum] * (t >=  DF_DATE_ADDED[i]) * (t <=  DF_DATE_FINISH[i]) ;  
      
      drdt =  (t >= DF_DATE_ADDED[i]) * (t <= DF_DATE_FINISH[i]) * (NET_A[i+t*IndNum] -dS[i+t*IndNum] ) ;
      
      
      
      if(!DF_STATE[i]){
          DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
          DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) ; // NEW WEIGHT
      }
      if(DF_STATE[i]){
		   if (Er2[i] > Er -.1 || Er2[i] < Er -.1)
          {
              NewRdry =DF_R[i]/Er2[i] + drdt/Er ; // NEW RESERVE DRY WEIGHT
              
              DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) +  Rdw*(drdt/Er) ; // NEW WEIGHT
              
              DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
              
              Er2[i] = DF_R[i]/NewRdry ; // NEW RESERVE ENERGY DENSITY
              
              Rdw2[i] = (DF_WEIGHT[i] -  Gdw*(DF_G[i] / Eg) - Sdw*(DF_S[i]/Es))/(DF_R[i] / Er2[i]) ; // NEW RESERVE WET WEIGHT TO DRY WEIGHT RATIO
              
          } else {
              DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
              DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) ; // NEW WEIGHT
          }
      }
      

    
    // Gonad production
     if( (JulianDay == GonadDay  )*(t >= DF_DATE_ADDED[i]) * (t <= DF_DATE_FINISH[i]))

    {
      
      dgdt = (DF_R[i]*GONAD(  DF_R[i]/(DF_G[i]+DF_S[i]),  DF_S[i],  rho0, rhow,G1,G2)) ;
      

          if(  (!Age[i] && (  (t-300) > DF_DATE_ADDED[i] ) && dgdt >0) ||  (Age[i] && dgdt >0) ){
        
           NewRdry =DF_R[i]/Er2[i] - dgdt/Eg ; // NEW RESERVE DRY WEIGHT

          
           DF_R[i] = DF_R[i] - dgdt ; // NEW RESERVES

           Er2[i] = DF_R[i]/NewRdry ; // NEW RESERVE ENERGY DENSITY
          
           DF_G[i] = DF_G[i] + dgdt ; // NEW GONADS
          
           Rdw2[i] = (DF_WEIGHT[i] -  Gdw*(DF_G[i] / Eg) - Sdw*(DF_S[i]/Es))/(DF_R[i] / Er2[i]) ; // NEW RESERVE WET WEIGHT TO DRY WEIGHT RATIO
         

	   }
   }
    
    DF_G[i] = !(JulianDay ==MeanSpawningDay)*DF_G[i] ; 

    
 
      
      					  // Change initial abundances AT TIME = DATE ADDED
      
      
      if(!Age[i] && (t == DF_DATE_ADDED[i])){ // IF AGE 0
          
          ABUNDANCE_DAILY[i] = MULTIPLIER1*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==0)+
          MULTIPLIER2*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==355)+
          MULTIPLIER3*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==730)+
          MULTIPLIER4*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1093)+
          MULTIPLIER5*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1804)+
          MULTIPLIER6*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==2192) ;
      }

      

    
      
					  // Starvation mortality
      

      if(!DF_STATE[i]){
    if( (t >=  DF_DATE_ADDED[i])  && (t <=  DF_DATE_FINISH[i]) )
    { 
      ABUNDANCE_DAILY[i] = ABUNDANCE_DAILY[i] /  (1+ exp(-sigma1*(  (DF_R[i]/(DF_G[i]+DF_S[i]))-sigma2)))  ;  // SURVIVAL PROBABILITY
      
    }
      }
      
      		         // Natural mortality
      
      
      
      if(!Age[i] && (t >= DF_DATE_ADDED[i]) &&  (t <=  DF_DATE_FINISH[i])){ // IF AGE 0
      
          ABUNDANCE_DAILY[i] = mu1*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==0)+
          mu2*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==355)+
          mu3*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==730)+
          mu4*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1093)+
          mu5*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1804)+
          mu6*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==2192) ;
      }

 
      

    
    


// Save energy, abundance results to text file


    if(DateSave)
    {
	
		
    fprintf(file97, "%d %d %d %d %s %G %G %G %G %G  %G %G \n",JulianDay,Age[i],DF_DATE_ADDED[i],t,asctime(&Time) , DF_R[i],DF_S[i],DF_G[i],DF_R[i]/DF_S[i], DF_WEIGHT[i],pow(  DF_S[i] / (LS_a*Es) , LS_b), ABUNDANCE_DAILY[i]) ;
    fflush(file97);

  }




  }
  
DateSave =false ; 


}



fclose(file97) ;



    

}

 

return( *result) ;

}











double MODEL_ERROR(double X[NoParameters] ,
		   double TT_FEED[NoDays] ,
		   double TT_OV[NoDays] ,
		   double ACT_MET[NoDays],
		   double ACT_CONS[NoDays],
		   int JulianDayV[NoDays],
		   double ABUNDANCE_DAILY[IndNum],
		   int Age[IndNum],
		   double FL[NoDays] ,
		   double FS[NoDays],
		   double DF_R[IndNum] ,
		   double DF_S[IndNum],
		   double DF_G[IndNum],
		   double Rdw2[IndNum],
		   double Er2[IndNum], 
		   int DF_STATE[IndNum] ,
		   int DF_DATE_ADDED[IndNum],
		   int DF_DATE_FINISH[IndNum],
		   double DF_LENGTH[IndNum] ,
		   double DF_WEIGHT[IndNum]  ,
		   int WL_DATE_CHECK[NumWLsurveys]  ,
		   double MEAN_WL_LENGTH[NumMeanLengths] ,
		   double MEAN_WL_WEIGHT[NumMeanLengths]  ,
		   double MEAN_WL_RS[NumMeanLengths] ,
		   int MEAN_WL_DATE_ADDED[NumMeanLengths]  ,
		   int MEAN_WL_DRUN[NumMeanLengths] ,
		   int MEAN_WL_AGE[NumMeanLengths],
		   int ABUNDANCE_DATE_CHECK[NumABUNDANCEsurveys],
		   double ABUNDANCE_FIT[NumAbundancePoints],
		   int ABUNDANCE_DATE_ADDED[NumAbundancePoints],
		   int ABUNDANCE_DRUN[NumAbundancePoints],
		   int ABUNDANCE_AGE[NumAbundancePoints],
		   double *result)
{
  
  

/*~~~~~~~~~~~ SET MODEL ERROR TO ZERO (LEAST SQUARE ERROR) ... ~~~~~~~~~~~*/
      


if(X[0] <= X[1]){*result = *result + 1E10 ; }

if( X[0] > X[1])  // parameter constraints
{



double aL = X[0], aS = X[1] , tau = X[2], OVTHRESH1 = X[3],OVTHRESH2 = X[4], sigma1 = X[5], sigma2 =  X[6], rho0 = X[7] , rhow = X[8], S1 = X[9], S2 = X[10], OVTHRESH3 = X[11] , G1 = X[12] , G2 = X[13], MULTIPLIER1 = X[14] , MULTIPLIER2= X[15] ,  MULTIPLIER3 = X[16] ,  MULTIPLIER4 = X[17] , MULTIPLIER5 = X[18] , MULTIPLIER6 = X[19] , mu1 = X[20] , mu2 = X[21] , mu3 = X[22] , mu4 = X[23] , mu5 = X[24] , mu6 = X[25] , B = X[26] , Eb = X[27] ;


  
/*~~~~~~~~~~~ ENVIRONMENTAL DATA VECTORS ~~~~~~~~~~~~*/
   
 
double CopepodEnergyDensityV[NoDays]; // This is  (1/E)
    for( int i = 0; i < NoDays; i++)
        CopepodEnergyDensityV[i] =   (pow(FL[i],2)*aL + pow(FS[i],2)*aS +B) /(Elargecop* pow(FL[i],2)*aL + Esmallcop*pow(FS[i],2)*aS +Eb*B)    ;
    
double ConsumptionTerm1V[NoDays];
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm1V[i] =  ACT_CONS[i]*(Epsilon1 + Epsilon2*TT_FEED[i])*(maxI*CopepodEnergyDensityV[i])* pow(Q10_U , TT_FEED[i] / 10 ) *( aL*pow(FL[i],2) + aS*pow(FS[i],2) +B) ;
    
double ConsumptionTerm2V[NoDays]; // This is (U0/E)*Q10
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm2V[i] =  (maxI*CopepodEnergyDensityV[i]);
    
double ConsumptionTerm3V[NoDays]; // This is (aL Fl + aS Fs)
    for( int i = 0; i < NoDays; i++)
        ConsumptionTerm3V[i] =    ( (aL*pow(FL[i],2))/Elargecop + (aS*pow(FS[i],2))/Esmallcop  + B/Eb ) ;
    
double METABOLISM_Q10_FEEDV[NoDays];
 for( int i = 0; i < NoDays; i++) 
	 METABOLISM_Q10_FEEDV[i] =  ACT_MET[i]*M_FEED*pow(Q10_MF , TT_FEED[i] / 10) ;

double METABOLISM_Q10_OVV[NoDays];
 for( int i = 0; i < NoDays; i++) 
	 METABOLISM_Q10_OVV[i] = M_OV*pow(Q10_MO , TT_OV[i] / 10 )  ;


/*
#=======================================#
  	       MAIN LOOP
#=======================================#
*/
    
      
static double NET_A[IndNum*NoDays];
static double    dS[IndNum*NoDays];


int JulianDay  ;

double ConsumptionTerm1;
double ConsumptionTerm2;
double ConsumptionTerm3;

    
double METABOLISM_Q10_FEED  ;
double METABOLISM_Q10_OV;


double ModelledReserveRatio[NumMeanLengths] = { 0 };
double ModelledWeight[NumMeanLengths] = { 0 };
double ModelledLength[NumMeanLengths] = { 0 };
double ModelledDenominator[NumMeanLengths] = { 0 };


double ModelledAbundance[NumAbundancePoints] = { 0 };

    
double dgdt ;
double drdt ;
double NewRdry ;
    

       
for( int t = 0; t < NoDays; t++)
{
     
  
  
  
  JulianDay =  JulianDayV[t];
  
  ConsumptionTerm1  = ConsumptionTerm1V[t] ;
  ConsumptionTerm2 =  ConsumptionTerm2V[t] ;
  ConsumptionTerm3 =  ConsumptionTerm3V[t] ;

    
  METABOLISM_Q10_FEED = METABOLISM_Q10_FEEDV[t];
  METABOLISM_Q10_OV = METABOLISM_Q10_OVV[t];

    
  
  for( int i = 0; i < IndNum; i++)
  {
    if(t >= DF_DATE_ADDED[i])
    {
      if( DF_STATE[i])
      {
	
	
	DF_STATE[i] = !(  ( (DF_R[i] / (DF_S[i]+DF_G[i])) > (OVTHRESH1 - JulianDay*OVTHRESH2)/pow(DF_S[i],OVTHRESH3)) && (JulianDay > 120)) ;  // overwintering entry

      }
      else
      {
                   
         DF_STATE[i] =  (JulianDay == OverwinteringExit) ;  // overwintering exit

      }
      
    }
    
    if( (t >= DF_DATE_ADDED[i])*(JulianDay ==360)*(DF_STATE[i]) )  //forcing overwinter by late december (day 360)
    {
     DF_STATE[i] = 0;  
    } 
    
         
      
      NET_A[i+t*IndNum] = (DF_R[i] > 0) * ((DF_STATE[i]) * (      (LAMBDA_F(DF_R[i] / (DF_S[i]+DF_G[i]), tau)*ConsumptionTerm1*DF_S[i]) / (LAMBDA_F(DF_R[i] / (DF_S[i]+DF_G[i]), tau)*ConsumptionTerm2*pow(DF_S[i],0.33)+ConsumptionTerm3)      )  -
                                           (  pow(DF_WEIGHT[i],rrr) ) * ((!DF_STATE[i]) * METABOLISM_Q10_OV  + (DF_STATE[i]) * METABOLISM_Q10_FEED )) ;


		  
					  // Allocation towards structure
      dS[i+t*IndNum] =  (NET_A[i+t*IndNum] > 0) * (NET_A[i+t*IndNum] * kappa(  DF_R[i]/(DF_S[i]+DF_G[i]),  DF_S[i],  rho0 ,   rhow ,   S1,S2)) ;

     
      DF_S[i] =  DF_S[i] + dS[i+t*IndNum] * (t >=  DF_DATE_ADDED[i]) * (t <=  DF_DATE_FINISH[i]) ;  
      
      drdt =  (t >= DF_DATE_ADDED[i]) * (t <= DF_DATE_FINISH[i]) * (NET_A[i+t*IndNum] -dS[i+t*IndNum] ) ;
      
      
      
      if(!DF_STATE[i]){
          DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
          DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) ; // NEW WEIGHT
      }
      if(DF_STATE[i]){
		   if (Er2[i] > Er -.1 || Er2[i] < Er -.1)
          {
              NewRdry =DF_R[i]/Er2[i] + drdt/Er ; // NEW RESERVE DRY WEIGHT
              
              DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) +  Rdw*(drdt/Er) ; // NEW WEIGHT
              
              DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
              
              Er2[i] = DF_R[i]/NewRdry ; // NEW RESERVE ENERGY DENSITY
              
              Rdw2[i] = (DF_WEIGHT[i] -  Gdw*(DF_G[i] / Eg) - Sdw*(DF_S[i]/Es))/(DF_R[i] / Er2[i]) ; // NEW RESERVE WET WEIGHT TO DRY WEIGHT RATIO
              
          } else {
              DF_R[i] = DF_R[i] + drdt ; // NEW RESERVES
              DF_WEIGHT[i] = Rdw2[i]*(DF_R[i]/Er2[i] ) + Gdw*(DF_G[i]/Eg) +  Sdw*(DF_S[i] /Es) ; // NEW WEIGHT
          }
      }
      

    
    // Gonad production
     if( (JulianDay == GonadDay  )*(t >= DF_DATE_ADDED[i]) * (t <= DF_DATE_FINISH[i]))

    {
      
      dgdt = (DF_R[i]*GONAD(  DF_R[i]/(DF_G[i]+DF_S[i]),  DF_S[i],  rho0, rhow,G1,G2)) ;
      

          if(  (!Age[i] && (  (t-300) > DF_DATE_ADDED[i] ) && dgdt >0) ||  (Age[i] && dgdt >0) ){
        
           NewRdry =DF_R[i]/Er2[i] - dgdt/Eg ; // NEW RESERVE DRY WEIGHT

          
           DF_R[i] = DF_R[i] - dgdt ; // NEW RESERVES

           Er2[i] = DF_R[i]/NewRdry ; // NEW RESERVE ENERGY DENSITY
          
           DF_G[i] = DF_G[i] + dgdt ; // NEW GONADS
          
           Rdw2[i] = (DF_WEIGHT[i] -  Gdw*(DF_G[i] / Eg) - Sdw*(DF_S[i]/Es))/(DF_R[i] / Er2[i]) ; // NEW RESERVE WET WEIGHT TO DRY WEIGHT RATIO
         

	   }
   }
    
    DF_G[i] = !(JulianDay ==MeanSpawningDay)*DF_G[i] ; 

    
 
      
      					  // Change initial abundances AT TIME = DATE ADDED
      
      
      if(!Age[i] && (t == DF_DATE_ADDED[i])){ // IF AGE 0
          
          ABUNDANCE_DAILY[i] = MULTIPLIER1*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==0)+
          MULTIPLIER2*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==355)+
          MULTIPLIER3*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==730)+
          MULTIPLIER4*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1093)+
          MULTIPLIER5*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1804)+
          MULTIPLIER6*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==2192) ;
      }

      

    
      
					  // Starvation mortality
      

      if(!DF_STATE[i]){
    if( (t >=  DF_DATE_ADDED[i])  && (t <=  DF_DATE_FINISH[i]) )
    { 
      ABUNDANCE_DAILY[i] = ABUNDANCE_DAILY[i] /  (1+ exp(-sigma1*(  (DF_R[i]/(DF_G[i]+DF_S[i]))-sigma2)))  ;  // SURVIVAL PROBABILITY
      
    }
      }
      
      		         // Natural mortality
      
      
      
      if(!Age[i] && (t >= DF_DATE_ADDED[i]) &&  (t <=  DF_DATE_FINISH[i])){ // IF AGE 0
      
          ABUNDANCE_DAILY[i] = mu1*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==0)+
          mu2*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==355)+
          mu3*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==730)+
          mu4*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1093)+
          mu5*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==1804)+
          mu6*ABUNDANCE_DAILY[i]*(DF_DATE_ADDED[i]==2192) ;
      }

 
      

      /*
       #=========================================#
       Compute mean length and weight at survey
       #=========================================#
       */
      
      
	  
	  for( int NumCheck = 0 ; NumCheck < NumWLsurveys ; NumCheck++ )
	  {
		  if ( t == WL_DATE_CHECK[NumCheck] )
		  {        
        
        if(t> DF_DATE_ADDED[i]  && t<= DF_DATE_FINISH[i] && DF_R[i] > 0)
	{ // remove dead sandeels
            for( int j = 0; j < NumMeanLengths ; j++) {								
                if(DF_DATE_ADDED[i] == MEAN_WL_DATE_ADDED[j] && t == MEAN_WL_DRUN[j] && Age[i]==MEAN_WL_AGE[j])
                {       		   						
                        ModelledReserveRatio[j] = ModelledReserveRatio[j] + ABUNDANCE_DAILY[i] *(DF_R[i]/DF_S[i]) ;
                        ModelledWeight[j] = ModelledWeight[j] + ABUNDANCE_DAILY[i] *DF_WEIGHT[i];
                        ModelledLength[j] = ModelledLength[j] + ABUNDANCE_DAILY[i] *pow(  DF_S[i] / (LS_a*Es) , LS_b) ;
                        ModelledDenominator[j] = ModelledDenominator[j] + ABUNDANCE_DAILY[i] ;
					}		
				}		
            }
        }
    }
	
    
    
    	  for( int AbundanceNumCheck = 0 ; AbundanceNumCheck < NumABUNDANCEsurveys ; AbundanceNumCheck++ )
	  {
	      if ( t == ABUNDANCE_DATE_CHECK[AbundanceNumCheck] )
	      {
	      if(t >= DF_DATE_ADDED[i]  && t<= DF_DATE_FINISH[i] && DF_R[i] > 0) { // remove dead sandeels

  
            for( int j = 0; j < NumAbundancePoints ; j++) {
								
				
                if(DF_DATE_ADDED[i] == ABUNDANCE_DATE_ADDED[j] && t == ABUNDANCE_DRUN[j] && Age[i]==ABUNDANCE_AGE[j])
                {       
                        ModelledAbundance[j] = ModelledAbundance[j] + ABUNDANCE_DAILY[i] ; 
					}
				}	
	  }
    
    
  
}
}
  }
  
}



}

 

return( *result) ;

}











