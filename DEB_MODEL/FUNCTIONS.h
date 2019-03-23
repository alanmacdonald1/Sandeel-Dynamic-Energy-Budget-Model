/*
#=======================================#
  	 DEFINE FUNCTIONS
#=======================================#
*/



/*
#=======================================#
         STARVATION RESPONSE
#=======================================#
*/

/* HUNGRY OR HEALTY (SEE JONES THESIS) , MAINTENCE IS ONLY REDUCED IN WINTERTIME */



double LAMBDA_F(float rho, float tau_1)
{      
   return((rho>tau_1) + (rho<= tau_1)*H_U);
}

/*
#=======================================================#
 PROPORTION OF EXCESS ASSIMILATE DIRECTED TOWARDS LENGTH
#=======================================================#
*/

double k_0 ;
double StructuralAllocation ;


double kappa(  float rho, float S,  float rho_0,  float rho_w , float  S1 ,float   S2)
{
k_0 = S1 -S2*log(S) ;
StructuralAllocation =  ((rho > (rho_0+rho_w) )*k_0 +  (rho <= (rho_0+rho_w) ) *(  k_0 * ( (rho> rho_0)*(rho-rho_0) ) /rho_w ) );
StructuralAllocation =   (StructuralAllocation>=1) + (StructuralAllocation>0)*(StructuralAllocation<1)*StructuralAllocation ; 
return(StructuralAllocation) ; 
}



/*
#=======================================================#
 DAILY PROPORTION OF RESERVES DIRECTED TOWARDS GONADS
#=======================================================#
*/



double GonadAllocation ;
double G_0 ;

double GONAD(  float rho,float  S,  float rho_0,   float rho_w ,  float G1 , float  G2)
{     
  G_0 = G1 +G2*log(S) ;
GonadAllocation =  ((rho > (rho_0+rho_w) )*G_0 +  (rho <= (rho_0+rho_w) ) *(  G_0 * ( (rho> rho_0)*(rho-rho_0) ) /rho_w ) );
GonadAllocation =   (GonadAllocation>=1) + (GonadAllocation>0)*(GonadAllocation<1)*GonadAllocation ; 
return(GonadAllocation) ; 
}

 
