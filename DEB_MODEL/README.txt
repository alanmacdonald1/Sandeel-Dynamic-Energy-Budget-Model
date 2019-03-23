SANDEEL DEB MODEL



#~~~ Summary ~~~#

This code produces model results in 
'Exploring the Influence of Food and Temperature on North Sea Sandeels Using a New Dynamic Energy Budget Model' by Alan MacDonald, Douglas C. Speirs, Simon P. R. Greenstreet and Michael R. Heath.
https://www.frontiersin.org/articles/10.3389/fmars.2018.00339/full


#~~~ Running instructions ~~~#

To run, simply run the 'ModelRun.R' script.

The user can parameterise the model by changing line 30 to

Parameterisation="T"

or run the model by  changing line 30 to

Parameterisation="F"

Running the model produces 
1) a text file of sandeel individual data, "Individuals.txt".
2) A series of plots showing changes in cohort abundance, mean length and mean weight.
3) A csv file,"Results.csv", which contains the plotting data.


*Note that sometimes the user will need to run the 'ModelRun.R' script twice to produce all plots.

#~~~ User input ~~~#

The directory USER_INPUT allows the user to change the run time, the age and year class of the sandeels and alter parameter values. 

