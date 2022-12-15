 

<!-- *** Based on this template -> https://github.com/othneildrew/Best-README-Template/blob/master/README.md  -->


<a name="readme-top"></a>

### sandeel population model for the North Sea



<div style="width:10vw">
<img float="left" margin = 10 padding = 10 width="40%" alt="example_of_model_output" src="https://user-images.githubusercontent.com/43473952/207916898-d9bba1c5-40e4-4bcb-93fc-a78178daa8d8.png">

<img float= "right" margin = 10 padding = 10 width="40%" alt = "paper" src="https://user-images.githubusercontent.com/43473952/207917653-5e3749a1-0a8d-4f37-8e09-8e8dddfc5d48.png">
 </div>


<!-- Summary -->
## Summary

This code produces model results in 
'Exploring the Influence of Food and Temperature on North Sea Sandeels Using a New Dynamic Energy Budget Model' by Alan MacDonald, Douglas C. Speirs, Simon P. R. Greenstreet and Michael R. Heath.
https://www.frontiersin.org/articles/10.3389/fmars.2018.00339/full


Running the model produces 
1) a text file of sandeel individual data, "Individuals.txt".
2) A series of plots showing changes in cohort abundance, mean length and mean weight.
3) A csv file,"Results.csv", which contains the plotting data.


*Note that sometimes the user will need to run the 'ModelRun.R' script twice to produce all plots.



<!-- Running instructions -->
## Running instructions
 
To run, simply run the 'ModelRun.R' script.

The user can parameterise the model by changing line 30 to

Parameterisation="T"

or run the model by  changing line 30 to

Parameterisation="F"


 

<!-- User input-->
## User input
 

The directory USER_INPUT allows the user to change the run time, the age and year class of the sandeels and alter parameter values. 

<p align="right"> @copyright 2018-2019 Alan MacDonald (<a href="#readme-top">back to top</a>)</p>


<!-- LICENSE
* Code based on *
 
MIT License

Copyright (c) 2021 Othneil Drew

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
  --> 
 

<p align="right">(<a href="#readme-top">back to top</a>)</p>
