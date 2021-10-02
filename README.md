As part of the desire to better understand what influences economic policy (the project was carried out at the beginning of the Corona period), it was only reasonable to examine how different factors affect the degree of economic freedom of different countries. The database was compiled from data for 2019 from the MyHeritage website (https://herit.ag/3uxSAYI). The final linear regression model and the various analyzes which led to its creation were performed using R Studio. 

The project consists of two parts: 

Part A - According to scattering diagrams between different pairs of variables, hypotheses have been raised as to whether there is a causal or sample relationship between these pairs. In addition, the variables were analyzed descriptively, it was examined whether there were any abnormal observations, and common one- and two-dimensional tables were presented. 

Part B - In this section, preliminary processing, regression model adjustment and testing of assumptions, model improvement, conclusions and recommendations were performed.

Pre-processing: initial definitions of the model variables were made - definition of additional variables (a variable that defines whether there was legislation in the field of domestic violence in the country as of 2018, and a variable that associates the country to the continent to which it belongs), removal (variable chosen to remove is Nonlinear distribution of the variable in relation to the explained variable, low Pearson index, etc.) and adjustment of variables from Part A. 
In addition, dummy variables were added (categories were merged for the variables “financial freedom”, “investment freedom”) and interaction variables for these dummy variables.

Regression model adjustment - the regression model was adjusted according to different criteria (R2, Radjusted2, AIC & BIC) and different algorithms (forward regression, backward and steps). 

Examination of model assumptions - At this stage, the assumption of equality of variances and the linear assumption of the model were examined (using the standardized error scatter graph and the Goldfeld-Quandt test), and the normality of the errors (using KS test and histogram and QQ plot). In addition, this section gave an example of using the model (using a model to predict observations from 2020). 
In addition to that, a hypothesis based on the model's results was tested (whether there is a linear relationship between the explained variable and at least one of the explanatory variables). 

Improving the model - in this part of the project it is examined whether it is necessary to add transformation variables / replace an existing variable with any transformation variable.

for the conclusions and recommendations you'd have to read the paper.

please feel free to contact me here for any questions!
