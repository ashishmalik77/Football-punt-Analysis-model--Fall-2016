Working Code

title1 'STAT 512 Project: Distance of American Football Punts';
title2 'Team 4: Songyan, Ashish, Pratik, Mallory';

*Insert the dataset;
data punting;
input Distance Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength;
cards;
162.5 4.75 170 170 106 106 240.57
144 4.07 140 130 92 93 195.49
147.5 4.04 180 170 93 78 152.99
163.5 4.18 160 160 103 93 197.09
192 4.35 170 150 104 93 266.56
171.75 4.16 150 150 101 87 260.56
162 4.43 170 180 108 106 219.25
104.93 3.2 110 110 86 92 132.68
105.67 3.02 120 110 90 86 130.24
117.59 3.64 130 120 85 80 205.88
140.25 3.68 120 140 89 83 153.92
150.17 3.6 140 130 92 94 154.64
165.17 3.85 160 150 95 95 240.57
;


*Print the dataset to ensure correctness;
proc print data=punting;
run;


*Run the regression with all predictor variables;
proc reg data=punting;
model Distance = Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength;
run;


*Sort the data set from shortest hang time to longest;
proc sort data=punting;
by Hang;

*Print the dataset to ensure correctness;
proc print data=punting;
run;

*Plot the graph of distance vs. hang time;
title3 'Distance vs. Hang Time';
proc gplot data=punting;
symbol1 v=circle i=sm70;
plot Distance*Hang;

*Create an additional variable cslope depending if the hang time is less or greater than 4.18;
data punting;
set punting;
if Hang le 4.18
then cslope=0;
if Hang gt 4.18
then cslope=Hang-4.18;

*Print the dataset to ensure correctness;
proc print data=punting;

*Run the regression based on the predictors of hang time and cslope;
title3 'Distance vs. Hang Time and Cslope';
proc reg data=punting;
model Distance=Hang cslope;

*Save the output and predicted values;
output out=punting1 p=Distancehat;

*Print output data to ensure correctness;
proc print data=punting1;

*Plot data with fitted values to obtain piecewise model;
title3 'Distance vs. Hang Time: Piecewise Model';
symbol1 v=circle i=none c=black;
symbol2 v=none i=join c=blue;
proc sort data=punting1; by Hang;
proc gplot data=punting1;
plot (Distance Distancehat)* Hang/overlay;
16

*TEST IF SAME LINE;
*Create a variable to be the sum of right leg strength and flexibility;
title3 ' ';
data punting;
set punting;
SUM = R_Strength + R_Flexibility;

*Print dataset to ensure correctness;
proc print data=punting;

*Run the regression for all variables and for all variables including sum;
proc reg data=punting;
title3 'Regression with all predictor variables';
model Distance = Hang L_Strength L_Flexibility O_Strength;
proc reg data=punting;
title3 'Regression with all predictor variables including the sum';
model Distance = Hang L_Strength L_Flexibility O_Strength SUM;
t1: test SUM=0;

title3 'Regression with Type I and II sums of squares';
proc reg data=punting;
model Distance = Hang L_Strength L_Flexibility O_Strength R_Strength R_Flexibility/ SS1 SS2;


title3 'Regressions with multiple different models';
proc reg data=punting;
model Distance= Hang;
model Distance= R_Strength;
model Distance= L_Strength;
model Distance= R_Flexibility;
model Distance= L_Flexibility;
model Distance= SUM;
model Distance= O_Strength;
model Distance= R_Strength O_Strength L_Strength SUM;
model Distance= R_Flexibility L_Flexibility SUM;
model Distance= R_Flexibility R_Strength;
model Distance= L_Flexibility L_Strength;
model Distance= Hang R_Strength R_Flexibility;
model Distance= Hang L_Strength L_Flexibility O_Strength R_Strength R_Flexibility;

*Run regression with all 6 variables;
title3 'Regression with all predictor variables';
proc reg data=punting;
model Distance = Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength;
output out=ash r=resid;
run;

*Generate plots for distance vs. all predictor variables individually;
title3 'Distance vs. Hang Time';
proc sort data=punting;
by Hang;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*Hang;
run;

title3 'Distance vs. Right Leg Flexibility';
proc sort data=punting;
by R_Flexibility;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*R_Flexibility;
run;
title3 'Distance vs. Left Leg Flexibility';
proc sort data=punting;

17
by L_Flexibility;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*L_Flexibility;
run;

title3 'Distance vs. Right Leg Strength';
proc sort data=punting;
by R_Strength;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*R_Strength;
run;

title3 'Distance vs. Left Leg Strength';
proc sort data=punting;
by L_Strength;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*L_Strength;
run;

title3 'Distance vs. Overall Leg Strength';
proc sort data=punting;
by O_Strength;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance*O_Strength;
run;

*Try to transform L_strength;
data punting;
set punting;
L_Str_new=log10(L_Strength);
proc print data=punting;
run;

title3 'Distance vs. log(Left Leg Strength)';
proc sort data= punting;
by L_Str_new;
symbol1 v=circle i=sm85;
proc gplot data=punting;
plot Distance * L_Str_new;
run;

*Generate histogram and QQplot for the residuals;
title3 'QQplot and Histogram for Residuals';
proc univariate data=ash plot normal;
var resid;
qqplot resid /normal (L=1 mu=est sigma=est);
histogram /normal (L=1 mu=est sigma=est);
run;

*Check BoxCox for potential transformations in Y;
title3 'BoxCox procedure';
proc transreg data=punting;
model boxcox(Distance) = identity(Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength);
run;

*Transform Distance;
title3 'Dataset with Transformed Distance, Y^2.25';
data punting1;
set punting;
Distance1= Distance*Distance*sqrt(sqrt(Distance));
proc print data=punting1;
run;

*Run regression with transformed Y;
title3 'Regression with Transformed Response Variable';
18
proc reg data=punting1;
model Distance1 = Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength;
output out=ash1 r=resid1;
run;

*Check relationship between transformed reponse and predictors;
title3 'Transformed Distance vs. Hang Time';
proc sort data= punting1;
by Hang;
symbol1 v=circle i=sm85;
proc gplot data=punting1;
plot Distance1 * Hang;
run;

*Decision: No transformations necessary;
*Selection of Best Model based on Cp Criterion;
title3 'Best model for each model number based on Cp criterion';
proc reg data=punting1;
model Distance = Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength/selection= rsquare cp b best=1;
run;

*Selection of Best Model based on Stepwise;
title3 'Best model based on Stepwise selection';
proc reg data=punting1;
model Distance = Hang R_Strength L_Strength R_Flexibility L_Flexibility O_Strength/selection= stepwise;
run;

*Check regression for "best" Cp model;
title3 'Distance vs. Right Leg Strength and Overall Leg Strength';
proc reg data=punting1;
model Distance= R_Strength O_Strength;
output out=ash2 r=resid2;
run;

*Check regression for "best" Stepwise model;
title3 'Distance vs. Hang and Overall Leg Strength';
proc reg data=punting1;
model Distance= Hang O_Strength;
output out=ash3 r=resid3;
run;

*Generate histogram and QQplot;
title3 'QQplot and histogram of best model residuals';
proc univariate data=ash3 plot normal;
var resid2;
qqplot resid2/normal(L=1 mu=est sigma=est);
histogram resid2/normal(L=1 mu=est sigma=est);
run;


*Generate regression diagnostics;
title3 'Regression Diagnostics';
proc reg data=punting1;
model Distance = R_Strength O_Strength/r influence;
output out=ash2 r=resid2;
run;

proc reg data=punting1;
model Distance = R_Strength R_Flexibility L_Flexibility O_Strength/tol vif;
output out=ash2 r=resid2;
run;

*Generate Partial Regression Plots;
title3 'Partial Regression Plot of Distance and Right Leg Strength';
proc reg data=punting;
model Distance R_Strength = O_Strength;
19
output out=song r=mallory pratik;
symbol1 v=circle i=rl;
axis1 label=('R_Strength');
axis2 label=(angle=90 'Distance');
proc gplot data=song;
plot mallory*pratik/haxis=axis1 vaxis=axis2 vref=0;
run;

title3 'Partial Regression Plot of Distance and Overall Leg Strength';
proc reg data=punting;
model Distance O_Strength = R_Strength;
output out=song2 r=mallory2 pratik2;
symbol1 v=circle i=rl;
axis1 label=('O_Strength');
axis2 label=(angle=90 'Distance');
proc gplot data=song2;
plot mallory2*pratik2/haxis=axis1 vaxis=axis2 vref=0;
run;

title3 'Partial Regression Plot of Right Leg Strength and Overall Leg Strength';
proc reg data=punting;
model O_Strength R_Strength = Distance;
output out=song1 r=mallory1 pratik1;
symbol1 v=circle i=rl;
axis1 label=('R_Strength');
axis2 label=(angle=90 'O_Strength');
proc gplot data=song1;
plot mallory1*pratik1/haxis=axis1 vaxis=axis2 vref=0;
Run;

*Generate correlation coefficients between predictor variables;
title3 'Correlation Coefficients';
proc corr data = punting noprob;

*Generate 90% confidence intervals and prediction intervals;
title3 'Best Model Inferences';
proc reg data=punting1 alpha=0.1;
model Distance= R_Strength O_Strength/ clb cli clm;
run;
