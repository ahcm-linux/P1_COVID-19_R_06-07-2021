_Legends for the tables generated as output from the script.R file of folder 'codes'. TabS1 and TabS2 show supplementary results. The text of TabS2 serves to describe results for the models fitted for each analyte. In TabS2, replace [...] by the name of the analyte._

__TabS1: (Sample results)__ Sample description. Number of alive and dead COVID-19 patients in the sample split by gender.

__TabS2: (Inferential results)__ Parameter estimates and related confidence intervals (CIs) for the Linear Mixed Model (LMM)
	fitted for the analyte [...]. The column Predictors shows all independent variables (iv's) entering the LMM.
	Interactions between iv's are represented by the symbol (:). For the Gender and Outcome categorical iv's, the
	categories being compared to the reference categories for the model (Female and Alive, respectively) are shown 
	between square brackets. The Time iv is continuous and was modeled through a B-spline basis matrix for a polynomial
	spline of degree 3. Estimates for the 1st 2nd and 3rd splines terms are shown in the table. Bootstrap CIs, with 95% 
	confidence level, are shown for each estimate. CIs not including the value zero are highlighted with the symbol (*),
	and indicate statistically significant variables for the model. Bootstrap CIs were based on 1000 bootstrap samples.