# Statistical methods

## Objectives

The data analysis aimed to identify the average time trends of clinical analytes in COVID-19 patients, differentiating the trends for males and females, surviving and non-surviving patients.

## Summary of statistical results

Data analysis was based on a sample of 237 COVID-19 patients (_c.f._ table [TabS1](https://github.com/ahcm-linux/P2-R_COVID-19_2021/tree/main/outputs/tables/csv/TabS1.csv) for more details on the data sample).

Statistically significant differences between males and females patients were observed on the levels of platelets, after the 20th day of hospitalization, and levels of erythrocytes, before the 20th day of hospitalization. For the same groups of patients, no statistically significant differences were observed on the levels of monocytes, neutrophils and leukocytes.

Higher levels of erythrocytes, neutrophils and leukocytes were observed for patients that survived the hole time of hospitalization, for the patients that ended up dying, the levels of these clinical analytes were significantly lower. The mean trend of platelets was higher for surviving patients, however, asymptotic confidence intervals for the trends of surviving and non-surviving patients overlap.

The results show the need for a further analysis with a sample including a higher number of non-surviving patients. That could lead to shorter CIs and more conclusive results regarding platelets levels in surviving and non-surviving COVID-19 patients.  

## Descriptive methods

For the clinical analytes, the average trajectory for groups of patients classified by gender (male, female) and outcome type (alive, dead) were calculated using the 1st, 2nd (_i.e._ the median) and 3rd quartiles of the observed data at each time point. Hence, descriptive plots show the range covering 50% of the observed data at each time point, highlighting the median trajectory of sampled patients, for each clinical analyte (_c.f._ [FigA1](https://github.com/ahcm-linux/P2-R_COVID-19_2021/blob/main/outputs/figures/jpg_low-quality/FigA1.jpg) and [FigA2](https://github.com/ahcm-linux/P2-R_COVID-19_2021/blob/main/outputs/figures/jpg_low-quality/FigA2.jpg)).

## Inferential methods

Linear Mixed Models (LMM) assuming normally distributed random errors were specified for modeling the mean of each clinical analyte as a function of patients characteristics, including gender (male, female), outcome type (alive, dead) and hospitalization time. The set of observations belonging to any particular patient was regarded as a data cluster and modeled by the LMM as normally distributed random effects. All fitted LMM included as independent variables:

1. Gender (taking Female as reference category)
2. Outcome (taking Alive as reference category)
3. B-spline of degree 3 for Time
4. Interaction of Gender with the B-spline for Time
5. Interaction of Outcome with the B-spline for Time

Random Effect Block (REB) bootstrapping ([Chambers & Chandra, 2013](http://www.jstor.org/stable/43304840)) was used to get consistent and bias reduced confidence intervals (CIs) for LMM fixed effects. REB bootstrap CIs aimed to reliably evaluate statistical significance of model parameters, circumventing non-compliance with parametric assumptions imposed by the LMM. In that sense, the REB bootstrap is free of the distribution and dependence assumptions associated with the LMM ([Chambers & Chandra, 2013](http://www.jstor.org/stable/43304840)).

Models estimates and corresponding REB bootstrap CIs are shown in LMM tables (_TabS2_Analyte_) in the folder [__outputs/tables/html__](https://github.com/ahcm-linux/P2-R_COVID-19_2021/tree/main/outputs/tables/html).

The mean trajectories predicted by the estimated LMM estimated for each clinical analyte are shown in figures [FigA3](https://github.com/ahcm-linux/P2-R_COVID-19_2021/blob/main/outputs/figures/jpg_low-quality/FigA3.jpg) and [FigA4](https://github.com/ahcm-linux/P2-R_COVID-19_2021/blob/main/outputs/figures/jpg_low-quality/FigA4.jpg) (regarding patients gender and outcome type, respectively). In these figures, CIs are based on asymptotic results ([Pinheiro _et al._, 2020](https://CRAN.R-project.org/package=nlme)) rather then on bootstrap.

## Statistical software

The statistical analysis described above was performed using the R software version 4.0.3 ([R Core Team, 2020](https://www.R-project.org/)). Statistical graphs were created using the R packages _ggplot2_ ([Wickham, 2016](https://ggplot2.tidyverse.org)) and _sjPLot_ ([Lüdecke, 2021](https://CRAN.R-project.org/package=sjPlot)). LMMs were estimated using the R package _nlme_ ([Pinheiro _et al._, 2020](https://CRAN.R-project.org/package=nlme)). REB bootstrap was implemented using the R code provided as supplemental material in [Chambers and Chandra (2013)](https://www.tandfonline.com/doi/suppl/10.1080/10618600.2012.681216?scroll=top).

The R source code developed for the data analysis can be found at the GitHub repository https://github.com/ahcm-linux/P2-R_COVID-19_2021 (under the commit id: ).

## References

Chambers, R., & Chandra, H. (2013). A Random Effect Block Bootstrap for Clustered Data. Journal of Computational and Graphical Statistics, 22(2), 452-470. Retrieved June 8, 2021, from http://www.jstor.org/stable/43304840

H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016. ISBN: 978-3-319-24277-4

Lüdecke D (2021). _sjPlot: Data Visualization for Statistics in Social
Science_. R package version 2.8.7, URL https://CRAN.R-project.org/package=sjPlot.

Pinheiro J, Bates D, DebRoy S, Sarkar D, R Core Team (2020). _nlme:
Linear and Nonlinear Mixed Effects Models_. R package version 3.1-149,
URL https://CRAN.R-project.org/package=nlme.

R Core Team (2020). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/