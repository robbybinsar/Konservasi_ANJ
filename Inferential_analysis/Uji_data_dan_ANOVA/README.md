---
title: "Homogeneity, Normality, and One Way ANOVA"
author: "Robby"
date: "5/4/2020"
output: 
  html_document:
    keep_md: TRUE
---



Parametric analysis with data validity using normality and homogeneity test before testing with One Way ANOVA. This analysis has been chosen as perceived suitable with the case provided.

`Uji_Data_dan_ANOVA_Satu.Arah.R` is an R script that consists of **Homogeneity, Normality, One Way ANOVA, and Post-Hoc Tukey Test**

### 1. Homogeneity

The test for homogeneity is a method, based on the chi-square statistic, for testing whether two or more multinomial distributions are equal.

**Levene Test**

* `leveneTest` ( Levene 1960) is used to test if k samples have equal variances. Equal variances across samples is called homogeneity of variance. Some statistical tests, for example the analysis of variance, assume that variances are equal across groups or samples. The Levene test can be used to verify that assumption (*if p > 0.05, homogeneity assumption is fulfilled*)

### 2. Normality

Test of the normality is an important step for deciding the measures of central tendency and statistical methods for data analysis. A normality test is used to determine whether sample data has been drawn from a normally distributed population (within some tolerance). A number of statistical tests, such as the Student's t-test and the one-way and two-way ANOVA require a normally distributed sample population. If the assumption of normality is not valid, the results of the tests will be unreliable.

**Shapiro Test**

* `shapiro.test` The test rejects the hypothesis of normality when the p-value is *less than or equal to 0.05*.  Failing the normality test allows you to state with 95% confidence the data does not fit the normal distribution.  Passing the normality test only allows you to state no significant departure from normality was found. Shapiro test usually being used for *n < 50*
* Shapiro-Wilk’s method is widely recommended for normality test and it provides better power than K-S. It is based on the correlation between the data and the corresponding normal scores.

> Note that, normality test is sensitive to sample size. Small samples most often pass normality tests. Therefore, it’s important to combine visual inspection and significance test in order to take the right decision.

**Kolmogorov-Smirnov Tests**

* `ks.test`The Kolmogorov-Smirnov test (Chakravart, Laha, and Roy, 1967) is used to decide if a sample comes from a population with a specific distribution.
  + n >= 50
  + p > 0.05, normality assumption is fulfilled
  
**Lilliefors (Kolmogorov-Smirnov) test for normality**

* `lillie.test` The Lilliefors (Kolomorov-Smirnov) test is the most famous EDF omnibus test for normality. Compared to the Anderson-Darling test and the Cramer-von Mises test it is known to perform worse. Although the test statistic obtained from lillie.test(x) is the same as that obtained from ks.test(x, "pnorm", mean(x), sd(x)), it is not correct to use the p-value from the latter for the composite hypothesis of normality (mean and variance unknown), since the distribution of the test statistic is different when the parameters are estimated.
  + n >= 50
  
##### **Normality test with graphic**

**Quantile-Comparison Plot**

* `qqPlot` Plots empirical quantiles of a variable, or of studentized residuals from a linear model, against theoretical quantiles of a comparison distribution.

**Box and Whisker Plot**

* `boxplot` Produce box-and-whisker plot(s) of the given (grouped) values.

### 3. One Way ANOVA

One-Way ANOVA ("analysis of variance") compares the means of two or more independent groups in order to determine whether there is statistical evidence that the associated population means are significantly different. One-Way ANOVA is a parametric test.

* One-Way ANOVA
  + Calculate Linear Model: `lm` function is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance
  + calculate ANOVA: `anova` Compute analysis of variance (or deviance) tables for one or more fitted model objects.
  
### 4. Post-Hoc Tukey

The Tukey Test (or Tukey procedure), also called Tukey’s Honest Significant Difference test, is a post-hoc test based on the studentized range distribution. An ANOVA test can tell you if your results are significant overall, but it won’t tell you exactly where those differences lie. After you have run an ANOVA and found significant results, then you can run Tukey’s HSD to find out which specific groups’s means (compared with each other) are different. The test compares all possible pairs of means.

  + `HSD.test` It makes multiple comparisons of treatments by means of Tukey. The level by alpha default is 0.05.
  
### 5. Generate Plot

with `ggplot` function generates plot summarizes the mean of each treatment, SE, and Post-Hoc annotation.

### 6. Real case application

> Please refer to `uji_normality_homogeneity.md` for normality and homogeneity test on a real case example

> Pleasee refer to `Analisis_ANOVA_dan_Uji_lanjut-Anggrek_tanah_(Tinggi).md` fot one way ANOVA and post hoc test application on a real case example.


  




