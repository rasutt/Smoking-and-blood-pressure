Effect of smoking on high-blood pressure
================
Robin Aldridge-Sutton
2023-05-16

The data in the following table was obtained from a study on smoking and
high blood pressure. The researcher wants to know whether people who
smoke are more likely to have high blood pressure.

``` r
# Gender, male as reference
g = rep(0:1, each = 12)

# Age group, using 20-24 as reference age group
ag = rbind(0, diag(5), 0, diag(5))[rep(1:12, each = 2), ]
colnames(ag) = paste0(c(35, 50, 60, 70, 80), "-")

# Smoking
s = rep(0:1, 12)

# High-blood pressure
hbp = cbind(c(6, 4, 18, 22, 30, 55, 22, 13, 12, 14, 10, 8, 
              2, 9, 12, 18, 28, 22, 24, 32, 22, 41, 11, 4),
            c(18, 11, 28, 17, 22, 44, 30, 52, 32, 26, 22, 5,
              14, 12, 27, 11, 40, 37, 38, 28, 39, 35, 19, 6))
colnames(hbp) = c("yes", "no")
cbind(g, ag, s, hbp)
```

    ##       g 35- 50- 60- 70- 80- s yes no
    ##  [1,] 0   0   0   0   0   0 0   6 18
    ##  [2,] 0   0   0   0   0   0 1   4 11
    ##  [3,] 0   1   0   0   0   0 0  18 28
    ##  [4,] 0   1   0   0   0   0 1  22 17
    ##  [5,] 0   0   1   0   0   0 0  30 22
    ##  [6,] 0   0   1   0   0   0 1  55 44
    ##  [7,] 0   0   0   1   0   0 0  22 30
    ##  [8,] 0   0   0   1   0   0 1  13 52
    ##  [9,] 0   0   0   0   1   0 0  12 32
    ## [10,] 0   0   0   0   1   0 1  14 26
    ## [11,] 0   0   0   0   0   1 0  10 22
    ## [12,] 0   0   0   0   0   1 1   8  5
    ## [13,] 1   0   0   0   0   0 0   2 14
    ## [14,] 1   0   0   0   0   0 1   9 12
    ## [15,] 1   1   0   0   0   0 0  12 27
    ## [16,] 1   1   0   0   0   0 1  18 11
    ## [17,] 1   0   1   0   0   0 0  28 40
    ## [18,] 1   0   1   0   0   0 1  22 37
    ## [19,] 1   0   0   1   0   0 0  24 38
    ## [20,] 1   0   0   1   0   0 1  32 28
    ## [21,] 1   0   0   0   1   0 0  22 39
    ## [22,] 1   0   0   0   1   0 1  41 35
    ## [23,] 1   0   0   0   0   1 0  11 19
    ## [24,] 1   0   0   0   0   1 1   4  6

``` r
# Fit binomial regression model with all interactions
brmais = glm(hbp ~ ag * g * s, family = binomial)
summary(brmais)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ ag * g * s, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##  [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept) -1.09861    0.47140  -2.331   0.0198 *
    ## ag35-        0.65678    0.55990   1.173   0.2408  
    ## ag50-        1.40877    0.54864   2.568   0.0102 *
    ## ag60-        0.78846    0.54864   1.437   0.1507  
    ## ag70-        0.11778    0.58035   0.203   0.8392  
    ## ag80-        0.31015    0.60636   0.511   0.6090  
    ## g           -0.84730    0.89087  -0.951   0.3416  
    ## s            0.08701    0.75042   0.116   0.9077  
    ## ag35-:g      0.47820    1.00264   0.477   0.6334  
    ## ag50-:g      0.18047    0.96600   0.187   0.8518  
    ## ag60-:g      0.69792    0.96975   0.720   0.4717  
    ## ag70-:g      1.25561    0.98961   1.269   0.2045  
    ## ag80-:g      1.08921    1.04050   1.047   0.2952  
    ## ag35-:s      0.61265    0.87102   0.703   0.4818  
    ## ag50-:s     -0.17402    0.82633  -0.211   0.8332  
    ## ag60-:s     -1.16315    0.85911  -1.354   0.1758  
    ## ag70-:s      0.27478    0.88747   0.310   0.7569  
    ## ag80-:s      1.17145    1.01665   1.152   0.2492  
    ## g:s          1.57122    1.15282   1.363   0.1729  
    ## ag35-:g:s   -0.96747    1.33843  -0.723   0.4698  
    ## ag50-:g:s   -1.64741    1.25773  -1.310   0.1903  
    ## ag60-:g:s    0.09799    1.28019   0.077   0.9390  
    ## ag70-:g:s   -1.20226    1.29520  -0.928   0.3533  
    ## ag80-:g:s   -2.68860    1.53612  -1.750   0.0801 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 6.6209e+01  on 23  degrees of freedom
    ## Residual deviance: 2.5091e-14  on  0  degrees of freedom
    ## AIC: 143.34
    ## 
    ## Number of Fisher Scoring iterations: 4

When all interactions between smoking and the other predictor variables
are included there are no statistically significant effects of smoking,
so remove the second-order interactions and check each of the
first-order interactions.

``` r
# Fit binomial regression model with interaction between smoking and age-group
brmisag = glm(hbp ~ g + ag * s, family = binomial)
summary(brmisag)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ g + ag * s, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -2.69676  -0.73944   0.00703   0.64266   2.60137  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.41322    0.39884  -3.543 0.000395 ***
    ## g            0.06652    0.12914   0.515 0.606488    
    ## ag35-        0.77640    0.45591   1.703 0.088573 .  
    ## ag50-        1.30882    0.43597   3.002 0.002681 ** 
    ## ag60-        0.98607    0.43937   2.244 0.024814 *  
    ## ag70-        0.63807    0.44756   1.426 0.153964    
    ## ag80-        0.71181    0.47792   1.489 0.136388    
    ## s            0.80372    0.52653   1.526 0.126900    
    ## ag35-:s      0.16150    0.62431   0.259 0.795881    
    ## ag50-:s     -0.77481    0.58115  -1.333 0.182450    
    ## ag60-:s     -0.98402    0.59064  -1.666 0.095710 .  
    ## ag70-:s     -0.17572    0.59582  -0.295 0.768054    
    ## ag80-:s     -0.04419    0.72383  -0.061 0.951315    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 66.209  on 23  degrees of freedom
    ## Residual deviance: 32.346  on 11  degrees of freedom
    ## AIC: 153.68
    ## 
    ## Number of Fisher Scoring iterations: 4

The interaction between smoking and age-group not statistically
significant.

``` r
# Fit binomial regression model with interaction between smoking and gender
brmisg = glm(hbp ~ ag + g * s, family = binomial)
summary(brmisg)
```

    ## 
    ## Call:
    ## glm(formula = hbp ~ ag + g * s, family = binomial)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -3.11698  -0.73515   0.08696   1.06925   1.61867  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.07592    0.27881  -3.859 0.000114 ***
    ## ag35-        0.84168    0.30569   2.753 0.005899 ** 
    ## ag50-        0.93507    0.28618   3.267 0.001085 ** 
    ## ag60-        0.49415    0.29110   1.698 0.089594 .  
    ## ag70-        0.55671    0.29304   1.900 0.057464 .  
    ## ag80-        0.61147    0.34280   1.784 0.074462 .  
    ## g           -0.16350    0.18222  -0.897 0.369587    
    ## s            0.08771    0.18225   0.481 0.630304    
    ## g:s          0.50865    0.25558   1.990 0.046575 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 66.209  on 23  degrees of freedom
    ## Residual deviance: 39.900  on 15  degrees of freedom
    ## AIC: 153.24
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Standard error, log-odds ratio, and confidence interval
se = sqrt(0.18225^2 + 0.25558^2)
lor = 0.08771 + 0.50865
exp(lor + c(-1, 0, 1) * 1.96 * se)
```

    ## [1] 0.981284 1.815498 3.358899

The interaction between gender and smoking is statistically significant,
p = 0.047. The effect of smoking for males is not statistically
significant, p = 0.63. For females the odds of high blood-pressure are
estimate to be 1.8 times higher for smokers than non-smokers, but this
is not quite statistically significant (95% confidence interval 0.98,
3.36).
