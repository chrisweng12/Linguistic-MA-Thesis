# Production study of  loss of final place contrasts among Mandarin-Min bilinguals



## Statistic for the data



### Library for the scripts

```r
library(lme4)
```



### Baseline Condition

1. Set the baseline as context /at/

   ```r
   new_dat$Vowel = factor(new_dat$Vowel, levels=c('a','i'))
   new_dat$Final = factor(new_dat$Final, levels=c('t','k','p','v'))
   ```



### Mix-effect logistic regression

1. Run the model with ...

   1. Simple effect
      1. Final
      2. Vowel
   2. Interaction
      1. Final and Vowel
   3. Random intercept
      1. Filename
      2. UR

   ```r
   model <- glmer(Result ~  Final*Vowel +  (1|filename) + (1|UR), new_dat , family=binomial(link="logit"))
   summary(model)
   ```

   

2. Results

   ```r
   Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
    Family: binomial  ( logit )
   Formula: Result ~ Final + Vowel + Final * Vowel + (1 | filename) + (1 |      UR)
      Data: new_dat
   
        AIC      BIC   logLik deviance df.resid 
     1384.4   1438.5   -682.2   1364.4     1641 
   
   Scaled residuals: 
        Min       1Q   Median       3Q      Max 
   -11.5408  -0.3652   0.1669   0.5151   2.7635 
   
   Random effects:
    Groups   Name        Variance Std.Dev.
    UR       (Intercept) 0.4111   0.6412  
    filename (Intercept) 1.5163   1.2314  
   Number of obs: 1651, groups:  UR, 24; filename, 15
   
   Fixed effects:
                 Estimate Std. Error z value Pr(>|z|)    
   (Intercept)   -0.09088    0.51904  -0.175   0.8610    
   Finalk         0.43600    0.57621   0.757   0.4493    
   Finalp         1.39727    0.58153   2.403   0.0163 *  
   Finalv         4.99525    0.80829   6.180 6.41e-10 ***
   Voweli         0.45943    0.57202   0.803   0.4219    
   Finalk:Voweli  1.28992    0.82013   1.573   0.1158    
   Finalp:Voweli -0.27748    0.81621  -0.340   0.7339    
   Finalv:Voweli -1.23768    1.05434  -1.174   0.2404    
   ---
   Signif. codes:  0 â***â 0.001 â**â 0.01 â*â 0.05 â.â 0.1 â â 1
   
   Correlation of Fixed Effects:
               (Intr) Finalk Finalp Finalv Voweli Fnlk:V Fnlp:V
   Finalk      -0.558                                          
   Finalp      -0.553  0.500                                   
   Finalv      -0.398  0.360  0.361                            
   Voweli      -0.562  0.508  0.504  0.364                     
   Finalk:Vowl  0.393 -0.702 -0.349 -0.249 -0.696              
   Finalp:Vowl  0.394 -0.356 -0.711 -0.253 -0.700  0.488       
   Finalv:Vowl  0.305 -0.275 -0.273 -0.757 -0.542  0.378  0.380
   convergence code: 0
   Model failed to converge with max|grad| = 0.00268493 (tol = 0.001, component 1)
   ```

   