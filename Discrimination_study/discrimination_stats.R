library(lme4)
data = read.csv(choose.files())
View(data)
##### model 1 #####
data$Group = factor(data$Group, levels=c('TSM_fluent','Korean','TSM_weak'))
data$Vowel = factor(data$Vowel, levels=c('a','i'))
data$ConsPair = factor(data$ConsPair, levels=c('k-t','p-t','k-p'),labels = c('t-k','p-t','p-k'))
data$Transition = factor(data$Transition, levels =c('talker 1', 'talker 2'))

model <- glmer(Accuracy ~ ConsPair*Vowel + Group*ConsPair  +
                   Group*Vowel*Transition + (1|Subject) + (1|XTokens),data,
               family=binomial(link="logit"))
summary(model)
"""
Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 Family: binomial  ( logit )
Formula: Accuracy ~ ConsPair * Vowel + Group * ConsPair + Group * Vowel *  
    Transition + (1 | Subject) + (1 | XTokens)
   Data: data

     AIC      BIC   logLik deviance df.resid 
 12493.1  12658.6  -6224.5  12449.1    13658 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-6.6360  0.2113  0.3568  0.5169  1.9770 

Random effects:
 Groups  Name        Variance Std.Dev.
 XTokens (Intercept) 0.3143   0.5606  
 Subject (Intercept) 0.3214   0.5670  
Number of obs: 13680, groups:  XTokens, 94; Subject, 57

Fixed effects:
                                         Estimate Std. Error z value Pr(>|z|)    
(Intercept)                              1.696340   0.214870   7.895 2.91e-15 ***
ConsPairp-t                              0.366405   0.131099   2.795  0.00519 ** 
ConsPairp-k                             -0.397916   0.127270  -3.127  0.00177 ** 
Voweli                                   0.037147   0.220174   0.169  0.86602    
GroupKorean                              0.602450   0.245119   2.458  0.01398 *  
GroupTSM_weak                           -0.618413   0.223053  -2.772  0.00556 ** 
Transitiontalker 2                       0.006448   0.213373   0.030  0.97589    
ConsPairp-t:Voweli                      -1.149101   0.155391  -7.395 1.41e-13 ***
ConsPairp-k:Voweli                       0.174592   0.159652   1.094  0.27414    
ConsPairp-t:GroupKorean                  0.144082   0.151813   0.949  0.34258    
ConsPairp-k:GroupKorean                  0.255646   0.151436   1.688  0.09138 .  
ConsPairp-t:GroupTSM_weak                0.114692   0.130056   0.882  0.37785    
ConsPairp-k:GroupTSM_weak                0.063850   0.128021   0.499  0.61796    
Voweli:GroupKorean                      -0.125056   0.176298  -0.709  0.47811    
Voweli:GroupTSM_weak                     0.231779   0.146032   1.587  0.11247    
GroupKorean:Transitiontalker 2           0.035591   0.183170   0.194  0.84593    
GroupTSM_weak:Transitiontalker 2         0.047401   0.147599   0.321  0.74810    
Voweli:Transitiontalker 2                0.207991   0.286388   0.726  0.46768    
Voweli:GroupKorean:Transitiontalker 2   -0.378016   0.248148  -1.523  0.12767    
Voweli:GroupTSM_weak:Transitiontalker 2  0.258731   0.209958   1.232  0.21784    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation matrix not shown by default, as p = 20 > 12.
Use print(x, correlation=TRUE)  or
    vcov(x)        if you need it

convergence code: 0
Model failed to converge with max|grad| = 0.00182115 (tol = 0.001, component 1)
"""

summary(model)$coefficients  
write.csv(summary(model)$coefficients, file = 'discrimination stats.csv', row.names = TRUE )

##### model 2 #####
data$Group = factor(data$Group, levels=c('TSM_fluent','Korean','TSM_weak'))
data$Vowel = factor(data$Vowel, levels=c('a','i'))
data$ConsPair = factor(data$ConsPair, levels=c('k-t','p-t','k-p'),labels = c('t-k','p-t','p-k'))
data$Transition = factor(data$Transition, levels =c('talker 1', 'talker 2'))

model5 <- glmer(Accuracy ~ Group*ConsPair  + 
                Group*Vowel*Transition + ConsPair*Vowel*Transition + (1|Subject) + (1|XTokens),data,
               family=binomial(link="logit"))

summary(model5)
"""
Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 Family: binomial  ( logit )
Formula: Accuracy ~ Group * ConsPair + Group * Vowel * Transition + ConsPair *  
    Vowel * Transition + (1 | Subject) + (1 | XTokens)
   Data: data

     AIC      BIC   logLik deviance df.resid 
 12475.8  12671.4  -6211.9  12423.8    13654 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-6.8613  0.2101  0.3545  0.5162  2.1815 

Random effects:
 Groups  Name        Variance Std.Dev.
 XTokens (Intercept) 0.2938   0.5420  
 Subject (Intercept) 0.3231   0.5684  
Number of obs: 13680, groups:  XTokens, 94; Subject, 57

Fixed effects:
                                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)                              1.77367    0.22057   8.041 8.88e-16 ***
GroupKorean                              0.59306    0.24597   2.411  0.01590 *  
GroupTSM_weak                           -0.61636    0.22373  -2.755  0.00587 ** 
ConsPairp-t                              0.07280    0.16720   0.435  0.66327    
ConsPairp-k                             -0.33285    0.16023  -2.077  0.03777 *  
Voweli                                  -0.25945    0.23252  -1.116  0.26451    
Transitiontalker 2                      -0.14166    0.23661  -0.599  0.54935    
GroupKorean:ConsPairp-t                  0.16026    0.15246   1.051  0.29319    
GroupTSM_weak:ConsPairp-t                0.10982    0.13044   0.842  0.39982    
GroupKorean:ConsPairp-k                  0.26035    0.15131   1.721  0.08531 .  
GroupTSM_weak:ConsPairp-k                0.06188    0.12802   0.483  0.62881    
GroupKorean:Voweli                      -0.12252    0.17578  -0.697  0.48579    
GroupTSM_weak:Voweli                     0.23388    0.14564   1.606  0.10830    
GroupKorean:Transitiontalker 2           0.04390    0.18344   0.239  0.81087    
GroupTSM_weak:Transitiontalker 2         0.04570    0.14782   0.309  0.75720    
Voweli:Transitiontalker 2                0.75932    0.33206   2.287  0.02221 *  
ConsPairp-t:Voweli                      -0.34619    0.22308  -1.552  0.12069    
ConsPairp-k:Voweli                       0.16733    0.21413   0.781  0.43454    
ConsPairp-t:Transitiontalker 2           0.57499    0.20493   2.806  0.00502 ** 
ConsPairp-k:Transitiontalker 2          -0.12692    0.19573  -0.648  0.51672    
GroupKorean:Voweli:Transitiontalker 2   -0.38683    0.24913  -1.553  0.12049    
GroupTSM_weak:Voweli:Transitiontalker 2  0.25791    0.21075   1.224  0.22104    
ConsPairp-t:Voweli:Transitiontalker 2   -1.50399    0.30945  -4.860 1.17e-06 ***
ConsPairp-k:Voweli:Transitiontalker 2    0.02094    0.32019   0.065  0.94786    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Correlation matrix not shown by default, as p = 24 > 12.
Use print(x, correlation=TRUE)  or
    vcov(x)        if you need it

convergence code: 0
Model failed to converge with max|grad| = 0.00442352 (tol = 0.001, component 1)
"""
summary(model5)$coefficients  
write.csv(summary(model5)$coefficients, file = 'discrimination stats update.csv', row.names = TRUE )
