# read in data
data_combined_new <- read.csv(choose.files(),stringsAsFactors = FALSE)
summary(data_combined_new)

for (i in 1:nrow(data_combined_new)){
  if (data_combined_new$Transition[i] == "large"){
    data_combined_new$Transition[i] <- "talker 2 (large)"
  } 
  else if (data_combined_new$Transition[i] == "small"){
    data_combined_new$Transition[i] <- "talker 1 (small)"
  }
}

for (i in 1:nrow(data_combined_new)){
  if (data_combined_new$Group[i] == "TW_fluent"){
    data_combined_new$Group[i] <- "TSM_fluent"
  } 
  else if (data_combined_new$Group[i] == "TW_weak"){
    data_combined_new$Group[i] <- "TSM_weak"
  }
}

summary(data_combined_new)
names(data_combined_new)[names(data_combined_new) == 'Result'] <- 'Accuracy'
names(data_combined_new)[names(data_combined_new) == 'Transition'] <- 'Talker'
names(data_combined_new)[names(data_combined_new) == 'Coda'] <- 'Final'
data_combined_new$word <- paste(data_combined_new$Onset,data_combined_new$Vowel,data_combined_new$Final,sep="")
data_combined_new$Talker = as.factor(data_combined_new$Talker)
data_combined_new$Group = as.factor(data_combined_new$Group)
View(data_combined_new)

# stats
library(lme4)
data_combined_new$Group = factor(data_combined_new$Group, levels=c('TSM_fluent','Korean','TSM_weak'))
data_combined_new$Vowel = factor(data_combined_new$Vowel, levels=c('a','i'))
data_combined_new$Final = factor(data_combined_new$Final, levels=c('t','p','k'))
data_combined_new$Talker <- factor(data_combined_new$Talker, levels =c('talker 1 (small)', 'talker 2 (large)'))

model2 <- glmer(Accuracy ~ Final*Vowel + Group*Final +
                    Group*Vowel*Talker + (1|Subject) + (1|word),data_combined_new , family=binomial(link="logit"))
summary(model2)
"""
Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
 Family: binomial  ( logit )
Formula: Accuracy ~ Final * Vowel + Group * Final + Group * Vowel * Talker +  
    (1 | Subject) + (1 | word)
   Data: data_combined_new

     AIC      BIC   logLik deviance df.resid 
 13826.4  13993.7  -6891.2  13782.4    14818 

Scaled residuals: 
     Min       1Q   Median       3Q      Max 
-16.6318  -0.6352   0.2693   0.5639   3.2282 

Random effects:
 Groups  Name        Variance Std.Dev.
 Subject (Intercept) 0.5113   0.7151  
 word    (Intercept) 0.2611   0.5110  
Number of obs: 14840, groups:  Subject, 53; word, 35

Fixed effects:
                                             Estimate Std. Error z value Pr(>|z|)    
(Intercept)                                  0.163357   0.297804   0.549 0.583323    
Finalp                                       0.686134   0.339997   2.018 0.043585 *  
Finalk                                      -0.121490   0.337218  -0.360 0.718644    
Voweli                                      -0.003167   0.318304  -0.010 0.992063    
GroupKorean                                  1.537373   0.295137   5.209 1.90e-07 ***
GroupTSM_weak                                0.214858   0.259199   0.829 0.407144    
Talkertalker 2 (large)                       0.525263   0.098647   5.325 1.01e-07 ***
Finalp:Voweli                                0.288957   0.446768   0.647 0.517779    
Finalk:Voweli                                0.092051   0.434677   0.212 0.832288    
Finalp:GroupKorean                           2.077325   0.256348   8.104 5.34e-16 ***
Finalk:GroupKorean                           1.185047   0.146251   8.103 5.37e-16 ***
Finalp:GroupTSM_weak                         0.975932   0.123917   7.876 3.39e-15 ***
Finalk:GroupTSM_weak                         0.114613   0.104167   1.100 0.271208    
Voweli:GroupKorean                           0.619885   0.208176   2.978 0.002904 ** 
Voweli:GroupTSM_weak                         0.130277   0.130026   1.002 0.316379    
GroupKorean:Talkertalker 2 (large)           0.356571   0.234137   1.523 0.127780    
GroupTSM_weak:Talkertalker 2 (large)        -0.042938   0.141239  -0.304 0.761121    
Voweli:Talkertalker 2 (large)               -0.454179   0.129678  -3.502 0.000461 ***
Voweli:GroupKorean:Talkertalker 2 (large)   -2.231655   0.295384  -7.555 4.19e-14 ***
Voweli:GroupTSM_weak:Talkertalker 2 (large) -0.365536   0.185123  -1.975 0.048318 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
"""
summary(model2)$coefficients  
write.csv(summary(model2)$coefficients, file = 'stats for rerunning.csv', row.names = TRUE )




