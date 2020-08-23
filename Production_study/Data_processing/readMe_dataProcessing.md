# Production study of  loss of final place contrasts among Mandarin-Min bilinguals



## Data processing



#### Libraries for the script

```R
library(dplyr)
library(tidyverse) 
library(stringr)
```



#### Handling the  unprocessed data



##### 1. Read in 'unprocessed_data.csv' and save it in the variable 'production_all' 

```r
production_all = read.csv(choose.files())
```



##### 2. Make a new column for the comparison between UR and SR

1. This column gets to check whether participants are producing the token correctly or not
2. Save the comparison result in the 'Lexical. Retrieval' column 

```r
production_all["Lexical.retrieval"]=NA
production_new <- production_all %>% 
  dplyr::mutate(Lexical.retrieval = ifelse(UR == SR, 1, 0))
View(production_new)
```



##### 3. Make another column for coda only

1. This column is retrieved from the UR column by using str_sub() function 
2. For those without finals, the representation form is 'v'

```r
production_new["Final"] = NA
production_new$Final = str_sub(production_new$UR,-1,-1)
production_new$Final = factor(production_new$Final, levels=c('p','t','k','i','a','v'))
production_new$Final[production_new$Final == 'a'] <- 'v'
production_new$Final[production_new$Final == 'i'] <- 'v'
```



##### 4. Make another column for vowel only

1. This column is retrieved from the UR column by using str_sub() function
2. For those that has final, it retrieves the second character of the string counting from the back 

```r
production_new$Vowel = str_sub(production_new$UR,-1,-1)
for(i in 1:nrow(production_new)){
  if(production_new$Vowel[i] == "p"){
    production_new$Vowel[i]= str_sub(production_new$UR[i],-2,-2)
  } else if (production_new$Vowel[i] == "t"){
    production_new$Vowel[i]= str_sub(production_new$UR[i],-2,-2)
  } else if (production_new$Vowel[i] == "k"){
    production_new$Vowel[i]= str_sub(production_new$UR[i],-2,-2)
  } 
}
```



##### 5. Judging three coders consistency

1. Add a new column named 'Inter_corr', for checking the consistency of three coders
2. If more than two coders agreed on what they perceived, it will be counted as 1
3. If three of them doesn't match with each other, it will be count as 0

```r
production_new$Inter_corr = NA
for (i in 1:nrow(production_new)){
  if (production_new$Felix[i] == production_new$Illa[i] &&
      production_new$Felix[i] == production_new$Chris[i]){
    production_new$Inter_corr[i] <- 1
  } else if (production_new$Felix[i] == production_new$Illa[i] || 
             production_new$Felix[i] == production_new$Chris[i] ||
             production_new$Illa[i] == production_new$Chris[i] ){
    production_new$Inter_corr[i] <- 1
  } 
  else {
    production_new$Inter_corr[i] <- 0
  }
}
View(production_new)
```



##### 6. Check what the coders are perceiving

1. Three conditions for what these three coders perceived
   1. All three coders have agreed on what they perceive
   2. Two of these three coders agreed on what they perceive
   3. None of the coders have a consistent agreement
2. Add a column 'Calculate' for checking what finals did these coders perceive
   1. Create a function for checking the duplicate value
3. For the condition that none coders have agreement on, the value will be 'none'

```r
duplicating = function(data){
  nest = strsplit(data, "")[[1]]
  value = unique(nest[ duplicated(nest)])
  return(value)
}

production_new$Calculate = NA

for (i in 1:nrow(production_new)){
    production_new$Calculate[i] <- paste(production_new$Felix[i] ,production_new$Illa[i], production_new$Chris[i], sep="") 
}

for (i in 1:nrow(production_new)){
  if (production_new$Inter_corr[i] == 0){
    production_new$Calculate[i] <- "none"
  }
  else if (production_new$Inter_corr[i] == 1){
    production_new$Calculate[i] <- duplicating(production_new$Calculate[i])
  } 
}
View(production_new)
```



##### 7. Add another column 'Accuracy' for classifying the result into four different categories

1. Unident
   1. When the column of Lexical retrieval is 1 (participants did not mispronounce the token initial or vowel) but the Inter_corr column has the value 0 (none of the coder has consistent answer)
2. Correct
   1. When the column of Lexical retrieval is 1 (participants did not mispronounce the token initial or vowel) and also Inter_corr column has the value 1 (more than two coders agreed on what they perceived)
3. Incorrect
   1. When the column of Lexical retrieval is 1 (participants did not mispronounce the token initial or vowel),  Inter_corr column has the value 1 (more than two coders agreed on what they perceived), the Calculate column is not 'none' (the condition that none of coders have agreement on), and the perceived final mismatch the underlying  form
4. Excluded
   1. When the column of Lexical retrieval is 0 (participants mispronounce the token initial or vowel)

```r
production_new$Accuracy = NA
for (i in 1:nrow(production_new)){
  if (production_new$Lexical.retrieval[i] == 0){
    production_new$Accuracy[i] <- 'excluded'
  }
  else if (production_new$Lexical.retrieval[i] == 1){
    if (production_new$Inter_corr[i] == 0){
      production_new$Accuracy[i] <- 'unident'
    }
    else if (production_new$Inter_corr[i] == 1 &&
             production_new$Calculate != "none" &&
             production_new$Final[i] != production_new$Calculate[i]){
      production_new$Accuracy[i] <- 'wrong'
    }
    else if (production_new$Inter_corr[i] == 1 && 
      production_new$Calculate[i] == production_new$Final[i]){
      production_new$Accuracy[i] <- 'correct'
    }
  } 
}
  
View(production_new)
```



##### 8. Add another column for saving the mispronunciation result

1. If the Accuracy column is 'wrong', then paste the UR form with '>' and what the coders perceived in the Calculate column

```r
production_new$Misproduction = NA
for (i in 1:nrow(production_new)){
  if (production_new$Accuracy[i] == 'wrong'){
    production_new$Misproduction[i] <- paste(production_new$Final[i] ,'>', production_new$Calculate[i]) 
  }else {
    production_new$Misproduction[i] <- 0
  }
}
View(production_new)
```



##### 9. Save the result as 'Coding_result_update.csv'

```r
write.csv(production_new,file="Coding_result_update.csv",row.names=FALSE)
```



