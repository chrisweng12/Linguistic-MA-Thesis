### data_processing ###

# Read all the data in a folder than bind it with the elements we need  
library(dplyr)
library(tidyverse) 
library(stringr)
production_all <-
  list.files(path = "C:/run",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
View(production_all)
summary(production_all)
#write.csv(production_all,file="unprocessed_data.csv",row.names=FALSE)
# Read the unprocessed_data.csv 
production_all = read.csv(choose.files())

# making a UR and SR comparison column
production_all["Lexical.retrieval"]=NA
production_new <- production_all %>% 
  dplyr::mutate(Lexical.retrieval = ifelse(UR == SR, 1, 0))
View(production_new)

# making a Coda column
production_new["Final"] = NA
production_new$Final = str_sub(production_new$UR,-1,-1)
View(production_new)
production_new$Final = factor(production_new$Final, levels=c('p','t','k','i','a','v'))
production_new$Final[production_new$Final == 'a'] <- 'v'
production_new$Final[production_new$Final == 'i'] <- 'v'
View(production_new)

# retrieving vowel from word 
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
summary(production_new)

# judging three coders consistency
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

# Find out the duplicate value (use for more than two coders' agreement)
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

# classifying the accuracy 
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

#  Matching misproduction with UR and coder 
production_new$Misproduction = NA
for (i in 1:nrow(production_new)){
  if (production_new$Accuracy[i] == 'wrong'){
    production_new$Misproduction[i] <- paste(production_new$Final[i] ,'>', production_new$Calculate[i]) 
  }else {
    production_new$Misproduction[i] <- 0
  }
}
View(production_new)
write.csv(production_new,file="Coding_result_update.csv",row.names=FALSE)