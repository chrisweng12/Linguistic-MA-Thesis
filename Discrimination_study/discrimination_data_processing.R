# Read all the data in a folder than bind it with the elements we need  
library(dplyr)
library(tidyverse) 
discrimination_new <-
  list.files(path = "C:/run/discrimination",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
View(discrimination_new)
data_new=discrimination_new[,c("ExperimentName","Subject","Sex","Name","GROUP","Running","ATokens","XTokens","BTokens","optionnn.RT","optionnn.RTTime","optionnn.RESP","Answer")]
View(data_new)
data_new["Transition"]=NA
library(stringr)
data_new['ExperimentName'] <- paste(str_sub(data_new$ExperimentName,7,16))
View(data_new)
for(i in 1:nrow(data_new)){
  if (data_new$Running[i] == 'Block1'){
    data_new$Transition[i] <- 'talker 1'
  }
  if (data_new$Running[i] == 'Block2'){
    data_new$Transition[i] <- 'talker 1'
  }
  if (data_new$Running[i] == 'Block3'){
    data_new$Transition[i] <- 'talker 2'
  }
  if (data_new$Running[i] == 'Block4'){
    data_new$Transition[i] <- 'talker 2'
  }
}
data_new = subset(data_new,data_new$Running!="Practice")
View(data_new)
data_new["Result"]=NA
data_new <- data_new %>% 
  dplyr::mutate(Result = ifelse(optionnn.RESP == Answer, 1, 0))

colnames(data_new)[which(colnames(data_new) %in% 'Result' )] <- 'Accuracy'

data_new["Vowel"]=NA
data_new['Vowel'] <- paste(str_sub(data_new$XTokens,-7,-7))
View(data_new)

data_new["Onset"]=NA
data_new['Onset'] <- paste(str_sub(data_new$XTokens,1,-8))
View(data_new)

data_new['A'] = NA
data_new$A <- paste(str_sub(data_new$ATokens,-6,-6))

data_new['B'] = NA
data_new$B <- paste(str_sub(data_new$BTokens,-6,-6))

data_new['ConsPair']=NA
for (i in 1:nrow(data_new)){
    data_new$ConsPair[i] <- paste(data_new$A[i] , '-' ,data_new$B[i]) 
}

for (i in 1:nrow(data_new)){
  if (data_new$ConsPair[i] == 'p-k'){
    data_new$ConsPair[i] = 'k-p'
  }
  if (data_new$ConsPair[i] == 't-k'){
    data_new$ConsPair[i] = 'k-t'
  }
  if (data_new$ConsPair[i] == 't-p'){
    data_new$ConsPair[i] = 'p-t'
  }
}

data_new$A <- NULL
data_new$B <- NULL

View(data_new)
data_new$Group = NA

for (i in 1:nrow(data_new)){
  if (data_new$GROUP[i] == 'TW_fluent'){
    data_new$Group[i] <- 'TSM_fluent'
  }
  if (data_new$GROUP[i] == 'TW_weak'){
    data_new$Group[i] <- 'TSM_weak'
  }
}

data_new <- data_new[c("ExperimentName", "Subject", "Sex","Name","Group","Running","Transition","ATokens","XTokens","BTokens","optionnn.RT","optionnn.RTTime","optionnn.RESP","Answer","Onset","Vowel","ConsPair","Accuracy")]
View(data_new)


write.csv(data_new,file="Second phase discrimination data.csv",row.names=FALSE)
data_new <- read.csv(choose.files())
data_new[is.na(data_new)] <- 0

data_previous <- read.csv(choose.files(),stringsAsFactors = FALSE)
for(i in 1:nrow(data_previous)){
  if (data_previous$Transition[i] == 'small'){
    data_previous$Transition[i] <- 'talker 1'
  }
  if (data_previous$Transition[i] == 'large'){
    data_previous$Transition[i] <- 'talker 2'
  }
}

data_all_new <- rbind(data_previous,data_new)
View(data_all_new)
write.csv(data_all_new,file="discrimination data final.csv",row.names=FALSE)
summary(data_all_new)
data_all_new = read.csv(choose.files())
###### data visualizing ######
library(plyr)
library(ggplot2)
library(grid)
my.theme <- theme(axis.text = element_text(colour="black", size=20),
                  text = element_text(size=20),
                  title = element_text(size=20),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line(),
                  legend.position = "top")
data_all_new$ConsPair = factor(data_all_new$ConsPair, levels=c('p-t','k-p','k-t'),labels = c('p-t','p-k','t-k'))
data_all_new$Transition = factor(data_all_new$Transition, levels=c('talker 1','talker 2'))
data_all_new$Vowel = factor(data_all_new$Vowel, levels = c('a','i'))
data_all_new$Group = factor(data_all_new$Group, levels = c('Korean','TSM_fluent','TSM_weak'))
data_all_new <- data_all_new[order(data_all_new$Accuracy,decreasing = TRUE),]
# SE = SD/sqrtN = sqrtV/sqrtN
std.error <- function(x, na.rm = T) {
  sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}
std.error <- function(x, ...) { sqrt(var(x, ...) / length(x)) }

# subject-wise condition means
mean.bySubj1 <- ddply(data_all_new, .(Name,Vowel,ConsPair, Transition,Group), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
View(mean.bySubj1)
grand.means1 <- ddply(mean.bySubj1, .(Vowel, ConsPair, Transition,Group), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1
View(grand.means1)
# ggplot1
plots = ggplot(grand.means1, aes(fill=Group, y=grand.Acurracy, x=ConsPair)) +
  xlab('finals') +
  ylab('accuracy') +
  coord_cartesian(ylim=c(0.5, 1)) +
  stat_summary(fun.y=mean, geom="bar", colour="black",position="dodge", stat="identity") +
  geom_errorbar(width =.25,aes(ymin= grand.Acurracy - se, ymax= grand.Acurracy + se,x=ConsPair),position=position_dodge(.9)) +
  my.theme +
  facet_grid(Transition~Vowel)

plots

##### detail data analysis #####
# Between group
mean.bySubj1 <- ddply(data_all_new, .(Name,Group), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(Group), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1

'''
       Group grand.Acurracy         se
1     Korean      0.8664352 0.01744659
2 TSM_fluent      0.7935185 0.02355391
3   TSM_weak      0.7444444 0.01712015
'''

# Between final pair
mean.bySubj1 <- ddply(data_all_new, .(Name,ConsPair), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(ConsPair), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1

'''
  ConsPair grand.Acurracy         se
1      p-t      0.7940789 0.01255901
2      p-k      0.7986842 0.01535300
3      t-k      0.8026316 0.01338406
'''