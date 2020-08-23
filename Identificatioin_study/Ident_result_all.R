library(dplyr)
library(tidyverse)
##### read in data and analyzing it #####
#Read all the data in a folder than bind it with the elements we need 

ident_all <-
  list.files(path = "C:/Users/chris/Documents/ident_all/new_data",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
View(ident_all)
data_new=ident_all[,c("Subject","Sex","Name","Group","Running","Transition","optionnn.RT","optionnn.RTTime","optionnn.RESP","Answer","Onset","Vowel","Coda")]
data_new["Result"]=NA
View(data_new)

# Compare 'answer' and 'option response' to see the result
library(data.table)
data_new <- data_new %>% 
  dplyr::mutate(Result = ifelse(optionnn.RESP == Answer, 1, 0))
View(data_new)
data_new[is.na(data_new)] <- 0
write.csv(data_new,file="identification second phase.csv",row.names=FALSE)
data_new = read.csv(choose.files())

#bind new data and old data together
data_combined <-
  list.files(path = "C:/Users/chris/Documents/ident",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 
View(data_combined)
write.csv(data_combined,file="identification final updated.csv",row.names=FALSE)
data_combined$Result <- as.numeric(data_combined$Result)
summary(data_combined)

##### Calculating values #####
library(plyr)
library(ggplot2)
library(grid)
std.error <- function(x, na.rm = T) {
  sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}
std.error <- function(x, ...) { sqrt(var(x, ...) / length(x)) }

###### Group Overall Mean Value ######
data_combined_new <- read.csv(choose.files())
View(data_combined_new)
mean.bySubj1 <- ddply(data_combined_new, .(Name,Group), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(Group), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1

'''
      Group grand.Acurracy          se
1    Korean      0.8988095 0.008506462
2 TW_fluent      0.5990079 0.041325943
3   TW_weak      0.6898214 0.028636293
'''

###### Final Stop Overall Mean Value ######
data_combined_new <- read.csv(choose.files())
mean.bySubj1 <- ddply(data_combined_new, .(Name,Coda), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(Coda), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1

'''
  Coda grand.Acurracy         se
1    k      0.6717767 0.03218792
2    p      0.8567753 0.02720574
3    t      0.6373821 0.02258185
'''

# subject-wise condition means
data_combined_new <- read.csv(choose.files())
View(data_combined_new)
data_combined_new$Transition = factor(data_combined_new$Transition, levels=c('small','large'), labels = c('talker 1', 'talker 2'))
data_combined_new$Coda = factor(data_combined_new$Coda, levels=c('p','t','k'))
data_combined_new$Group = factor(data_combined_new$Group, levels=c('Korean','TW_fluent','TW_weak'), labels = c('Korean','TSM_fluent','TSM_weak'))
names(data_combined_new)[names(data_combined_new) == 'Result'] <- 'Accuracy'
View(data_combined_new)

mean.bySubj1 <- ddply(data_combined_new, .(Name,Vowel,Coda, Transition,Group), summarize, "mean.Acurracy" = mean(Accuracy))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(Vowel, Coda, Transition,Group), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1


##### plotting #####
my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=15),
                  title = element_text(size=15),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line())

# barplot
plot = ggplot(grand.means1, aes(fill=Group, y=grand.Acurracy, x=Coda)) +
  xlab('final stop') +
  ylab('accuracy') +
  coord_cartesian(ylim=c(0.3, 1)) +
  stat_summary(fun.y=mean, geom="bar", colour = 'black', position="dodge", stat="identity") +
  geom_errorbar(width =.25,aes(ymin= grand.Acurracy - se, ymax= grand.Acurracy + se,x=Coda),position=position_dodge(.9)) +
  my.theme +
  facet_grid(Transition~Vowel)

plot

# stacked bar 
data_new = read.csv(choose.files())
View(data_new)
data_new$Transition = factor(data_new$Transition, levels=c('small','large'), labels = c('talker 1', 'talker 2'))
data_new$Group = factor(data_new$Group, levels = c('Korean','TW_fluent','TW_weak'), labels = c('Korean','TSM_fluent','TSM_weak'))
data_new$optionnn.RESP = factor(data_new$optionnn.RESP, levels=c('1','2','3'))
data_new$optionnn.RESP = factor(data_new$optionnn.RESP, labels=c('p','t','k'))
data_new$Answer = factor(data_new$Answer, levels=c('1','2','3'))
data_new$Answer = factor(data_new$Answer, labels=c('p','t','k'))
#data_new$Vowel = factor(data_new$Vowel, labels=c('i','a'))


#percentData <- tone %>% group_by(Phase, InputTone, Task) %>% count(PerceptTone) %>%
#  mutate(ratio=scales::percent(n/sum(n)))
#percentData
library("RColorBrewer")


all <-  ggplot(data_new, aes(x = Answer,  fill = as.factor(optionnn.RESP))) +
  xlab("Correct answer") +
  geom_bar(width=0.7, position = "fill") + 
  #geom_text(data=percentData, aes(label=ratio), stat='count', size = 3, position=position_fill(vjust=0.5)) +
  scale_fill_manual(values = c("#1FDEB1", "#469C88", "#6A8981")) +
  theme(legend.position="top") +
  guides(fill=guide_legend(title="perceived as")) +    
  my.theme +
  facet_grid(Transition~Vowel~Group)
all






