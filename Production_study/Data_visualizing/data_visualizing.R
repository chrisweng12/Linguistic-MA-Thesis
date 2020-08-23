##### data visualizing #####
library(plyr)
library(ggplot2)
library(grid)

# read in the 'Coding_result_update.csv' data
production_new = read.csv(choose.files())
new_dat = subset(production_new, production_new$Accuracy !='unident')
new_dat = subset(new_dat,new_dat$Accuracy !='excluded')
# 1651 after excluding unident and excluded data

# adding a coding column
for (i in 1:nrow(new_dat)){
  if (new_dat$Chris[i] == 'p'){
    new_dat$Coding[i] <- 1
  } else if (new_dat$Chris[i] == 't'){
    new_dat$Coding[i] <- 2
  } else if (new_dat$Chris[i] == 'k'){
    new_dat$Coding[i] <- 3
  } else if (new_dat$Chris[i] == 'v'){
    new_dat$Coding[i] <- 4
  } 
}

# adding an answer column
for (i in 1:nrow(new_dat)){
  if (new_dat$Final[i] == 'p'){
    new_dat$Answer[i] <- 1
  } else if (new_dat$Final[i] == 't'){
    new_dat$Answer[i] <- 2
  } else if (new_dat$Final[i] == 'k'){
    new_dat$Answer[i] <- 3
  } else if (new_dat$Final[i] == 'v'){
    new_dat$Answer[i] <- 4
  } 
}

# adding a result column (comparison between coding and answer)
new_dat <- new_dat %>% 
  dplyr::mutate(Result = ifelse(Coding == Answer, 1, 0))
View(new_dat)

# save it as a plotting data
write.csv(new_dat,file="Coding_data_plotting_update.csv",row.names=FALSE)

# calculating values 
std.error <- function(x, na.rm = T) {
  sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}
std.error <- function(x, ...) { sqrt(var(x, ...) / length(x)) }

#new_dat <- read.csv(choose.files())

new_dat$Final = factor(new_dat$Final, levels = c('p','t','k','v'))
new_dat$Vowel = factor(new_dat$Vowel, levels = c('a','i'))
summary(new_dat)

mean.bySubj1 <- ddply(new_dat, .(filename,Vowel,Final), summarize, "mean.Acurracy" = mean(Result))
mean.bySubj1
grand.means1 <- ddply(mean.bySubj1, .(Vowel, Final), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
grand.means1

# barplot
my.theme <- theme(axis.text = element_text(colour="black", size=15),
                  text = element_text(size=15),
                  title = element_text(size=15),
                  axis.title.x=  element_text(vjust=-0.45),
                  axis.title.y = element_text(vjust=.2),
                  axis.ticks = element_line(colour="black"),
                  axis.line = element_line())

plot = ggplot(grand.means1, aes(fill=Vowel, y=grand.Acurracy, x=Final)) +
  xlab('final') +
  ylab('accuracy') +
  coord_cartesian(ylim=c(0.25, 1)) +
  stat_summary(fun.y=mean, geom="bar", colour = 'black', position="dodge", stat="identity") +
  geom_errorbar(width =.25,aes(ymin= grand.Acurracy - se, ymax= grand.Acurracy + se, x=Final),position=position_dodge(.9)) +
  my.theme 

plot

# stackedbar
new_dat$Coding = factor(new_dat$Coding, levels=c('1','2','3','4'), labels = c('p','t','k','v'))
new_dat$Answer = factor(new_dat$Answer, levels=c('1','2','3','4'), labels = c('p','t','k','v'))

library("RColorBrewer")

stacked <-  ggplot(new_dat, aes(x = Answer,  fill = as.factor(Coding))) +
  xlab("Input") +
  ylab('Count') +
  geom_bar(width=0.7, position = "fill") + 
  #geom_text(data=percentData, aes(label=ratio), stat='count', size = 3, position=position_fill(vjust=0.5)) +
  scale_fill_manual(values = c("#1FDEB1", "#469C88", "#6A8981","#18654B")) +
  theme(legend.position="top") +
  guides(fill=guide_legend(title="produced as")) +    
  my.theme +
  facet_grid(~Vowel)
stacked

# heatmap
library(pheatmap)
# making a matrix
new_dat = read.csv(choose.files())
std.error <- function(x, na.rm = T) {
  sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
}
std.error <- function(x, ...) { sqrt(var(x, ...) / length(x)) }
View(new_dat)
for (i in 1:nrow(new_dat)){
  if(new_dat$Final[i] == 'v'){
    new_dat$Condition[i] <- new_dat$Vowel[i]
  }else {
    new_dat$Condition[i] <- paste(new_dat$Vowel[i],new_dat$Final[i],sep="") 
  }
}
View(new_dat)
new_dat$Condition <- as.character(new_dat$Condition)
new_dat$Condition[new_dat$Condition == 1] <- 'a'
new_dat$Condition[new_dat$Condition == 2] <- 'i'
View(new_dat)

mean.bySubj1 <- ddply(new_dat, .(filename,Condition), summarize, "mean.Acurracy" = mean(Result))
mean.bySubj1

trans_dat = as.matrix(sapply(mean.bySubj1, as.numeric))  
View(trans_dat)

test = matrix(mean.bySubj1$mean.Acurracy, 15, 8,byrow=TRUE)
View(test)

colnames(test) = paste("Condition", 1:8, sep = "")
colnames(test) <- c('a','ak','ap','at','i','ik','ip','it')
rownames(test) = paste("Participants", 1:15, sep = "")

# pheatmap
View(test)
pheatmap(test)

##### Creating tableu #####
# Calculating accuracy of each individuals 
library(expss)
library(openxlsx)
mean.bySubj <- ddply(new_dat, .(filename), summarize, "mean.Accurracy" = mean(Result,na.rm=TRUE))
mean.bySubj

wb2 = createWorkbook()
sh2 = addWorksheet(wb2, "Tables")
xl_write(mean.bySubj, wb2, sh2)
saveWorkbook(wb2, "Individual production percentage.xlsx", overwrite = TRUE)

# Production table
production_table = read.csv(choose.files())
View(production_table)
misproduction_table = cro(production_table$Vowel,production_table$Final,production_table$Accuracy)
misproduction_table
wb = createWorkbook()
sh = addWorksheet(wb, "Tables")
xl_write(misproduction_table, wb, sh)
saveWorkbook(wb, "Production_table.xlsx", overwrite = TRUE)

# Misproduction table
misproduction_table = cro(production_table$Vowel,production_table$Misproduction,production_table$Accuracy)
misproduction_table
wb = createWorkbook()
sh = addWorksheet(wb, "Tables")
xl_write(misproduction_table, wb, sh)
saveWorkbook(wb, "Misproduction_table.xlsx", overwrite = TRUE)


