# Production study of  loss of final place contrasts among Mandarin-Min bilinguals



## Data visualizing



### Libraries for the script

```r
library(plyr)
library(ggplot2)
library(grid)
library("RColorBrewer")
library(pheatmap)
library(expss)
library(openxlsx)
```



### Plotting

#### Read in data and minor process

1. First read in the data

2. We need accuracy rate for the visualizing process, the column needed is 'correct' and 'incorrect'

   ```r
   production_new = read.csv(choose.files())
   new_dat = subset(production_new, production_new$Accuracy !='unident')
   new_dat = subset(new_dat,new_dat$Accuracy !='excluded')
   ```

3. Transfer the character into numeric for coding column and answer column

   ```r
   # adding an coding column
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
   
   ```

4. Compare the coding and answer column 

   ```r
   # adding a result column (comparison between coding and answer)
   new_dat <- new_dat %>% 
     dplyr::mutate(Result = ifelse(Coding == Answer, 1, 0))
   View(new_dat)
   ```

5. Save it as the plotting data

   ```r
   write.csv(new_dat,file="Coding_data_plotting_update.csv",row.names=FALSE)
   ```

   

#### Bar plot

1. SD function for plotting error bar in the bar plot

   ```r 
   std.error <- function(x, na.rm = T) {
     sqrt(var(x, na.rm = na.rm)/length(x[complete.cases(x)]))
   }
   std.error <- function(x, ...) { sqrt(var(x, ...) / length(x)) }
   ```

2. Setting factor level

   ```R
   new_dat$Final = factor(new_dat$Final, levels = c('p','t','k','v'))
   new_dat$Vowel = factor(new_dat$Vowel, levels = c('a','i'))
   ```

3. Calculate the overall mean and standard error

   ```R
   mean.bySubj1 <- ddply(new_dat, .(filename,Vowel,Final), summarize, "mean.Acurracy" = mean(Result))
   
   grand.means1 <- ddply(mean.bySubj1, .(Vowel, Final), summarize, "grand.Acurracy"= mean(mean.Acurracy), "se" = std.error(mean.Acurracy))
   
   ```

4. Themes for the bar plot

   ```R
   my.theme <- theme(axis.text = element_text(colour="black", size=15),
                     text = element_text(size=15),
                     title = element_text(size=15),
                     axis.title.x=  element_text(vjust=-0.45),
                     axis.title.y = element_text(vjust=.2),
                     axis.ticks = element_line(colour="black"),
                     axis.line = element_line())
   ```

5. Plot the result

   ```R
   plot = ggplot(grand.means1, aes(fill=Vowel, y=grand.Acurracy, x=Final)) +
     xlab('final') +
     ylab('accuracy') +
     coord_cartesian(ylim=c(0.25, 1)) +
     stat_summary(fun.y=mean, geom="bar", colour = 'black', position="dodge", stat="identity") +
     geom_errorbar(width =.25,aes(ymin= grand.Acurracy - se, ymax= grand.Acurracy + se, x=Final),position=position_dodge(.9)) +
     my.theme 
   
   plot
   ```

   <img src="C:\Users\USER\Google 雲端硬碟\Min final stop\production study\New production study\new production\fluent\ALL_DATA\rerun\Production barplot update.png" style="zoom:75%;" />



#### Stacked bar

1. Relabel the factor

   ```R
   new_dat$Coding = factor(new_dat$Coding, levels=c('1','2','3','4'), labels = c('p','t','k','v'))
   
   new_dat$Answer = factor(new_dat$Answer, levels=c('1','2','3','4'), labels = c('p','t','k','v'))
   
   ```

2. Plot the result

   ```r
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
   ```

   <img src="C:\Users\USER\Google 雲端硬碟\Min final stop\production study\New production study\new production\fluent\ALL_DATA\rerun\production stackedbar update.png" style="zoom:75%;" />

#### Heatmap

1. Transferring the data into matrix

   ```R
   for (i in 1:nrow(new_dat)){
     if(new_dat$Final[i] == 'v'){
       new_dat$Condition[i] <- new_dat$Vowel[i]
     }else {
       new_dat$Condition[i] <- paste(new_dat$Vowel[i],new_dat$Final[i],sep="") 
     }
   }
   
   new_dat$Condition <- as.character(new_dat$Condition)
   new_dat$Condition[new_dat$Condition == 1] <- 'a'
   new_dat$Condition[new_dat$Condition == 2] <- 'i'
   View(new_dat)
   
   mean.bySubj1 <- ddply(new_dat, .(filename,Condition), summarize, "mean.Acurracy" = mean(Result))
   
   trans_dat = as.matrix(sapply(mean.bySubj1, as.numeric))  
   
   test = matrix(mean.bySubj1$mean.Acurracy, 15, 8,byrow=TRUE)
   
   colnames(test) = paste("Condition", 1:8, sep = "")
   colnames(test) <- c('a','ak','ap','at','i','ik','ip','it')
   rownames(test) = paste("Participants", 1:15, sep = "")
   
   ```

2. Plot the heatmap with pheatmap function

   ```R
   # pheatmap
   View(test)
   pheatmap(test)
   ```

   <img src="C:\Users\USER\Google 雲端硬碟\Min final stop\production study\New production study\new production\fluent\ALL_DATA\Production heatmap.png" style="zoom:75%;" />



## Creating Tableau



#### Individual percentage

1. Calculate the individual percentage

   ```r
   mean.bySubj <- ddply(new_dat, .(filename), summarize, "mean.Accurracy" = mean(Result,na.rm=TRUE))
   mean.bySubj
   
   ```

2. Create workbook for saving the results

   ```R
   wb2 = createWorkbook()
   sh2 = addWorksheet(wb2, "Tables")
   xl_write(mean.bySubj, wb2, sh2)
   saveWorkbook(wb2, "Individual production percentage.xlsx", overwrite = TRUE)
   ```

   

#### Production table

1. Read in the data of 'Coding_result_update.csv'

   ```r
   production_table = read.csv(choose.files())
   View(production_table)
   ```

2. Create a table between different vowels, categories, and finals

   ```r
   misproduction_table = cro(production_table$Vowel,production_table$Final,production_table$Accuracy)
   misproduction_table
   ```

3. Save the table

   ```R
   wb = createWorkbook()
   sh = addWorksheet(wb, "Tables")
   xl_write(misproduction_table, wb, sh)
   saveWorkbook(wb, "Production_table.xlsx", overwrite = TRUE)
   ```

   

#### Mispronunciation table

1. Create a table between vowel, accuracy, and misproduction

   ```R
   misproduction_table = cro(production_table$Vowel,production_table$Misproduction,production_table$Accuracy)
   misproduction_table
   ```

2. Save the table

   ```r
   wb = createWorkbook()
   sh = addWorksheet(wb, "Tables")
   xl_write(misproduction_table, wb, sh)
   saveWorkbook(wb, "Misproduction_table.xlsx", overwrite = TRUE)
   ```

   

   