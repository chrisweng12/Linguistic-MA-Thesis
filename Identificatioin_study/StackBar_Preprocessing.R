library(expss)
library(openxlsx)

data_new = read.csv(choose.files())
View(data_new)
summary(data_new)

Korean_data = subset(data_new,data_new$Group == "Korean")
summary(Korean_data)
TSM_fluent_data = subset(data_new,data_new$Group == "TW_fluent")
summary(TSM_fluent_data)
TSM_weak_data = subset(data_new,data_new$Group == "TW_weak")
summary(TSM_weak_data)

######### Korean ##########
Korean_data$optionnn.RESP[is.na(Korean_data$optionnn.RESP)] <- "None"
Korean_data$Transition = factor(Korean_data$Transition, levels=c('small','large'))
Korean_data$Vowel = factor(Korean_data$Vowel, levels = c('i','a'))
Korean_data$optionnn.RESP = factor(Korean_data$optionnn.RESP, levels=c('1','2','3','0'),labels = c('p','t','k',"None"))
Korean_data$Answer = factor(Korean_data$Answer, levels=c('1','2','3'),labels = c('p','t','k'))
summary(Korean_data)

Korean_table = table(Korean_data$Transition,Korean_data$Vowel, Korean_data$optionnn.RESP,Korean_data$Answer)
Kor_table = as.data.frame(Korean_table)
View(Korean_table)
Kor_table["stimuli"] = paste(Kor_table$Var1,' ',Kor_table$Var2,' ',Kor_table$Var4)
View(Kor_table)
Korean_all = Kor_table[,c("stimuli","Var3","Freq")]
View(Korean_all)

######### TSM_fluent ##########
TSM_fluent_data$optionnn.RESP[is.na(TSM_fluent_data$optionnn.RESP)] <- "None"
TSM_fluent_data$Transition = factor(TSM_fluent_data$Transition, levels=c('small','large'))
TSM_fluent_data$Vowel = factor(TSM_fluent_data$Vowel, levels = c('i','a'))
TSM_fluent_data$optionnn.RESP = factor(TSM_fluent_data$optionnn.RESP, levels=c('1','2','3','0'),labels = c('p','t','k',"None"))
TSM_fluent_data$Answer = factor(TSM_fluent_data$Answer, levels=c('1','2','3'),labels = c('p','t','k'))
summary(TSM_fluent_data)

TSM_fluent_data_table = table(TSM_fluent_data$Transition,TSM_fluent_data$Vowel, TSM_fluent_data$optionnn.RESP,TSM_fluent_data$Answer)
TSM_fluent_table = as.data.frame(TSM_fluent_data_table)
View(TSM_fluent_table)
TSM_fluent_table["stimuli"] = paste(TSM_fluent_table$Var1,' ',TSM_fluent_table$Var2,' ',TSM_fluent_table$Var4)
View(TSM_fluent_table)
TSM_fluent_all = TSM_fluent_table[,c("stimuli","Var3","Freq")]
View(TSM_fluent_all)

######### TSM_weak ##########
TSM_weak_data$optionnn.RESP[is.na(TSM_weak_data$optionnn.RESP)] <- "None"
TSM_weak_data$Transition = factor(TSM_weak_data$Transition, levels=c('small','large'))
TSM_weak_data$Vowel = factor(TSM_weak_data$Vowel, levels = c('i','a'))
TSM_weak_data$optionnn.RESP = factor(TSM_weak_data$optionnn.RESP, levels=c('1','2','3','0'),labels = c('p','t','k',"None"))
TSM_weak_data$Answer = factor(TSM_weak_data$Answer, levels=c('1','2','3'),labels = c('p','t','k'))
summary(TSM_weak_data)

TSM_weak_data_table = table(TSM_weak_data$Transition,TSM_weak_data$Vowel, TSM_weak_data$optionnn.RESP,TSM_weak_data$Answer)
TSM_weak_table = as.data.frame(TSM_weak_data_table)
View(TSM_weak_table)
TSM_weak_table["stimuli"] = paste(TSM_weak_table$Var1,' ',TSM_weak_table$Var2,' ',TSM_weak_table$Var4)
View(TSM_weak_table)
TSM_weak_all = TSM_weak_table[,c("stimuli","Var3","Freq")]
View(TSM_weak_all)
