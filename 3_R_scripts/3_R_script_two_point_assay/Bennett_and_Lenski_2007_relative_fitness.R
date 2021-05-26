library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


setwd("/home/sarthak/Documents/2020_04_01_SM_Temperature_niche_review/Graphs_and_R_codes/Two _point_papers/1_data_two_point_assay")

df<-read.table(file="Bennett_and_Lenski_2007.txt" ,header= TRUE,sep = " ")
df$New_X_axis=df$Assay_temperature-df$Selection_temperature 


#df0 <- df%>% slice(:)
a<- ggplot(df, aes(x=New_X_axis, y=Mean_sex_not_considered,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("An experimental test of evolutionary trade-offs during temperature adaptation") +
  ylab(" relative fitness through competition assays ") +
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Confidence_interval, ymax=Mean_sex_not_considered+Confidence_interval), width=.2,)+
  ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)

print(a)



Sel_temp<-df$Selection_temperature
New_X_axis<-df$New_X_axis
Rel_fitness<-df$Mean_sex_not_considered
CI<-df$Confidence_interval
Paper_id<-replicate(17,"Bennett_lenski")
Replicate<-df$Replicate
Bennett_lenski<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Bennett_lenski,file = "Dataframe_Bennett_lenski.txt",sep = " ")