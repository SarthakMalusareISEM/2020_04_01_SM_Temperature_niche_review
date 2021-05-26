library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


setwd("/home/sarthak/Documents/2020_04_01_SM_Temperature_niche_review/Graphs_and_R_codes/Multipoint_point_assay_papers/1_data_multipoint_assay")

df<-read.table(file="Mongold_et_al.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(2:6)
df0$New_X_axis=df0$Assay_temperature-df0$Selection_temperature 


a<- ggplot(df0, aes(x=New_X_axis, y=Mean_sex_not_considered,fill= )) +
  geom_line(aes(group=Selection_temperature, color=Selection_temperature)) +geom_point(aes(color=Selection_temperature))+
  ggtitle("Evolutionary adaptation to temperature .4. Adaptation of Escherichia coli at a niche boundary	") +
  ylab("Fitness relative to ancestor	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Confidence_interval., ymax=Mean_sex_not_considered+Confidence_interval.), width=.2)+
  geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)+ylim(0,2)
#              position=position_dodge(.9) +geom_point()

print(a)



Sel_temp<-df0$Selection_temperature

New_X_axis<-df0$New_X_axis
Rel_fitness<-df0$Mean_sex_not_considered
CI<-df0$Confidence_interval.
Replicate<-df0$Replicate
Paper_id<-replicate(5,"Mongold")

Data_frame_Mongold<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)

write.table(Data_frame_Mongold,file = "Data_frame_Mongold.txt",sep = " ")
