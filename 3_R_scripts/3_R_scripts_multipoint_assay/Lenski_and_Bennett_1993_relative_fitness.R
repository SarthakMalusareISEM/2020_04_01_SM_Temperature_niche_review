library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)



df<-read.table(file="../../1_data/1_data_multipoint_assay/Lenski_and_Bennett_1993.txt" ,header= TRUE,sep = " ")


df0 <- df%>% slice(1:28)
df0$Selection_temperature=as.numeric(df0$Selection_temperature)
df0$New_X_axis=df0$Assay_temperature-df0$Selection_temperature 

a<- ggplot(df0, aes(x=New_X_axis, y=Mean_sex_not_considered,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Evolutionary Response of Escherichia-Coli to Thermal-Stress	") +
  ylab("Fitness relative to ancestor	") +ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)

#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

print(a)


Sel_temp<-df0$Selection_temperature
New_X_axis<-df0$New_X_axis
Rel_fitness<-df0$Mean_sex_not_considered
CI<-df0$Confidence_interval
Replicate<-df0$Replicate
Paper_id<-replicate(28,"Lenski_and _Bennett")

Data_frame_Lenski_and_Bennett<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)

write.table(Data_frame_Lenski_and_Bennett,file = "../../1_data/1_data_multipoint_assay/Combined_graph_dataframes/Data_frame_Lenski_and_Bennett.txt",sep = " ")













#df1 <- df%>% slice(53:55)


#b<- ggplot(df1, aes(x=Assay_temperature, y=Mean_sex_not_considered,fill= )) +
 # geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  #ggtitle("Evolutionary Response of Escherichia-Coli to Thermal-Stress	") +
  #ylab("Fitness relative to ancestor	")  +ylim(0,2)+geom_hline(yintercept=1)

#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

#print(b)


