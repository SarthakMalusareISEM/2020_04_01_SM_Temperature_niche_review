library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


setwd("/home/sarthak/Documents/2020_04_01_SM_Temperature_niche_review/Graphs_and_R_codes/Multipoint_point_assay_papers/1_data_multipoint_assay")

df<-read.table(file="Blaby_et_al_2012.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(3:16)
df0$New_X_axis=df0$Assay_temperature-49.7 
Selection_line <- df0 %>%
  filter(Control_or_selection_line== "EVG1064 49.7 degree")

Control_line<- df0 %>%
  filter(Control_or_selection_line== "MG1655")


df0 <- left_join(Selection_line,Control_line, by = "New_X_axis", suffix = c("_s", "_c")) %>%
  mutate(Relative_mean = Mean_sex_not_considered_s/Mean_sex_not_considered_c)

a<- ggplot(df0, aes(x=New_X_axis, y=Relative_mean,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Experimental Evolution of a Facultative Thermophile from a Mesophilic Ancestor") +
  ylab("Mean generation time (min^-1)   Relative ")+ geom_hline(yintercept=1)+ylim(0,2)+xlim(-20, 20)+geom_vline(xintercept=0)
  #geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_deviation, ymax=Mean_sex_not_considered+standard_deviation), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(a)


Sel_temp<-df0$Selection_temperature_s
New_X_axis<-df0$New_X_axis
Rel_fitness<-df0$Relative_mean
CI<-df0$Confidence_interval._s
Paper_id<-replicate(7,"Blaby_1")
Replicate<-df0$Replicate_s
Data_frame_Blaby_1<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Data_frame_Blaby_1,file = "Data_frame_Blaby_1.txt",sep = " ")
################################################################################################################


df1 <- df%>% slice(22:35)
df1$New_X_axis=df1$Assay_temperature-49.7 

Selection_line <- df1 %>%
  filter(Control_or_selection_line== "EVG1064 49.7 degree")

Control_line<- df1 %>%
  filter(Control_or_selection_line== "MG1655")


df1 <- left_join(Selection_line,Control_line, by = "New_X_axis", suffix = c("_s", "_c")) %>%
  mutate(Relative_mean = Mean_sex_not_considered_s/Mean_sex_not_considered_c)


b<- ggplot(df1, aes(x=New_X_axis, y=Relative_mean,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Experimental Evolution of a Facultative Thermophile from a Mesophilic Ancestor") +
  ylab("Optical density OD 600    standard deviation shown")+ geom_hline(yintercept=1)+ylim(0,25)+xlim(-20, 20)+geom_vline(xintercept=0)
  #geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_deviation, ymax=Mean_sex_not_considered+standard_deviation), width=.2,)
#              position=position_dodge(.9) +geom_point()
print(b)



Sel_temp<-df0$Selection_temperature_s
New_X_axis<-df0$New_X_axis
Rel_fitness<-df0$Relative_mean
CI<-df0$Confidence_interval._s
Paper_id<-replicate(7,"Blaby_2")
Replicate<-df0$Replicate_s
Data_frame_Blaby_2<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Data_frame_Blaby_2,file = "Data_frame_Blaby_2.txt",sep = " ")

########################################################################################################################
#df2 <- df%>% slice(39:54)


#c<- ggplot(df2, aes(x=Assay_temperature, y=Mean_sex_not_considered,fill= )) +
 # geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  #ggtitle("Experimental Evolution of a Facultative Thermophile from a Mesophilic Ancestor") +
  #ylab("Mean generation time (min^-1)  standard deviation shown" )+
  #geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_deviation, ymax=Mean_sex_not_considered+standard_deviation), width=.2,)
#              position=position_dodge(.9) +geom_point()         

#print(c)

