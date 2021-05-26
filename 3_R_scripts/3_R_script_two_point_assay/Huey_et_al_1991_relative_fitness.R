library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


df<-read.table(file="../../1_data/1_data_two_point_assay/Huey_et_al_1991.txt" ,header= TRUE,sep = " ")
df$New_X_axis=df$Assay_temperature-16.5

m=1.96
Calc_CI<-function(x) m*x
df$Calc_CI<-(Calc_CI(df$standard_error))




df0 <- df%>% slice(1:4)


df0$Error_squared= df0$standard_error*df0$standard_error

df0$Mean_squared=df0$Mean_male*df0$Mean_male


Selection_line_male<- df0 %>%
  filter(Control_or_selection_line== "dev temp 16.5")

Control_line_male<- df0 %>%
  filter(Control_or_selection_line== "dev temp 25")


df1<- left_join(Selection_line_male,Control_line_male, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_fitness_male =Mean_male.s /Mean_male.c)%>%
  mutate(Combined_Standard_error=Relative_fitness_male*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)

a<- ggplot(df1, aes(x=New_X_axis, y=Relative_fitness_male ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("THERMAL SENSITIVITY OF DROSOPHILA MELANOGASTER 
          RESPONDS RAPIDLY TO LABORATORY NATURAL SELECTION") +
  ylab("Development time 	male Relative fitness")+ ylim(0,2)+ geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)+
  geom_errorbar(aes(ymin=Relative_fitness_male-Combined_CI, ymax=Relative_fitness_male+Combined_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(a)



Sel_temp<-df1$Selection_temperature.s
New_X_axis<-df1$New_X_axis
Rel_fitness<-df1$Relative_fitness_male
CI<-df1$Confidence_interval.s
Paper_id<-replicate(2,"Huey_male")
Replicate<-df1$Replicate.s
Data_frame_Huey_male<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Data_frame_Huey_male,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Data_frame_Huey_male.txt",sep = " ")

###########################################################################################################################################

df2 <- df%>% slice(6:9)
df2$New_X_axis=df2$Assay_temperature-16.5


df2$Error_squared= df2$standard_error*df2$standard_error

df2$Mean_squared=df2$Mean_female*df2$Mean_female

Selection_line_female<- df2 %>%
  filter(Control_or_selection_line== "dev temp 16.5")

Control_line_female<- df2 %>%
  filter(Control_or_selection_line== "dev temp 25")


df3<- left_join(Selection_line_female,Control_line_female, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_fitness_female =Mean_female.s /Mean_female.c)%>%
  mutate(Combined_Standard_error=Relative_fitness_female*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)



b<- ggplot(df3, aes(x=New_X_axis, y=Relative_fitness_female ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("THERMAL SENSITIVITY OF DROSOPHILA MELANOGASTER 
          RESPONDS RAPIDLY TO LABORATORY NATURAL SELECTION")+
  ylab("Development time 	female relative fitness") + ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)+
  geom_errorbar(aes(ymin=Relative_fitness_female-Combined_CI, ymax=Relative_fitness_female+Combined_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(b)


Sel_temp<-df3$Selection_temperature.s
New_X_axis<-df3$New_X_axis
Rel_fitness<-df3$Relative_fitness_female
CI<-df3$Confidence_interval.s
Paper_id<-replicate(2,"Huey_female")
Replicate<-df3$Replicate.s
Data_frame_Huey_female<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Data_frame_Huey_female,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Data_frame_Huey_female.txt",sep = " ")
######################################################################################################################################3333333

b<- ggplot(df3, aes(x=Assay_temperature, y=Relative_fitness_female ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("THERMAL SENSITIVITY OF DROSOPHILA MELANOGASTER 
          RESPONDS RAPIDLY TO LABORATORY NATURAL SELECTION		") +
  ylab("Development time 	female relative fitness")
# geom_errorbar(aes(ymin=Mean_female-Calc_CI, ymax=Mean_female+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

