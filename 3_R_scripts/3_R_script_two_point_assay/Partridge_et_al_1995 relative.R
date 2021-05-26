library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)



df<-read.table(file="../../1_data/1_data_two_point_assay/Partridge_et_al_1995.txt" ,header= TRUE,sep = " ")

df$New_X_axis=df$Assay_temperature-16


df0 <- df%>% slice(1:6)

df0$Error_squared= df0$standard_error*df0$Standard_error

df0$Mean_squared=df0$Mean_male*df0$Mean_male


Selection_line_longivity_male <- df0 %>%
  filter(Control_or_selection_line== "16 line")

Control_line_longivity_male<- df0 %>%
  filter(Control_or_selection_line== "25 line")


df1<- left_join(Selection_line_longivity_male,Control_line_longivity_male, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_longivity_male =Mean_male.s /Mean_male.c)

a1<- ggplot(df1, aes(x=New_X_axis, y=Relative_mean_longivity_male ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Rapid Laboratory Evolution of Adult Life-History Traits 
          in Drosophila melanogaster inResponse to Temperature		") +
  ylab("Median Longevities (days) of males relative fitness 	")+ ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)
  
print(a1)

Sel_temp<-df1$Selection_temperature.s
New_X_axis<-df1$New_X_axis
Rel_fitness<-df1$Relative_mean_longivity_male
CI<-df1$Confidence_interval.s
Paper_id<-replicate(2,"Partridge_longivity_male")
Replicate<-df1$Replicate.s
Partridge_longivity_male<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Partridge_longivity_male,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Partridge_longivity_male.txt",sep = " ")
#############################################################################################3333
df2 <- df%>% slice(1:6)

Selection_line_longivity_female <- df2 %>%
  filter(Control_or_selection_line== "16 line")

Control_line_longivity_female<- df2 %>%
  filter(Control_or_selection_line== "25 line")


df3<- left_join(Selection_line_longivity_female ,Control_line_longivity_female, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_longivity_female =Mean_female.s /Mean_female.c)

a2<- ggplot(df3, aes(x=New_X_axis, y=Relative_mean_longivity_female ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Rapid Laboratory Evolution of Adult Life-History Traits 
          in Drosophila melanogaster inResponse to Temperature			") +
  ylab("Median Longevities (days) of females 		") + ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)
print(a2)

Sel_temp<-df3$Selection_temperature.s
New_X_axis<-df3$New_X_axis
Rel_fitness<-df3$Relative_mean_longivity_female
CI<-df3$Confidence_interval.s
Paper_id<-replicate(2,"Partridge_longivity_female")
Replicate<-df3$Replicate.s
Partridge_longivity_female<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Partridge_longivity_female,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Partridge_longivity_female.txt",sep = " ")
#########################################################################################################33
df4 <- df%>% slice(8:11)
m=1.96
Calc_error<-function(x) x/m
df4$standard_error<-(Calc_error(df4$Confidence_interval))


df4$Error_squared= df4$standard_error*df4$standard_error

df4$Mean_squared=df4$Mean_female*df4$Mean_female

Selection_line_fecundity <- df4 %>%
  filter(Control_or_selection_line== "16 line")

Control_line_fecundity<- df4%>%
  filter(Control_or_selection_line== "25 line")


df5<- left_join(Selection_line_fecundity,Control_line_fecundity, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_fecudity =Mean_female.s /Mean_female.c)%>%
  mutate(Combined_Standard_error=Relative_mean_fecudity*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)

b<- ggplot(df5, aes(x=New_X_axis, y=Relative_mean_fecudity ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Rapid Laboratory Evolution of Adult Life-History Traits in Drosophila melanogaster inResponse to Temperature		") +
  ylab("Fecundity relative fitness")+ ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)+
  geom_errorbar(aes(ymin=Relative_mean_fecudity-Combined_CI, ymax=Relative_mean_fecudity+Combined_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(b)

Sel_temp<-df5$Selection_temperature.s
New_X_axis<-df5$New_X_axis
Rel_fitness<-df5$Relative_mean_fecudity
CI<-df5$Confidence_interval.s
Paper_id<-replicate(2,"Partridge_fecundity")
Replicate<-df5$Replicate.s
Partridge_fecundity<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Partridge_fecundity,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Partridge_fecundity.txt",sep = " ")
#############################################################################################3333

df7 <- df%>% slice(14:17)

m=1.96
Calc_error<-function(x) x/m
df7$standard_error<-(Calc_error(df7$Confidence_interval))

df7$Error_squared= df7$standard_error*df4$standard_error

df7$Mean_squared=df7$Mean_female*df7$Mean_female


Selection_line_lifetime_progeny <- df7 %>%
  filter(Control_or_selection_line== "16 line")

Control_line_lifetime_progeny<- df7 %>%
  filter(Control_or_selection_line== "25 line")


df8<- left_join(Selection_line_lifetime_progeny,Control_line_lifetime_progeny, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_lifetime_progeny =Mean_female.s /Mean_female.c)%>%
  mutate(Combined_Standard_error=Relative_mean_lifetime_progeny*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)

c<- ggplot(df8, aes(x=New_X_axis, y=Relative_mean_lifetime_progeny ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Rapid Laboratory Evolution of Adult Life-History Traits 
          in Drosophila melanogaster inResponse to Temperature		") +
  ylab("Lifetime progeny  production by female relative fitness	")+ ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept = 0)+xlim(-20,20)+
  geom_errorbar(aes(ymin=Relative_mean_lifetime_progeny-Combined_CI, ymax=Relative_mean_lifetime_progeny+Combined_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(c)

Sel_temp<-df8$Selection_temperature.s
New_X_axis<-df8$New_X_axis
Rel_fitness<-df8$Relative_mean_lifetime_progeny
CI<-df8$Confidence_interval.s
Paper_id<-replicate(2,"Partridge_lifetime_progeny")
Replicate<-df8$Replicate.s
Partridge_lifetime_progeny<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Partridge_lifetime_progeny,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Partridge_lifetime_progeny.txt",sep = " ")

