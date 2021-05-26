library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)



df<-read.table(file="../../1_data/1_data_two_point_assay/Plesnar_Bielak_et_al_2012.txt" ,header= TRUE,sep = " ")
df$New_X_axis=df$Assay_temperature-28               


m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$Standard_error_female))

df0 <- df%>% slice(1:9)

df0$Error_squared= df0$Standard_error_female*df0$Standard_error_female

df0$Mean_squared=df0$Mean_female*df0$Mean_female

Selection_line_fecundity_monogamous <- df0 %>%
  filter(Control_or_selection_line== "Monogamous 28")

Control_line_fecundity_monogamous<- df0 %>%
  filter(Control_or_selection_line== "Control monogamous 20")


df1<- left_join(Selection_line_fecundity_monogamous,Control_line_fecundity_monogamous, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_fecundity_monogamous = Mean_female.s /Mean_female.c)%>%
  mutate(Combined_Standard_error=Relative_mean_fecundity_monogamous*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)

a<- ggplot(df1, aes(x=New_X_axis, y=Relative_mean_fecundity_monogamous ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Mating system affects population performance
          and extinction risk under environmental challenge		") +
  ylab("fecundity monogamous relative fitness")+ geom_hline(yintercept=1)+ylim(0,2)+geom_vline(xintercept = 0)+xlim(-20,20)+
  geom_errorbar(aes(ymin=Relative_mean_fecundity_monogamous-Combined_CI, ymax=Relative_mean_fecundity_monogamous+Combined_CI), width=.4)
#              position=position_dodge(.9) +geom_point()

print(a)

Sel_temp<-df1$Selection_temperature.s
New_X_axis<-df1$New_X_axis
Rel_fitness<-df1$Relative_mean_fecundity_monogamous
CI<-df1$Confidence_interval.s
Paper_id<-replicate(2,"Plesnar_fecundity_monogamous")
Replicate<-df1$Replicate.s
Plesnar_fecundity_monogamous<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Plesnar_fecundity_monogamous,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Plesnar_fecundity_monogamous.txt",sep = " ")
##########################################
df3 <- df%>% slice(1:9)

Selection_line_fecundity_polygamous <- df3 %>%
  filter(Control_or_selection_line== "Polygamous 28")

Control_line_fecundity_polygamous<- df3 %>%
  filter(Control_or_selection_line== "Control polygamous 20")


df4<- left_join(Selection_line_fecundity_polygamous,Control_line_fecundity_polygamous, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_fecundity_polygamous = Mean_female.s /Mean_female.c)

b<- ggplot(df4, aes(x=New_X_axis, y=Relative_mean_fecundity_polygamous ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Mating system affects population performance
          and extinction risk under environmental challenge		") +
  ylab("fecundity polygamous relative fitness")+ geom_hline(yintercept=1)+ylim(0,2)+geom_vline(xintercept = 0)+xlim(-20,20)
#geom_errorbar(aes(ymin=Mean_female-Calc_CI, ymax=Mean_female+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(b)

Sel_temp<-df4$Selection_temperature.s
New_X_axis<-df4$New_X_axis
Rel_fitness<-df4$Relative_mean_fecundity_polygamous
CI<-df4$Confidence_interval.s
Paper_id<-replicate(2,"Plesnar_fecundity_polygamous")
Replicate<-df4$Replicate.s
Plesnar_fecundity_polygamous<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Plesnar_fecundity_polygamous,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Plesnar_fecundity_polygamous.txt",sep = " ")

####################################################33333
df5 <- df%>% slice(12:19)

Selection_line_fertile_monogamous <- df5 %>%
  filter(Control_or_selection_line== "Monogamous 28")

Control_line_fertile_monogamous<- df5 %>%
  filter(Control_or_selection_line== "Control monogamous 20")


df6<- left_join(Selection_line_fertile_monogamous,Control_line_fertile_monogamous, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_fertile_monogamous =Mean_female.s /Mean_female.c)

c<- ggplot(df6, aes(x=New_X_axis, y=Relative_mean_fertile_monogamous ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Mating system affects population performance and 
          extinction risk under environmental challenge		") +
  ylab("female fertility (% of fertile females)	monogamous relative fitness")+ geom_hline(yintercept=1)+ylim(0,2)+geom_vline(xintercept = 0)+xlim(-20,20)
 # geom_errorbar(aes(ymin=Mean_female-Calc_CI, ymax=Mean_female+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(c)

Sel_temp<-df6$Selection_temperature.s
New_X_axis<-df6$New_X_axis
Rel_fitness<-df6$Relative_mean_fertile_monogamous
CI<-df1$Confidence_interval.s
Paper_id<-replicate(2,"Plesnar_fertile_monogamous")
Replicate<-df6$Replicate.s
Plesnar_fertile_monogamous<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Plesnar_fertile_monogamous,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Data_frame_Plesnar_fertile_monogamous.txt",sep = " ")
#######################################################33





df7 <- df%>% slice(12:19)

Selection_line_fertile_polygamous <- df7 %>%
  filter(Control_or_selection_line== "Polygamous 28")

Control_line_fertile_polygamous<- df7 %>%
  filter(Control_or_selection_line== "Control polygamous 20")


df8<- left_join(Selection_line_fertile_polygamous,Control_line_fertile_polygamous, by = c("New_X_axis"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean_fertile_polygamous =Mean_female.s /Mean_female.c)

d<- ggplot(df8, aes(x=New_X_axis, y=Relative_mean_fertile_polygamous ,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Mating system affects population performance and 
          extinction risk under environmental challenge		") +
  ylab("female fertility (% of fertile females)	monogamous relative fitness")+ geom_hline(yintercept=1)+ylim(0,2)+geom_vline(xintercept = 0)+xlim(-20,20)
# geom_errorbar(aes(ymin=Mean_female-Calc_CI, ymax=Mean_female+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(d)

Sel_temp<-df8$Selection_temperature.s
New_X_axis<-df8$New_X_axis
Rel_fitness<-df8$Relative_mean_fertile_polygamous
CI<-df1$Confidence_interval.s
Paper_id<-replicate(2,"Plesnar_fertile_polygamous")
Replicate<-df8$Replicate.s
Plesnar_fertile_polygamous<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Plesnar_fertile_polygamous,file = "../../1_data/1_data_two_point_assay/Combined_graph_dataframes/Dataframe_Plesnar_fertile_polygamous.txt",sep = " ")

####################################################3333