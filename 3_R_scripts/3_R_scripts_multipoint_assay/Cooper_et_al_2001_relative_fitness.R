library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


df<-read.table(file="../../1_data/1_data_multipoint_assay/Cooper_et_al_2001.txt" ,header= TRUE,sep = " ")
m<-1.96
t<-1
calc_SE<- function(x) x/m
df$standard_error<-(calc_SE(df$Confidence_interval))
#Calculating squares of mean and standard errors

df$Error_squared= df$standard_error*df$standard_error

df$Mean_squared=df$Mean_sex_not_considered*df$Mean_sex_not_considered

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(2:19)
df0$New_X_axis=df0$Assay_temperature-37


Selection_line <- df0 %>%
  filter(Control_or_selection_line== "degrees for 2000 generations")

Control_line <- df0 %>%
  filter(Control_or_selection_line== "Ancestor")


df0 <- left_join(Selection_line,Control_line, by = "New_X_axis", suffix = c(".s", ".c")) %>%
  mutate(Relative_mean = Mean_sex_not_considered.s/Mean_sex_not_considered.c)%>%
  mutate(Combined_Standard_error=Relative_mean*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)
# mutate(Total_error= Error.s + Error.c)


a<- ggplot(df0, aes(x=New_X_axis, y=Relative_mean,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Evolution of thermal dependence of growth rate of Escherichia coli populations 
          during 20,000 generations in a constant environment") +
  ylab("Maximum growth rate Vmax  relative fitness") +
  ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)+
  geom_errorbar(aes(ymin=Relative_mean-Combined_CI, ymax=Relative_mean+Combined_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(a)



Sel_temp<-df0$Selection_temperature_s
New_X_axis<-df0$New_X_axis
Rel_fitness<-df0$Relative_mean
CI<-df0$Confidence_interval_s
Replicate<-df0$Replicate_s
Paper_id<-replicate(9,"Cooper")

Data_frame_Cooper<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)
write.table(Data_frame_Cooper,file = "../../1_data/1_data_multipoint_assay/Combined_graph_dataframes/Data_frame_Cooper.txt",sep = " ")

