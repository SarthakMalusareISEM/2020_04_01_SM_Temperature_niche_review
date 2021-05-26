library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)
library(tidyr)



df<-read.table(file="../../1_data/1_data_multipoint_assay/Schaum_et_al_2017.txt" ,header= TRUE,sep = " ")


m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$Error))

df0 <- df%>% slice(1:16)

#Calculating squares of mean and standard errors

df0$Error_squared= df0$Error*df0$Error

df0$Mean_squared=df0$Mean*df0$Mean



Selection_line <- df0 %>%
  filter(Selection_temperature== "Ambient+4")

Control_line<- df0 %>%
  filter(Selection_temperature== "Ambient")


df1 <- left_join(Selection_line,Control_line, by = "Assay_temperature_in_paper", suffix = c(".s", ".c")) %>%
  mutate(Relative_mean = Mean.s/Mean.c)%>%
  mutate(Combined_Standard_error=Relative_mean*sqrt((Error_squared.s/Mean_squared.s)+(Error_squared.c/Mean_squared.c)))%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)
  #mutate((Confidence_interval_combined=Standard_error_combined*))
 


a<- ggplot(df1, aes(x=Assay_temperature_in_paper, y=Relative_mean,fill= )) +
  geom_line() +geom_point()+
  ggtitle("Adaptation of phytoplankton to a decade of experimental 
          warming linked to increased photosynthesis	") +
  ylab("ln mu max")+ geom_hline(yintercept=1)+ylim(-1.5,2.5)+
  geom_errorbar(aes(ymin=Relative_mean-Combined_CI, ymax=Relative_mean+Combined_CI), width=.2,)
            #  position=position_dodge(.9) +geom_point()
print(a)

