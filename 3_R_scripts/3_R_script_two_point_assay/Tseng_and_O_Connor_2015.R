library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Tseng_and_O_Connor_2015.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	

m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$standard_error))

df0 <- df%>% slice(1:16)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line),)+
  ggtitle("Predators modify the evolutionary response of prey to temperature change		") +
  ylab("per capita growth rate, r	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(a)

ggplot(data = df0, aes(x=Assay_temperature, y=Mean_sex_not_considered , color = Control_or_selection_line)) +
  
  geom_pointrange(aes(ymin = Mean_sex_not_considered-Calc_CI, ymax = Mean_sex_not_considered+Calc_CI), 
                  position=position_jitter(width=0.5), 
                  linetype='dotted')