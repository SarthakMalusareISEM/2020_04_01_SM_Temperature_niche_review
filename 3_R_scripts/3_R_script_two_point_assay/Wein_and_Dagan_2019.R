library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Wein_and_Dagan_2019.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	

m=1.96
Calc_CI<-function(x) m*x
df$Calc_CI<-Calc_CI(df$standard_error)

df0 <- df%>% slice(2:7)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("The Effect of Population Bottleneck Size and Selective Regime on Genetic Diversity and Evolvability in Bacteria		") +
  ylab("relative fitness through competition assays ")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,)+ylim(0,2)+geom_hline(yintercept=1)
#              position=position_dodge(.9) +geom_point()

print(a)

