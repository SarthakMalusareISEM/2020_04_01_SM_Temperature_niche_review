library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Duncan_et_al_2011.txt" ,header= TRUE,sep = " ")

m=1.96
Calc_CI<-function(x) m*x
df$Calc_CI<-(df$standard_error)

df0 <- df%>% slice(3:6)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Adaptation of Paramecium caudatum to variable conditions of temperature Stress	") +
  ylab(" Mean cumulative paramecium growth	") +
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,)

print(a)


