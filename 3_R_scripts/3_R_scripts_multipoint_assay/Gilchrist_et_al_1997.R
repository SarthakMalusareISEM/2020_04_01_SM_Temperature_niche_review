library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Gilchrist_et_al_1997.txt" ,header= TRUE,sep = " ")
m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$standard_error))

	
df0 <- df%>% slice(2:22)


a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_male ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Thermal Sensitivity of <i>Drosophila melanogaster:</i> Evolutionary 
          Responses of Adults and Eggs to Laboratory Natural Selection at Different Temperatures		") +
  ylab("Walking speed (cm/s) Male	")

#geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,
#              position=position_dodge(.9) +geom_point()

print(a)


df1 <- df%>% slice(25:45)


b<- ggplot(df1, aes(x=Assay_temperature, y=Mean_female,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Thermal Sensitivity of <i>Drosophila melanogaster:</i> Evolutionary 
          Responses of Adults and Eggs to Laboratory Natural Selection at Different Temperatures		") +
  ylab("Walking speed (cm/s) Female		")

#geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,
#              position=position_dodge(.9) +geom_point()

print(b)

