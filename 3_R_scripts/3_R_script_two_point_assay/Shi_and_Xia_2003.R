library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Shi_and_Xia_2003.txt" ,header= TRUE,sep = " ")



m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$standard_error))

df0 <- df%>% slice(3:34)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Changes in growth parameters of Pseudomonas pseudoalcaligenes after
          ten months culturing at increasing temperature			") +
  ylab("maximum yield reached	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,position=position_dodge(.9),stat ="identity")
              #position=position_dodge(.9) +geom_point()

print(a)



df1 <- df%>% slice(36:67)
b<- ggplot(df1, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Changes in growth parameters of Pseudomonas pseudoalcaligenes after 
          ten months culturing at increasing temperature		") +
  ylab("maximum growth rate, h^−1		")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(b)




df2 <- df%>% slice(70:101)
c<- ggplot(df2, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Changes in growth parameters of Pseudomonas pseudoalcaligenes after
          ten months culturing at increasing temperature		") +
  ylab("lag time, h	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(c)

