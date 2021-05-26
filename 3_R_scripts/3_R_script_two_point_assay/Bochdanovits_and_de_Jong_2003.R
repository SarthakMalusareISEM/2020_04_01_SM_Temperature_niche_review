library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Bochdanovits_and_de_Jong_2003.txt" ,header= TRUE,sep = " ")




df0 <- df%>% slice(13:20)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Experimental evolution in Drosophila melanogaster: Interaction of temperature and food quality selection regimes		") +
  ylab("Larval survival (%recovered)	 	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered -Confidence_interval, ymax=Mean_sex_not_considered +Confidence_interval), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(a)

df1 <- df%>% slice(22:29)
b<- ggplot(df1, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Experimental evolution in Drosophila melanogaster: Interaction of temperature and food quality selection regimes	") +
  ylab("Feeding rate in bites per minute			")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered -Confidence_interval, ymax=Mean_sex_not_considered +Confidence_interval), width=.2,)
#              position=position_dodge(.9) +geom_point()
print(b)

df2 <- df%>% slice(31:38)
c<- ggplot(df2, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Experimental evolution in Drosophila melanogaster: Interaction of temperature and food quality selection regimes	") +
  ylab("Development time to pupa in hours 	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered -Confidence_interval, ymax=Mean_sex_not_considered +Confidence_interval), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(c)


df3 <- df%>% slice(40:47)
d<- ggplot(df3, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Experimental evolution in Drosophila melanogaster: Interaction of temperature and food quality selection regimes	") +
  ylab("Development time to adult in hours	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered -Confidence_interval, ymax=Mean_sex_not_considered +Confidence_interval), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(d)