library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Sikkink_et_al_2014.txt" ,header= TRUE,sep = " ")

df0 <- df%>% slice(1:18)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_sex_not_considered ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("The transgenerational effects of heat stress in 
          the nematode Caenorhabditis remanei are negative and rapidly eliminated under directselection for increased stress resistance in larvae") +
  ylab("Probability of survival conditional means	")+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Confidence_interval, ymax=Mean_sex_not_considered+Confidence_interval), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(a)


