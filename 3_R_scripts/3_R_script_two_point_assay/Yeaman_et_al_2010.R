library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Yeaman_et_al_2010.txt" ,header= TRUE,sep = " ")



df0 <- df%>% slice(2:5)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_female,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("No Effect of Environmental Heterogeneity on the Maintenance 
          of Genetic Variation in Wing Shape in Drosophila Melanogaster		") +
  ylab(" progeny productivity		") 
 #geom_errorbar(aes(ymin=Mean_female-Calc_CI, ymax=Mean_female+Calc_CI), width=.2,)

print(a)


