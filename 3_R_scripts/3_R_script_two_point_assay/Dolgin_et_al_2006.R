library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Dolgin_et_al_2006.txt" ,header= TRUE,sep = " ")


m=1.96
Calc_CI<-function(x) m*x
df$Calc_CI<-(Calc_CI(df$Standard_error_male))

df0 <- df%>% slice(1:7)
a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_male ,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Male Drosophila melanogaster have higher 
          mating success when adapted to their thermal environment		") +
  ylab("Percentage of matings with males adapted to the experimental temperature	")+
  geom_errorbar(aes(ymin=Mean_male-Calc_CI, ymax=Mean_male+Calc_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()

print(a)

