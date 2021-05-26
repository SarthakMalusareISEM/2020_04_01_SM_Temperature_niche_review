library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Listmann_et_al_2016.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(1:16)


a<- ggplot(df0, aes(x=Assay_temperature_in_paper, y=Mean ,fill= )) +
  geom_line(aes(group=Selection_temperature, color=Selection_temperature)) +geom_point(aes(color=Selection_temperature))+
  ggtitle("Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	") +
  ylab("ln mu max")

#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

print(a)

a<-aggregate(df[, 28], list(df$Control_or_selection_line~Assay_temperature), mean)
