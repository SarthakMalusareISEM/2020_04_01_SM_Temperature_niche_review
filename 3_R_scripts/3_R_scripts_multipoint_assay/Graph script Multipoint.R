library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Trial_relative_fitness_graphs_multipoint.txt" ,header= TRUE,sep = " ")

# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(1:16)


a<- ggplot(df0, aes(x=Assay_temperature_in_paper, y=Mean ,fill= )) +
  geom_line(aes(group=Selection_temperature, color=Selection_temperature)) +geom_point(aes(color=Selection_temperature))+ggtitle("Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	")
 #geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
  #              position=position_dodge(.9) +geom_point()

print(a)



#Evolution of Hsp90 expression in Tetrahymena thermophila (Protozoa, Ciliata) populations exposed to thermally variable environments	
df1 <- df%>% slice(21:29)


b<- ggplot(df1, aes(x=Assay_temperature_in_paper, y=Mean,fill= )) + ggtitle("R max count")+
  geom_line(aes(group=Selection_temperature,color = Selection_temperature))+geom_point(aes(color=Selection_temperature))
#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

print(b)


df2 <- df%>% slice(31:39)


c<- ggplot(df2, aes(x=Assay_temperature_in_paper, y=Mean,fill= )) +ggtitle("R max biomass")+
  geom_line(aes(group=Selection_temperature,color = Selection_temperature))+geom_point(aes(color=Selection_temperature))
#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

print(c)


df3 <- df%>% slice(41:49)


d<- ggplot(df3, aes(x=Assay_temperature_in_paper, y=Mean,fill= )) +ggtitle("K count")+
  geom_line(aes(group=Selection_temperature,color = Selection_temperature))+geom_point(aes(color=Selection_temperature))
#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()

print(d)

df4 <- df%>% slice(51:59)


e<- ggplot(df4, aes(x=Assay_temperature_in_paper, y=Mean,fill= )) + ggtitle("K biomass")+
  geom_line(aes(group=Selection_temperature,color = Selection_temperature))+geom_point(aes(color=Selection_temperature))
#geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_error, ymax=Mean_sex_not_considered+standard_error), width=.2,
#              position=position_dodge(.9) +geom_point()
print (e)
