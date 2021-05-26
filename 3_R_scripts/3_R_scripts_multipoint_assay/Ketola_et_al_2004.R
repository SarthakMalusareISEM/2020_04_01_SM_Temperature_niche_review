library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


df<-read.table(file="Ketola_et_al_2004.txt" ,header= TRUE,sep = " ")

m=1.96
Calc_CI<-function(x) m*x

df$Calc_CI<-(Calc_CI(df$standard_error))


df1 <- df%>% slice(2:10)


b<- ggplot(df1, aes(x=Assay_Temperature, y=Mean_sex_not_considered,fill= )) + 
ggtitle("R max count: Evolution of Hsp90 expression in Tetrahymena thermophila (Protozoa, Ciliata)
populations exposed to thermally variable environments")+
  geom_line(aes(group=Control_or_selection_line,color = Control_or_selection_line))+geom_point(aes(color=Control_or_selection_line))+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(b)


df2 <- df%>% slice(12:20)


c<- ggplot(df2, aes(x=Assay_Temperature, y=Mean_sex_not_considered,fill=  )) +
ggtitle("R max biomass: Evolution of Hsp90 expression in Tetrahymena thermophila (Protozoa, Ciliata)
populations exposed to thermally variable environments
")+
  geom_line(aes(group=Control_or_selection_line,color = Control_or_selection_line))+geom_point(aes(color=Control_or_selection_line))+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(c)


df3 <- df%>% slice(22:30)


d<- ggplot(df3, aes(x=Assay_Temperature, y=Mean_sex_not_considered,fill=  )) +
ggtitle("K count: Evolution of Hsp90 expression in Tetrahymena thermophila (Protozoa, Ciliata)
populations exposed to thermally variable environments
")+
  geom_line(aes(group=Control_or_selection_line,color = Control_or_selection_line))+geom_point(aes(color=Control_or_selection_line))+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()

print(d)

df4 <- df%>% slice(32:40)


e<- ggplot(df4, aes(x=Assay_Temperature, y=Mean_sex_not_considered,fill=  )) + ggtitle("K biomass: Evolution of Hsp90 expression in Tetrahymena thermophila (Protozoa, Ciliata) populations exposed to thermally variable environments
")+
  geom_line(aes(group=Control_or_selection_line,color = Control_or_selection_line))+geom_point(aes(color=Control_or_selection_line))+
  geom_errorbar(aes(ymin=Mean_sex_not_considered-Calc_CI, ymax=Mean_sex_not_considered+Calc_CI), width=.2)
#              position=position_dodge(.9) +geom_point()
print (e)
