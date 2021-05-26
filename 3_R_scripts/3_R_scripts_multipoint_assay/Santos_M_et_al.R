library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)

df<-read.table(file="Santos_M_et_al.txt" ,header= TRUE,sep = " ")


# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df0 <- df%>% slice(3:11)


a<- ggplot(df0, aes(x=Assay_temperature, y=Mean_female,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("EThermal evolution of pre-adult life history traits, geometric size and shape, 
          and developmental stability in Drosophila subobscura		") +
  ylab("Egg-lay to adult development time (hours)	female	") +
  geom_errorbar(aes(ymin=Mean_female-Confidence_interval., ymax=Mean_female+Confidence_interval.), width=.2)

#              position=position_dodge(.9) +geom_point()

print(a)



df1 <- df%>% slice(13:21)


b<- ggplot(df1, aes(x=Assay_temperature, y=Mean_male,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Thermal evolution of pre-adult life history traits, geometric size and shape,
          and developmental stability in Drosophila subobscura		") +
  ylab("Egg-lay to adult development time (hours) male		") +
  geom_errorbar(aes(ymin=Mean_male-Confidence_interval., ymax=Mean_male+Confidence_interval.), width=.2)+ylim(0,2)+geom_hline(yintercept=1)

#              position=position_dodge(.9) +geom_point()

print(b)



df3 <- df%>% slice(23:31)


c<- ggplot(df3, aes(x=Assay_temperature, y=Mean_female,fill= )) +
  geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  ggtitle("Thermal evolution of pre-adult life history traits, geometric size and shape,
          and developmental stability in Drosophila subobscura		") +
  ylab("Viability (# adults/30)		") +
  geom_errorbar(aes(ymin=Mean_female-Confidence_interval., ymax=Mean_female+Confidence_interval.), width=.2)+ylim(0,2)+geom_hline(yintercept=1)

#              position=position_dodge(.9) +geom_point()


print(c)