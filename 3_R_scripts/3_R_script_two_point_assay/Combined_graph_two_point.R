library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)


setwd("/home/sarthak/Documents/2020_04_01_SM_Temperature_niche_review/Graphs_and_R_codes/Two _point_papers/1_data_two_point_assay")

df1<-read.table(file="Dataframe_Bennett_lenski.txt" ,header= TRUE,sep = " ")
df2<-read.table(file="Data_frame_Huey_female.txt" ,header= TRUE,sep = " ")
df3<-read.table(file="Data_frame_Huey_male.txt" ,header= TRUE,sep = " ")
df4<-read.table(file="Dataframe_Partridge_fecundity.txt" ,header= TRUE,sep = " ")
df5<-read.table(file="Dataframe_Partridge_lifetime_progeny.txt" ,header= TRUE,sep = " ")
df6<-read.table(file="Dataframe_Partridge_longivity_female.txt" ,header= TRUE,sep = " ")
df7<-read.table(file="Dataframe_Partridge_longivity_male.txt" ,header= TRUE,sep = " ")
df8<-read.table(file="Dataframe_Plesnar_fecundity_monogamous.txt" ,header= TRUE,sep = " ")
df9<-read.table(file="Dataframe_Plesnar_fecundity_polygamous.txt" ,header= TRUE,sep = " ")
df10<-read.table(file="Data_frame_Plesnar_fertile_monogamous.txt" ,header= TRUE,sep = " ")
df11<-read.table(file="Dataframe_Plesnar_fertile_polygamous.txt" ,header= TRUE,sep = " ")


df8<-rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)
#df8 %>% replace_na(list(x = 0, y = "unknown"))
#df8$Replicate[is.na(df8$Replicate), ] <- 0

a<- ggplot(df8, aes(x=New_X_axis, y=Rel_fitness,fill= )) +
  geom_line(aes(group=interaction(Paper_id,Sel_temp), color=interaction(Paper_id,Sel_temp))) +geom_point(aes(color=interaction(Paper_id,Sel_temp)))+
  ggtitle("Relative Fitness Graph	")+
  geom_errorbar(aes(ymin=Rel_fitness-CI, ymax=Rel_fitness+CI,), width=.5)+
  geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)+ylim(0,2)
#              position=position_dodge(.9) +geom_point()

print(a)

