library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)
df1<-read.table(file="Data_frame_Arribas.txt" ,header= TRUE,sep = " ")
df2<-read.table(file="Data_frame_Cooper.txt" ,header= TRUE,sep = " ")
df3<-read.table(file="Data_frame_Santos.txt" ,header= TRUE,sep = " ")
df4<-read.table(file="Data_frame_Blaby_1.txt" ,header= TRUE,sep = " ")
df5<-read.table(file="Data_frame_Blaby_2.txt" ,header= TRUE,sep = " ")
df6<-read.table(file="Data_frame_Mongold.txt" ,header= TRUE,sep = " ")
df7<-read.table(file="Data_frame_Lenski_and_Bennett.txt" ,header= TRUE,sep = " ")

df8<-rbind(df1,df2,df3,df4,df5,df6,df7)
df8 %>% replace_na(list(x = 0, y = "unknown"))
df8$Replicate[is.na(df8$Replicate), ] <- 0

a<- ggplot(df8, aes(x=New_X_axis, y=Rel_fitness,fill= )) +
  geom_line(aes(group=interaction(Paper_id,Sel_temp), color=interaction(Paper_id,Sel_temp))) +geom_point(aes(color=interaction(Paper_id,Sel_temp)))+
  ggtitle("Relative Fitness Graph	")+
  geom_errorbar(aes(ymin=Rel_fitness-CI, ymax=Rel_fitness+CI,), width=.5)+
  geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)+ylim(0,2)
#              position=position_dodge(.9) +geom_point()

print(a)
