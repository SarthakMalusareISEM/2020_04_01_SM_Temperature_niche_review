library(ggplot2)
library(reshape)
library(dplyr)
library(ggpubr)
library(tibble)

setwd("/home/sarthak/Documents/2020_04_01_SM_Temperature_niche_review/Graphs_and_R_codes/Multipoint_point_assay_papers/1_data_multipoint_assay")

df<-read.table(file="Arribas_et_al_2014.txt" ,header= TRUE,sep = " ")


# Adaptation of phytoplankton to a decade of experimental warming linked to increased photosynthesis	
df1 <- df%>% slice(2:38)
df1$standard_error = df1$standard_deviation/sqrt(3)


df1$New_X_axis=df1$Assay_temperature-df1$Selection_temperature 
Selection_line <- df1 %>%
  filter(Control_or_selection_line== "Selection line")

Control_line<- df1 %>%
  filter(Control_or_selection_line== "Anc")


df1<- left_join(Selection_line,Control_line, by = c("Selection_temperature","New_X_axis","Replicate","Plotting"), suffix = c(".s", ".c"))%>%
  mutate(Relative_mean =Mean.s /Mean.c) %>%
  mutate(Combined_Standard_error=standard_error.s/Mean.c)%>%
  mutate(Absolute_combined_error=abs(Combined_Standard_error))%>%
  mutate(Combined_CI = Absolute_combined_error*1.96)


a<- ggplot(df1, aes(x=New_X_axis, y=Relative_mean,fill= )) +
  geom_line(aes(group=Plotting, color=Plotting)) +geom_point(aes(group=Plotting, color=Plotting))+
  ggtitle("Adaptation to Fluctuating Temperatures in an RNA
          Virus is Driven by the Most Stringent Selective Pressure") +
  ylab("Growth rate e log2 [(virusend – virus0)/virus0]  Relative fitness")  +
  ylim(0,2)+geom_hline(yintercept=1)+geom_vline(xintercept=0)+xlim(-20, 20)+
  geom_errorbar(aes(ymin=Relative_mean-Combined_CI, ymax=Relative_mean+Combined_CI), width=.2,)
#              position=position_dodge(.9) +geom_point()
print(a)

Sel_temp<-df1$Selection_temperature
New_X_axis<-df1$New_X_axis
Rel_fitness<-df1$Relative_mean
CI<-df1$Confidence_interval.s
Paper_id<-replicate(18,"Arribas")
Replicate<-df1$Replicate
Data_frame_Arribas<-cbind(Paper_id,Sel_temp,New_X_axis,Rel_fitness,CI,Replicate)

namevector <- c("CI")
Data_frame_Arribas[ , namevector] <- NA

write.table(Data_frame_Arribas,file = "Data_frame_Arribas.txt",sep = " ")





#b<- ggplot(df0, aes(x=Assay_temperature, y=Relative_mean,fill= )) +
  #geom_line(aes(group=Control_or_selection_line, color=Control_or_selection_line)) +geom_point(aes(color=Control_or_selection_line))+
  #ggtitle("Adaptation to Fluctuating Temperatures in an RNA Virus is Driven by the Most Stringent Selective Pressure") +
  #ylab("Growth rate e log2 [(virusend – virus0)/virus0]     standard devation") +
  #geom_errorbar(aes(ymin=Mean_sex_not_considered-standard_deviation, ymax=Mean_sex_not_considered+standard_deviation), width=.2)
#              position=position_dodge(.9) +geom_point()

print(b)

