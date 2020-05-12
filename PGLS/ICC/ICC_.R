library(ICC)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)

#import data set
df<-read.csv(file.choose())
#ID column = specimen
#Code column = replications
#all other columns = measurements (raw values)

#make list of measurements for which the ICC stat is desired
todo<-names(df)[c(4:11,13:19)]

#apply it across the list of measurements for which ICC is desired
#combine into single dataframe
byspecies<-split(df,df$Species)
ICCrd<- do.call(rbind.data.frame, lapply(todo,function(i){
  ICC<-ICCest(ID,i,data = byspecies$`Rock dove`)#chek dataset
  ICC$measure<-i
  ICC$species<-"Rock dove"
  return(ICC)
}))

ICCSAL<- do.call(rbind.data.frame, lapply(todo,function(i){
  ICC<-ICCest(ID,i,data = byspecies$`Salvin's prion`)#chek dataset
  ICC$measure<-i
  ICC$species<-"Salvin's prion"
  return(ICC)
}))

ICCboth<-rbind(ICCrd,ICCSAL)

#write.csv(ICCboth,"E:/Analysis_plots/ICCmay8.csv")

################plot ICCs
all<-ggplot(data = ICCboth, aes(x = reorder(measure,-ICC), y = ICC, color = species)) +
  #geom_bar(stat = "identity", fill = Species) +
  geom_pointrange(aes(ymin = LowerCI, ymax = UpperCI), position=position_dodge(.3))+
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1)) +
  #coord_flip() +
  xlab("")+
  theme_bw() +
  #facet_wrap(~use)+
  #geom_text(aes(y = -0.1), angle = 45, hjust = 0, size = 2, color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Intraclass correlation coefficients (+/- 95% CI)")
all
