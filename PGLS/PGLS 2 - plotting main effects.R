#plotting main effect coefficients

#Formatting table with model outputs
all_pgls_tables$formula<-as.factor(all_pgls_tables$formula)
all_pgls_tables$coefficients<-as.factor(all_pgls_tables$coefficients)
all_pgls_tables$group[grep("Terr",all_pgls_tables$coefficients)]<-"Terrestrial"#extract aquatic category groupings form coefficents
all_pgls_tables$group[grep("Surface",all_pgls_tables$coefficients)]<-"Surface foraging"
all_pgls_tables$group<-as.factor(all_pgls_tables$group)

#Compute the +/- standard deviation of the estimates on log scale
all_pgls_tables$SEupper_log<-all_pgls_tables$Estimate+all_pgls_tables$`Std. Error`#coefficient +SE on log scale
all_pgls_tables$SElower_log<-all_pgls_tables$Estimate-all_pgls_tables$`Std. Error`

#convert estimates from log-transformed back into raw units
all_pgls_tables$exp<-exp(all_pgls_tables$Estimate)#converted to raw data points
all_pgls_tables$SEupper_exp<-exp(all_pgls_tables$SEupper_log)#converted to raw data points
all_pgls_tables$SElower_exp<-exp(all_pgls_tables$SElower_log)#converted to raw data points
all_pgls_tables$formula<-as.factor(all_pgls_tables$formula)
str(all_pgls_tables)

all_pgls_tables$ISSIG<-ifelse(all_pgls_tables$`Pr(>|t|)`<0.05, "yes","no")#column denoting significance
#

#I examined the table with interaction model outpus in excel to 
#find which measurements have head size*category interactions with interactions,
#and drop them from the main effects plot here
#4,5,12,13,16 = indices of variables with interaction terms to drop
dropinters<-c(which(all_pgls_tables$formula == levels(all_pgls_tables$formula)[4]),
              which(all_pgls_tables$formula == levels(all_pgls_tables$formula)[5]),
              which(all_pgls_tables$formula == levels(all_pgls_tables$formula)[12]),
              which(all_pgls_tables$formula == levels(all_pgls_tables$formula)[13]),
              which(all_pgls_tables$formula == levels(all_pgls_tables$formula)[16]))
maineffects<-all_pgls_tables[-dropinters,]

#regular expressions to extract ear measure from formula name, to make a more readable x axis 
maineffects$measureregex<-NA
for (i in 1:length(maineffects$formula)){
  maineffects$measureregex[i]<-gsub("[\\(\\)]", "", regmatches(maineffects$formula, gregexpr("\\(.*?\\)", maineffects$formula))[[i]])
}

#plot main effects for each measurements all together
allmaineffects<-ggplot(data = subset(maineffects, maineffects$group=="Terrestrial"|
                                       maineffects$group=="Surface foraging"), 
                       aes(x = reorder(measureregex,-exp), y = exp, group = group, label = ISSIG))+
  #geom_point(aes(color = group),stat = "identity", size = 5,position=position_dodge(.3))+
  geom_pointrange(data = subset(maineffects, maineffects$group=="Terrestrial"|
                                  maineffects$group=="Surface foraging"),
                  aes(ymin = SElower_exp, ymax = SEupper_exp, color = group), position=position_dodge(.3))+
  geom_hline(yintercept = 1, color = "blue")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text()+
  scale_color_manual(values=c("darkgrey","green","lightblue","green","darkgray","darkgreen","cornsilk4","blue"))
#coord_flip()
allmaineffects

#write.csv(maineffects,"E:/Analysis_plots/maineffectsmay8.csv")
ggsave("E:/Analysis_plots/interactions may 8main effects.pdf",
       width = 10, height = 10, units = "in")

#subplotting, grouping main effects by functional category
maineffects$util<-NA
maineffects$util[grep("coltip_distancetoTMplane",maineffects$formula)]<-"othersshapemetric'"
maineffects$util[grep("angle_Col_EC)",maineffects$formula)]<-"othersshapemetric'"
maineffects$util[grep("angle_FP_TM",maineffects$formula)]<-"othersshapemetric'"
maineffects$util[grep("totalECDlength",maineffects$formula)]<-"Cochlea length"
maineffects$util[grep("dis_coltip_TMcentroid",maineffects$formula)]<-"Columella offset"
maineffects$util[grep("Umbo_distancetoTMplane",maineffects$formula)]<-"othersshapemetric'"
maineffects$util[grep("RWtotalarea",maineffects$formula)]<-"Input/output areas"
maineffects$util[grep("TMtotalarea",maineffects$formula)]<-"Input/output areas"
maineffects$util[grep("CAtotalarea",maineffects$formula)]<-"Input/output areas"
maineffects$util[grep("area_ratio",maineffects$formula)]<-"Input/output areas"
maineffects$util[grep("FPtotalarea",maineffects$formula)]<-"Input/output areas"
maineffects$util[grep("Umbo_distancetoTMplane",maineffects$formula)]<-"Conical protrusion"
maineffects$util[grep("meanTMangle",maineffects$formula)]<-"Conical protrusion"
maineffects$util[grep("Columella.length.mm",maineffects$formula)]<-"Columella size"
maineffects$util[grep("Columella.volume.mm3",maineffects$formula)]<-"Columella size"
maineffects$util[grep("totalEClength",maineffects$formula)]<-"ME Stiffness"
maineffects$util[grep("Air.volume",maineffects$formula)]<-"ME Stiffness"
maineffects$util<-as.factor(maineffects$util)

levels(maineffects$formula)
levels(maineffects$util)

#plot main effects individually according to functional grouping. Make a list of plots:
effectplts<-list()
for (i in seq_along(levels(maineffects$util))){
  effectplts[[i]]<-subset(maineffects, maineffects$group=="Terrestrial"|
                            maineffects$group=="Surface foraging") %>% filter(util==levels(maineffects$util)[i]) %>% ggplot(., 
                                                                                                                            aes(x = reorder(measureregex,-exp), y = exp, label = ISSIG))+
    geom_point(aes(color = group),stat = "identity", size = 5,position=position_dodge(.3))+
    geom_pointrange(aes(ymin = SElower_exp, ymax = SEupper_exp, color = group), 
                    position=position_dodge(.3))+
    geom_hline(yintercept = 1, color = "blue")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 20, hjust = 1))+
    theme(legend.position = "none")+
    xlab("")+
    geom_text()+
    scale_color_manual(values=c("darkgrey","green","lightblue","green","darkgray","darkgreen","cornsilk4","blue"))
  #coord_flip()
}
#apply(1:6,effectplts)
#ggarrange(effectplts[[1]],#ECD length
#          effectplts[[2]],#columella offset
#          effectplts[[3]],#columella size
#          effectplts[[4]],#mean TM angle (conical protrusion)
#          effectplts[[5]],#FP total area
#          effectplts[[6]],#ME stiffness
#          effectplts[[7]])##other metrics


