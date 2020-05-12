library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(stringr)
library(tidyr)

#Load data
df<-read.csv(file.choose(), stringsAsFactors = FALSE) # PGLS data frame

#Column names:
#Umbo_distancetoTMplane	= height of eardrum conical protrusion
#coltip_distancetoTMplane	= distance from columella tip to tympanic membrane base plane
#totalEClength = extrastapedius length
#totalcollength	= columella length
#totalECDlength	= endosseous cochlear duct length
#meanTMangle	= mean tympanic membrane angle
#angle_FP_TM	= angle between footplate and tympanic membrane
#angle_Col_EC	 = angle between columella and extracolumella
#TMtotalarea	 = tympanic membrane area
#FPtotalarea	= footplate total area
#CAtotalarea	 = cochlear aqueduct area
#RWtotalarea	= round window area
#dis_coltip_TMcentroid	 =distance from columella to tympanic membrane centroids (offset of columella)
#Columella.length.mm	= columella length (manual measurement from 3D model)
#Columella.volume.mm3	= columella volume
#Air.volume	= cranial air cavity volume
#Head.mass..g.	= head mass
#area_ratio = tympanic membrane area/footplate area


#Since PGLS uses one point per species,I make the dataframe to have average measuement 
#values for the few species that have more than 1 data point/specimen:
avgdf<-df %>% group_by(Binomial) %>% summarise_at(vars(Skull.width..mm.:area_ratio),mean, na.rm = TRUE)                         
avgdf<-as.data.frame(avgdf)

#make 'distinct df', which has only one species per line, to append categorical variables back onto onto avgdf
distinctdf<-distinct(df, Birdtree, .keep_all = TRUE)
distinctdforder<-arrange(distinctdf,Binomial)#match the order betwen dataframes
avgdf$Category<-distinctdforder$Category
avgdf$Species<-distinctdforder$Species
avgdf$Low.Hz<-distinctdforder$Low.Hz

#subset data into categories if needed
#df_terronly<-avgdf[avgdf$Category == "Terrestrial",]
#df_terr<-avgdf[avgdf$Category != "Pursuit diving",]
#df_pursuitd<-avgdf[avgdf$Category == "Pursuit diving",]

#load phylogeny and set up pgls analysis
birdtree <-read.nexus(file.choose())#load nexus file
birdtreels<- ls.consensus(birdtree) # consensus reduces equally parsimonious phylogenies to one consensus phylogeny
plot(birdtreels)# plot simply

#make 'comparative data object' for caper PGLS analysis
birdCDO<-comparative.data(phy = birdtreels,data = avgdf, 
                          names.col = Binomial, 
                          vcv = TRUE, na.omit = FALSE, 
                          warn.dropped = TRUE)

#check any tree tips dropped when linking together phylogeny and dataframe
birdCDO$dropped

#make list of PGLS model formulas to run
pgls_todo <- c("log(Columella.length.mm)~log(Skull.width..mm.)",
  "log(Columella.length.mm)~log(Head.mass..g.^(1/3))",  
  
  "log(Columella.volume.mm3^(1/3))~log(Skull.width..mm.)",
  "log(Columella.volume.mm3)~log(Head.mass..g.)", 
  
  "log(totalEClength)~log(Skull.width..mm.)",
  "log(totalEClength)~log(Head.mass..g.^(1/3))",
  
  "log(meanTMangle)~log(Skull.width..mm.)",
  "log(meanTMangle)~log(Head.mass..g.)",
  
  "log(Air.volume^(1/3))~log(Skull.width..mm.)",
  "log(Air.volume)~log(Head.mass..g.)",#   
  
  "log(FPtotalarea^(0.5))~log(Skull.width..mm.)",
  "log(FPtotalarea)~log(Head.mass..g.^(2/3))",#    
  
  "log(TMtotalarea^0.5)~log(Skull.width..mm.)",
  "log(TMtotalarea)~log(Head.mass..g.^(2/3))",# 
  
  "log(RWtotalarea^(0.5))~log(Skull.width..mm.)",
  "log(RWtotalarea)~log(Head.mass..g.^(2/3))", 
  
  "log(CAtotalarea^(0.5))~log(Skull.width..mm.)",
  "log(CAtotalarea^(0.5))~log(Head.mass..g.^(2/3))",
  
  "log(area_ratio)~log(Skull.width..mm.)",
  "log(area_ratio)~log(Head.mass..g.^(2/3))",
  
  "log(Umbo_distancetoTMplane)~log(Skull.width..mm.)",
  "log(Umbo_distancetoTMplane)~log(Head.mass..g.^(1/3))",
  
  "log(coltip_distancetoTMplane)~log(Skull.width..mm.)",
  "log(coltip_distancetoTMplane)~log(Head.mass..g.^(1/3))",
  
  "log(angle_FP_TM)~log(Skull.width..mm.)",
  "log(angle_FP_TM)~log(Head.mass..g.)",
  
  "log(angle_Col_EC)~log(Skull.width..mm.)",
  "log(angle_Col_EC)~log(Head.mass..g.)",
  
  "log(totalECDlength)~log(Skull.width..mm.)",
  "log(totalECDlength)~log(Head.mass..g.^(1/3))",
  
  "log(dis_coltip_TMcentroid)~log(Skull.width..mm.)",
  "log(dis_coltip_TMcentroid)~log(Head.mass..g.^(1/3))")

#Modify formula list, as desired, to run different models
pgls_todo_sw<-pgls_todo[seq(1,length(pgls_todo),2)]#formulas regressed against skull width only
pgls_todo_sw_int<- paste(pgls_todo_sw,":Category")#interaction term
pgls_todo_category_int <- paste(pgls_todo,"*Category")#interactions + main effects
pgls_todo_category_int_sw<-pgls_todo_category_int[seq(1,length(pgls_todo_category_int),2)]#interaction+ main effects, skull width only
pgls_todo_category_main <- paste(pgls_todo,"+Category")#main effects only
pgls_todo_category_main_sw<-pgls_todo_category_main[seq(1,length(pgls_todo_category_main),2)]#main effects, sw only

#function to run the PGLS model
pgls_models<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, #check comparative data object here<---
                lambda = 'ML', #find lambda using maximum likelihood
                bounds = list(lambda=c(0.001,1)))#####
}

#Apply the PGLS function across the list of formulas. 
#Replace the first operater here, depending on the specific formula list to be tested,
#and be sure to remember which formula go into the 'pgls_models_list' object
pgls_models_list<-lapply(pgls_todo_sw_int,pgls_models)#main effects+interaction
pgls_models_list<-lapply(pgls_todo_category_main_sw,pgls_models)#main effects

#make a table of model outputs. 
#Check that assignment of "tbllist[[i]]$formula" in the function
#matches the formula list input for 'pgls_model_list' 
tbllist<-list()
for (i in seq_along(pgls_models_list)){
  tbllist[[i]]<-as.data.frame(summary(pgls_models_list[[i]])$'coefficients')
  tbllist[[i]]$Rsquared<-summary(pgls_models_list[[i]])$'r.squared'#rsquared
  tbllist[[i]]$formula<-pgls_todo_category_main_sw[i]#formula<_____________________CHECK FORMULA LIST HERE is correct
  tbllist[[i]]$lambda<-summary(pgls_models_list[[i]])$'param'[[2]]#lambda
}

#combine list of model outputs into one dataframe
all_pgls_tables<- do.call(rbind.data.frame, tbllist)#bind rows
#reorder columns to make a nicer table:
all_pgls_tables$coefficients<-row.names(all_pgls_tables)
col_order <- c("formula", "coefficients", "Estimate", "Std. Error","t value", "Pr(>|t|)", "Rsquared", "lambda")
all_pgls_tables <- all_pgls_tables[, col_order]

#write table of model outputs if desired
#write.csv(all_pgls_tables, "D:/Analysis_plots/swint.csv", row.names = FALSE)

