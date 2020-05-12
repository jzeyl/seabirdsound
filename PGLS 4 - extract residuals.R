library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(stringr)

#get formulas list that regresses each ear measure with skull width only
pgls_todo_sw<-pgls_todo[seq(1,length(pgls_todo),2)]# from pgls model R script

#function to extract head size-correcteded PGLS residuals for each PGLS formula
pgls_resid_outputs<-function(i){
  pglsfit<-pgls(as.formula(i), data = birdCDO, lambda = 'ML', 
                bounds = list(lambda = c(0.1,1)))
  resids<-residuals(pglsfit)
}

#apply the residuals function to the list of formulas
all_pgls_resids<-lapply(pgls_todo_sw,pgls_resid_outputs)# returns a list of dataframes

# extract STANDARDIZED residuals:
#pgls_resid_std<-function(i){
#  pglsfit<-pgls(as.formula(i), data = birdCDO, lambda = 'ML', bounds = list(lambda = c(0.1,1)))
#  resids_std<-residuals(pglsfit)/sd(residuals(pglsfit))
#  #species<- row.names(resids)
#  return(resids_std)
#}
#
##2) apply the residuals function to the list of formulas
#all_pgls_resids<-lapply(pgls_todo_odd,pgls_resid_std)# returns a list of dataframes


#attach the residuals to the original data frame
avgdf$V1<-all_pgls_resids[[1]][match(avgdf$Binomial,row.names(all_pgls_resids[[1]]))]# attach
avgdf$V2<-all_pgls_resids[[2]][match(avgdf$Binomial,row.names(all_pgls_resids[[2]]))]
avgdf$V3<-all_pgls_resids[[3]][match(avgdf$Binomial,row.names(all_pgls_resids[[3]]))]
avgdf$V4<-all_pgls_resids[[4]][match(avgdf$Binomial,row.names(all_pgls_resids[[4]]))]
avgdf$V5<-all_pgls_resids[[5]][match(avgdf$Binomial,row.names(all_pgls_resids[[5]]))]
avgdf$V6<-all_pgls_resids[[6]][match(avgdf$Binomial,row.names(all_pgls_resids[[6]]))]
avgdf$V7<-all_pgls_resids[[7]][match(avgdf$Binomial,row.names(all_pgls_resids[[7]]))]
avgdf$V8<-all_pgls_resids[[8]][match(avgdf$Binomial,row.names(all_pgls_resids[[8]]))]
avgdf$V9<-all_pgls_resids[[9]][match(avgdf$Binomial,row.names(all_pgls_resids[[9]]))]
avgdf$V10<-all_pgls_resids[[10]][match(avgdf$Binomial,row.names(all_pgls_resids[[10]]))]
avgdf$V11<-all_pgls_resids[[11]][match(avgdf$Binomial,row.names(all_pgls_resids[[11]]))]
avgdf$V12<-all_pgls_resids[[12]][match(avgdf$Binomial,row.names(all_pgls_resids[[12]]))]
avgdf$V13<-all_pgls_resids[[13]][match(avgdf$Binomial,row.names(all_pgls_resids[[13]]))]
avgdf$V14<-all_pgls_resids[[14]][match(avgdf$Binomial,row.names(all_pgls_resids[[14]]))]
avgdf$V15<-all_pgls_resids[[15]][match(avgdf$Binomial,row.names(all_pgls_resids[[15]]))]
avgdf$V16<-all_pgls_resids[[16]][match(avgdf$Binomial,row.names(all_pgls_resids[[16]]))]
#avgdf$V17<-all_pgls_resids[[17]][match(avgdf$Binomial,row.names(all_pgls_resids[[17]]))]

#rename the newly added residual columns to be more informative
names(avgdf)#check numbers of newly added columns to rename<--
oldnames = colnames(avgdf[,24:39])# check numbers of newly added columns to rename<-------------------------<----<-----
newnames = paste("RES_",str_replace_all(pgls_todo_sw,"[^[:alnum:]]",""), sep = "")

#create new dataframe with residuals
dfwithresids<-avgdf %>% rename_at(vars(oldnames), ~ newnames)#dplyr function
names(dfwithresids)


