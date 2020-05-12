#plot predicted values of interactions for small and large birds, for different aquatic categories
library(caper)
library(phytools)
library(ape)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(stringr)
library(tidyr)


#ensure that the model list includes the models with main effect and interaction terms;
#otherwise, the extraction of the coefficient numbers here will be off. From preceding script I used
#this model list:
pgls_models_list<-lapply(pgls_todo_category_int_sw,pgls_models)#


#Split skull width into 3 groups: (small, medium, and large heads) to be used for computing
#predicted values
avgdf$Skull_width<-as.factor(ntile(avgdf$Skull.width..mm.,3))
meanheads<-lapply(split(avgdf$Skull.width..mm.,avgdf$Skull_width),mean)#take the mean in each group
loggedheads<-unname(unlist(lapply(meanheads,log)))#log transform the mean

#get indices of models in the list that have significant interaction terms 
#(I found these by sorting the table of model outputs output in excel)
pgls_todo_category_int_sw
#Significant skull width *category interactions were found for RW, TM, height of Umbo, and area ratio, models # 7,8,9,10, 11, 

#function to compute predicted values in each aquatic group for small, medium, and large head sizes
predslist<-function(x,modelnumber){
  predictedPD<-unname(pgls_models_list[modelnumber][[1]]$model$coef[2]*x+
                        pgls_models_list[modelnumber][[1]]$model$coef[1])
  predictedSF<-unname((pgls_models_list[modelnumber][[1]]$model$coef[2]+pgls_models_list[modelnumber][[1]]$model$coef[5])*x+
                        pgls_models_list[modelnumber][[1]]$model$coef[1]+pgls_models_list[modelnumber][[1]]$model$coef[3])
  predictedterr<-unname((pgls_models_list[modelnumber][[1]]$model$coef[2]+pgls_models_list[modelnumber][[1]]$model$coef[6])*x+
                          pgls_models_list[modelnumber][[1]]$model$coef[1]+pgls_models_list[modelnumber][[1]]$model$coef[4])
  mylist<-c(PD = predictedPD,SF = predictedSF,Terr = predictedterr)
  return(mylist)
}

#apply the predicted values functions to the specific models that have significant interaction terms
modelindices<-c(7,8,9,10,11)
for (i in modelindices){
  df<-as.data.frame(sapply(loggedheads,predslist,modelnumber = i))#input logged head size, get matrix out
  df<-as.data.frame(t(df))
  #names(df) <- c("17mm","27 mm","44 mm")
  df$headsize <- c("17mm","27 mm","44 mm")# values of meanheads above
  df$'TERR_diff_logged'<-df$'Terr'-df$'PD'#get difference of predicted values (on log scale)
  df$'TERR_diff_exp'<-exp(df$'TERR_diff_logged')#get ratio of predicted values in raw units
  df$'SF_diff_logged'<-df$'SF'-df$'PD'#get difference of predicted values (on log scale)
  df$'SF_diff_exp'<-exp(df$'SF_diff_logged')#get ratio of predicted values in raw units
  df$measure<-pgls_todo_category_int_sw[i]#add the formula as a column
  assign(paste0("A",i),df)# this assigns each dataframe as an object, to be combined in the next step with
}

#combine dataframes for each different ear measurement into single
#function to combine objects into single dataframes:
dfnames<-as.list(paste0("A",modelindices)) 
AppendMe <- function(dfNames) {#https://stackoverflow.com/questions/15162197/combine-rbind-data-frames-and-create-column-with-name-of-original-data-frames
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}
combineddf<-AppendMe(dfnames)#run function on dataframe names

#format dataframe
combineddf$headsize<-as.factor(combineddf$headsize)
combineddf$measure<-as.factor(combineddf$measure)

#select parameters of interest and put in long format for plotting
combineddfclean<-dplyr::select(combineddf, PD, SF,Terr,headsize,TERR_diff_exp,SF_diff_exp,measure)
combineddflong<-gather(combineddfclean, key = "aqgroup", value = "ratiovalue", -c(PD,SF,Terr,headsize,measure))
#

#make list of ggplots interaction terms
p_int<-list()
for (i in seq_along(levels(combineddflong$measure))){
  p_int[[i]]<-ggplot(data = combineddflong[combineddflong$headsize != "27 mm"&
                                             combineddflong$measure ==levels(combineddflong$measure)[i],], 
                     aes(x = headsize, y = ratiovalue, group = aqgroup, label = exp(PD)))+
    #facet_wrap(~measure, scales = "free")+
    geom_line(aes(color = aqgroup),stat = "identity")+#, position=position_dodge(.3)
    #ggtitle(combineddflong$measure[1])+
    geom_hline(yintercept = 1, color = "blue")+
    theme_bw()+
    #geom_text()+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_color_manual(values = c("grey","green","lightgreen","black","gray","green","darkgray","darkgreen","cornsilk4","blue"))
}
p_int[[1]]#check one of the plots

ggsave("E:/Analysis_plots/interactions may 8.pdf",
       width = 10, height = 10, units = "in")


