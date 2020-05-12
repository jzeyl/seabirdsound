library(phytools)
library(ggalt)
library(ggrepel)
library(cowplot)
library(gridGraphics)

#select species names and residuals for new dataframe
dfwithresids2<-dfwithresids[,c(1,24:39)]
dfwithresids$Birdtree<-gsub(" ", "_", distinctdf$Birdtree)# change species names formatting to match tree

#isolate columns to be used in PCA
names(dfwithresids2)
PCAset<-dfwithresids2[,c(2:17)]# data to be used in PCA
row.names(PCAset)<-dfwithresids2[,1]#species names need to be in row names for use of phy.PCA function

#remove NAs
naomit_PCA<-na.omit(PCAset)

#trim phylogeny to the species used in the PCA
phyPCA<-keep.tip(birdtreels,row.names(naomit_PCA))
plot(phyPCA)

#run phyPCA
pPCA<- phyl.pca(phyPCA,naomit_PCA, 
                method = "lambda", 
                mode = "corr")

#plot PCA
par(mar = c(4,3,3,3))
plot(pPCA)
biplot(pPCA)

#pdf("D:/Analysis_plots/May10phyPCAbiplot.pdf", onefile = TRUE,
#    width = 10,
#    height = 10)
#dev.off()

#put PCA values and loadings in dataframes
speciesPCAvalues<-as.data.frame(pPCA$S)
pPCAloadings<-as.data.frame(pPCA$L)

#attach categores and species names to the speciesPCAvalues dataframe
# input the appropriate column number in the last term
names(dfwithresids)
speciesPCAvalues$Category<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                    dfwithresids$Binomial),21])#<---------------<------------#(names(dfwithresids))
speciesPCAvalues$Low.Hz<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                      dfwithresids$Binomial),23])#<-------------<--------
speciesPCAvalues$Binomial<-as.factor(dfwithresids[match(row.names(speciesPCAvalues),
                                                        dfwithresids$Binomial),1])#<-------------<-------

#plot PC1 vs PC2 by aquatic category
scattercat1<-ggplot(speciesPCAvalues, aes(x = PC1, y = PC2, label = Binomial)) +
  geom_point(aes(col = Category)) +
  geom_text_repel(aes(col = Category)) +
  scale_color_manual(values=c("blue","purple","green"))+
  scale_fill_manual(values=alpha(c("blue","purple","green"), 0.3))+
  theme_bw()+
  geom_encircle(aes(colour = Category, fill = Category),s_shape=1, expand=0)#s_shape = 1 and expan = 0 are convex hull
scattercat1

#aquatic category PC2 PC3
scattercat2<-ggplot(speciesPCAvalues, aes(x = PC2, y = PC3, label = Binomial)) +
  geom_point(aes(col = Category)) +
  geom_text(aes(col = Category)) +
  scale_color_manual(values=c("blue","purple","green"))+
  theme_bw()
scattercat2

#aquatic category PC3 PC4
scattercat3<-ggplot(speciesPCAvalues, aes(x = PC3, y = PC4, label = Binomial)) +
  geom_point(aes(col = Category)) +
  geom_text(aes(col = Category)) +
  scale_color_manual(values=c("blue","purple","green"))+
  theme_bw()
scattercat3

#get % variance explained for scree plot
#from:
#http://blog.phytools.org/2011/12/percent-variance-from-phylpca.html
#require(phytools)
#res<-phyl.pca(tree,X)
#diag(res$Eval)/sum(res$Eval)*100
#PC1      PC2      PC3      PC4      PC5 
#26.83767 23.78144 20.18687 15.85040 13.34361

#SCREE PLOT
d<-as.data.frame(diag(pPCA$Eval)/sum(pPCA$Eval)*100)
d$PC1<-row.names(d)
d$percentexplained<-d$`diag(pPCA$Eval)/sum(pPCA$Eval) * 100`
str(diag(pPCA$Eval)/sum(pPCA$Eval)*100)

#plot screeplot
p<-ggplot(d, aes(x = reorder(PC1,-percentexplained), y = percentexplained))+
geom_bar(stat = "identity")+
theme_bw()
p

#plot loadings onto PCA axes in terms of functional groupings
#barchart loadings
pPCAloadings$variable<-as.factor(row.names(pPCAloadings))

#add category data in 'use' column
levels(pPCAloadings$variable)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[1]|
                     pPCAloadings$variable ==levels(pPCAloadings$variable)[15],
                     "Impedance: Middle ear stiffness",
                     "toclassify")
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[7]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[8],
                         "Impedance: Columella size",
                         pPCAloadings$use)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[10]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[12]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[13]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[5]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[4],
                         "Impedance: areas of vibrational input/output",
                         pPCAloadings$use)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[2]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[3]|
                           pPCAloadings$variable ==levels(pPCAloadings$variable)[6],
                         "Other shape metrics",
                         pPCAloadings$use)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[14],
                         "Cochlea length",
                         pPCAloadings$use)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[11],
                           #pPCAloadings$variable ==levels(pPCAloadings$variable)[16],
                         "Impedance: conical protrusion",
                         pPCAloadings$use)
pPCAloadings$use<-ifelse(pPCAloadings$variable ==levels(pPCAloadings$variable)[9],
                         "Impedance: offset of columella \n from TM center (possible lever)",
                         pPCAloadings$use)

#plot loadings
loadingplot<-ggplot(pPCAloadings, aes(x = reorder(variable,PC1), y = PC1))+
    geom_bar(aes(fill = use), stat = "identity")+
    coord_flip()+
    theme_bw()+
    theme(legend.position="bottom")
loadingplot

#plot PC1 on phylogeny
PC1named<-setNames(speciesPCAvalues$PC1,row.names(speciesPCAvalues))
PC1contmap<-contMap(phyPCA,PC1named,plot=FALSE)#basically combines the tree + variable
PC1contmap
plot(PC1contmap)#plot

