voca = read.table("vocalisation_data.txt",sep="\t",header=TRUE)

par(mfrow=c(2,2))
myPCA <- prcomp(voca, scale. = T, center = T)
sx=summary(myPCA)
plot(myPCA$x[,1:2],main="Scale = TRUE | Center = TRUE",
     xlab=paste("PCA1",round(sx$importance[2,1]*100,digits=1),"%"),ylab=paste("PCA2:",round(sx$importance[2,2]*100,digits=1),"%") )

myPCA <- prcomp(voca, scale. = F, center = T)
sx=summary(myPCA)
plot(myPCA$x[,1:2],main="Scale = FALSE | Center = TRUE",
     xlab=paste("PCA1",round(sx$importance[2,1]*100,digits=1),"%"),ylab=paste("PCA2:",round(sx$importance[2,2]*100,digits=1),"%") )

myPCA <- prcomp(voca, scale. = T, center = F)
sx=summary(myPCA)
plot(myPCA$x[,1:2],main="Scale = TRUE | Center = FALSE",
     xlab=paste("PCA1",round(sx$importance[2,1]*100,digits=1),"%"),ylab=paste("PCA2:",round(sx$importance[2,2]*100,digits=1),"%") )

myPCA <- prcomp(voca, scale. = F, center = F)
sx=summary(myPCA)
plot(myPCA$x[,1:2],main="Scale = FALSE | Center = FALSE",
     xlab=paste("PCA1",round(sx$importance[2,1]*100,digits=1),"%"),ylab=paste("PCA2:",round(sx$importance[2,2]*100,digits=1),"%") )


###### Selecting Scale FALSE and Center TRUE
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

par(mfrow=c(2,2))

myPCA <- prcomp(voca, scale. = F, center = T)
sx=summary(myPCA)
screeplot(myPCA,"SCREE plot") 

# PCA corr with variables
g <- ggbiplot(myPCA, obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# Clustering
options(scipen=999)
library(gplots)
library(factoextra)
library(RColorBrewer)
colors <- rev(colorRampPalette( (brewer.pal(9, "RdBu")) )(9))
x = heatmap.2(as.matrix(voca),col=colors,scale="column", trace="none",distfun = function(x) get_dist(x,method="pearson"),srtCol=90,cexCol=.8)    
x = heatmap.2(as.matrix(voca),col=colors,scale="none", trace="none",distfun = function(x) get_dist(x,method="euclidean"),srtCol=90,cexCol=.8)    
hc <- as.hclust( x$rowDendrogram )
groups=as.character(cutree( hc, k=6 ))

g <- ggbiplot(myPCA, obs.scale = 1, var.scale = 1, 
               ellipse = TRUE, groups = groups,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

########## B
library(MCMCglmm )
data(BTdata)
data(BTped)
              
