#Akash Gairola

# Hierarchical Clustering demonstration as a supervised learning technique.

#Directly set the working directly using file panel 

library(cluster)
library(factoextra)
library(fpc)

options(digits=3)

rm(list=ls())

trans <- read.csv(file = "file46.txt",comment.char = "#",sep = "", header= T, row.names = 1)

#(a)Running Hierarchical clustering
c.single <- eclust(trans, "hclust", hc_method="single")
fviz_dend(c.single, palette="jco", as.ggplot=T, show_labels = TRUE,main = "Single Linkage")

c.complete <- eclust(trans, "hclust", hc_method="complete")
fviz_dend(c.complete, palette="jco", as.ggplot=T, show_labels = TRUE,main = "Complete Linkage")

c.average <- eclust(trans, "hclust", hc_method="average")
fviz_dend(c.average, palette="uchicago", as.ggplot=T, show_labels = TRUE,main = "Average Linkage")



#(B) countries clustered together as two-singleton clusters
#Single Linkage
#Denmark, Norway
#France, Belgium
#Luxemburg, Switzerland
#West Germany, Austria
#Great Britain, Ireland 

#Compete Linkage
#France, Belgium
#Great Britain, Ireland
#Denmark, Norway
#Luxemburg, Switzeland
#West Germany, Austria  


#Average Linkage
#Great Britain, Ireland
#Denmark, Norway
#Belgium, France 
#Luxemburg, Switzerland
#West Germany, Austria
#Portugal, Spain  


#(C)Complete link can break large clusters. According to raw data, Italy is quite similar 
# to France and therefore is clustered to it. Complete cluster accurately reflects how Italy should be
#clustered


#(D)
#purity as the linkage strategy that produces the most two-singleton clusters
#Therefore Average Linkage would be purest as it produces 6 two-singleton clusters

#(E)Cut at Height = 125, we get 7 Clusters
cut.avg <- cutree(c.average, h = 125)
table(cut.avg)

#(F)hierarchical clustering with k = 7
c.single <- eclust(trans, "hclust", hc_method="single", k=7)
fviz_dend(c.single, palette="jco", as.ggplot=T, show_labels = TRUE)

c.complete <- eclust(trans, "hclust", hc_method="complete", k=7)
fviz_dend(c.complete, palette="jco", as.ggplot=T, show_labels = TRUE)

c.average <- eclust(trans, "hclust", hc_method="average", k=7)
fviz_dend(c.average, palette="uchicago", as.ggplot=T, show_labels = TRUE)


#(g)Dunn and Silhouette width
Single <- cluster.stats(dist(trans), c.single$cluster)
Single$dunn
Single$avg.silwidth

Complete <- cluster.stats(dist(trans), c.complete$cluster)
Complete$dunn
Complete$avg.silwidth

Average <- cluster.stats(dist(trans), c.average$cluster)
Average$dunn
Average$avg.silwidth

#(H)Average linkage cluster is best cluster obtained based on Dunn index as it is highest as compare to others.

#(I)Complete linkage cluster is best cluster obtained based on Silhoutte width as it is highest as compare to others.