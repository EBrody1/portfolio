# Evan Brody
# UMGC Data 630 Assignment #5
#import the data set
leaf <- read.csv("C:/Users/Yissa/OneDrive/data_UMGC/630/Clustering/leaf.csv", as.is = FALSE)
# set variable types as factor for categorical
leaf$Species <- factor(leaf$Species)
leaf$SpecimenNumber <- factor(leaf$SpecimenNumber)
# check structure
str(leaf)
# summary stats
summary(leaf)
# explore
# dependent variabe distribution 
barplot(table(leaf$Species), main = 'Distribution of Species', xlab= 'Species')
# which variables vary with species
sapply(names(leaf), function(cname){
  if (is.numeric(leaf[[cname]]))
    barplot(by(leaf[[cname]],leaf$Species, mean), main=cname, xlab = 'Species')
    
})
# find skewed variables for the whole data set
sapply(names(leaf), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(leaf[[cname]]))
    # use the `main` param to put column name as plot title
    hist(leaf[[cname]], main=cname)
})

# make a copy of the data set
leaf1 <- leaf
# drop categorical variables of class
leaf1$Species <- NULL
leaf1$SpecimenNumber <- NULL

# heatmap of correlation matrix
cormat <- round(cor(leaf1),2)
library(reshape2)
melted_cormat <- melt(cormat)
library(ggplot2)
ggheatmap <-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# add the coefficients
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    #legend.justification = c(1, 0),
    #legend.position = c(0.6, 0.7),
    #legend.direction = "horizontal"
  )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#preprocessing
# Number of Missing Values for Each Variable
apply(leaf1, 2, function (leaf) sum(is.na(leaf)))

# scale all columns
for( i in names(leaf1))
{
  leaf1[i]<-scale(leaf1[i])
}
summary(leaf1)
# Run the method
set.seed(1234)

#Run the method with k=6 for 30 species of leaves
kc<-kmeans(leaf1, 6, nstart = 20)
# Print the method output
kc

#Total sum of squared distances within cluster.
kc$tot.withinss
#Sum of squared distances between instances and other cluster centers
kc$betweenss
#Sum of squared distances within cluster+sum of squared distances between other clusters
kc$totss
# calculate summary stats
library("fpc")
clust_stats <- cluster.stats(d = dist(leaf1), 
                              kc$cluster)
clust_stats

# Cluster to class analysis confusion matrix
table(leaf$Species, kc$cluster,  dnn=c('Class in the dataset', 'Cluster mumber'))

# plot orginial df by cluster
#Let's plot  2 variables and the cluster centers
plot(leaf1[c('Eccentricity', 'Entropy')], col=kc$cluster)
# add centers to the plot
points(kc$centers[,c('Eccentricity', 'Entropy')], col=1:6, pch=8, cex=2)

# Cluster visualization
library('cluster')
#Number the points, number the clusters, use different colors and shading
clusplot(leaf1, kc$cluster, color=TRUE, shade=T, labels=2, lines=0)
plotcluster(leaf1,kc$cluster)

# find other values for K
#Plot the sum of squared distances between clusters as k value increases
#Use k between 2 and 15
bss<-integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(leaf1,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",
     ylab="Sum of squares", col="blue") 
wss<-integer(length(2:15))
#Plot the sum of squared distacnes within clusters as k value increases
for (i in 2:15) wss[i] <- kmeans(leaf1, centers=i)$tot.withinss
lines(1:15, wss, type="b" ) 
# looks like 4, 6, or 10
# try with k =4 
kc<-kmeans(leaf1, 4, nstart = 20)
# Print the method output
kc

#Total sum of squared distances within cluster.
kc$tot.withinss
#Sum of squared distances between instances and other cluster centers
kc$betweenss

clust_stats <- cluster.stats(d = dist(leaf1), 
                             kc$cluster)
clust_stats

#Number of iterations the method took to run
kc$iter

# Cluster to class analysis confusion matrix
table(leaf$Species, kc$cluster,  dnn=c('Class in the dataset', 'Cluster mumber'))

# plot orginial df by cluster
#Let's plot  2 variables and the cluster centers
plot(leaf1[c('Eccentricity', 'Entropy')], col=kc$cluster)
# add centers to the plot
points(kc$centers[,c('Eccentricity', 'Entropy')], col=1:6, pch=8, cex=2)

# Cluster visualization
#Number the points, number the clusters, use different colors and shading
clusplot(leaf1, kc$cluster, color=TRUE, shade=T, labels=2, lines=0)
plotcluster(leaf1,kc$cluster)

# try with k =13 as sqrt of n/2 l
kc<-kmeans(leaf1, 13, nstart = 20)
# Print the method output- most of the output components
kc

#Total sum of squared distances within cluster.
kc$tot.withinss
#Sum of squared distances between instances and other cluster centers
kc$betweenss

clust_stats <- cluster.stats(d = dist(leaf1), 
                             kc$cluster)
clust_stats


# Cluster to class analysis confusion matrix
table(leaf$Species, kc$cluster,  dnn=c('Class in the dataset', 'Cluster mumber'))

# plot orginial df by cluster
#Let's plot  2 variables and the cluster centers
plot(leaf1[c('Eccentricity', 'Entropy')], col=kc$cluster)
# add centers to the plot
points(kc$centers[,c('Eccentricity', 'Entropy')], col=1:6, pch=8, cex=2)

# Cluster visualization
#Number the points, number the clusters, use different colors and shading
clusplot(leaf1, kc$cluster, color=TRUE, shade=T, labels=2, lines=0)
plotcluster(leaf1,kc$cluster)


# different algorithm for calculating the k
best<-pamk(leaf1)

#best number of clusters
best$nc
#Cluster information, including min and average distance
best$pamobject["clusinfo"]
#Cluster centers
best$pamobject["medoids"]

#Clusters to class comparison
table(best$pamobject[["clustering"]],leaf$Species)

#Plot the clusters
clusplot(leaf1,best$pamobject[["clustering"]] , color=T)
plotcluster(leaf1,best$pamobject[["clustering"]] )
# Compute cluster stats
species <- as.numeric(leaf$Species)
clust_stats <- cluster.stats(d = dist(leaf1), 
                             species, best$cluster)
clust_stats

#Hierchial
# Euclidean distance matrix
d <- dist(leaf1, method = "euclidean")

H.fit <- hclust(d, method="ward.D")

plot(H.fit) # display dendrogram
groups <- cutree(H.fit, k=6) # cut tree into 6 clusters 
# display groups
groups
# draw dendogram with red borders around the clusters 
rect.hclust(H.fit, k=6, border="red") 
# confusion matrix
table(leaf$Species,groups)
stats <- cluster.stats(d,  groups)
# Dun index
stats$dunn
stats

groups <- cutree(H.fit, k=4) 
# display groups
groups
# draw dendogram with blue borders around the clusters 
rect.hclust(H.fit, k=4, border="Blue") 
# confusion matrix
table(leaf$Species,groups)
stats <- cluster.stats(d,  groups)
# Dun index
stats$dunn
# all stats
stats


# cluster the species and not the leaves
# create df of mean values for each species 
library(dplyr)
leaf2 <- leaf %>% 
  group_by(Species) %>%
  summarise_all("mean")
# drop the identifier
leaf2$SpecimenNumber <- NULL
# scale
leaf2 <- as.data.frame(scale(leaf2[,c(2:15)]))


# find values for K
#Plot the sum of squared distances between clusters as k value increases
#Use k between 2 and 15
for (i in 2:15) bss[i] <- kmeans(leaf2,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",
     ylab="Sum of squares", col="blue") 
#Plot the sum of squared distacnes within clusters as k value increases
for (i in 2:15) wss[i] <- kmeans(leaf2, centers=i)$tot.withinss
lines(1:15, wss, type="b" ) 

# try with k = 9
kc<-kmeans(leaf2, 9, nstart = 20)
# Print the method output- most of the output components
kc

#Total sum of squared distances within cluster.
kc$tot.withinss
#Sum of squared distances between instances and other cluster centers
kc$betweenss

clust_stats <- cluster.stats(d = dist(leaf2), 
                             kc$cluster)
clust_stats

#Cluster centers matrix.
kc$centers

# plot orginial df by cluster
#Let's plot  2 variables and the cluster centers
plot(leaf2[c('Eccentricity', 'Entropy')], col=kc$cluster)
# add centers to the plot
points(kc$centers[,c('Eccentricity', 'Entropy')], col=1:6, pch=8, cex=2)

# Cluster visualization
#Number the points, number the clusters, use different colors and shading
clusplot(leaf2, kc$cluster, color=TRUE, shade=T, labels=2, lines=0)
plotcluster(leaf2,kc$cluster)

# Euclidean distance matrix
d <- dist(leaf2, method = "euclidean") 
H.fit <- hclust(d, method="ward.D")
plot(H.fit) # display dendrogram
groups <- cutree(H.fit, k=9) # cut tree into 9 clusters
# display groups
groups
# draw dendogram with red borders around the clusters 
rect.hclust(H.fit, k=9, border="red") 
# compare to models above with 4 clusters
rect.hclust(H.fit, k=4, border="Blue") 
# find metrics
stats <- cluster.stats(d,  groups)
# Dun index
stats$dunn
