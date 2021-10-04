## load the data set
iris <- datasets::iris

##Perform hierarchical clustering using complete linkage method

hc.iris.complete<- hclust(dist(iris[, 3:4]),method = 'complete')
plot(hc.iris.complete)

# Cut off the tree at the desired number of clusters 
clusterCut <- cutree(hc.iris.complete, 4)
table(clusterCut, iris$Species)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
  geom_point(alpha = 0.1, size = 3) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('blue', 'red', 'green'))

## Additional linkage method for future test
hc.iris.average  <- hclust(dist(iris[, 3:4]), method = 'average')
hc.iris.single   <- hclust(dist(iris[, 3:4]), method = 'single')

## Denodgarm representation
library(dendextend)
library(colorspace)
dend <- as.dendrogram(hc.iris.complete)
# order it the closest we can to the order of the observations:
dend <- rotate(dend, 1:150)

# Color the branches based on the clusters:
dend <- color_branches(dend, k=3) #, groupLabels=iris_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
labels_colors(dend) <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

# We shall add the flower type to the labels:
labels(dend) <- paste(as.character(iris[,-5])[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")
# We hang the dendrogram a bit:
dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
dend <- set(dend, "labels_cex", 0.5)
# And plot:
par(mar = c(3,3,3,7))
plot(dend, 
     main = "Clustered Iris data set
     (the labels give the true flower species)", 
     horiz =  TRUE,  nodePar = list(cex = .007))
legend("topleft", legend = unique(iris$Species), fill = rainbow_hcl(3))

some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

## Additional heat map plot to represent correlation among columns
library(gplots)
gplots::heatmap.2(as.matrix(iris[, -5]), 
                  main = "Heatmap for the Iris data set",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA", # this to make sure the columns are not ordered
                  trace="none",          
                  margins =c(5,0.1),      
                  key.xlab = "Cm",
                  denscol = "grey",
                  density.info = "density",
                  RowSideColors = rev(labels_colors(dend)), # to add nice colored strips        
                  col = some_col_func
)
