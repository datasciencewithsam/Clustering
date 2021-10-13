gc()
rm(list = ls(all = TRUE))


packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(tidyverse) # data manipulation
packages(corrplot)
packages(gridExtra)
packages(GGally)
packages(cluster) # clustering algorithms 
packages(factoextra)

data("USArrests")      # Loading the data set
#scaled the dataset
USArrests.scaled <- as.data.frame(apply(USArrests, MARGIN = 2, FUN = scale))
#apply kmeans 
USArrests.scaled.clusters <- kmeans(USArrests.scaled, centers = 2, nstart = 50, iter.max = 1000)
#Assign cluster value
USArrests.scaled$Cluster <- USArrests.scaled.clusters$cluster
USArrests$Cluster <- USArrests.scaled.clusters$cluster # Scaling the data


# Factor Clusters
USArrests.scaled$Cluster <- factor(USArrests.scaled$Cluster, levels = c(1, 2), labels = c('Not-So-Safe State', 'Safe State'))

# Murders Against Population and cluster accordingly 
murder <- ggplot(USArrests.scaled, aes(y = Murder, x = UrbanPop, shape = Cluster)) +
  geom_point() + 
  stat_ellipse(aes(y = Murder, x = UrbanPop, fill = Cluster), geom = 'polygon', alpha = 0.21, level = 0.95)

# Murders Against Population and cluster accordingly 
assault <- ggplot(USArrests.scaled, aes(y = Assault, x = UrbanPop, shape = Cluster)) +
  geom_point() +
  stat_ellipse(aes(y = Assault, x = UrbanPop, fill = Cluster), geom = 'polygon', alpha = 0.21, level = 0.95)

# Murders Against Population and cluster accordingly 
rape <- ggplot(USArrests.scaled, aes(y = Rape, x = UrbanPop, col = Cluster, shape = Cluster)) +
  geom_point() +
  stat_ellipse(aes(y = Rape, x = UrbanPop, fill = Cluster), geom = 'polygon', alpha = 0.21, level = 0.95)

murder
