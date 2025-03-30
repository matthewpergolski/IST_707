

# Clustering | Attempt Utilizing Sample Data


###############################################################################################

# LOAD DATA SET
    revised.proj.data <- read.csv('/Users/pergolicious/OneDrive - Syracuse University/Syracuse University/Courses/IST 707/Final Project/Balanced_FoodDesertComparisonData_2006_2019.csv',
                                  stringsAsFactors = TRUE)
    
    str(revised.proj.data)
    summary(revised.proj.data)
    #revised.proj.data <- revised.proj.data[revised.proj.data$foodDesert==1,]
    revised.proj.data
    View(revised.proj.data)


###############################################################################################


# PRE-PROCESSING / DATA MUNGING
    
    # Remove NAs
    revised.proj.data <- na.omit(revised.proj.data)
    head(revised.proj.data)
    
    
    # Convert Variables to FACTOR
    colnames(revised.proj.data[,c(4,7,10)])
    revised.proj.data[,c(4,7,10)] <- lapply(revised.proj.data[,c(4,7,10)], factor)
    str(revised.proj.data)
    
    revised.proj.data$`Poverty Rate Factored` <- revised.proj.data$PovertyRate
    str(revised.proj.data)
    
    
    # Discretize poverty rate
    min.pov <- min(revised.proj.data$`Poverty Rate Factored`)
    min.pov
    
    max.pov <- max(revised.proj.data$`Poverty Rate Factored`)
    max.pov
    
    bins <- 3
    
    mid.pov <- (max.pov - min.pov) / bins
    mid.pov
    
    mid.pov * 3
    
    revised.proj.data$`Poverty Rate Factored` <- cut(revised.proj.data$`Poverty Rate Factored`,
                                                     breaks = c(min.pov, mid.pov, max.pov, Inf),
                                                     labels = c('min', 'mid', 'max'))
    
    str(revised.proj.data)
    
    
    # Load Additional Packages
    library(tidyverse)  # data manipulation
    library(cluster)    # clustering algorithms
    library(factoextra) # clustering visualization
    library(dendextend) # for comparing two dendrograms
    
    
    str(revised.proj.data)
    str(revised.proj.data[,c(1,5:6,8:9)])
    num.revised.proj.data <- revised.proj.data[,c(1,5:6,8:9)]
        #num.revised.proj.data <- revised.proj.data[,c(-2,-3)]
    str(num.revised.proj.data)
    
    num.revised.proj.data <- na.omit(num.revised.proj.data)
        #num.revised.proj.data <- data.frame(scale(num.revised.proj.data))
    str(num.revised.proj.data)

####################################################################################

# MODELS


# Agnes Function (based on sample data, with replacement) /Dendrogram

    samp.num.proj.dataa <- num.revised.proj.data[sample(nrow(num.revised.proj.data), 1000, replace = TRUE),]
    #num.revised.proj.data <- revised.proj.data[1:5000,]
    #hc2 <- agnes(condensed.num.revised.proj.data, method = "complete")
    H.C <- agnes(samp.num.proj.dataa, method = "ward")
    
    # Agglomerative coefficient
    H.C$ac
    ## [1] 0.8531583
    
    pltree(H.C, main = "Dendrogram of agnes") 
    

# hclust Function / Dendrogram
    
    # Dissimilarity matrix
    d <- dist(samp.num.proj.dataa, method = "euclidean")
    
    # Hierarchical clustering using Complete Linkage
    hc1 <- hclust(d, method = "complete" )
    
    # Plot the obtained dendrogram
    plot(hc1, cex = 0.6, hang = -1)
    
    # Agglomerative coefficient
    H.C$ac
    
    pltree(H.C, cex = 0.6, hang = -1, main = "Dendrogram of AGNES") 
    
    rect.hclust(H.C, k = 2)



# Cluster Plot
    
    # Cut tree into 4 groups
    sub_grp <- cutree(H.C, k = 2)
    
    # Number of members in each cluster
    table(sub_grp)
    ## sub_grp
    ##  1  2  3  4 
    ##  7 12 19 12
    
    samp.num.proj.dataa %>%
      mutate(cluster = sub_grp) %>%
      head
    fviz_cluster(list(data = samp.num.proj.dataa, cluster = sub_grp))  

    
# K MEANS
    
    k.means <- kmeans(samp.num.proj.dataa, 2)
    k.means
    k.means$centers
    
    assignment_clusters <- data.frame(samp.num.proj.dataa, k.means$cluster)
    assignment_clusters
    head(assignment_clusters)
    
    library(cluster)
    clusplot(samp.num.proj.dataa, k.means$cluster, color=TRUE, shade=TRUE,
             Labels=2, lines=0) # plot clusters
    
    
    
    
    
    distance <- get_dist(samp.num.proj.dataa)
    #fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white"))
    
    
    
    
