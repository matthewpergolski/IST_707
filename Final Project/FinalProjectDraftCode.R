
#smol.proj.data <- read_excel("/Users/pergolicious/Downloads/FoodAccessResearchAtlasData2019.xlsx", 
                           #sheet = 'Food Access Research Atlas')
#warnings()

###############################################################################################

# LOAD DATA SET
  smol.proj.data <- read.csv('/Users/pergolicious/OneDrive - Syracuse University/Syracuse University/Courses/IST 707/Final Project/FoodDesertCompareData_2006_2019.csv',
                           stringsAsFactors = TRUE)
  
  str(smol.proj.data)
  summary(smol.proj.data)
  smol.proj.data[smol.proj.data$foodDesert=='YES',]
  View(smol.proj.data)
  
  
  
  og.proj.data <- read.csv('/Users/pergolicious/OneDrive - Syracuse University/Syracuse University/Courses/IST 707/Final Project/OG_FoodAccessResearchAtlasData2019.csv',
                           stringsAsFactors = FALSE)
  
  
  str(og.proj.data)
  summary(og.proj.data)
  og.proj.data[og.proj.data$foodDesert=='YES',]
  View(og.proj.data)

###############################################################################################
  

  # PRE-PROCESSING / DATA MUNGING
  
  # Remove NAs
  smol.proj.data <- na.omit(smol.proj.data)
  
  # Convert Urban.x and LowIncomeTracts to Factor
  smol.proj.data[,c(4,7)] <- lapply(smol.proj.data[,c(4,7)], factor)
  str(smol.proj.data)
  smol.proj.data$`Poverty Rate Factored` <- smol.proj.data$PovertyRate
  str(smol.proj.data)
  
  # discretize poverty rate
  
  min.pov <- min(smol.proj.data$`Poverty Rate Factored`)
  min.pov
  
  max.pov <- max(smol.proj.data$`Poverty Rate Factored`)
  max.pov
  
  bins <- 3
  
  mid.pov <- (max.pov - min.pov) / bins
  mid.pov
  
  mid.pov * 3
  
  smol.proj.data$`Poverty Rate Factored` <- cut(smol.proj.data$`Poverty Rate Factored`,
                        breaks = c(min.pov, mid.pov, max.pov, Inf),
                        labels = c('min', 'mid', 'max'))
  
  str(smol.proj.data)
  
  # Convert rest of variables to numeric
  # head(smol.proj.data[,c(5,6,8,9)])
  # smol.proj.data[,c(5,6,8,9)] <- lapply(smol.proj.data[,c(5,6,8,9)], numeric)
  
  # Census tract variable necessary?
  smol.proj.data <- smol.proj.data[,-1]
  str(smol.proj.data)
  
  
  
  # ASSOCIATION RULE MINING
  ARM.df <- smol.proj.data[,c(1:3,6,9,10)]
  str(ARM.df)
  
  
  ARM.df.rules <- apriori(ARM.df, 
          parameter = list(supp = 0.1, 
                           conf = 0.6),
          appearance = list(rhs = c('foodDesert=NO','foodDesert=YES')))
  
  # ARM.df.rules <- apriori(ARM.df, 
  #                         parameter = list(supp = 0.02, 
  #                                          conf = 0.75, 
  #                                          maxlen = 4))
  
  options(digits = 2)
  
  ARM.df.rules.sort <- sort(ARM.df.rules, by = 'lift', decreasing = TRUE)
  inspect(ARM.df.rules.sort)
  View(inspect(ARM.df.rules.sort))
  
  ARM.df.rules.vis <- plot(ARM.df.rules.sort, method = 'graph')
  ARM.df.rules
  
# Questions

# 1. Determine data types for all variables | which variables we want to use
# 2. How to deal with NAs/missing values


# Food desert = (1) if you have an area with a poverty rate of 20% or greater and low access (more than 1 mile away from grocery store); 
# take into account household has a vehicle

# how to grocery stores in general decide where to add new stores/?  are they avoiding food deserts?  Does it make the problem worse?

# health insurance / provide discounts for certain benefits for people living in food deserts



  library(tidyverse)  # data manipulation
  library(cluster)    # clustering algorithms
  library(factoextra) # clustering visualization
  library(dendextend) # for comparing two dendrograms

  
  str(smol.proj.data)
  num.smol.proj.data <- smol.proj.data[,c(5:6,8:9,11)]
  str(num.smol.proj.data)
  
  num.smol.proj.data <- na.omit(num.smol.proj.data)
  num.smol.proj.data <- data.frame(scale(num.smol.proj.data))
  str(num.smol.proj.data)
  
  # Compute with agnes
  #condensed.num.smol.proj.data <- num.smol.proj.data[1:10000,]
  samp.num.proj.dataa <- num.smol.proj.data[sample(nrow(num.smol.proj.data), 1000), ]
  num.smol.proj.data <- smol.proj.data[1:5000,]
  #hc2 <- agnes(condensed.num.smol.proj.data, method = "complete")
  H.C <- agnes(samp.num.proj.dataa, method = "ward")
  
  # Agglomerative coefficient
  H.C$ac
  ## [1] 0.8531583
  
  pltree(H.C, main = "Dendrogram of agnes") 
  
####################################################################################
  
  # HIERARCHICAL 1
  
  # Dissimilarity matrix
  d <- dist(samp.num.proj.dataa, method = "euclidean")
  
  # Hierarchical clustering using Complete Linkage
  hc1 <- hclust(d, method = "complete" )
  
  # Plot the obtained dendrogram
  plot(hc1, cex = 0.6, hang = -1)

  
  
  
  # HIERARCHICAL 2
  
  # Agglomerative coefficient
  H.C$ac
  ## [1] 0.8531583
  
  pltree(H.C, cex = 0.6, hang = -1, main = "Dendrogram of AGNES") 
  
  rect.hclust(H.C, k = 4)
  
  
  
  
  
  
  
  
  
  
  
  # Cut tree into 4 groups
  sub_grp <- cutree(H.C, k = 4)
  
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
  
  k.means <- kmeans(num.smol.proj.data, 6)
  k.means
  k.means$centers
  
  assignment_clusters <- data.frame(num.smol.proj.data, k.means$cluster)
  assignment_clusters
  head(assignment_clusters)
  
  
  plot(zoo$type ~ jitter (k.means$cluster, 1),
       pch=21, col=as.factor(zoo$milk))
  
  
  library(cluster)
  clusplot(num.smol.proj.data, k.means$cluster, color=TRUE, shade=TRUE,
           Labels=2, lines=0) # plot clusters

  
  
    
  
  distance <- get_dist(num.smol.proj.data)
  fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high))
  
  
  
  
  
  
  
  