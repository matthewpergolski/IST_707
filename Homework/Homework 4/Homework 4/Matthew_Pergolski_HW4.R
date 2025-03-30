# Matthew L. Pergolski

################################################

# NOTE : SOME CODE IN THIS ASSIGNMENT WAS INSPIRED BY THE FOLLOWING LINK:

# https://uc-r.github.io/hc_clustering

# PROVIDING THE LINK FOR TRANSPARENCY AND CREDIBILITY

################################################

# HW4: Use Clustering to Solve a Mystery in History
# 
# In this homework assignment, you are going to use clustering methods to solve a mystery In
# history: who wrote the disputed essays, Hamilton or Madison?
#   
#   1. About the Federalist Papers
# 
# Quote from the Library of Congress
# http://www.loc.gov/rr/program/bib/ourdocs/federalist.html
# 
# The Federalist Papers were a series of eighty-five essays urging the citizens of New York to
# ratify the new United States Constitution. Written by Alexander Hamilton, James Madison, and
# John Jay, the essays originally appeared anonymously in New York newspapers in 1787 and
# 1788 under the pen name "Publius." A bound edition of the essays was first published in 1788,
# but it was not until the 1818 edition published by the printer Jacob Gideon that the authors of
# each essay were identified by name. The Federalist Papers are considered one of the most
# important sources for interpreting and understanding the original intent of the Constitution.
# 
# 2. About the disputed authorship
# 
# The original essays can be downloaded from the Library of Congress.
# http://thomas.loc.gov/home/histdox/fedpapers.html
# 
# In the author column, you will find 74 essays with identified authors: 51 essays written by
# Hamilton, 15 by Madison, 3 by Hamilton and Madison, 5 by Jay. The remaining 11 essays,
# however, is authored by "Hamilton or Madison". These are the famous essays with disputed
# authorship. Hamilton wrote to claim the authorship before he was killed in a duel. Later Madison
# also claimed authorship. Historians were trying to find out which one was the real author.
# 
# 3. Computational approach for authorship attribution
# 
# In 1960s, statistician Mosteller and Wallace analyzed the frequency distributions of common
# function words in the Federalist Papers, and drew their conclusions. This 1S a pioneering work on
# using mathematical approaches for authorship attribution.
# http://www.stat.cmu.edu/~vlachos/courses/724/final/mosteller.pdf
# 
# Nowadays, authorship attribution has become a classic problem in the data mining field, with
# applications in forensics (e.g. deception detection), and information organization.
# 
# In this homework you are provided with the Federalist Paper data set. The features are a set of
# "function words". for example, "upon". The feature value is the percentage of the word
# occurrence in an essay. For example, for the essay "Hamilton fed 31 .txt". if the function word
# "upon" appeared 3 times, and the total number of words in this essay is 1000, the feature value is
# 3/1000=0.3%
# 
# Now you are going to try solving this mystery using clustering algorithms k-Means and
# HAC. Document your analysis process and draw your conclusion on who wrote the disputed
# essays. Provide evidence for each method to demonstrate what patterns had been learned to
# predict the disputed papers, for example, visualize the clustering results and show where the
# disputed papers are located in relation to Hamilton and Madison's papers. By the way, where are
# the papers with joint authorship located? For k-Means, analyze the centroids to explain
# which attributes are most useful for clustering. Hint: the centroid values on these dimensions
# should be far apart from each other to be able to distinguish the clusters.
# 

library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)

# Load Data Set
  
  fed.papes <- read.csv('/Users/pergolicious/Downloads/fedPapers85.csv')
  str(fed.papes)
  summary(fed.papes)
  View(fed.papes)

  
# Data Preparation / Cleaning
  
  fed.papes[,c('author', 'filename')] <- lapply(fed.papes[,c('author', 'filename')], factor)
  str(fed.papes)
  
  # scale data
  str(fed.papes[,3:ncol(fed.papes)])
  fed.papes.scale <- scale(fed.papes[,3:ncol(fed.papes)])
  colnames(fed.papes)

  
  
  # K MEANS
  
  k.means <- kmeans(fed.papes.scale, 5)
  k.means
  
  k.means$centers
  
  assignment.clusters <- data.frame(fed.papes[,1:2], k.means$cluster)
  head(assignment.clusters)
  View(assignment.clusters)
  
  clusplot(fed.papes.scale, k.means$cluster, color = TRUE, shade = TRUE,
           Labels = 2, lines = 0)
  
  
  
# CLUSTERING METHODS
  
  # Hierarchical 
  
  euc <- dist(fed.papes.scale, method = 'euclidean')
  euc  
  
  hc <- hclust(euc, method = 'complete')
  hc

  hc.plot <- plot(hc, cex = 0.6, hang = -1)
  hc.plot <- plot(hc)
  hc.plot <- plot(hc, cex = .06, hang = 1)

  # Alternative method to hclust | ward has highest value at .93
  
  hc.methods <- c( "average", "single", "complete", "ward")
  names(hc.methods) <- c( "average", "single", "complete", "ward")

  hc.function <- function(x) {
    agnes(fed.papes.scale, method = x)$ac
  }
  
  map_dbl(hc.methods, hc.function)
  
  hc.agnes <- agnes(fed.papes.scale, method = 'ward')
  hc.agnes
  
  pltree(hc.agnes, cex = .6, hang = -1, main = 'Dendrogram of Agnes HC for fed.papes Data Set')
  rect.hclust(hc.agnes, k = 5, border = 2:5)
  
  
################
  # Adding data to cluster column
  
  hc.cluster <- hclust(euc, method = 'ward.D2')
  hc.cluster
  
  cluster.groups <- cutree(hc.cluster, k = 5)
  cluster.groups
  
############

  plot(hc.cluster, cex = .6, hang = -1)
  rect.hclust(hc.cluster, k = 5, border = 2:5)
  


  