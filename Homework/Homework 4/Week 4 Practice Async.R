

# define a function that calculates the Euclidean distance between two vectors a and b
ED <- function(a,b) sqrt(sum((a-b)^2))

# define a function that calculates the cosine similarity between two vectors a and b
CS <- function(a,b) a%*% b/sqrt(a%*%a*b%*%b)

# given two vectors and b
a = c(1,2,3)
b = c(4,5,6)

# call functions to calculate distance
ED(a,b) #= 5.196
CS(a,b) #= 0.975


c <- c(22,1,42,10)
d <- c(20,0,36,8)

ED(c,d)
CS(c,d)







zoo <- read.csv("/Users/pergolicious/Downloads/zoo.csv")
head(zoo)
str(zoo)

zoo_unlabel <- zoo[,c(2:17)]
str(zoo_unlabel)


model_r <- kmeans(zoo_unlabel, 7)
model_r
model_r$centers

assignment_clusters <- data.frame(zoo, model_r$cluster)
assignment_clusters
head(assignment_clusters)


plot(zoo$type ~ jitter (model_r$cluster, 1),
     pch=21, col=as.factor(zoo$milk))


#PCA Visualiuzation of K means
#install.packages("cluster")
library(cluster)
clusplot(zoo_unlabel, model_r$cluster, color=TRUE, shade=TRUE,
         Labels=2, lines=0) # plot clusters


# HAC algorithm within R
d <- dist(as.matrix(zoo_unlabel))
hc <- hclust(d)
plot(hc)

?HierarchicalClusterer
??HierarchicalClusterer







DF <- USArrests
str(USArrests)

i <- iris[,1:3]
str(i)
i <- na.omit(i)
i<- scale(i)

i.d <- agnes(i, method = "complete")
i.d$ac

i.single <- agnes(i, method = "single")
i.d$ac

pltree(i.single, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
pltree(i.d, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 







zoo.ag <- agnes(zoo_unlabel, method = 'complete')
pltree(zoo.ag, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
str(zoo_unlabel)







df <- USArrests
df <- na.omit(df)

df <- scale(df)

























