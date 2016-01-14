#TESTING : Remove all those who have not even viewed the course 
df.original <- rbind(harvard, mit)
df.original <- subset(df.original, viewed == 1)

rm(harvard,mit)

#Adjusting % variables for graph
df.original$grade <- df.original$grade *100
df.original$d_percentChapters <- df.original$d_percentChapters *100


#Clustering dataset
df.original.c <- subset(df.original, select= c(grade, d_percentChapters, nevents,
                               ndays_act, explored, certified))

# # # Log transformation
#  df.original.c$nevents <- log(df.original.c$nevents + 1)
#  df.original.c$ndays_act <- log(df.original.c$ndays_act + 1)
#  df.original.c$grade <-log(df.original.c$grade +1) 
#  df.original.c$d_percentChapters <- log(df.original.c$d_percentChapters+1)


# Scaling all variables to [0,1]
df.original.c$nevents <- df.original.c$nevents/max(df.original.c$nevents)
df.original.c$ndays_act <- df.original.c$ndays_act/max(df.original.c$ndays_act)
df.original.c$grade <- df.original.c$grade / 100
df.original.c$d_percentChapters <- df.original.c$d_percentChapters /100

# #Determine optimal no of clusters: pamk()
# library(fpc)
# pamk.best <- pamk(df.original.c)
# cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
# plot(pam(df.original.c, pamk.best$nc))

# # etermine optimal no of clusters: we could also do:
# library(fpc)
# asw <- numeric(20)
# for (k in 2:20)
#   asw[[k]] <- pam(d, k) $ silinfo $ avg.width
# k.best <- which.max(asw)
# cat("silhouette-optimal number of clusters:", k.best, "\n")
# # still 4


# #Silhouette analysis for determining the number of clusters
# library(fpc)
# asw <- numeric(5)
# for (k in 2:6)
#   asw[[k]] <- pam(df.original.c, k) $ silinfo $ avg.width
# k.best <- which.max(asw)
# 
# cat("silhouette-optimal number of clusters:", k.best, "\n")
# plot(pam(d, k.best))

## Determine k
# library(NbClust)
# nb <- NbClust(as.matrix(df.original.c),  min.nc=2, max.nc=6, method = "kmeans", 
#               index = "alllong")
# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

# ##Determine k
# library(cluster)
# clusGap(df.original.c, kmeans, 7, B = 100, verbose = interactive())

library(LICORS) #library for kmeans++

# 
# 
# Determining optimal no. of clusters- Elbow method
index=1:7
k_scores= rep(0,8)
for(i in 1:7)
{
   set.seed(9876)
   seeds <- kmeanspp(df.original.c, k=i)
   kObj <- kmeans(df.original.c, seeds$centers)
   #kObj <- kmeans(df.original.c, i)
   k_scores[i] <-  kObj$tot.withinss
}
plot(k_scores, pch = 19,type = "b" , xlab = "Number of clusters",
     ylab = "Total Within group SS")
rm(seeds, i, kObj)

# Running clustering
# set.seed(1234)
set.seed(9876)
# set.seed(67505)
# kObj <- kmeans(df.original.c, 3) # k = put value here yourself after seeing above plot 

## Alternate Clustering APPROACH
# Determining optimal cluster centres

cluster_centres <- kmeanspp(df.original.c, k=6)
#Running clustering with seeds
kObj <- kmeans(df.original.c, cluster_centres$centers)

#Clusters strength
strength <- kObj$betweenss / kObj$tot.withinss

#Adding cluster column to df.original.c
df.original.c <- cbind(df.original.c, kObj$cluster)
names(df.original.c)[7] <- "cluster"
df.original.c$cluster <- as.factor(df.original.c$cluster)


#Adding cluster column to df.original
df.original <- cbind(df.original, kObj$cluster)
names(df.original)[31] <- "cluster"
df.original$cluster <- as.factor(df.original$cluster)

## ------- PLOTTING on original df.original------------
library(ggplot2)

# PLOT 1: CHAPTERS VS GRADE
q <- ggplot(df.original, aes(x= d_percentChapters, y= grade))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(x = "Chapters Explored (in %)") +
  labs(y = "Grade (in %)")

# PLOT 2: Grade VS nevents
q <- ggplot(df.original, aes(x= nevents, y= grade))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(x = "No. of events") +
  labs(y = "Grade (in %)")

# PLOT 3: Chapters VS nevents
q <- ggplot(df.original, aes(x= nevents, y= d_percentChapters))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(x = "No. of events") +
  labs(y = "Chapters Explored (in %)")

# PLOT 4: Grade VS ndays_act
q <- ggplot(df.original, aes(x= ndays_act, y= grade))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(x = "No. of days active") +
  labs(y = "Grade (in %)")

# PLOT 5: Chapters VS ndays_act
q <- ggplot(df.original, aes(y= ndays_act, x= d_percentChapters))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(y = "No. of days active") +
  labs(x = "Chapters Explored (in %)")

# PLOT 6: nevents VS ndays_act
q <- ggplot(df.original, aes(y= ndays_act, x= nevents))
q + geom_point(alpha=1/3, aes(colour= cluster)) + labs(y = "No. of days active") +
  labs(x = "No. of events")
