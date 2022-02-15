## MIDTERM  No -1 DANA 4830
## Langara ID-100340257
##Name-Radhika Maini
##Date-14 feb 2022
## Topic -Clustering Analysis  And Clustering Distances And Types
## Clustering can be defined as group of similar observations within a dataset .When we cluster the dataset we want the observations within the dataset to be similar and observations in different groups to be dissimilar, And when there is no response variable this is an unspervised method of learning .Clustering helps in categoirising the observations into one and categorises them And in this assignment we will be using k means clustering method which is the simplest one
##Before Clustering there is a need to prepare the data as well so we need to take few things into consideration one we have to have a clean dataset and the other thing missing values should be treated and the values should be standardised meaning the eman should be zero and the standard deviation should be one
## Question 1(Grocery Store Data Analysis)
##As the promotion might be into certain targeted groups hence there was a need to divide the datasetinto several clusters and we know the main objective behind clustering is to make observations in the same group as homogenous as possible so for that we needed different methods of clustering like elbow method( to find the optimal no of clusters) , ch score method
##And as the dataset had outliers so we know that clustering is sensitive to the outliers because they shift the cluster centres ressulting in decreasing in the efficiency of the clusters.So what should we do is to check the outliers just as we will do in second question using the boxplot method of outlier detection 
# The reasonable approach is to detect the outliers, using univariate methods like boxplots, or percentiles, and 
# remove the outliers, or use multivariate approaches like the mahalanobis distance to detect outliers and remove.
## istead of k means we can use k mediods which uses the median rather than mean aswith k mean there is chance of shifting 
# Alternatively, instead of using K means which uses mean distance of all the datapoints in a cluster,
## Question 2 (Data cleaning and validation) (10 Bonus marks)
## Clean the provided dataset (do not remove any column), store it as "df".
## loading the dataset 
segmentation.data <- read.csv("C:/Users/16043/Downloads/segmentation data.csv")
View(segmentation.data)
str(segmentation.data)## Str function clearly demonstrates the internal structure of the dataset
## checking the missing values
library(visdat)
vis_dat(segmentation.data)
vis_miss(segmentation.data) 
## Deduction there are less missing values present in the dataset 
sum(is.na(segmentation.data))
## So this indicates that there are two  missing record in the dataset 
df <- read.csv("C:/Users/16043/Downloads/segmentation data.csv")
View(df)
str(df) 
dim(df)
unique(df)
##drop first value
df <- df[,-1]
# check NA values
sum(is.na(df)) 

# remove NA
df <-df[complete.cases(df),] 
sum(is.na(df))
#check if any duplicate rows
sum(duplicated(df)) 
str(df)
## no duplicated values in the dataframe 

# We checked by making the boxplot for detecting the outliers 
windows()
library(ggplot2)
hist(df$Age)
boxplot(df$Age)
# Variable 'income'
hist(df$Income)
boxplot(df$Income)

#################### clustering analysis

library(cluster)
gd1<-daisy(df, metric = "gower")
gd1
set.seed(123)

# in order to find optimal number of clusters

library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
sw<-function(km, d)
{
  s<-silhouette(km$cluster, d)
  s<-mean(s[,3])
  return(s)
}

CH<-function(km)
{
  penalty<-(length(km$cluster)-length(km$size))/(length(km$size)-1) # penalty for increasing k
  ch<-penalty*km$betweenss/sum(km$withinss) # calculate CH Score
  return (ch)
}
## inorder to find the optimal k
sw_result1<-numeric()
CH_result1<-numeric()
set.seed(123) # set seed
for (i in 2:10) # try k-means with k = 2 to 10
{
  km<-kmeans(gd1,i)
  sw_result1<-c(sw_result1, sw(km, gd1))
  CH_result1<-c(CH_result1, CH(km))
}

which.max(sw_result1)+1
which.max(CH_result1)+1
## Conclusion - using the wss and silhoutte method the optimal number of clusters came out to be 2  
# clusters for this datset is 2.Where the using sw the number of clusters came out to be 2 

##(d) Use the "gd2" to perform k-means with k = 2 with seed = 123. Store the result as "km1".
set.seed(123) # set seed 
km1<-kmeans((gd1),2)
# Cluster number for each of the observations
km1$cluster
## Cluster size
km1$size ##which the no of observations in each cluster
### Cluster means
km1$centers


####################


##Question 3 (Statistical test for clustering ) (10 marks)
## So the third question states that there was a significant difference between the two tests and in order to determine the that we needed to perform the tests so we divided the data into two samples and then performed the var test and t test but instead of doing them we could have also performed the boostrapping test instead of these tests we can perform boostrapping means because this approach is quite robust and it supports differences between the clusters  In addition, you could generate another variable and  the result of bootstrap estimates of such difference variable would be similar to  test of hypothesis.


df <- cbind(df, CLUSTER = km1$cluster)

df$CLUSTER <- as.factor(df$CLUSTER)

var.test(df$Income[df$CLUSTER == 1],
         df$Income[df$CLUSTER == 2])

t.test(df$Income[df$CLUSTER == 1],
       df$Income[df$CLUSTER == 2], paired = FALSE)

## So the given tests indicate that the p values in both the tests were less than the significant level of alpha which indicated that the test indiacted significant difference means there was a significant different between the incomes of both the clusters

##############################################################################################


##Question 4 (Limitations or Bias) (10 marks)

## Inorder to indiacte the bais in case of clustering we need to check for the confusion matrix for measuring the performance


###############################################################################

