# Set data path as per your data file
setwd ("C:\\Users\\Manas Ranjan Goth\\BDVA Program\\RecommenderSystems\\Project\\ml-latest-small")


# If not installed, first install following three packages in R
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(splitstackshape)
library(gsubfn)
library(sqldf)
# Read training file along with header
inputdata<-read.csv("ratings.csv",header=TRUE)
# Just look at first few lines of this file
head(inputdata)

# Remove 'timestamp' column. We do not need it
inputdata<-inputdata[,-c(4)]

#Taking stratified sampling from the data set based on user id . Taking 80% data to train the model.
tr = stratified(inputdata, "userId", .8)
# te is the validation data set . Taking the remaining 20% of the data to test the model output.
te = sqldf("select * from inputdata except select * from tr")


#set.seed(1)
#tr = stratified(inputdata, , .8, seed = 1)
#setting seed
#set.seed(12)

# shuffling the dataset 
#inputdata <- inputdata[sample(n),]
#head(inputdata)



# Splitting the data into train and test
#tr <- inputdata[1:round(0.8*nrow(inputdata)),]
#te  <- inputdata[(round(0.8*nrow(inputdata))+1):nrow(inputdata),]
#tr <- inputdata[,-c(4)]

head(te)

# Using acast to convert above data as follows:
#       m1  m2   m3   m4
# u1    3   4    2    5
# u2    1   6    5
# u3    4   4    2    5
g<-acast(tr, userId ~ movieId)
# Check the class of g
class(g)

# Convert it as a matrix
R<-as.matrix(g)
# Convert R into realRatingMatrix data structure
# realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r



# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard or Pearson
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",nn = 5,method="Cosine", minRating = 1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Pearson",nn=5,minRating=1))


# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$method

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#      'type' parameter decides whether you want ratings or top-n items
#         we will go for ratings
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom


# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))



########## Create submission File from model #######################
# We will create 3 files by running the model 3 times using 3 different similarity Jaccard,Cosine, Pearson
# Get ratings list
rec_list<-as(recom,"list")
head(summary(rec_list))
ratings<-NULL

# For all lines in test file, one by one
for ( u in 1:length(te[,1]))
{
  # Read userid and movieid from columns 2 and 3 of test data
  userid <- te[u,1]
  movieid<-te[u,2]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1<-as.data.frame(rec_list[[userid]])
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by movie-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==movieid,1]
  # print(u)
  # print(length(x))
  # If no ratings were found, assign 0. You could also
  #   assign user-average
  if (length(x)==0)
  {
    ratings[u] <- 0
  }
  else
  {
    ratings[u] <-x
  }
  
}
length(ratings)
tx<-cbind(te[,1:3],round(ratings))
# Write to a csv file: submitfile.csv in your folder
write.table(tx,file="submitfile.csv",row.names=FALSE,col.names=FALSE,sep=',')
# Submit now this csv file to kaggle

