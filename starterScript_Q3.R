### Q3 Setup ####

PackageList=c("MASS", 
              "ISLR",
              "animation",
              "ElemStatLearn",
              "glmnet",
              "textir",
              "nnet",
              "methods",
              "statmod",
              "stats",
              "graphics",
              "RCurl",
              "jsonlite",
              "tools",
              "utils",
              "data.table",
              "gbm",
              "ggplot2",
              "randomForest",
              "tree",
              "class",
              "kknn",
              "e1071",
              "data.table",
              "R.utils",
              "recommenderlab")
NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages)
lapply(PackageList,require,character.only=TRUE)#array function


download.file('https://github.com/ChicagoBoothML/MLClassData/raw/master/Amazon/videoGames.json.gz', 'videoGames.json.gz')

fileConnection <- gzcon(file("videoGames.json.gz", "rb"))
InputData = stream_in(fileConnection)


ratingData = as(InputData[c("reviewerID", "itemID", "rating")], "realRatingMatrix")

# we keep users that have rated more than 2 video games
ratingData = ratingData[rowCounts(ratingData) > 2,]

# we will focus only on popular video games that have 
# been rated by more than 3 times
ratingData = ratingData[,colCounts(ratingData) > 3]

# we are left with this many users and items
dim(ratingData)

# example on how to recommend using Popular method
r = Recommender(ratingData, method="POPULAR")

# recommend 5 items to user it row 13
rec = predict(r, ratingData[13, ], type="topNList", n=5)
as(rec, "list")

# predict ratings 
rec = predict(r, ratingData[13, ], type="ratings")
as(rec, "matrix")

#### Q3.1 - Find User that has rated the most amount of video games ####
userIndex = which.max(rowCounts(ratingData, na.rm = TRUE)) # Find Row(Users) with Highest non-NA ratings
mostRatedUser = rownames(ratingData)[userIndex] 
mostRatedUser # Printout - TODO: REMOVE

#### Q3.2 - Find Video Game that has the most user ratings ####
videoGameIndex = which.max(colCounts(ratingData, na.rm = TRUE)) # Find Col(Games) with highest non-NA ratings
mostRatedVideoGame = colnames(ratingData)[videoGameIndex]
mostRatedVideoGame # Printout - TODO: REMOVE

#### Q3.3 - Find User that is most Similar to “U141954350” ####
userID = "U141954350"

# Transform NA's to 0s for Similarity Analysis
transformedRatingData <- as(ratingData,"matrix") # coerce to matrix so that NA can be transformed
transformedRatingData <- replace(transformedRatingData, is.na(transformedRatingData),0) # replace all NA with 0
transformedRatingData <- as(transformedRatingData, "realRatingMatrix") # coerce back to realRatingMatrix

# Gather Matrix of User & non-User ratings for Similarity Comparison
userRatings = transformedRatingData[userID]
otherUserRatings = transformedRatingData[!rownames(transformedRatingData) == userID]

# Using Pearson method for Ratings Similarities
sim <- similarity(userRatings, otherUserRatings, method = "pearson", which = "users") # Finding similarity vector of all other users
similarUser = colnames(sim)[which.max(sim)] # Finding Most Similar User
similarUser # Printout - TODO: REMOVE

#### Q3.4 - Recommend a video game to the user “U141954350 ####
userID = "U141954350"
currentUser = ratingData[userID]

# Train Data for Recommendation Model
scheme = evaluationScheme(transformedRatingData, method="cross-validation", k = 15, train = 0.8, given = 2)
trainedRatings = getData(scheme, "train")

# Predict 
recModel = Recommender(trainedRatings, method = "POPULAR")
rec = predict(recModel, currentUser, type = "topNList", n = 5)
rec = as(rec, "list")
rec # Printout - TODO: REMOVE
