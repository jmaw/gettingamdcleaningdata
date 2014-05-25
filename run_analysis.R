#Common variables

baseDir <- "UCIHDS/"
trainDir <- "train/"
testDir <- "test/"

#Common functions

mergeDS <- function(trainFile,testFile){
  f1 <- paste(baseDir,trainDir,trainFile,sep="")
  f2 <- paste(baseDir,testDir,testFile,sep="")
  ds1 <- read.table(f1)
  ds2 <- read.table(f2)
  return (rbind(ds1,ds2))
}

getFeatures <- function(featureFile){
  f = paste(baseDir,featureFile,sep="")
  return (read.table(f))
}

featuresIndex <- function(features){
  return (grep("-mean\\(\\)|-std\\(\\)", features[, 2]))
}

filterX <- function(x,indexes){
  return(x[,indexes])
}

nameX <- function(features,idx,x){
  names(x) <- features[idx, 2]
  names(x) <- gsub("\\(|\\)", "", names(x))
  names(X) <- tolower(names(x))
  return (names(X)) 
}

getActivities <- function(actFile){
  f = paste(baseDir,actFile,sep="")
  return (read.table(f))
}

makeFinalDS <- function(S,Y,X){
  return(cbind(S, Y, X))
}

makeTidyDS <- function(S,activities,cleandata){
  uniqueSubjects = unique(S)[,1]
  totalSubjects = length(unique(S)[,1])
  totalActivities = length(activities[,1])
  totalColumns = dim(cleandata)[2]
  result = cleandata[1:(totalSubjects*totalActivities), ]
  
  row = 1
  for (s in 1:totalSubjects) {
    for (a in 1:totalActivities) {
      result[row, 1] = uniqueSubjects[s]
      result[row, 2] = activities[a, 2]
      tmp <- cleandata[cleandata$subject==s & cleandata$activity==activities[a, 2], ]
      result[row, 3:totalColumns] <- colMeans(tmp[, 3:totalColumns])
      row = row+1
    }
  }
  return (result)  
}

#1. Merges the training and the test sets to create one data set.

X <- mergeDS("X_train.txt","X_test.txt")
Y <- mergeDS("y_train.txt","y_test.txt")
S <- mergeDS("subject_train.txt","subject_test.txt") 

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

features = getFeatures("features.txt")
findexes = featuresIndex(features)
X <- filterX(X,findexes)
names(X) <- nameX(features,findexes,X)

#3. Uses descriptive activity names to name the activities in the data set

activities <- getActivities("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

#4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleanDS <- makeFinalDS(S,Y,X)

#5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

resultDS <- makeTidyDS(S,activities,cleanDS)
write.table(resultDS, "tidy_dataset.txt")
