library(reshape2)
library(data.table)
path<-"C:/Users/lenovo/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
setwd(path)
dataSubjectTrain<-fread("./train/subject_train.txt")
dataSubjectTest<-fread("./test/subject_test.txt")
dataActivityTrain <- fread("./train/Y_train.txt")
dataActivityTest  <- fread("./test/Y_test.txt" )
dataTrain <- read.table("./train/X_train.txt")
dataTest  <- read.table("./test/X_test.txt" )
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(dataSubject, "V1", "subject")
dataActivity <- rbind(dataActivityTrain, dataActivityTest)
setnames(dataActivity, "V1", "activityCode")
data <- rbind(dataTrain, dataTest)
dataSubject <- cbind(dataSubject, dataActivity)
data <- cbind(dataSubject, data)
setkey(data, subject, activityCode)

################# 2.Extracts only the measurements on the mean and standard deviation for each measurement ##########
dataFeatures <- fread("./features.txt")
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
dataFeatures <- dataFeatures[grep("mean\\(\\)|std\\(\\)", featureName)]
dataFeatures$featureCode <- paste0("V", dataFeatures$featureNum)
data <- data[, c(key(data), dataFeatures$featureCode), with=FALSE]


################# 3.Uses descriptive activity names to name the activities in the data set #######
dataActivityLabels <- fread(file.path(path, "activity_labels.txt"))
setnames(dataActivityLabels, c("activityCode", "activityName"))
data <- merge(data, dataActivityLabels, by="activityCode", all.x=TRUE)

################# 4.Appropriately labels the data set with descriptive variable names ############
setkey(data, subject, activityCode, activityName)
data <- data.table(melt(data, key(data), variable.name="featureCode"))
data <- merge(data, dataFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
data$Domain[grep("^t",data$featureName)] <- "Time"
data$Domain[grep("^f",data$featureName)] <- "Frequency"
data$Equipment[grep("Acc",data$featureName)] <- "Accelerometer"
data$Equipment[grep("Gyro",data$featureName)] <- "Gyroscope"
data$Acceleration[grep("Body",data$featureName)]<-"body"
data$Acceleration[grep("Gravity",data$featureName)]<-"Gravity"
data$Stats[grepl("mean\\(\\)",data$featureName)]<-"Mean"
data$Stats[grepl("std\\(\\)",data$featureName)]<-"Stdev"
data$Jerk[grepl("Jerk",data$featureName)]<-"Jerk"
data$Magnitude[grepl("Mag",data$featureName)]<-"Magnitude"
data$Axis[grepl("-Y",data$featureName)]<-"Y"
data$Axis[grepl("-X",data$featureName)]<-"X"
data$Axis[grepl("-Z",data$featureName)]<-"Z"

##################### 5. Create tidy data ##################
setkey(data, subject, activityName, Domain, Acceleration, Equipment, Jerk, Magnitude, Stats, Axis)
TidyData <- data[, list(count = .N, average = mean(value)), by=key(data)]

write.table(TidyData,"e:/uconn/table.txt",row.name=FALSE)
