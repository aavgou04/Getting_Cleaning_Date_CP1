library(dplyr)

#Reading ALL Data from unzipped file

activity_labels<-read.table("activity_labels.txt")
features<-read.table("features.txt")

subject_train<-read.table("./train/subject_train.txt")
x_train<-read.table("./train/X_train.txt")
y_train<-read.table("./train/Y_train.txt")


subject_test<-read.table("./test/subject_test.txt")
x_test<-read.table("./test/X_test.txt")
y_test<-read.table("./test/Y_test.txt")

#Rename Columns of data

names(activity_labels)<-c("ActivityID", "ActivityNAME")
names(features)<-c("FeatureID", "FeatureNAME")

names(subject_train)<- "SubjectID"
names(x_train)<-features[,2]
names(y_train)<-"ActivityID"

names(subject_test)<- "SubjectID"
names(x_test)<-features[,2]
names(y_test)<-"ActivityID"

#Compbining Data to final table

final_test<-cbind(subject_test, y_test, x_test)
final_train<-cbind(subject_train, y_train, x_train)
Final_Data<-rbind(final_test, final_train)

#Select variable with MEAN or STD

names_vector<-names(Final_Data)
tf<- (grepl("mean()",names_vector, fixed=TRUE))| (grepl("std", names_vector, fixed=TRUE))

#Create the new data set

temp_data<-Final_Data[,tf]
data2<-cbind(Final_Data$SubjectID, Final_Data$ActivityID, temp_data)
names(data2)[1]<-"SubjectID"
names(data2)[2]<-"ActivityID"

#Descriptive Activity Names

data3<-inner_join(activity_labels, data2, by="ActivityID")

# Extend abbrivations in variable names

names(data3)<-gsub("^t", "time", names(data3))
names(data3)<-gsub("^f", "frequency", names(data3))
names(data3)<-gsub("Acc", "Accelerometer", names(data3))
names(data3)<-gsub("Gyro", "Gyroscope", names(data3))
names(data3)<-gsub("Mag", "Magnitude", names(data3))
names(data3)<-gsub("BodyBody", "Body", names(data3))

#Step 5 and export

data4<-aggregate(. ~SubjectID + ActivityNAME, data3, mean)
data4<-arrange(data4, SubjectID, ActivityNAME) 
write.table(data4, file = "tidydata.txt",row.name=FALSE)
