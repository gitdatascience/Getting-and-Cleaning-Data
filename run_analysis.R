library(data.table)  
library(reshape2)

##1. Merges the training and the test sets to create one dataset.
	#read the datasets in
	xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
	ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
	subtest<-read.table("./UCI HAR Dataset/test/subject_test.txt")
	xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
	ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt") 
	subtrain<-read.table("./UCI HAR Dataset/train/subject_train.txt")
 
##2. Extracts only the measurements on the mean and standard deviation
##	for each measurement. 
	#read in features columns only
	#read features dataset in
	features<-read.table("./UCI HAR Dataset/features.txt")[,2]
	feature<-grepl("mean\\(\\)|std\\(\\)",features)
	 
	#replace current xtest column names with features column names
	xtest<-xtest[,feature]
	#following features column name clean up code
	#sourced from Xiaodan's github
	#Xiaodan snippet begin
	names(xtest) <- gsub("\\(\\)","",features[feature])
	#capitalize M
	names(xtest) <- gsub("mean", "Mean", names(xtest))
	#capitalize S
	names(xtest) <- gsub("std", "Std", names(xtest))
	#remove "-" in column names 
	names(xtest) <- gsub("-", "", names(xtest))
	#Xiaodan snippet end
	
	#replace current xtrain column names with features column names
	xtrain<-xtrain[,feature]
	#Xiaodan snippet begin
	names(xtrain) <- gsub("\\(\\)","",features[feature])
	#capitalize M
	names(xtrain) <- gsub("mean", "Mean", names(xtrain))
	#capitalize S
	names(xtrain) <- gsub("std", "Std", names(xtrain))
	#remove "-" in column names 
	names(xtrain) <- gsub("-", "", names(xtrain))
	#Xiaodan snippet end
   
##3. Uses descriptive activity names to name the activities in the dataset
##4. Appropriately labels the dataset with descriptive variable names. 
	#read activity labels dataset in
	actLbl<-read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
	#add actLbl to ytest columns
	ytest[,2]<-actLbl[ytest[,1]]
	#rename ytest column names to be descriptive
	names(ytest)<-c("ActivityId","ActivityName")
	#rename subtest column name to be descriptive
	names(subtest)<-"Subject"
	
	#add actLbl to ytrain columns
	ytrain[,2]<-actLbl[ytrain[,1]]
	#rename ytrain column names to be descriptive
	names(ytrain)<-c("ActivityId","ActivityName")
	#rename subtrain column name to be descriptive
	names(subtrain)<-"Subject"
 
##5. From the dataset in step 4, creates a second
## 	, independent tidy dataset with the average of 
## 	each variable for each activity and each subject.
	#combine all test datasets
	alltest<-cbind(as.data.table(subtest),ytest,xtest)
	#combine all train datasets
 	alltrain<-cbind(as.data.table(subtrain),ytrain,xtrain)
	#combine alltest and alltrain to make one complete dataset
	complete<-rbind(alltest,alltrain)
	#create and applyvector of desired column names for complete dataset
	columns<-c("Subject","ActivityId","ActivityName")
	columns2<-setdiff(colnames(complete),columns)
	
	#stack columns
	meltcomplete<-melt(complete,id=columns,measure.vars=columns2)
	
	#create and apply mean to second indepedent tidy dataset
	tidy<-dcast(meltcomplete,Subject+ActivityName~variable,mean)

	#write second independent tidy dataset
 	write.table(tidy,file="./tidy_data.txt",row.name=FALSE)


 
 
 

 


 


 

