#Course Project
#1. Merges the training and the test sets to create one data set

library(dplyr)# for selection and filter functions
dataTestX<-read.table("./test/X_test.txt",quote="")
dataTestY<-read.table("./test/y_test.txt",quote="")
dataTestSub<-read.table("./test/subject_test.txt",quote="")
#print(head(dataTestX,2))
dataTrainX<-read.table("./train/X_train.txt",quote="")
dataTrainY<-read.table("./train/y_train.txt",quote="")
dataTrainSub<-read.table("./train/subject_train.txt",quote="")
#print(head(dataTrain,2))
#get the dimensions to understand the data width
mergeDataX<-rbind(dataTestX,dataTrainX)
mergeDataY<-rbind(dataTestY,dataTrainY)
mergeDataSub<-rbind(dataTestSub,dataTrainSub)

#2.Extracts only the measurements on the mean and standard deviation for each measurement
feature<-read.table("./features.txt",quote="")
listofNames <-as.character(feature[,2])
make.names(listofNames, unique=TRUE)
#4.Appropriately labels the data set with descriptive variable names
names(mergeDataX)<-c(listofNames) # OR this way >names(mergeDataX)<-feature[,2]
#select only the mean and standard deviation for this table
meanVal<-mergeDataX[,grep("mean()",names(mergeDataX),value=TRUE,fixed=TRUE)]
stdVal<- mergeDataX[,grep("std()",names(mergeDataX),value=TRUE,fixed=TRUE)]
meanStdData<-cbind(meanVal,stdVal)

#merge the data of X with Label(1. walking 2. downstairs etc.) and subject(person)
mergeDataLabelSub<-cbind(mergeDataSub,mergeDataY,meanStdData)
#change the name of Y and Z
names(mergeDataLabelSub)[1:2]<-c("subject","label")

#3. Uses descriptive activity names to name the activities in the data set
activityNames<-read.table("./activity_labels.txt")
mergeDataLabelSub[,2]=activityNames[mergeDataLabelSub[,2],2]

#5.From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity(6) and each subject(30)

#step1 rearrange/order data as per subject then as per activity
orderedData<-mergeDataLabelSub[order(mergeDataLabelSub$subject,mergeDataLabelSub$label),]

library(reshape2) #used for melt and dcast functions
#melt the data into long format
meltData<-melt(orderedData, id=c("subject", "label"))
# cast the long format data to wide format, and call mean() on all variables 
#but subject and label/activity
averageData<-dcast(data = meltData, 
                     formula = subject+label~variable, 
                     fun.aggregate = mean)
#arrange the data by both subject and label
averageData<-arrange(averageData, subject, label)


library(data.table)
#the column names of averageData need to be changed to reflect the fact that
#the data they contain are averaged values
new_colNames<-paste("ave", colnames(averageData[3:ncol(averageData)]), sep=".")
setnames(x = averageData, 
         old = colnames(averageData[3:ncol(averageData)]), 
         new = new_colNames[1:length(new_colNames)])

#Please upload your data set as a txt file created with write.table() using row.name=FALSE 
 write.table(x = averageData, 
            file = "./CourseProjectTidyData.txt", 
            row.names = FALSE)
#read the table using View() function
#View(tidyData<-read.table("./CourseProjectTidyData.txt"))

### Comments Developer####
#find if it has same dimensions(after reading back the data using read.table 
#from ./CourseProjectTidyData.txt).it shows 1 extra row as it counts the 
#column names too. #dim(averageData)= 180x68 and 
#dim(tidyData<-read.table("./CourseProjectTidyData.txt"))
