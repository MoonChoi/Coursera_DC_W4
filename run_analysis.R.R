##Set Library
requiredPackages <- c("plyr", "dplyr", "sqldf")
installReuiredPackages <- requiredPackages[!(requiredPackages %in% utils::installed.packages()[,"Package"])]
if(length(installReuiredPackages)) install.packages(installReuiredPackages, repos = "http://cran.us.r-project.org")
library("plyr")
library("dplyr")
library("sqldf")

##Data Download 
OS <-.Platform$OS.type
dataSetNm <- "initDataSet.zip"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(OS == "unix"){
  download.file(fileUrl, dataSetNm, method = "curl")
}else{
  download.file(fileUrl, dataSetNm)
}
unzip(dataSetNm)

## Data loading
##1.Merges the training and the test sets to create one data set.
##2.Extracts only the measurements on the mean and standard deviation for each measurement.
##3.Uses descriptive activity names to name the activities in the data set.
##4.Appropriately labels the data set with descriptive variable names. 
ACTIVITY_LABELS <- read.table("UCI HAR Dataset/activity_labels.txt",col.names = c("ACTV_CD","ACTV_NM"), stringsAsFactors = FALSE)
FEATURE_LIST <- read.table("UCI HAR Dataset/features.txt", col.names = c("FTR_SEQ","FTR_NM"), stringsAsFactors = FALSE)
FEATURE_TARGET = sqldf("SELECT * FROM FEATURE_LIST WHERE FTR_NM LIKE '%mean()%' or FTR_NM LIKE '%std()%' ")

FEATURE_TARGET <- FEATURE_TARGET %>% 
mutate( 
  FTR_TP_NM = gsub("-mean()", "", .$FTR_NM)
  , FTR_TP_NM = gsub("-std()", "", FTR_TP_NM)
  , MEAN_CD = regexpr(".*mean().*", .$FTR_NM)
  , STD_CD  = regexpr(".*std().*" , .$FTR_NM)
) %>% 
mutate( STAT_TP_CD =
          case_when(.$MEAN_CD == 1 ~ "Mean", .$STD_CD  == 1 ~ "Std" )
) %>%
select(1,2,3,6)

TRAIN_SET_X <- read.table("UCI HAR Dataset/train/X_train.txt")[, FEATURE_TARGET$FTR_SEQ]
names(TRAIN_SET_X) <- FEATURE_TARGET$FTR_NM
TRAIN_SET_Y <- read.table("UCI HAR Dataset/train/Y_train.txt")
TRAIN_SET_Y[,1] = ACTIVITY_LABELS[TRAIN_SET_Y[,1],2]
names(TRAIN_SET_Y) <- 'ACTV_NM'
TRAIN_SUBJECT <- read.table("UCI HAR Dataset/train/subject_train.txt")
names(TRAIN_SUBJECT) <- 'SUBJ_NM'
TRAIN_DATA_SET <- cbind(ORIGIN_SET =c("TRN"),TRAIN_SET_Y, TRAIN_SUBJECT, TRAIN_SET_X)

TEST_SET_X <- read.table("UCI HAR Dataset/test/X_test.txt")[, FEATURE_TARGET$FTR_SEQ]
names(TEST_SET_X) <- FEATURE_TARGET$FTR_NM
TEST_SET_Y <- read.table("UCI HAR Dataset/test/Y_test.txt")
TEST_SET_Y[,1] = ACTIVITY_LABELS[TEST_SET_Y[,1],2]
names(TEST_SET_Y) <- 'ACTV_NM'
TEST_SUBJECT <- read.table("UCI HAR Dataset/test/subject_test.txt")
names(TEST_SUBJECT) <- 'SUBJ_NM'
TEST_DATA_SET <- cbind(ORIGIN_SET =c("TST"),TEST_SET_Y, TEST_SUBJECT, TEST_SET_X)

DATA_SET_INIT <- rbind(TRAIN_DATA_SET, TEST_DATA_SET)

##5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
DATA_SET <- ddply(DATA_SET_INIT, .(SUBJ_NM, ACTV_NM), function(x) colMeans(x[, 4:69]))

write.table(DATA_SET_INIT, "UCI HAR Dataset/Mean_And_Std_Raw_Data.txt", row.name=FALSE)
write.table(DATA_SET, "UCI HAR Dataset/Mean_And_Std_Average_Data.txt", row.name=FALSE)
