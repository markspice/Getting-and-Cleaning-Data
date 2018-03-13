## Create nodir and nozip logical objects and set both to FALSE

nodir <- FALSE
nozip <- FALSE

## Check whether UCI HAR Dataset directory exists in wd. If not, set nodir to TRUE. Check whether samsundata.zip
## exists in wd. If not, set nozip to TRUE and download from url. Unzip files if needed.

if(!file.exists("UCI HAR Dataset")) {
        nodir <- TRUE
        if (!file.exists("samsungdata.zip")) {
                nozip <- TRUE
                fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(fileurl,"./samsungdata.zip")
        }
        unzip("samsungdata.zip")
}

## Check whether dependent packages are installed. Install any that are missing and load.

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
        install.packages("dplyr")
}
if("tidyr" %in% rownames(installed.packages()) == FALSE) {
        install.packages("tidyr")
}
if("stringr" %in% rownames(installed.packages()) == FALSE) {
        install.packages("tidyr")
}
require(dplyr)
require(tidyr)
require(stringr)

##read subject lists, accelerometer/gyro data (mean and standard deviation columns only), activity labels and 
## column headers from both test and train directories and combine into a single dataframe

labels <- read.table("./UCI HAR Dataset/features.txt")
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("activitycode","activity"))
type <- c("test","train")
for (i in 1:2) {
        filename <- file.path(".", "UCI HAR Dataset", type[i], paste0("y_", type[i], ".txt"),fsep = .Platform$file.sep)
        activities <- read.table(filename, col.names = "activitycode")
        filename <- file.path(".", "UCI HAR Dataset", type[i], paste0("subject_", type[i], ".txt"),fsep = .Platform$file.sep)
        subjects <- read.table(filename, col.names = "subjectcode")
        filename <- file.path(".", "UCI HAR Dataset", type[i], paste0("X_", type[i], ".txt"),fsep = .Platform$file.sep)
        data <- read.table(filename, col.names = labels[,2])
        population <- c(type[i])
        assign(paste0("data_", type[i]), cbind(subjects, activities, data, population))
}
df <- rbind(data_test,data_train)
df <- merge(activitylabels, df, by.x = "activitycode", by.y = "activitycode")

## Gather all non-factor variables into a single column and separate out the elements of the column headers (instrument,
## signal, signalfilter, domain, direction, metric) 

df <- df %>% gather(metrics, value, -subjectcode, -activitycode, -activity, -population) 
df <- df %>% mutate(includerow = ifelse((grepl("mean",df$metrics) == TRUE & grepl("meanFreq",df$metrics) == FALSE) | grepl("std",df$metrics) == TRUE, TRUE, FALSE )) 
df <- filter(df, df$includerow == TRUE)
df <- df %>% mutate(dir = substr(df$metrics, nchar(df$metrics), nchar(df$metrics))) %>% mutate(direction = ifelse(dir == ".", "Magnitude", dir), domain = ifelse(substr(df$metrics,1,1) == "t", "time", "frequency"), instrument = ifelse(grepl("Gyro",df$metrics) == TRUE, "Gyroscope","Accelerometer"), signal = ifelse(grepl("Jerk",df$metrics) == TRUE, "Jerk","Raw"), signalfilter = ifelse(grepl("Body",df$metrics) == TRUE, "Body","Gravity"), metric = ifelse(grepl("mean",df$metrics) == TRUE, "mean","std"))
df <- select(df, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, metric, value)

df <- df %>% group_by(subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, metric) %>% summarise(value = mean(value))

## reconstruct the dataframe so that each observation is on one row with meaningful column headings for each variable

zzz_X_means <- filter(df, metric == "mean", direction == "X")
names(zzz_X_means)[names(zzz_X_means)=="value"] <- "mean_X"
zzz_X_stds <- filter(df, metric == "std", direction == "X")
names(zzz_X_stds)[names(zzz_X_stds)=="value"] <- "std_X"
zzz_finaldata_X <- merge(
        select(zzz_X_means, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, mean_X),                
        select(zzz_X_stds, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, std_X))
zzz_Y_means <- filter(df, metric == "mean", direction == "Y")
names(zzz_Y_means)[names(zzz_Y_means)=="value"] <- "mean_Y"
zzz_Y_stds <- filter(df, metric == "std", direction == "Y")
names(zzz_Y_stds)[names(zzz_Y_stds)=="value"] <- "std_Y"
zzz_finaldata_Y <- merge(
        select(zzz_Y_means, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, mean_Y),                
        select(zzz_Y_stds, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, std_Y))
zzz_Z_means <- filter(df, metric == "mean", direction == "Z")
names(zzz_Z_means)[names(zzz_Z_means)=="value"] <- "mean_Z"
zzz_Z_stds <- filter(df, metric == "std", direction == "Z")
names(zzz_Z_stds)[names(zzz_Z_stds)=="value"] <- "std_Z"
zzz_finaldata_Z <- merge(
        select(zzz_Z_means, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, mean_Z),                
        select(zzz_Z_stds, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, std_Z))
zzz_Mag_means <- filter(df, metric == "mean", direction == "Magnitude")
names(zzz_Mag_means)[names(zzz_Mag_means)=="value"] <- "mean_Magnitude"
zzz_Mag_stds <- filter(df, metric == "std", direction == "Magnitude")
names(zzz_Mag_stds)[names(zzz_Mag_stds)=="value"] <- "std_Magnitude"
zzz_finaldata_Mag <- merge(
        select(zzz_Mag_means, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, mean_Magnitude),                
        select(zzz_Mag_stds, subjectcode, activity, population, instrument, signal, signalfilter, domain, direction, std_Magnitude))

finaldata <- merge(zzz_finaldata_X, zzz_finaldata_Y, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_finaldata_Z, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_finaldata_Mag, all.x = TRUE, all.y = TRUE)

finaldata <- finaldata %>% group_by(subjectcode, activity, population, instrument, signal, signalfilter, domain) %>% summarise(mean_X = mean(mean_X, na.rm = TRUE), mean_Y =  mean(mean_Y, na.rm = TRUE),  mean_Z = mean(mean_Z, na.rm = TRUE),  mean_Magnitude = mean(mean_Magnitude, na.rm = TRUE), std_X = mean(std_X, na.rm = TRUE), std_Y =  mean(std_Y, na.rm = TRUE),  std_Z = mean(std_Z, na.rm = TRUE),  std_Magnitude = mean(std_Magnitude, na.rm = TRUE))

zzz_rawbodyaccelerationtime <- filter(finaldata, instrument == "Accelerometer", signal == "Raw", signalfilter == "Body", domain == "time")
names(zzz_rawbodyaccelerationtime) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "rawbodyaccelerationtime_meanmean_X", "rawbodyaccelerationtime_meanmean_Y", "rawbodyaccelerationtime_meanmean_Z", "rawbodyaccelerationtime_meanmean_Magnitude",  "rawbodyaccelerationtime_meanstd_X", "rawbodyaccelerationtime_meanstd_Y", "rawbodyaccelerationtime_meanstd_Z", "rawbodyaccelerationtime_meanstd_Magnitude")

zzz_rawgravityaccelerationtime <- filter(finaldata, instrument == "Accelerometer", signal == "Raw", signalfilter == "Gravity", domain == "time")
names(zzz_rawgravityaccelerationtime) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "rawgravityaccelerationtime_meanmean_X", "rawgravityaccelerationtime_meanmean_Y", "rawgravityaccelerationtime_meanmean_Z", "rawgravityaccelerationtime_meanmean_Magnitude",  "rawgravityaccelerationtime_meanstd_X", "rawgravityaccelerationtime_meanstd_Y", "rawgravityaccelerationtime_meanstd_Z", "rawgravityaccelerationtime_meanstd_Magnitude")

zzz_jerkbodyaccelerationtime <- filter(finaldata, instrument == "Accelerometer", signal == "Jerk", signalfilter == "Body", domain == "time")
names(zzz_jerkbodyaccelerationtime) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "jerkbodyaccelerationtime_meanmean_X", "jerkbodyaccelerationtime_meanmean_Y", "jerkbodyaccelerationtime_meanmean_Z", "jerkbodyaccelerationtime_meanmean_Magnitude",  "jerkbodyaccelerationtime_meanstd_X", "jerkbodyaccelerationtime_meanstd_Y", "jerkbodyaccelerationtime_meanstd_Z", "jerkbodyaccelerationtime_meanstd_Magnitude")

zzz_rawbodyvelocitytime <- filter(finaldata, instrument == "Gyroscope", signal == "Raw", signalfilter == "Body", domain == "time")
names(zzz_rawbodyvelocitytime) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "rawbodyvelocitytime_meanmean_X", "rawbodyvelocitytime_meanmean_Y", "rawbodyvelocitytime_meanmean_Z", "rawbodyvelocitytime_meanmean_Magnitude",  "rawbodyvelocitytime_meanstd_X", "rawbodyvelocitytime_meanstd_Y", "rawbodyvelocitytime_meanstd_Z", "rawbodyvelocitytime_meanstd_Magnitude")

zzz_jerkbodyvelocitytime <- filter(finaldata, instrument == "Gyroscope", signal == "Jerk", signalfilter == "Body", domain == "time")
names(zzz_jerkbodyvelocitytime) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "jerkbodyvelocitytime_meanmean_X", "jerkbodyvelocitytime_meanmean_Y", "jerkbodyvelocitytime_meanmean_Z", "jerkbodyvelocitytime_meanmean_Magnitude",  "jerkbodyvelocitytime_meanstd_X", "jerkbodyvelocitytime_meanstd_Y", "jerkbodyvelocitytime_meanstd_Z", "jerkbodyvelocitytime_meanstd_Magnitude")

zzz_rawbodyaccelerationfrequency <- filter(finaldata, instrument == "Accelerometer", signal == "Raw", signalfilter == "Body", domain == "frequency")
names(zzz_rawbodyaccelerationfrequency) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "rawbodyaccelerationfrequency_meanmean_X", "rawbodyaccelerationfrequency_meanmean_Y", "rawbodyaccelerationfrequency_meanmean_Z", "rawbodyaccelerationfrequency_meanmean_Magnitude",  "rawbodyaccelerationfrequency_meanstd_X", "rawbodyaccelerationfrequency_meanstd_Y", "rawbodyaccelerationfrequency_meanstd_Z", "rawbodyaccelerationfrequency_meanstd_Magnitude")

zzz_jerkbodyaccelerationfrequency <- filter(finaldata, instrument == "Accelerometer", signal == "Jerk", signalfilter == "Body", domain == "frequency")
names(zzz_jerkbodyaccelerationfrequency) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "jerkbodyaccelerationfrequency_meanmean_X", "jerkbodyaccelerationfrequency_meanmean_Y", "jerkbodyaccelerationfrequency_meanmean_Z", "jerkbodyaccelerationfrequency_meanmean_Magnitude",  "jerkbodyaccelerationfrequency_meanstd_X", "jerkbodyaccelerationfrequency_meanstd_Y", "jerkbodyaccelerationfrequency_meanstd_Z", "jerkbodyaccelerationfrequency_meanstd_Magnitude")

zzz_rawbodyvelocityfrequency <- filter(finaldata, instrument == "Gyroscope", signal == "Raw", signalfilter == "Body", domain == "frequency")
names(zzz_rawbodyvelocityfrequency) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "rawbodyvelocityfrequency_meanmean_X", "rawbodyvelocityfrequency_meanmean_Y", "rawbodyvelocityfrequency_meanmean_Z", "rawbodyvelocityfrequency_meanmean_Magnitude",  "rawbodyvelocityfrequency_meanstd_X", "rawbodyvelocityfrequency_meanstd_Y", "rawbodyvelocityfrequency_meanstd_Z", "rawbodyvelocityfrequency_meanstd_Magnitude")

zzz_jerkbodyvelocityfrequency <- finaldata %>% select(subjectcode, activity, population, instrument, signal, signalfilter, domain, mean_Magnitude, std_Magnitude) %>% filter(instrument == "Gyroscope", signal == "Jerk", signalfilter == "Body", domain == "frequency")
names(zzz_jerkbodyvelocityfrequency) <- c("subjectcode", "activity", "population", "instrument", "signal", "signalfilter", "domain", "jerkbodyvelocityfrequency_meanmean_Magnitude",  "jerkbodyvelocityfrequency_meanstd_Magnitude")

finaldata <- merge(zzz_rawbodyaccelerationtime, zzz_rawgravityaccelerationtime, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_jerkbodyaccelerationtime, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_rawbodyvelocitytime, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_jerkbodyvelocitytime, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_rawbodyaccelerationfrequency, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_jerkbodyaccelerationfrequency, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_rawbodyvelocityfrequency, all.x = TRUE, all.y = TRUE)
finaldata <- merge(finaldata, zzz_jerkbodyvelocityfrequency, all.x = TRUE, all.y = TRUE)

finaldata <- finaldata %>% select(-instrument, -signal, -signalfilter, -domain) %>% group_by(subjectcode, activity, population) %>% summarise_all(mean, na.rm = TRUE)
finaldata$subjectcode <- as.factor(finaldata$subjectcode)

## Remove any files added to the working directory during the process

if (nodir[1] == TRUE) {unlink("./UCI HAR Dataset", recursive = TRUE, force = TRUE)}
if (nozip[1] == TRUE) {unlink("./samsungdata.zip", recursive = TRUE, force = TRUE)}

##Remove all intermediary objects from the global environment created during the process

rm("activities","activitylabels","data","data_test","data_train","labels","subjects","df","filename","i","nodir","nozip","population","type")
rm(list=ls(pattern = "^zzz_"))

##output final dataframe to a text file

write.table(finaldata, file="samsung_output_summary.txt", row.name=FALSE)
