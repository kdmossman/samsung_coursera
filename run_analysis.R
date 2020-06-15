## Assume we have the dplyr and tidyr packages installed; read libraries
## so functions are available
library(tidyverse)

## 1. MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET.

## Load test set
test.data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.tbl <- tibble(test.data)

## Load test subject identifier column
test.subj.id <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test.subj.tbl <- tibble(test.subj.id)
## Rename column
test.subj.tbl <- rename(test.subj.tbl, Subject = V1)

## Load test activity identifier column
test.activ.id <- read.table("./UCI HAR Dataset/test/y_test.txt")
test.activ.tbl <- tibble(test.activ.id)
## Rename column
test.activ.tbl <- rename(test.activ.tbl, Activity_Num = V1)

## Bind all test columns: data + subject & activity identifiers
test.all.tbl <- bind_cols(test.subj.tbl, test.activ.tbl, test.tbl)

## Load training set
train.data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.tbl <- tibble(train.data)

## Load training subject identifier column
train.subj.id <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train.subj.tbl <- tibble(train.subj.id)
## Rename column
train.subj.tbl <- rename(train.subj.tbl, Subject = V1)

## Load training activity identifier column
train.activ.id <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.activ.tbl <- tibble(train.activ.id)
## Rename column
train.activ.tbl <- rename(train.activ.tbl, Activity_Num = V1)

## Bind all training columns: data + subject & activity identifiers
train.all.tbl <- bind_cols(train.subj.tbl, train.activ.tbl, train.tbl)

## Bind test and training rows to get all data and identifiers in one table
data.id.all <- bind_rows(test.all.tbl, train.all.tbl)

## 2. EXTRACT ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION 
## FOR EACH MEASUREMENT. (WE'LL LEAVE OUT THE ANGLE VARIABLES FOR NOW.)
## Variable columns to select: Columns from original data
## Remember to add 2 to column number since we have bound subject and activity.
## tBodyAcc-XYZ         1:6 (3 columns for mean, 3 for st dev)
## tGravityAcc-XYZ      41:46           "       "
## tBodyAccJerk-XYZ     81:86           "       "
## tBodyGyro-XYZ        121:126         "       "
## tBodyGyroJerk-XYZ    161:166         "       "
## tBodyAccMag          201:202 (1 col for mean, 1 for st dev)
## tGravityAccMag       214:215         "       "
## tBodyAccJerkMag      227:228         "       "
## tBodyGyroMag         240:241         "       "
## tBodyGyroJerkMag     253:254         "       "
## fBodyAcc-XYZ         266:271  (3 columns for mean, 3 for st dev)
## fBodyAccJerk-XYZ     345:350         "       "
## fBodyGyro-XYZ        424:429         "       "
## fBodyAccMag          503:504 (1 col for mean, 1 for st dev)
## fBodyAccJerkMag      516:517         "       "
## fBodyGyroMag         529:530         "       "
## fBodyGyroJerkMag     542:543         "       "
data.id.select <- select(data.id.all, 1:2, 3:8, 43:48, 83:88, 123:128, 163:168,
                         203:204, 216:217, 229:230, 242:243, 255:256, 268:273,
                         347:352, 426:431, 505:506, 518:519, 531:532, 544:545)

## 3. USE DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET.
## Add column to contain descriptive activity names
## Yes, I know this is going to end up way on the right hand side!
data.id.sel.act <- mutate(data.id.select, Activity = as.character(Activity_Num))
## Define function to map numeric "Activity_Num" value to descriptive name
change.name <- function(x){
        if (x == "1"){ x <- "Walking"}
        else if (x == "2"){ x <- "Walking_Upstairs"}
        else if (x == "3"){ x <- "Walking_Downstairs"}
        else if (x == "4"){ x <- "Sitting"}
        else if (x == "5"){ x <- "Standing"}
        else if (x == "6"){ x <- "Laying"}
        else {x <- "NA"}
}
## Apply this function to the Activity column
data.id.sel.act$Activity <- lapply(data.id.sel.act$Activity, change.name)
## Note, each element in the $Activity column is now a list length 1
## So we have to "unlist" them.
## Now, replace the Activity_Num column with descriptive names
data.id.sel.act$Activity_Num <- unlist(data.id.sel.act$Activity)

## Get rid of Activity column
data.id.sel.act <- select(data.id.sel.act, -Activity)
## Rename Activity_Num to Activity
data.id.sel.act <- rename(data.id.sel.act, Activity = Activity_Num)
## Convert Activity column to factor for sorting
data.id.sel.act$Activity <- as.factor(data.id.sel.act$Activity)

## 4. APPROPRIATELY LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES.
## Remember to use column numbering from original data.
## Time domain body acceleration
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_accel_mean_x = V1, 
                          time_body_accel_mean_y = V2, 
                          time_body_accel_mean_z = V3,
                          time_body_accel_stdev_x = V4,
                          time_body_accel_stdev_y = V5,
                          time_body_accel_stdev_z = V6
                        )
## Time domain acceleration due to gravity
data.id.sel.act <- rename(data.id.sel.act, 
                          time_gravity_accel_mean_x = V41, 
                          time_gravity_accel_mean_y = V42, 
                          time_gravity_accel_mean_z = V43,
                          time_gravity_accel_stdev_x = V44,
                          time_gravity_accel_stdev_y = V45,
                          time_gravity_accel_stdev_z = V46
                        )
## Time domain body jerk
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_jerk_mean_x = V81, 
                          time_body_jerk_mean_y = V82, 
                          time_body_jerk_mean_z = V83,
                          time_body_jerk_stdev_x = V84,
                          time_body_jerk_stdev_y = V85,
                          time_body_jerk_stdev_z = V86
                        )
## Time domain angular velocity
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_ang_vel_mean_x = V121, 
                          time_body_ang_vel_mean_y = V122, 
                          time_body_ang_vel_mean_z = V123,
                          time_body_ang_vel_stdev_x = V124,
                          time_body_ang_vel_stdev_y = V125,
                          time_body_ang_vel_stdev_z = V126
)
## Time domain angular jerk
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_ang_jerk_mean_x = V161, 
                          time_body_ang_jerk_mean_y = V162, 
                          time_body_ang_jerk_mean_z = V163,
                          time_body_ang_jerk_stdev_x = V164,
                          time_body_ang_jerk_stdev_y = V165,
                          time_body_ang_jerk_stdev_z = V166
)
## Time domain body acceleration magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_accel_mag_mean = V201, 
                          time_body_accel_mag_stdev = V202 
)
## Time domain gravity acceleration magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          time_gravity_accel_mag_mean = V214, 
                          time_gravity_accel_mag_stdev = V215 
)
## Time domain body jerk magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_jerk_mag_mean = V227, 
                          time_body_jerk_mag_stdev = V228 
)
## Time domain angular velocity magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_ang_vel_mag_mean = V240, 
                          time_body_ang_vel_mag_stdev = V241 
)
## Time domain angular jerk magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          time_body_ang_jerk_mag_mean = V253, 
                          time_body_ang_jerk_mag_stdev = V254 
)
## Frequency domain body acceleration
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_accel_mean_x = V266, 
                          freq_body_accel_mean_y = V267,
                          freq_body_accel_mean_z = V268,
                          freq_body_accel_stdev_x = V269, 
                          freq_body_accel_stdev_y = V270,
                          freq_body_accel_stdev_z = V271
)
## Frequency domain body jerk
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_jerk_mean_x = V345, 
                          freq_body_jerk_mean_y = V346,
                          freq_body_jerk_mean_z = V347,
                          freq_body_jerk_stdev_x = V348, 
                          freq_body_jerk_stdev_y = V349,
                          freq_body_jerk_stdev_z = V350
)
## Frequency domain body angular velocity
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_ang_vel_mean_x = V424, 
                          freq_body_ang_vel_mean_y = V425,
                          freq_body_ang_vel_mean_z = V426,
                          freq_body_ang_vel_stdev_x = V427, 
                          freq_body_ang_vel_stdev_y = V428,
                          freq_body_ang_vel_stdev_z = V429
)
## Frequency domain body acceleration magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_accel_mag_mean = V503, 
                          freq_body_accel_mag_stdev = V504
)
## Frequency domain body jerk magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_jerk_mag_mean = V516, 
                          freq_body_jerk_mag_stdev = V517
)
## Frequency domain body angular velocity magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_ang_vel_mag_mean = V529, 
                          freq_body_ang_vel_mag_stdev = V530
)
## Frequency domain body angular jerk magnitude
data.id.sel.act <- rename(data.id.sel.act, 
                          freq_body_ang_jerk_mag_mean = V542, 
                          freq_body_ang_jerk_mag_stdev = V543
)

## 5. FROM THE DATA SET IN STEP 4, CREATE A SECOND, INDEPENDENT TIDY DATA SET 
## WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT.
data.grouped <- group_by(data.id.sel.act, Activity, Subject)
data.group.summ <- summarize_all(data.grouped, mean)
## write.csv(data.group.summ, file = "samsung_galaxy_activity.csv")
write.table(data.group.summ, file = "samsung_galaxy_activity.txt", row.names = FALSE)