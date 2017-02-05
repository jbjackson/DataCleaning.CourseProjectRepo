#Code Book
This code book has four sections
1. Information Regarding Original Data Source
2. The Data Overview
3. The Script Used to Modify Source Data
4. References

##Information Regarding Orignial Data Source

The source of the data contained in this repository is drawn from a public domain dataset [1].  The data contains the following data types collected from Samsung Galaxy S smartphones:
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration
- Triaxial Angular velocity from the gyroscope 
- A 561-feature vector with time and frequency domain variables 
- Its activity label 
- An identifier of the subject who carried out the experiment

The data utilized in this dataset was divided among multiple different text files. The data is further subdivided across training and test data.  Each of the subdivisions contained:
- A list of subjects
- A list of the activities they performed
- The list of measurements

Additionally general information that covered both data sets was also utilized, including:
- A list of variables in each measurement set
- A list describing the activities 

Collectively these datasets were utilized to combine the data into a singular table which was then modified to produce the summary table.


## Data Overview

This dataset contains a reduced number of variables from the original set.  The original dataset[1] contained the following:  
- 33 types of measurements 
  - tBodyAcc-XYZ
  - tGravityAcc-XYZ
  - tBodyAccJerk-XYZ
  - tBodyGyro-XYZ
  - tBodyGyroJerk-XYZ
  - tBodyAccMag
  - tGravityAccMag
  - tBodyAccJerkMag
  - tBodyGyroMag
  - tBodyGyroJerkMag
  - fBodyAcc-XYZ
  - fBodyAccJerk-XYZ
  - fBodyGyro-XYZ
  - fBodyAccMag
  - fBodyAccJerkMag
  - fBodyGyroMag
  - fBodyGyroJerkMag

(XYZ indicates 3 measurements for that variable)
(t-prefix denotes time)
(f-prefix denotes frequency)
  
- 17 calculated variables of each source measurement
  - mean(): Mean value
  - std(): Standard deviation
  - mad(): Median absolute deviation 
  - max(): Largest value in array
  - min(): Smallest value in array
  - sma(): Signal magnitude area
  - energy(): Energy measure. Sum of the squares divided by the number of values. 
  - iqr(): Interquartile range 
  - entropy(): Signal entropy
  - arCoeff(): Autorregresion coefficients with Burg order equal to 4
  - correlation(): correlation coefficient between two signals
  - maxInds(): index of the frequency component with largest magnitude
  - meanFreq(): Weighted average of the frequency components to obtain a mean frequency
  - skewness(): skewness of the frequency domain signal 
  - kurtosis(): kurtosis of the frequency domain signal 
  - bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each window.
  - angle(): Angle between to vectors.

- 5 additional calculated vectors
  - gravityMean
  - tBodyAccMean
  - tBodyAccJerkMean
  - tBodyGyroMean
  - tBodyGyroJerkMean

Of these measures the collected dataset (tidy.data) only included those measurements of mean and standard deviation.  This resulted in 86 total measurements in addition to activity and subject identifications for each observation. The measurement types appear as column names in the datasets and have not been modified.  This was done as the names of the measurements are thoroughly descriptive of the data.

The summary dataset (mean.tidy.data), further simplified the data by calculating the mean of the observations for each subject by activity.
  
## Script to Modify Source Data

The following section outlines the scripts used for collation and manipulation of accelerometer data from Samsung Galaxy S smartphones using R as the final project for the Data Cleaning course.  The entire script and final data sets are included within the repository as well.

Data for this script existed in seven different file locations.  Data was accessed with parameters to facilitate combination into a complete set.

The first step was to access the names of the measured variables

'''

        name.t1 <- read.table("*file location*")
        n1 <- name.t1[,2]       #pulling the names from the table
        n2 <- as.character(n1)   #turn the names to characters 
'''

Then the measurement data from both training and test cases was accessed and combined, utilizing the names of variables as column names.

'''

        #reading the list of train and test observations with the names of variables from above as column names
        obs.t1 <- read.table("*file location*", col.names = n2)
        obs.t2 <- read.table("*file location*", col.names = n2)
        
        #combining the tables of observations 
        obs.t3 <- rbind(obs.t1,obs.t2)
'''

Then only those columns pertaining to mean scores and standard deviation were selected.

'''
       
        #subsetting variables regarding mean or std dev from all observations
        ss.var.n1 <- grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",n2)
        obs.t4 <- obs.t3[ss.var.n1]
'''

Then the data identifying the subject and the activity they performed either in training or test cases were accessed and combined, all of which was combined with the observation data to form the complete data set.

'''

        #reading the list of subject numbers from training and test data
        sub.t1 <-read.table("*file location*", col.names = 'subject')
        sub.t2 <-read.table("*file location*", col.names = 'subject')
                
        #combining the list of subject numbers 
        sub.t3 <- rbind(sub.t1,sub.t2)
        
        #read list of activity numbers from training and test data
        act.t1 <- read.table("*file location*", col.names = "activity")
        act.t2 <- read.table("*file location*", col.names = "activity")
        
        #combining the list of activity numbers 
        act.t3 <- rbind(act.t1,act.t2)

        #combing all three types of data
        all.data <- cbind(sub.t3,act.t3,obs.t4)
'''

Finally the activities were substituted from number indicators to descriptive text.  The resulting final data was output to the global environment.

'''

        #updating activity variables to text for all data
        col <- "activity"
        all.data[,col][all.data[,col] == 1] <- "walking"
        all.data[,col][all.data[,col] == 2] <- "walking upstairs"        
        all.data[,col][all.data[,col] == 3] <- "walking downstairs"        
        all.data[,col][all.data[,col] == 4] <- "sitting"        
        all.data[,col][all.data[,col] == 5] <- "standing"
        all.data[,col][all.data[,col] == 6] <- "laying"
        
        #output of complete data to global environment
        tidy.data <- tbl_df(all.data)
        tidy.data <<- tidy.data
'''

For the summary of data, a few additional steps were taken.  A loop was created to determine mean values of the measurements for each subject, further broken-down by the type of activity.  There were a total of 30 subjects and 6 activities.

'''

        #lists of subjects and activities
        sub.l1 <- c(1:30)
        act.l1 <- c(1:6)
        
        #create a new data frame with existing column names for summary data
        all.data2 <- all.data[0, ]
        
        #filter data by subjects
        for(s in sub.l1){
                sub.data <- filter(all.data, subject == s)
                
                # filter data by activity for each subject
                for(a in act.l1){
                        sub.act.data <- filter(sub.data, activity == a)
                        
                        #determine means for each variable
                        col.num.list<- c(3:88)
                        col.avg<- colMeans(sub.act.data[,col.num.list])
                        
                        #adding data and subject/activity to the new data frame
                        col.nam <- sub.act.data[1,c(1,2)]
                        avg.sub.act.data <- c(col.nam,col.avg)
                        all.data2 <- rbind(all.data2,avg.sub.act.data)
'''

The resulting output was then modified so the activities were substituted from number indicators to descriptive text, and the resulting final data was output to the global environment, in much the same manner as the larger dataset described above.

'''

        #updating activity variables to text for all summary data
        col <- "activity"  **not actually repeated in code, just listed for reference**
        all.data2[,col][all.data2[,col] == 1] <- "walking"
        all.data2[,col][all.data2[,col] == 2] <- "walking upstairs"        
        all.data2[,col][all.data2[,col] == 3] <- "walking downstairs"        
        all.data2[,col][all.data2[,col] == 4] <- "sitting"        
        all.data2[,col][all.data2[,col] == 5] <- "standing"
        all.data2[,col][all.data2[,col] == 6] <- "laying"  
        
        #output of summary data to global environment
        mean.tidy.data <- tbl_df(all.data2)
        mean.tidy.data <<- mean.tidy.data
'''

This concluded the modification of data sets.  For further information regarding the data please see the reference section.

##Reference:
[1]Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012