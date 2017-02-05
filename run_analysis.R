run_analysis <- function() {
        library(dplyr)
        # reading variable names
        name.t1 <- read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
        n1 <- name.t1[,2]       #pulling the names from the table
        n2 <- as.character(n1)   #turn the names to characters
        
        #reading the list of train and test observations 
        #with the names of variables from above as column names
        obs.t1 <- read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", col.names = n2)
        obs.t2 <- read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", col.names = n2)
        
        #combining the tables of observations 
        obs.t3 <- rbind(obs.t1,obs.t2)
        
        #subsetting variables regarding mean and std dev from all observations
        ss.var.n1 <- grep("[Mm][Ee][Aa][Nn]|[Ss][Tt][Dd]",n2)
        obs.t4 <- obs.t3[ss.var.n1]

        #reading the list of subject numbers from train and test data
        sub.t1 <-read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", col.names = 'subject')
        sub.t2 <-read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", col.names = 'subject')
                
        #combining the list of subject numbers 
        sub.t3 <- rbind(sub.t1,sub.t2)
        
        #read list of activity numbers from train and test data
        act.t1 <- read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt", col.names = "activity")
        act.t2 <- read.table("C:/Users/Jeff/DataCleaning/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt", col.names = "activity")
        
        #combining the list of activity numbers 
        act.t3 <- rbind(act.t1,act.t2)

        #combing all three types of data
        all.data <- cbind(sub.t3,act.t3,obs.t4)
        
        ##Summarizing by subject and activity
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
                }
        }
        #updating activity variables to text for all data
        col <- "activity"
        all.data[,col][all.data[,col] == 1] <- "walking"
        all.data[,col][all.data[,col] == 2] <- "walking upstairs"        
        all.data[,col][all.data[,col] == 3] <- "walking downstairs"        
        all.data[,col][all.data[,col] == 4] <- "sitting"        
        all.data[,col][all.data[,col] == 5] <- "standing"
        all.data[,col][all.data[,col] == 6] <- "laying"        
        
        #same as above for the summary data
        all.data2[,col][all.data2[,col] == 1] <- "walking"
        all.data2[,col][all.data2[,col] == 2] <- "walking upstairs"        
        all.data2[,col][all.data2[,col] == 3] <- "walking downstairs"        
        all.data2[,col][all.data2[,col] == 4] <- "sitting"        
        all.data2[,col][all.data2[,col] == 5] <- "standing"
        all.data2[,col][all.data2[,col] == 6] <- "laying"  
        
        #output of complete data to global environment
        tidy.data <- tbl_df(all.data)
        tidy.data <<- tidy.data
        mean.tidy.data <- tbl_df(all.data2)
        mean.tidy.data <<- mean.tidy.data
}