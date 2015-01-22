run_analysis <- function() {
        #Libraries needed
        library(dplyr)
        
        #Define All Text Files
        testX     <- read.table("./test/X_test.txt")
        trainX    <- read.table("./train/X_train.txt")
        testSub   <- read.table("./test/Subject_test.txt")
        trainSub  <- read.table("./train/Subject_train.txt")
        testY     <- read.table("./test/Y_test.txt")
        trainY    <- read.table("./train/Y_train.txt")
        features  <- read.table("./features.txt", stringsAsFactors = FALSE)
        actLabels <- read.table("./activity_labels.txt", stringsAsFactors = FALSE)
        
        #Rename ColNames for Subject & Y Datasets
        colnames(testY)    <- "Activity"
        colnames(trainY)   <- "Activity"
        colnames(testSub)  <- "Subject"
        colnames(trainSub) <- "Subject"
        
        #Column Bind datasets in each folder
        testComb  <- cbind(testY , testSub , testX )
        trainComb <- cbind(trainY, trainSub, trainX)
        
        #1. Row Bind Test and Train Datasets
        FullComb <- rbind(testComb, trainComb)
        
        #4. Appropriately Label the data set with descriptive variable names
        colnames(FullComb) <- c("Activity","Subject",features$V2)
        
        #2. Subset to Mean and Std containing variables
        subsetComb1 <- FullComb[grepl("mean",colnames(FullComb))]
        subsetComb2 <- FullComb[grepl("std",colnames(FullComb))]
        subsetComb <- cbind(FullComb[,1:2], subsetComb1, subsetComb2)
        
        #3. Use Descriptive Activity Names to Name activities 
        colnames(actLabels) <- c("Activity", "ActivityLabel")
        mergedComb <- merge(subsetComb, actLabels, by = "Activity", all = TRUE)
        
        #5. Create independent tidy dataset with average of each variable for each activity and subject
        summary_mergedComb <-
                mergedComb %>%
                select(-(Activity)) %>%
                group_by(ActivityLabel, Subject) %>%
                summarise_each(funs(mean))
        
        ### Write output as a text file 
        write.table(summary_mergedComb,file = "project_output_sk.txt",row.names = FALSE)
}
