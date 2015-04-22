test_df = data.frame(matrix(vector(), 2947, 6, dimnames=list(c(), c( "RawDataRowNumber", "subject", "TrainOrTest", "activity", "mean", "standarddeviation"))), stringsAsFactors=F)
## creates an empty dataframe to hold testing data specifying column names
X_test_df = read.table("X_test.txt") ## read data text file into data frame
Y_test_df = read.table("Y_test.txt") ## read activity text file into data frame
subject_test_df = read.table("subject_test.txt") ## read subject text file into data frame
test_df$subject <- subject_test_df[, 1] ##put the subject df into test_df subject activity column
test_df$TrainOrTest <- rep("test", 2947) ## put the word test in all rows of the column train or test
X_test_Means <- apply(X_test_df,1, mean, na.rm = TRUE) ## calculate the mean of each row in test_df
test_df$mean <- X_test_Means ## put mean of each row of raw data into mean column of test data frame
test_df$activity <- Y_test_df[, 1] ##put the activity df into test_df activity column
for (a in 1:nrow(test_df)){   ## this for loop replaces activity code with actual activity name
      if(test_df[a,"activity"] == 1){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "WALKING"
      }      
      if(test_df[a,"activity"] == 2){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "WALKING_UPSTAIRS"
      }
      if(test_df[a,"activity"] == 3){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "WALKING_DOWNSTAIRS"
      }
      if(test_df[a,"activity"] == 4){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "SITTING"
      }
      if(test_df[a,"activity"] == 5){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "STANDING"
      }
      if(test_df[a,"activity"] == 6){
            as.character(test_df[a,"activity"])
            test_df[a,"activity"] = "LAYING"
      }
}
X_test_StdDev <- apply(X_test_df,1, sd, na.rm = TRUE) ## compute the standard deveiation of each row of test_df
test_df$standarddeviation <- X_test_StdDev ## put standard deviation computations into standard deviation column of test_df
raw_data_row_num = seq(1, nrow(X_test_df)) ## create an array of a sequence of numbers
test_df$RawDataRowNumber <- raw_data_row_num ## put those numbers into Raw data row number column of train_df

train_df = data.frame(matrix(vector(), 7352, 6, dimnames=list(c(), c( "RawDataRowNumber", "subject", "TrainOrTest", "activity", "mean", "standarddeviation"))), stringsAsFactors=F)
## creates an empty dataframe to hold training data and names each column
X_train_df = read.table("X_train.txt") ## read data text file into data frame
Y_train_df = read.table("Y_train.txt") ## read activity text file into data frame
subject_train_df = read.table("subject_train.txt") ## read subject text file into data frame
train_df$subject <- subject_train_df[, 1] ##put the subject df into train_df subject activity column
train_df$TrainOrTest <- rep("train", 7352) ## put the word train in all rows of the column train or test
X_train_Means <- apply(X_train_df,1, mean, na.rm = TRUE)  ## create an array of mean values
train_df$mean <- X_train_Means ## put standard deviation of each row of raw data into mean column of train data frame
train_df$activity <- Y_train_df[, 1] ##put the activity df into train_df activity column
for (a in 1:nrow(train_df)){   ## this for loop replaces activity code with actual activity name
      if(train_df[a,"activity"] == 1){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "WALKING"
      }      
      if(train_df[a,"activity"] == 2){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "WALKING_UPSTAIRS"
      }
      if(train_df[a,"activity"] == 3){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "WALKING_DOWNSTAIRS"
      }
      if(train_df[a,"activity"] == 4){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "SITTING"
      }
      if(train_df[a,"activity"] == 5){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "STANDING"
      }
      if(train_df[a,"activity"] == 6){
            as.character(train_df[a,"activity"])
            train_df[a,"activity"] = "LAYING"
      }
}
X_train_StdDev <- apply(X_train_df,1, sd, na.rm = TRUE) ## compute the standard deveiation of each row of train_df
train_df$standarddeviation <- X_train_StdDev ## put standard deviation computations into standard deviation column of train_df
raw_data_row_num = seq(1, nrow(X_train_df)) ## create an array of a sequence of numbers
train_df$RawDataRowNumber <- raw_data_row_num ## put those numbers into Raw data row number column of train_df

df <- rbind(test_df, train_df) ## combine test_df and train_df

summary_df = data.frame(matrix(vector(), 12, 30, dimnames=list(c("walkingaveragemean", "walkingupstairsaveragemean", "walkingdownstairsaveragemean", "sittingaveragemean", "standingaveragemean", "layingaveragemean", "walkingaveragestddeviation", "walkingupstairsaveragestddeviation", "walkingdownstairsaveragestddeviation", "sittingaveragestddeviation", "standingaveragestddeviation", "layingaveragestddeviation"), c("subject1", "subject2", "subject3", "subject4", "subject5", "subject6", "subject7", "subject8", "subject9", "subject10", "subject11", "subject12", "subject13", "subject14", "subject15", "subject16", "subject17", "subject18", "subject19", "subject20", "subject21", "subject22", "subject23", "subject24", "subject25", "subject26", "subject27", "subject28", "subject29", "subject30"))), stringsAsFactors=F)
## creates an empty dataframe to hold summarized data naming the columns and the rows

act_var <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
## create array for the choices in df activity column
for (a in 1:ncol(summary_df)){  ## loops once for each column of the summary_df
      subset_df <- subset(df, subject == a) ## creates a subset for the subject that is being examined
      for (b in 1:6){ ## loops through the six separate activities of the subject being examined
            subsubset_df <- subset(subset_df, activity == act_var[b]) ## creates a subset of the activity being examined of the subject being examined
            mean_std_dev <- mean(subsubset_df$standarddeviation, na.rm = TRUE) ## calculates the average standard deviation of all the tests of the activity/subject being examined
            mean_mean <- mean(subsubset_df$mean, na.rm = TRUE)  ## calculates the average mean of all the tests of the activity/subject being examined 
            summary_df[(b), a] <- mean_mean ## puts the average mean in the correct cell of the data frame
            summary_df[(b+6), a] <- mean_std_dev ## puts the average standard deviation in the correct cell of the data frame
      }
}

write.table(summary_df, file = "run_analysis.txt",sep="\t", row.names=TRUE,col.names=TRUE, quote = FALSE) ## prints the summary_df to a text file residing in the working directory
