######################################
#Importing the dataset
M2030 <- read.csv("C:/Users/ourio/Documents/Master DS/SMDE/Assignment 2/marathon_results_2015.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)

#Preprocessing
library(lubridate)
M2030$X5K <- lubridate::period_to_seconds(hms(M2030$X5K))
M2030$X10K <- lubridate::period_to_seconds(hms(M2030$X10K))
M2030$X15K <- lubridate::period_to_seconds(hms(M2030$X15K))
M2030$X20K <- lubridate::period_to_seconds(hms(M2030$X20))
M2030$X25K <- lubridate::period_to_seconds(hms(M2030$X25K))
M2030$X30K <- lubridate::period_to_seconds(hms(M2030$X30K))
M2030$X35K <- lubridate::period_to_seconds(hms(M2030$X35K))
M2030$X40K <- lubridate::period_to_seconds(hms(M2030$X40K))
M2030$Official.Time <- lubridate::period_to_seconds(hms(M2030$Official.Time))
M2030$Bib <- as.numeric(M2030$Bib)

#Filtering for our target: Man of 18-34 years
M2030 <- subset(M2030, M2030$M.F == "M" & M2030$Age >= 18 & M2030$Age <= 34 & M2030$Official.Time < 11100,)

#Delete some no relevant columns
M2030 <- subset(M2030, select = -c(State,Citizen,X.1,Half,Pace,Proj.Time))
summary(M2030)

#Remove the rows that have null values in any column
M2030 <- na.omit(M2030)

#Compute The parcial times of each segment
distance<-c("X5K","X10K","X15K","X20K","X25K","X30K","X35K","X40K","Official.Time")
for (k in distance) {
    if (k != "X5K") {
        colName <- paste(k,"Parcial", sep = "-")
        M2030[,colName] <- M2030[,k] - M2030[,temp]
        #print(summary(M2030[,k]))
        #print(sd(M2030[,k]))
    }
    else {
        colName <- paste(k,"Parcial", sep = "-")
        M2030[,colName] <- M2030[,k]
    }
    temp = k
}


#Pinting the summary and std of all columns in c
distance<-c("X5K-Parcial","X10K-Parcial","X15K-Parcial","X20K-Parcial","X25K-Parcial","X30K-Parcial","X35K-Parcial","X40K-Parcial","Official.Time-Parcial")
for (k in distance) {
    print(k)
    print(summary(M2030[,k]))
    print(sd(M2030[,k]))
}
