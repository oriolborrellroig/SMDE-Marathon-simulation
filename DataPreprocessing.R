######################################
#Importing the dataset
M1834 <- read.csv("C:/Users/ourio/Documents/Master DS/SMDE/Assignment 2/marathon_results_2015.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)

#Preprocessing
library(lubridate)
M1834$X5K <- lubridate::period_to_seconds(hms(M1834$X5K))
M1834$X10K <- lubridate::period_to_seconds(hms(M1834$X10K))
M1834$X15K <- lubridate::period_to_seconds(hms(M1834$X15K))
M1834$X20K <- lubridate::period_to_seconds(hms(M1834$X20))
M1834$X25K <- lubridate::period_to_seconds(hms(M1834$X25K))
M1834$X30K <- lubridate::period_to_seconds(hms(M1834$X30K))
M1834$X35K <- lubridate::period_to_seconds(hms(M1834$X35K))
M1834$X40K <- lubridate::period_to_seconds(hms(M1834$X40K))
M1834$Official.Time <- lubridate::period_to_seconds(hms(M1834$Official.Time))
M1834$Bib <- as.numeric(M1834$Bib)

#Filtering for our target: Man of 18-34 years
M1834 <- subset(M1834, M1834$M.F == "M" & M1834$Age >= 18 & M1834$Age <= 34 & M1834$Official.Time < 11100,)

#Delete some no relevant columns
M1834 <- subset(M1834, select = -c(State,Citizen,X.1,Half,Pace,Proj.Time))
summary(M1834)

#Remove the rows that have null values in any column
M1834 <- na.omit(M1834)

#Compute The parcial times of each segment
distance<-c("X5K","X10K","X15K","X20K","X25K","X30K","X35K","X40K","Official.Time")
for (k in distance) {
    if (k != "X5K") {
        colName <- paste(k,"Parcial", sep = "-")
        M1834[,colName] <- M1834[,k] - M1834[,temp]
        #print(summary(M1834[,k]))
        #print(sd(M1834[,k]))
    }
    else {
        colName <- paste(k,"Parcial", sep = "-")
        M1834[,colName] <- M1834[,k]
    }
    temp = k
}


#Pinting the summary and std of all columns in c
distance<-c("X5K-Parcial","X10K-Parcial","X15K-Parcial","X20K-Parcial","X25K-Parcial","X30K-Parcial","X35K-Parcial","X40K-Parcial","Official.Time-Parcial")
for (k in distance) {
    print(k)
    print(summary(M1834[,k]))
    print(sd(M1834[,k]))
}

#Calculant la p
d <- sort(M1834$Official.Time)
i <- round(nrow(M1834)*0.1)
print(d[i+2])
TABLEV2 = as.data.frame((with(M1834,table(Age))))
print(TABLEV2[,"Freq"]/nrow(M1834))

#Calculant la p acumulada
TABLEV2$cumsum <- cumsum(TABLEV2[,"Freq"]/nrow(M1834))

#Printant el resultat per fer copypaste a gpss
cumsums <- t(TABLEV2$cumsum)
for (k in order(cumsums)) {
    cat(cumsums[k],k+17, sep = ",")
    cat("/")
}





