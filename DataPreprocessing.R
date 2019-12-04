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
M1834$Pace <- lubridate::period_to_seconds(hms(M1834$Pace))
M1834$Official.Time <- lubridate::period_to_seconds(hms(M1834$Official.Time))
M1834$Bib <- as.numeric(M1834$Bib)

#Filtering for our target: Man of 18-34 years
M1834 <- subset(M1834, M1834$M.F == "M" & M1834$Age >= 18 & M1834$Age <= 34 & M1834$Official.Time < 11100,)

#Delete some no relevant columns
M1834 <- subset(M1834, select = -c(State,Citizen,X.1,Half,Proj.Time))
summary(M1834)

#Remove the rows that have null values in any column
M1834 <- na.omit(M1834)

#Compute The parcial times of each segment
distance<-c("X5K","X10K","X15K","X20K","X25K","X30K","X35K","X40K","Official.Time")
for (k in distance) {
    if (k != "X5K") {
        colName <- paste(k,"Parcial", sep = "")
        M1834[,colName] <- M1834[,k] - M1834[,temp]
        if (k == "Official.Time") {
            paceName <- paste(k,"Pace", sep = "")
            M1834[,paceName] <- M1834[,colName]/2
        }
        else {
            paceName <- paste(k,"Pace", sep = "")
            M1834[,paceName] <- M1834[,colName]/5
        }
        
        #print(summary(M1834[,k]))
        #print(sd(M1834[,k]))
    }
    else {
        colName <- paste(k,"Parcial", sep = "")
        M1834[,colName] <- M1834[,k]
        paceName <- paste(k,"Pace", sep = "")
        M1834[,paceName] <- M1834[,colName]/5
    }
    temp = k
}
names(M1834) <- make.names(names(M1834))


#Pinting the summary and std of all columns in c
distance<-c("X5KParcial","X10KParcial","X15KParcial","X20KParcial","X25KParcial","X30KParcial","X35KParcial","X40KParcial","Official.TimeParcial")
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

#Mitjana de pace d'elits i no elits
Elite <- subset(M1834, M1834$Official.Time <= 9000)
NoElite <- subset(M1834, M1834$Official.Time > 9000)
summary(Elite$Pace)
summary(NoElite$Pace)




model <- lm(X5KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X10KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X15KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X20KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X25KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X30KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(X35KParcial ~ Age + Pace, data = M1834)
summary(model)

model <- lm(Official.TimeParcial ~ Age + Pace, data = M1834)
summary(model)




              





