#First, need to install RSpinalc package
install.packages("RSpinalc")
library(RSpinalc)

#--------read file----------#
sensor_data <- read.csv(file.choose(), header=F)

#----------giving names to column-----------#
colnames(bkm_data) <- c("Seq_Num", "MAC_ID_of_sensor", "Time_Stamp", "Ignore","Position","Quat_X","Quat_Y", "Quat_Z", "Quat_W", "Ignore")

#delete seq_num to clean duplicate data(time_stamp)
U_data <- unique(sensor_data)
U_data <- as.data.frame(U_data)

#clean NA values and where position=100 rows (has no quaternion values) from dataframe
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
cleaned_data <- delete.na(U_data)
cleaned_data <- subset(cleaned_data, cleaned_data[,3]!="100")


#----YOU DONT NEED TO RUN THE CODES BELOW. I'VE ALREADY LOAD THE LAST CLEANED DATA-----###


#Matching 2 sensors according to quaternions
deneme_data <- subset.data.frame(cleaned_data[1:10000,])

for (x in 1:nrow(deneme_data)) {
  if((deneme_data[x,2]!="C5:8B:38:1A:05:73") & (deneme_data[x,2]!=deneme_data[x+1,2])){
    value <- (deneme_data[x+1,1])
    deneme_data[x,9] <- value
    
  }else {
    deneme_data[x,9] <- "unmatched"
  }
  x = x+1
}
quaternions <- subset.data.frame(deneme_data[,5:8])
quaternions <- quaternions[-1,]
quaternions <- rbind(quaternions,x)
deneme_data <- cbind.data.frame(deneme_data,quaternions)

#------------QangularDifference function------------#
#QangularDifference returns the angular difference between 2 quaternions.
newdata <- subset(deneme_data, deneme_data[,9]!="unmatched")#Received non-unmatched rows as a new table

#Create new dataframes just for quaternions
quats1 <- newdata[,5:8] #First sensor quaternions values
quats2 <- newdata[,10:13] #Second sensor quaternions values

#Translate dataframes to matrix not to get a non-numeric argument error. 
quats1 <- as.matrix(quats1)
quats2 <- as.matrix(quats2)

#Use "QangularDifference" function
QAD <- QangularDifference(quats1, quats2)
QAD <- as.matrix(QAD)

#Use k-means clustering