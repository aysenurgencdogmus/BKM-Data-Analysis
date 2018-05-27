install.packages("ggplot2")
install.packages("RSpincalc")
library(ggplot2)
library(RSpincalc)


#load the newdata
#newdata has just one position (Position 3)
newdata1_1 <- read.csv(file.choose(),header = F)
colnames(newdata1_1) <- c("Seq_Num", "MAC_ID_of_sensor", "Time_Stamp", "Ignore","Position","Quat_X","Quat_Y", "Quat_Z", "Quat_W", "Ignore")
newdata1_1$Seq_Num<-NULL
newdata1_1$Ignore<-NULL
newdata1_1$Ignore<-NULL
#take just unique data
U_new_data1_1 <- unique(newdata1_1)
#clean the NA values from dataframe
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
newdata1_1 <- delete.na(U_new_data1_1)

#seperate the data into two different sensors
DB_data <- subset.data.frame(newdata1_1, newdata1_1[,1]=="DB:20:73:6D:E5:BF")
E6_data <- subset.data.frame(newdata1_1, newdata1_1[,1]=="E6:D8:91:23:C7:7F")

#change the quaternion order as "w,y,x,z" because quaternion sort shoulde be as "w,x,y,z"
colnames(DB_data) <- c("MAC_ID_of_sensor","Time_Stamp","position","quatW", "quatX", "quatY", "quatZ")
colnames(E6_data) <- c("MAC_ID_of_sensor","Time_Stamp","position","quatW", "quatX", "quatY", "quatZ")

#calculate Euler Angles
#DB sensor
Q_DB <- DB_data[,4:7]
Q_DB <- as.matrix(Q_DB)
EA_DB <- Q2EA(Q_DB, EulerOrder='zyx')
colnames(EA_DB) <- c("z","y","x")
EA_DB <- (180/pi)*EA_DB
#add sequence number to dataframe
DB_data$X <- c(1:nrow(DB_data))
EA_DB <- cbind.data.frame(EA_DB, DB_data[,2])
colnames(EA_DB) <- c("z","y","x","Time_Stamp")

#E6 sensor
Q_E6 <- E6_data[,4:7]
Q_E6 <- as.matrix(Q_E6)
EA_E6 <- Q2EA(Q_E6, EulerOrder='zyx')
colnames(EA_E6) <- c("z","y","x")
EA_E6 <- (180/pi)*EA_E6
#add sequence number to dataframe
E6_data$X <- c(1:nrow(E6_data))
EA_E6 <- cbind.data.frame(EA_E6, E6_data[,2])
colnames(EA_E6) <- c("z","y","x","Time_Stamp")

#------------------------PLOTTING-----------------------#

#----------------------DB id Sensor---------------------#
#there is just one position value. (Position 3)
#(13:03:45-15:08:04) / 2 hours 5 minutes

#all minutes of P3
options(digits.secs=5)
EA_DB$Time_Stamp <- as.POSIXct(strptime(EA_DB$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_DB) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:03-15:08] / all minutes")

#seperate the minutes
#[13:03-13:23)
ggplot(EA_DB[1:76653,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:03-13:23) / first 20 minutes")

#[13:23-13:43)
ggplot(EA_DB[76654:156840,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:23-13:43) / second 20 minutes")

#[13:43-14:03)
ggplot(EA_DB[156841:236429,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:43-14:03) / third 20 minutes")

#[14:03-14:23)
ggplot(EA_DB[236430:315143,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:03-14:23) / fourth 20 minutes")

#[14:23-14:43)
ggplot(EA_DB[315144:394093,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:23-14:43) / fifth 20 minutes")

#[14:43-15:08]
ggplot(EA_DB[394094:493946,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:43-15:08] / last 26 minutes")


#----------------------E6 id Sensor---------------------#
#there is just one position value. (Position 3)
#(13:03:45-15:08:04) / 2 hours 5 minutes

#all minutes of P3
options(digits.secs=5)
EA_E6$Time_Stamp <- as.POSIXct(strptime(EA_E6$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_E6) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:03-15:08) / all minutes")

#seperate the minutes
#[13:03-13:23)
ggplot(EA_E6[1:74863,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:03-13:23) / first 20 minutes")

#[13:23-13:43)
ggplot(EA_E6[74864:153702,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:23:13:43) / second 20 minutes")

#[13:43-14:03)
ggplot(EA_E6[153703:231739,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [13:43-14:03) / third 20 minutes")

#[14:03-14:23)
ggplot(EA_E6[231740:308112,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:03-14:23) / fourth 20 minutes")

#[14:23-14:43)
ggplot(EA_E6[308113:386138,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:23-14:43) / fifth 20 minutes")

#[14:43-15:08]
ggplot(EA_E6[386139:487726,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y"))+ 
  geom_line(aes(Time_Stamp, z, color="z")) +
  labs(title = "Between [14:43-15:08] / last 26 minutes")