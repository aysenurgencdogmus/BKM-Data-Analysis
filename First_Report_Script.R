install.packages("RSpincalc")
library(RSpincalc)
#read data file
clean_data <- read.csv(file.choose(), header = TRUE)

#replace the MAC_ID_of_sensor value, because it has an extra space character 
clean_data$MAC_ID_of_sensor[clean_data$MAC_ID_of_sensor == " C6:1B:0C:91:9E:4F"] <- "C6:1B:0C:91:9E:4F"
clean_data$MAC_ID_of_sensor[clean_data$MAC_ID_of_sensor == " E7:36:F6:7E:02:9E"] <- "E7:36:F6:7E:02:9E"

#seperate the data into two different sensors
C6_data <- subset.data.frame(clean_data, clean_data[,2]=="C6:1B:0C:91:9E:4F")
E7_data <- subset.data.frame(clean_data, clean_data[,2]=="E7:36:F6:7E:02:9E")

#change the quaternion order as "w,y,x,z"
C6_data <- C6_data[c("X","MAC_ID_of_sensor","Time_Stamp","Position","Quat_W", "Quat_X", "Quat_Y", "Quat_Z")] #quaternion sort shoulde be as "w,x,y,z" 
E7_data <- E7_data[c("X","MAC_ID_of_sensor","Time_Stamp","Position","Quat_W", "Quat_X", "Quat_Y", "Quat_Z")] #quaternion sort shoulde be as "w,x,y,z"

#calculate Euler Angles
#C6 sensor
Q_C6 <- C6_data[,5:8]
Q_C6 <- as.matrix(Q_C6)
EA_C6 <- Q2EA(Q_C6, EulerOrder='zyx')
colnames(EA_C6) <- c("z","y","x")
EA_C6 <- (180/pi)*EA_C6
#ex1
deneme2 <- EA_C6[1:10000,]
deneme2 <- as.data.frame(deneme2)
deneme2$X <- c(1:10000)
#ex2
deneme22 <- EA_C6[10001:20000,]
deneme22 <- as.data.frame(deneme22)
deneme22$X <- c(1:10000)
#ex3
deneme222 <- EA_C6[20001:30000,]
deneme222 <- as.data.frame(deneme222)
deneme222$X <- c(1:10000)

#E7 sensor
Q_E7 <- E7_data[,5:8]
Q_E7 <- as.matrix(Q_E7)
EA_E7 <- Q2EA(Q_E7, EulerOrder='zyx')
colnames(EA_E7) <- c("z","y","x")
EA_E7 <- (180/pi)*EA_E7
EA_E7 <- as.data.frame(EA_E7)

#ex1
deneme <- EA_E7[1:10000,]
deneme <- as.data.frame(deneme)
deneme$X <- c(1:10000)
#ex2
denemee <- EA_E7[10001:20000,]
denemee <- as.data.frame(denemee)
denemee$X <- c(1:10000)
#ex3
denemeee <- EA_E7[20001:30000,]
denemeee <- as.data.frame(denemeee)
denemeee$X <- c(1:10000)

#Plotting
#C6_data 2016/04/19 17:35:10 - 2016/04/20 05:19:08 
#(11 hours 43 minutes 58 seconds= 42238s )
#data has 2331109 rows, so each row refers to 0.0181s)
ggplot(deneme2) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))
ggplot(deneme22) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))
ggplot(deneme222) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))

#E7_data 2016/04/19 17:35:16 - 2016/04/20 05:24:38
#(11 hours 49 minutes 22 seconds = 42562s)
#data has 1083764 rows, so each row refers to 0.0392s)
ggplot(deneme) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))
ggplot(denemee) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))
ggplot(denemeee) + 
  geom_line(aes(X, x, color="x")) + 
  geom_line(aes(X, y, color="y")) + 
  geom_line(aes(X, z, color="z"))