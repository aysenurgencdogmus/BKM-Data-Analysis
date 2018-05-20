install.packages("ggplot2")
install.packages("RSpincalc")
library(ggplot2)
library(RSpincalc)

#load the data which has milisecond value
cleaned_data <- read.csv(file.choose(), header = TRUE)
#seperate the data into two different sensors
C5_data <- subset.data.frame(cleaned_data, cleaned_data[,2]=="C5:8B:38:1A:05:73")
E7_data <- subset.data.frame(cleaned_data, cleaned_data[,2]=="E7:36:F6:7E:02:9E")

#change the quaternion order as "w,y,x,z" because quaternion sort shoulde be as "w,x,y,z"
C5_data <- C5_data[c("X","MAC_ID_of_sensor","Time_Stamp","position","quatW", "quatX", "quatY", "quatZ")]  
E7_data <- E7_data[c("X","MAC_ID_of_sensor","Time_Stamp","position","quatW", "quatX", "quatY", "quatZ")] 

#calculate Euler Angles
#C5 sensor
Q_C5 <- C5_data[,5:8]
Q_C5 <- as.matrix(Q_C5)
EA_C5 <- Q2EA(Q_C5, EulerOrder='zyx')
colnames(EA_C5) <- c("z","y","x")
EA_C5 <- (180/pi)*EA_C5

#E7 sensor
Q_E7 <- E7_data[,5:8]
Q_E7 <- as.matrix(Q_E7)
EA_E7 <- Q2EA(Q_E7, EulerOrder='zyx')
colnames(EA_E7) <- c("z","y","x")
EA_E7 <- (180/pi)*EA_E7

#Seperate the positions an add a sequence number column
#C5  Sensor
EA_C5 <- cbind.data.frame(EA_C5,C5_data[,3:4])
EA_C5_P3 <- subset.data.frame(EA_C5,EA_C5[,5]== 3 )
EA_C5_P3$X <- c(1:nrow(EA_C5_P3))
EA_C5_P4 <- subset.data.frame(EA_C5,EA_C5[,5]== 4 )
EA_C5_P4$X <- c(1:nrow(EA_C5_P4))
EA_C5_P5 <- subset.data.frame(EA_C5,EA_C5[,5]== 5 )
EA_C5_P5$X <- c(1:nrow(EA_C5_P5))
#E7 Sensor
EA_E7 <- cbind.data.frame(EA_E7,E7_data[,3:4])
EA_E7_P3 <- subset.data.frame(EA_E7,EA_E7[,5]== 3 )
EA_E7_P3$X <- c(1:nrow(EA_E7_P3))
EA_E7_P4 <- subset.data.frame(EA_E7,EA_E7[,5]== 4 )
EA_E7_P4$X <- c(1:nrow(EA_E7_P4))
EA_E7_P5 <- subset.data.frame(EA_E7,EA_E7[,5]== 5 )
EA_E7_P5$X <- c(1:nrow(EA_E7_P5))



#------------------------PLOTTING-----------------------#

#----------------------C5 id Sensor---------------------#
#EA_C5_P3 plot (17:49:49-18:15:16 / 26 minutes)
#all minutes of P3
options(digits.secs=5)
EA_C5_P3$Time_Stamp <- as.POSIXct(strptime(EA_C5_P3$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_C5_P3) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#seperate the minutes
# [17:49-17:55)
ggplot(EA_C5_P3[1:20759,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))
# [17:55-18:00)
ggplot(EA_C5_P3[20760:40787,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))
# [18:00-18:05)
ggplot(EA_C5_P3[40788:60871,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 
# [18:05-18:10)
ggplot(EA_C5_P3[60872:80941,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 
# [18:10-18:15]
ggplot(EA_C5_P3[80942:102103,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 



#EA_C5_P4 plot (16:37:37-18:30:39 / 1 hour 53 minutes)
#all minutes of P4
options(digits.secs=5)
EA_C5_P4$Time_Stamp <- as.POSIXct(strptime(EA_C5_P4$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_C5_P4) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#seperate the minutes
#[16:37-16:57)
ggplot(EA_C5_P4[1:77782,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[16:57-17:17)
ggplot(EA_C5_P4[77783:154906,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[17:17-17:36)
ggplot(EA_C5_P4[154907:231492,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#17:36-18:15 there are no Position 4 values,

#[18:15-18:30]
ggplot(EA_C5_P4[231493:293854,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#EA_C5_P5 plot (17:36:08-18:41:05 / 1 hour 5 minutes)
#all minutes of P5
options(digits.secs=5)
EA_C5_P5$Time_Stamp <- as.POSIXct(strptime(EA_C5_P5$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_C5_P5) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#seperate into the minutes
#[17:36-17:49]
ggplot(EA_C5_P5[1:55388,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#17:49-18:31 there are no Position 5 values
#[18:31-18:41]
ggplot(EA_C5_P5[55389:94046,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))


#----------------------E7 id Sensor---------------------#
#EA_E7_P3 plot (17:49:49-18:15:16 / 26 minutes)
#all minutes of P3
options(digits.secs=5)
EA_E7_P3$Time_Stamp <- as.POSIXct(strptime(EA_E7_P3$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_E7_P3) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#seperate into the minutes
#[17:49-17:54)
ggplot(EA_E7_P3[1:16463,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[17:54-17:59)
ggplot(EA_E7_P3[16464:36704,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[17:59-18:04)
ggplot(EA_E7_P3[36705:56991,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[18:04-18:09)
ggplot(EA_E7_P3[56992:77405,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[18:09-18:15]
ggplot(EA_E7_P3[77406:102901,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))


#EA_E7_P4 plot (16:37:37-18:30:38 / 1 hour 53 minutes)
#all minutes of P4
options(digits.secs=5)
EA_E7_P4$Time_Stamp <- as.POSIXct(strptime(EA_E7_P4$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_E7_P4) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#seperate into the minutes
#[16:37-16:57)
ggplot(EA_E7_P4[1:79060,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#[16:58-17:17)
ggplot(EA_E7_P4[79061:157563,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#[17:17-17:36]
ggplot(EA_E7_P4[157564:234937,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 

#[17:17-18:14] THERE IS NO POSTITON 4 VALUE

#[18:15-18:30]
ggplot(EA_E7_P4[234938:297129,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z")) 


#EA_E7_P5 plot (17:36:08-18:41:05 / 1 hour 53 minutes)
#all minutes of P5
options(digits.secs=5)
EA_E7_P5$Time_Stamp <- as.POSIXct(strptime(EA_E7_P5$Time_Stamp, '%Y/%m/%d %H:%M:%OS',tz='GMT'))
ggplot(EA_E7_P5) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#seperate into the minutes
#[17:36-17:49]
ggplot(EA_E7_P5[1:55582,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))

#[17:50-18:30] THERE IS NO POSITION 5 VALUE

#[18:31-18:41]
ggplot(EA_E7_P5[55583:90024,]) + 
  geom_line(aes(Time_Stamp, x, color="x"))+ 
  geom_line(aes(Time_Stamp, y, color="y")) + 
  geom_line(aes(Time_Stamp, z, color="z"))



#finding patterns (not working)
delta = (sign(diff(EA_C5_P3$x)) == 1) + 0
head(delta)
tail(delta)
ds = do.call(paste0, as.list(c(delta)))
matches = gregexpr("00+", ds, perl = T)
matches
m.length = attr(matches,"match.length")
recessions = sapply(1:length(matches),function(ind) matches[ind]+0:(m.length[ind]))
