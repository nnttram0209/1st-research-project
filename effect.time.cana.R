file.path <- "D:/MASTER/1st Research Project/Effects/HbA1C, SBP, BMI reduction (CANVAS Program)/data.a1c.sbp (canvas).csv"
dt <- read.csv(file.path, header = TRUE)
effect.dt <- dt[,c(1,6,7)]


# Create number of possible sets for D, t1, t2, t3
# 1.HbA1C, max for D is set as 0.7 because the max.a1c.diff is only 0.63
a1c.D <- seq(0,0.7, by = 0.1)
# 2. SBP, max for D is set as 5.0 because the max.sbp.diff is only 4.91
sbp.D <- seq(0, 5.0, by = 0.1)
# 3. delta.t1, delta.t2, delta.t3, max for duration (delta) is set as 6 because total duration is 338 weeks = 6.5 yrs
delta.t1 <- seq (1,6, by = 1)
delta.t3 <- seq (1,6, by = 1)
delta.t2 <- seq (0,6, by = 1)

# Create a grid for A1C cases and SBP cases
install.packages("tidyr")
library(tidyr)
# 1. HbA1c
a1c.set <- as.data.frame(crossing(a1c.D, delta.t1, delta.t2, delta.t3))
a1c.set$t0 <- rep(0, times = nrow(a1c.set))
a1c.set$t1 <- a1c.set$delta.t1
a1c.set$t2 <- a1c.set$delta.t1 + a1c.set$delta.t2
a1c.set$t3 <- a1c.set$delta.t1 + a1c.set$delta.t2 + a1c.set$delta.t3

# 2. SBP
sbp.set <- as.data.frame(crossing(sbp.D, delta.t1, delta.t2, delta.t3))
sbp.set$t0 <- rep(0, times = nrow(sbp.set))
sbp.set$t1 <- sbp.set$delta.t1
sbp.set$t2 <- sbp.set$delta.t1 + sbp.set$delta.t2
sbp.set$t3 <- sbp.set$delta.t1 + sbp.set$delta.t2 + sbp.set$delta.t3

#Steps to calculate RMSE for each set of D, t1, t2, t3 of A1C
a1c.set$rmse <- NA
for (i in 1:nrow(a1c.set)) {
     a1c.x <- c(a1c.set$t0[i]*52, a1c.set$t1[i]*52, a1c.set$t2[i]*52, a1c.set$t3[i]*52)
     a1c.y <- c(0, -a1c.set$a1c.D[i], -a1c.set$a1c.D[i], 0)
     #The line below is to interpolate y value if x=(week) 0, 1, 2, 3...338
     a1c.itpl.dt <- approx(a1c.x, a1c.y, 0:338, ties = "ordered", rule = 2) 
     #The approx function returns a list, and we only need the interpolation of y to calculate RMSE, 
     #which is the second element in this list, so the line below is to get the interpolated y
     a1c.itpl.y <- a1c.itpl.dt[[2]]
     a1c.rmse <- sqrt((mean(effect.dt$a1c.diff - a1c.itpl.y))^2)
     a1c.set$rmse[i] <- a1c.rmse
}

#Steps to calculate RMSE for each set of D, t1, t2, t3 of SBP
sbp.set$rmse <- NA
for (j in 1:nrow(sbp.set)) {
     sbp.x <- c(sbp.set$t0[j]*52, sbp.set$t1[j]*52, sbp.set$t2[j]*52, sbp.set$t3[j]*52)
     sbp.y <- c(0, -sbp.set$sbp.D[j], -sbp.set$sbp.D[j], 0)
     #The line below is to interpolate y value if x=(week) 0, 1, 2, 3...338
     sbp.itpl.dt <- approx(sbp.x, sbp.y, 0:338, ties = "ordered", rule = 2) 
     #The approx function returns a list, and we only need the interpolation of y to calculate RMSE, 
     #which is the second element in this list, so the line below is to get the interpolated y
     sbp.itpl.y <- sbp.itpl.dt[[2]]
     sbp.rmse <- sqrt((mean(effect.dt$sbp.diff - sbp.itpl.y))^2)
     sbp.set$rmse[j] <- sbp.rmse
}

#The set of D, t1, t2, t3 that has the lowest RMSE (for HbA1c) is obtained via the following line
print("The set of D, t1, t2, t3 that has the lowest RMSE (for HbA1c) is:")
a1c.set[which(a1c.set$rmse == min(a1c.set$rmse)),1:4]
print("The set of D, t1, t2, t3 that has the lowest RMSE (for SBP) is:")
sbp.set[which(sbp.set$rmse == min(sbp.set$rmse)),1:4]

par(mfrow=c(1,2))

plot(effect.dt$week, effect.dt$a1c.diff, type ="l", col = "blue")
lines(c(0,52*3,52*3,52*8),c(0,-0.6,-0.6,0), col = "red")

plot(effect.dt$week, effect.dt$sbp.diff, type ="l", col = "blue")
lines(c(0,52*1,52*5,52*8),c(0,-4.6,-4.6,0), col = "red")
