# Load external script for helper functions
setwd("EBS4043")
source("utils.r")

# Seed for reproducibility
set.seed(2026)

# Read data
data <- read.csv("data/ScanRecords.csv"); invisible(attach(data))
summary(data)

# Default variables
B <- 10000
alpha <- 0.05

# Split data by type
p1 <- data[data$PatientType == "Type 1", ]
p2 <- data[data$PatientType == "Type 2", ]

par(mfrow = c(2, 1), bg = "white")
hist(Duration[PatientType == "Type 1"],
     main = "Histogram of Duration - Patient Type 1",
     xlab = "Duration")

hist(Duration[PatientType == "Type 2"],
     main = "Histogram of Duration - Patient Type 2",
     xlab = "Duration")

par(bg = "white")
boxplot(Duration ~ factor(PatientType), data = data,
        main = "Boxplot of Duration by Patient Type",
        xlab = "Patient Type")

p1.d <- boot.mean(B, p1$Duration)
ci(p1.d)

p2.d <- boot.mean(B, p2$Duration)
ci(p2.d)

p1.dV <- boot.var(B, p1$Duration)
p2.dV <- boot.var(B, p2$Duration)

test.pois(p2$Duration)
test.gamma(p2$Duration)

p1.daily <- daily(p1$Date, tab = FALSE)
p2.daily <- daily(p2$Date, tab = FALSE)

par(mfrow = c(2, 1), bg = "white")
hist(p1.daily,
     main = "Histogram of daily patients",
     xlab = "Patients - Type 1")
hist(p2.daily,
     main = "Histogram of daily patients",
     xlab = "Patients - Type 2")

p1.day <- boot.mean(B, p1.daily)
ci(p1.day)

p2.day <- boot.mean(B, p2.daily)
ci(p2.day)

par(mfrow = c(2, 1), bg = "white")
hist(p1.day,
     main = "Histogram of Number of daily Patients - Bootstrap",
     xlab = "Patients - Type 1")
hist(p2.day,
     main = "Histogram of Number of daily Patients - Bootstrap",
     xlab = "Patients - Type 2")

test.pois(p1.daily)
test.pois(p2.daily)

p1$bwArrive <- ave(p1$Time, p1$Date, FUN = function(x) c(NA, diff(x)))
p2$bwArrive <- ave(p2$Time, p2$Date, FUN = function(x) c(NA, diff(x)))

# Histograms of interarrival times
par(mfrow = c(2, 1), bg = "white")

hist(p1$bwArrive,
     main = "Histogram of interarrival times",
     xlab = "Patients - Type 1")

hist(p2$bwArrive,
     main = "Histogram of interarrival times",
     xlab = "Patients - Type 2")

test.gamma(p1$bwArrive[!is.na(p1$bwArrive)])
test.gamma(p2$bwArrive[!is.na(p2$bwArrive)])

p1.ia <- boot.mean(B, p1$bwArrive[!is.na(p1$bwArrive)])
ci(p1.ia)

p2.ia <- boot.mean(B, p2$bwArrive[!is.na(p2$bwArrive)])
ci(p2.ia)

par(mfrow = c(2, 1), bg = "white")
hist(p1.ia,
     main = "Histogram of Interarrivals - Bootstrap",
     xlab = "Patients - Type 1")

hist(p2.ia,
     main = "Histogram of Interarrivals - Bootstrap",
     xlab = "Patients - Type 2")

p1.iaV <- boot.var(B, p1$bwArrive[!is.na(p1$bwArrive)])
p2.iaV <- boot.var(B, p2$bwArrive[!is.na(p2$bwArrive)])

par(mfrow = c(2, 1), bg = "white")
hist(p1.iaV,
     main = "Histogram of Interarrivals (Var) - Bootstrap",
     xlab = "Patients - Type 1")

hist(p2.iaV,
     main = "Histogram of Interarrivals (Var) - Bootstrap",
     xlab = "Patients - Type 2")

# Test whether Exponentially distributed
n <- nrow(p1)
rate <- 1 / mean(p1.ia)
theta <- mean(p1.ia)

# I do not trust this function
monte.carlo(p.dist = rexp(n, rate), theta = theta)

# Test whether Gamma distributed
n <- nrow(p2)
m <- mean(p2.ia)
v <- var(p2.ia) 
shape <- m ^ 2 / v
scale <- v / m
theta <- mean(p2.ia)

# I do not trust this function
monte.carlo(p.dist = rgamma(n, shape, scale = scale), theta = theta)

# Compute hourly arrivals per day
p1.hd <- hourly.day(p1)
p2.hd <- hourly.day(p2)

par(mfrow = c(2, 1), bg = "white")
hist(p1.hd$count,
    breaks = 0:ceiling(max(p1.hd$count)),
    main = "Histogram of hourly arrivals",
    xlab = "Patients - Type 1"
    )
hist(p2.hd$count,
    breaks = 0:ceiling(max(p2.hd$count)),
    main = "Histogram of hourly arrivals",
    xlab = "Patients - Type 2"
    )

test.pois(p1.hd$count)
test.pois(p2.hd$count)

p1.hdc <- boot.mean(B, p1.hd$count)
p2.hdc <- boot.mean(B, p2.hd$count)

par(mfrow = c(2, 1), bg = "white")
# tH-hose graphs look weird
hist(p1.hdc,
    breaks = 0:ceiling(max(p1.hd$count)),
    main = "Histogram of hourly arrivals - Bootstrap",
    xlab = "Patients - Type 1"
    )
hist(p2.hdc,
    breaks = 0:ceiling(max(p2.hd$count)),
    main = "Histogram of hourly arrivals - Bootstrap",
    xlab = "Patients - Type 2"
    )

n <- length(p1.hd$count)
ma1.pois <- rep(NA, n)

for(i in 1:n){
    ma1.pois[i] <- mean(p1.hd$count[1:i]) 
}

n <- length(p2.hd$count)
ma2.pois <- rep(NA, n)

for(i in 1:n){
    ma2.pois[i] <- mean(p2.hd$count[1:i]) 
}

par(mfrow = c(2, 1), bg = "white")
plot(ma1.pois,
     main = "Moving average of Type 1 patients",
     ylab = "Average")
plot(ma2.pois,
     main = "Moving average of Type 2 patients",
     ylab = "Average")

n <- length(p2$bwArrive)
m <- mean(p2.ia)
v <- mean(p2.iaV)

shape <- m ^ 2 / v
scale <- v / m
gamma.var <- rgamma(n, shape, scale = scale)

par(mfrow = c(2, 1), bg = "white")
hist(gamma.var,
     main = "Simulated Gamma Variable")
hist(p2$bwArrive[!is.na(p2$bwArrive)],
    main = "Interarrivals of Type 2 Patients",
    xlab = "Patients - Type 2")

slot.options <- seq(0, 90, by = 5) / 60
n <- length(slot.options)
slot1.bp <- rep(NA, n)

p1.bdur <- sample(x = p1$Duration, 
                  size = B * length(p1$Duration), 
                  replace = TRUE)

for(i in 1:n){
    slot1.bp[i] <- 1 - edf(p1.bdur, slot.options[i])
}

p1.slot <- data.frame(slots = slot.options * 60,
           boot.p = round(slot1.bp, 4))

slot.options <- seq(0, 90, by = 5) / 60
n <- length(slot.options)
slot2.bp <- rep(NA, n)

p2.bdur <- sample(x = p2$Duration, 
                  size = B * length(p2$Duration), 
                  replace = TRUE)

for(i in 1:n){
    slot2.bp[i] <- 1 - edf(p2.bdur, slot.options[i])
}

p2.slot <- data.frame(slots = slot.options * 60,
           boot.p = round(slot2.bp, 4))

par(mfrow = c(2, 1), bg = "white")
plot(x = slot.options * 60, y = slot1.bp,
     main = "Proportion of values above x - Type 1",
     xlab = "Slot options", ylab = "Probability")
abline(a = 0.1, b = 0, col = 'orange')
abline(a = 0.05, b = 0, col = 'red')

plot(x = slot.options * 60, y = slot2.bp,
     main = "Proportion of values above x - Type 2",
     xlab = "Slot options", ylab = "Probability")
abline(a = 0.1, b = 0, col = 'orange')
abline(a = 0.05, b = 0, col = 'red')

# Global parameters
days <- 20
start <- 8.00
end <- 17.00

# Type 1 parameters
m1 <- mean(p1.d)
v1 <- mean(p1.dV)
ia1 <- mean(p1.ia)
rate <- 1 / ia1
slot1 <- 35

# Type 2 parameters
m <- mean(p2.d)
v <- mean(p2.dV)
shape.d <- m ^ 2 / v
scale.d <- v / m

m <- mean(p2.ia)
v <- mean(p2.iaV)
shape.ia <- m ^ 2 / v
scale.ia <- v / m
slot2 <- 60


T1 <- data.frame()
T2 <- data.frame()


for(i in 1:days){

time1 <- c(start)
last <- length(time1)
while(time1[last] < end){
    last <- length(time1)
    new <- time1[last] + rexp(1, rate)
    time1 <- append(time1, round(new, 2), after = length(time1))
}

time2 <- c(start)
last <- length(time2)
while(time2[last] < end){
    last <- length(time2)
    new <- time2[last] + rgamma(1, shape.ia, scale = scale.ia)
    time2 <- append(time2, round(new, 2), after = length(time2))
}

# Remove start time and times after 17.00
time1 <- time1[start < time1 & time1 <= end]
time2 <- time2[start < time2 & time2 <= end]

n1 <- length(time1)
n2 <- length(time2)

T1 <- rbind(T1, data.frame(Date = i, Time = time1, 
                           Duration = rnorm(n1, m1, sqrt(v1)), PatientType = "Type 1"))
T2 <- rbind(T2, data.frame(Date = i, Time = time2,
                           Duration = rgamma(n2, shape.d, scale = scale.d),
                           PatientType = "Type 2"))
}

T1$ia <- ave(T1$Time, T1$Date, FUN = function(x) c(NA, diff(x)))
T2$ia <- ave(T2$Time, T2$Date, FUN = function(x) c(NA, diff(x)))

breaks <- 15

par(mfrow = c(4, 2), bg = "white")
hist(p1$bwArrive, breaks = breaks, 
main = "Interarrivals - Empirical",
xlab = "Type 1")
hist(T1$ia, breaks = breaks, 
main = "Interarrivals - Simulated",
xlab = "Type 1")
hist(p2$bwArrive, breaks = breaks, 
main = "Interarrivals - Empirical",
xlab = "Type 2")
hist(T2$ia, breaks = breaks, 
main = "Interarrivals - Simulated",
xlab = "Type 2")
hist(p1$Duration, breaks = breaks, 
main = "Duration - Empirical",
xlab = "Type 1")
hist(T1$Duration, breaks = breaks, 
main = "Duration - Simulated",
xlab = "Type 1")
hist(p2$Duration, breaks = breaks, 
main = "Duration - Empirical",
xlab = "Type 2")
hist(T2$Duration, breaks = breaks, 
main = "Duration - Simulated",
xlab = "Type 2")

sim.data <- rbind(T1, T2)
sim.data <- sim.data[order(sim.data$Date, sim.data$Time), ]
sim.data$id <- 1:nrow(sim.data)
sim.data