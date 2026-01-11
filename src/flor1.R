#--------- The schedule of the old system ----------------

# Set seed for reproducibility
set.seed(2026)

#Read CSV file
data <- read.csv("data/ScanRecords.csv",
                 stringsAsFactors = FALSE)

# Convert duration from hours to minutes
data$DurationMinutes <- data$Duration * 60

# Clean patient type
data$PatientType <- ifelse(data$PatientType == "Type 1", 1, 2)

#  Define constants
start_time <- 8*60      # 08:00 in minutes
end_time   <- 17*60     # 17:00 in minutes

# Fixed slot durations per type
duration_type <- c("1" = 35, "2" = 60)

# Machines per patient type
machines <- c("1" = "MRI1", "2" = "MRI2")

# Helper function: next working day
next_workday <- function(date){
  d <- as.Date(date)
  w <- weekdays(d)
  if(w == "Friday") return(d + 3)
  if(w == "Saturday") return(d + 2)
  return(d + 1)
}

# Initialize schedule per machine
schedule <- list(MRI1 = list(), MRI2 = list())

# Add columns for results
data$ScheduledDate <- as.Date(NA)
data$ScheduledTime <- NA
data$Machine <- NA

# Assign appointments
for(i in 1:nrow(data)){
  
  type <- as.character(data$PatientType[i])
  machine <- machines[type]
  duration <- duration_type[type]
  
  # Start scheduling on next working day
  date <- next_workday(data$Date[i])
  
  repeat{
    # Get booked times for this day
    booked <- schedule[[machine]][[as.character(date)]]
    if(is.null(booked)) booked <- c()
    
    # Find earliest feasible start time
    if(length(booked) == 0){
      start <- start_time
    } else {
      start <- booked[length(booked)] + duration
    }
    
    # Check if slot fits in working hours
    if(start + duration <= end_time){
      # Assign appointment
      schedule[[machine]][[as.character(date)]] <- c(booked, start)
      data$ScheduledDate[i] <- date
      
      # Convert minutes to HH:MM
      hours <- start %/% 60
      minutes <- start %% 60
      data$ScheduledTime[i] <- sprintf("%02d:%02d", hours, minutes)
      
      data$Machine[i] <- machine
      break
    } else {
      # Move to next working day if no slot available
      date <- next_workday(date)
    }
  }
}

# View scheduled data
View(data)
head(data)





#------------------- Code measure performance ------------------





# Set seed for reproducibility
set.seed(2026)

# Read CSV file
data <- read.csv(
  "/Users/flor/Downloads/Maastricht university/Master/Computational Research Skills/ScanRecords.csv",
  stringsAsFactors = FALSE
)

# Convert duration from hours to minutes
data$DurationMinutes <- data$Duration * 60

# Clean patient type
data$PatientType <- ifelse(data$PatientType == "Type 1", 1, 2)

# Define constants
start_time <- 8 * 60      # 08:00
end_time   <- 17 * 60     # 17:00
workday_minutes <- end_time - start_time

# Fixed slot durations per type
duration_type <- c("1" = 35, "2" = 60)

# Machines per patient type
machines <- c("1" = "MRI1", "2" = "MRI2")

# Helper function: next working day
next_workday <- function(date){
  d <- as.Date(date)
  w <- weekdays(d)
  if (w == "Friday") return(d + 3)
  if (w == "Saturday") return(d + 2)
  return(d + 1)
}

# Initialize schedule
schedule <- list(MRI1 = list(), MRI2 = list())

# Output columns
data$ScheduledDate <- as.Date(NA)
data$ScheduledTime <- NA
data$Machine <- NA
data$WaitingTimeMinutes <- NA
data$WaitingTimeDays <- NA

# -------------------------
# Scheduling
# -------------------------
for(i in 1:nrow(data)){
  
  type     <- as.character(data$PatientType[i])
  machine  <- machines[type]
  duration <- duration_type[type]
  
  call_date <- as.Date(data$Date[i])
  date <- next_workday(call_date)
  
  repeat {
    
    booked <- schedule[[machine]][[as.character(date)]]
    if (is.null(booked)) booked <- c()
    
    start <- ifelse(length(booked) == 0,
                    start_time,
                    tail(booked, 1) + duration)
    
    if (start + duration <= end_time) {
      
      # Assign
      schedule[[machine]][[as.character(date)]] <-
        c(booked, start)
      
      data$ScheduledDate[i] <- date
      data$ScheduledTime[i] <- sprintf(
        "%02d:%02d",
        start %/% 60,
        start %% 60
      )
      data$Machine[i] <- machine
      
      # Waiting time
      data$WaitingTimeMinutes[i] <-
        as.numeric(date - call_date) * workday_minutes +
        (start - start_time)
      
      data$WaitingTimeDays[i] <-
        as.numeric(date - call_date)
      
      break
    } else {
      date <- next_workday(date)
    }
  }
}

# -------------------------
# DAILY PERFORMANCE METRICS
# -------------------------
# Create slot duration column
data$SlotDuration <- duration_type[as.character(data$PatientType)]

# Unique combinations of day and machine
days <- unique(data$ScheduledDate)
machines_unique <- unique(data$Machine)

daily_stats <- data.frame()

for (d in days) {
  for (m in machines_unique) {
    
    subset_data <- data[data$ScheduledDate == d & data$Machine == m, ]
    
    if (nrow(subset_data) > 0) {
      
      patients <- nrow(subset_data)
      total_scan <- sum(subset_data$SlotDuration)
      idle_time <- workday_minutes - total_scan
      utilisation <- total_scan / workday_minutes
      
      daily_stats <- rbind(
        daily_stats,
        data.frame(
          ScheduledDate = d,
          Machine = m,
          PatientsScheduled = patients,
          TotalScanTime = total_scan,
          IdleTime = idle_time,
          Utilisation = utilisation
        )
      )
    }
  }
}

# View results
View(daily_stats)
head(daily_stats)

