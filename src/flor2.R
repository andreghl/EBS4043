#--------- The schedule of the new system ----------------

# Set seed for reproducibility
set.seed(2026)

# Read CSV file
data <- read.csv(
  "data/ScanRecords.csv",
  stringsAsFactors = FALSE
)

# Convert duration from hours to minutes (if needed)
data$DurationMinutes <- data$Duration * 60

# Clean patient type
data$PatientType <- ifelse(data$PatientType == "Type 1", 1, 2)

# Define constants
start_time <- 8 * 60     # 08:00
end_time   <- 17 * 60    # 17:00

# Slot duration per patient type
duration_type <- c("1" = 35, "2" = 60)

# Machines
machines <- c("MRI1", "MRI2")

# Helper function: next working day
next_workday <- function(date){
  d <- as.Date(date)
  w <- weekdays(d)
  if (w == "Friday") return(d + 3)
  if (w == "Saturday") return(d + 2)
  return(d + 1)
}

# Initialize schedule: machine → date → vector of start times
schedule <- list(MRI1 = list(), MRI2 = list())

# Output columns
data$ScheduledDate <- as.Date(NA)
data$ScheduledTime <- NA
data$Machine <- NA

# Main scheduling loop
for(i in 1:nrow(data)){
  
  type     <- as.character(data$PatientType[i])
  duration <- duration_type[type]
  
  # Earliest possible day
  date <- next_workday(data$Date[i])
  
  repeat {
    
    best_start   <- Inf
    best_machine <- NA
    
    # Check each machine
    for(machine in machines){
      
      booked <- schedule[[machine]][[as.character(date)]]
      if (is.null(booked)) booked <- data.frame(start=numeric(0), end=numeric(0))
      
      # Find earliest start on this machine
      if (nrow(booked) == 0) {
        start <- start_time
      } else {
        # Sort booked by start time
        booked <- booked[order(booked$start), ]
        # Look for the first gap
        start <- start_time
        for(j in 1:nrow(booked)){
          if (start + duration <= booked$start[j]) {
            break
          } else {
            start <- booked$end[j]
          }
        }
      }
      
      # Check feasibility
      if (start + duration <= end_time) {
        if (start < best_start) {
          best_start   <- start
          best_machine <- machine
        }
      }
    }
    
    # If a feasible machine was found
    if (!is.na(best_machine)) {
      
      # Assign appointment
      booked <- schedule[[best_machine]][[as.character(date)]]
      if (is.null(booked)) booked <- data.frame(start=numeric(0), end=numeric(0))
      booked <- rbind(booked, data.frame(start=best_start, end=best_start+duration))
      schedule[[best_machine]][[as.character(date)]] <- booked
      
      data$ScheduledDate[i] <- date
      data$ScheduledTime[i] <- sprintf(
        "%02d:%02d",
        best_start %/% 60,
        best_start %% 60
      )
      data$Machine[i] <- best_machine
      
      break
    }
    
    # Otherwise go to next working day
    date <- next_workday(date)
  }
}


# Inspect result
View(data)
head(data)




#--------------- Code meausre performance ---------------



# -------------------------
# SETUP
# -------------------------
set.seed(2026)

data <- read.csv(
  "data/ScanRecords.csv",
  stringsAsFactors = FALSE
)

# Convert duration from hours to minutes
data$DurationMinutes <- data$Duration * 60

# Clean patient type
data$PatientType <- ifelse(data$PatientType == "Type 1", 1, 2)

# Constants
start_time <- 8 * 60      # 08:00
end_time   <- 17 * 60     # 17:00
workday_minutes <- end_time - start_time

# Slot duration by type
duration_type <- c("1" = 35, "2" = 60)

# Machines
machines <- c("MRI1", "MRI2")

# Next working day function
next_workday <- function(date){
  d <- as.Date(date)
  w <- weekdays(d)
  if (w == "Friday") return(d + 3)
  if (w == "Saturday") return(d + 2)
  return(d + 1)
}

# Schedule storage
schedule <- list(MRI1 = list(), MRI2 = list())

# Output columns
data$ScheduledDate <- as.Date(NA)
data$ScheduledTime <- NA
data$Machine <- NA
data$WaitingTimeMinutes <- NA
data$WaitingTimeDays <- NA

# -------------------------
# SCHEDULING (POOLED MACHINES)
# -------------------------
for(i in 1:nrow(data)){
  
  type     <- as.character(data$PatientType[i])
  duration <- duration_type[type]
  
  call_date <- as.Date(data$Date[i])
  date <- next_workday(call_date)
  
  repeat {
    
    best_start <- Inf
    best_machine <- NA
    
    for(machine in machines){
      
      booked <- schedule[[machine]][[as.character(date)]]
      if (is.null(booked)) booked <- c()
      
      start <- if (length(booked) == 0) {
        start_time
      } else {
        max(booked) + duration
      }
      
      if (start + duration <= end_time && start < best_start) {
        best_start <- start
        best_machine <- machine
      }
    }
    
    if (!is.na(best_machine)) {
      
      schedule[[best_machine]][[as.character(date)]] <-
        c(schedule[[best_machine]][[as.character(date)]], best_start)
      
      data$ScheduledDate[i] <- date
      data$ScheduledTime[i] <- sprintf(
        "%02d:%02d",
        best_start %/% 60,
        best_start %% 60
      )
      data$Machine[i] <- best_machine
      
      # Waiting times
      data$WaitingTimeMinutes[i] <-
        as.numeric(date - call_date) * workday_minutes +
        (best_start - start_time)
      
      data$WaitingTimeDays[i] <-
        as.numeric(date - call_date)
      
      break
    }
    
    date <- next_workday(date)
  }
}

# -------------------------
# DAILY PERFORMANCE METRICS (BASE R)
# -------------------------

data$SlotDuration <- duration_type[as.character(data$PatientType)]

unique_days <- sort(unique(data$ScheduledDate))
unique_machines <- unique(data$Machine)

daily_stats <- data.frame()

for (d in unique_days) {
  for (m in unique_machines) {
    
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

# -------------------------
# RESULTS
# -------------------------
View(data)
View(daily_stats)

head(data)
head(daily_stats)

