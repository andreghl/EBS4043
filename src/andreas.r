############################################################
# Time-slot optimization for MRI scheduling (s1, s2) + SIMULATION
# Adds:
#  - calendar waiting time (to ACTUAL scan start)
#  - mean / max / p95 waiting, % waiting > thresholds
#  - intraday delays (planned vs actual start) bucketed
#  - overtime per machine-day (actual end past 17:00)
#  - simulation (nsim replications) to summarize "95% behavior"
#  - boxplots for baseline vs recommended
#
# Assumes file: ScanRecords.csv in working directory
############################################################

rm(list=ls()); cat("\014")

# -------------------------
# 0) Read + prep data
# -------------------------
# setwd("C:/Users/andre/Desktop/COMP_SKILLS/Mine") # <— adjust if needed
df <- read.csv("ScanRecords.csv", stringsAsFactors = FALSE)

df$Date <- as.Date(df$Date)
df$PatientType <- as.integer(gsub("[^0-9]", "", df$PatientType))
df$DurationMinutes <- df$Duration * 60
df$Time <- as.numeric(df$Time)

START_TIME <- 8 * 60
END_TIME   <- 17 * 60
WORKDAY_MIN <- END_TIME - START_TIME

#allowing overtime, otherwise it would be capped at 0 minutes as one of the charts in the end show
OT_ALLOW <- 15          # minutes of allowed planned overtime (0 = forbid)
DAY_END  <- END_TIME + OT_ALLOW


# Tunables
arbitrarylimit <- 0.10   # <— change this to 0.05, 0.15, etc.
OVERRUN_MAX <- arbitrarylimit

W_WAIT <- 0.80           # weight on waiting time (normalized)
W_UTIL <- 0.20           # weight on utilisation (normalized, higher is better)

NSIM <- 30              # simulation replications per configuration
SEED <- 123

# -------------------------
# 1) Helpers: time parsing, datetimes
# -------------------------
next_workday <- function(date){
  d <- as.Date(date)
  w <- weekdays(d)
  if (w == "Friday")   return(d + 3)
  if (w == "Saturday") return(d + 2)
  return(d + 1)
}

hh_to_minutes <- function(hh) {
  m <- floor(hh * 60)
  pmin(pmax(m, START_TIME), END_TIME)
}

hhmm_to_minutes <- function(x) {
  p <- strsplit(x, ":", fixed = TRUE)
  sapply(p, function(z) as.integer(z[1]) * 60 + as.integer(z[2]))
}

count_workdays_between <- function(d1, d2) {
  if (d2 <= d1) return(0)
  days <- seq(as.Date(d1) + 1, as.Date(d2), by = "day")
  sum(!weekdays(days) %in% c("Saturday", "Sunday"))
}

# Working-time waiting (08–17 weekdays only)
waiting_work_minutes <- function(call_date, call_time,
                                 sched_date, sched_start_min) {
  call_min <- hh_to_minutes(call_time)
  remaining_call_day <- END_TIME - call_min
  wdays_between <- count_workdays_between(call_date, sched_date)
  into_sched_day <- sched_start_min - START_TIME
  remaining_call_day + wdays_between * WORKDAY_MIN + into_sched_day
}

# Calendar-time waiting (elapsed minutes) to a given actual start
waiting_calendar_minutes <- function(call_date, call_time,
                                     sched_date, sched_start_min) {
  call_dt <- as.POSIXct(paste(as.Date(call_date), "00:00:00")) + as.difftime(call_time, units="hours")
  sched_dt <- as.POSIXct(paste(as.Date(sched_date), "00:00:00")) + as.difftime(sched_start_min, units="mins")
  as.numeric(difftime(sched_dt, call_dt, units = "mins"))
}

# -------------------------
# 2) Scheduling policies (PLANNED schedule only)
# -------------------------
schedule_old <- function(data, s1, s2) {
  dat <- data[order(data$Date, data$Time), ]
  slot_duration <- c("1" = as.integer(s1), "2" = as.integer(s2))
  machines <- c("1" = "MRI1", "2" = "MRI2")
  avail <- list(MRI1 = list(), MRI2 = list())

  dat$ScheduledDate <- as.Date(NA)
  dat$PlannedStartMin <- NA_integer_
  dat$ScheduledTime <- NA_character_
  dat$Machine <- NA_character_
  dat$SlotLen <- NA_integer_

  for (i in seq_len(nrow(dat))) {
    type <- as.character(dat$PatientType[i])
    m <- machines[type]
    dur <- slot_duration[type]
    d <- next_workday(dat$Date[i])

    repeat {
      key <- as.character(d)
      if (is.null(avail[[m]][[key]])) avail[[m]][[key]] <- START_TIME
      start <- avail[[m]][[key]]

      if (start + dur <= DAY_END) { # changed
        dat$ScheduledDate[i] <- d
        dat$PlannedStartMin[i] <- start
        dat$ScheduledTime[i] <- sprintf("%02d:%02d", start %/% 60, start %% 60)
        dat$Machine[i] <- m
        dat$SlotLen[i] <- dur
        avail[[m]][[key]] <- start + dur
        break
      } else {
        d <- next_workday(d)
      }
    }
  }
  dat
}

schedule_new <- function(data, s1, s2) {
  dat <- data[order(data$Date, data$Time), ]
  slot_duration <- c("1" = as.integer(s1), "2" = as.integer(s2))
  machines <- c("MRI1", "MRI2")
  booked <- list(MRI1 = list(), MRI2 = list())

  find_earliest_slot <- function(intervals, dur) {
    if (is.null(intervals) || nrow(intervals) == 0) return(START_TIME)
    if (START_TIME + dur <= intervals[1, 1]) return(START_TIME)
    if (nrow(intervals) >= 2) {
      for (k in 1:(nrow(intervals) - 1)) {
        gap_start <- intervals[k, 2]
        gap_end   <- intervals[k + 1, 1]
        if (gap_start + dur <= gap_end) return(gap_start)
      }
    }
    intervals[nrow(intervals), 2]
  }

  add_interval <- function(intervals, start, dur) {
    end <- start + dur
    if (is.null(intervals) || nrow(intervals) == 0) {
      out <- matrix(c(start, end), ncol = 2, byrow = TRUE)
      colnames(out) <- c("start","end"); return(out)
    }
    out <- rbind(intervals, c(start, end))
    out <- out[order(out[, 1]), , drop = FALSE]
    colnames(out) <- c("start","end")
    out
  }

  dat$ScheduledDate <- as.Date(NA)
  dat$PlannedStartMin <- NA_integer_
  dat$ScheduledTime <- NA_character_
  dat$Machine <- NA_character_
  dat$SlotLen <- NA_integer_

  for (i in seq_len(nrow(dat))) {
    type <- as.character(dat$PatientType[i])
    dur <- slot_duration[type]
    d <- next_workday(dat$Date[i])

    repeat {
      best_m <- NA_character_
      best_start <- Inf

      for (m in machines) {
        key <- as.character(d)
        intervals <- booked[[m]][[key]]
        start <- find_earliest_slot(intervals, dur)
        if (start + dur <= DAY_END && start < best_start) { #changed
          best_start <- start
          best_m <- m
        }
      }

      if (!is.na(best_m)) {
        key <- as.character(d)
        booked[[best_m]][[key]] <- add_interval(booked[[best_m]][[key]], best_start, dur)

        dat$ScheduledDate[i] <- d
        dat$PlannedStartMin[i] <- best_start
        dat$ScheduledTime[i] <- sprintf("%02d:%02d", best_start %/% 60, best_start %% 60)
        dat$Machine[i] <- best_m
        dat$SlotLen[i] <- dur
        break
      } else {
        d <- next_workday(d)
      }
    }
  }
  dat
}

# -------------------------
# 3) Planned KPIs (idle/util)
# -------------------------
daily_kpis_planned <- function(scheduled_df) {
  agg <- aggregate(
    SlotLen ~ ScheduledDate + Machine,
    data = scheduled_df,
    FUN = function(x) c(TotalSlotTime = sum(x), Patients = length(x))
  )
  out <- data.frame(
    ScheduledDate = as.Date(agg$ScheduledDate),
    Machine = agg$Machine,
    TotalSlotTime = agg$SlotLen[, "TotalSlotTime"],
    Patients = agg$SlotLen[, "Patients"]
  )
  out$Utilisation <- out$TotalSlotTime / WORKDAY_MIN
  out$IdleTime <- WORKDAY_MIN - out$TotalSlotTime
  out
}

overrun_rate <- function(type_durations_minutes, slot_len) mean(type_durations_minutes > slot_len)

# -------------------------
# 4) Simulation: realized execution -> intraday delays + overtime
# -------------------------
simulate_execution <- function(planned_df, d1, d2) {
  dat <- planned_df
  dat$ActualDuration <- NA_real_
  dat$ActualStartMin <- NA_real_
  dat$ActualEndMin <- NA_real_
  dat$IntradayDelayMin <- NA_real_

  draw_dur <- function(pt) if (pt == 1) sample(d1, 1, TRUE) else sample(d2, 1, TRUE)

  keys <- unique(paste(dat$ScheduledDate, dat$Machine))
  overtime_by_key <- numeric(length(keys)); names(overtime_by_key) <- keys

  for (k in seq_along(keys)) {
    parts <- strsplit(keys[k], " ", fixed = TRUE)[[1]]
    d <- as.Date(parts[1]); m <- parts[2]

    idx <- which(dat$ScheduledDate == d & dat$Machine == m)
    idx <- idx[order(dat$PlannedStartMin[idx])]

    prev_end <- START_TIME
    for (j in idx) {
      act_dur <- draw_dur(dat$PatientType[j])
      act_start <- max(dat$PlannedStartMin[j], prev_end)
      act_end <- act_start + act_dur

      dat$ActualDuration[j] <- act_dur
      dat$ActualStartMin[j] <- act_start
      dat$ActualEndMin[j] <- act_end
      dat$IntradayDelayMin[j] <- act_start - dat$PlannedStartMin[j]

      prev_end <- act_end
    }
    overtime_by_key[k] <- max(0, max(dat$ActualEndMin[idx]) - END_TIME)
  }

  ot <- data.frame(
    key = keys,
    ScheduledDate = as.Date(sapply(strsplit(keys, " ", fixed=TRUE), `[`, 1)),
    Machine = sapply(strsplit(keys, " ", fixed=TRUE), `[`, 2),
    OvertimeMin = as.numeric(overtime_by_key)
  )
  list(exec = dat, overtime = ot)
}

delay_bucket_props <- function(delay_minutes) {
  d <- pmax(0, delay_minutes)
  c(
    lt30 = mean(d < 30),
    lt60 = mean(d >= 30 & d < 60),
    lt120 = mean(d >= 60 & d < 120),
    ge120 = mean(d >= 120)
  )
}

# -------------------------
# 5) Candidate generation around "90% rule"
# -------------------------
d1 <- df$DurationMinutes[df$PatientType == 1]
d2 <- df$DurationMinutes[df$PatientType == 2]

q_grid <- seq(0.85, 0.95, by = 0.01) #unchanged
cand <- expand.grid(q1 = q_grid, q2 = q_grid)
cand$s1 <- as.integer(ceiling(quantile(d1, probs = cand$q1, type = 7, names = FALSE, na.rm = TRUE)))
cand$s2 <- as.integer(ceiling(quantile(d2, probs = cand$q2, type = 7, names = FALSE, na.rm = TRUE)))
cand <- cand[!duplicated(cand[, c("s1", "s2")]), ]
row.names(cand) <- NULL

BASE_S1 <- 35
BASE_S2 <- 60

# -------------------------
# 6) Evaluate each candidate (with simulation)
# -------------------------
set.seed(SEED)

results <- data.frame(
  s1 = integer(0), s2 = integer(0),
  q1 = numeric(0), q2 = numeric(0),
  over1 = numeric(0), over2 = numeric(0),
  policy = character(0),

  mean_wait_cal_days = numeric(0),
  p95_wait_cal_days = numeric(0),
  max_wait_cal_days = numeric(0),
  pct_wait_gt7d = numeric(0),
  pct_wait_gt14d = numeric(0),

  mean_util = numeric(0),
  mean_idle = numeric(0),

  mean_overtime_min = numeric(0),
  p95_overtime_min = numeric(0),

  delay_lt30 = numeric(0),
  delay_lt60 = numeric(0),
  delay_lt120 = numeric(0),
  delay_ge120 = numeric(0),
  stringsAsFactors = FALSE
)

summarize_config <- function(planned_fun, data, s1, s2, nsim) {
  planned <- planned_fun(data, s1, s2)
  planned_daily <- daily_kpis_planned(planned)

  waits_mean <- numeric(nsim)
  waits_p95  <- numeric(nsim)
  waits_max  <- numeric(nsim)
  pct_gt7    <- numeric(nsim)
  pct_gt14   <- numeric(nsim)

  ot_mean <- numeric(nsim)
  ot_p95  <- numeric(nsim)

  b_lt30 <- numeric(nsim); b_lt60 <- numeric(nsim); b_lt120 <- numeric(nsim); b_ge120 <- numeric(nsim)

  for (b in 1:nsim) {
    sim <- simulate_execution(planned, d1, d2)
    exec <- sim$exec
    ot   <- sim$overtime

    wait_cal_days <- mapply(
      waiting_calendar_minutes,
      call_date = exec$Date,
      call_time = exec$Time,
      sched_date = exec$ScheduledDate,
      sched_start_min = exec$ActualStartMin
    ) / (60 * 24)

    waits_mean[b] <- mean(wait_cal_days)
    waits_p95[b]  <- as.numeric(quantile(wait_cal_days, probs = 0.95, names = FALSE, type = 7))
    waits_max[b]  <- max(wait_cal_days)

    pct_gt7[b]  <- mean(wait_cal_days > 7)
    pct_gt14[b] <- mean(wait_cal_days > 14)

    ot_mean[b] <- mean(ot$OvertimeMin)
    ot_p95[b]  <- as.numeric(quantile(ot$OvertimeMin, probs = 0.95, names = FALSE, type = 7))

    props <- delay_bucket_props(exec$IntradayDelayMin)
    b_lt30[b]  <- props["lt30"]
    b_lt60[b]  <- props["lt60"]
    b_lt120[b] <- props["lt120"]
    b_ge120[b] <- props["ge120"]
  }

  list(
    planned_daily = planned_daily,
    mean_wait_cal_days = mean(waits_mean),
    p95_wait_cal_days  = mean(waits_p95),
    max_wait_cal_days  = mean(waits_max),
    pct_wait_gt7d      = mean(pct_gt7),
    pct_wait_gt14d     = mean(pct_gt14),
    mean_overtime_min  = mean(ot_mean),
    p95_overtime_min   = mean(ot_p95),
    delay_lt30 = mean(b_lt30),
    delay_lt60 = mean(b_lt60),
    delay_lt120 = mean(b_lt120),
    delay_ge120 = mean(b_ge120)
  )
}
### fix start
rows <- vector("list", length = 2*nrow(cand))
k <- 0

for (r in seq_len(nrow(cand))) {
  s1 <- cand$s1[r]; s2 <- cand$s2[r]
  over1 <- overrun_rate(d1, s1); over2 <- overrun_rate(d2, s2)
  
  if (over1 > OVERRUN_MAX || over2 > OVERRUN_MAX) next
  
  old_sum <- summarize_config(schedule_old, df, s1, s2, NSIM)
  old_daily <- old_sum$planned_daily
  
  k <- k + 1
  rows[[k]] <- data.frame(
    s1=s1, s2=s2, q1=cand$q1[r], q2=cand$q2[r],
    over1=over1, over2=over2, policy="old",
    mean_wait_cal_days=old_sum$mean_wait_cal_days,
    p95_wait_cal_days=old_sum$p95_wait_cal_days,
    max_wait_cal_days=old_sum$max_wait_cal_days,
    pct_wait_gt7d=old_sum$pct_wait_gt7d,
    pct_wait_gt14d=old_sum$pct_wait_gt14d,
    mean_util=mean(old_daily$Utilisation),
    mean_idle=mean(old_daily$IdleTime),
    mean_overtime_min=old_sum$mean_overtime_min,
    p95_overtime_min=old_sum$p95_overtime_min,
    delay_lt30=old_sum$delay_lt30,
    delay_lt60=old_sum$delay_lt60,
    delay_lt120=old_sum$delay_lt120,
    delay_ge120=old_sum$delay_ge120
  )
  
  new_sum <- summarize_config(schedule_new, df, s1, s2, NSIM)
  new_daily <- new_sum$planned_daily
  
  k <- k + 1
  rows[[k]] <- data.frame(
    s1=s1, s2=s2, q1=cand$q1[r], q2=cand$q2[r],
    over1=over1, over2=over2, policy="new",
    mean_wait_cal_days=new_sum$mean_wait_cal_days,
    p95_wait_cal_days=new_sum$p95_wait_cal_days,
    max_wait_cal_days=new_sum$max_wait_cal_days,
    pct_wait_gt7d=new_sum$pct_wait_gt7d,
    pct_wait_gt14d=new_sum$pct_wait_gt14d,
    mean_util=mean(new_daily$Utilisation),
    mean_idle=mean(new_daily$IdleTime),
    mean_overtime_min=new_sum$mean_overtime_min,
    p95_overtime_min=new_sum$p95_overtime_min,
    delay_lt30=new_sum$delay_lt30,
    delay_lt60=new_sum$delay_lt60,
    delay_lt120=new_sum$delay_lt120,
    delay_ge120=new_sum$delay_ge120
  )
}

results <- do.call(rbind, rows[seq_len(k)])

results$feasible <- (results$over1 <= OVERRUN_MAX) & (results$over2 <= OVERRUN_MAX)

# -------------------------
# 7) Scoring and recommended (NEW)
# -------------------------
normalize01 <- function(x) {
  if (length(unique(x)) == 1) return(rep(0, length(x)))
  (x - min(x)) / (max(x) - min(x))
}

results$wait_norm <- NA_real_
results$util_norm <- NA_real_
results$score <- NA_real_

for (pol in unique(results$policy)) {
  idx <- which(results$policy == pol)
  results$wait_norm[idx] <- normalize01(results$mean_wait_cal_days[idx])
  results$util_norm[idx] <- normalize01(results$mean_util[idx])
  results$score[idx] <- W_WAIT * results$wait_norm[idx] + W_UTIL * (1 - results$util_norm[idx])
}

new_feas <- results[results$policy=="new" & results$feasible, ]
if (nrow(new_feas) == 0) {
  warning("No feasible (s1,s2) found under the overrun constraint within the quantile grid.")
} else {
  best_new_score <- new_feas[which.min(new_feas$score), ]
  cat("\nRecommended (NEW) by SCORE:\n"); print(best_new_score)
}

# -------------------------
# 8) Boxplots (one simulation draw) for baseline vs recommended

#It goes as: OLD system, NEW system, and NEW system with optimal time slots

# -------------------------
evaluate_for_plots <- function(policy_fun, s1, s2, label) {
  planned <- policy_fun(df, s1, s2)
  sim <- simulate_execution(planned, d1, d2)
  exec <- sim$exec
  ot <- sim$overtime

  wait_days <- mapply(
    waiting_calendar_minutes,
    call_date = exec$Date,
    call_time = exec$Time,
    sched_date = exec$ScheduledDate,
    sched_start_min = exec$ActualStartMin
  ) / (60*24)

  list(label=label, wait_days=wait_days, delay_min=exec$IntradayDelayMin, overtime_min=ot$OvertimeMin)
}

plot_list <- list(
  baseline_old = evaluate_for_plots(schedule_old, BASE_S1, BASE_S2, "Baseline OLD (35,60)"),
  baseline_new = evaluate_for_plots(schedule_new, BASE_S1, BASE_S2, "Baseline NEW (35,60)")
)

if (exists("best_new_score") && nrow(new_feas) > 0) {
  plot_list$recommended_new <- evaluate_for_plots(
    schedule_new,
    best_new_score$s1, best_new_score$s2,
    paste0("Recommended NEW (", best_new_score$s1, ",", best_new_score$s2, ")")
  )
}

op <- par(no.readonly = TRUE)
par(mfrow=c(1,3), mar=c(5,4,4,1))

boxplot(lapply(plot_list, function(z) z$wait_days),
        names=sapply(plot_list, function(z) z$label),
        las=2, ylab="Waiting (calendar days)", main="Waiting time")

boxplot(lapply(plot_list, function(z) z$overtime_min),
        names=sapply(plot_list, function(z) z$label),
        las=2, ylab="Overtime (min per machine-day)", main="Overtime")

boxplot(lapply(plot_list, function(z) z$delay_min),
        names=sapply(plot_list, function(z) z$label),
        las=2, ylab="Intraday delay (minutes)", main="Intraday delays")

par(op)

# -------------------------
# 9) Export
# -------------------------
write.csv(results, "timeslot_optimization_results_with_sim.csv", row.names = FALSE)
cat("\nSaved: timeslot_optimization_results_with_sim.csv\n")
