# CASE STUDY: MRI SCHEDULING ANALYSIS (PART 1)

set.seed(2026) # For reproducibility

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

# Function to calculate inter-arrival times respecting 08:00-17:00 working hours
get_interarrival_times <- function(dates, times) {
  # Create a dataframe and sort by date/time
  df <- data.frame(date = dates, time = times)
  df <- df[order(df$date, df$time), ]
  
  # Convert unique dates to a numeric index (Day 1, Day 2, etc.)
  day_index <- as.numeric(factor(df$date, levels = unique(df$date)))
  
  # Calculate continuous "working hours" timestamp
  # Logic: (Day-1) * 9 hours + (Time - 8.0)
  # This ignores the 15 hours between 17:00 and 08:00
  continuous_time <- (day_index - 1) * 9 + (df$time - 8)
  
  # Return differences between consecutive calls
  return(diff(continuous_time))
}

# Generic Bootstrap Function (Handles both Parametric and Non-Parametric)
# - If 'sim_function' is provided -> Parametric (simulates data)
# - If 'sim_function' is NULL     -> Non-Parametric (resamples data)
run_bootstrap <- function(data, B, statistic_fn, sim_function = NULL) {
  n <- length(data)
  replicates <- numeric(B)
  
  for(i in 1:B) {
    if(!is.null(sim_function)) {
      # Parametric: Generate new data from model
      sample_data <- sim_function(n) 
    } else {
      # Non-Parametric: Resample existing data
      sample_data <- sample(data, size = n, replace = TRUE)
    }
    replicates[i] <- statistic_fn(sample_data)
  }
  
  # Return stats
  list(
    est = mean(replicates),
    se  = sd(replicates),
    ci  = quantile(replicates, probs = c(0.025, 0.975))
  )
}

# ==============================================================================
# 2. LOAD AND PREPARE DATA
# ==============================================================================

# Ensure the file is in your working directory or use setwd()
if(file.exists("EBS4043/data/ScanRecords.csv")) {
  df <- read.csv("EBS4043/data/ScanRecords.csv")
} else {
  stop("Error: 'ScanRecords.csv' not found.")
}

# Split data by patient type
data_p1 <- subset(df, PatientType == "Type 1")
data_p2 <- subset(df, PatientType == "Type 2")

# ==============================================================================
# 3. TYPE 1 ANALYSIS (PARAMETRIC)
# ==============================================================================
# Known: Duration ~ Normal, Arrivals ~ Poisson (Inter-arrivals ~ Exponential)

# --- A. Durations ---
mu1_hat <- mean(data_p1$Duration)
sd1_hat <- sd(data_p1$Duration)

# Define simulation function for Normal distribution
sim_normal <- function(n) { rnorm(n, mean = mu1_hat, sd = sd1_hat) }

# Bootstrap: Mean Duration
boot_p1_mean <- run_bootstrap(data_p1$Duration, B = 5000, mean, sim_normal)

# --- B. Arrivals ---
# Calculate inter-arrival times (hours)
inter_arrivals_p1 <- get_interarrival_times(data_p1$Date, data_p1$Time)
lambda1_hat <- 1 / mean(inter_arrivals_p1) # Rate parameter for Exponential

# ==============================================================================
# 4. TYPE 2 ANALYSIS (NON-PARAMETRIC)
# ==============================================================================
# Known: Distributions are UNKNOWN.

# --- A. Durations ---
# Bootstrap: Mean Duration (Resampling directly from data)
boot_p2_mean <- run_bootstrap(data_p2$Duration, B = 5000, mean, sim_function = NULL)

# Bootstrap: 95th Percentile (Quantile)
q95_fn <- function(x) { as.numeric(quantile(x, 0.95)) }
boot_p2_q95 <- run_bootstrap(data_p2$Duration, B = 5000, q95_fn, sim_function = NULL)

# --- B. Arrivals ---
# Calculate inter-arrival times (hours)
inter_arrivals_p2 <- get_interarrival_times(data_p2$Date, data_p2$Time)
mean_inter_p2 <- mean(inter_arrivals_p2) # Just the empirical mean

# ==============================================================================
# 5. ROBUSTNESS CHECK (MONTE CARLO STUDY)
# ==============================================================================
# Goal: Prove to management that Non-Parametric Bootstrap works even if we 
# don't know the true distribution. We simulate a "True" Gamma distribution.

cat("\nRunning Monte Carlo Robustness Check (Type 2)...\n")
mc_reps <- 1000
coverage_count <- 0
# Assume a "True" Gamma distribution mimicking Type 2 data stats
shape_true <- (mean(data_p2$Duration)^2) / var(data_p2$Duration)
scale_true <- var(data_p2$Duration) / mean(data_p2$Duration)
true_q95   <- qgamma(0.95, shape = shape_true, scale = scale_true)

for(i in 1:mc_reps) {
  # 1. Generate synthetic data from the "Truth"
  syn_data <- rgamma(length(data_p2$Duration), shape = shape_true, scale = scale_true)
  
  # 2. Run Non-Parametric Bootstrap on this synthetic data
  bs_res <- run_bootstrap(syn_data, B = 500, q95_fn, sim_function = NULL)
  
  # 3. Check if the interval captured the "True" Q95
  if(true_q95 >= bs_res$ci[1] && true_q95 <= bs_res$ci[2]) {
    coverage_count <- coverage_count + 1
  }
}
coverage_pct <- (coverage_count / mc_reps) * 100

# ==============================================================================
# 6. SLOT OPTIMIZATION (OVERRUN ANALYSIS)
# ==============================================================================
# Goal: Find slot length where P(Duration > Slot) <= 10%

slot_options <- seq(20, 90, by = 5) # Minutes
results_df <- data.frame(Minutes = slot_options, Hours = slot_options / 60)

# Calculate overrun probability for each slot
# Type 1: Use Theoretical Normal (1 - pnorm)
results_df$P1_Overrun <- 1 - pnorm(results_df$Hours, mean = mu1_hat, sd = sd1_hat)

# Type 2: Use Empirical Data (mean(duration > slot))
results_df$P2_Overrun <- sapply(results_df$Hours, function(s) {
  mean(data_p2$Duration > s)
})

# Select recommended slots (first one where overrun <= 10%)
rec_slot_p1 <- results_df$Minutes[which(results_df$P1_Overrun <= 0.10)[1]]
rec_slot_p2 <- results_df$Minutes[which(results_df$P2_Overrun <= 0.10)[1]]

# ==============================================================================
# 7. PRINT FINAL REPORT
# ==============================================================================

cat("\n--- PATIENT TYPE 1 (Parametric: Normal/Exp) ---\n")
cat(sprintf("Mean Duration:      %.2f hrs (SE: %.4f)\n", mu1_hat, boot_p1_mean$se))
cat(sprintf("Arrival Rate:       %.2f patients/hour\n", lambda1_hat))
cat(sprintf("Mean Inter-arrival: %.2f mins\n", (1/lambda1_hat)*60))

cat("\n--- PATIENT TYPE 2 (Non-Parametric) ---\n")
cat(sprintf("Mean Duration:      %.2f hrs (SE: %.4f)\n", boot_p2_mean$est, boot_p2_mean$se))
cat(sprintf("95%% Q Duration:     %.2f hrs (SE: %.4f)\n", boot_p2_q95$est, boot_p2_q95$se))
cat(sprintf("Mean Inter-arrival: %.2f mins\n", mean_inter_p2 * 60))

cat("\n--- ROBUSTNESS CHECK (Monte Carlo) ---\n")
cat(sprintf("Bootstrap Coverage: %.1f%% of intervals captured the true value.\n", coverage_pct))
cat("(A value near 95% confirms the method is reliable for unknown distributions.)\n")

cat("\n--- SLOT RECOMMENDATIONS (Target: <10% Overrun) ---\n")
cat(sprintf("Type 1 Recommended Slot: %d minutes (Overrun Prob: %.1f%%)\n", 
            rec_slot_p1, results_df$P1_Overrun[results_df$Minutes == rec_slot_p1]*100))
cat(sprintf("Type 2 Recommended Slot: %d minutes (Overrun Prob: %.1f%%)\n", 
            rec_slot_p2, results_df$P2_Overrun[results_df$Minutes == rec_slot_p2]*100))

cat("\n--- OVERRUN TABLE (Partial) ---\n")
print(format(results_df[results_df$Minutes %in% seq(20, 90, 10), ], digits=3))

# Visualize Overrun 
 plot(results_df$Minutes, results_df$P1_Overrun, type='l', col='blue', 
      main="Overrun Probability vs Slot Length", xlab="Minutes", ylab="Prob(Overrun)")
 lines(results_df$Minutes, results_df$P2_Overrun, col='red')
 legend("topright", legend=c("Type 1", "Type 2"), col=c("blue", "red"), lty=1)
 