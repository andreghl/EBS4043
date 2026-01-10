####

boot.mean <- function(B, data, tab = TRUE, p.dist = NULL){
    n <- length(data)
    Q <- rep(0, B)

    if(!is.null(p.dist)){
      # Parametric bootstrap
      p.dist <- substitute(p.dist)

      for(b in 1:B){
        s <- eval(p.dist)
        Q[b] <- mean(s)
      }
    } else {
      for(b in 1:B){
        # Non parametrc bootstrap
        s <- sample(data, size = n, replace = TRUE)
        Q[b] <- mean(s)
      }
    }

    if(tab){
          print(knitr::kable(data.frame(mean = mean(Q),
                                    se = sd(Q))))
    }
    Q
}

boot.var <- function(B, data, tab = TRUE, p.dist = NULL){
    n <- length(data)
    Q <- rep(0, B)

    if(!is.null(p.dist)){
      # Parametric bootstrap
      p.dist <- substitute(p.dist)

      for(b in 1:B){
        s <- eval(p.dist)
        Q[b] <- var(s)
      }
    } else {
      for(b in 1:B){
        s <- sample(data, size = n, replace = TRUE)
        Q[b] <- var(s)
        }
      }

    if(tab){
          print(knitr::kable(data.frame(mean = mean(Q),
                                    se = sd(Q))))
    }
    Q
}

alternatives <- function(data, as.int = FALSE){
  if(as.int){
    as.integer(levels(factor(data)))
  } else {
    levels(factor(data))
  }
}

edf <- function(data, Q){
    mean(data <= Q)
}

daily <- function(data, with.dates = FALSE, tab = TRUE){
  dates <- table(factor(data))

  if(tab){
    print(dates)
  }


  if(!with.dates){
    dates <- list(unname(dates))[[1]]
  }
  dates
}

hourly <- function(data, with.hours = FALSE, tab = TRUE){
  hours <- table(factor(as.integer(data)))

  if(tab){
    print(hours)
  }

  if(!with.hours){
    hours <- list(unname(hours))[[1]]
  }
  hours
}

hourly.day <- function(data){

  counts <- aggregate(
    x = list(count =  rep(1, nrow(data))),
    by = list(date = data$Date, time = as.integer(data$Time)),
    FUN = sum
  )
  counts
}

ci <- function(data, CI = c(0.025, 0.975)){
  ci <- as.data.frame(quantile(data, probs = CI))
  colnames(ci) <- "CI"
  knitr::kable(ci)
}

monte.carlo <- function(N = 1000, B = 1000, alpha = 0.05, p.dist = NULL, theta, data = NULL){

  S <- rep(0, N)
  if(!is.null(p.dist)){
    p.dist <- substitute(p.dist)
  }
  

  for(n in 1:N){

    if(!is.null(p.dist)){
      s <- eval(p.dist)
    }

    if(!is.null(data)){
      s <- sample(data, size = length(data), replace = TRUE)
    }

    Q  <- boot.mean(B, s, tab = FALSE)
    lb <- quantile(Q, prob = alpha / 2)
    ub <- quantile(Q, prob = 1 - (alpha / 2))

    if(lb <= theta && theta <= ub){
      S[n] <- 1
    }
  }
  mean(S)
}

stat.pois <- function(data, lambda){
  # Get all possible bins in Poisson histogram
  k <- as.integer(names(table(data)))
  exp <- dpois(k, lambda) * length(data)
  sum((table(data) - exp) ^ 2 / exp) 
} 

test.pois <- function(data, n.sim = 10000){
  # Null - (-> matches Poisson)
  # Alt. - Not Poisson distributed
  # ChiSq Test

  n <- length(data)
  m <- mean(data)

  if(n.sim > 0){
    obs.T <- stat.pois(data, m)

    sim.T <- replicate(n.sim, {sim.data <- rpois(n, m)
                               stat.pois(sim.data, m)}
                             )

    p.value <- mean(sim.T >= obs.T)
    data.frame(stat = obs.T, p.value = p.value)
  }
}

stat.gamma <- function(data, shape, scale, breaks = 10) {
  n <- length(data)
  
  # Transform gamma into bins
  prob <- seq(0, 1, length.out = breaks + 1)
  bins <- qgamma(prob, shape = shape, scale = scale)
  obs <- table(cut(data, bins, include.lowest = TRUE))
  
  exp.probs <- pgamma(bins[-1], shape, scale) -
               pgamma(bins[-length(bins)], shape, scale)
  
  exp <- n * exp.probs
  sum((obs - exp)^2 / exp)
}

test.gamma <- function(data, n.sim = 10000){
  # Null - (-> matches Gamma)
  # Alt. - Not Gamma distributed
  # ChiSq Test

  n <- length(data)
  shape <- mean(data) ^ 2 / var(data)
  scale <- var(data) / mean(data)

  if(n.sim > 0){
    obs.T <- stat.gamma(data, shape, scale)

    sim.T <- replicate(n.sim, {sim.data <- rgamma(n, shape, scale = scale)
                               stat.gamma(sim.data, shape, scale)}
                               )
    
    p.value <- mean(sim.T >= obs.T)
    data.frame(stat = obs.T, p.value = p.value)
  }
}

simulator <- function(data, timeslots = c(35, 60)){
  # Not done yet
  
}