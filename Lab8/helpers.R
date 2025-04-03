columns_summary <- function(data.set) {
  mean <- sapply(data.set, mean)
  min <- sapply(data.set, min)
  first.quantile <- sapply(data.set, quantile)[2, ]
  second.quantile <- sapply(data.set, quantile)[3, ]
  third.quantile <- sapply(data.set, quantile)[4, ]
  max <- sapply(data.set, max)
  std.deviation <- sapply(data.set, sd)
  
  out <- rbind(min, first.quantile, second.quantile, third.quantile, max, mean, std.deviation)
  
  return(out)
}

