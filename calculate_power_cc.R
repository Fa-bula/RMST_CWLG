calculate_power_cc <- function(params) {
  H0_rejected_logrank = c()
  H0_rejected_maxcombo = c()
  for (exp in (1:N)) {
    i <- 0
    df <- data.frame()
    for (par in params) {
      i <- i + 1
      begin <- 1 + (i - 1) * n
      end <- n + (i - 1) * n
      shape <- par[1]
      scale <- par[2]
      p_cens <- par[3]
      aval <- rweibull(n = n, shape = shape, scale = scale)
      event <- rbinom(n, 1, 1 - p_cens)
      cohort <- sprintf("WEIBULL-%.2f-%.2f", shape, scale)
      
      usubjid <- sprintf("SUBJ-%03d", begin:end)
      df <-
        rbind(df, data.frame(usubjid, cohort, aval, event, scale, shape))
    }
    
    logrank_pval <- survdiff(Surv(time = aval,
                                  event = event,
                                  type = 'right') ~ cohort,
                             data = df)$pvalue
    
    maxcombo_pval <- logrank.maxtest(df$aval,
                                     df$event,
                                     df$cohort,
                                     "two.sided")$pmult
    
    H0_rejected_logrank <-
      append(H0_rejected_logrank, logrank_pval < alpha)
    
    H0_rejected_maxcombo <- append(H0_rejected_maxcombo,
                                   maxcombo_pval < alpha)
  }
  
  newList <-
    list(
      "logrank_power" = (sum(H0_rejected_logrank) / length(H0_rejected_logrank)),
      "maxcombo_power" = (sum(H0_rejected_maxcombo) / length(H0_rejected_maxcombo))
    )
  return(newList)
}