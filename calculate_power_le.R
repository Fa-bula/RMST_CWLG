calculate_power_le <- function(params) {
  H0_rejected_logrank = c()
  H0_rejected_maxcombo = c()
  for (exp in (1:N)) {
    i <- 0
    df <- data.frame()
    for (par in params) {
      i <- i + 1
      begin <- 1 + (i - 1) * n
      end <- n + (i - 1) * n
      rate_1 <- par[1]
      rate_2 <- par[2]
      effect_time <- par[3]
      p_cens <- par[4]
      v1 <- rexp(n = n, rate = rate_1)
      v2 <- rexp(n = n, rate = rate_2)
      aval <- ifelse(v1 > effect_time, effect_time + v2, v1)
      event <- rbinom(n, 1, 1 - p_cens)
      cohort <- sprintf("EXP-%.2f-%.2f-%.2f", rate_1, rate_2, effect_time)
      
      usubjid <- sprintf("SUBJ-%03d", begin:end)
      df <-
        rbind(df, data.frame(usubjid, cohort, aval, event, rate_1, rate_2, effect_time))
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