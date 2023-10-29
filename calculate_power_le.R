calculate_power_le <- function(params) {
  H0_rejected <- data.frame()
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
      # Censoring by end of trial
      le_trial_duration <- aval <= t_duration
      aval <- ifelse(aval > t_duration, t_duration, aval)
      event <- event & le_trial_duration

      cohort <- sprintf("EXP-%.2f-%.2f-%.2f", rate_1, rate_2, effect_time)
      arm <- i - 1
      
      usubjid <- sprintf("SUBJ-%03d", begin:end)
      df <-
        rbind(df, data.frame(usubjid, cohort, arm, aval, event, rate_1, rate_2, 
                             effect_time, t_duration))
    }

    pvals <- run_tests(df)
    H0_rejected <-
      rbind(
        H0_rejected,
        data.frame(
          logrank = pvals$logrank_pval < alpha,
          maxcombo = pvals$maxcombo_pval < alpha,
          rmst = pvals$rmst_pval < alpha
        )
      )
  }
  
  results <-
    list(
      "logrank_power" = (sum(H0_rejected$logrank) / length(H0_rejected$logrank)),
      "maxcombo_power" = (sum(H0_rejected$maxcombo) / length(H0_rejected$maxcombo)),
      "rmst_power" = (sum(H0_rejected$rmst) / length(H0_rejected$rmst))
    )
  return(results)
}