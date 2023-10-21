run_tests <-
  function(df) {
    # Performing Log-rank test on df
    diff <- survdiff(Surv(
      time = aval,
      event = event,
      type = 'right'
    ) ~ cohort,
    data = df)
    
    logrank_pval = pchisq(diff$chisq, length(diff$n) - 1, lower.tail = FALSE)

    # Performing MaxCombo test on df
    maxcombo_pval <- logrank.maxtest(df$aval,
                                     df$event,
                                     df$cohort,
                                     "two.sided")$pmult
    
    # Performing RMST test on df
    rmst_pval <- rmst2(
      time = df$aval,
      status = df$event,
      alpha = alpha,
      arm = df$arm
    )[5]$unadjusted.result[1, 4]
    
    results <-
      list(
        "logrank_pval" = logrank_pval,
        "maxcombo_pval" = maxcombo_pval,
        "rmst_pval" = rmst_pval
      )
    return(results)
  }
