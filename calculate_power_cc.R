calculate_power_cc <- function(params) {
  H0_rejected <- data.frame()
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
      arm <- i - 1
      usubjid <- sprintf("SUBJ-%03d", begin:end)
      df <-
        rbind(df, data.frame(usubjid, cohort, arm, aval, event, scale, shape))
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