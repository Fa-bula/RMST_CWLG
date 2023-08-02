library("survival")
library("survminer")

# Parameters of generated data
params <- list()
params[[1]] <- c(1,1)
params[[2]] <- c(1,10)

n <- 100
p_cens <- 0.3

i <- 0
df <- data.frame()
for (par in params) {
  i <- i + 1
  begin <- 1 + (i - 1) * n
  end <- n + (i - 1) * n
  shape <- par[1]
  scale <- par[2]
  aval <- rweibull(n = n, shape = shape, scale = scale)
  event <- rbinom(n, 1, 1 - p_cens)
  cohort <- sprintf("WEIBULL-%d-%d", shape, scale)
  
  usubjid <- sprintf("SUBJ-%03d", begin:end)
  df <-
    rbind(df, data.frame(usubjid, cohort, aval, event, scale, shape))
}

fit <-
  survfit(formula = Surv(time = aval, event = event, type = 'right') ~ cohort,
          data = df)

ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

