library("survival")
library("survminer")
library("plotly")
library("nph")

# Number of observations in each treatment groups
n <- 100
# Level of significance 
alpha <- 0.05
# Number of experiments
N <- 100
# Scale parameter of first (control) group
scale_1 <- 1
# Scale parameter of the second (treatment) group
scale_2 <- 1.6
# List of shape parameters of Weibull distribution
shape_list <- seq(1,2,0.1)
# List of censoring probabilities
p_cens_list <- seq(0.2,0.4,0.05)

calculate_power <- function(params) {
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

results <- data.frame()
for (shape in shape_list) {
  for (p_cens in p_cens_list) {
    # Parameters of generated data
    params <- list()
    params[[1]] <- c(1, scale_1, p_cens)
    params[[2]] <- c(shape, scale_2, p_cens)
    powers <- calculate_power(params=params)
    logrank_power <- powers$logrank_power
    maxcombo_power <- powers$maxcombo_power
    results <-
      rbind(results,
            data.frame(shape, p_cens, logrank_power, maxcombo_power))
  }
}

# 3D plot of results
fig <- plot_ly(results, x = ~shape, y = ~p_cens, z = ~logrank_power, color = ~p_cens)

fig <-
  fig %>% add_mesh(
    x = c(
      min(shape_list),
      min(shape_list),
      max(shape_list),
      max(shape_list)
    ),
    y = c(
      min(p_cens_list),
      max(p_cens_list),
      min(p_cens_list),
      max(p_cens_list)
    ),
    z = c(0.8, 0.8, 0.8, 0.8),
    opacity = 0.4
  )

fig <- fig %>% add_markers()

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Shape'),

                                   yaxis = list(title = 'p_cens'),

                                   zaxis = list(title = 'Log-rank test power')))

fig

fig <- plot_ly(results, x = ~shape, y = ~p_cens, z = ~maxcombo_power, color = ~p_cens)

fig <-
  fig %>% add_mesh(
    x = c(
      min(shape_list),
      min(shape_list),
      max(shape_list),
      max(shape_list)
    ),
    y = c(
      min(p_cens_list),
      max(p_cens_list),
      min(p_cens_list),
      max(p_cens_list)
    ),
    z = c(0.8, 0.8, 0.8, 0.8),
    opacity = 0.4
  )

fig <- fig %>% add_markers()

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Shape'),
                                   
                                   yaxis = list(title = 'p_cens'),
                                   
                                   zaxis = list(title = 'MaxCombo test power')))

fig

# 2D plots of results
results %>%
  ggplot(aes(x = shape, y = logrank_power)) +
  geom_point(aes(colour = p_cens), size = 2) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Log-rank test power') +
  ggtitle(label = 'Log-rank test power')

results %>%
  ggplot(aes(x = shape, y = maxcombo_power)) +
  geom_point(aes(colour = p_cens), size = 2) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Maxcombo log-rank test power') +
  ggtitle(label = 'MaxCombo test power')

# 2D plots of survival functions
base <-
  ggplot() +
  xlim(0, 10)

base + lapply(shape_list, function(shape) {
  fun_name <- paste0("fun.", shape)
  geom_function(
    fun = pweibull,
    aes(colour = shape),
    args = list(
      shape = shape,
      scale = scale_2,
      lower.tail = FALSE
    )
  )
}) + geom_function(
  fun = pweibull,
  colour = "red",
  args = list(
    shape = 1,
    scale = scale_1,
    lower.tail = FALSE
  )
) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions')

