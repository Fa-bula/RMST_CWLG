library("survival")
library("survminer")
library("plotly")

# Number of observations in each treatment groups
n <- 100
# Level of significance 
alpha <- 0.05
# Number of experiments
N <- 100
# Scale parameter of first (control) group
scale_1 <- 1
# Scale parameter of the second (treatment) group
scale_2 <- 1.5

calculate_logrank_power <- function(params) {
  # print(params)
  H0_rejected = c()
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
    
    survdiff <- survdiff(Surv(time = aval, event = event, type = 'right') ~ cohort, data = df)
    
    H0_rejected <- append(H0_rejected, survdiff$pvalue < alpha)
  }
  
  return (sum(H0_rejected) / length(H0_rejected))
}

shape_list <- seq(1,2,0.1)
p_cens_list <- seq(0.2,0.4,0.05)

results <- data.frame()
for (shape in shape_list) {
  for (p_cens in p_cens_list) {
    # Parameters of generated data
    params <- list()
    params[[1]] <- c(1, scale_1, p_cens)
    params[[2]] <- c(shape, scale_2, p_cens)
    power <- calculate_logrank_power(params=params)
    results <- rbind(results, data.frame(shape, p_cens, power))
  }
}

fig <- plot_ly(results, x = ~shape, y = ~p_cens, z = ~power, color = ~p_cens)

fig <- fig %>% add_markers()

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Shape'),
                                   
                                   yaxis = list(title = 'p_cens'),
                                   
                                   zaxis = list(title = 'Log-rank test power')))
fig
