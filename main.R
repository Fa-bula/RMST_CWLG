library("survival")
library("survminer")
library("plotly")
library("nph")

source("calculate_power_cc.R")

# Number of observations in each treatment groups
n <- 100
# Level of significance 
alpha <- 0.05
# Number of experiments
N <- 200
# Scale parameter of first (control) group
scale_1 <- 1
# Scale parameter of the second (treatment) group
scale_2 <- 1.6
# List of shape parameters of Weibull distribution
shape_list <- seq(1,2,0.05)
# List of censoring probabilities
p_cens_list <- seq(0.2,0.4,0.05)

cc_results <- data.frame()
for (shape in shape_list) {
  for (p_cens in p_cens_list) {
    # Parameters of generated data
    params <- list()
    params[[1]] <- c(1, scale_1, p_cens)
    params[[2]] <- c(shape, scale_2, p_cens)
    cc_powers <- calculate_power_cc(params=params)
    logrank_power <- cc_powers$logrank_power
    maxcombo_power <- cc_powers$maxcombo_power
    cc_results <-
      rbind(cc_results,
            data.frame(shape, p_cens, logrank_power, maxcombo_power))
  }
}

# 3D plot of results
fig <- plot_ly(cc_results, x = ~shape, y = ~p_cens, z = ~logrank_power, color = ~p_cens)

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

fig <- plot_ly(cc_results, x = ~shape, y = ~p_cens, z = ~maxcombo_power, color = ~p_cens)

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
cc_results %>%
  ggplot(aes(x = shape, y = logrank_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Test Power') +
  ggtitle(label = 'Log-rank test')

cc_results %>%
  ggplot(aes(x = shape, y = maxcombo_power, group = p_cens)) +
  geom_point(aes(colour = p_cens), size = 3) +
  geom_smooth(aes(group = p_cens, colour = p_cens),
              method = "lm",
              linewidth = 0.5) +
  geom_hline(yintercept = 0.8) +
  xlab('Shape') + ylab('Test Power') +
  ggtitle(label = 'Maxcombo test')

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

