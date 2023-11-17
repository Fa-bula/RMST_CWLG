library("grid")
library("gridExtra")
library("plotly")
library("eha")

args <- list()
for (n in n_list) {
  for (t_duration in t_duration_list) {
    args [[length(args) + 1]] <- list(
      var = "logrank_power",
      title = "Log-rank test",
      n = n,
      t_duration = t_duration
    )
    args [[length(args) + 1]] <- list(
      var = "maxcombo_power",
      title = "Maxcombo test",
      n = n,
      t_duration = t_duration
    )
    args [[length(args) + 1]] <- list(
      var = "rmst_power",
      title = "RMST test",
      n = n,
      t_duration = t_duration
    )
  }
}

# 2D plot of results
cc_plots <- lapply(args,
function (description) {
  return(
    cc_results %>% filter(n == description[["n"]] &
                            t_duration == description[["t_duration"]]) %>%
      ggplot(aes(
        x = shape, y = !!sym(description[["var"]])
      )) +
      geom_point(aes(colour = p_cens), size = 2, show.legend = FALSE) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5,
        show.legend = FALSE
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Shape') + ylab('Test Power') + scale_y_continuous(limits = c(0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                                             " Trial Duration = ", description[["t_duration"]])
      ) + guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=10))
  )
})

le_plots <- lapply(args,
function (description) {
  return(
    le_results %>% filter(n == description[["n"]] &
                            t_duration == description[["t_duration"]]) %>% ggplot(aes(
      x = effect_time, y = !!sym(description[["var"]])
    )) +
      geom_point(aes(colour = p_cens), size = 3, show.legend = FALSE) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5,
        show.legend = FALSE
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Effect Time') + ylab('Test Power') + scale_y_continuous(limits = c(0.0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                       " Trial Duration = ", description[["t_duration"]])
      ) +
      guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=10))
  )
})

ee_plots <- lapply(args,
function (description) {
  return(
    ee_results %>% filter(n == description[["n"]] &
                            t_duration == description[["t_duration"]]) %>% ggplot(aes(
      x = effect_time, y = !!sym(description[["var"]])
    )) +
      geom_point(aes(colour = p_cens), size = 3) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Effect Time') + ylab('Test Power') + scale_y_continuous(limits = c(0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                       " Trial Duration = ", description[["t_duration"]])
      ) +
      guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=10))
  )
})

# 2D plots of survival functions
base <-
  ggplot() +
  xlim(0, 2)

cc_survival <- base + lapply(shape_list, function(shape) {
  fun_name <- paste0("fun.", shape)
  geom_function(
    fun = pweibull,
    aes(colour = shape),
    args = list(
      shape = shape,
      scale = scale_2_cc,
      lower.tail = FALSE
    )
  )
}) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = pweibull,
    colour = "red",
    args = list(
      shape = 1,
      scale = scale_1_cc,
      lower.tail = FALSE
    )
  ) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions') + 
  guides(color = guide_legend(title = "Shape"))

cc_hazard <- base + lapply(shape_list, function(shape) {
  fun_name <- paste0("fun.", shape)
  geom_function(
    fun = hweibull,
    aes(colour = shape),
    args = list(shape = shape,
                scale = scale_2_cc)
  )
}) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = hweibull,
    colour = "red",
    args = list(shape = 1,
                scale = scale_1_cc)
  ) + xlab('Time') +
  ylab('Hazard') +
  ggtitle(label = 'Hazard Functions') + 
  guides(color = guide_legend(title = "Shape"))

# 2D plots of survival functions for late effect
pexp_le <- function(q, rate_1, rate_2, effect_time) {
  return(ifelse(
    q <= effect_time,
    pexp(q, rate = rate_1, lower.tail = FALSE),
    (1 - pexp(
      effect_time, rate = rate_1, lower.tail = TRUE
    )) *
      pexp(q - effect_time, rate = rate_2, lower.tail = FALSE)
  ))
}

base <-
  ggplot() +
  xlim(0, 1)

le_survival <-
  base + lapply(effect_time_list_le, function(effect_time) {
    fun_name <- paste0("fun.", effect_time)
    geom_function(
      fun = pexp_le,
      aes(colour = effect_time),
      args = list(
        rate_1 = rate_1_le,
        rate_2 = rate_2_le,
        effect_time = effect_time
      )
    )
  }) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = pexp,
    colour = "red",
    args = list(rate = rate_1_le,
                lower.tail = FALSE)
  ) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions')+ 
  guides(color = guide_legend(title = expression(
    paste("t" ["effect"]))))


le_hazard <- base + lapply(effect_time_list_le, function(effect_time) {
  fun_name <- paste0("fun.", effect_time)
  geom_function(
    fun = function(x, rate_1, rate_2, effect_time) {
      return(ifelse(x < effect_time, rate_1, rate_2))
    },
    aes(colour = effect_time),
    args = list(
      rate_1 = rate_1_le,
      rate_2 = rate_2_le,
      effect_time = effect_time
    )
  )
}) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = function(x, rate_1) {
      return(rate_1)
    },
    colour = "red",
    args = list(rate_1 = rate_1_le)
  ) + xlab('Time') +
  ylab('Hazard') +
  ggtitle(label = 'Hazard Functions')+ 
  guides(color = guide_legend(title = expression(
    paste("t" ["effect"]))))

for (start in 3 * seq(0, (length(n_list) * length(t_duration_list) - 1))) {
  grid.arrange(
    cc_plots[[start+1]],
    cc_plots[[start+2]],
    cc_plots[[start+3]],
    cc_survival,
    cc_hazard,
    ncol = 3,
    top = textGrob("Crossing Curves",
                   gp = gpar(fontsize = 10)),
    bottom = textGrob(
      sprintf(
        "Control group (red): ~exp(1)\n Treatment groups(green): Weibull(%.1f, shape)",
        scale_2_cc
      ),
      gp = gpar(fontsize = 10)
    )
  )
}

for (start in 3 * seq(0, (length(n_list) * length(t_duration_list) - 1))) {
  grid.arrange(
    le_plots[[start+1]],
    le_plots[[start+2]],
    le_plots[[start+3]],
    le_survival,
    le_hazard,
    ncol = 3,
    top = textGrob("Late Effect",
                   gp = gpar(fontsize = 17)),
    bottom = textGrob(expression(
      paste(
        "Control group (red): ~exp(1)\nTreatment groups(green): ~exp(1) before t" ["effect"],
        ", ~exp(0.5) thereafter"
      )
    ),
    gp = gpar(fontsize = 10))
  )
}

for (start in 3 * seq(0, (length(n_list) * length(t_duration_list) - 1))) {
  grid.arrange(
    ee_plots[[start+1]],
    ee_plots[[start+2]],
    ee_plots[[start+3]],
    ncol = 3,
    top = textGrob("Early Effect",
                   gp = gpar(fontsize = 17)),
    bottom = textGrob(expression(
      paste(
        "Control group (red): ~exp(1)\nTreatment groups(green): ~exp(1) before t" ["effect"],
        ", ~exp(0.5) thereafter"
      )
    ),
    gp = gpar(fontsize = 10))
  )
}
