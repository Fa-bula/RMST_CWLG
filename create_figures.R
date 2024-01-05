library("grid")
library("gridExtra")
library("plotly")
library("eha")
library("ggpubr")

axis_text_size <- 8

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
      geom_point(aes(colour = p_cens), size = 1, show.legend = TRUE) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5,
        show.legend = TRUE
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Shape') + ylab('Test Power') + scale_y_continuous(limits = c(0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                                             "\nTrial Duration = ", description[["t_duration"]])
      ) + guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=axis_text_size),
            axis.title.x = element_text(size=axis_text_size),
            axis.title.y = element_text(size=axis_text_size))
  )
})

le_plots <- lapply(args,
function (description) {
  return(
    le_results %>% filter(n == description[["n"]] &
                            t_duration == description[["t_duration"]]) %>% ggplot(aes(
      x = effect_time, y = !!sym(description[["var"]])
    )) +
      geom_point(aes(colour = p_cens), size = 1, show.legend = TRUE) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5,
        show.legend = TRUE
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Effect Time') + ylab('Test Power') + scale_y_continuous(limits = c(0.0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                       "\nTrial Duration = ", description[["t_duration"]])
      ) +
      guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=axis_text_size),
             axis.title.x = element_text(size=axis_text_size),
             axis.title.y = element_text(size=axis_text_size))
  )
})

ee_plots <- lapply(args,
function (description) {
  return(
    ee_results %>% filter(n == description[["n"]] &
                            t_duration == description[["t_duration"]]) %>% ggplot(aes(
      x = effect_time, y = !!sym(description[["var"]])
    )) +
      geom_point(aes(colour = p_cens), size = 1) +
      geom_smooth(
        aes(group = p_cens, colour = p_cens),
        method = "lm",
        linewidth = 0.5
      ) +
      geom_hline(yintercept = 0.8) +
      xlab('Effect Duration') + ylab('Test Power') + scale_y_continuous(limits = c(0, 1.1)) +
      ggtitle(
        label = paste0(description[["title"]], " N = ", description[["n"]],
                       "\nTrial Duration = ", description[["t_duration"]])
      ) +
      guides(color = guide_legend(title = "Censoring probability")) +
      theme(plot.title = element_text(size=axis_text_size),
            axis.title.x = element_text(size=axis_text_size),
            axis.title.y = element_text(size=axis_text_size))
  )
})


# 2D plots of survival and hazard functions for crossing curves
base <-
  ggplot() +
  xlim(0, 5)

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


# 2D plots of survival and hazard functions for late effect
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


# 2D plots of survival and hazard functions for early effect
ee_survival <-
  base + lapply(effect_time_list_ee, function(effect_dur) {
    fun_name <- paste0("fun.", effect_dur)
    geom_function(
      fun = pexp_le,
      aes(colour = effect_dur),
      args = list(
        rate_1 = rate_2_ee,
        rate_2 = rate_1_ee,
        effect_time = effect_dur
      )
    )
  }) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = pexp,
    colour = "red",
    args = list(rate = rate_1_ee,
                lower.tail = FALSE)
  ) + xlab('Time') +
  ylab('Survival Probability') +
  ggtitle(label = 'Survival Functions')+ 
  guides(color = guide_legend(title = expression(
    paste("dur" ["effect"]))))

ee_hazard <- base + lapply(effect_time_list_ee, function(effect_dur) {
  fun_name <- paste0("fun.", effect_dur)
  geom_function(
    fun = function(x, rate_1, rate_2, effect_dur) {
      return(ifelse(x < effect_dur, rate_1, rate_2))
    },
    aes(colour = effect_dur),
    args = list(
      rate_1 = rate_2_ee,
      rate_2 = rate_1_ee,
      effect_dur = effect_dur
    )
  )
}) + scale_color_gradient(low = "#00FF00", high = "darkgreen") +
  geom_function(
    fun = function(x, rate_1) {
      return(rate_1)
    },
    colour = "red",
    args = list(rate_1 = rate_1_ee)
  ) + xlab('Time') +
  ylab('Hazard') +
  ggtitle(label = 'Hazard Functions')+ 
  guides(color = guide_legend(title = expression(
    paste("dur" ["effect"]))))

# Combining plots together
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
    ee_survival,
    ee_hazard,
    ncol = 3,
    top = textGrob("Early Effect",
                   gp = gpar(fontsize = 17)),
    bottom = textGrob(expression(
      paste(
        "Control group (red): ~exp(1)\nTreatment groups(green): ~exp(1) before t" ["effect"],
        ", ~exp(0.4) thereafter"
      )
    ),
    gp = gpar(fontsize = 10))
  )
}

# Combine plots for poster
cc_res_plot <- ggarrange(
  cc_plots[[1]],
  cc_plots[[2]],
  cc_plots[[3]],
  cc_plots[[7]],
  cc_plots[[8]],
  cc_plots[[9]],
  common.legend = TRUE, legend="bottom",
  ncol = 3,
  nrow=2)

ggexport(cc_res_plot, filename = "cc_res_plot.pdf", height=4, width=6)

cc_sur_haz_plot <- ggarrange(
  cc_survival,
  cc_hazard
)

cc_sur_haz_plot <- annotate_figure(cc_sur_haz_plot, bottom = textGrob(
  sprintf(
    "Control group (red): ~exp(1) Treatment groups(green): Weibull(%.1f, shape)",
    scale_2_cc
  ),
  gp = gpar(fontsize = 10)
))

ggexport(cc_sur_haz_plot, filename = "cc_sur_haz_plot.pdf", height=4, width=10)

ee_res_plot <- ggarrange(
  ee_plots[[10]],
  ee_plots[[11]],
  ee_plots[[12]], 
  ee_plots[[13]],
  ee_plots[[14]],
  ee_plots[[15]],
  common.legend = TRUE, legend="bottom",
  ncol = 3,
  nrow=2)

ggexport(ee_res_plot, filename = "ee_res_plot.pdf", height=4, width=10)

ee_sur_haz_plot <- ggarrange(
  ee_survival,
  ee_hazard
)

ee_sur_haz_plot <-
  annotate_figure(ee_sur_haz_plot, bottom = textGrob(expression(
    paste(
      "Control group (red): ~exp(1); Treatment groups(green): ~exp(0.4) before dur" ["effect"],
      ", ~exp(1) thereafter"
      )
  )))

ggexport(ee_sur_haz_plot, filename = "ee_sur_haz_plot.pdf", height=4, width=10)


le_res_plot <- ggarrange(
  le_plots[[10]],
  le_plots[[11]],
  le_plots[[12]], 
  le_plots[[13]],
  le_plots[[14]],
  le_plots[[15]],
  common.legend = TRUE, legend="bottom",
  ncol = 3,
  nrow=2)

ggexport(le_res_plot, filename = "le_res_plot.pdf", height=4, width=10)

le_sur_haz_plot <- ggarrange(
  le_survival,
  le_hazard
)

le_sur_haz_plot <-
  annotate_figure(le_sur_haz_plot, bottom = textGrob(expression(
    paste(
      "Control group (red): ~exp(1); Treatment groups(green): ~exp(1) before t" ["effect"],
      ", ~exp(0.5) thereafter"
    )
  )))

ggexport(le_sur_haz_plot, filename = "le_sur_haz_plot.pdf", height=4, width=10)
