library(KMtoIPD)
library(dplyr)
library(survival)
library(gtsummary)
library(ggsurvfit)

library(nph)
library(survRM2)

source("run_tests.R")
source("parameters.R")

censored <- read.csv("censored.csv", header = TRUE)
bottom <- read.csv("bottom.csv", header = TRUE)
cohort_info <- list(list(name = "Fulvestrant plus placebo", n = 71),
                  list(name = "Fulvestrant plus capivasertib", n = 69))

IPD_list <- lapply(cohort_info,
       function(cohort_info) {
         return(getIPD(
           n = cohort_info$n,
           t = bottom %>% filter(cohort == cohort_info$name) %>% pull(x),
           S = bottom %>% filter(cohort == cohort_info$name) %>% pull(y),
           cens.t = censored %>% filter(cohort == cohort_info$name) %>% pull(x)
         ) %>% mutate(cohort = cohort_info$name) %>% rename(aval = t))
       })

IPD <-
  rbind(IPD_list[[1]], IPD_list[[2]]) %>%
  mutate(arm = -as.numeric(factor(cohort)) + 2)

run_tests(df = IPD)

coxph(Surv(aval, event) ~ arm, data = IPD) %>% tbl_regression(exp = TRUE)

survfit2(Surv(aval, event) ~ cohort, data = IPD) %>% 
  ggsurvfit() +
  labs(
    x = "Time since randomisation (months)",
    y = "Overall survival"
  ) +
  add_censor_mark(size = 3, alpha = 0.4) +
  scale_x_continuous(labels = seq(0,40,5),
                     breaks = seq(0,40,5),
                     limits = c(0,40),
                     expand = c(0,1)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0,1,0.1),
                     limits = c(0,1),
                     ) +
  add_risktable(risktable_stats = "{n.risk} ({cum.censor})",
                times = seq(0,40,5))
