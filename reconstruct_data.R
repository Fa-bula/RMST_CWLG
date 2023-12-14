library(KMtoIPD)
library(dplyr)
library(survival)

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
  mutate(arm = as.numeric(factor(IPD$cohort)) - 1)

run_tests(df = IPD)
