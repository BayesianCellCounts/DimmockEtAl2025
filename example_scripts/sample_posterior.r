#!/usr/bin/env r

cat("opt=", getOption("cmdstanr_save_cmdstan_config"),
    " env='", Sys.getenv("CMDSTANR_SAVE_CMDSTAN_CONFIG"), "'\n", sep="")
Sys.setenv(CMDSTANR_VERBOSE="1")

suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
})

## --- data ---
df <- readr::read_csv("data.csv", show_col_types = FALSE) |>
  dplyr::group_by(BrainRegion, group, rat_ID) |>
  dplyr::summarise(
    y    = sum(Cfos_Counts),
    area = sum((1 - VOID_PERCENTAGE/100) * (0.94 * 0.67)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    BrainRegion = forcats::fct_drop(factor(BrainRegion)),
    group       = forcats::fct_drop(factor(group))
  )

# sanity checks before log()
stopifnot(all(df$area > 0))
stan_data <- list(
  R          = nlevels(df$BrainRegion),
  G          = nlevels(df$group),
  N          = nrow(df),
  region_idx = as.integer(df$BrainRegion),
  group_idx  = as.integer(df$group),
  E          = log(df$area),
  y          = df$y
)
stopifnot(length(unique(df$BrainRegion)) == stan_data$R)
stopifnot(length(unique(df$group)) == stan_data$G)

## --- compile ---
mod <- cmdstan_model(
  "models/model_poiss.stan",
  cpp_options = list(stan_threads = TRUE)  # keep TRUE only if model uses reduce_sum/map_rect
)

## --- sample ---
fit <- mod$sample(
  data              = stan_data,
  chains            = 4,
  parallel_chains   = 4,
  iter_warmup       = 4000,
  iter_sampling     = 4000,
  seed              = 3511,
  threads_per_chain = 2                       # required because we compiled with stan_threads=TRUE
)

## --- save ---
fit$save_object("fits/fit_poiss.rds")

## --- quick console summary (useful under littler) ---
print(fit$summary())
