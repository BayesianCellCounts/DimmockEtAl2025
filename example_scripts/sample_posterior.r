#!/usr/bin/env r

# ---- libraries ----
suppressPackageStartupMessages({
  library(tidyverse)
  library(cmdstanr)
})

# ---- print basic env/tooling info ----
Sys.setenv(CMDSTANR_VERBOSE = "1")  # show the exact CmdStan command
Sys.setenv(MAKEFLAGS = paste0("-j", max(1, parallel::detectCores() - 1)))  # tame compile parallelism
cat("CmdStan path: ", cmdstanr::cmdstan_path(), "\n", sep = "")
cat("CmdStan version: ", cmdstanr::cmdstan_version(), "\n", sep = "")
cat("Detected cores: ", parallel::detectCores(), "\n", sep = "")

# ---- data ----
raw <- readr::read_csv("data.csv", show_col_types = FALSE)

# If 'void' is missing, treat it as 0; coalesce any NAs to 0
if (!"void" %in% names(raw)) raw <- dplyr::mutate(raw, void = 0)
raw <- dplyr::mutate(raw, void = as.numeric(void))

df <- raw |>
  dplyr::group_by(region, group, id) |>
  dplyr::summarise(
    y    = sum(counts),
    area = sum((1 - dplyr::coalesce(void, 0) / 100) * (0.94 * 0.67)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    region = forcats::fct_drop(factor(region)),
    group  = forcats::fct_drop(factor(group))
  )

# quick sanity prints
cat("Unique regions (R): ", nlevels(df$region), "\n", sep = "")
cat("Unique groups  (G): ", nlevels(df$group),  "\n", sep = "")
cat("Rows used      (N): ", nrow(df),           "\n", sep = "")
cat("Regions: ", paste(levels(df$region), collapse = ", "), "\n", sep = "")
cat("Groups : ", paste(levels(df$group),  collapse = ", "), "\n", sep = "")

# ---- stan data ----
stopifnot(all(df$area > 0))
stan_data <- list(
  R          = nlevels(df$region),
  G          = nlevels(df$group),
  N          = nrow(df),
  region_idx = as.integer(df$region),
  group_idx  = as.integer(df$group),
  E          = log(df$area),
  y          = df$y
)
stopifnot(length(unique(df$region)) == stan_data$R)
stopifnot(length(unique(df$group))  == stan_data$G)

# ---- compile (threads enabled only if your Stan uses reduce_sum/map_rect) ----
cat("Compiling model...\n")
mod <- cmdstan_model(
  "models/model_poiss.stan",
  cpp_options = list(stan_threads = TRUE)
)
cat("Compile done.\n")

# ---- sample ----
chains <- 4
parallel_chains <- 4
threads_per_chain <- 2  # because we compiled with stan_threads=TRUE
cat(
  sprintf("Sampling: chains=%d (parallel=%d), iters=%d warmup + %d sampling, threads_per_chain=%d\n",
          chains, parallel_chains, 4000, 4000, threads_per_chain)
)

fit <- mod$sample(
  data              = stan_data,
  chains            = chains,
  parallel_chains   = parallel_chains,
  iter_warmup       = 4000,
  iter_sampling     = 4000,
  seed              = 3511,
  threads_per_chain = threads_per_chain,
  refresh           = 100    # print progress every 100 iters
)

# ---- save ----
if (!dir.exists("fits")) dir.create("fits", recursive = TRUE)
fit$save_object("fits/fit_poiss.rds")
cat("Saved fit to fits/fit_poiss.rds\n")

# ---- quick summary ----
print(head(fit$summary(), 12))
