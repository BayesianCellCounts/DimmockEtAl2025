#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(cmdstanr); library(tidyverse); library(HDInterval)
})

## ---- helpers ----
canon <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_+|_+$", "", x) }
initials <- function(x) { p <- unlist(strsplit(as.character(x), "_+")); p <- p[nzchar(p)]
  paste0(toupper(substr(p,1,1)), collapse="") }
read_region_list <- function(path) {
  rl <- readr::read_lines(path, progress = FALSE); rl <- trimws(rl); rl <- rl[nzchar(rl)]
  unique(rl)
}
print_help <- function() cat("
Usage:
  Rscript plot_results.r TOP_GROUP BOTTOM_GROUP [regions.txt] [-o alpha|t|b|tabs|babs]
  Rscript plot_results.r --help

Required:
  TOP_GROUP           Name of the 'top' group (case/punctuation insensitive)
  BOTTOM_GROUP        Name of the 'bottom' group (case/punctuation insensitive)

Optional:
  regions.txt         Plain text file with one region per line. If omitted, all regions are used.
  -o ORDER            X-axis order:
                        alpha  - alphabetical by region
                        t      - ascending frequentist mean (top - bottom)
                        b      - ascending Bayesian mean
                        tabs   - ascending |frequentist mean|
                        babs   - ascending |Bayesian mean|

Details:
  • Reads data from data.csv and fit from fits/fit_poiss.rds (no re-sampling).
  • Y-axis: log₂(TOP/BOTTOM). Output: results/INITIALS_TOP_INITIALS_BOTTOM.png
  • X labels rotated 45°.
\n")

## ---- CLI ----
args <- commandArgs(trailingOnly = TRUE)
if (any(args %in% c("--help","-h"))) { print_help(); quit(status=0) }

o_mode <- NA_character_
if (length(args)) {
  i <- which(args == "-o")
  if (length(i)) {
    if (i == length(args)) stop("Flag -o needs a value (alpha|t|b|tabs|babs). See --help.")
    o_mode <- tolower(args[i+1]); args <- args[-c(i, i+1)]
  }
}
if (!is.na(o_mode) && !o_mode %in% c("alpha","t","b","tabs","babs"))
  stop("Unknown -o mode: ", o_mode, " (use alpha|t|b|tabs|babs). See --help.")

if (length(args) < 2) { print_help(); stop("Need TOP_GROUP and BOTTOM_GROUP.") }
top_req <- args[1]; bot_req <- args[2]
regions_file <- if (length(args) >= 3) args[3] else NA_character_

## ---- load data ----
data <- readr::read_csv("data.csv", show_col_types = FALSE) |>
  dplyr::select(-any_of("...1"))

regions_full <- levels(factor(as.character(data$region)))
groups       <- levels(factor(as.character(data$group)))
Gd <- length(groups); Rfull <- length(regions_full)
stopifnot(Gd >= 2, Rfull >= 1)

## ---- group matching (case/punct insensitive) ----
keys <- canon(groups)
top_idx <- match(canon(top_req), keys)
bot_idx <- match(canon(bot_req), keys)
if (is.na(top_idx) || is.na(bot_idx)) {
  cat("Requested (canon):", canon(top_req), "/", canon(bot_req), "\n")
  cat("Available groups:\n"); print(groups)
  cat("Available (canon):\n"); print(keys)
  stop("Could not match requested groups to data. See --help.")
}
top_label <- groups[top_idx]; bot_label <- groups[bot_idx]

## ---- load fit & parse theta dims ----
fit <- readRDS("fits/fit_poiss.rds")
dm  <- fit$draws("theta", format = "draws_matrix")
if (ncol(dm) == 0) stop("No theta columns in fit.")

m  <- regexec("^theta\\[(\\d+),(\\d+)\\]$", colnames(dm))
mm <- regmatches(colnames(dm), m)
if (any(lengths(mm) < 3)) stop("Couldn't parse theta indices. Example cols: ",
                               paste(head(colnames(dm), 6), collapse = ", "))
i1 <- as.integer(vapply(mm, function(x) x[2], "", USE.NAMES = FALSE))
i2 <- as.integer(vapply(mm, function(x) x[3], "", USE.NAMES = FALSE))
d1 <- max(i1); d2 <- max(i2)

# which index = groups vs regions (use full region count from data)
map <- if (d1 == Gd && d2 == Rfull) "gr" else if (d1 == Rfull && d2 == Gd) "rg" else NA
if (is.na(map)) stop(sprintf("Fit dims theta[%d,%d] don't match data G=%d R=%d.", d1, d2, Gd, Rfull))

## ---- region selection (optional list) ----
if (!is.na(regions_file)) {
  want_raw   <- read_region_list(regions_file)
  want_canon <- canon(want_raw)
  reg_keys   <- canon(regions_full)
  sel_idx    <- match(want_canon, reg_keys)
  if (any(is.na(sel_idx))) {
    missing <- want_raw[is.na(sel_idx)]
    cat("Unmatched regions in file:\n"); print(missing)
    cat("Available regions:\n"); print(regions_full)
    stop("Some regions from file could not be matched.")
  }
  regions_sel <- regions_full[sel_idx]
  sel_idx     <- as.integer(sel_idx)
} else {
  regions_sel <- regions_full
  sel_idx     <- seq_along(regions_full)
}
Rsel <- length(sel_idx)

## ---- rebuild samples[draw, group, region_subset] ----
samples <- array(NA_real_, dim = c(nrow(dm), Gd, Rsel))
if (map == "gr") {
  for (g in seq_len(Gd)) for (j in seq_along(sel_idx)) {
    r <- sel_idx[j]; samples[, g, j] <- dm[, paste0("theta[", g, ",", r, "]")]
  }
} else {
  for (g in seq_len(Gd)) for (j in seq_along(sel_idx)) {
    r <- sel_idx[j]; samples[, g, j] <- dm[, paste0("theta[", r, ",", g, "]")]
  }
}

## ---- Bayesian contrast: log2(top/bottom) per selected region ----
nat_diff <- samples[, top_idx, ] - samples[, bot_idx, ]
log2_fc  <- nat_diff * log2(exp(1))
hdi_mat  <- apply(log2_fc, 2, function(x) HDInterval::hdi(x, 0.95))
bayes_df <- tibble::tibble(
  region = regions_sel,
  b_mean = colMeans(log2_fc),
  b_low  = hdi_mat[1, ],
  b_high = hdi_mat[2, ]
)

## ---- Frequentist contrast: build counts_per_area from counts & void ----
# Treat missing 'void' as 0, and coalesce NAs to 0
data <- data %>% mutate(void = as.numeric(ifelse("void" %in% names(data), void, 0)))
TILE_AREA <- 0.94 * 0.67

sample_data <- data %>%
  dplyr::filter(region %in% regions_sel) %>%
  dplyr::group_by(region, group, id) %>%
  dplyr::summarise(
    counts_sum = sum(counts),
    area_sum   = sum((1 - dplyr::coalesce(void, 0) / 100) * TILE_AREA),
    mu_cpr     = log2(counts_sum / area_sum),
    .groups    = "drop"
  ) %>%
  dplyr::select(region, group, mu_cpr)

stopifnot(all(is.finite(sample_data$mu_cpr)))

run_t <- function(df, g_top, g_bot){
  df_top <- dplyr::filter(df, group == g_top)
  df_bot <- dplyr::filter(df, group == g_bot)
  if (nrow(df_top) == 0 || nrow(df_bot) == 0) {
    return(tibble::tibble(t_mean = NA_real_, t_p = NA_real_, t_low = NA_real_, t_high = NA_real_))
  }
  t <- stats::t.test(df_top$mu_cpr, df_bot$mu_cpr, var.equal = FALSE, paired = FALSE,
                     conf.level = 0.95, alternative = "two.sided")
  tibble::tibble(
    t_mean = mean(df_top$mu_cpr) - mean(df_bot$mu_cpr),
    t_p    = t$p.value,
    t_low  = t$conf.int[1],
    t_high = t$conf.int[2]
  )
}

freq_df <- dplyr::bind_rows(
  lapply(split(sample_data, sample_data$region),
         function(x) dplyr::bind_cols(region = unique(x$region), run_t(x, top_label, bot_label)))
)

## ---- decide x-axis order per -o ----
ord_df <- dplyr::left_join(bayes_df, freq_df, by = "region")
levels_order <- regions_sel
if (!is.na(o_mode)) {
  if (o_mode == "alpha") {
    levels_order <- sort(regions_sel)
  } else if (o_mode == "t") {
    levels_order <- ord_df$region[order(ord_df$t_mean, na.last = TRUE)]
  } else if (o_mode == "b") {
    levels_order <- ord_df$region[order(ord_df$b_mean, na.last = TRUE)]
  } else if (o_mode == "tabs") {
    levels_order <- ord_df$region[order(abs(ord_df$t_mean), na.last = TRUE)]
  } else if (o_mode == "babs") {
    levels_order <- ord_df$region[order(abs(ord_df$b_mean), na.last = TRUE)]
  }
  levels_order <- levels_order[levels_order %in% regions_sel]
}

## ---- assemble long data for plotting ----
plot_df <- dplyr::bind_rows(
  dplyr::transmute(freq_df, region, model = "t-test CI", mean = t_mean, lower = t_low, upper = t_high, p = t_p),
  dplyr::transmute(bayes_df, region, model = "Normal HDI", mean = b_mean, lower = b_low, upper = b_high, p = NA_real_)
) %>%
  dplyr::mutate(region = factor(region, levels = levels_order),
                p = tidyr::replace_na(p, 1))

## ---- plot ----
if (!dir.exists("results")) dir.create("results", recursive = TRUE)
outfile <- file.path("results",
                     paste0(initials(top_label), "_", initials(bot_label), ".png"))

ggplot2::theme_set(ggplot2::theme_classic(base_size = 10))
p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = region, y = mean, group = model, color = model, fill = model)) +
  ggplot2::geom_hline(yintercept = 0, alpha = 0.33, linewidth = 0.25) +
  ggplot2::geom_crossbar(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.33, linewidth = 0.25,
                         position = ggplot2::position_dodge2(), width = 0.75) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::scale_fill_brewer(palette = "Dark2") +
  ggplot2::ylab(paste0("log\u2082(", initials(top_label), "/", initials(bot_label), ")")) +
  ggplot2::xlab("brain region") +
  ggplot2::coord_cartesian(ylim = c(-2, 1)) +
  ggplot2::scale_y_continuous(breaks = seq(-2, 2, by = 1)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))

ggplot2::ggsave(outfile, plot = p, dpi = 600, width = 17.8, height = 17.8/4, units = "cm")
cat("Wrote:", outfile, "\n")
