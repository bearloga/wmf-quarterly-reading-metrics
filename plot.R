#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)
message("\nLoading data and generating plots\n")
today <- args[1]
suppressPackageStartupMessages({
  library(glue)
  library(ggplot2)
})

fig_path <- glue("figures/{today}")

if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}

output_figure <- function(plot, filename, width = 12, height = 6) {
  ggsave(
    plot = plot, filename = paste0(filename, ".png"), path = fig_path,
    width = width, height = height, units = "in", dpi = 300
  )
  message("Plot saved as ", file.path(fig_path, filename), ".png")
}

# Wikipedia.org Portal

## Referrer Breakdown
Data <- readr::read_tsv(glue("data/portal/pageviews-referrer_{today}.tsv"))
p <- ggplot(Data)
output_figure(p, "portal-pageviews-referrer")

## Platform Breakdown
Data <- readr::read_tsv(glue("data/portal/pageviews-platform_{today}.tsv"))
p <- ggplot(Data)
output_figure(p, "portal-pageviews-platform")

# Wikimedia Maps

## Prevalence
Data <- readr::read_tsv(glue("data/maps/prevalence_{today}.tsv"))
p <- ggplot(Data)
output_figure(p, "maps-prevalence")

## Users Stats
Data <- readr::read_tsv(glue("data/maps/kartotherian-users_{today}.tsv"))
p <- ggplot(Data)
output_figure(p, "maps-kartotherian-users")

## Referrer Breakdown
Data <- readr::read_tsv(glue("data/maps/kartotherian-referrers_{today}.tsv"))
p <- ggplot(Data)
output_figure(p, "maps-kartotherian-referrers")
