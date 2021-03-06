#!/usr/bin/env Rscript

# Wikipedia.org PVs by referrer
# Phabricator: T171529

args = commandArgs(trailingOnly = TRUE)
today <- args[1]

message("\nFetching portal pageviews referrer data leading up to ", today, "\n")

suppressPackageStartupMessages({
  library(magrittr)
  library(glue)
})

tsv_path <- glue("data/portal/pageviews-referrer_{today}.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

suppressWarnings(
  pageviews <- readr::read_tsv(
    "/srv/published-datasets/discovery/metrics/portal/referer_data.tsv",
    col_types = "Dlcci"
  )
  # pageviews <- polloi::read_dataset("discovery/metrics/portal/referer_data.tsv", col_types = "Dlcci")
)

results <- pageviews %>%
  dplyr::filter(referer_class != "unknown", date > "2016-03-06") %>%
  dplyr::mutate(referer_class = forcats::fct_recode(
    referer_class,
    `Referred by something other than search engine` = "external",
    `Referred by a search engine` = "external (search engine)",
    `Referred internally (itself or a sister wiki)` = "internal",
    `Direct (not referred by anything)` = "none"
  )) %>%
  dplyr::group_by(date, referrer = referer_class) %>%
  dplyr::summarize(pageviews = sum(pageviews)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(referrer), !is.na(pageviews)) %>%
  dplyr::distinct(date, referrer, .keep_all = TRUE) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(proportion = round(100 * (pageviews/sum(pageviews)), 3))

readr::write_tsv(results, tsv_path)
message("\nData written to ", tsv_path, "\n")
