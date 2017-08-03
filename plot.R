#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)
message("\nLoading data and generating plots\n")
today <- args[1]
suppressPackageStartupMessages({
  library(glue)
  library(magrittr)
  library(ggplot2)
  library(viridis)
})

fig_path <- glue("figures/{today}")

if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}

theme_min <- function(legend.position = "bottom", ...) {
  ggplot2::theme_minimal(
      base_size = 14,
      base_family = "Source Sans Pro"
    ) +
    ggplot2::theme(legend.position = legend.position, ...)
}

subtitle <- function(x) {
  return(paste0(
    "Data from ",
    format(min(x$date), "%d %B %Y"),
    " to ",
    format(max(x$date), "%d %B %Y"),
    " (", as.numeric(max(x$date) - min(x$date)), " days)"
  ))
}

output_figure <- function(plot, filename, legend.position = "bottom", width = 12, height = 6, ...) {
  # return(plot + theme_min())
  ggsave(
    plot = plot + theme_min(legend.position, ...),
    filename = paste0(filename, ".png"), path = fig_path,
    width = width, height = height, units = "in", dpi = 300
  )
  message("Plot saved as ", file.path(fig_path, filename), ".png")
}

# Wikipedia.org Portal

## EventLogging-based Metrics
Data <- readr::read_tsv(glue("data/portal/overall_{today}.tsv"))
Data %>%
  dplyr::mutate(approx = sessions * 200) %>%
  dplyr::summarize(approx = median(approx))

### Device
Data <- readr::read_tsv(glue("data/portal/device_{today}.tsv"))
p <- ggplot(Data, aes(x = date, y = ctr, color = device)) +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%a\n%d %b", date_minor_breaks = "1 week") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  scale_color_brewer("Device", palette = "Set1") +
  facet_wrap(~ device) +
  labs(
    title = "Wikipedia.org Portal clickthrough rate by device",
    subtitle = subtitle(Data),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  )
output_figure(p, "portal-engagement-device")

Data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = sessions/sum(sessions)) %>%
  dplyr::group_by(device) %>%
  dplyr::summarize(prop = median(prop))

p <- ggplot(Data, aes(x = date, y = sessions, fill = device)) +
  geom_area(position = "fill", color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%a\n%d %b", date_minor_breaks = "1 week") +
  scale_y_continuous("% of tracked sessions", labels = scales::percent_format()) +
  scale_fill_brewer("Device", palette = "Set1") +
  labs(
    title = "Proportion of Wikipedia.org Portal visitors on a mobile device vs a desktop",
    subtitle = subtitle(Data),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  )
output_figure(p, "portal-sessions-device")

### U.S. vs the world
Data <- readr::read_tsv(glue("data/portal/us-vs-world_{today}.tsv")) %>%
  dplyr::mutate(region = dplyr::if_else(region == "United States", region, "Everywhere else"))
p <- ggplot(Data, aes(x = date, y = bounce_rate, color = region)) +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%a\n%d %b", date_minor_breaks = "1 week") +
  scale_y_continuous("Bounce rate", labels = scales::percent_format()) +
  scale_color_brewer("Region", palette = "Set1") +
  labs(
    title = "Wikipedia.org Portal visitors' bounce rate in U.S. vs everywhere else",
    subtitle = subtitle(Data)
  )
output_figure(p, "portal-engagement-us")
p <- ggplot(Data, aes(x = date, y = mobile/sessions, color = region)) +
  geom_line() +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  facet_wrap(~ region) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%a\n%d %b", date_minor_breaks = "1 week") +
  scale_y_continuous("Proportion of visitors on a mobile device", labels = scales::percent_format()) +
  scale_color_brewer("Location", palette = "Set1") +
  labs(
    title = "Proportion of Wikipedia.org Portal visitors on mobile devices in U.S. vs everywhere else",
    subtitle = subtitle(Data),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  )
output_figure(p, "portal-engagement-us")

Data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = sessions/sum(sessions)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(prop = median(prop))

### Groupings
Data <- readr::read_tsv(glue("data/portal/groupings_{today}.tsv"))
p <- ggplot(Data, aes(x = date, y = bounce_rate, color = grouping)) +
  geom_line(alpha = 0.75) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_x_date("Date", date_breaks = "2 weeks", date_labels = "%d\n%b", date_minor_breaks = "1 week") +
  scale_y_continuous("Bounce rate", labels = scales::percent_format()) +
  scale_color_brewer("Grouping", palette = "Set1") +
  facet_wrap(~ grouping) +
  labs(
    title = "Wikipedia.org Portal bounce rate in developing countries and States",
    subtitle = subtitle(Data),
    caption = "Economic grouping via M49 standard from United Nations Statistics Division"
  )
output_figure(p, "portal-engagement-groupings")

## Referrer Breakdown
Data <- readr::read_tsv(glue("data/portal/pageviews-referrer_{today}.tsv"))
p <- ggplot(Data, aes(x = date, y = pageviews, color = referrer)) +
  # geom_area(position = "stack") +
  geom_line(alpha = 0.75) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 9), se = FALSE) +
  scale_x_date("Date", date_breaks = "1 month", date_labels = "%b\n'%y", date_minor_breaks = "1 month") +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer", palette = "Set1") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Wikipedia.org Portal pageviews",
    subtitle = subtitle(Data)
  )
output_figure(p, "portal-pageviews-referrer", width = 5.3 * 1.5, height = 3.4 * 1.5)

## Platform Breakdown
Data <- readr::read_tsv(glue("data/portal/pageviews-platform_{today}.tsv"))
p <- ggplot(Data, aes(x = date, y = pageviews, fill = device)) +
  geom_area(position = "stack") +
  scale_x_date("Date", date_breaks = "1 week", date_labels = "%a\n%d %b") +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_fill_brewer("Device", palette = "Set1") +
  labs(
    title = "Wikipedia.org Portal pageviews by device",
    subtitle = subtitle(Data),
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  )
output_figure(p, "portal-pageviews-platform")

Data %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = pageviews/sum(pageviews)) %>%
  dplyr::group_by(device) %>%
  dplyr::summarize(prop = median(prop), pvs = median(pageviews))

## Region Breakdown
Data <- readr::read_tsv(glue("data/portal/regions_{today}.tsv")) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(prop = sessions/sum(sessions)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(
    sessions = median(sessions),
    mobile = median(mobile/sessions),
    ctr = median(ctr),
    bounce_rate = median(bounce_rate),
    mobile = dplyr::if_else(mobile == 1.0, as.numeric(NA), mobile),
    prop = median(prop)
  ) %>%
  dplyr::filter(sessions > 10)
na_rm <- function(x) {
  return(dplyr::if_else(x == "NA%", "", x))
}
p <- ggplot(Data, aes(x = reorder(region, -sessions), y = sessions, fill = mobile)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = na_rm(sprintf("%.1f%% of sessions", 100 * prop)), hjust = "left"), nudge_y = 10) +
  scale_y_continuous("Sessions", labels = polloi::compress) +
  scale_fill_viridis("% on mobile", labels = scales::percent_format(), option = "inferno") +
  coord_flip() +
  labs(
    title = "Where Wikipedia.org Portal visitors come from & what they use", x = NULL,
    subtitle = "Median across the past 90 days of data",
    caption = "This is an approximation, as there is no m.wikipedia.org and we had to create rules for detecting mobile device from user agents."
  )
output_figure(p, "portal-sessions-region", legend.position = "right", legend.key.height = unit(0.8, "in"))

Data %>%
  dplyr::mutate(
    is_asia = grepl("Asia$", region),
    is_africa = grepl("Africa$", region),
    is_asia_or_africa = is_asia | is_africa
  ) %>%
  dplyr::group_by(is_asia_or_africa) %>%
  dplyr::summarize(sessions = sum(sessions)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(prop = sessions/sum(sessions))

p <- ggplot(Data, aes(x = reorder(region, -bounce_rate), y = bounce_rate, fill = log10(sessions))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous("Bounce rate", labels = scales::percent_format()) +
  scale_fill_viridis("Sessions", labels = function(x) {
    return(polloi::compress(10^x))
  }) +
  coord_flip() +
  labs(
    title = "Where we're losing Wikipedia.org Portal visitors", x = NULL,
    subtitle = "Median across the past 90 days of data"
  )
output_figure(p, "portal-engagement-region", legend.key.width = unit(0.8, "in"))

# Wikimedia Maps

## Prevalence
lang_proj <- polloi::get_langproj() %>%
  dplyr::mutate(name = dplyr::if_else(is.na(language), project, paste(language, project)))
Data <- readr::read_tsv(glue("data/maps/prevalence_{today}.tsv")) %>%
  dplyr::mutate(prevalence = instanced_articles/total_articles) %>%
  dplyr::left_join(lang_proj, by = c("wiki" = "wikiid"))
Data %>%
  dplyr::filter(feature == "mapframe") %>%
  dplyr::top_n(5, prevalence) %>%
  dplyr::arrange(desc(prevalence)) %>%
  dplyr::select(Wiki = name, Prevalence = prevalence) %>%
  dplyr::mutate(Prevalence = sprintf("%.1f%%", 100 * Prevalence)) %>%
  knitr::kable(format = "html")
Data %>%
  dplyr::filter(feature == "maplink") %>%
  dplyr::top_n(5, prevalence) %>%
  dplyr::arrange(desc(prevalence)) %>%
  dplyr::select(Wiki = name, Prevalence = prevalence) %>%
  dplyr::mutate(Prevalence = sprintf("%.1f%%", 100 * Prevalence)) %>%
  knitr::kable(format = "html")

## Referrer Breakdown
Data <- readr::read_tsv(glue("data/maps/kartotherian-referrers_{today}.tsv")) %>%
  dplyr::mutate(referrer = factor(referrer, levels = c(
    "Wikivoyage", "Pokemon Go fansite", "Wikipedia",
    "Wikimedia Labs", "Wikidata Query Service",
    "Other Wikimedia projects", "Other"
  )))
p <- ggplot(Data, aes(x = date, y = tiles, fill = referrer)) +
  geom_area(position = "stack") +
  geom_vline(
    xintercept = as.Date("2017-07-23"),
    linetype = "dashed"
  ) +
  scale_x_date("Date", date_breaks = "1 week", date_labels = "%a\n%d %b") +
  scale_fill_manual("Referrer (top to bottom)", values = c(
    "Wikivoyage" = "black", "Pokemon Go fansite" = "#E41A1C", "Wikipedia" = "#377EB8",
    "Wikimedia Labs" = "#4DAF4A", "Wikidata Query Service" = "#984EA3",
    "Other Wikimedia projects" = "#FF7F00", "Other" = "#A65628"
  )) +
  scale_y_continuous("Tiles served", labels = polloi::compress) +
  labs(
    title = "Users of Wikimedia Maps' Kartotherian tile service",
    subtitle = subtitle(Data),
    caption = "The top 3 Pokemon Go users of tiles from Kartotherian are: pkget.com, pkmtracker.com, and instances of PoGoMap tool
    There was a live event in Chicago on July 22 (23rd in UTC) called \"Pokemon Go Fest\""
  )
output_figure(p, "maps-kartotherian-referrers", legend.position = "right")
