#!/usr/bin/env Rscript

# Wikipedia.org bounce rate by platform
# Phabricator: T171529

args = commandArgs(trailingOnly = TRUE)
today <- args[1]

message("\nFetching portal events for the 90 days leading up to ", today, "\n")

suppressPackageStartupMessages({
  library(magrittr)
  library(glue)
})

if (!dir.exists("data/portal")) {
  dir.create("data/portal", recursive = TRUE)
}

query <- "SELECT
  timestamp AS ts,
  userAgent AS user_agent,
  event_session_id AS session,
  UPPER(event_country) AS country_code,
  event_destination AS destination,
  event_event_type AS type,
  event_section_used AS section_used
FROM WikipediaPortal_15890769
WHERE
  LEFT(timestamp, 8) = '{condensed_date}'
  AND (
    event_cohort IS NULL
    OR event_cohort IN('null', 'baseline')
  )
  AND event_country != 'US'
  AND event_event_type IN('landing', 'clickthrough');"

null2na <- function(x) {
  return(lapply(x, function(y) {
    if (is.null(y)) {
      return(as.character(NA))
    } else {
      return(y)
    }
  }))
}

results <- do.call(rbind, lapply(
  seq(as.Date(today) - 1, as.Date(today) - 1, by = "day"),
  function(date) {
    message("Fetching data from ", format(date, "%Y-%m-%d"))
    condensed_date <- format(date, "%Y%m%d")
    query <- glue(query, .open = "{", .close = "}")
    result <- wmf::mysql_read(query, "log")
    result$ts <- lubridate::ymd_hms(result$ts)
    user_agents <- purrr::map_df(result$user_agent, ~ null2na(jsonlite::fromJSON(.x, simplifyVector = FALSE)))
    return(cbind(date = date, result[, setdiff(names(result), "user_agent")], user_agents))
  }
))

data("ISO_3166_1", package = "ISOcodes")
countries <- ISO_3166_1[, c("Alpha_2", "Numeric")]; rm(ISO_3166_1)
data("UN_M.49_Regions", package = "ISOcodes")
un_regions <- UN_M.49_Regions; rm(UN_M.49_Regions)

regions <- dplyr::filter(un_regions, Type == "Region")
regions <- strsplit(regions$Children, ", ") %>%
  set_names(regions$Name) %>%
  purrr::map(~ data.frame(Numeric = .x, stringsAsFactors = FALSE)) %>%
  dplyr::bind_rows(.id = "region") %>%
  dplyr::left_join(countries, by = "Numeric")

groupings <- dplyr::filter(un_regions, Type == "Grouping")
groupings <- strsplit(groupings$Children, ", ") %>%
  set_names(groupings$Name) %>%
  purrr::map(~ data.frame(Numeric = .x, stringsAsFactors = FALSE)) %>%
  dplyr::bind_rows(.id = "region") %>%
  dplyr::left_join(countries, by = "Numeric") %>%
  dplyr::rename(grouping = region)

results <- results %>%
  dplyr::left_join(polloi::get_country_state(), by = c("country_code" = "abb")) %>%
  dplyr::rename(country = name) %>%
  dplyr::mutate(
    is_mobile = browser_family %in% c("Opera Mini") | grepl("^Symbian", os_family) |
      os_family %in% c("iOS", "Android", "Firefox OS", "BlackBerry OS", "Chrome OS", "Kindle", "Windows Phone") |
      grepl("(phone)|(mobile)|(tablet)|(lumia)", device_family, ignore.case = TRUE),
    is_US = grepl("^U\\.S\\.", country_code),
    country_code = dplyr::if_else(is_US, "US", country_code)
  ) %>%
  dplyr::left_join(regions[, c("region", "Alpha_2")], by = c("country_code" = "Alpha_2")) %>%
  dplyr::left_join(groupings[, c("grouping", "Alpha_2")], by = c("country_code" = "Alpha_2")) %>%
  dplyr::arrange(date, session, ts) %>%
  dplyr::group_by(date, session) %>%
  dplyr::mutate(valid = any("landing" %in% type)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(valid) %>%
  dplyr::select(-valid)

nth_non_na <- function(x, n) {
  if (sum(!is.na(x)) > 0) {
    if (n == Inf) {
      return(rev(x[!is.na(x)])[1])
    } else {
      return(x[!is.na(x)][n])
    }
  } else {
    return(as.character(NA))
  }
}

sessions <- results %>%
  dplyr::group_by(date, region, grouping, is_mobile, session) %>%
  dplyr::summarize(
    clickthrough = any(type == "clickthrough"),
    last_action = nth_non_na(section_used, Inf),
    first_action = nth_non_na(section_used, 1)
  )

most_common <- function(x) {
  return(names(head(sort(table(x), decreasing = TRUE), 1)))
}

sessions %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::group_by(date, region) %>%
  dplyr::summarize(
    sessions = n(),
    mobile = sum(is_mobile),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = 1 - ctr,
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  readr::write_tsv(glue("data/portal/regions_{today}.tsv"))

sessions %>%
  dplyr::filter(!is.na(grouping)) %>%
  dplyr::group_by(date, grouping) %>%
  dplyr::summarize(
    sessions = n(),
    mobile = sum(is_mobile),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = 1 - ctr,
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  readr::write_tsv(glue("data/portal/groupings_{today}.tsv"))

sessions %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    sessions = n(),
    mobile = sum(is_mobile),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = 1 - ctr,
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  readr::write_tsv(glue("data/portal/overall_{today}.tsv"))

sessions %>%
  dplyr::mutate(device = dplyr::if_else(is_mobile, "mobile", "desktop")) %>%
  dplyr::group_by(date, device) %>%
  dplyr::summarize(
    sessions = n(),
    ctr = round(sum(clickthrough)/n(), 4),
    bounce_rate = 1 - ctr,
    first_action = most_common(first_action),
    last_action = most_common(last_action)
  ) %>%
  readr::write_tsv(glue("data/portal/device_{today}.tsv"))
