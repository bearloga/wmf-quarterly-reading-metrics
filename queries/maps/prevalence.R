#!/usr/bin/env Rscript

# Mapframe & Maplink prevalence across wikis
# Phabricator: T171531

args = commandArgs(trailingOnly = TRUE)
message("\nFetching mapframe & maplink prevalence stats\n")
today <- args[1]
suppressPackageStartupMessages({
  library(glue)
})
tsv_path <- glue("data/maps/prevalence_{today}.tsv")

if (!dir.exists(dirname(tsv_path))) {
  dir.create(dirname(tsv_path), recursive = TRUE)
}

enabled <- yaml::yaml.load_file("prevalence.yaml")

wikis <- function(type) {
  if (type == "mapframe") {
    wikis <- c(
      enabled$mapframe$wikipedias,
      enabled$mapframe$miscellaneous,
      setdiff(enabled$maplink$wikivyoages, enabled$mapframe$wikivoyages)
    )
  } else {
    wikis <- unname(unlist(enabled$maplink))
  }
  names(wikis) <- wikis
  return(wikis)
}

prevalence_query <- function(type, wiki) {
  prop_name <- ifelse(type == "maplink", "kartographer_links", "kartographer_frames")
  ns <- ifelse(wiki == "commonswiki", 6, 0)
  query <- glue("SELECT
  COUNT(*) AS total_articles,
  SUM(IF({type}s > 0, 1, 0)) AS instanced_articles,
  SUM(COALESCE({type}s, 0)) AS total_instances
FROM (
  SELECT
    page.page_id,
    pp_value AS {type}s
  FROM (
    SELECT pp_page, pp_value
    FROM page_props
    WHERE pp_propname = '{prop_name}' AND pp_value > 0
  ) AS filtered_props
  RIGHT JOIN page ON page.page_id = filtered_props.pp_page AND page.page_namespace = {ns}
) joined_tables;")
  return()
}

# Fetch data from MySQL database:
results <- do.call(rbind, lapply(c("mapframe", "maplink"), function(type) {
  # Wikis <- wikis(type) # production
  Wikis <- c("frwiki", "frwikivoyage") # development
  results <- dplyr::bind_rows(lapply(Wikis, function(wiki) {
    message("Fetching data for ", type, " on ", wiki)
    result <- tryCatch(
      suppressMessages(wmf::mysql_read(
        prevalence_query(type, wiki),
        wiki
      )),
      error = function(e) {
        return(data.frame())
      }
    )
    return(result)
  }), .id = "wiki")
  return(cbind(feature = type, results))
}))

readr::write_tsv(results, tsv_path)
message("\nData written to ", tsv_path, "\n")
