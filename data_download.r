# data_download.r

## Set organization key for UB-ERI ------------------------
org_key <- "8e3e7b8f-88de-4be0-bdcf-f4524171313a"

## Define function to fetch GBIF literature records for an organization ------------------------
fetch_gbif_literature <- function(org_key, limit = 300, offset = 0) {
    url <- paste0(
        "https://api.gbif.org/v1/literature/search",
        "?publishingOrganizationKey=", org_key,
        "&limit=", limit,
        "&offset=", offset
    )
    jsonlite::fromJSON(url, simplifyVector = TRUE)
}

## Fetch GBIF literature records for an organization ------------------------
gbif_lit_records_path <- "data/gbif_literature_records.rds"
if (!file.exists(gbif_lit_records_path)) {
    message("Fetching from GBIF API...")
    result <- fetch_gbif_literature(org_key)
    records <- result$results
    saveRDS(records, gbif_lit_records_path)
    message("Saved to ", gbif_lit_records_path)
} else {
    message("Loading cached records from file...")
    records <- readRDS(gbif_lit_records_path)
}

## Pull DOIs from GBIF data ------------------------
df_records <- records |>
    rowwise() |>
    mutate(doi = {
        ids <- identifiers
        if (is.data.frame(ids) && "doi" %in% names(ids) && nrow(ids) > 0) {
            ids$doi[1]
        } else {
            NA_character_
        }
    }) |>
    ungroup() |>
    select(title, year, source, publisher, peerReview, openAccess, literatureType, doi) |>
    filter(!is.na(doi))

## Define function to fetch CrossRef data for a vector of DOIs ------------------------
fetch_crossref_data <- function(dois) {
    cr_works(doi = dois)$data %>%
        select(url, is.referenced.by.count, issn, type)
}

## Fetch CrossRef data ------------------------
crossref_data_path <- "data/crossref_data.rds"
if (!file.exists(crossref_data_path)) {
    message("Fetching from CrossRef API...")
    crossref_data <- fetch_crossref_data(df_records$doi)
    saveRDS(crossref_data, crossref_data_path)
    message("Saved to ", crossref_data_path)
} else {
    message("Loading cached CrossRef data from file...")
    crossref_data <- readRDS(crossref_data_path)
}

## Define function to fetch journal metrics from OpenAlex by ISSN ------------------------
fetch_journal_metrics <- function(issns) {
    issns_clean <- unique(na.omit(unlist(issns)))
    issns_clean <- trimws(issns_clean)
    purrr::map_dfr(issns_clean, function(issn) {
        result <- tryCatch(
            oa_fetch(entity = "sources", issn = issn, verbose = FALSE),
            error = function(e) NULL
        )
        if (is.null(result) || nrow(result) == 0) {
            return(NULL)
        }
        result %>%
            mutate(
                h_index = purrr::map_dbl(summary_stats, "h_index"),
                i10_index = purrr::map_dbl(summary_stats, "i10_index"),
                mean_citedness_2yr = purrr::map_dbl(summary_stats, "2yr_mean_citedness")
            ) %>%
            select(issn_l, display_name, cited_by_count, works_count, h_index, i10_index, mean_citedness_2yr)
    })
}

## Fetch journal metrics from OpenAlex ------------------------
journal_metrics_path <- "data/journal_metrics.rds"
if (!file.exists(journal_metrics_path)) {
    message("Fetching journal metrics from OpenAlex...")
    journal_metrics <- fetch_journal_metrics(crossref_data$issn)
    saveRDS(journal_metrics, journal_metrics_path)
    message("Saved to ", journal_metrics_path)
} else {
    message("Loading cached journal metrics from file...")
    journal_metrics <- readRDS(journal_metrics_path)
}
