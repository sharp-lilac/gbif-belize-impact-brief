# data_analysis.r

## Calculate top 5 most cited publications ------------------------
top_cited_publications <- crossref_data |>
    arrange(desc(is.referenced.by.count)) |>
    slice_head(n = 5) |>
    select(url, is.referenced.by.count)

## Calculate top 5 journals by impact factor (2yr mean citedness) ------------------------
top_if_journals <- journal_metrics |>
    arrange(desc(mean_citedness_2yr)) |>
    slice_head(n = 5) |>
    select(display_name, mean_citedness_2yr)

## Calculate total citations from publications ------------------------
total_citations_publications <- sum(mutate(crossref_data, is.referenced.by.count = as.numeric(is.referenced.by.count))$is.referenced.by.count)

## Calculate total journals ------------------------
total_journals <- length(unique(journal_metrics$display_name))

## Save outputs ------------------------
write_csv(top_cited_publications, "outputs/top_cited_publications.csv")
write_csv(top_if_journals, "outputs/top_if_journals.csv")
write_csv(
    tibble(total_citations_publications, total_journals),
    "outputs/summary_stats.csv"
)
