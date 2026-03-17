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

## Save tabular outputs ------------------------
write_csv(top_cited_publications, "outputs/top_cited_publications.csv")
write_csv(top_if_journals, "outputs/top_if_journals.csv")
write_csv(
    tibble(total_citations_publications, total_journals),
    "outputs/summary_stats.csv"
)

## Visualize publications per year ------------------------
publications_by_year <- df_records |>
    filter(!is.na(year)) |>
    group_by(year) |>
    summarise(n_publications = n())

plot_publications_by_year <- ggplot(publications_by_year, aes(x = as.character(year), y = n_publications)) +
    geom_col(fill = palette[2]) +
    labs(
        x = "Year",
        y = "Publications"
    ) +
    theme_pubclean() +
    custom_theme

## Visualize open access breakdown ------------------------
oa_breakdown <- df_records |>
    mutate(open_access = ifelse(openAccess, "Open access", "Closed access")) |>
    count(open_access)

plot_oa_breakdown <- ggplot(oa_breakdown, aes(x = open_access, y = n, fill = open_access)) +
    geom_col() +
    scale_fill_manual(values = palette[c(1, 3)]) +
    labs(
        x = NULL,
        y = "Publications"
    ) +
    theme_pubclean() +
    custom_theme +
    theme(legend.position = "none")

## Visualize literature type breakdown ------------------------
lit_type_breakdown <- df_records |>
    filter(!is.na(literatureType), literatureType != "GENERIC") |>
    count(literatureType) |>
    mutate(literatureType = stringr::str_to_title(literatureType)) |>
    arrange(desc(n))

plot_lit_type <- ggplot(lit_type_breakdown, aes(x = reorder(literatureType, n), y = n)) +
    geom_col(fill = palette[4]) +
    labs(
        x = NULL,
        y = "Publications"
    ) +
    theme_pubclean() +
    custom_theme

## Visualize top journals by Publications ------------------------
top_journals_by_pubs <- df_records |>
    filter(!is.na(source)) |>
    count(source) |>
    arrange(desc(n)) |>
    slice_head(n = 10)

plot_top_journals <- ggplot(top_journals_by_pubs, aes(x = reorder(source, n), y = n)) +
    geom_col(fill = palette[5]) +
    coord_flip() +
    labs(
        x = NULL,
        y = "Publications"
    ) +
    theme_pubclean() +
    custom_theme

## Combine plots into 2x2 figure ------------------------
publications_figure <- ggarrange(
    plot_publications_by_year, plot_oa_breakdown,
    plot_lit_type, plot_top_journals,
    ncol = 2, nrow = 2,
    labels = "AUTO"
)

ggsave("outputs/publications_figure.png", publications_figure, width = 16, height = 12, dpi = 300)
