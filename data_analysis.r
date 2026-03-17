# data_analysis.r

## Calculate publication tabular summary values ------------------------
top_cited_publications <- crossref_data |>
    arrange(desc(is.referenced.by.count)) |>
    slice_head(n = 5) |>
    select(url, is.referenced.by.count)
top_if_journals <- journal_metrics |>
    arrange(desc(mean_citedness_2yr)) |>
    slice_head(n = 5) |>
    select(display_name, mean_citedness_2yr)
total_citations_publications <- sum(mutate(crossref_data, is.referenced.by.count = as.numeric(is.referenced.by.count))$is.referenced.by.count)
total_journals <- length(unique(journal_metrics$display_name))
write_csv(top_cited_publications, "outputs/top_cited_publications.csv")
write_csv(top_if_journals, "outputs/top_if_journals.csv")
write_csv(
    tibble(total_citations_publications, total_journals),
    "outputs/summary_stats.csv"
)

## Visualize publications ------------------------
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
publications_figure <- ggarrange(
    plot_publications_by_year, plot_oa_breakdown,
    plot_lit_type, plot_top_journals,
    ncol = 2, nrow = 2,
    labels = "AUTO"
)
ggsave("outputs/publications_figure.png", publications_figure, width = 16, height = 12, dpi = 300)

# Load Shapefiles ---------------------------
belize_map <- st_read("shapefiles/Belize_Basemap.shp") %>%
    st_transform(4326)
protected_map <- st_read("shapefiles/ProtectedAreas_2024.shp") %>%
    st_transform(4326)

## Visualize species occurrence density ------------------------
grid_size <- 0.1
occ_grid_sf <- occ_grid |>
    mutate(geometry = purrr::pmap(list(lon, lat), function(x, y) {
        st_polygon(list(rbind(
            c(x, y), c(x + grid_size, y), c(x + grid_size, y + grid_size),
            c(x, y + grid_size), c(x, y)
        )))
    })) |>
    st_as_sf(crs = 4326)
occ_grid_clipped <- st_filter(occ_grid_sf, st_make_valid(belize_map))

plot_occ_density <- ggplot() +
    geom_sf(data = occ_grid_clipped, aes(fill = count), color = NA) +
    geom_sf(data = belize_map, linewidth = 1, color = "black", fill = NA) +
    scale_fill_gradientn(
        colours = palette_cont,
        name = "Occurrence Density",
        trans = "log10",
        breaks = scales::log_breaks(n = 6),
        labels = scales::comma
    ) +
    geom_sf(data = protected_map, linewidth = 0.25, color = "black", fill = "grey", alpha = 0.3) +
    annotation_scale(location = "br", width_hint = 0.2, style = "ticks") +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
    theme_minimal() +
    custom_theme
ggsave("outputs/occurrence_density_map.png", plot_occ_density, width = 10, height = 14, dpi = 300)
