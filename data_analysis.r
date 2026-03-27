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

## Calculate unique species per taxon ------------------------
taxa <- c(
    birds = 212, plants = 6, fungi = 5,
    animalia = 1, insects = 216,
    mammals = 359, reptiles = 358,
    molluscs = 52, amphibians = 131, arachnids = 367
)
species_counts <- sapply(taxa, function(key) {
    res <- occ_search(
        taxonKey = key,
        country = "BZ",
        limit = 0,
        facet = "speciesKey",
        facetLimit = 100000
    )
    nrow(res$facets$speciesKey)
})
write_csv(
    cbind(group = rownames(as.data.frame(species_counts)), as.data.frame(species_counts)),
    "outputs/species_counts.csv"
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

# Visualize Literature ---------------------------
top_keywords <- tibble(keywords = records$keywords) |>
    unnest_longer(keywords) |>
    filter(!is.na(keywords), str_squish(keywords) != "") |>
    mutate(keywords = str_to_sentence(str_squish(keywords))) |>
    filter(str_to_lower(keywords) != "gbif") |>
    (\(df) {
        all_lower <- str_to_lower(unique(df$keywords))
        df |> mutate(keywords = ifelse(
            !str_ends(str_to_lower(keywords), "s") &
                paste0(str_to_lower(keywords), "s") %in% all_lower,
            str_to_sentence(paste0(str_to_lower(keywords), "s")),
            keywords
        ))
    })() |>
    count(keywords, sort = TRUE) |>
    slice_head(n = 15)
plot_top_keywords <- ggplot(top_keywords, aes(x = reorder(keywords, n), y = n)) +
    geom_col(fill = palette[1]) +
    coord_flip() +
    labs(x = NULL, y = "Publications") +
    theme_pubclean() +
    custom_theme
top_countries <- tibble(country = records$countriesOfResearcher) |>
    unnest_longer(country) |>
    filter(!is.na(country)) |>
    mutate(country_name = countrycode(country, origin = "iso2c", destination = "country.name")) |>
    filter(!is.na(country_name)) |>
    count(country_name, sort = TRUE) |>
    slice_head(n = 10)
plot_top_countries <- ggplot(top_countries, aes(x = reorder(country_name, n), y = n)) +
    geom_col(fill = palette[2]) +
    coord_flip() +
    labs(x = NULL, y = "Publications") +
    theme_pubclean() +
    custom_theme
central_america_caribbean <- c(
    "MX", "BZ", "GT", "HN", "SV", "NI", "CR", "PA",
    "CU", "JM", "HT", "DO", "TT", "BB", "BS", "GD",
    "LC", "VC", "AG", "DM", "KN", "PR", "TC", "KY", "VG"
)
regional_global <- tibble(country_lists = records$countriesOfResearcher) |>
    mutate(scope = ifelse(
        sapply(country_lists, function(x) any(x %in% central_america_caribbean)),
        "Regional", "Global"
    )) |>
    count(scope)
plot_regional_global <- ggplot(regional_global, aes(x = scope, y = n, fill = scope)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("Regional" = palette[3], "Global" = palette[4])) +
    labs(x = NULL, y = "Publications") +
    theme_pubclean() +
    custom_theme +
    theme(legend.position = "none")
right_col <- plot_top_countries / plot_regional_global + plot_layout(heights = c(3, 1))
literature_figure <- (plot_top_keywords | right_col) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 22, face = "bold"))
ggsave("outputs/literature_figure.png", literature_figure, width = 16, height = 12, dpi = 300)

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

## Visualize non-bird species occurrence density ------------------------
occ_grid_nonbird <- occ_grid |>
    left_join(occ_grid_birds |> rename(count_birds = count), by = c("lon", "lat")) |>
    mutate(
        count_birds = replace_na(count_birds, 0L),
        count = count - count_birds
    ) |>
    filter(count > 0)

occ_grid_nonbird_sf <- occ_grid_nonbird |>
    mutate(geometry = purrr::pmap(list(lon, lat), function(x, y) {
        st_polygon(list(rbind(
            c(x, y), c(x + grid_size, y), c(x + grid_size, y + grid_size),
            c(x, y + grid_size), c(x, y)
        )))
    })) |>
    st_as_sf(crs = 4326)
occ_grid_nonbird_clipped <- st_filter(occ_grid_nonbird_sf, st_make_valid(belize_map))

shared_count_limits <- c(1, max(occ_grid_clipped$count, occ_grid_nonbird_clipped$count))

plot_occ_density <- ggplot() +
    geom_sf(data = occ_grid_clipped, aes(fill = count), color = NA) +
    geom_sf(data = belize_map, linewidth = 1, color = "black", fill = NA) +
    scale_fill_gradientn(
        colours = palette_cont,
        name = "Occurrence Density",
        trans = "log10",
        limits = shared_count_limits,
        breaks = scales::log_breaks(n = 6),
        labels = scales::comma
    ) +
    geom_sf(data = protected_map, linewidth = 0.25, color = "black", fill = "grey", alpha = 0.3) +
    annotation_scale(location = "br", width_hint = 0.2, style = "ticks") +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
    labs(title = "All taxa") +
    theme_minimal() +
    custom_theme
ggsave("outputs/occurrence_density_map.png", plot_occ_density, width = 10, height = 14, dpi = 300)

plot_occ_density_nonbird <- ggplot() +
    geom_sf(data = occ_grid_nonbird_clipped, aes(fill = count), color = NA) +
    geom_sf(data = belize_map, linewidth = 1, color = "black", fill = NA) +
    scale_fill_gradientn(
        colours = palette_cont,
        name = "Occurrence Density",
        trans = "log10",
        limits = shared_count_limits,
        breaks = scales::log_breaks(n = 6),
        labels = scales::comma
    ) +
    geom_sf(data = protected_map, linewidth = 0.25, color = "black", fill = "grey", alpha = 0.3) +
    annotation_scale(location = "br", width_hint = 0.2, style = "ticks") +
    annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
    labs(title = "Non-bird taxa") +
    theme_minimal() +
    custom_theme
ggsave("outputs/occurrence_density_map_nonbird.png", plot_occ_density_nonbird, width = 10, height = 14, dpi = 300)

plot_occ_density_combined <- (plot_occ_density | plot_occ_density_nonbird) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "A") &
    theme(legend.position = "right", plot.tag = element_text(size = 22, face = "bold"))
ggsave("outputs/occurrence_density_map_combined.png", plot_occ_density_combined, width = 18, height = 14, dpi = 300)

## Packed bubble chart: taxonomic groups by observation count ------------------------
species_counts_df <- read_csv("outputs/species_counts.csv") |>
    filter(group != "animalia") |>
    mutate(species_counts = as.numeric(species_counts))
bubble_data <- tibble(
    group     = c(names(taxa_obs_counts), "bony_fish", "reptiles"),
    obs_count = c(as.integer(taxa_obs_counts), 51401L, 11771L)
) |>
    left_join(species_counts_df, by = "group") |>
    filter(!is.na(obs_count), obs_count > 0) |>
    mutate(
        display_name = dplyr::recode(group,
            birds = "Birds", plants = "Plants", fungi = "Fungi",
            insects = "Insects", mammals = "Mammals", reptiles = "Reptiles",
            molluscs = "Molluscs", amphibians = "Amphibians",
            bony_fish = "Bony fish", arachnids = "Arachnids"
        ),
        species_counts = replace_na(species_counts, 0),
        border_type = ifelse(group %in% c("bony_fish", "reptiles"), "dashed", "solid"),
        fill_color = dplyr::recode(group,
            birds = palette[1], plants = palette[3], fungi = palette[8],
            insects = palette[6], mammals = palette[2], reptiles = palette[7],
            molluscs = palette[9], amphibians = palette[4], bony_fish = palette[5],
            arachnids = palette[10]
        )
    ) |>
    arrange(desc(obs_count))
build_bubble_polygons <- function(df, n_vertices = 100) {
    layout <- packcircles::circleProgressiveLayout(df$obs_count, sizetype = "area")
    polys <- packcircles::circleLayoutVertices(layout, npoints = n_vertices, sizetype = "radius")
    polys <- left_join(polys, df |> mutate(id = row_number()), by = "id")
    centroids <- as_tibble(layout) |>
        mutate(id = row_number()) |>
        left_join(df |> mutate(id = row_number()), by = "id")
    list(polys = polys, centroids = centroids)
}
bubble_all <- build_bubble_polygons(bubble_data)
bubble_nobird <- build_bubble_polygons(bubble_data |> filter(group != "birds"))
bubble_fill_scale <- scale_fill_manual(
    name   = "Taxon",
    values = setNames(bubble_data$fill_color, bubble_data$group),
    labels = setNames(bubble_data$display_name, bubble_data$group),
    drop   = FALSE
)
bubble_lw_scale <- scale_linewidth_continuous(
    name   = "Species richness",
    range  = c(0.5, 4),
    labels = scales::comma,
    breaks = c(100, 500, 1500, 3500, 6000)
)
bubble_linetype_scale <- scale_linetype_manual(
    values = c(solid = "solid", dashed = "dashed"),
    guide  = "none"
)
bubble_theme <- theme_void() + theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(
        size = 22, face = "bold", hjust = 0,
        margin = margin(b = 10)
    ),
    plot.margin = margin(10, 10, 10, 10)
)
make_bubble_panel <- function(bubble, title, label_size = 8, show_fill_legend = TRUE) {
    centroids_labeled <- bubble$centroids |>
        mutate(txt_size = pmax(2.5, label_size * (radius / max(radius))^0.35)) |>
        filter(radius > max(radius) * 0.20) |>
        mutate(label = paste0(
            display_name, "<br>", scales::comma(obs_count),
            ifelse(species_counts > 0,
                paste0(
                    "<br><span style='font-size:", round(txt_size * 2.845 * 0.65, 1), "pt'><i>(",
                    scales::comma(species_counts), ")</i></span>"
                ),
                ""
            )
        ))
    p <- ggplot() +
        geom_polygon(
            data = bubble$polys,
            aes(
                x = x, y = y, group = id, fill = group,
                linewidth = species_counts, linetype = border_type
            ),
            color = "grey20"
        ) +
        ggtext::geom_richtext(
            data = centroids_labeled,
            aes(x = x, y = y, label = label, size = txt_size),
            lineheight = 0.9, fontface = "bold", color = "white",
            fill = NA, label.color = NA
        ) +
        bubble_fill_scale +
        bubble_lw_scale +
        bubble_linetype_scale +
        scale_size_identity(guide = "none") +
        coord_equal() +
        labs(title = title) +
        bubble_theme +
        guides(linewidth = "none", fill = if (show_fill_legend) "legend" else "none")
    p
}
plot_bubble_all <- make_bubble_panel(bubble_all, "A", show_fill_legend = TRUE)
plot_bubble_nobird <- make_bubble_panel(bubble_nobird, "B", show_fill_legend = FALSE)
bubble_combined <- (plot_bubble_all | plot_bubble_nobird) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")
ggsave("outputs/bubble_chart_taxa.png", bubble_combined, width = 18, height = 10, dpi = 300)
