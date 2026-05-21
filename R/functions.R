# ============================================================
# functions.R — reusable utilities for Case-ID annotation analysis
#
# Source this file at the top of Analyses.R:
#   source(here::here("R", "functions.R"))
# ============================================================

# ---- Misc helpers -----------------------------------------------------------

#' Capitalise only the first character of a string (vectorised)
first_up <- function(x) {
  ifelse(is.na(x), NA_character_,
         paste0(toupper(substr(x, 1, 1)), substring(x, 2)))
}

# ---- Inter-rater agreement --------------------------------------------------

pct_agreement <- function(a, b) mean(a == b, na.rm = TRUE)

confusion_tables <- function(a, b) {
  tab <- table(a, b, useNA = "no")
  list(counts    = tab,
       row_props = prop.table(tab, margin = 1),
       col_props = prop.table(tab, margin = 2))
}

kappa_unweighted <- function(a, b) irr::kappa2(data.frame(a, b), weight = "unweighted")
kappa_weighted   <- function(a, b, w = "squared") irr::kappa2(data.frame(a, b), weight = w)

kripp_alpha_nominal <- function(a, b) irr::kripp.alpha(cbind(a, b), method = "nominal")
kripp_alpha_ordinal <- function(a, b) irr::kripp.alpha(cbind(a, b), method = "ordinal")

mcnemar_safe <- function(a, b) {
  tab <- table(a, b)
  if (all(dim(tab) == c(2L, 2L)))
    list(table = tab, test = suppressWarnings(mcnemar.test(tab)))
  else
    list(table = tab, test = NA)
}

#' Compute agreement metrics for two rater columns in a data frame
#'
#' @param df   data frame / data.table
#' @param a_col,b_col  column names (strings)
#' @param type "nominal" or "ordinal"
compute_metrics <- function(df, a_col, b_col, type = c("nominal", "ordinal")) {
  type <- match.arg(type)
  a <- df[[a_col]]; b <- df[[b_col]]
  pa    <- pct_agreement(a, b)
  kap_u <- kappa_unweighted(a, b)
  kap_w <- if (type == "ordinal") kappa_weighted(a, b, w = "squared") else list(value = NA_real_, se = NA_real_, p.value = NA_real_)
  alpha <- if (type == "ordinal") kripp_alpha_ordinal(a, b) else kripp_alpha_nominal(a, b)
  tibble::tibble(
    n                   = sum(!is.na(a) & !is.na(b)),
    pct_agreement       = pa,
    kappa_unweighted    = kap_u$value,
    kappa_unweighted_se = kap_u$se,
    kappa_unweighted_p  = kap_u$p.value,
    kappa_weighted      = kap_w$value,
    kappa_weighted_se   = kap_w$se,
    kappa_weighted_p    = kap_w$p.value,
    kripp_alpha         = alpha$value
  )
}

# ---- Data loading (shared / de-identified CSVs) -----------------------------

#' Load the impulsivity shared data
#'
#' Reads Data/impulsivity_SM.csv and returns it as a data.table with boolean
#' columns as logical vectors.
#'
#' @param path Full path to the CSV. Defaults to Data/impulsivity_SM.csv
#'   relative to the project root (via here::here).
load_impulsivity_data <- function(path = here::here("data", "impulsivity_SM.csv")) {
  dt <- data.table::setDT(read.csv2(path, stringsAsFactors = FALSE))
  bool_cols <- c("reversibility_p", "statement_p", "exclusion", "dose_relation",
                 "temporality_p", "change", "invariance", "reversibility_n",
                 "statement_n", "innocent_bystander", "underlying_disease")
  dt[, (bool_cols) := lapply(.SD, as.logical), .SDcols = bool_cols]
  dt
}

#' Load and reconstruct the suicidality shared data
#'
#' Reads Data/suicidality_SM.csv, renames columns to match the internal
#' naming convention, applies the same role-transformation pipeline used
#' during original annotation, and derives relevance / rel_category.
#' Returns a data.table with the same structure as `suicide_smq_formatted`
#' in Analyses.R.
#'
#' @param path Full path to the CSV. Defaults to Data/suicidality_SM.csv.
load_suicidality_data <- function(path = here::here("data", "suicidality_SM.csv")) {
  dt <- data.table::setDT(
    read.csv2(path, check.names = FALSE, stringsAsFactors = FALSE)
  )

  # Map CSV column names → internal names
  data.table::setnames(dt,
    old = c("Reversibility+", "Statement+", "Dose-relation", "Temporality+",
            "Reversibility-", "Statement-", "Innocent bystander",
            "Underlying disease", "Annotator assessment",
            "Exclusion", "Change", "Invariance"),
    new = c("reversibility", "statement", "dose_relation", "temporality",
            "reversibility_2", "statement_2", "innocent_bystander",
            "underlying_disease", "annotator_assessment",
            "exclusion", "change", "invariance")
  )

  # Boolean evidence columns: TRUE → non-NA marker; FALSE → NA
  # Downstream code uses !is.na() to detect annotation presence.
  bool_cols <- c("reversibility", "statement", "exclusion", "dose_relation",
                 "temporality", "change", "invariance", "reversibility_2",
                 "statement_2", "innocent_bystander", "underlying_disease")
  for (col in bool_cols) {
    dt[, (col) := ifelse(as.logical(get(col)), TRUE, NA)]
  }

  # Role transformations (mirrors original script)
  dt[, role_backup := role]
  dt[, role := ifelse(role == "suicidal intentions and acts as reactions",
                      "reaction", role)]
  dt[, role := ifelse(role == "suicidal intentions and acts as result of inefficacy",
                      "Inefficacy", role)]
  dt[, role := ifelse(role %in% c("relationship-noncompliance",
                                   "suicidal intentions and acts as a result of interrupting medication"),
                      "Discontinuation", role)]
  dt[, role := ifelse(role == "overdose suicidal intent", "Overdose suicidal", role)]
  dt[, role := ifelse(role == "intentional poisoning of other", "Overdose homicidal", role)]
  dt[, role := ifelse(!is.na(role), first_up(role), role)]
  dt[, role := ifelse(role == "Interruption", "Discontinuation", role)]

  dt[, suspicion := annotator_assessment]
  dt[, suspicion := ifelse(suspicion == "Uninformative", "Unclear", suspicion)]

  dt[, irrelevant_category := data.table::fcase(
    grepl("Overdose",   role), "Event",
    grepl("Medication", role), "Exposure",
    default = "Relationship"
  )]
  dt[, role := ifelse(grepl("Medication", role),
                      first_up(gsub("Medication – ", "", role)), role)]
  dt[, role_specification := data.table::fcase(
    role_backup == "relationship-noncompliance", "Non-adherence",
    grepl("Overdose", role), first_up(gsub("Overdose ", "", role)),
    default = NA_character_
  )]
  dt[, role := ifelse(grepl("Overdose", role), "Overdose", role)]

  dt[, relevance := data.table::fcase(
    role == "Reaction",                           "Relevant",
    role == "Unclear",                            "Not assessable",
    !role %in% c("Reaction", "Unclear"),          "Irrelevant",
    default = NA_character_
  )]
  dt[, rel_category := data.table::fcase(
    relevance == "Relevant",   suspicion,
    relevance == "Irrelevant", irrelevant_category,
    default = NA_character_
  )]
  dt[, rel_category := ifelse(
    rel_category %in% c("Uninformative", "uninformative"), "Unclear", rel_category
  )]
  dt[, role := ifelse(
    rel_category %in% c("Exposure", "Event", "Relationship"), role, NA_character_
  )]

  dt[, .(id = report_id, drug, pt, relevance, rel_category, role, role_specification,
         reversibility_p = reversibility, statement_p = statement,
         exclusion, dose_relation, temporality_p = temporality,
         change, invariance, reversibility_n = reversibility_2,
         statement_n = statement_2, innocent_bystander, underlying_disease)]
}

# ---- Color palettes ---------------------------------------------------------

#' Standard fill colors and patterns for annotation categories
category_aesthetics <- function() {
  list(
    fill_colors = c(
      "suspected"    = "#118811",
      "uninformative" = "gray",
      "precautionary" = "yellow",
      "medication"   = "thistle2",
      "event"        = "navajowhite",
      "relationship" = "lightcoral"
    ),
    fill_patterns = c(
      "suspected"    = "none",
      "precautionary" = "none",
      "uninformative" = "none",
      "medication"   = "crosshatch",
      "event"        = "crosshatch",
      "relationship" = "crosshatch"
    ),
    group_pattern_fill = c("in-Scope" = NA, "out-of-Scope" = "red")
  )
}

#' Full color palette for Sankey / heatmap nodes
sankey_palette <- function() {
  c(
    "Retrieved (SMQ)"  = "palegreen",
    "Relevant"         = "#9ACD32",
    "Irrelevant"       = "#EE2C2C",
    "Not assessable"   = "#707070",

    "Suspected"        = "#008B45",
    "Precautionary"    = "#EEEE00",
    "Unclear"          = "#CDBA96",

    "Event"            = "#FF6A6A",
    "Relationship"     = "saddlebrown",
    "Exposure"         = "#FF6A9A",

    "Overdose"         = "pink",
    "Other impulsivity" = "#436EEE",

    "Homicidal"        = "pink3",
    "Medication error" = "#EE6AA7",
    "Misuse"           = "#8B1C62",
    "Suicidal"         = "#CD00CD",
    "Unclear intent"   = "#8B668B",

    "Discontinuation"  = "#EE9A49",
    "Inefficacy"       = "#FF8247",
    "Bystander"        = "#FF6247",
    "Timing"           = "#FF5247",

    "Shortage"         = "#8B6A2B",
    "Withdrawal"       = "#8B8A2B",
    "Non-adherence"    = "#8B5A2B"
  )
}

# ---- Category bar / pie plots -----------------------------------------------

#' Stacked proportional bar chart of annotation categories, faceted by drug group
#'
#' @param plot_data  data.table with columns: drug, drug_group, Category, Group, N
#' @param aes_list   output of category_aesthetics()
plot_category_bars <- function(plot_data, aes_list) {
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = drug, y = N,
                 fill    = Category,
                 pattern = Category,
                 pattern_fill = Group)
  ) +
    ggpattern::geom_bar_pattern(
      stat = "identity", position = "fill",
      pattern_density = 0.1, pattern_spacing = 0.05,
      pattern_key_scale_factor = 0.6, pattern_color = NA
    ) +
    ggplot2::scale_fill_manual(values = aes_list$fill_colors) +
    ggpattern::scale_pattern_manual(values = aes_list$fill_patterns) +
    ggpattern::scale_pattern_fill_manual(values = aes_list$group_pattern_fill) +
    ggplot2::facet_wrap("drug_group", scales = "free", space = "free_x") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "", x = "Drug", fill = "Category") +
    ggplot2::guides(pattern = "none", pattern_fill = "none")
}

#' Pie chart variant for a single drug group
#'
#' @param plot_data      output of prepare_category_plot_data()
#' @param drug_group_val one of "Positive", "Ambiguous", "Negative"
#' @param aes_list       output of category_aesthetics()
#' @param show_legend    logical; show legend (TRUE for last panel)
plot_category_pie <- function(plot_data, drug_group_val, aes_list, show_legend = FALSE) {
  p <- ggplot2::ggplot(
    plot_data[drug_group == drug_group_val],
    ggplot2::aes(x = drug_group, y = N,
                 fill    = Category,
                 pattern = Category,
                 pattern_fill = Group)
  ) +
    ggpattern::geom_bar_pattern(
      stat = "identity", position = "fill",
      pattern_density = 0.1, pattern_spacing = 0.05,
      pattern_key_scale_factor = 0.6, pattern_color = NA
    ) +
    ggplot2::scale_fill_manual(values = aes_list$fill_colors) +
    ggpattern::scale_pattern_manual(values = aes_list$fill_patterns) +
    ggpattern::scale_pattern_fill_manual(values = aes_list$group_pattern_fill) +
    ggplot2::labs(y = "", x = "Drug", fill = "Category") +
    ggplot2::guides(pattern = "none", pattern_fill = "none") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void()
  if (show_legend) p <- p + ggplot2::theme(legend.position = "bottom")
  p
}

#' Waffle chart of annotation categories
#'
#' @param plot_data  data.table with columns: drug, Category, Group, N
#' @param W          waffle width (squares per row, default 10)
#' @param fill_colors  named colour vector from category_aesthetics()
plot_category_waffle <- function(plot_data, W = 10, fill_colors) {
  waff <- plot_data[N > 0,
                    .(Category = rep(Category, N),
                      drug_group = drug_group[1L]),
                    by = .(drug, Group)]
  waff[, i := seq_len(.N), by = .(drug, Group)]
  waff[, x := (i - 1L) %% W + 1L]
  waff[, y := max((i - 1L) %/% W) - ((i - 1L) %/% W) + 1L, by = .(drug, Group)]

  ggplot2::ggplot(waff, ggplot2::aes(x, y, fill = Category)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::coord_equal() +
    ggplot2::facet_grid(Group ~ drug) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "bottom")
}

# ---- Sankey helpers ---------------------------------------------------------

#' Build link table between adjacent stages for a Sankey diagram
#'
#' @param data    data frame with one column per stage
#' @param stages  character vector of stage column names in order
make_links <- function(data, stages) {
  purrr::map2_dfr(stages[-length(stages)], stages[-1], ~ {
    data |>
      dplyr::count(dplyr::across(dplyr::all_of(c(.x, .y))), name = "value") |>
      dplyr::filter(.data[[.x]] != "(stop)", .data[[.y]] != "(stop)") |>
      dplyr::rename(source_name = 1, target_name = 2)
  })
}

#' Add stage columns (stage0–stage4) to a formatted annotation data frame
#'
#' @param df  data frame with columns: relevance, rel_category, role, role_specification
build_sankey_stages <- function(df) {
  dplyr::mutate(df,
    stage0 = "Retrieved (SMQ)",
    stage1 = dplyr::coalesce(relevance,         "(stop)"),
    stage2 = dplyr::coalesce(rel_category,       "(stop)"),
    stage3 = dplyr::coalesce(role,               "(stop)"),
    stage4 = dplyr::coalesce(role_specification, "(stop)")
  )
}

#' Build a Sankey diagram with plotly from a formatted annotation data frame
#'
#' @param df   data frame / data.table with columns: relevance, rel_category,
#'             role, role_specification, drug (NAs in drug are dropped)
#' @param pal  named colour vector (from sankey_palette())
plot_sankey <- function(df, pal) {
  df_path <- df |>
    dplyr::filter(!is.na(drug)) |>
    build_sankey_stages() |>
    dplyr::select(stage0, stage1, stage2, stage3, stage4) |>
    dplyr::mutate(stage3 = utf8::as_utf8(trimws(stage3)))

  links_named <- make_links(df_path, c("stage0", "stage1", "stage2", "stage3", "stage4"))

  pal_ext <- c(pal, "Other" = "black")
  nodes <- tibble::tibble(
    name  = unique(c(links_named$source_name, links_named$target_name)),
    color = unname(pal_ext[unique(c(links_named$source_name, links_named$target_name))])
  )

  links <- links_named |>
    dplyr::mutate(
      source = match(source_name, nodes$name) - 1L,
      target = match(target_name, nodes$name) - 1L
    )

  plotly::plot_ly(
    type = "sankey",
    node = list(label = nodes$name, color = nodes$color),
    link = list(source = links$source, target = links$target,
                value  = links$value, label  = as.character(links$value))
  )
}

# ---- Heatmap helpers --------------------------------------------------------

#' Prepare proportional heatmap data for drugs or preferred terms
#'
#' @param dt          data.table with columns: <entity_col>, category
#' @param entity_col  "drug" or "pt" (the x-axis entity)
#' @param min_n       minimum total occurrences to include an entity (default 2)
#' @param cat_levels  optional ordered category factor levels
build_heatmap_data <- function(dt, entity_col, min_n = 2, cat_levels = NULL) {
  df <- dt[, .(entity = get(entity_col), category)][!is.na(entity)]
  df <- df[, .N, by = c("entity", "category")]
  df[, value := N / sum(N), by = "entity"]
  df <- df[, .(entity = entity, category, value, n = N)]

  totals     <- df[, .(tot = sum(n)), by = "entity"][order(-tot)]
  keep       <- totals[tot > min_n]$entity
  df         <- df[entity %in% keep]
  df[, entity := factor(entity, levels = keep)]

  if (!is.null(cat_levels))
    df[, category := factor(category, levels = cat_levels)]

  df
}

#' Three-palette heatmap (Non-relevant / Non-assessable / Potential ADR)
#'
#' Produces a ggplot object.  Each category cluster uses its own colour scale
#' via ggnewscale.
#'
#' @param df          data frame from build_heatmap_data(); must have columns
#'                    entity, category, value, n, group (Non-relevant /
#'                    Non-assessable / Potential ADR)
#' @param x_label     x-axis label string
#' @param sep_ymin,sep_ymax  y-intercepts for the two separator lines
#'   (typically 3.5 and 4.5 for the default 7-category ordering)
#' @param pal_red,pal_green,pal_grey  gradient colour vectors
plot_heatmap_detail <- function(df, x_label,
                                sep_ymin = 3.5, sep_ymax = 4.5,
                                pal_red   = c("#fff5f0", "#fcbba1", "#fb6a4a", "#cb181d"),
                                pal_green = c("#f7fcf5", "#bae4b3", "#31a354", "#006d2c"),
                                pal_grey  = c("#f7f7f7", "#cccccc", "#969696")) {
  non_rel <- c("Overdose", "Discontinuation", "Inefficacy", "Bystander", "Batch problem")
  non_ass <- "Non-assessable"

  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data  = df[df$category %in% non_rel, ],
      ggplot2::aes(x = entity, y = category, fill = value),
      color = "white"
    ) +
    ggplot2::scale_fill_gradientn(colours = pal_red,  limits = c(0, 1), name = "Non-relevant") +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data  = df[df$category == non_ass, ],
      ggplot2::aes(x = entity, y = category, fill = value),
      color = "white"
    ) +
    ggplot2::scale_fill_gradientn(colours = pal_grey, limits = c(0, 1), name = "Non-assessable") +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(
      data  = df[df$category %in% c("Precautionary", "Unclear", "Suspected"), ],
      ggplot2::aes(x = entity, y = category, fill = value),
      color = "white"
    ) +
    ggplot2::scale_fill_gradientn(colours = pal_green, limits = c(0, 1), name = "Potential ADR") +
    ggplot2::geom_text(
      data    = df,
      ggplot2::aes(x = entity, y = category, label = ifelse(n > 0, n, "")),
      size = 2, colour = "black"
    ) +
    ggplot2::geom_hline(yintercept = sep_ymin, colour = "grey40", linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = sep_ymax, colour = "grey40", linewidth = 0.7) +
    ggplot2::labs(x = x_label, y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid      = ggplot2::element_blank(),
      legend.box      = "vertical",
      legend.key.height = ggplot2::unit(8,  "pt"),
      legend.key.width  = ggplot2::unit(6,  "pt"),
      legend.spacing.y  = ggplot2::unit(1,  "pt"),
      legend.box.spacing = ggplot2::unit(1, "pt"),
      legend.text     = ggplot2::element_text(size = 8),
      legend.title    = ggplot2::element_text(size = 9)
    )
}
