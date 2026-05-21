# ============================================================
# Analyses.R — Case-ID annotation study
#
# DATA AVAILABILITY
# -----------------
# Two sections require the original (non-public) annotation files:
#   [REQUIRES ORIGINAL DATA]  sections 2 and 3
#
# The remaining sections work from two de-identified shared CSVs:
#   [WORKS WITH SHARED DATA]  sections 4–7
#   Data/impulsivity_SM.csv
#   Data/suicidality_SM.csv
#
# EXTERNAL DEPENDENCY (section 7 only)
#   ATC classification table (ATC_DiAna.csv) — required only for the
#   drug×ATC-class bar chart and for ordering the drug heatmap by ATC class.
#   Set ATC_PATH below if you have this file; otherwise that chart is skipped.
# ============================================================

# ---- 0. Libraries & functions -----------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(purrr)
library(irr)
library(psych)
library(vcd)
library(writexl)
library(data.table)
library(ggplot2)
library(ggpattern)
library(ComplexUpset)
library(patchwork)
library(plotly)
library(ggnewscale)
library(scales)
library(utf8)

source(here::here("R", "functions.R"))

# ---- 1. Constants -----------------------------------------------------------

scope_levels <- c(
  "in-scope",
  "medication",
  "event",
  "relationship",
  "relationship-inefficacy",
  "relationship-innocent bystander",
  "relationship-withdrawal",
  "relationship-impossible timing",
  "relationship-noncompliance",
  "relationship-shortage"
)

relevance_levels <- c("precautionary", "uninformative", "suspected")

key_cols <- c("report_id", "drug")

# Path to original (non-public) annotation folder — edit before running sec. 2–3
ANNOTATION_PATH <- here::here("25_12_10_annotations")

# Path to ATC classification table — optional, needed only in section 7
# Set to NULL to skip ATC-dependent plots
ATC_PATH <- "/Users/MicheleF/Desktop/DiAna/external_sources/ATC_DiAna.csv"

# ==============================================================================
# 2. [REQUIRES ORIGINAL DATA] Inter-rater agreement
# ==============================================================================
# Needs: <ANNOTATION_PATH>/Annotation_JF.xlsx
#        <ANNOTATION_PATH>/Annotation_MF.xlsx
# (Or the post-adjudication versions, commented below.)
# ==============================================================================

if (FALSE) {   # Change to TRUE when original files are available

  jf_raw <- setDT(read_excel(file.path(ANNOTATION_PATH, "Annotation_JF.xlsx")) %>% clean_names())
  mf_raw <- setDT(read_excel(file.path(ANNOTATION_PATH, "Annotation_MF.xlsx")) %>% clean_names())
  # Post-adjudication versions:
  # jf_raw <- setDT(read_excel(file.path(ANNOTATION_PATH, "Annotation_JF post.xlsx")) %>% clean_names())
  # mf_raw <- setDT(read_excel(file.path(ANNOTATION_PATH, "Annotation_MF post.xlsx")) %>% clean_names())

  # Merge annotators ----
  df_ann <- full_join(
    jf_raw[, .(scope_jf = out_of_scope, relevance_jf = annotator_assessment, report_id, drug)],
    mf_raw[, .(scope_mf = out_of_scope, relevance_mf = annotator_assessment, report_id, drug)],
    by = key_cols
  ) %>%
    mutate(
      scope_jf     = factor(scope_jf,     levels = scope_levels),
      scope_mf     = factor(scope_mf,     levels = scope_levels),
      relevance_jf = factor(relevance_jf, levels = relevance_levels, ordered = TRUE),
      relevance_mf = factor(relevance_mf, levels = relevance_levels, ordered = TRUE)
    )

  # Sanity checks ----
  dups_jf <- jf_raw %>% count(across(all_of(key_cols))) %>% filter(n > 1)
  dups_mf <- mf_raw %>% count(across(all_of(key_cols))) %>% filter(n > 1)
  if (nrow(dups_jf) > 0) warning("JF has duplicate keys — inspect dups_jf.")
  if (nrow(dups_mf) > 0) warning("MF has duplicate keys — inspect dups_mf.")

  # Agreement metrics ----
  metrics_scope <- compute_metrics(df_ann, "scope_jf", "scope_mf", type = "nominal")
  metrics_rel   <- compute_metrics(df_ann, "relevance_jf", "relevance_mf", type = "ordinal")

  metrics_overall <- bind_rows(
    metrics_scope %>% mutate(task = "scope"),
    metrics_rel   %>% mutate(task = "relevance")
  ) %>% select(task, everything())

  print(metrics_overall)

  # Confusion matrices ----
  scope_tabs <- confusion_tables(df_ann$scope_jf, df_ann$scope_mf)
  rel_tabs   <- confusion_tables(df_ann$relevance_jf, df_ann$relevance_mf)
  scope_tabs$counts
  rel_tabs$counts

  # McNemar tests (binary derived variables) ----
  df_ann <- df_ann %>%
    mutate(
      in_scope_jf  = scope_jf     == "in-scope",
      in_scope_mf  = scope_mf     == "in-scope",
      suspected_jf = relevance_jf == "suspected",
      suspected_mf = relevance_mf == "suspected"
    )

  mcn_scope <- mcnemar_safe(df_ann$in_scope_jf,  df_ann$in_scope_mf)
  mcn_susp  <- mcnemar_safe(df_ann$suspected_jf, df_ann$suspected_mf)
  mcn_scope$table; mcn_scope$test
  mcn_susp$table;  mcn_susp$test

  metrics_overall <- setDT(metrics_overall)[
    , mcnemar := c(mcn_scope$test$p.value, mcn_susp$test$p.value)
  ]
  print(metrics_overall %>%
          select(-kripp_alpha, -kappa_unweighted_p, -kappa_weighted_se, -kappa_weighted_p))

  # Disagreement typology ----
  df_ann <- df_ann %>%
    mutate(
      scope_agree     = scope_jf == scope_mf,
      relevance_agree = relevance_jf == relevance_mf,
      scope_disagreement_type = case_when(
        scope_agree                           ~ "agreement",
        is.na(scope_jf) | is.na(scope_mf)    ~ "missing_label",
        TRUE ~ paste0("JF_", scope_jf, "__vs__MF_", scope_mf)
      ),
      relevance_disagreement_type = case_when(
        relevance_agree                                                         ~ "agreement",
        is.na(relevance_jf) | is.na(relevance_mf)                              ~ "missing_label",
        abs(as.integer(relevance_jf) - as.integer(relevance_mf)) == 1          ~ "adjacent_disagreement",
        abs(as.integer(relevance_jf) - as.integer(relevance_mf)) >= 2          ~ "far_disagreement",
        TRUE                                                                    ~ "other"
      ),
      relevance_pair = case_when(
        relevance_agree ~ "agreement",
        TRUE            ~ paste0("JF_", relevance_jf, "__vs__MF_", relevance_mf)
      )
    )

  scope_disagree_summary <- df_ann %>% count(scope_disagreement_type, sort = TRUE)
  rel_disagree_summary   <- df_ann %>% count(relevance_disagreement_type, relevance_pair, sort = TRUE)
  print(scope_disagree_summary)
  print(rel_disagree_summary)

  discordant <- df_ann %>%
    filter(!scope_agree | !relevance_agree) %>%
    select(any_of(key_cols),
           scope_jf, scope_mf, scope_disagreement_type,
           relevance_jf, relevance_mf, relevance_disagreement_type, relevance_pair)

  # Export for adjudication ----
  write_xlsx(
    list(
      overall_metrics             = metrics_overall,
      scope_confusion_counts      = as.data.frame.matrix(scope_tabs$counts),
      relevance_confusion_counts  = as.data.frame.matrix(rel_tabs$counts),
      scope_disagreement_summary  = scope_disagree_summary,
      relevance_disagreement_summary = rel_disagree_summary,
      discordant_cases            = discordant
    ),
    path = "agreement_outputs.xlsx"
  )
  message("Done. Outputs written to: agreement_outputs.xlsx")

  # Spot-check specific disagreement types ----
  innocent_bystander_ids <- discordant[
    scope_disagreement_type == "JF_in-scope__vs__MF_relationship-innocent bystander"
  ]$report_id

  if (interactive()) {
    View(mf_raw[report_id %in% innocent_bystander_ids,
                .(report_id, drug, assessment, text,
                  innocent_bystander, statement_2, annotator_assessment, notes)])
  }
  write_xlsx(
    jf_raw[report_id %in% innocent_bystander_ids,
           .(report_id, drug, assessment, text,
             innocent_bystander, statement_2, annotator_assessment, notes)],
    "Check1_innocent_bystander.xlsx"
  )
  write_xlsx(
    df_ann[!scope_disagreement_type %in% c(
      "JF_in-scope__vs__MF_relationship-innocent bystander", "agreement"
    ), .(report_id, scope_disagreement_type)][
      mf_raw[report_id %in% discordant[
        !scope_disagreement_type %in% c(
          "JF_in-scope__vs__MF_relationship-innocent bystander", "agreement"
        )
      ]$report_id,
      .(report_id, drug, icd_pt, assessment, text, annotator_assessment, notes)],
      on = "report_id"
    ],
    "Check2_different_scope.xlsx"
  )
  write_xlsx(
    df_ann[, .(report_id, relevance_disagreement_type)][
      mf_raw[report_id %in% discordant[
        !relevance_disagreement_type %in% "missing_label"
      ]$report_id],
      on = "report_id"
    ],
    "Check3_different_relevance.xlsx"
  )

}  # end [REQUIRES ORIGINAL DATA] section 2

# ==============================================================================
# 3. [REQUIRES ORIGINAL DATA] Annotation characterisation
# ==============================================================================
# Needs: <ANNOTATION_PATH>/Annotation_solved.xlsx   (adjudicated annotations)
#        <ANNOTATION_PATH>/vigiGrade_VB.csv          (completeness scores)
# ==============================================================================

if (FALSE) {   # Change to TRUE when original files are available

  # 3a. Load solved annotations ----
  Annotation <- setDT(
    read_excel(file.path(ANNOTATION_PATH, "Annotation_solved.xlsx")) %>% clean_names()
  )
  Annotation <- Annotation[is.na(disagreement)]
  Annotation <- copy(Annotation)[
    , Category := ifelse(out_of_scope == "in-scope", annotator_assessment, out_of_scope)
  ][
    , drug := ifelse(drug == "methylphenidate", "Methylphenidate", drug)
  ][
    , Category := ifelse(grepl("relationship", Category), "relationship", Category)
  ][
    , Category := factor(Category,
                         levels = rev(c("suspected", "uninformative", "precautionary",
                                        "medication", "event", "relationship")),
                         ordered = TRUE)
  ][
    , Group := fifelse(Category %in% c("suspected", "uninformative", "precautionary"),
                       "in-Scope", "out-of-Scope")
  ]

  # Print distribution tables ----
  print(Annotation[, .N, by = c("drug", "out_of_scope")][, perc := round(N * 100 / sum(N), 2), by = "drug"])
  print(Annotation[, .N, by = c("drug", "annotator_assessment")][, perc := round(N * 100 / sum(N), 2), by = "drug"])
  print(Annotation[!drug %in% c("DAA", "Methylphenidate", "SSRI", "TGA")][
    , .N, by = "out_of_scope"][, perc := round(N * 100 / sum(N), 2)])
  print(Annotation[!drug %in% c("DAA", "Methylphenidate", "SSRI", "TGA")][
    , .N, by = "annotator_assessment"][, perc := round(N * 100 / sum(N), 2)])

  # 3b. Completeness (vigiGrade) ----
  Completeness_VB <- setDT(read.csv(file.path(ANNOTATION_PATH, "vigiGrade_VB.csv")))
  Completeness_VB <- Completeness_VB[
    ReportID %in% Annotation$report_id,
    .(AverageCompleteness = as.numeric(AverageCompleteness) / 1000,
      report_id = ReportID)
  ]
  ggplot(Completeness_VB) +
    geom_density(aes(x = AverageCompleteness)) +
    theme_bw()

  Annotation <- Completeness_VB[Annotation, on = "report_id"]

  ggplot(
    Annotation[, .N, by = c("AverageCompleteness", "annotator_assessment")][
      , perc := N / sum(N), by = "annotator_assessment"]
  ) +
    geom_area(aes(x = AverageCompleteness, y = perc, fill = annotator_assessment),
              alpha = .3, position = "identity") +
    theme_bw()

  summary(Annotation[annotator_assessment == "suspected"]$AverageCompleteness)
  summary(Annotation[annotator_assessment == "precautionary"]$AverageCompleteness)
  summary(Annotation[annotator_assessment == "uninformative"]$AverageCompleteness)

  # Per-drug breakdowns (representative subset shown)
  for (d in c("DAA", "TGA", "Methylphenidate", "SSRI")) {
    for (r in c("suspected", "precautionary", "uninformative")) {
      cat(d, r, ":\n")
      print(summary(Annotation[drug == d & annotator_assessment == r]$AverageCompleteness))
    }
  }

  # 3c. Feature patterns by relevance category ----
  # Set `relevance` to one of the three values below to inspect that group.
  relevance <- "suspected"
  # relevance <- "precautionary"
  # relevance <- "uninformative"

  list_supportive <- c(
    "Reversibility" = round(mean(!is.na(Annotation[annotator_assessment == relevance]$reversibility))   * 100),
    "Statement"     = round(mean(!is.na(Annotation[annotator_assessment == relevance]$statement))       * 100),
    "Exclusion"     = round(mean(!is.na(Annotation[annotator_assessment == relevance]$exclusion))       * 100),
    "Dose-relation" = round(mean(!is.na(Annotation[annotator_assessment == relevance]$dose_relation))   * 100),
    "Temporality"   = round(mean(!is.na(Annotation[annotator_assessment == relevance]$temporality))     * 100),
    "Change"        = round(mean(!is.na(Annotation[annotator_assessment == relevance]$change))          * 100)
  )
  list_against <- c(
    "Reversibility"     = round(mean(!is.na(Annotation[annotator_assessment == relevance]$reversibility_2))    * 100),
    "Statement"         = round(mean(!is.na(Annotation[annotator_assessment == relevance]$statement_2))        * 100),
    "Invariance"        = round(mean(!is.na(Annotation[annotator_assessment == relevance]$invariance))         * 100),
    "Innocent bystander"= round(mean(!is.na(Annotation[annotator_assessment == relevance]$innocent_bystander)) * 100),
    "Underlying disease"= round(mean(!is.na(Annotation[annotator_assessment == relevance]$underlying_disease)) * 100)
  )
  cat("Relevance group:", relevance, "\n")
  cat("Supportive features (%):\n"); print(sort(list_supportive, decreasing = TRUE))
  cat("Against features (%):\n");    print(sort(list_against,    decreasing = TRUE))

  # Check: off-scope cases that still have evidence annotations ----
  off_scope_with_evidence <- Annotation[
    out_of_scope != "in-scope" &
    !annotator_assessment %in% c("precautionary", "suspected", "uninformative") &
    (  !is.na(reversibility) | !is.na(reversibility_2) |
       !is.na(statement)     | !is.na(statement_2)     |
       !is.na(exclusion)     | !is.na(innocent_bystander) |
       !is.na(underlying_disease) | !is.na(change) |
       !is.na(invariance)    | !is.na(dose_relation) |
       !is.na(temporality))
  ]
  if (interactive()) View(off_scope_with_evidence)

  # Upset plots ----
  upset_data <- Annotation[annotator_assessment == "suspected"][
    , .(report_id = paste0(report_id, drug),
        reversibility = ifelse(
          (!is.na(reversibility) | !is.na(dose_relation)) & is.na(reversibility_2), 1,
          ifelse(is.na(reversibility) & is.na(dose_relation) & !is.na(reversibility_2), -1,
                 ifelse((!is.na(reversibility) | !is.na(dose_relation)) & !is.na(reversibility_2), 0, NA))),
        causal_language = ifelse(
          !is.na(statement) & is.na(statement_2), 1,
          ifelse(is.na(statement) & !is.na(statement_2), -1,
                 ifelse(!is.na(statement) & !is.na(statement_2), 0, NA))),
        exclusion_alternatives = ifelse(
          !is.na(exclusion) & is.na(innocent_bystander) & is.na(underlying_disease), 1,
          ifelse(is.na(exclusion) & (!is.na(innocent_bystander) | !is.na(underlying_disease)), -1,
                 ifelse(!is.na(exclusion) & (!is.na(statement_2) | !is.na(underlying_disease)), 0, NA))),
        temporality = ifelse(!is.na(temporality), 1, NA),
        experience_change = ifelse(
          !is.na(change) & is.na(invariance), 1,
          ifelse(is.na(change) & !is.na(invariance), -1,
                 ifelse(!is.na(change) & !is.na(invariance), 0, NA)))
    )
  ]

  # Example: upset for SSRI suspected cases (binary presence of each dimension)
  upset_data2 <- Annotation[annotator_assessment == "suspected"][drug %in% "SSRI"][
    , .(report_id      = paste0(report_id, drug),
        reversibility_p = !is.na(reversibility) | !is.na(dose_relation),
        reversibility_n = !is.na(reversibility_2),
        causal_language_p = !is.na(statement),
        causal_language_n = !is.na(statement_2),
        exclusion_p     = !is.na(exclusion),
        exclusion_n     = !is.na(innocent_bystander) | !is.na(underlying_disease),
        temporality_p   = !is.na(temporality),
        change_p        = !is.na(change),
        change_n        = !is.na(invariance))
  ]
  upset_data3 <- upset_data2[
    , .(report_id       = report_id,
        reversibility   = reversibility_p   | reversibility_n,
        causal_language = causal_language_n | causal_language_p,
        exclusion       = exclusion_p       | exclusion_n,
        temporality     = temporality_p,
        change          = change_p          | change_n)
  ]

  upset(upset_data2, intersect = c("reversibility_p", "reversibility_n",
                                   "causal_language_p", "causal_language_n",
                                   "exclusion_p", "exclusion_n",
                                   "temporality_p", "change_p", "change_n"))
  upset(upset_data3[upset_data2, on = "report_id"],
        intersect = c("reversibility", "causal_language", "exclusion",
                      "temporality", "change",
                      "reversibility_p", "reversibility_n",
                      "causal_language_p", "causal_language_n",
                      "exclusion_p", "exclusion_n",
                      "temporality_p", "change_p", "change_n"))

  # Export Impulsivity_formatted for sharing (produces Data/impulsivity_SM.csv) ----
  Impulsivity_formatted <- Annotation[
    , relevance   := fifelse(out_of_scope == "in-scope", "Relevant", "Irrelevant")
  ][
    , rel_category := fcase(
        relevance == "Relevant"   & TRUE,                         first_up(annotator_assessment),
        relevance == "Irrelevant" & grepl("event",       out_of_scope), "Event",
        relevance == "Irrelevant" & grepl("relationship", out_of_scope), "Relationship",
        relevance == "Irrelevant" & grepl("medication",   out_of_scope), "Medication",
        default = NA_character_)
  ][
    , role := fcase(
        grepl("event",       out_of_scope), "Other impulsivity",
        grepl("relationship", out_of_scope), gsub("relationship-", "", out_of_scope),
        grepl("medication",   out_of_scope), "Miscoding",
        default = NA_character_)
  ][
    , .(id = report_id, drug, pt = icd_pt, relevance, rel_category, role,
        role_specification = NA_character_,
        reversibility_p = reversibility, statement_p = statement,
        exclusion, dose_relation, temporality_p = temporality,
        change, invariance, reversibility_n = reversibility_2,
        statement_n = statement_2, innocent_bystander, underlying_disease)
  ]

  Impulsivity_formatted[
    , role := ifelse(!is.na(role), first_up(role), NA)
  ][
    , role_specification := ifelse(role %in% c("Withdrawal", "Noncompliance", "Shortage"),
                                   role, role_specification)
  ][
    , role := ifelse(role %in% c("Withdrawal", "Noncompliance", "Shortage"),
                     "Discontinuation", role)
  ][
    , role := ifelse(role == "Innocent bystander", "Bystander", role)
  ][
    , role := ifelse(role == "Impossible timing", "Timing", role)
  ][
    , rel_category := ifelse(rel_category == "Uninformative", "Unclear", rel_category)
  ][
    , role_specification := ifelse(role_specification == "Noncompliance",
                                   "Non-adherence", role_specification)
  ]

  write.csv2(
    Impulsivity_formatted[
      , .(drug, pt, relevance, rel_category, role, role_specification,
          reversibility_p     = !is.na(reversibility_p),
          statement_p         = !is.na(statement_p),
          exclusion           = !is.na(exclusion),
          dose_relation       = !is.na(dose_relation),
          temporality_p       = !is.na(temporality_p),
          change              = !is.na(change),
          invariance          = !is.na(invariance),
          reversibility_n     = !is.na(reversibility_n),
          statement_n         = !is.na(statement_n),
          innocent_bystander  = !is.na(innocent_bystander),
          underlying_disease  = !is.na(underlying_disease))
    ],
    here::here("Data", "impulsivity_SM.csv")
  )

}  # end [REQUIRES ORIGINAL DATA] section 3

# ==============================================================================
# 4. [WORKS WITH SHARED DATA] Load processed data
# ==============================================================================

Impulsivity_formatted   <- load_impulsivity_data()
suicide_smq_formatted   <- load_suicidality_data()

# ==============================================================================
# 5. [WORKS WITH SHARED DATA] Category overview — impulsivity
# ==============================================================================

aes_cats <- category_aesthetics()

plot_data <- copy(Impulsivity_formatted)[
  , .N, by = .(Category = rel_category, drug, Group = fifelse(
    rel_category %in% c("Suspected", "Uninformative", "Precautionary"),
    "in-Scope", "out-of-Scope"
  ))
]
# Recode labels to match the original plotting convention (lowercase Category)
plot_data[, Category := factor(tolower(Category),
  levels = rev(c("suspected", "uninformative", "precautionary",
                 "medication", "event", "relationship")),
  ordered = TRUE
)]
plot_data[, perc := N / sum(N), by = "drug"]
plot_data[, drug := ifelse(drug == "DAA", "Dopamine agonists", drug)]
plot_data[, drug_group := fcase(
  drug %in% c("Dopamine agonists", "TGA"), "Positive",
  drug %in% c("Methylphenidate", "SSRI"),  "Ambiguous",
  default = "Negative"
)][, drug_group := factor(drug_group, levels = c("Positive", "Ambiguous", "Negative"), ordered = TRUE)]

# Faceted stacked bar chart
plot_category_bars(plot_data, aes_cats)

# Pie charts by drug group
plot_category_pie(plot_data, "Positive",  aes_cats)
plot_category_pie(plot_data, "Ambiguous", aes_cats, show_legend = TRUE)
plot_category_pie(plot_data, "Negative",  aes_cats)

# Waffle chart
plot_category_waffle(plot_data, W = 10, fill_colors = aes_cats$fill_colors)

# ==============================================================================
# 6. [WORKS WITH SHARED DATA] Sankey diagrams
# ==============================================================================
# Edit the `df` argument to switch between datasets or subsets.

pal <- sankey_palette()

# All suicidality cases
plot_sankey(suicide_smq_formatted, pal)

# All impulsivity cases
plot_sankey(Impulsivity_formatted %>%
              rename(id = any_of("id"), relevance = relevance,
                     rel_category = rel_category, role = role,
                     role_specification = role_specification,
                     drug = drug),
            pal)

# Subsets — uncomment as needed:
# plot_sankey(suicide_smq_formatted[drug == "gabapentin"], pal)
# plot_sankey(Impulsivity_formatted[drug %in% c("DAA", "TGA")], pal)
# plot_sankey(Impulsivity_formatted[!drug %in% c("DAA", "TGA", "Methylphenidate", "SSRI")], pal)

# ==============================================================================
# 7. [WORKS WITH SHARED DATA] Heatmaps
# ==============================================================================

# Optional: load ATC classification for drug ordering
if (!is.null(ATC_PATH) && file.exists(ATC_PATH)) {
  ATC <- setDT(read.csv2(ATC_PATH))[code == primary_code]
} else {
  ATC <- NULL
  message("ATC file not found — drug heatmap ordered by frequency, ATC class chart skipped.")
}

# ---- 7a. Drug heatmap (suicidality) ----

dt_drug <- copy(suicide_smq_formatted)[!is.na(drug)]

# Build category column (mirrors original script)
dt_drug[, category := fcase(
  relevance == "Not assessable",                                    "Non-assessable",
  rel_category %in% c("Precautionary", "Unclear", "Suspected"),    rel_category,
  default = role
)]

df_drug <- build_heatmap_data(
  dt     = dt_drug,
  entity_col = "drug",
  min_n  = 2,
  cat_levels = c("Overdose", "Discontinuation", "Inefficacy",
                 "Non-assessable",
                 "Precautionary", "Unclear", "Suspected")
)
setnames(df_drug, "entity", "drug")

df_drug <- df_drug %>%
  mutate(group = case_when(
    category %in% c("Overdose", "Discontinuation", "Inefficacy") ~ "Non-relevant",
    category == "Non-assessable"                                  ~ "Non-assessable",
    TRUE                                                          ~ "Potential ADR"
  ))

# Order drugs: by ATC if available, else by frequency
if (!is.null(ATC)) {
  drugs_in_ATC     <- ATC[Substance %in% df_drug$drug]$Substance
  drugs_not_in_ATC <- setdiff(df_drug$drug, drugs_in_ATC)
  drug_order       <- c(drugs_in_ATC, drugs_not_in_ATC)
} else {
  drug_order <- levels(df_drug$drug)
}
df_drug$drug <- factor(df_drug$drug, levels = drug_order)

p_drug <- plot_heatmap_detail(
  df      = df_drug %>% rename(entity = drug),
  x_label = "Drug (ordered by N drug occurrences)",
  sep_ymin = 3.5, sep_ymax = 4.5
)
p_drug

# ---- 7b. Bar chart by ATC class (requires ATC file) ----
if (!is.null(ATC)) {
  suicide_atc <- ATC[, .(drug = Substance, Class4, Class3, Class2, Class1)][
    setDT(suicide_smq_formatted), on = "drug"
  ]

  # Bar chart by drug (min 5 reports)
  drugs_shown <- suicide_atc[, .N, by = "drug"][N > 4]$drug
  ggplot(suicide_atc[drug %in% drugs_shown], aes(x = drug, fill = role)) +
    geom_bar(position = position_fill()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60)) +
    scale_fill_manual(values = pal)

  # Bar chart by ATC Class 1
  ggplot(suicide_atc[!is.na(Class1)], aes(x = Class1, fill = role)) +
    geom_bar(position = position_fill()) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60)) +
    scale_fill_manual(values = pal)
}

# ---- 7c. PT heatmap (suicidality) ----

dt_pt <- copy(suicide_smq_formatted) %>%
  separate_longer_delim(pt, delim = ";") %>%
  setDT()
dt_pt[, pt := trimws(pt)]

dt_pt[, category := fcase(
  relevance == "Not assessable",                                    "Non-assessable",
  rel_category %in% c("Precautionary", "Unclear", "Suspected"),    rel_category,
  default = role
)]

pt_order_manual <- c(
  "intentional overdose", "poisoning deliberate", "completed suicide",
  "suicide attempt", "suicidal behaviour", "intentional self-injury",
  "self-injurious ideation", "suicidal ideation", "depression suicidal"
)

df_pt <- build_heatmap_data(
  dt         = dt_pt,
  entity_col = "pt",
  min_n      = 2,
  cat_levels = c("Bystander", "Batch problem", "Overdose", "Discontinuation", "Inefficacy",
                 "Non-assessable",
                 "Precautionary", "Unclear", "Suspected")
)
setnames(df_pt, "entity", "pt")

df_pt <- df_pt %>%
  mutate(group = case_when(
    category %in% c("Overdose", "Discontinuation", "Inefficacy",
                    "Bystander", "Batch problem")       ~ "Non-relevant",
    category == "Non-assessable"                        ~ "Non-assessable",
    TRUE                                                ~ "Potential ADR"
  ))

shown_pts  <- levels(df_pt$pt)
pt_ordered <- c(pt_order_manual[pt_order_manual %in% shown_pts],
                setdiff(shown_pts, pt_order_manual))
df_pt$pt   <- factor(df_pt$pt, levels = pt_ordered)

p_pt <- plot_heatmap_detail(
  df      = df_pt %>% rename(entity = pt),
  x_label = "Preferred term (ordered by clinical sequence)",
  sep_ymin = 5.5, sep_ymax = 6.5
)
p_pt

