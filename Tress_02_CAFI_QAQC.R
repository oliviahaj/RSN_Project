#=============================================================================#
# Title: CAFI Tree Inventory QAQC
# Created by: Zach Madsen
# Created for: BNZ CAFI Site Network
# Date Created:

## Update by OLH - August 17, 2026
#=============================================================================#

#==================================#
# Libraries----
#==================================#
suppressMessages(library(tidyverse))
library(fs)
library(readxl)
library(stringi)
library(googledrive)

#==================================#
# Directories + Google Drive----
#==================================#
# Local folders mirror the Google Drive folders (created if missing)
proc.dir <- path("data", "processed")
meta.dir <- path("data", "metadata")
dir_create(c(proc.dir, meta.dir))

# Input file names (same name locally and on Drive)
inv.file <- "452_CAFI_TREE_v20241220_Complete.csv"
lookup.file <- "code_lookup_table.xlsx"

#---Download inputs from Google Drive---#
drive_auth()

drive_download(
  as_dribble(str_glue("data/processed/{inv.file}")),
  path = path(proc.dir, inv.file), overwrite = TRUE
)
drive_download(
  as_dribble(str_glue("data/metadata/{lookup.file}")),
  path = path(meta.dir, lookup.file), overwrite = TRUE
)

#---Output helper---#
# Every output goes flat into data/processed as:
#   CAFI_Trees_QAQC_<Type>_<Description>.csv
# Type = Final / Log / Quarantine. No date, so each run replaces the previous file.
# Paths are tracked for upload to Drive at the end.
out.files <- character()

save_qaqc <- function(df, type, desc) {
  f <- path(proc.dir, str_glue("CAFI_Trees_QAQC_{type}_{desc}.csv"))
  write_csv(df, f)
  out.files <<- c(out.files, f)
  invisible(f)
}

#==================================#
# Data----
#==================================#
# CAFI Tree Inventory
CI <- readr::read_csv(
  path(proc.dir, inv.file), guess_max = 215632
)

#---Coding Reference Sheets---#
# Get sheet names
lookup_names <- excel_sheets(path(meta.dir, lookup.file))
# List all the sheets
lookup_table <- setNames(map(lookup_names, ~ read_excel(path(meta.dir, lookup.file), sheet = .x)), lookup_names)

#========================================================#
# 1) Set up File Coding for Reference Later----
#========================================================#
#-----Join lookup table metadata to coded columns-----#
# Loop through the reference sheets to join the meta values to the code values
for(vari in names(lookup_table)){
  CI <- CI %>%
    left_join(lookup_table[[vari]], by = vari) %>%
    relocate(last_col(), .after = all_of(vari))
}

#-----Create UniqueID, update DIA from inches to cm, and set standard HTDMP-----#
# UniqueID combines plot, subplot, and tree_id for longitudinal tracking
# HTDMP defaulted to 4.5 where missing (standard measurement height)
# PLOT 1034 has some data entry issues where the inventory years and the cycle are incorrect, fixing that here
CI <- CI %>%
  mutate(
    DIA = DIA * 2.54, PREVDIA = PREVDIA * 2.54,
    INVYR = case_when(
      PLOT == 1034 & CYCLE == 3 ~ 2012,
      PLOT == 1034 & CYCLE == 4 ~ 2017,
      TRUE ~ INVYR
    ),
    QAQC_Action = "Fine",
    UniqueID = paste(PLOT, SUBP, TREE_ID, sep = '_'),
    HTDMP.Raw = HTDMP,
    HTDMP = if_else(is.na(HTDMP), 4.5, HTDMP),
    QAQC_Action = if_else(is.na(HTDMP.Raw), "Set HTDMP to standard DBH height", QAQC_Action)
  ) %>%
  relocate(UniqueID, .before = 1)

#-----Resolve unknown species from notes-----#
# SPCD 98 (Lutz spruce) not in lookup table; catch from NOTE field
# All remaining unknowns labeled "Unknown"
# ***NOTE: Some SPCD == 98 records do not mention Lutz in notes — these remain Unknown***
CI <- CI %>%
  mutate(
    Species = str_to_title(Species),
    Species.Raw = Species,
    Species = case_when(
      is.na(Species) & SPCD == 98 & str_detect(NOTE, "Lutz") ~ "Lutz",
      is.na(Species) & !is.na(SPCD) ~ "Unknown",
      is.na(Species) ~ "Unknown",
      TRUE ~ Species
    ),
    QAQC_Action = case_when(
      QAQC_Action != "Fine" & Species == "Lutz" ~ str_glue("{QAQC_Action}; Species Updated to Lutz by Notes"),
      Species == "Lutz" ~ "Species Updated to Lutz by Notes",
      QAQC_Action != "Fine" & Species == "Unknown" ~ str_glue("{QAQC_Action}; Species is unknown"),
      Species == "Unknown" ~ "Species is unknown",
      TRUE ~ QAQC_Action
    )
  )

#========================================================#
# 2) QAQC Log Creation----
#========================================================#
#-----Create the log table that will record all changes-----#
# Each row in this log represents one field change on one tree-year observation
QAQC_Report <- tibble(
  UniqueID = character(),
  INVYR = integer(),
  Field = character(),
  Old_Value = character(),
  New_Value = character(),
  Issue = character(),
  Rule = character()
)

#-----Helper function to build log entries-----#
# Pass in a filtered dataframe of changed rows plus field/issue/rule info
# Returns a tibble formatted for binding into QAQC_Report
log_change <- function(df, field, old, new, issue, rule) {
  tibble(
    UniqueID = df$UniqueID,
    INVYR = df$INVYR,
    Field = field,
    Old_Value = as.character(old),
    New_Value = as.character(new),
    Issue = issue,
    Rule = rule
  )
}

#========================================================#
# 3) Missing Tag Check----
#========================================================#
#-----Identify trees with missing tag indicator-----#
# TREE == 0 and notes contain "missing tag" indicate unidentifiable trees
# These cannot be linked to a specific tree and must be quarantined
missing_tag_ids <- CI %>%
  filter(TREE == 0 & str_detect(str_to_lower(NOTE), "missing tag"))

#---Quarantine missing tag trees, log, and save---#
# Separate missing tag trees and update QAQC_Action
CI_missing_tag_quarantine <- missing_tag_ids %>%
  mutate(
    QAQC_Action = case_when(
      QAQC_Action == "Fine" ~ "QUARANTINE_MissingTag",
      TRUE ~ paste(QAQC_Action, "QUARANTINE_MissingTag", sep = ", ")
    )
  )

# Log to master report
if (nrow(CI_missing_tag_quarantine) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      CI_missing_tag_quarantine,
      field = "Row",
      old = as.character(CI_missing_tag_quarantine$DIA),
      new = NA_character_,
      issue = "Quarantined — missing tag",
      rule = "TREE == 0 and NOTE contains 'missing tag' — tree cannot be linked to a specific individual"
    )
  )
}

# Remove from clean dataset
CI_clean <- CI %>%
  anti_join(missing_tag_ids, by = "UniqueID") # Twelve missing tags dropped.

# Save missing tag log and quarantine
missing_tag_log <- CI_missing_tag_quarantine %>%
  group_by(UniqueID) %>%
  summarise(
    n_rows_quarantined = n(),
    action = "quarantined_missing_tag",
    .groups = "drop"
  )

save_qaqc(missing_tag_log, "Log", "MissingTag")
save_qaqc(CI_missing_tag_quarantine, "Quarantine", "MissingTag")

#========================================================#
# 3) Duplicate Handling----
#========================================================#
# ***NOTE: Duplicates in CAFI are identified by UniqueID + INVYR (plot_subp_treeid + inventory year)***
# ***NOTE: RECONCILECD == 7 (cruiser error) and == 6 (moved) are informative for some duplicate cases***

#--------------------------------#
## A) Flag exact DIA + species duplicates----
#--------------------------------#
#-----Identify rows that are exact duplicates on DIA and Species-----#
# These are safe to drop — same tree, same year, same measurement
CI_clean <- CI_clean %>%
  group_by(UniqueID, INVYR, DIA, Species) %>%
  arrange(UniqueID, INVYR, DIA, Species) %>%
  mutate(
    Duplicate.DIA.Species = n() > 1,
    Row.Order = row_number()
  ) %>%
  ungroup()
sum(CI_clean$Duplicate.DIA.Species) # 0 duplicates all missing tree info


#---Capture dropped rows, log them, and remove from main dataset---#
if(sum(CI_clean$Duplicate.DIA.Species) > 0){
  # Filter to duplicate rows and keep only identifying columns for the log
  CI_drop <- CI_clean %>%
    filter(Duplicate.DIA.Species & Row.Order > 1) 
  
  # Log the dropped rows
  log_exact_dup_drop <- CI_drop %>%
    group_by(UniqueID) %>%
    summarise(
      n_rows_dropped = n(),
      action = "dropped_exact_duplicate_dia_species",
      .groups = "drop"
    )
  
  # Log exact duplicate drops to master report
  if (nrow(CI_drop) > 0) {
    QAQC_Report <- bind_rows(
      QAQC_Report,
      log_change(
        CI_drop,
        field = "Row",
        old = CI_drop$DIA,
        new = NA_character_,
        issue = "Exact duplicate removed",
        rule = "Duplicate UniqueID + INVYR + DIA + Species — kept first row, dropped remainder"
      )
    )
  }
  
  # Drop exact duplicates from main dataset
  CI_clean <- CI %>%
    filter(!(Duplicate.DIA.Species & Row.Order > 1)) %>%
    select(-Duplicate.DIA.Species, -Row.Order)
  
} else {
  CI_drop <- tibble()
  log_exact_dup_drop <- tibble()
  CI_clean <- CI_clean %>%
    select(-Duplicate.DIA.Species, -Row.Order)
}

#--------------------------------#
## B) Duplicates with different species----
#--------------------------------#
#-----Check if any remaining duplicates differ only in Species-----#
# These suggest a species mis-entry on one of the records
CI_clean <- CI_clean %>%
  group_by(UniqueID, INVYR, DIA) %>%
  mutate(Duplicate.DIA = n() > 1) %>%
  ungroup()


if(sum(CI_clean$Duplicate.DIA) > 0){
  # Determine most commonly recorded species from non-duplicate observations only
  # If no clean observations exist, most_common_species is NA — routed to quarantine below
  CI_clean <- CI_clean %>%
    group_by(UniqueID) %>%
    mutate(
      most_common_species = {
        non_dup <- Species[!Duplicate.DIA]
        tt <- table(non_dup, useNA = "no")
        if (length(tt) == 0) NA_character_ else names(tt)[which.max(tt)]
      }
    ) %>%
    ungroup()
  
  #-----Preview species conflict rows before dropping-----#
  # Review these before proceeding — check NOTE and OFFICE for context
  # ***NOTE: Review CI_species_dup_review before proceeding — RECONCILECD == 7 (cruiser error) may explain mismatches***
  # ***NOTE: Trees with NA most_common_species have no clean reference observations and will be quarantined***
  CI_species_dup <- CI_clean %>%
    filter(Duplicate.DIA == TRUE)
  CI_species_dup_review <- CI_clean %>%
    filter(UniqueID %in% CI_species_dup$UniqueID)
  
  #---Capture dropped rows, log them, and remove from main dataset---#
  # Filter to resolvable species mismatch duplicates (most_common_species is known)
  CI_drop2 <- CI_clean %>%
    filter(Duplicate.DIA == TRUE & Species != most_common_species & !is.na(most_common_species))
  
  # Log the dropped rows
  log_species_mismatch_drop <- CI_drop2 %>%
    group_by(UniqueID) %>%
    summarise(
      n_rows_dropped = n(),
      action = "dropped_dia_duplicate_wrong_species",
      .groups = "drop"
    )
  
  # Log species mismatch duplicate drops to master report
  if (nrow(CI_drop2) > 0) {
    QAQC_Report <- bind_rows(
      QAQC_Report,
      log_change(
        CI_drop2,
        field = "Species",
        old = CI_drop2$Species,
        new = CI_drop2$most_common_species,
        issue = "Species mismatch duplicate removed",
        rule = "Duplicate UniqueID + INVYR + DIA with wrong species — kept most common species row based on non-duplicate observations"
      )
    )
  }
  
  # Drop resolvable species-mismatch duplicates from main dataset
  CI_clean <- CI_clean %>%
    filter(!(Duplicate.DIA == TRUE & Species != most_common_species & !is.na(most_common_species)))
  
  #---Quarantine unresolvable species duplicates and log---#
  # Trees where all observations are duplicates — no clean reference to determine correct species
  ambiguous_dup_ids <- CI_clean %>%
    filter(Duplicate.DIA == TRUE & is.na(most_common_species)) %>%
    distinct(UniqueID)
  
  CI_ambiguous_quarantine <- CI_clean %>%
    semi_join(ambiguous_dup_ids, by = "UniqueID") %>%
    mutate(
      changed = TRUE,
      QAQC_Action = case_when(
        QAQC_Action == "Fine" ~ "QUARANTINE_AmbiguousSpeciesDuplicate",
        TRUE ~ paste(QAQC_Action, "QUARANTINE_AmbiguousSpeciesDuplicate", sep = ", ")
      )
    ) %>%
    select(-changed)
  
  # Log ambiguous quarantine to master report
  if (nrow(CI_ambiguous_quarantine) > 0) {
    QAQC_Report <- bind_rows(
      QAQC_Report,
      log_change(
        CI_ambiguous_quarantine,
        field = "Row",
        old = as.character(CI_ambiguous_quarantine$DIA),
        new = NA_character_,
        issue = "Quarantined — ambiguous species duplicate",
        rule = "Duplicate UniqueID + INVYR + DIA but no clean reference observations to determine correct species"
      )
    )
  }
  
  # Remove ambiguous trees from clean dataset
  CI_clean <- CI_clean %>%
    anti_join(ambiguous_dup_ids, by = "UniqueID") %>%
    select(-most_common_species)
} else {
  CI_drop2 <- tibble()
  log_species_mismatch_drop <- tibble()
  CI_ambiguous_quarantine <- tibble()
  CI_clean <- CI_clean %>%
    select(-Duplicate.DIA)
}


#--------------------------------#
## C) Remaining duplicates — different DIA in same year----
#--------------------------------#
#-----Find trees with more than one observation in the same INVYR-----#
# After the above passes, any remaining duplicates have conflicting DIA values
# These cannot be auto-resolved and must be quarantined for manual review
dup_years <- CI_clean %>%
  group_by(UniqueID, INVYR) %>%
  summarise(n_rows = n(), Duplicate = n_rows > 1, .groups = "drop") %>%
  filter(Duplicate)

if (nrow(dup_years) > 0) {
  # Get unique tree IDs with unresolved duplicates
  dup_ids <- dup_years %>%
    distinct(UniqueID)
  
  #-----Preview unresolved duplicates before quarantine-----#
  # RECONCILECD, DIACHECK, and notes may indicate which row to keep
  # ***NOTE: RECONCILECD == 7 (cruiser error) or DIACHECK == 1 (estimated) may indicate which row to keep***
  CI_dup_review <- CI_clean %>%
    semi_join(dup_ids, by = "UniqueID") %>%
    select(UniqueID, INVYR, DIA, PREVDIA, Species,
           STATUSCD_Meaning, RECONCILECD, Reconciled_Meaning,
           DIACHECK, DIACHECK_Meaning, NOTE, OFFICE) %>%
    arrange(UniqueID, INVYR)
  
  #---Log quarantined trees, separate datasets, compile and save---#
  # Build quarantine log with duplicate year count per tree
  log_quarantine <- CI_clean %>%
    semi_join(dup_ids, by = "UniqueID") %>%
    group_by(UniqueID) %>%
    summarise(
      n_rows_quarantined = n(),
      action = "quarantined_tree_due_to_remaining_duplicates",
      .groups = "drop"
    ) %>%
    left_join(
      dup_years %>% count(UniqueID, name = "n_duplicate_years"),
      by = "UniqueID"
    )
  
  # Separate quarantined trees from clean dataset
  CI_dup_quarantine <- CI_clean %>%
    semi_join(dup_ids, by = "UniqueID")
  
  CI_clean <- CI_clean %>%
    anti_join(dup_ids, by = "UniqueID")
  
  # Log quarantined trees to master report
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      CI_dup_quarantine,
      field = "Row",
      old = as.character(CI_dup_quarantine$DIA),
      new = NA_character_,
      issue = "Quarantined — conflicting DIA in same year",
      rule = "Remaining duplicate UniqueID + INVYR with different DIA values — cannot auto-resolve"
    )
  )
  
} else {
  CI_dup_quarantine <- tibble()
  log_quarantine <- tibble(UniqueID = character(), n_rows_quarantined = integer(), action = character())
}

# Compile all duplicate logs and dropped rows
if (nrow(bind_rows(log_exact_dup_drop, log_species_mismatch_drop, log_quarantine, CI_ambiguous_quarantine)) > 0) {
  dup_log <- bind_rows(
    log_exact_dup_drop,
    log_species_mismatch_drop,
    log_quarantine,
    CI_ambiguous_quarantine %>%
      group_by(UniqueID) %>%
      summarise(
        n_rows_quarantined = n(),
        action = "quarantined_ambiguous_species_duplicate_no_clean_reference",
        .groups = "drop"
      )
  )
} else{
  dup_log <- tibble()
}

drop_quarantine <- bind_rows(CI_drop, CI_drop2)

CI_dup_quarantine <- bind_rows(CI_dup_quarantine, CI_ambiguous_quarantine)

# Save duplicate outputs
if (nrow(drop_quarantine) > 0) {
  save_qaqc(drop_quarantine, "Quarantine", "Duplicates_Dropped")
} else{
  cat("No dropped duplicates found")
}

if(nrow(dup_log) > 0){
  save_qaqc(dup_log, "Log", "Duplicates")
} else{
  cat("No duplicates found")
}

if (nrow(CI_dup_quarantine) > 0) {
  save_qaqc(CI_dup_quarantine, "Quarantine", "Duplicates_ConflictingDIA_AmbiguousSpecies")
} else{
  cat("No duplicates quarantined")
}


#--------------------------------#
## D) Duplicate Tree Tags but Different TREE ID----
#--------------------------------#
#-----Flag records that share field tag and location but have different permanent IDs-----#
# May indicate fused stems, tag reuse, or data entry error — cannot auto-resolve
fused_stem_ids <- CI_clean %>%
  group_by(PLOT, SUBP, TREE, INVYR) %>%
  filter(n_distinct(TREE_ID) > 1) %>%
  ungroup() %>%
  arrange(TREE, INVYR, UniqueID)

#-----Flag by Type-----#
# Flag by ingrowth or duplicate tag
duplicate.issues <- fused_stem_ids %>% 
  group_by(PLOT, SUBP, TREE, INVYR) %>% 
  mutate(
    Duplicate.tag = n() > 1,
    Different_Species = Duplicate.tag & n_distinct(Species) > 1,
    Note.Tag = Duplicate.tag & str_detect(str_to_lower(NOTE), "tag"),
    Ingrowth = !Different_Species & any(RECONCILECD == 1, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  select(UniqueID, INVYR, TREE, Duplicate.tag, Different_Species, Note.Tag, Ingrowth)

#---Flag records and log---#
# Flag rather than quarantine — needs human review before removal
CI_clean <- CI_clean %>%
  left_join(duplicate.issues, by = join_by(UniqueID, INVYR, TREE)) %>% 
  mutate(
    QAQC_Action = case_when(
      Different_Species & Note.Tag ~ if_else(QAQC_Action == "Fine",
                                             "Same tag, but different species. Notes mention tag issue",
                                             paste(QAQC_Action, "Same tag, but different species. Notes mention tag issue", 
                                                   sep = ", ")),
      Note.Tag ~ if_else(QAQC_Action == "Fine",
                         "Same tag, and same species. Notes mention tag issue",
                         paste(QAQC_Action, "Same tag, and same species. Notes mention tag issue", 
                               sep = ", ")),
      Ingrowth ~ if_else(QAQC_Action == "Fine",
                         "Same tag, same species, and noted ingrowth, but no tag notes",
                         paste(QAQC_Action, "Same tag, same species, and noted ingrowth, but no notes", 
                               sep = ", ")),
      Duplicate.tag ~ if_else(QAQC_Action == "Fine",
                              "Same tag, same species, but no notes",
                              paste(QAQC_Action, "Same tag, same species, but no notes", 
                                    sep = ", ")),
      TRUE ~ QAQC_Action
    )
  ) %>%
  select(-any_of(c("Duplicate.tag", "Different_Species", "Note.Tag", "Ingrowth")))

# Log to master report
fused_flagged <- CI_clean %>%
  filter(str_detect(QAQC_Action, "Same tag"))

if (nrow(fused_flagged) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      fused_flagged,
      field = "TREE_ID",
      old = as.character(fused_flagged$TREE_ID),
      new = NA_character_,
      issue = "Duplicate tag flagged for review",
      rule = fused_flagged$QAQC_Action
    )
  )
}

#========================================================#
# 4) Species Conflict Check----
#========================================================#
# ***NOTE: Species conflicts refer to a UniqueID recorded as different species across INVYR cycles***
# ***NOTE: suggesting tag reuse or data entry error***
# ***NOTE: Using Species column (decoded + Lutz-corrected) rather than raw SPCD***

#--------------------------------#
## A) Fill Unknown if only one known species per tree----
#--------------------------------#
#-----Summarize species consistency per tree across all years-----#
# Get count of distinct known species and the single species name if only one exists
Species_pre <- CI_clean %>%
  group_by(UniqueID) %>%
  summarise(
    n_nonUnknown = n_distinct(Species[Species != "Unknown"]),
    species_single = {
      s <- unique(Species[Species != "Unknown"])
      if (length(s) == 1) s else "Unknown"
    },
    .groups = "drop"
  )
unique(Species_pre$species_single)
unique(Species_pre$n_nonUnknown)

#---Fill Unknown rows, update QAQC_Action, and log changes---#
# Join species summary back and fill Unknown where only one known species exists for the tree
CI_clean <- CI_clean %>%
  left_join(Species_pre %>% select(UniqueID, n_nonUnknown, species_single),
            by = "UniqueID") %>%
  mutate(
    Species_before_fill = Species,
    Species = if_else(
      n_nonUnknown == 1 & Species == "Unknown" & species_single != "Unknown",
      species_single, Species
    ),
    species_filled = Species_before_fill != Species,
    QAQC_Action = case_when(
      species_filled & QAQC_Action == "Fine" ~ "Filled_Unknown_FromSingleSpecies",
      species_filled ~ paste(QAQC_Action, "Filled_Unknown_FromSingleSpecies", sep = ", "),
      n_nonUnknown > 1 | n_nonUnknown == 0 & QAQC_Action == "Fine" ~ "Flag_SpeciesConflict",
      n_nonUnknown > 1 | n_nonUnknown == 0 ~ paste(QAQC_Action, "Flag_SpeciesConflict", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  select(-n_nonUnknown, -species_single, -species_filled)

# Log filled species rows
filled_species <- CI_clean %>%
  filter(Species_before_fill != Species)

CI_clean %>% 
  filter(UniqueID %in% unique(filled_species$UniqueID)) %>% 
  select(UniqueID, INVYR, TREE, SPCD, Species.Raw, Species_before_fill, Species) %>% 
  arrange(UniqueID, INVYR) %>% 
  View()

if (nrow(filled_species) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      filled_species,
      field = "Species",
      old = filled_species$Species_before_fill,
      new = filled_species$Species,
      issue = "Unknown species filled",
      rule = "Single known species across all years for this UniqueID — filled Unknown rows"
    )
  )
}

#--------------------------------#
## B) Preview and quarantine remaining species conflicts----
#--------------------------------#
#-----Count non-Unknown species per tree after fill-----#
# Any tree still showing more than one known species is a true conflict
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  mutate(Species.count = n_distinct(Species[Species != "Unknown"])) %>%
  ungroup()
sum(CI_clean$Species.count > 1) # 21 mismatches

#-----Preview conflicts before quarantine-----#
# STATUSCD transitions or RECONCILECD == 6 (moved) may explain apparent species changes
# ***NOTE: Review CI_species_conflict_review before quarantine***
# ***NOTE: RECONCILECD == 6 (moved) may indicate tag was reassigned to a different tree***
CI_species_conflict_review <- CI_clean %>%
  filter(Species.count > 1) %>%
  select(UniqueID, INVYR, Species, SPCD,
         STATUSCD_Meaning, PREV_STAT_Meaning,
         RECONCILECD, Reconciled_Meaning,
         NOTE, OFFICE) %>%
  arrange(UniqueID, INVYR)

# ***NOTE: Tree 1162_2_135364 had a note saying they changed it last year recorded
#          Flagging this instead of quarantining***
# Drop from the conflict review
CI_species_conflict_review <- CI_species_conflict_review %>% 
  filter(UniqueID != "1162_2_135364")

# Note in data and change
CI_clean <- CI_clean %>% 
  mutate(
    Species = if_else(UniqueID == "1162_2_135364", "Kenai Birch", Species),
    QAQC_Action = case_when(
      UniqueID == "1162_2_135364" & QAQC_Action == "Fine" ~ 
        paste(QAQC_Action, "Changed species to Kenai Birch based on notes for 2022"),
      UniqueID == "1162_2_135364" ~ "Changed species to Kenai Birch based on notes for 2022",
      TRUE ~ QAQC_Action
    )
  )

#---Summarize conflicts, quarantine unresolvable trees, and save---#
# Build summary of conflict details per tree
Species_post <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  summarise(
    n_nonUnknown = n_distinct(Species[Species != "Unknown"]),
    species_list = paste(unique(Species[Species != "Unknown"]), collapse = ", "),
    first_nonUnknown_yr = if (any(Species != "Unknown")) min(INVYR[Species != "Unknown"]) else NA_integer_,
    last_nonUnknown_yr = if (any(Species != "Unknown")) max(INVYR[Species != "Unknown"]) else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(
    SpeciesConflict = n_nonUnknown != 1,
    conflict_reason = case_when(
      n_nonUnknown == 0 ~ "all_species_unknown",
      n_nonUnknown > 1 ~ "multiple_species_recorded",
      TRUE ~ NA_character_
    )
  )

# Get IDs of all trees with unresolvable species conflicts
species_conflict_ids <- Species_post %>%
  filter(SpeciesConflict) %>%
  select(UniqueID)

# Separate quarantine dataset and update QAQC_Action
CI_species_quarantine <- CI_clean %>%
  semi_join(species_conflict_ids, by = "UniqueID") %>%
  mutate(
    changed = TRUE,
    QAQC_Action = case_when(
      QAQC_Action == "Fine" ~ "QUARANTINE_SpeciesConflict",
      TRUE ~ paste(QAQC_Action, "QUARANTINE_SpeciesConflict", sep = ", ")
    )
  ) %>%
  select(-changed)

# Remove quarantined trees from clean dataset
CI_clean <- CI_clean %>%
  anti_join(species_conflict_ids, by = "UniqueID")

# Log species conflict quarantine to master report
if (nrow(CI_species_quarantine) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      CI_species_quarantine,
      field = "Row",
      old = CI_species_quarantine$Species,
      new = NA_character_,
      issue = "Quarantined — species conflict across cycles",
      rule = "UniqueID recorded as multiple species across INVYR cycles — cannot auto-resolve"
    )
  )
}

#---Build and save species conflict log---#
# Build the log
species_conflict_log <- Species_post %>%
  filter(SpeciesConflict) %>%
  mutate(action = "quarantined_tree_due_to_species_conflict")

# Save the log and quarantined data
save_qaqc(species_conflict_log, "Log", "SpeciesConflict")
save_qaqc(CI_species_quarantine, "Quarantine", "SpeciesConflict")

#-----Cleanup helper columns-----#
CI_clean <- CI_clean %>%
  select(-any_of(c(
    "Species_before_fill", "Species.count", "Duplicate.DIA",
    "Duplicate.DIA.Species", "Row.Order", "most_common_species"
  )))

#========================================================#
# 5) Missing Value Interpolation----
#========================================================#
# ***NOTE: Only interpolating middle missing values — leading and trailing NAs are ignored***
# ***NOTE: Dead trees (STATUSCD != 1) are not interpolated***

#--------------------------------#
## A) Assess missing middle values----
#--------------------------------#
#-----Set up parameters to classify missing observations-----#
# Only fill between known values (Middle). Everything else is ignored.
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    n_obs = sum(!is.na(DIA) & STATUSCD == 1),
    first_obs = if_else(n_obs >= 1, min(INVYR[!is.na(DIA) & STATUSCD == 1]), NA_integer_),
    last_obs = if_else(n_obs >= 1, max(INVYR[!is.na(DIA) & STATUSCD == 1]), NA_integer_),
    Missing.Place = case_when(
      is.na(DIA) & STATUSCD == 1 & n_obs >= 2 & INVYR > first_obs & INVYR < last_obs ~ "Middle",
      is.na(DIA) ~ "Ignore",
      TRUE ~ "Observed"
    )
  ) %>%
  ungroup()

#--------------------------------#
## B) Check PREVDIA against lag DIA-----
#--------------------------------#
#-----Flag where PREVDIA does not match lag DIA-----#
# Use lag DIA as truth for all downstream logic
# ***NOTE: Mismatch may result from upstream QAQC corrections propagating forward***
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    PREVDIA.Raw = PREVDIA,
    lag_DIA = lag(DIA),
    PREVDIA_mismatch = !is.na(PREVDIA) & !is.na(lag_DIA) & PREVDIA != lag_DIA,
    PREVDIA = if_else(PREVDIA_mismatch, lag_DIA, PREVDIA),
    changed = PREVDIA_mismatch,
    QAQC_Action = case_when(
      changed & QAQC_Action == "Fine" ~ "Flag_PREVDIA_Mismatch",
      changed ~ paste(QAQC_Action, "Flag_PREVDIA_Mismatch", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup() %>%
  select(-changed)

# Log PREVDIA mismatches
prevdia_mismatch <- CI_clean %>%
  filter(PREVDIA_mismatch)

if (nrow(prevdia_mismatch) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      prevdia_mismatch,
      field = "PREVDIA",
      old = as.character(prevdia_mismatch$PREVDIA.Raw),
      new = as.character(prevdia_mismatch$PREVDIA),
      issue = "PREVDIA does not match lag DIA",
      rule = "PREVDIA replaced with lag DIA — lag DIA used as truth for all downstream logic"
    )
  )
}

#--------------------------------#
## C) Correct missing middle values----
#--------------------------------#
#---Interpolate middle missing DIA values, update PREVDIA, and log---#
# Linear interpolation between bounding observations only
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    DIA_before = DIA,
    n_obs = sum(!is.na(DIA)),
    can_interp = n_obs >= 2,
    interp_vals = if (dplyr::first(can_interp)) {
      approx(
        x = INVYR[!is.na(DIA)],
        y = DIA[!is.na(DIA)],
        xout = INVYR,
        rule = 1
      )$y
    } else {
      rep(NA_real_, dplyr::n())
    },
    DIA_filled = case_when(
      Missing.Place == "Middle" & can_interp ~ interp_vals,
      TRUE ~ DIA
    )
  ) %>%
  ungroup() %>%
  mutate(
    DIA = round(DIA_filled, 2),
    # Update PREVDIA of next record to match corrected DIA
    PREVDIA = if_else(Missing.Place == "Middle" & !is.na(DIA), lag(DIA), PREVDIA),
    changed = Missing.Place == "Middle" & is.na(DIA_before) & !is.na(DIA),
    QAQC_Action = case_when(
      changed & QAQC_Action == "Fine" ~ "Fixed_Middle_Missing",
      changed ~ paste(QAQC_Action, "Fixed_Middle_Missing", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  select(-changed)

# Log filled middle values
filled_mid <- CI_clean %>%
  filter(Missing.Place == "Middle" & is.na(DIA_before) & !is.na(DIA))

if (nrow(filled_mid) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      filled_mid,
      field = "DIA",
      old = filled_mid$DIA_before,
      new = filled_mid$DIA,
      issue = "Middle missing DIA",
      rule = "Linear interpolation between bounding INVYR observations only"
    )
  )
}

# Cleanup helper columns
CI_clean <- CI_clean %>%
  select(-any_of(c("DIA_filled", "n_obs", "can_interp", "interp_vals",
                   "first_obs", "last_obs", "PREVDIA_mismatch", "lag_DIA")))

#========================================================#
# 6) Decimal Error Correction----
#========================================================#
# ***NOTE: High decimal errors are auto-fixed (divide by 10)***
# ***NOTE: Low decimal errors on first obs are flagged only — no auto-fix***

#--------------------------------#
## A) First observation decimal error----
#--------------------------------#
#-----Detect and fix decimal errors on the first measured observation-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    DIA_before_decimal = DIA,
    n_obs = sum(!is.na(DIA)),
    First_Year = !is.na(DIA) & row_number() == which.max(!is.na(DIA)),
    # HIGH: first obs is 10x next obs — divide by 10 (fix)
    First_Decimal_Error_High = if_else(
      n_obs > 1 & First_Year & !is.na(DIA) & lead(!is.na(DIA)),
      DIA >= lead(DIA) * 10,
      FALSE
    ),
    # LOW: first obs is 10x smaller than next obs — flag only
    First_Decimal_Error_Low = if_else(
      n_obs > 1 & First_Year & !is.na(DIA) & lead(!is.na(DIA)),
      DIA <= lead(DIA) / 10,
      FALSE
    ),
    # Apply only HIGH fix
    DIA = case_when(
      First_Decimal_Error_High ~ DIA / 10,
      TRUE ~ DIA
    ),
    # Update PREVDIA of next record to match corrected DIA
    PREVDIA = if_else(First_Decimal_Error_High, lead(DIA), PREVDIA),
    QAQC_Action = case_when(
      First_Decimal_Error_High & QAQC_Action == "Fine" ~ "Fixed_High_Decimal_FirstObs",
      First_Decimal_Error_High ~ paste(QAQC_Action, "Fixed_High_Decimal_FirstObs", sep = ", "),
      First_Decimal_Error_Low & QAQC_Action == "Fine" ~ "Flag_Low_Decimal_FirstObs",
      First_Decimal_Error_Low ~ paste(QAQC_Action, "Flag_Low_Decimal_FirstObs", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup()

# Log first obs decimal fixes and flags
fixed_high_first <- CI_clean %>% filter(First_Decimal_Error_High)
if (nrow(fixed_high_first) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      fixed_high_first,
      field = "DIA",
      old = fixed_high_first$DIA_before_decimal,
      new = fixed_high_first$DIA,
      issue = "Decimal error — first obs",
      rule = "DIA >= 10x next observation — divided by 10"
    )
  )
}

flagged_low_first <- CI_clean %>% filter(First_Decimal_Error_Low)
if (nrow(flagged_low_first) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      flagged_low_first,
      field = "DIA",
      old = flagged_low_first$DIA_before_decimal,
      new = flagged_low_first$DIA_before_decimal,
      issue = "Decimal error (flag only) — first obs",
      rule = "First obs DIA <= 1/10 next observation — flagged only, no auto-fix"
    )
  )
}

#--------------------------------#
## B) Middle observation decimal errors----
#--------------------------------#
#-----Detect and fix decimal errors on middle observations-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    DIA_before_decimal = DIA,
    first_meas_idx = if (any(!is.na(DIA))) min(which(!is.na(DIA))) else NA_integer_,
    last_meas_idx = if (any(!is.na(DIA))) max(which(!is.na(DIA))) else NA_integer_,
    IsMiddleMeas = !is.na(DIA) & row_number() != first_meas_idx & row_number() != last_meas_idx,
    # Current is 10x previous — divide by 10 (middle only)
    Decimal.Error.Up =
      IsMiddleMeas &
      !is.na(lag(DIA)) &
      !str_detect(coalesce(lag(QAQC_Action), ""), "Flag_Low_Decimal_FirstObs") &
      DIA >= lag(DIA) * 10,
    DIA = case_when(
      Decimal.Error.Up ~ DIA / 10,
      TRUE ~ DIA
    ),
    # Update PREVDIA of next record to match corrected DIA
    PREVDIA = if_else(Decimal.Error.Up, lead(DIA), PREVDIA),
    QAQC_Action = case_when(
      Decimal.Error.Up & QAQC_Action == "Fine" ~ "Fixed_High_Decimal_Mid",
      Decimal.Error.Up ~ paste(QAQC_Action, "Fixed_High_Decimal_Mid", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup() %>%
  select(-first_meas_idx, -last_meas_idx, -IsMiddleMeas)

# Log middle decimal fixes
fixed_mid_decimal <- CI_clean %>% filter(Decimal.Error.Up)
if (nrow(fixed_mid_decimal) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      fixed_mid_decimal,
      field = "DIA",
      old = fixed_mid_decimal$DIA_before_decimal,
      new = fixed_mid_decimal$DIA,
      issue = "Decimal error — middle obs",
      rule = "Middle obs DIA >= 10x previous observation — divided by 10"
    )
  )
}

#--------------------------------#
## C) Last observation decimal error----
#--------------------------------#
#-----Detect and fix decimal errors on the last measured observation-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    DIA_before_decimal = DIA,
    n_obs = sum(!is.na(DIA)),
    Last_Year = !is.na(DIA) & row_number() == max(which(!is.na(DIA))),
    # Last obs is 10x previous — divide by 10
    Last_Decimal_Error_High = if_else(
      n_obs > 1 & Last_Year & !is.na(DIA) & lag(!is.na(DIA)),
      DIA >= lag(DIA) * 10,
      FALSE
    ),
    DIA = case_when(
      Last_Decimal_Error_High ~ DIA / 10,
      TRUE ~ DIA
    ),
    QAQC_Action = case_when(
      Last_Decimal_Error_High & QAQC_Action == "Fine" ~ "Fixed_High_Decimal_LastObs",
      Last_Decimal_Error_High ~ paste(QAQC_Action, "Fixed_High_Decimal_LastObs", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup()

# Log last obs decimal fixes
fixed_high_last <- CI_clean %>% filter(Last_Decimal_Error_High)
if (nrow(fixed_high_last) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      fixed_high_last,
      field = "DIA",
      old = fixed_high_last$DIA_before_decimal,
      new = fixed_high_last$DIA,
      issue = "Decimal error — last obs",
      rule = "Last obs DIA >= 10x previous observation — divided by 10"
    )
  )
}

# Cleanup decimal helper columns
CI_clean <- CI_clean %>%
  select(-any_of(c("DIA_before_decimal", "First_Year", "Last_Year", "n_obs",
                   "First_Decimal_Error_High", "First_Decimal_Error_Low",
                   "Last_Decimal_Error_High", "Decimal.Error.Up", "DIA_before")))

#========================================================#
# 7) Spike / Dip Detection + Correction----
#========================================================#
# ***NOTE: No species growth rate thresholds used — detection based on pattern logic only***
# ***NOTE: Fixes applied to mid-series only — last observations are flagged only***
# ***NOTE: Trees with NOTE entries are flagged but not auto-fixed***

#--------------------------------#
## A) Prep for DIA changes----
#--------------------------------#
#-----Calculate lag/lead values and year differences for spike/dip detection-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    last_meas_idx = if (any(!is.na(DIA))) max(which(!is.na(DIA))) else NA_integer_,
    IsLastMeas = !is.na(DIA) & (row_number() == last_meas_idx),
    year_diff = INVYR - lag(INVYR),
    prev_DIA = lag(DIA),
    next_DIA = lead(DIA),
    dia_change = DIA - prev_DIA,
    next_change = next_DIA - DIA,
    dia_year_change = if_else(
      !is.na(dia_change) & !is.na(year_diff) & year_diff > 0,
      dia_change / year_diff,
      NA_real_
    ),
    DIA_before_fix = DIA,
    Noted = !is.na(NOTE) & NOTE != ""
  ) %>%
  ungroup()

#--------------------------------#
## B) Detect spikes----
#--------------------------------#
#-----Flag observations that show a sudden implausible increase in DIA-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    high_spike = case_when(
      # don't flag if previous obs was flagged as low decimal
      str_detect(coalesce(lag(QAQC_Action), ""), "Flag_Low_Decimal_FirstObs") ~ FALSE,
      # 3-point spike pattern
      !is.na(prev_DIA) & !is.na(next_DIA) & !is.na(dia_change) & !is.na(next_change) ~
        (DIA > (((prev_DIA + next_DIA) / 2) + 1.5)) &
        (next_change < 0) &
        (abs(next_change) < dia_change) &
        (dia_change > -1),
      # absolute spike fallback
      !is.na(dia_change) ~ dia_change >= 10,
      TRUE ~ FALSE
    ),
    high_spike = replace_na(high_spike, FALSE)
  ) %>%
  ungroup()

#--------------------------------#
## C) Detect dips----
#--------------------------------#
#-----Flag observations that show a sudden implausible decrease in DIA-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    extreme_dip = if_else(
      lag(high_spike) %in% TRUE, FALSE,
      !is.na(dia_change) & dia_change <= -1.5
    ),
    extreme_dip = replace_na(extreme_dip, FALSE),
    # don't allow a spike immediately after an extreme dip
    high_spike = if_else(lag(extreme_dip) %in% TRUE, FALSE, high_spike),
    Dip.Rebound = extreme_dip & !is.na(next_DIA) & !is.na(prev_DIA) & (next_DIA >= (prev_DIA - 1)),
    Fix.Dip = extreme_dip & Dip.Rebound,
    New.Growth = extreme_dip & !Dip.Rebound,
    Fix = Fix.Dip | high_spike,
    Last.obs = Fix & IsLastMeas,
    LastObs.Dip = extreme_dip & IsLastMeas
  ) %>%
  ungroup()

#--------------------------------#
## D) Propagate new growth forward----
#--------------------------------#
#-----Mark all observations from new growth point forward-----#
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    NewGrowth_StartYear = if_else(
      any(New.Growth, na.rm = TRUE),
      min(INVYR[New.Growth], na.rm = TRUE),
      NA_integer_
    ),
    New.Growth = if_else(
      !is.na(NewGrowth_StartYear) & INVYR >= NewGrowth_StartYear,
      TRUE,
      New.Growth
    ),
    Fix = Fix.Dip | high_spike,
    Anomaly = high_spike | Fix.Dip | New.Growth,
    Anomaly_Type = case_when(
      high_spike ~ "Extreme Spike",
      Fix.Dip ~ "Extreme Dip",
      New.Growth ~ "New Growth",
      TRUE ~ "None"
    )
  ) %>%
  ungroup() %>%
  select(-NewGrowth_StartYear)

#--------------------------------#
## E) Apply corrections (mid-series only) + flag last obs----
#--------------------------------#
#-----Fix mid-series anomalies using mean of neighboring observations-----#
# Last observations and noted records are flagged only — no auto-fix
CI_clean <- CI_clean %>%
  group_by(UniqueID) %>%
  arrange(INVYR, .by_group = TRUE) %>%
  mutate(
    IsLastObs = IsLastMeas,
    FixAllowed_Mid = Fix & !IsLastObs & !Noted & !is.na(prev_DIA) & !is.na(next_DIA),
    NotedBlockedFix = Fix & !IsLastObs & Noted,
    DIA = case_when(
      FixAllowed_Mid ~ round((prev_DIA + next_DIA) / 2, 2),
      TRUE ~ DIA
    ),
    # Update PREVDIA of next record when DIA is corrected
    PREVDIA = if_else(FixAllowed_Mid, lead(DIA), PREVDIA),
    QAQC_Action = case_when(
      FixAllowed_Mid & high_spike & DIA != DIA_before_fix & QAQC_Action == "Fine" ~ "Fixed_Spike_Mid",
      FixAllowed_Mid & high_spike & DIA != DIA_before_fix ~ paste(QAQC_Action, "Fixed_Spike_Mid", sep = ", "),
      FixAllowed_Mid & Fix.Dip & DIA != DIA_before_fix & QAQC_Action == "Fine" ~ "Fixed_Dip_Mid",
      FixAllowed_Mid & Fix.Dip & DIA != DIA_before_fix ~ paste(QAQC_Action, "Fixed_Dip_Mid", sep = ", "),
      NotedBlockedFix & high_spike & QAQC_Action == "Fine" ~ "Flag_Spike_NotFixed_HasNote",
      NotedBlockedFix & high_spike ~ paste(QAQC_Action, "Flag_Spike_NotFixed_HasNote", sep = ", "),
      NotedBlockedFix & Fix.Dip & QAQC_Action == "Fine" ~ "Flag_Dip_NotFixed_HasNote",
      NotedBlockedFix & Fix.Dip ~ paste(QAQC_Action, "Flag_Dip_NotFixed_HasNote", sep = ", "),
      LastObs.Dip & QAQC_Action == "Fine" ~ "Flag_LastObs_ExtremeDip",
      LastObs.Dip ~ paste(QAQC_Action, "Flag_LastObs_ExtremeDip", sep = ", "),
      New.Growth & QAQC_Action == "Fine" ~ "Flag_NewGrowth",
      New.Growth ~ paste(QAQC_Action, "Flag_NewGrowth", sep = ", "),
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup()

#--------------------------------#
## F) Log spike/dip corrections----
#--------------------------------#
#-----Log all mid-series fixes to master report-----#
fixed_spike_dip <- CI_clean %>%
  filter(FixAllowed_Mid & !is.na(DIA_before_fix) & !is.na(DIA) & DIA != DIA_before_fix)

if (nrow(fixed_spike_dip) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    log_change(
      fixed_spike_dip,
      field = "DIA",
      old = fixed_spike_dip$DIA_before_fix,
      new = fixed_spike_dip$DIA,
      issue = case_when(
        fixed_spike_dip$high_spike ~ "Spike correction",
        fixed_spike_dip$Fix.Dip ~ "Dip correction"
      ),
      rule = case_when(
        fixed_spike_dip$high_spike ~ "Mid-series spike fix: mean of neighboring observations (Noted = FALSE)",
        fixed_spike_dip$Fix.Dip ~ "Mid-series dip fix: mean of neighboring observations (Noted = FALSE; dip rebounds within 1 cm)"
      )
    )
  )
}

#========================================================#
# 8) Review Tables----
#========================================================#
#-----Create review tables for anomalies and new growth-----#
Anomaly_Review <- CI_clean %>% filter(Anomaly & Noted)
NewGrowth_Review <- CI_clean %>% filter(New.Growth)

#========================================================#
# 9) Cleanup + Save----
#========================================================#
#-----Remove all helper columns-----#
CI_clean <- CI_clean %>%
  select(-any_of(c(
    "Missing.Place", "DIA_before", "DIA_before_fix",
    "last_meas_idx", "IsLastMeas", "IsLastObs",
    "year_diff", "prev_DIA", "next_DIA", "dia_change", "next_change", "dia_year_change",
    "high_spike", "extreme_dip", "Dip.Rebound", "Fix.Dip", "Fix",
    "Last.obs", "LastObs.Dip", "Anomaly_Type",
    "FixAllowed_Mid", "NotedBlockedFix", "Noted",
    "First_Decimal_Error_High", "First_Decimal_Error_Low",
    "Last_Decimal_Error_High", "Decimal.Error.Up",
    "DIA_before_decimal", "First_Year", "Last_Year", "n_obs",
    "PREVDIA.Raw", "lag_DIA", "PREVDIA_mismatch"
  )))

#-----Save final outputs-----#
save_qaqc(CI_clean, "Final", "CleanedInventory")
save_qaqc(QAQC_Report, "Log", "ChangeReport")
save_qaqc(Anomaly_Review, "Log", "AnomalyNoted")
save_qaqc(NewGrowth_Review, "Log", "NewGrowthFlagged")

#-----Upload all outputs to Google Drive data/processed-----#
drive.proc <- as_dribble("data/processed/")

walk(out.files, \(f) drive_upload(
  media = f, path = drive.proc, name = path_file(f), overwrite = TRUE
))

#=============================================================================#
# End of Script
#=============================================================================#
