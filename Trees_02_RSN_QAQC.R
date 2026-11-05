#=============================================================================#
# RSN Tree Inventory QAQC – Cleaned, Full-Scope Rewrite
# By Zach Madsen  + some edits by Olivia to make it fit in the workflow
# For BNZ RSN site network to clean up errored DBH data
# Added QAQC_Action column (Fine / Fixed / Flagged w/ reason)
#=============================================================================#

#--------------------#
# Tools and Outpaths----
#--------------------#
#---Libraries Needed---#
library(tidyverse)
library(fs)
library(googledrive)

#---Parameters for directories and pathways---#
# Today's date for saving new versions
today <- Sys.Date()

# Local folders mirror the Google Drive folders (created if missing)
proc.dir <- path("data", "processed")
meta.dir <- path("data", "metadata")
plot.dir <- path("figures")
dir_create(c(proc.dir, meta.dir, plot.dir))

# Input file names (same name locally and on Drive)
inv.file <- "320_TreeInventory_1989-2026_OLH.csv"
grw.file <- "Species_DBH_GrowthRate.csv"

#---Download inputs from Google Drive---#
drive_auth()

drive_download(
  as_dribble(str_glue("data/processed/{inv.file}")),
  path = path(proc.dir, inv.file), overwrite = TRUE
)
drive_download(
  as_dribble(str_glue("data/metadata/{grw.file}")),
  path = path(meta.dir, grw.file), overwrite = TRUE
)

#---Output helper---#
# Every output goes flat into data/processed as:
#   RSN_Trees_QAQC_<Type>_<Description>.csv
# Type = Final / Log / Quarantine. Paths are tracked for upload at the end.
# No date in the name, so each run replaces the previous file (locally and on Drive).
out.files <- character()

save_qaqc <- function(df, type, desc) {
  f <- path(proc.dir, str_glue("RSN_Trees_QAQC_{type}_{desc}.csv"))
  write_csv(df, f)
  out.files <<- c(out.files, f)
  invisible(f)
}


#------------------------------------------------------------#
# 1. Load + Prep----
#------------------------------------------------------------#
#Bring in tree inventory data provided by Mark
MI <- read.csv(
  path(proc.dir, inv.file),
  stringsAsFactors = FALSE, encoding = "latin1"
)

# Species growth-rate reference (for spike plausibility)
Sps.Grw <- read.csv(
  path(meta.dir, grw.file),
  stringsAsFactors = FALSE
)
Sps.Grw <- Sps.Grw %>% rename(SPECIES = Sps.Code)

# Standardize names and prep data for QAQC
MI <- MI %>%
  mutate(
    DATE = mdy(DATE),
    Year = year(DATE),
    # Clean site name white spaces and make sure upper case
    SITE = str_squish(str_to_upper(SITE)),
    # Save original TREE for comparison
    TREE_raw = TREE,
    # Fix Tree names that have two tags with first inside ()
    TREE = if_else(
      str_detect(TREE, "\\(\\d+\\)"), 
      str_match(TREE, "\\((\\d+)\\)")[,2], 
      TREE
    ),
    # Recreate UniqueID and triple check no weird space or extrac symbols
    UniqueID = paste(SITE, PLOT, TREE, sep = "_"),
    UniqueID = str_replace_all(UniqueID, ' ', ''),
    # SPECIES cleanup (normalize + set unknowns to "UNK")
    SPECIES_raw = SPECIES,
    SPECIES = str_to_upper(str_squish(SPECIES)),
    SPECIES = if_else(SPECIES %in% c("", "ND", "NOSPECIESENTERED", "NOTREES"), "UNK", SPECIES),
    # enforce plausible codes: keep 5–6 char codes (PICEA is 5; most others are 6); everything else -> UNK
    SPECIES = if_else(SPECIES == "UNK" | nchar(SPECIES) %in% c(5, 6), SPECIES, "UNK"),
    #Save original DBH in separate column to compare to
    DBH_raw = suppressWarnings(as.numeric(DBH)),
    DBH = if_else(!is.na(DBH_raw) & DBH_raw > 0, DBH_raw, NA_real_),
    
    DA_raw = suppressWarnings(as.integer(DA)),
    #Signify the status of the coding from DBH and DA columns
    STATUS = case_when(
      DA_raw %in% c(-999, -9999) ~ "Missing",
      DA_raw == 1 & !is.na(DBH_raw) & DBH_raw < 0 ~ "Alive_DBHcoded",
      DA_raw == 0 & !is.na(DBH_raw) & DBH_raw > 0 ~ "Dead_DBHmeasured",
      Year <= 2018 & DBH_raw == -9999 ~ "Missing/Dead Pre-2018",
      DBH_raw == -7777 ~ "StandingDead",
      DBH_raw == -9999 ~ "FallenDead",
      DBH_raw == -8888 ~ "Missing",
      is.na(DBH_raw)   ~ "Missing",
      DBH_raw > 0      ~ "Alive",
      TRUE             ~ "Unknown"
    ),
    #Signify if notes exist
    Noted = (!is.na(NOTES) & str_trim(NOTES) != "") |
      (!is.na(OfficeNotes) & str_trim(OfficeNotes) != ""),
    #Create plot size
    Plot_Size = if_else(SITE == "UP1A", "10x15", "10x10"),
    #Create ID with year to help determine data that needs to be fixed
    ReplaceID = paste(UniqueID, Year, sep = "_"),
    #Set the QAQC decided action to fine for all (will adjust through code)
    QAQC_Action = "Fine"
  ) %>%
  rename(Plot.Area.M2 = PLOT.AREA..m..) %>% 
  relocate(Year, .before = DATE) %>% 
  relocate(Plot.Area.M2, Plot_Size, .before = NOTES)

# Identify odd tree naming
trees.dis <- MI %>% 
  distinct(TREE, .keep_all = TRUE) %>% 
  mutate(
    x_num = suppressWarnings(as.numeric(TREE)),
    tree.order = case_when(
      TREE  %in%  c("565_1", "565_2") ~ 3,
      is.na(x_num) ~ 1,
      x_num < 0 ~ 2,
      TRUE ~ 3
    )
  ) %>% 
  arrange(tree.order) %>%
  filter(tree.order != 3) %>% 
  select(UniqueID, SITE, PLOT, TREE, Year, TREE_raw, NOTES, OfficeNotes, DBH)

#-----------------#
# ***Note*** 
#-----------------#
# UP4A has a tree labeled ND, with a note saying maybe 1023
# UP4A has three years of observations: 2008, 2013, and 2018
#-----------------#
# End of Note
#-----------------#

# Inspect Site: UP4A, Plot: 13, TREE: 1023
up4a <- MI %>% 
  filter(SITE == 'UP4A', PLOT == '13', TREE == '1023') %>% 
  arrange(UniqueID, Year)

#-----------------#
# ***Note*** 
#-----------------#
# After inspecting UP4A 1023, there is a no observation for 2018
# The tree in question was observed in 2018
#-----------------#
# End of Note
#-----------------#

# Inspect all UP4A_13 that are missing year 2018
up4a.all <- MI %>% 
  filter(SITE == 'UP4A', PLOT == '13', Year %in% c(2008, 2013)) %>% 
  distinct(UniqueID, .keep_all = TRUE)
up4a.2018 <- MI %>% 
  filter(SITE == 'UP4A', PLOT == '13', Year == 2018)

setdiff(up4a.all$UniqueID, up4a.2018$UniqueID)

#-----------------#
# ***Note*** 
#-----------------#
# After inspecting all trees measured in 2013, the only one missing in 2018 is 1023
# It is safe to assume that the ND tree in UP4A_13 is 1023
#-----------------#
# End of Note
#-----------------#

# Fix the ND tree
MI <- MI %>% 
  mutate(
    TREE = if_else(TREE == "nd", "1023", TREE),
    UniqueID = if_else(UniqueID == "UP4A_13_nd", "UP4A_13_1023", UniqueID),
    QAQC_Action = if_else(
      UniqueID == "UP4A_13_1023" & Year == 2018, 
      "Changed tree number to 1023 after inspection.", 
      QAQC_Action
    )
  )

# Extract trees that have no value or aren't determined
MI.weird.tree <- MI %>% 
  semi_join(trees.dis, join_by(TREE)) %>% 
  arrange(UniqueID, DATE)

# Remove those tress from the data
MI <- MI %>% 
  anti_join(trees.dis, join_by(TREE)) %>% 
  arrange(UniqueID, DATE)

# Save the quarantined weird trees
save_qaqc(MI.weird.tree, "Quarantine", "OddTreeNumbers")

#------------------------------------------------------------#
# 2. QAQC Log Creation----
#------------------------------------------------------------#
#Creat the log table that will describe changes
QAQC_Report <- tibble(
  ReplaceID = character(),
  UniqueID  = character(),
  Year      = integer(),
  Field     = character(),
  Old_Value = character(),
  New_Value = character(),
  Issue     = character(),
  Rule      = character()
)

#Create a helper function for updating log table
log_change <- function(df, field, old, new, issue, rule) {
  tibble(
    ReplaceID = df$ReplaceID,
    UniqueID  = df$UniqueID,
    Year      = df$Year,
    Field     = field,
    Old_Value = as.character(old),
    New_Value = as.character(new),
    Issue     = issue,
    Rule      = rule
  )
}

#------------------------------------------------------------#
# 3. Duplicate Handling----
#------------------------------------------------------------#
#--------------------------------#
## A) Flag exact DBH+species duplicates----
#--------------------------------#
MI <- MI %>%
  group_by(UniqueID, Year, DBH, SPECIES) %>%
  arrange(UniqueID, Year, DBH, SPECIES) %>%
  mutate(
    Duplicate.DBH.Species = n() > 1,
    Row.Order = row_number()
  ) %>%
  ungroup()

# rows to drop
MI_drop <- MI %>%
  filter(Duplicate.DBH.Species & Row.Order > 1) %>% 
  select(UniqueID, Year, DBH_raw, DBH, DA, DA_raw, SPECIES, SPECIES_raw, STATUS, NOTES, OfficeNotes)

# LOG what will be dropped
log_dbh_species_drop <- MI_drop %>%
  group_by(UniqueID) %>%
  summarise(
    n_rows_dropped = n(),
    action = "dropped_exact_duplicate_dbh_species",
    .groups = "drop"
  )

# Drop the duplicates
MI <- MI %>%
  filter(!(Duplicate.DBH.Species & Row.Order > 1)) %>%
  select(-Duplicate.DBH.Species, -Row.Order)

#--------------------------------#
## B) Duplicates with different species----
#--------------------------------#
#Determine if any duplicates have different species
MI <- MI %>%
  group_by(UniqueID, Year, DBH) %>%
  mutate(Duplicate.DBH = n() > 1) %>%
  ungroup()

# Find common species for each tree
MI <- MI %>%
  group_by(UniqueID) %>%
  mutate(most_common = {
    tt <- table(SPECIES, useNA = "no")
    if (length(tt) == 0) NA_character_ else names(tt)[which.max(tt)]
  }) %>%
  ungroup()

# rows to drop
MI_drop2 <- MI %>%
  filter(Duplicate.DBH == TRUE & SPECIES != most_common) %>% 
  select(UniqueID, Year, DBH_raw, DBH, DA, DA_raw, SPECIES, SPECIES_raw, STATUS, NOTES, OfficeNotes)

# LOG what will be dropped
log_dbh_species_mismatch_drop <- MI_drop2 %>%
  group_by(UniqueID) %>%
  summarise(
    n_rows_dropped = n(),
    action = "dropped_dbh_duplicate_wrong_species",
    .groups = "drop"
  )

# DROP them
MI <- MI %>%
  filter(!(Duplicate.DBH == TRUE & SPECIES != most_common)) %>%
  select(-most_common)

#--------------------------------#
## C) Duplicate and different DBH----
#--------------------------------#
# Find duplicate tree observation in same year with different DBH
dup_years <- MI %>%
  group_by(UniqueID, Year) %>%
  summarise(n_rows = n(), Duplicate = n_rows > 1, .groups = "drop") %>%
  filter(Duplicate)

# List the names of those duplcates
dup_ids <- dup_years %>%
  distinct(UniqueID)

# LOG: how many duplicate-years and how many rows are being quarantined per ID
log_quarantine <- MI %>%
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

# Separate out all observations with the Unique Id of the duplicates with different DBH
# This is for quarantine
MI.2 <- MI %>%
  semi_join(dup_ids, by = "UniqueID")

# Remove all observations with the Unique Id of the duplicates with different DBH
# For main dataset
MI_clean <- MI %>%
  anti_join(dup_ids, by = "UniqueID")

# Log which Unique ID got quarantined or dropped
dup_log <- bind_rows(
  log_dbh_species_drop,
  log_dbh_species_mismatch_drop,
  log_quarantine
)

# Quarantine dropped duplicates
drop.quarantine <- bind_rows(
  MI_drop,
  MI_drop2
)

# Save the quarantine log
save_qaqc(dup_log, "Log", "Duplicates")

# Save the quarantined data
save_qaqc(MI.2, "Quarantine", "Duplicates_DifferentDBH")

# Save dropped data
save_qaqc(drop.quarantine, "Quarantine", "Duplicates_Dropped")

#------------------------------------------------------------#
# 4. Species conflict correction----
#------------------------------------------------------------#
# Create the valid list of species codes
valid_species <- c("PICMAR","PICGLA","BETNEO","POPTRE","POPBAL","LARLAR","SALBEB","PICEA")

#-----------------------------#
## A) Fill UNK if only one known species per tree----
#-----------------------------#
Species_Integrity_pre <- MI_clean %>%
  group_by(UniqueID) %>%
  summarise(
    n_nonUNK_species = n_distinct(SPECIES[SPECIES != "UNK"]),
    species_single = {
      s <- unique(SPECIES[SPECIES != "UNK"])
      if (length(s) == 1) s else "UNK"
    },
    .groups = "drop"
  )

MI_clean <- MI_clean %>%
  left_join(Species_Integrity_pre %>% select(UniqueID, n_nonUNK_species, species_single),
            by = "UniqueID") %>%
  mutate(
    SPECIES_before_fill = SPECIES,
    SPECIES = if_else(n_nonUNK_species == 1 & SPECIES == "UNK" & species_single != "UNK",
                      species_single, SPECIES),
    QAQC_Action = case_when(
      SPECIES_before_fill != SPECIES ~ if_else(QAQC_Action != 'Fine', 
                                               str_glue("{QAQC_Action}; Filled_UNK_FromSingleSpecies"),
                                               "Filled_UNK_FromSingleSpecies"),
      n_nonUNK_species > 1 & SPECIES == "UNK" ~ if_else(QAQC_Action != 'Fine', 
                                                        str_glue("{QAQC_Action}; Flag_UNK_WithSpeciesConflict"),
                                                        "Flag_UNK_WithSpeciesConflict"),
      TRUE ~ QAQC_Action
    )
  ) %>%
  select(-n_nonUNK_species, -species_single)

#-----------------------------#
## B) Resolve remaining conflicts using notes---- 
# - Fix only if exactly ONE valid code in notes
# - If notes contain >1 valid code: flag, no auto-fix
#-----------------------------#

# conflict count AFTER UNK fill (ignore UNK)
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  mutate(
    Species.count = n_distinct(SPECIES[SPECIES != "UNK"])
  ) %>%
  ungroup()

# Make notes all upper case to help find
MI_clean <- MI_clean %>%
  mutate(
    Notes_all = toupper(paste(NOTES, OfficeNotes, sep = " ")),
    Note_tokens = stringr::str_extract_all(Notes_all, "[A-Z]{5,6}")
  )

# Scan notes for species codes and summarize what species (if any) are mentioned
note_species_map <- MI_clean %>%
  transmute(
    ReplaceID,
    Note_species = lapply(Note_tokens, function(tok) unique(tok[tok %in% valid_species])),
    n_note_species = vapply(Note_species, length, integer(1)),
    Note_species_single = vapply(Note_species, function(x) if (length(x) == 1) x else NA_character_, character(1))
  )

# Fix conflicting species with notes when applicable
MI_clean <- MI_clean %>%
  left_join(
    note_species_map %>% select(ReplaceID, n_note_species, Note_species_single),
    by = "ReplaceID"
  ) %>%
  mutate(
    SPECIES_before_note = SPECIES,
    
    # override only when there is a true conflict (>1 non-UNK) AND exactly one valid code in notes
    SPECIES = if_else(Species.count > 1 & !is.na(Note_species_single),
                      Note_species_single, SPECIES),
    
    QAQC_Action = case_when(
      Species.count > 1 & 
        !is.na(Note_species_single) & 
        SPECIES_before_note != SPECIES ~ if_else(QAQC_Action != "Fine", 
                                                 str_glue("{QAQC_Action}; Fixed_Species_FromNotes"), 
                                                 "Fixed_Species_FromNotes"),
      Species.count > 1 & 
        n_note_species > 1 ~ if_else(QAQC_Action != "Fine",
                                     str_glue("{QAQC_Action}; Flag_SpeciesNotes_ConflictingCodes"),
                                     "Flag_SpeciesNotes_ConflictingCodes"),
      TRUE ~ QAQC_Action
    )
  )

# Log actual fixes from notes
fixed_species_note <- MI_clean %>%
  filter(str_detect(QAQC_Action, "Fixed_Species_FromNotes") & SPECIES_before_note != SPECIES)

if (nrow(fixed_species_note) > 0) {
  QAQC_Report <- dplyr::bind_rows(
    QAQC_Report,
    log_change(
      fixed_species_note,
      field = "SPECIES",
      old   = fixed_species_note$SPECIES_before_note,
      new   = fixed_species_note$SPECIES,
      issue = "Species corrected from notes",
      rule  = "NOTES/OfficeNotes contained exactly one valid species code (override only when tree had >1 non-UNK species)"
    )
  )
}

# cleanup helper cols from notes step
MI_clean <- MI_clean %>%
  select(-Notes_all, -Note_tokens, -n_note_species, -Note_species_single)

#-----------------------------#
## C) Quarantine any UniqueID still showing >1 non-UNK species or all UNK species----
#-----------------------------#
Species_Integrity_post <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  summarise(
    n_nonUNK_species = n_distinct(SPECIES[SPECIES != "UNK"]),
    species_list = paste(unique(SPECIES[SPECIES != "UNK"]), collapse = ", "),
    first_nonUNK_year = if (any(SPECIES != "UNK")) min(Year[SPECIES != "UNK"]) else NA_integer_,
    last_nonUNK_year  = if (any(SPECIES != "UNK")) max(Year[SPECIES != "UNK"]) else NA_integer_,
    .groups = "drop"
  ) %>%
  mutate(
    SpeciesConflict = n_nonUNK_species != 1,
    conflict_reason = case_when(
      n_nonUNK_species == 0 ~ "all_species_unknown",
      n_nonUNK_species > 1 ~ "multiple_species_codes",
      TRUE ~ NA_character_
    )
  )

species_conflict_ids <- Species_Integrity_post %>%
  filter(SpeciesConflict) %>%
  select(UniqueID)

MI_species_conflict <- MI_clean %>%
  semi_join(species_conflict_ids, by = "UniqueID") %>%
  mutate(QAQC_Action = if_else(QAQC_Action != "Fine",
                               str_glue("{QAQC_Action}; QUARANTINE_SpeciesConflict_TagReused"),
                               "QUARANTINE_SpeciesConflict_TagReused"))

MI_clean <- MI_clean %>%
  anti_join(species_conflict_ids, by = "UniqueID")

species_conflict_log <- Species_Integrity_post %>%
  filter(SpeciesConflict) %>%
  mutate(
    action = "quarantined_tree_due_to_species_change"
  )

# Save Species conflict log
save_qaqc(species_conflict_log, "Log", "SpeciesConflict")

# Save Species Conflict Quarantine
save_qaqc(MI_species_conflict, "Quarantine", "SpeciesConflict")

#------------------------------------------------------------#
# 5. Missing Classification (+ species thresholds)----
#------------------------------------------------------------#
#--------------------------------#
## A) Assess missing middle values----
#--------------------------------#
# Join the data with the species growth resource
MI_clean <- MI_clean %>%
  left_join(
    Sps.Grw %>% select(SPECIES, Max.Growth, `Max.Growth.Error`),
    by = "SPECIES"
  ) %>%
  rename(
    Species_Max_Growth = Max.Growth,
    Species_Max_Growth_Error = `Max.Growth.Error`
  )

# Set up paramaters to find missing values
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    n_obs = sum(!is.na(DBH)),
    first_obs = if_else(n_obs >= 1, min(Year[!is.na(DBH)]), NA_integer_),
    last_obs  = if_else(n_obs >= 1, max(Year[!is.na(DBH)]), NA_integer_),
    
    # Only fill between known values (Middle). Everything else is ignored.
    Missing.Place = case_when(
      is.na(DBH) & n_obs >= 2 & Year > first_obs & Year < last_obs ~ "Middle",
      is.na(DBH) ~ "Ignore",
      TRUE ~ "Observed"
    )
  ) %>%
  ungroup()

#--------------------------------#
## B) Correct missing middle values----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    DBH_before = DBH,
    
    # count usable points for interpolation (same value repeated within group)
    n_obs = sum(!is.na(DBH)),
    can_interp = n_obs >= 2,
    
    # compute approx() only when safe (use scalar condition)
    interp_vals = if (dplyr::first(can_interp)) {
      approx(
        x = Year[!is.na(DBH)],
        y = DBH[!is.na(DBH)],
        xout = Year,
        rule = 1
      )$y
    } else {
      rep(NA_real_, dplyr::n())
    },
    
    DBH_filled = dplyr::case_when(
      Missing.Place == "Middle" & can_interp ~ interp_vals,
      TRUE ~ DBH
    )
  ) %>%
  ungroup() %>%
  mutate(
    DBH = round(DBH_filled, 2),
    DA = case_when(
      Missing.Place == "Middle" & DA == 0 ~ 1, 
      Missing.Place == "Ignore" & DA == 1 & DBH_raw %in% c(-7777, -9999) ~ 0,
      TRUE ~ DA
    ),
    QAQC_Action = case_when(
      Missing.Place == "Middle" & 
        is.na(DBH_before) & 
        !is.na(DBH) & 
        DA_raw == 0 ~ if_else(QAQC_Action != "Fine",
                              str_glue("{QAQC_Action}; Fixed_Middle_Missing_Changed_DA_0_to_1"),
                              "Fixed_Middle_Missing_Changed_DA_0_to_1"),
      Missing.Place == "Middle" & 
        is.na(DBH_before) & 
        !is.na(DBH) ~ if_else(QAQC_Action != "Fine", 
                              str_glue("{QAQC_Action}; Fixed_Middle_Missing"),
                              "Fixed_Middle_Missing"),
      TRUE ~ QAQC_Action
    )
  ) %>%
  {
    filled <- dplyr::filter(., Missing.Place == "Middle" & is.na(DBH_before) & !is.na(DBH))
    if (is.data.frame(filled) && nrow(filled) > 0) {
      QAQC_Report <<- dplyr::bind_rows(
        QAQC_Report,
        log_change(
          filled,
          field = "DBH",
          old   = filled$DBH_before,
          new   = filled$DBH,
          issue = "Middle Missing DBH",
          rule  = "Linear interpolation between bounding years only"
        )
      )
    }
    .
  } %>%
  select(-any_of(c("DBH_filled","n_obs","can_interp","interp_vals","first_obs","last_obs")))

#------------------------------------------------------------#
# 6. First Dead Observations Correction----
#------------------------------------------------------------#
#--------------------------------#
## A) Find all first observations that are dead----
#--------------------------------#
# Filter out first dead observations and create variables to test
first.dead <- MI_clean %>% 
  group_by(UniqueID) %>% 
  mutate(n.obs = n()) %>% 
  slice_min(order_by = Year, n = 1) %>% 
  filter(DA == 0)  %>% 
  ungroup() %>% 
  mutate(
    First.Dead = TRUE,
    DBH.NA = is.na(DBH)
  ) %>% 
  select(UniqueID, First.Dead, n.obs, DBH.NA)

# Join info to original data and clean up na's
MI_first.dead <- MI_clean %>% 
  left_join(first.dead, join_by(UniqueID)) %>% 
  mutate(
    First.Dead = replace_na(First.Dead, FALSE),
    DBH.NA = replace_na(DBH.NA, FALSE),
    n.obs = replace_na(n.obs, 0)
  )

#--------------------------------#
## B) Find all first dead that are NA DBH----
#--------------------------------#
# First observation is NA filter
#-----------------#
# ****Note****
#-----------------#
# BFY4_12_433 has a note that says it's alive and well from second obs, so kept.
#-----------------#
# End of Note
#-----------------#
first.na <- MI_first.dead %>% 
  filter(DBH.NA) %>% 
  arrange(n.obs, UniqueID) %>% 
  select(UniqueID, Year, DBH, n.obs, DA, NOTES, OfficeNotes) %>% 
  filter(!UniqueID == 'BFY4_12_433')

# Choose which should be kept based on how many observations don't have NA
# Rule is more less half observations must be good
first.na <- first.na %>% 
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>% 
  mutate(
    first.obs = row_number() == 1,
    t.na = sum(is.na(DBH)),
    na.keep = t.na/n.obs < 0.5,
    obs.alive = sum(DA[!first.obs]),
    obs.keep = if_else(n.obs > 1, obs.alive / (n.obs - 1) > 0.5, FALSE)
  )

# Get names of those removed
first.na.rm <- first.na %>% 
  filter(!na.keep) %>% 
  distinct(UniqueID)

# Get names of those to keep for QAQC action
first.na.kp <- first.na %>% 
  filter(na.keep) %>% 
  distinct(UniqueID)

# Quarantine those that don't pass
first.na.quar <- MI_first.dead %>% 
  semi_join(first.na.rm, by = "UniqueID")

# Quarantine log
first.na.log <- first.na.quar %>% 
  distinct(UniqueID) %>% 
  mutate(Action = 'Most DBH observations are NA, quarantined for final check')

# Remove from data
MI_first.dead.narm <- MI_first.dead %>% 
  anti_join(first.na.rm, by = "UniqueID") %>% 
  mutate(
    QAQC_Action = case_when(
      UniqueID %in% first.na.kp$UniqueID ~ if_else(QAQC_Action != 'Fine',
                                                   str_glue("{QAQC_Action}; First observation dead and NA, but enough alive observations after to keep"),
                                                   "First observation dead and NA, but enough alive observations after to keep"
      ),
      TRUE ~ QAQC_Action
    )
  )

#--------------------------------#
## C) Remove Dead first that are only 1 or 2 observations----
#--------------------------------#
# Filter first dead but has DBH and only 1 to 2 observations
first.dead.1.2.ob.quar <- MI_first.dead.narm %>% 
  filter(First.Dead, n.obs %in% c(1, 2), !UniqueID %in% "BFY4_12_433")

# Separate between all dead
all.dead.quar <- first.dead.1.2.ob.quar %>% 
  group_by(UniqueID) %>% 
  filter(sum(DA) == 0)

# All dead log
all.dead.log <- all.dead.quar %>% 
  distinct(UniqueID) %>% 
  mutate(Action = 'All these observations were recorded dead, quarantined')

# Separate for those that come back alive next round
back.alive.quar <- first.dead.1.2.ob.quar %>% 
  group_by(UniqueID) %>% 
  filter(sum(DA) > 0)

# Back alive log
back.alive.log <- back.alive.quar %>% 
  distinct(UniqueID) %>% 
  mutate(Action = 'Two observations, first dead, second alive, look over quarantined')

# Remove from data
MI_first.dead.narm.1.2.rm <- MI_first.dead.narm %>% 
  anti_join(first.dead.1.2.ob.quar, join_by(UniqueID))

#--------------------------------#
## D) First dead multi observation (most dead)----
#--------------------------------#
# Quarantine first dead that have mostly dead observations
discrep.dead.quar <- MI_first.dead.narm.1.2.rm %>%
  group_by(UniqueID) %>%
  filter(n.obs > 0, sum(DA)/n.obs <= 0.5)

# Most dead log
discrep.dead.log <- discrep.dead.quar %>% 
  distinct(UniqueID) %>% 
  mutate(Action = 'First dead recorded DBH, the rest mostly dead. Quarantined.')

# Remove from data
MI_first.dead.final.step <- MI_first.dead.narm.1.2.rm %>%
  anti_join(discrep.dead.quar, join_by(UniqueID))

#--------------------------------#
## E) Find potential tag change or actual alive----
#--------------------------------#
# Find any weird DBH jumps that might mean tag changed trees
weird.dead <- MI_first.dead.final.step %>% 
  filter(First.Dead & !DBH.NA) %>% 
  select(UniqueID, Year, DBH, DA) %>% 
  group_by(UniqueID) %>% 
  arrange(Year, .by_group = TRUE) %>% 
  mutate(
    Year.diff = lead(Year) - Year,
    DBH.diff = lead(DBH) - DBH,
    DBH.change = DBH.diff / Year.diff
  )

# Summarize the growth
weird.dead.sum <- weird.dead %>% 
  summarise(
    first.change = DBH.change[1],
    later.change = mean(DBH.change[-1], na.rm = TRUE)
  )

# Dead-Alive-Missing(Alive) (Use for QAQC-Action)
first.dead.last.missing <- weird.dead.sum %>% 
  filter(is.na(later.change))

# Fine and can keep and change first dead to alive
actual.alive <- weird.dead.sum %>% 
  filter(
    later.change == 0 |
      first.change < 0 |
      first.change < 2 * abs(later.change)
  ) %>% 
  anti_join(first.dead.last.missing, join_by(UniqueID))

# Potential new tree
p.new.tree <- weird.dead.sum %>% 
  anti_join(actual.alive, join_by(UniqueID)) %>% 
  anti_join(first.dead.last.missing, join_by(UniqueID))

# Quarantine potential new tree tagged
p.new.tree.quar <- MI_first.dead.final.step %>% 
  semi_join(p.new.tree, join_by(UniqueID))

# New tree tagged log
p.new.tree.log <- p.new.tree.quar %>% 
  distinct(UniqueID) %>% 
  mutate(Action = 'These trees showed much lower DBH growth after first dead obs, potentially ID tag moved. Quarantined for examination.')

# Remove from data and qaqc log the remaining
MI_clean <- MI_first.dead.final.step %>% 
  anti_join(p.new.tree, join_by(UniqueID)) %>% 
  mutate(
    DA = if_else(UniqueID %in% actual.alive$UniqueID & DA == 0, 1, DA),
    QAQC_Action = case_when(
      UniqueID %in% first.dead.last.missing$UniqueID ~ if_else(QAQC_Action != 'Fine',
                                                               str_glue('{QAQC_Action}; The first observation was dead, second alive, third missing (Kept in data for now)'),
                                                               'The first observation was dead, second alive, third missing (Kept in data for now)'),
      UniqueID %in% actual.alive$UniqueID ~ if_else(QAQC_Action != 'Fine',
                                                    str_glue('{QAQC_Action}; The first dead observation was likely alive and first observation DA was changed from 0 to 1'),
                                                    'The first dead observation was likely alive and first observation DA was changed from 0 to 1'),
      TRUE ~ QAQC_Action
    )
  ) %>% 
  select(-First.Dead, -n.obs, -DBH.NA)

#--------------------------------#
## F) Save Quarantine and Logs----
#--------------------------------#


# Combine the logs
first.dead.log <- bind_rows(first.na.log, all.dead.log, back.alive.log, 
                            discrep.dead.log, p.new.tree.log)

# Save logs
save_qaqc(first.dead.log, "Log", "FirstDead")

# Save first dead and NA with most NA
save_qaqc(first.na.quar, "Quarantine", "FirstDead_MostObsNA")

# Save all dead 2 max observation
save_qaqc(all.dead.quar, "Quarantine", "FirstDead_AllDead_Max2Obs")

# Save back alive 2 observation
save_qaqc(back.alive.quar, "Quarantine", "FirstDead_SecondObsAlive")

# Save most observations are dead (3+ observations)
save_qaqc(discrep.dead.quar, "Quarantine", "FirstDead_MostObsDead_3plusObs")

# Save ones that could have had tree tag switched ot new tree
save_qaqc(p.new.tree.quar, "Quarantine", "FirstDead_PotentialNewTree")

#------------------------------------------------------------#
# 7. Decimal Error Correction----
#------------------------------------------------------------#
#--------------------------------#
## A) First observation decimal error----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    DBH_before_decimal = DBH,
    n_obs = sum(!is.na(DBH)),
    First_Year = !is.na(DBH) & row_number() == which.max(!is.na(DBH)),
    
    # HIGH: first obs is 10x next obs -> divide by 10 (fix)
    First_Decimal_Error_High = if_else(
      n_obs > 1 & First_Year & !is.na(DBH) & lead(!is.na(DBH)),
      DBH >= lead(DBH) * 10,
      FALSE
    ),
    # LOW: first obs is 10x smaller than next obs -> multiply by 10 (FLAG ONLY)
    First_Decimal_Error_Low = if_else(
      n_obs > 1 & First_Year & !is.na(DBH) & lead(!is.na(DBH)) & DBH < 1,
      DBH <= lead(DBH) / 10,
      FALSE
    ),
    # apply only HIGH fix
    DBH = case_when(
      First_Decimal_Error_High ~ DBH / 10,
      TRUE ~ DBH
    ),
    
    QAQC_Action = case_when(
      First_Decimal_Error_High ~ if_else(QAQC_Action != "Fine",
                                         str_glue("{QAQC_Action}; Fixed_High_Decimal_FirstObs"),
                                         "Fixed_High_Decimal_FirstObs"),
      First_Decimal_Error_Low  ~ if_else(QAQC_Action != "Fine",
                                         str_glue("{QAQC_Action}; Flagged_Low_Decimal_FirstObs"),
                                         "Flagged_Low_Decimal_FirstObs"),
      TRUE ~ QAQC_Action
    )
  ) %>%
  {
    fixed_high_first <- dplyr::filter(., First_Decimal_Error_High)
    if (is.data.frame(fixed_high_first) && nrow(fixed_high_first) > 0) {
      QAQC_Report <<- dplyr::bind_rows(
        QAQC_Report,
        log_change(
          fixed_high_first,
          field = "DBH",
          old   = fixed_high_first$DBH_before_decimal,
          new   = fixed_high_first$DBH,
          issue = "Decimal Error",
          rule  = "DBH >= 10x next year (divide by 10)"
        )
      )
    }
    flagged_low_first <- dplyr::filter(., First_Decimal_Error_Low)
    if (is.data.frame(flagged_low_first) && nrow(flagged_low_first) > 0) {
      QAQC_Report <<- dplyr::bind_rows(
        QAQC_Report,
        log_change(
          flagged_low_first,
          field = "DBH",
          old   = flagged_low_first$DBH_before_decimal,
          new   = flagged_low_first$DBH_before_decimal,
          issue = "Decimal Error (Flag Only)",
          rule  = "First obs DBH <= 1/10 next obs (flag only; no auto-fix)"
        )
      )
    }
    .
  } %>%
  ungroup()

#--------------------------------#
## B) In between observation decimal errors----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    DBH_before_decimal = DBH,
    
    # identify first + last measured DBH rows (ignore trailing NA years)
    first_meas_idx = if (any(!is.na(DBH))) min(which(!is.na(DBH))) else NA_integer_,
    last_meas_idx  = if (any(!is.na(DBH))) max(which(!is.na(DBH))) else NA_integer_,
    IsMiddleMeas   = !is.na(DBH) & row_number() != first_meas_idx & row_number() != last_meas_idx,
    
    # classic: current is 10x previous -> divide by 10 (MIDDLE ONLY)
    Decimal.Error.Up =
      IsMiddleMeas &
      !is.na(lag(DBH)) &
      !str_detect(lag(QAQC_Action), "Flagged_Low_Decimal_FirstObs") &
      DBH >= lag(DBH) * 10,
    
    DBH = case_when(
      Decimal.Error.Up ~ DBH / 10,
      TRUE ~ DBH
    ),
    
    QAQC_Action = case_when(
      Decimal.Error.Up ~ if_else(QAQC_Action != "Fine",
                                 str_glue("{QAQC_Action}; Fixed_High_Decimal"),
                                 "Fixed_High_Decimal"),
      TRUE ~ QAQC_Action
    )
  ) %>%
  {
    fixed_up <- dplyr::filter(., Decimal.Error.Up)
    if (is.data.frame(fixed_up) && nrow(fixed_up) > 0) {
      QAQC_Report <<- dplyr::bind_rows(
        QAQC_Report,
        log_change(
          fixed_up,
          field = "DBH",
          old   = fixed_up$DBH_before_decimal,
          new   = fixed_up$DBH,
          issue = "Decimal Error",
          rule  = "MIDDLE ONLY: DBH >= 10x previous year (divide by 10)"
        )
      )
    }
    .
  } %>%
  ungroup() %>%
  select(-first_meas_idx, -last_meas_idx, -IsMiddleMeas)



#--------------------------------#
## C) Last observation decimal error----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    n_obs = sum(!is.na(DBH)),
    Last_Year = !is.na(DBH) & row_number() == max(which(!is.na(DBH))),
    
    # last obs is 10x previous obs -> divide by 10
    Last_Decimal_Error_High = if_else(
      n_obs > 1 & Last_Year & !is.na(DBH) & lag(!is.na(DBH)),
      DBH >= lag(DBH) * 10,
      FALSE
    ),
    
    DBH = case_when(
      Last_Decimal_Error_High ~ DBH / 10,
      TRUE ~ DBH
    ),
    
    QAQC_Action = case_when(
      Last_Decimal_Error_High ~ if_else(QAQC_Action != "Fine",
                                        str_glue("{QAQC_Action}; Fixed_High_Decimal_LastObs"),
                                        "Fixed_High_Decimal_LastObs"),
      TRUE ~ QAQC_Action
    )
  ) %>%
  {
    fixed_high_last <- dplyr::filter(., Last_Decimal_Error_High)
    if (is.data.frame(fixed_high_last) && nrow(fixed_high_last) > 0) {
      QAQC_Report <<- dplyr::bind_rows(
        QAQC_Report,
        log_change(
          fixed_high_last,
          field = "DBH",
          old   = fixed_high_last$DBH_before_decimal,
          new   = fixed_high_last$DBH,
          issue = "Decimal Error",
          rule  = "DBH >= 10x previous year (divide by 10)"
        )
      )
    }
    .
  } %>%
  ungroup()


#------------------------------------------------------------#
# 8. Spike / Dip Detection + Correction----
#------------------------------------------------------------#
#--------------------------------#
## A) Prep for DBH changes----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    # index of the last non-NA DBH row within the UniqueID
    last_meas_idx = if (any(!is.na(DBH))) max(which(!is.na(DBH))) else NA_integer_,
    
    # TRUE only for the last measured DBH observation (ignores trailing NA rows)
    IsLastMeas = !is.na(DBH) & (row_number() == last_meas_idx),
    year_diff = Year - lag(Year),
    
    prev_DBH     = lag(DBH),
    next_DBH     = lead(DBH),
    dbh_change   = DBH - prev_DBH,
    next_change  = next_DBH - DBH,
    
    dbh_year_change = if_else(
      !is.na(dbh_change) & !is.na(year_diff) & year_diff > 0,
      dbh_change / year_diff,
      NA_real_
    ),
    
    DBH_before_fix = DBH
  ) %>% 
  ungroup()

#--------------------------------#
## B) Detect Spikes----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(    
    high_spike = case_when(
      coalesce(str_detect(lag(QAQC_Action), "Flagged_Low_Decimal_FirstObs"), TRUE) ~ FALSE,
      # species-based plausibility
      !is.na(Species_Max_Growth_Error) & !is.na(dbh_year_change) &
        dbh_year_change > Species_Max_Growth_Error ~ TRUE,
      
      # 3-point spike pattern
      !is.na(prev_DBH) & !is.na(next_DBH) & !is.na(dbh_change) & !is.na(next_change) ~
        (DBH > (((prev_DBH + next_DBH) / 2) + 1.5)) &
        (next_change < 0) &
        (abs(next_change) < dbh_change) &
        (dbh_change > -1),
      
      # absolute insane spike fallback
      !is.na(dbh_change) ~ dbh_change >= 10,
      
      TRUE ~ FALSE
    ),
    high_spike = replace_na(high_spike, FALSE)
  ) 
#--------------------------------#
## C) Detect Dips----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate( 
    extreme_dip = if_else(
      lag(high_spike) %in% TRUE, FALSE,
      !is.na(dbh_change) & dbh_change <= -1.5
    ),
    extreme_dip = replace_na(extreme_dip, FALSE),
    
    # don't allow a spike immediately after an extreme dip
    high_spike = if_else(lag(extreme_dip) %in% TRUE, FALSE, high_spike),
    
    # ---- Your dip rule (lead-1 only; within 1 cm of pre-dip)
    Dip.Rebound = extreme_dip & !is.na(next_DBH) & !is.na(prev_DBH) & (next_DBH >= (prev_DBH - 1)),
    Fix.Dip     = extreme_dip & Dip.Rebound,
    New.Growth  = extreme_dip & !Dip.Rebound,
    
    Fix      = Fix.Dip | high_spike,
    Last.obs = Fix & IsLastMeas,
    
    LastObs.Dip = extreme_dip & IsLastMeas
  ) 
#--------------------------------#
## D) Propagate new growth forward----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    NewGrowth_StartYear = if_else(
      any(New.Growth, na.rm = TRUE),
      min(Year[New.Growth], na.rm = TRUE),
      NA_integer_
    ),
    New.Growth = if_else(
      !is.na(NewGrowth_StartYear) & Year >= NewGrowth_StartYear,
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
## E) Apply corrections (MID-SERIES ONLY) + flag last obs only----
#--------------------------------#
MI_clean <- MI_clean %>%
  group_by(UniqueID) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    IsLastObs = IsLastMeas,
    
    FixAllowed_Mid = Fix & !IsLastObs & !Noted & !is.na(prev_DBH) & !is.na(next_DBH),
    
    # this identifies anomalies we WOULD have fixed, except notes prevent it
    NotedBlockedFix = Fix & !IsLastObs & Noted,
    
    DBH = case_when(
      FixAllowed_Mid ~ (prev_DBH + next_DBH) / 2,
      TRUE ~ DBH
    ),
    DBH = round(DBH, 2),
    
    Flag_LastObs_SpeciesSpike =
      IsLastObs & !is.na(Species_Max_Growth_Error) & !is.na(dbh_year_change) &
      dbh_year_change > Species_Max_Growth_Error,
    
    QAQC_Action = case_when(
      # fixed mid-series
      FixAllowed_Mid & high_spike & 
        DBH != DBH_before_fix ~ if_else(QAQC_Action != "Fine",
                                        str_glue("{QAQC_Action}; Fixed_Spike_Mid"),
                                        "Fixed_Spike_Mid"),
      FixAllowed_Mid & Fix.Dip & 
        DBH != DBH_before_fix ~ if_else(QAQC_Action != "Fine",
                                        str_glue("{QAQC_Action}; Fixed_Dip_Mid"),
                                        "Fixed_Dip_Mid"),
      
      # not fixed because notes exist
      NotedBlockedFix & high_spike ~ if_else(QAQC_Action != "Fine",
                                             str_glue("{QAQC_Action}; Flag_Spike_NotFixed_HasNote_Check"),
                                             "Flag_Spike_NotFixed_HasNote_Check"),
      NotedBlockedFix & Fix.Dip    ~ if_else(QAQC_Action != "Fine",
                                             str_glue("{QAQC_Action}; Flag_Dip_NotFixed_HasNote_Check"),
                                             "Flag_Dip_NotFixed_HasNote_Check"),
      
      # last obs flags (no fixes)
      Flag_LastObs_SpeciesSpike ~ if_else(QAQC_Action != "Fine",
                                          str_glue("{QAQC_Action}; Flag_LastObs_SpeciesGrowthSpike"),
                                          "Flag_LastObs_SpeciesGrowthSpike"),
      
      # keep your existing flags
      LastObs.Dip ~ if_else(QAQC_Action != "Fine",
                            str_glue("{QAQC_Action}; Flag_LastObsExtremeDip"),
                            "Flag_LastObsExtremeDip"),
      New.Growth ~ if_else(QAQC_Action != "Fine",
                           str_glue("{QAQC_Action}; Flag_NewGrowth_Maybe?"),
                           "Flag_NewGrowth_Maybe?"),
      
      TRUE ~ QAQC_Action
    )
  ) %>%
  ungroup()

#--------------------------------#
## F) Log QAQC----
#--------------------------------#
fixed_spike_dip <- MI_clean %>%
  filter(FixAllowed_Mid, !is.na(DBH_before_fix), !is.na(DBH), DBH != DBH_before_fix)

if (nrow(fixed_spike_dip) > 0) {
  QAQC_Report <- bind_rows(
    QAQC_Report,
    tibble(
      ReplaceID = fixed_spike_dip$ReplaceID,
      UniqueID  = fixed_spike_dip$UniqueID,
      Year      = fixed_spike_dip$Year,
      Field     = "DBH",
      Old_Value = as.character(fixed_spike_dip$DBH_before_fix),
      New_Value = as.character(fixed_spike_dip$DBH),
      Issue     = dplyr::case_when(
        fixed_spike_dip$high_spike ~ "Spike Correction",
        fixed_spike_dip$Fix.Dip    ~ "Dip Correction"
      ),
      Rule      = dplyr::case_when(
        fixed_spike_dip$high_spike ~ "Mid-series SPIKE fix: mean of neighboring years (Noted=FALSE)",
        fixed_spike_dip$Fix.Dip    ~ "Mid-series DIP fix: mean of neighboring years (Noted=FALSE; dip rebounds within 1 cm)"
      )
    )
  )
}

#------------------------------------------------------------#
# 9. Review Tables + Plot Dataset----
#------------------------------------------------------------#

Anomaly_Review  <- MI_clean %>% filter(Anomaly & Noted)
NewGrowth_Review <- MI_clean %>% filter(New.Growth)

MI_plot <- MI_clean %>%
  filter(!is.na(DBH))

#------------------------------------------------------------#
# 10. Visualization (action colors, DA shapes, SPECIES lines)----
#------------------------------------------------------------#

site_list <- unique(MI_plot$SITE)

for (s in site_list) {
  gdat <- MI_plot %>% filter(SITE == s)
  
  p <- ggplot(gdat, aes(x = Year, y = DBH, group = UniqueID)) +
    geom_line(
      aes(linetype = SPECIES),
      color = "grey30",
      linewidth = 0.5,
      alpha = 0.7
    ) +
    geom_point(
      aes(color = QAQC_Action, shape = factor(DA_raw)),
      size = 2
    ) +
    facet_wrap(~PLOT) +
    theme_classic() +
    ggtitle(str_glue("Site {s} QAQC (Point color = QAQC_Action; Shape = DA; Line type = SPECIES)"))
  
  p.name <- str_glue("RSN_Trees_QAQC_Plot_{s}.png")
  
  ggsave(
    plot = p,
    filename = path(plot.dir, p.name),
    width = 10, height = 6
  )
}

#------------------------------------------------------------#
# 11. Clean out columns----
#------------------------------------------------------------#
MI_clean <- MI_clean %>%
  select(
    -any_of(c(
      # duplicate-handling helpers
      "Duplicate.DBH.Species", "Row.Order", "Duplicate.DBH", "most_common",
      
      # species correction helpers
      "SPECIES_before_fill", "SPECIES_before_note", "species_single",
      "Notes_all", "Note_tokens", "n_note_species", "Note_species_single",
      
      # misc / earlier steps (keep if present)
      "Missing.Place", "DBH_before", "can_interp", "interp_vals",
      "first_obs", "last_obs", "n_obs",
      
      # decimal error helpers (older + newer naming variants)
      "DBH_before_decimal", "Decimal.Error.Up", "Decimal.Error.Down",
      "First_Year", "First_Obs", "IsFirstObsRow", "First_DBH_median", "n_first",
      "First.Decimal.Error.Up", "First.Decimal.Error.Down",
      
      # spike/dip helpers (UPDATED to match your current Section 7)
      "last_meas_idx", "IsLastMeas", "IsLastObs",
      "year_diff", "prev_DBH", "next_DBH", "dbh_change", "next_change",
      "dbh_year_change", "DBH_before_fix",
      "high_spike", "extreme_dip", "Dip.Rebound", "Fix.Dip", "Fix",
      "Last.obs", "LastObs.Dip", "NewGrowth_StartYear",
      "Anomaly_Type", "FixAllowed_Mid", "NotedBlockedFix",
      
      # legacy spike/dip columns from older versions (safe to include)
      "DBH_before_spikefix",
      "mean.dbh.change", "PlotSpecies_Growth", "n_ps",
      "LastSpikeGrowthAvail", "DeadSpikeConflict", "FixAllowed", "NoNeighbors"
    ))
  )

#------------------------------------------------------------#
# 12. Save Outputs----
#------------------------------------------------------------#
#---Save files---#
# Final data (cleaned)
save_qaqc(MI_clean, "Final", "CleanedInventory")

# Anomoly qaqc log
save_qaqc(Anomaly_Review, "Log", "AnomalyNoted")

# New growth qaqc log
save_qaqc(NewGrowth_Review, "Log", "NewGrowthFlagged")

# Full qaqc report
save_qaqc(QAQC_Report, "Log", "ChangeReport")

#---Upload all outputs to Google Drive data/processed---#
drive.proc <- as_dribble("data/processed/")

walk(out.files, \(f) drive_upload(
  media = f, path = drive.proc, name = path_file(f), overwrite = TRUE
))

#=============================================================================#
# END SCRIPT
#=============================================================================#

