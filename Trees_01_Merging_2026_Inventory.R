## Appending the 2026 Data!

# Read in the 2026 data
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(googledrive)
`%!in%` = Negate(`%in%`)

# 1) Create all of the folders needed
dir.create(file.path('data'), showWarnings = F)
dir.create(file.path('data', 'raw'), showWarnings = F)
dir.create(file.path('data', 'metadata'), showWarnings = F)
dir.create(file.path('data', 'processed'), showWarnings = F)
dir.create(file.path('data', 'raw', '2026_inventory_RSN'), showWarnings = F)

# 2) Download 2026 Data
# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1W8aY5bW8-syFAwrbES_D2etwbC-Akmqh")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.xlsx$"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path('data', 'raw', '2026_inventory_RSN'))
files_local

# Overwrite local data files?
update <- TRUE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# 3) Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path('data', 'raw', '2026_inventory_RSN', .y)))


# 4. Get all matching files (ignore.case handles All/all/ALL)
files <- list.files(path = file.path('data', 'raw', '2026_inventory_RSN'),
                    pattern = "^2026_.*_Trees_All\\.xlsx$",
                    full.names = TRUE,
                    ignore.case = TRUE)

# 5. Pull out the "XXX" portion, case-insensitively
site_names <- str_extract(basename(files),
                          regex("(?<=^2026_)[^_]+(?=_Trees_All\\.xlsx$)", ignore_case = TRUE))

# 6. Read every file with your function, naming each result by its site code

# create a function to read in the data
read_tree_sheet <- function(path) {
  
  # Read the ENTIRE sheet as raw text, no column names yet
  # (col_types = "text" also avoids the "Expecting numeric... got a date" warnings,
  # since nothing is being type-guessed at this stage)
  raw <- read_excel(path, col_names = FALSE, col_types = "text")
  
  row1 <- trimws(as.character(raw[1, ]))
  row2 <- trimws(as.character(raw[2, ]))
  
  col_names <- ifelse(is.na(row2) | row2 == "",
                      row1,
                      paste0(row1, "_", row2))
  
  # Make any duplicate column names unique (e.g., "NOTES", "NOTES" -> "NOTES", "NOTES_1")
  col_names <- make.unique(col_names, sep = "_")
  
  # Drop the two header rows; the rest is data
  data <- raw[-c(1, 2), ]
  names(data) <- col_names
  
  # Everything was read as text above -- convert numeric-looking columns back
  data <- type_convert(data, guess_integer = FALSE)
  
  data
}

# drop FP5D because format is different
# Drop FP5D (case-insensitive, in case it's sometimes "fp5d" or "Fp5d")
keep <- toupper(site_names) != "FP5D"
files <- files[keep]
site_names <- site_names[keep]

site_list <- files %>%
  set_names(site_names) %>%
  map(read_tree_sheet)


# 7 Create a dataframe iwht a column lookup for this, will use this to merge 
column_name_lookup <- imap_dfr(site_list, ~ tibble(site = .y, raw_name = names(.x))) %>%
  mutate(clean_name = NA_character_)

# Read in FP5D and merge that here
FP5D <- read_excel(path = file.path('data', 'raw', '2026_inventory_RSN', '2026_FP5D_Trees_all.xlsx')) %>%
  # I think will want to coalesce the notes for FP5D
  mutate(NOTES = coalesce(...15, ...16,...17)) %>%
  rename(DBH = "2026.0")

glimpse(FP5D)
fp5d_colnames <- tibble(
  site = "FP5D",
  raw_name = colnames(FP5D),
  clean_name = NA_character_)

# Join FP5D into the column lookup
column_lookup <-rbind(column_name_lookup, fp5d_colnames)


# Export the column lookup and will manually add it 
##write.csv(column_lookup, '/Users/olhajek/Desktop/RSN/RSN_proj/Data/2026 Data/Data_checks/column_lookup_2026_data2.csv', row.names = F)

# 8 Read in teh lookup column
match <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/18bWXqzXFJNiO5GpMecTAB3qB4PYk_OdK")) %>%
  dplyr::filter(name == "column_lookup_2026_data.csv")

# Did that work?
match 

# Download the excluded treatmetn file
googledrive::drive_download(file = match$id, overwrite = T, type = "csv",
                            path = file.path('data', 'metadata', match$name))

# read it in
match <- read.csv(file.path('data', 'metadata','column_lookup_2026_data.csv')) %>%
  filter(!is.na(clean_name) & trimws(clean_name) != "")


# fix that NA name in UP4d
names(site_list$UP4D)
names(site_list$UP4D)[is.na(names(site_list$UP4D))] <- "NOTES"

names(site_list$BDm1)

site_list_clean <- imap(site_list, function(df, site_id) {
  
  site_lookup <- match %>%
    filter(site == site_id, raw_name %in% names(df))
  
  df %>%
    select(all_of(site_lookup$raw_name)) %>%
    rename(!!!setNames(as.list(site_lookup$raw_name), site_lookup$clean_name))
})

# join the site list
# lots of inconsistencies and errors in the data
map(site_list_clean, ~sapply(.x, class))

# going to convert everything to character then individually convert back
# combining here
all_sites <- map(site_list_clean, ~ mutate(.x, across(everything(), as.character))) %>%
  bind_rows(.id = "site")

# going to add in FP5D now!

site_lookup <- match %>%
  filter(site == "FP5D")

fp5d.2 <- FP5D %>%
  select(all_of(site_lookup$raw_name)) %>%
  rename(!!!setNames(as.list(site_lookup$raw_name), site_lookup$clean_name)) %>%
  mutate(across(everything(), as.character))
str(fp5d.2)

all_site2 <- all_sites %>%
  bind_rows(fp5d.2)

## Make sure to remove any totally blank rows, site, add in the same rows that are present in the raw data set
glimpse(all_site2)

unique(all_site2$SITE)
unique(all_site2$PLOT)
unique(all_site2$SPECIES)
unique(all_site2$TOP)
unique(all_site2$LEAN)
unique(all_site2$BOWED)
unique(all_site2$DOWN)

all <- all_site2 %>%
  # remove site column 
  select(-site)%>%
  #remove any all blank rows
  filter(!if_all(everything(), is.na)) %>% 
  # remove these two rows because they are duplicates and will be corrected in the PLOT line
  filter(is.na(DBH) | DBH != "Entered in plot 2") %>%
  filter(is.na(DBH) | DBH != "this is now tree 357") %>%
  # start to clean up the data frame
  mutate(SITE = case_when(
    SITE == "BDMI" ~ "BDM1",
    SITE == "GAM4" ~ "GSM4",
    SITE == "Up4b" ~ "UP4B",
    TRUE ~ SITE
  ), 
  PLOT = case_when(
    is.na(PLOT) & SITE == "GSM2" ~ "12", 
    is.na(PLOT) & SITE == "UP4B" ~ "12",
    SITE == "UP4C" & TREE == "30" & PLOT == "2" ~ "3",
    SITE == "UP4C" & TREE == "31" & PLOT == "2" ~ "3",
    TRUE ~PLOT
  ), 
  TREE = ifelse(is.na(TREE), "2579", TREE), 
  # NOTE THERE IS an 802a and 801A in UP4B. neither of these show up before, but just adding them here now
  TREE = as.integer(str_remove(TREE, "[Aa]$")), 
  SPECIES = case_when(
    SPECIES == "Picmar" ~"PICMAR",
    SPECIES == "PICMA" ~"PICMAR",
    # assuming these trees are PICMAR; slight chance could be LARLAR, but likely PICMAR and these are NEW
    SPECIES == "UP4C" ~ "PICMAR", 
    is.na(SPECIES) ~ "PICMAR",
    TRUE ~ SPECIES), 
  DBH = case_when(
    str_starts(DBH, "-7") ~ "-7777",
    str_starts(DBH, "-8") ~ "-8888",
    str_starts(DBH, "-9") ~ "-9999",
    TRUE ~ DBH), 
  TOP = as.numeric(gsub("m", "", TOP)), 
  LEAN = gsub("m", "", LEAN), 
  # many NOTES indicate leaning or bowed, and making that update here
  LEAN = if_else(
    str_detect(NOTES, regex("lean", ignore_case = TRUE)),
    "leaning",
    LEAN
  ),
  TOP = if_else(
    str_detect(NOTES, regex("topp", ignore_case = TRUE)),
    as.numeric(str_extract(
      str_extract(
        NOTES,
        regex("topp.*?\\d+(?:\\.\\d+)?", ignore_case = TRUE)
      ),
      "\\d+(?:\\.\\d+)?"
    )),
    as.numeric(TOP)
  ),
  BOWED = if_else(
    str_detect(NOTES, regex("bowed", ignore_case = TRUE)),
    "bowed",
    BOWED), 
  DOWN = ifelse(DBH == -9999, 0, 1),
  DBH = ifelse(is.na(DBH), -8888, DBH))%>%
  mutate(UniqueID = paste(SITE,PLOT,TREE, sep="_"), 
         DATE = "6/15/2026", DA = ifelse(DBH %in% c(-7777, -9999), 0, 1)) %>%
  # Drop these two rows because they were entered wrongly and they've been updated in the previous section
  filter(UniqueID %!in% c("UP4C_2_30", "UP4C_2_31"))

unique(all$SITE)
unique(all$PLOT)
unique(all$SPECIES)
unique(all$TOP)
unique(all$LEAN)

# There are still a few DBH to update where it was read in as a DATE versus a numeric format
all <- all %>%
  mutate(DBH = case_when(
    UniqueID == "GSM2_6_877" ~ "9.3",
    UniqueID == "UP4A_13_888"~ "8.7",
    UniqueID == "TRM4_4_171"~ "7.3",
    UniqueID == "TRM4_2_91"~ "5.5",
    UniqueID == "BDM1_14_1531"~ "5.3",
    UniqueID == "BDM1_11_1390"~ "4.7",
    UniqueID == "BDM1_13_1461"~ "4.5",
    UniqueID == "TRM4_8_319"~ "4.5",
    UniqueID == "WCM1_3_472"~ "3.5",
    UniqueID == "BDM1_14_1540"~ "3.4",
    UniqueID == "UP4D_8_371"~ "2.7",
    UniqueID == "YRM1_7_2520"~ "2.6",
    TRUE ~ DBH
  ))

# NOTES
# TREE 357 is the old 26 in UP4A Plot 1 - this doesn't seem to be updated in the final script either
# deifnitley some tag changes that should reflect on 


# 9) Join this with the old data frame and then export 
current_rsn  <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1NBCDoIoIhiqlI1GXCh7P0YpavuMkv1dm")) %>%
  dplyr::filter(name == "320_TreeInventory_1989-2025FinalforJasonRecentUpdates021126.csv")

# Did that work?
current_rsn 

# Download the excluded treatmetn file
googledrive::drive_download(file = current_rsn$id, overwrite = T, type = "csv",
                            path = file.path('data', 'raw', current_rsn$name))

current_rsn <- read.csv(file.path('data', 'raw','320_TreeInventory_1989-2025FinalforJasonRecentUpdates021126.csv'),
                        stringsAsFactors = FALSE, encoding = "latin1"
)

# join teh two
glimpse(current_rsn)
glimpse(all)
# missing PLOT.AREA..m.., LOCATION, SEVERITY, DAMAGE

all.join <- all %>%
  mutate(
    PLOT.AREA..m.. = 100, 
    LOCATION = "", SEVERITY = "", DAMAGE = "", 
    TREE = as.character(TREE),
    TOP = as.character(TOP),
    DA = as.integer(DA)
  )

rsn.join <- current_rsn %>%
  mutate(
    LEAN = as.character(LEAN),
    DA = as.integer(DA),
    DOWN = as.character(DOWN),
    BOWED = as.character(BOWED)
  ) 

joined <- rbind(rsn.join, all.join)

# Fix a few notes based on 2026 data
# proceeded to make two updates with tree FP5D_1_2947 (was 1194) and BDM1_1_1054 (2899)
glimpse(joined)
joined.2 <- joined %>%
  mutate(TREE = case_when(
    UniqueID == "FP5D_1_1194" ~ "2947", 
    UniqueID == "BDM1_1_1054" ~ "2899", 
    TRUE ~ TREE
  ),
  UniqueID = case_when(
    UniqueID == "FP5D_1_1194" ~ "FP5D_1_2947", 
    UniqueID == "BDM1_1_1054" ~ "BDM1_1_2899", 
    TRUE ~ UniqueID
  )) %>%
  filter(!(UniqueID == "BDM1_1_2899" & DBH == "-8888"))


## Export the raw data
#write.csv(joined.2, file.path('data','processed','320_TreeInventory_1989-2026_OLH.csv'), row.names = F)

# drive_upload(
#   media     = file.path('data','processed','320_TreeInventory_1989-2026_OLH.csv'),
#   path      = as_dribble("data/processed/"),
#   name      = '320_TreeInventory_1989-2026_OLH.csv',
#   overwrite = TRUE
# )


