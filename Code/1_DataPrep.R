# ============================================================
# SAVE THIS AS: Code/startup.R
# SOURCE AT START OF EVERY SESSION: source("Code/startup.R")
# ============================================================

# ============================================================
# Code/startup.R
# SOURCE AT START OF EVERY SESSION: source("Code/startup.R")
# ============================================================

# Set persistent library path
.libPaths(c("/home/jovyan/R/library", .libPaths()))

# Load all packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(readxl)
  library(vegan)
  library(cowplot)
  library(lme4)
  library(glmmTMB)
  library(sjPlot)
  library(broom)
  library(stringr)
  library(brms)
  library(cmdstanr)
  library(DHARMa)
  library(ggeffects)
  library(marginaleffects)
  library(ggrepel)
  library(paletteer)
  library(MASS)
  library(patchwork)
  library(gllvm)
  library(permute)
})

# Fix common masking conflicts
select    <- dplyr::select
filter    <- dplyr::filter
summarise <- dplyr::summarise
rename    <- dplyr::rename
ordiplot  <- gllvm::ordiplot

cat("All packages loaded successfully\n")
cat("Session started:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("All packages loaded successfully\n")
cat("Session started:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

# ============================================================
# LOAD DATA
# ============================================================
Wide_3 <- read_csv("Data/Wide 4_qaqc.csv")
str(Wide_3)
names(Wide_3)

# ============================================================
# DATA WRANGLING
# ============================================================

# Split out the Subtreatment into 2 columns
Wide_3$SubTrt <- substring(Wide_3$Subtreatment, 2)
Wide_3 <- Wide_3 %>% relocate(SubTrt, .after = Treatment)

# Fix names using case_when (cleaner than nested ifelse)
Wide_3 <- Wide_3 %>%
  mutate(
    SubTrt = case_when(
      SubTrt == "C"  ~ "Control",
      SubTrt == "S"  ~ "Scraped + Seeded",
      SubTrt == "M"  ~ "Mowed",
      SubTrt == "MS" ~ "Mowed + Scraped + Seeded",
      .default = SubTrt
    ),
    Treatment = case_when(
      Treatment == "G" ~ "Grazed",
      Treatment == "S" ~ "Seasonal Graze",
      .default = "Ungrazed"
    ),
    Subtreatment = case_when(
      Subtreatment == "GC"  ~ "Grazed",
      Subtreatment == "GS"  ~ "Grazed + Scraped + Seeded",
      Subtreatment == "SC"  ~ "Seasonal Graze",
      Subtreatment == "SS"  ~ "Seasonal Graze + Scraped + Seeded",
      Subtreatment == "UC"  ~ "Ungrazed",
      Subtreatment == "UM"  ~ "Ungrazed + Mowed",
      Subtreatment == "UMS" ~ "Ungrazed + Mowed + Scraped + Seeded",
      Subtreatment == "US"  ~ "Ungrazed + Scraped + Seeded",
      .default = Subtreatment
    )
  )

# Remove columns that sum to zero
Wide_3 <- Wide_3 %>%
  select_if(negate(function(col) is.numeric(col) && sum(col) < 1))

# Check all rows sum > 0
Wide_3[-c(1:5)] %>% filter(if_all(everything(.), ~. != 0))

# Sum dead cover and remove _d columns
DeadSum <- Wide_3 %>%
  dplyr::select(contains("_d")) %>%
  reframe(DeadSum = rowSums(across()))

Wide_4 <- Wide_3 %>%
  dplyr::select(-contains("_d"))

Wide_5 <- bind_cols(Wide_4, DeadSum)

# ============================================================
# SPECIES CODES
# ============================================================
SpeciesCode <- read_excel("Data/Species_qaqc3_CalIPCcategories_lmr_abr2.xlsx")
#view(SpeciesCode)
names(SpeciesCode)

SpeciesCode$FxlGrp <- SpeciesCode$`lmr suggested groups`


# Remove _d species
SpeciesCode <- SpeciesCode %>%
  filter(!str_detect(Species, '_d'))

unique(SpeciesCode$FxlGrp)

# Rename FxlGrp using case_when (cleaner than nested ifelse)
# SpeciesCode <- SpeciesCode %>%
#   mutate(FxlGrp = case_when(
#     FxlGrp == "YAF" ~ "Native-AF",
#     FxlGrp == "YPG" ~ "Native-PG",
#     FxlGrp == "YPF" ~ "Native-PF",
#     FxlGrp == "NAG" ~ "NonNative-AG",
#     FxlGrp == "NPF" ~ "NonNative-PF",
#     FxlGrp == "NAF" ~ "NonNative-AF",
#     .default = FxlGrp
#   ))

# ============================================================
# ENVIRONMENT FILE AND RAINFALL
# ============================================================
Wide_6.env <- Wide_5[, c(1:5)]
Wide_6     <- Wide_5[, -c(1:5)]

RAIN <- read_csv("Data/PINN_PPT_rainfall.csv")
colnames(RAIN)[colnames(RAIN) == 'Water_Year'] <- 'Year'

Wide_6.env <- left_join(Wide_6.env, RAIN, by = "Year")
hist(Wide_6.env$PPT_CM)

# Rainfall categories
Wide_6.env <- Wide_6.env %>%
  mutate(
    Rain.f = case_when(
      PPT_CM < 30 ~ "Low",
      PPT_CM > 60 ~ "High",
      .default    = "Med"
    ),
    Rain.f    = factor(Rain.f, levels = c("Low", "Med", "High")),
    PrePost.f = factor(ifelse(Year == 2021, "Pre", "Post"),
                       levels = c("Pre", "Post"))
  )

# ============================================================
# SET FACTORS
# ============================================================
Wide_6.env <- Wide_6.env %>%
  mutate(
    Year.f       = as.factor(Year),
    PrePost      = as.factor(ifelse(Year < 2022, "A", "B")),
    Treatment    = factor(Treatment,
                          levels = c('Ungrazed', 'Grazed', 'Seasonal Graze')),
    SubTrt       = factor(SubTrt,
                          levels = c('Control', 'Scraped + Seeded',
                                     'Mowed', 'Mowed + Scraped + Seeded')),
    Subtreatment = factor(Subtreatment,
                          levels = c("Ungrazed", "Ungrazed + Scraped + Seeded",
                                     "Ungrazed + Mowed",
                                     "Ungrazed + Mowed + Scraped + Seeded",
                                     "Grazed", "Grazed + Scraped + Seeded",
                                     "Seasonal Graze",
                                     "Seasonal Graze + Scraped + Seeded")),
    Rain.f       = factor(Rain.f, levels = c("Low", "Med", "High"))
  )

Wide_7.env <- Wide_6.env

# ============================================================
# SPECIES GROUPING LOOKUP
# ============================================================
lookup <- data.frame(SpeciesCode[, c(1, 6)])

# Add unique trailing number to FxlGrp
lookup$FxlGrp <- paste0(lookup$FxlGrp, "_", 1:length(lookup$FxlGrp))

# Keep species of interest using case_when
lookup <- lookup %>%
  mutate(FxlGrp = case_when(
    Species == "HIIN"    ~ "HIIN",
    Species == "CESO"    ~ "CESO",
    Species == "ESCA"    ~ "ESCA",
    Species == "DELO"    ~ "DELO",
    Species == "CAEX"    ~ "CAEX",
    Species == "LAGR"    ~ "LAGR",
    Species == "DeadSum" ~ "DeadSum",
    Species == "BG"      ~ "BareGround",
    .default = FxlGrp
  ))

unique(lookup$FxlGrp)

# ============================================================
# BUILD WIDE_7.FxlGrp
# ============================================================
Wide_6.FxlGrp       <- tibble(Wide_6)
Wide_6.FxlGrp.names <- lookup$FxlGrp[match(names(Wide_6.FxlGrp), lookup$Species)]
Wide_6.FxlGrp.names[43] <- "DeadSum"
colnames(Wide_6.FxlGrp) <- Wide_6.FxlGrp.names

Wide_7.FxlGrp <- Wide_6.FxlGrp %>%
  rowwise() %>%
  summarize(
    "Nonnative Forb" = sum(c_across(starts_with("Nonnative Forb")), na.rm = TRUE),
    "Native Grass"    = sum(c_across(starts_with("Native Grass")),    na.rm = TRUE),
    "CAEX"         = sum(c_across(starts_with("CAEX")),         na.rm = TRUE),
    "CESO"         = sum(c_across(starts_with("CESO")),         na.rm = TRUE),
    "Native Forb"    = sum(c_across(starts_with("Native Forb")),    na.rm = TRUE),
    "DELO"         = sum(c_across(starts_with("DELO")),         na.rm = TRUE),
    "Nonnative Grass" = sum(c_across(starts_with("Nonnative Grass")), na.rm = TRUE),
    "ESCA"         = sum(c_across(starts_with("ESCA")),         na.rm = TRUE),
#    "Other"        = sum(c_across(starts_with("XXX")),          na.rm = TRUE),
    "HIIN"         = sum(c_across(starts_with("HIIN")),         na.rm = TRUE),
    "LAGR"         = sum(c_across(starts_with("LAGR")),         na.rm = TRUE),
    "DeadSum"      = sum(c_across(starts_with("DeadSum")),      na.rm = TRUE),
    "BareGround"   = sum(c_across(starts_with("BareGround")),   na.rm = TRUE)
  )

head(Wide_7.FxlGrp)
view(Wide_7.FxlGrp)

# ============================================================
# COLOR PALETTES (defined once, used throughout)
# ============================================================
subtrt_colors <- c(
  "Control"                  = "#8B0000",
  "Scraped + Seeded"         = "#4A4A4A",
  "Mowed"                    = "#E69A00",
  "Mowed + Scraped + Seeded" = "#4D7A3A"
)

type_colors <- c(
  "Native"     = "#1B6CA8",
  "Non-native" = "#CC4E00",
  "Other"      = "black",
  "Weed"       = "#006B6B",
  "Wildflower" = "#B8006A"
)

rain_colors <- c(
  "Low"  = "#6BAED6",
  "Med"  = "#2171B5",
  "High" = "#08306B"
)

# Facet labels
year_labels <- c(
  "2021" = "2021 (Pre-Treatment)",
  "2022" = "2022",
  "2023" = "2023",
  "2024" = "2024",
  "2025" = "2025"
)





