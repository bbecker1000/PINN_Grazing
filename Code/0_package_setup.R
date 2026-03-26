#package setup

# ============================================================
# RUN THIS ONCE TO SET UP PERSISTENT PACKAGE LIBRARY
# ============================================================

# 1. Create persistent library directory
lib_path <- "/home/jovyan/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

# 2. Add to .Renviron so R always uses this path
renviron_path <- "/home/jovyan/.Renviron"
renviron_line <- paste0('R_LIBS_USER=', lib_path)

# Only add if not already there
existing <- readLines(renviron_path, warn = FALSE)
if (!any(grepl("R_LIBS_USER", existing))) {
  cat(renviron_line, "\n", file = renviron_path, append = TRUE)
  cat("Added R_LIBS_USER to .Renviron\n")
} else {
  cat("R_LIBS_USER already set in .Renviron\n")
}

# 3. Reload .Renviron
readRenviron(renviron_path)
.libPaths(c(lib_path, .libPaths()))

# 4. Install all packages to persistent library
packages_cran <- c(
  "tidyverse", "readr", "readxl", "vegan", "cowplot",
  "lme4", "glmmTMB", "sjPlot", "broom", "stringr",
  "brms", "DHARMa", "ggeffects", "marginaleffects",
  "ggrepel", "paletteer", "MASS", "patchwork",
  "ggordiplots", "gllvm", "permute"
)

# Install missing packages
installed <- rownames(installed.packages(lib.loc = lib_path))
to_install <- packages_cran[!packages_cran %in% installed]

if (length(to_install) > 0) {
  cat("Installing:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, lib = lib_path)
} else {
  cat("All CRAN packages already installed\n")
}

# 5. Install cmdstanr separately (special repo)
if (!"cmdstanr" %in% installed) {
  install.packages("cmdstanr",
                   lib   = lib_path,
                   repos = c("https://mc-stan.org/r-packages/",
                             getOption("repos")))
}

# 6. Install CmdStan if needed
library(cmdstanr)
if (!dir.exists(cmdstan_path())) {
  install_cmdstan()
} else {
  cat("CmdStan already installed at:", cmdstan_path(), "\n")
}

cat("\nSetup complete! Restart R and run the startup script.\n")

