# ============================================================
# Code/startup.R
# Run once to install, then source at start of every session
# ============================================================

# ── 1. Set persistent library path ──────────────────────────
lib_path <- "/home/jovyan/R/library"
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_path, .libPaths()))

# ── 2. Define all required packages ─────────────────────────
packages_cran <- c(
  "tidyverse", "lme4", "readxl", "MASS", "marginaleffects",
  "paletteer", "DHARMa", "ggeffects", "brms", "glmmTMB",
  "patchwork", "vegan", "gllvm", "ggrepel", "cowplot",
  "broom", "stringr", "permute", "parallel", "doParallel"
)

# ── 3. Install any missing packages ─────────────────────────
install_if_missing <- function(pkgs, lib = lib_path) {
  installed <- rownames(installed.packages(lib.loc = lib))
  to_install <- pkgs[!pkgs %in% installed]
  if (length(to_install) > 0) {
    cat("Installing missing packages:", 
        paste(to_install, collapse = ", "), "\n")
    install.packages(to_install, lib = lib)
  } else {
    cat("All CRAN packages already installed\n")
  }
}

install_if_missing(packages_cran)

# cmdstanr needs special repo
if (!"cmdstanr" %in% rownames(installed.packages(lib.loc = lib_path))) {
  install.packages("cmdstanr",
                   lib   = lib_path,
                   repos = c("https://mc-stan.org/r-packages/",
                             getOption("repos")))
}

# CmdStan itself
library(cmdstanr)
if (!dir.exists(tryCatch(cmdstan_path(), error = function(e) ""))) {
  install_cmdstan()
} else {
  cat("CmdStan already installed at:", cmdstan_path(), "\n")
}

# ── 4. Load all packages ─────────────────────────────────────
suppressPackageStartupMessages({
  library(tidyverse)
  library(lme4)
  library(readxl)
  library(MASS)
  library(marginaleffects)
  library(paletteer)
  library(DHARMa)
  library(ggeffects)
  library(brms)
  library(cmdstanr)
  library(glmmTMB)
  library(patchwork)
  library(vegan)
  library(gllvm)
  library(ggrepel)
  library(cowplot)
  library(broom)
  library(stringr)
  library(permute)
  library(parallel)
  library(doParallel)
})

# ── 5. Fix common masking conflicts ──────────────────────────
select    <- dplyr::select
filter    <- dplyr::filter
summarise <- dplyr::summarise
rename    <- dplyr::rename
ordiplot  <- gllvm::ordiplot

cat("────────────────────────────────────\n")
cat("All packages loaded successfully\n")
cat("Session:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("────────────────────────────────────\n")

