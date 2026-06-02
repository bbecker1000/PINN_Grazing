# ============================================================
# FUNCTION TO LOAD ALL SAVED MODELS
# ============================================================

load_models <- function(output_dir = "Output") {
  
  models <- list(
    thatch_model      = "thatch_model.rds",
    visobs_model      = "visobs_model.rds",
    bareground_model  = "bareground_model.rds",
    yield_model       = "yield_model.rds",
    thatch_bio_model  = "thatch_biomass_model.rds",
    mustard_cov_model = "mustard_cover_model.rds",
    mustard_den_model = "mustard_dens_model.rds",
    vegheight_model   = "vegheight_model.rds",
    ystar_model       = "ystar_presence_model.rds"
  )
  
  loaded <- list()
  missing <- character(0)
  
  for (name in names(models)) {
    path <- file.path(output_dir, models[[name]])
    if (file.exists(path)) {
      loaded[[name]] <- readRDS(path)
      cat("✓ Loaded:", models[[name]], "\n")
    } else {
      missing <- c(missing, models[[name]])
      cat("✗ Missing:", models[[name]], "\n")
    }
  }
  
  if (length(missing) > 0) {
    cat("\nWarning:", length(missing), "model(s) not found:\n")
    cat(paste(" -", missing, collapse = "\n"), "\n")
  } else {
    cat("\nAll", length(models), "models loaded successfully\n")
  }
  
  return(loaded)
}

# ── Usage ────────────────────────────────────────────────────

# Load all models into a named list
m <- load_models()

# Access individual models
# m$thatch_model
# m$visobs_model
# etc.

# Or assign to original variable names
AvThatch_cm.m1.brms  <- m$thatch_model
VisObs_Av.m1.brms    <- m$visobs_model
bareground.m1.brms   <- m$bareground_model
AllYield.m1.brms     <- m$yield_model
thatch.m1.brms       <- m$thatch_bio_model
Mustard.m1.brms      <- m$mustard_cov_model
MustardDens.m1.brms  <- m$mustard_den_model
AvVegHeight.m1.brms  <- m$vegheight_model
YStarPresence.m1.brms <- m$ystar_model

# Clean up list after assigning
rm(m)
