remove.packages("camtrapDensity")
# See https://github.com/MarcusRowcliffe/camtrapDensity for detailed instructions

# 1. INITIAL SETUP
# One-off
install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

# 2. LOAD PACKAGES
# At the beginning of each session
library(camtrapDensity)
library(camtraptor)
library(lubridate)

# 3. LOAD DATA
# Assumes data are in the root project directory and Windows OS.
# Delete the "." for Mac.
pkg <- read_camtrapDP("datapackage_V1.0/datapackage.json")

# 4. SUBSET DEPLOYMENTS (OPTIONAL)
# e.g. Selects only deployments occuring within 2023
subpkg <- subset_deployments(pkg, start > ymd("2023-01-01") &
                               end < ymd("2024-01-01"))

# 5. CHECK DEPLOYMENT SCHEDULE
plot_deployment_schedule(pkg)

# 6. CHECK DEPLOYMENT CALIBRATION MODELS
pkg_chk <- check_deployment_models(pkg)

# 7. REM ANALYSIS
res <- rem_estimate(pkg_chk, check_deployments=FALSE)

# 8. EVALUATE DATA DISTRIBUTIONS AND MODEL FITS
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)
hist(res$speed_model)

# 9. EXTRACT MODEL ESTIMATES
res$estimates

# 10. SAVE WORKSPACE FOR FUTURE REFERENCE
save.image()
