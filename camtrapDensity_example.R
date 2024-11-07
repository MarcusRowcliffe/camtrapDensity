# See https://github.com/MarcusRowcliffe/camtrapDensity for detailed instructions

# 1. INITIAL SETUP
# One-off
# remove.packages("camtrapDensity")
install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

# 2. LOAD PACKAGES
# At the beginning of each session
library(camtrapDensity)
library(camtraptor)
library(lubridate)

# 3. LOAD DATA
# Assumes data are in a subdirectory of root project directory.
pkg <- read_camtrapDP("datapackage_V1.0/datapackage.json")
plot_deployment_schedule(pkg)

# 4. SUBSET DEPLOYMENTS (IF NECESSARY)
# e.g. Selects only deployments occuring within 2023
subpkg <- subset_deployments(pkg, start > ymd("2023-01-01") &
                               end < ymd("2024-01-01"))
# e.g. Slices all deployments to given date range
subpkg <- slice_camtrap_dp(pkg,
                           start = "2017/10/09",
                           end = "2017/10/26")
# e.g. Slices deployment at location S02 to end at the given time
subpkg <- slice_camtrap_dp(pkg,
                           end = "2017/10/27 16:45:00",
                           depChoice = locationName=="S02")
plot_deployment_schedule(subpkg)

# recheck deployment schedule of filtered datapackage
plot_deployment_schedule(subpkg)

# 5. CHECK DEPLOYMENT CALIBRATION MODELS
pkg_chk <- check_deployment_models(pkg)

# 6. REM ANALYSIS
res <- rem_estimate(pkg_chk, check_deployments=FALSE)

# 7. EVALUATE DATA DISTRIBUTIONS AND MODEL FITS
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)
hist(res$speed_model)

# 8. EXTRACT MODEL ESTIMATES / EXPORT CSV FOR SHARING
res$estimates # inspect
write_rem_csv(res) # export

# 9. SAVE WORKSPACE FOR FUTURE REFERENCE
save.image()
