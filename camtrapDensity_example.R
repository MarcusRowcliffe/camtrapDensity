################################################
# INITIAL SETUP

install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

################################################
# LOADING PACKAGES
# At the beginning of each session

library(camtrapDensity)
library(camtraptor)
library(dplyr)
library(lubridate)

################################################
# LOADING DATA
# Chose style for your platform and data organisation

# Data in project directory root
pkg <- read_camtrapDP("./datapackage.json") # Windows
pkg <- read_camtrapDP("/datapackage.json") # Mac

# Data in subdirectory of project directory (replace your_local_folder)
pkg <- read_camtrapDP("./your_local_folder/datapackage.json") # Windows
pkg <- read_camtrapDP("/your_local_folder/datapackage.json") # Mac

# Data not in project directory (use full path)
pkg <- read_camtrapDP("C:/path/to/your/folder/datapackage.json") # Windows
pkg <- read_camtrapDP("/path/to/your/folder/datapackage.json") # Mac

################################################
# CHECKING DEPLOYMENT SCHEDULE
# Shows a deployment schedule Gantt chart

plot_deployment_schedule(pkg)

################################################
# SUBSETTING DEPLOYMENTS
# Examples taking subsets of the deployments from the data

# Keep only deployments starting after a given date
subpkg <- subset_deployments(pkg, start > ymd("2017-10-09"))
# Keep only deployments ending before a given date
subpkg <- subset_deployments(pkg, end < ymd("2017-10-23"))
# Keep only deployments occurring between given dates
subpkg <- subset_deployments(pkg,
                             start > ymd("2017-10-09") &
                               end < ymd("2017-10-23") )
# Keep only deployments at a given set of locations
subpkg <- subset_deployments(pkg, locationName %in% c("S01", "S02"))
# Keep all deployments except those at a given set of locations
subpkg <- subset_deployments(pkg, !locationName %in% c("S01", "S02"))
# Keep all deployments except those at a given location
subpkg <- subset_deployments(pkg, locationName != "S01")

# After subsetting, check your sub-package to verify correct selection
plot_deployment_schedule(subpkg)

################################################
# CORRECTING TIMESTAMPS

# Example identifying deployment by long ID
pkg_corrected <- correct_time(pkg,
                              depID = "c95a566f-e75e-4e7b-a905-0479c8770da3",
                              wrongTime = "1970-01-01 00:05:00",
                              rightTime = "2023-10-10 14:23:00")

# Showing deployment data to find deployment IDs
View(pkg$data$deployments)

# Example identifying times to change by location name
pkg_corrected <- correct_time(pkg,
                              locName = "S01",
                              wrongTime = "2000-01-01 00:00:00",
                              rightTime = "2000-01-01 12:00:00")

################################################
# CHECKING CALIBRATION MODELS

pkg_chk <- check_deployment_models(pkg)

################################################
# ONE-STEP REM ANALYSIS

# Simplest use, runs interactive species selection and
# deployment calibration model checking
res <- rem_estimate(pkg)

# Switches off model checking, specifies species, speeds up the run
res_vul <- rem_estimate(pkg_chk,
                    check_deployments=FALSE,
                    species = "Vulpes vulpes",
                    reps=10)

################################################
# MULTI-STEP REM ANALYSIS

sp <- select_species(pkgchk) # runs interactive species selection
rmod <- fit_detmodel(radius~1, pkgchk, order=0, truncation=6, species=sp)
amod <- fit_detmodel(angle~1, pkgchk, order=0, unit="radian", species=sp)
smod <- fit_speedmodel(pkgchk, species=sp)
pmod <- fit_actmodel(pkgchk, species=sp)
res2 <- rem_estimate(pkgchk, check_deployments=FALSE, species=sp,
                     radius_model = rmod,
                     angle_model = amod,
                     speed_model = smod,
                     activity_model = pmod)
res2$estimates

################################################
# UNDERSTANDING THE OUTPUT

vul_res$estimates # prints the density and other model parameter estimates
vul_res$species # reminds you which species was analysed
View(vul_res$data) # views the location-specific trap rate data

# Writing results to external files
write.csv(vul_res$estimates, "vulpes_REM_results.csv")
write.csv(vul_res$data, "vulpes_traprates.csv")

# Evaluating model fits
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)


################################################
# GENERAL R TIPS

# Saving the workspace for re-use
save.image() # Creates a file named .RData
save.image("12Feb24.RData") # Creates a file named 12Feb24.RData

# Loading an existing workspace
load("12Feb24.RData")

# Getting help on a function
?rem_estimate
