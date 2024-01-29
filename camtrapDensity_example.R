# install.packages(c("devtools", "dply", "tidyr", "jsonlite"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

library(camtrapDensity)
library(camtraptor)
library(dplyr)

################################################
# Load data
pkg <- read_camtrapDP("./datapackage_V1.0/datapackage.json")

################################################
# Check deployment schedule and calibration models
plot_deployment_schedule(pkg)
pkg2 <- check_deployment_models(pkg)

################################################
# One step REM analysis
res <- rem_estimate(pkg, check_deployments=F, reps=50)
# Inspect outputs
res$estimates
res$species
res$data
plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)

################################################
# Fit component models separately (more flexible)
sp <- select_species(pkg2)
rmod <- fit_detmodel(radius~1, pkg2, order=0, truncation=8, species=sp)
amod <- fit_detmodel(angle~1, pkg2, order=0, unit="radian", species=sp)
smod <- fit_speedmodel(pkg2, species=sp)
pmod <- fit_actmodel(pkg2, species=sp)
res2 <- rem_estimate(pkg2, check_deployments=F, species=sp,
                    radius_model = rmod,
                    angle_model = amod,
                    speed_model = smod,
                    activity_model = pmod)
res2$estimates
