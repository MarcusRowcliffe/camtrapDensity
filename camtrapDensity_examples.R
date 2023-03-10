install.packages(c("devtools", "dply", "tidyr"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

library(camtrapDensity)
library(camtraptor)

pkg <- camtraptor::read_camtrap_dp("./datapackage/datapackage.json")
# save(pkg, version = 2, file = "./data/pkg.rda")

plot_deployment_schedule(pkg)
subpkg <- subset_deployments(pkg,
                             locationName != "S01" &
                               start > as.POSIXct("2017-10-01", tz="UTC") &
                               end < as.POSIXct("2017-10-31", tz="UTC"))
pkg_corrected <- correct_time(pkg,
                              depID = "0d620d0e-5da8-42e6-bcf2-56c11fb3d08e",
                              wrongTime = "2017-10-02 12:30:00",
                              rightTime = "2017-10-03 00:30:00")

pkg2 <- check_deployment_models(pkg)
res1 <- rem_estimate(pkg2, check_deployments=F)
res1$estimates
res1$data
plot(res1$activity_model)
plot(res1$radius_model, pdf=TRUE)
plot(res1$angle_model)

sp <- select_species(pkg2)
rmod <- fit_detmodel(radius~1, pkg2, order=0, truncation=8, species=sp)
plot(rmod, pdf=TRUE)
amod <- fit_detmodel(angle~1, pkg2, order=0, unit="radian", species=sp)
smod <- fit_speedmodel(pkg2, species=sp)
pmod <- fit_actmodel(pkg2, species=sp)
res2 <- rem_estimate(pkg2, check_deployments=F, species=sp,
                    rmod, amod, smod, pmod)
res2$estimates
