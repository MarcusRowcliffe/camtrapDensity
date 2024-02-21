################################################
# INITIAL SETUP
# 1. Download data from Agouti
# 2. Extract the resulting zip file to a specific directory
# 3. Create a new project in RStudio: File > New Project > Existing Directory >
#    Browse to the directory containing the data files > Create Project.
# 4. Install some packages necessary for analysis by running:

install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")

# Generally you only need to do this once at the initial installation of R,
# although you may need to re-run the camtraptor or camtrapDensity lines
# to get the latest versions when there are important updates to these packages.

################################################
# LOAD PACKAGES
# Run this at the beginning of each session (any time you close and
# reopen RStudio).

library(camtrapDensity)
library(camtraptor)
library(dplyr)
library(lubridate)

################################################
# LOAD DATA
# This creates an "object" called pkg containing all the data for analysis.
# It assumes that you created your R project within the directory containing
# the data as indicated above.

pkg <- read_camtrapDP("./datapackage.json")

# If you prefer to load data from a sub-directory within your project directory
# you can run the following (where "datapakage_V1.0" is the name of the
# subdirectory - replace this part with your own sub-directory name):

pkg <- read_camtrapDP("./datapackage_V1.0/datapackage.json")

# If your data are in an entirely different directory you will need to give
# the full directory path e.g.:

pkg <- read_camtrapDP("C:/my_files/my_analysis/my_data/datapackage.json")

################################################
# CHECK DEPLOYMENT SCHEDULE
# This shows a chart with black lines indicating the period of operation
# of each deployment and red ticks indicating the time at which each
# observation occurs. It highlights any incorrect dates, or where to split
# the data if you only want to analyse a subset of the deployments.

plot_deployment_schedule(pkg)

################################################
# OPTIONAL - SUBSET DEPLOYMENTS
# Use this function if you want to analyse only a subset of deployments,
# commonly useful if your data contains multiple seasons and you only
# want to analyse one of them. Some examples (note that these create a
# new data object called subpkg):

# Use only deployments starting after a given date
subpkg <- subset_deployments(pkg, start > ymd("2017-10-09"))
# Use only deployments ending before a given date
subpkg <- subset_deployments(pkg, end < ymd("2017-10-23"))
# Use only deployments occurring between given dates
subpkg <- subset_deployments(pkg,
                             start > ymd("2017-10-09") &
                               end < ymd("2017-10-23") )
# Use only deployments at a given set of locations
subpkg <- subset_deployments(pkg, locationName %in% c("S01", "S02"))
# Use all deployments except those at a given set of locations
subpkg <- subset_deployments(pkg, !locationName %in% c("S01", "S02"))
# Use all deployments except those at a given location
subpkg <- subset_deployments(pkg, locationName != "S01")

# After subsetting, check your sub-package to ensure correct selection
plot_deployment_schedule(subpkg)

################################################
# CHECK CALIBRATION MODELS
# Run this to show diagnostic plots for the deployment calibration models
# used to generate animal position and speed data (replacing pkg with subpkg
# if you have created a subset of deployments). Follow instructions to accept
# or reject each calibration model. See this document for guidance on how to
# interpret diagnostic plots and decide:
# https://github.com/MarcusRowcliffe/camtrapDensity/blob/master/Interpreting%20deployment%20model%20diagnostic%20plots.docx

pkgchk <- check_deployment_models(pkg)

################################################
# ONE-STEP REM ANALYSIS
# This runs an REM density analysis in a single step, using default
# settings for all parts of the analysis, including calibration checking
# (as in the last step), and interactive species selection.

res <- rem_estimate(pkg)

# You can add some arguments to the call for a little more flexibility. For
# example the call below adds three arguments (separated by commas):
# - check_deployments=FALSE: switches off the interactive deployment checking
#   procedure (we can do this because we've used pkgchk as the datapackage,
#   which has already been checked to  flag any inadequately calibrated
#   deployments);
# - species="Vulpes vulpes": provides an explicit species name instead of
#   using interactive species selection (note the name of the object containing
#   results is changed to res_vul to reflect this species selection);
# - reps=10: reduces the number of replications when generating standard errors
#   (this speeds things up for exploration, but should be left out when
#   generating final results).

res_vul <- rem_estimate(pkgchk,
                    check_deployments=FALSE,
                    species = "Vulpes vulpes",
                    reps=10)

# To inspect the different components of the results you can run the following:
res$estimates # prints the density and other model parameter estimates
res$species # reminds you which species was analysed
View(res$data) # views the location-specific trap rate data

# Run this to save any of these components to a data file (a file named as
# indicated will be created in your project directory):

write.csv(res$estimates, "vulpes_REM_results.csv")

# To show diagnostic plots that help to evaluate how well component models
# (for activity, detection radius and detection angle) fit the data:

plot(res$activity_model)
plot(res$radius_model, pdf=TRUE)
plot(res$angle_model)

################################################
# MULTI-STEP REM ANALYSIS
# This approach allows you to fit one or more of the REM component models
# separately before the density estimation step, allowing greater flexibility
# in defining model settings. In this example, all component models are fitted
# separately before passing to the rem_estimate function, and this has allowed
# a different truncation distance to be tried for the detection radius data.

sp <- select_species(pkgchk) # runs interactive species selection
rmod <- fit_detmodel(radius~1, pkgchk, order=0, truncation=6, species=sp)
plot(rmod, pdf=TRUE) # check model fit
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
# GETTING HELP
# To get help on any function, including details on all the argument
# options, run the function name preceded by a question mark, e.g.

?fit_detmodel
