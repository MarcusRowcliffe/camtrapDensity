camtrapDensity
================

This is a development R package designed to interface, and ultimately
merge, with
[camtraptor](https://github.com/inbo/camtraptor/blob/main/README.md),
and to interface seamlessly with
[camtrapDP](https://tdwg.github.io/camtrap-dp/data/) datapackages
generated in [Agouti](https://www.agouti.eu). The package currently
provides functions to run single species random encounter models to
estimate animal density, including models for the component parameters
(speed, activity level and detection zone dimensions). There is also
some basic functionality to check and correct the data.

## <a id="GettingStarted"></a>Getting started with RStudio

If you don’t already have them, install [R](https://cran.r-project.org)
and [RStudio](https://posit.co/download/rstudio-desktop).

Run analyses within an RStudio project. To set up a new project in
RStudio in either a new or an existing project directory, use menu
options:

**File \> New Project \> New OR Existing Directory \> Browse \> select
directory \> Open \> Create Project**

This creates a file with extension .Rproj in the directory you chose or
created. By default, the last project you opened will re-open next time
you run RStudio. You can open a different project by double clicking the
relevant .Rproj file in your file browser, or by navigating to it from
the RStudio menu:

**File \> Open Project**

or

**File \> Recent Projects**

To start running analyses, open a new R script file:

**File \> New File \> R Script**

You can now add lines of code to the script window from the examples
below, edit them as necessary to fit your own case, and run them to
perform the analysis. To run code, move your cursor to the line you want
to run, or highlight multiple lines, and press **Ctrl+Enter** (Windows)
or **Cmd+Enter** (Mac), or click **Run** at the top of the code pane.

## Installing packages

Install `camtrapDensity`, `camptraptor` and the other necessary
supporting packages by running this code. You only need to run this code
once for the initial install, although it may be necessary to re-install
packages when important updates are released from time to time:

``` r
install.packages(c("devtools", "dply", "tidyr", "jsonlite", "lubridate"))
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")
```

## Example usage

Load the required packages at the beginning of each session (each time
you close and restart):

``` r
library(camtraptor)
library(camtrapDensity)
library(lubridate)
```

### Loading data

Having annotated your images in Agouti (including marking animal
positions and deployment calibration poles):

1.  download the data from Agouti;
2.  open R Studio and create a new project (see
    [above](#GettingStarted));
3.  unzip the Agouti download within your project directory.

This allows you to access multiple datapackages from a single project.
Alternatively you can create a new project for each datapackage by
reversing steps 2 and 3 above (unzip the data download first, then
create a new project in the resulting directory). To load data, use the
relevant platform- and case-specific code below (replacing the path
before `/datapackage.json` with your local directories):

``` r
# Data are in the project directory
pkg <- read_camtrapDP("./datapackage.json") # Windows
pkg <- read_camtrapDP("/datapackage.json") # Mac

# Data are in a subdirectory of the project directory
pkg <- read_camtrapDP("./your_local_directory/datapackage.json") # Windows
pkg <- read_camtrapDP("/your_local_directory/datapackage.json") # Mac

# Data are outside the project directory (requires full path)
pkg <- read_camtrapDP("C:/path/to/your/folder/datapackage.json") # Windows
pkg <- read_camtrapDP("/path/to/your/folder/datapackage.json") # Mac
```

### <a id="ScheduleChecking"></a>Checking the deployment schedule

This line provides a visualisation of the deployment schedules. It can
quickly highlight any incorrect dates, or help you decide where to split
the data if you only want to analyse a subset of the deployments. Black
lines indicate the period of operation of each deployment and red ticks
indicate the time at which each observation occurred.

``` r
plot_deployment_schedule(pkg)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Mapping deployments

You can create a map of deployments using this code:

``` r
map_deployments(pkg)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

You can plot the same map but with point diameter proportional to
deployment-specific trap rate for a given species like this (use
scientific names):

``` r
map_traprates(pkg, species="Vulpes vulpes")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Subsetting deployments

Use this function if you want to analyse only a subset of deployments.
Commonly useful if your data contains multiple survey seasons and you
need to extract one of them for analysis, selecting deployments by date.
You can also select or exclude deployments or locations by name. Here
are some examples (note that these create a new data object called
`subpkg`):

``` r
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
```

After subsetting, check your sub-package to verify correct selection, as
[above](#ScheduleChecking).

### Correcting timestamps

Sometimes a camera is deployed with date-time set incorrectly. In this
case you can correct all the times for this deployment by running the
following, with arguments identifying the datapackage (`pkg`), the
deployment to correct (`depID`), and reference times indicating the
amount by which the times are out (`wrongTime` and `rightTime`). The
next example represents a scenario where the camera spontaneously reset
to an arbitrary date-time at the start of a deployment, but it is known
when this reset happened (for example `rightTime` taken from the correct
time on the image of an info board at set up, and `wrongTime` taken from
the info strip added by the camera to this image):

``` r
pkg_corrected <- correct_time(pkg,
                              depID = "c95a566f-e75e-4e7b-a905-0479c8770da3",
                              wrongTime = "1970-01-01 00:05:00",
                              rightTime = "2023-10-10 14:23:00")
```

To find the long deployment ID, you can view the deployment data table
and copy the relevant value from the deploymentID column:

``` r
View(pkg$data$deployments)
```

Alternatively, you can change date-times by location name (remembering
that a location can include more than one deployment). Also, if you know
the time offset required and don’t need to take it from a reference
image on a particular date, (for example, you know that when setting the
camera the wrong time zone was used, or there was an AM/PM mix up), you
can supply times with the desired time offset but an arbitrary
date-time. For example, this adds 12 hours to all the timestamps from
location S01:

``` r
pkg_corrected <- correct_time(pkg,
                              locName = "S01",
                              wrongTime = "2000-01-01 00:00:00",
                              rightTime = "2000-01-01 12:00:00")
```

Both examples create a new datapackage object called `pkg_corrected`.

### <a id="CalibrationChecking"></a>Checking deployment calibration models

Run the next chunk to show diagnostic plots for the deployment
calibration models used to generate animal position and speed data
(replacing `pkg` with the name of a subsetted and/or time-corrected
package if necessary). Then follow the interactive instructions to
accept or reject each calibration model. See the document “*Interpreting
deployment model diagnostic plots*” for guidance on how to decide,
available [here](http://www.tinyurl.com/CalibrationPlots).

``` r
pkg_chk <- check_deployment_models(pkg)
```

The datapackage called `pkg_chk` will now keep a record of which
deployments have been rejected, and position and speed data from these
deployments will be excluded from subsequent analysis, since we expect
them to be unreliable.

### Estimating REM density

Density estimation can be run as a single step, most simply like this:

``` r
res <- rem_estimate(pkg)
```

This will prompt you to choose which species to analyse, and run
deployment calibration model checks as [above](#CalibrationChecking).
The next line of code is an alternative example that runs the analysis
on a datapackage that has already had its deployment calibration models
checked (`pkg_chk`) and switches off calibration model checking
(`check_deployments=FALSE`), avoiding having to do this each time you
analyse a different species; it also specifies a species
(`species = "Vulpes vulpes"`), rather than using interactive selection;
and it reduces the number of replications used to generate standard
errors (`reps = 10` - this can be useful to speed things up for
exploration, but should be left out when generating final results):

``` r
res_vul <- rem_estimate(pkg_chk,
                        check_deployments = FALSE,
                        species = "Vulpes vulpes",
                        reps = 10)
```

Alternatively, one or more REM component models can be fitted separately
before the density estimation step, allowing more flexibility in model
choices. In this example all four models (detection radius, detection
angle, speed while active and activity level) are fitted first, with
species pre-defined. Species can be chosen either interactively:

``` r
sp <- select_species(pkg_chk)
```

or specified directly:

``` r
sp <- "Vulpes vulpes"
```

Then model fitting might look like this:

``` r
smod <- fit_speedmodel(pkg, species=sp)
pmod <- fit_actmodel(pkg, reps=100, species=sp)
rmod <- fit_detmodel(radius~1, pkg, order=0, species=sp, truncation=6)
amod <- fit_detmodel(angle~1, pkg, order=0, unit="radian", species=sp)
res_vul <- rem_estimate(pkg_chk, check_deployments=F, species=sp,
                        radius_model = rmod,
                        angle_model = amod,
                        speed_model = smod,
                        activity_model = pmod)
```

### Understanding the output

The output of `rem_estimate` is a list with components:

- `estimates`: the density estimate and component parameters with their
  errors
- `species`: the species to which the estimates apply
- `data`: the trap rate data in the form of a table with counts and
  camera effort for each location
- `radius_model`, `angle_model`, `speed_model`, `activity_model`: the
  model objects from which parameter estimates were derived.

You can inspect these list components individually by attaching the
compenent name to the result object spearated with a \$ sign, like this:

``` r
res_vul$estimates
```

    ##                  estimate          se        cv      lcl95      ucl95  n
    ## radius          3.3516525  0.78550867 0.2343646  1.8120555  4.8912495 13
    ## angle          44.9831275  9.42829597 0.2095963 26.5036674 63.4625876 15
    ## active_speed    1.3414083  0.60283473 0.4494044  0.1598522  2.5229644  6
    ## activity_level  0.2451959  0.07206383 0.2939031  0.1039508  0.3864410 15
    ## overall_speed   7.8937874  4.23877495 0.5369761 -0.4142115 16.2017863 NA
    ## trap_rate       0.3829885  0.05529651 0.1443816  0.2639141  0.4088946  3
    ## density        16.3286340 12.37272034 0.7577315  4.3610520 61.1376076 NA
    ##                   unit
    ## radius               m
    ## angle           degree
    ## active_speed   km/hour
    ## activity_level    none
    ## overall_speed   km/day
    ## trap_rate        n/day
    ## density          n/km2

This table provides the core results. Rows `radius` and `angle` are the
effective detection radius and angle respectively. The `active_speed`
row refers to the estimated speed while active. `activity_level` is the
estimated proportion of time spent active. The `overall_speed` row
refers to the product of `active_speed` and `activity_level`, giving the
overall average speed, or day range. The `trap_rate` row is the number
of animal observations per unit time. `density` is the estimated number
of animals per unit area. Columns contain the estimates, their standard
errors (se), coefficients of variation (cv - se as a proportion of the
estimate), lower and upper 95% confidence intervals (lcl95 and ucl95),
sample sizes (n), and units. Sample size are the numbers of observations
available for estimation, except for trap rate, where n is the number of
locations.

Inspecting the trap rate data shows the number of observations available
at each location (n), the amounts of camera effort and its unit, and the
species to which this applies:

``` r
res_vul$data
```

    ## # A tibble: 3 × 5
    ##   locationName     n effort effort_unit scientificName
    ##   <chr>        <int>  <dbl> <chr>       <chr>         
    ## 1 S01              3  10.0  day         Vulpes vulpes 
    ## 2 S02              2   4.55 day         Vulpes vulpes 
    ## 3 S03             10  19.4  day         Vulpes vulpes

To save a results table to an external file, run this (a file named as
indicated will be created in your project directory):

``` r
write.csv(res_vul$estimates, "vulpes_REM_results.csv")
```

Component model fits can be evaluated by inspecting diagnostic plots:

``` r
plot(res_vul$activity_model)
```

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
plot(res_vul$radius_model, pdf=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
plot(res_vul$angle_model)
```

![](README_files/figure-gfm/unnamed-chunk-22-3.png)<!-- -->

## General R tips

Once you have fitted one or more REM models in a session, you may want
to save your progress so that you don’t have to run again everything
next session. To do this, save your workspace image when you exit
RStudio. By default, RStudio should prompt you to do this on exit, and
automatically reload your work next time you open the project. However,
you can save your workspace manually, either using the RStudio menu:

**Session \> Save Workspace As \> provide a file name \> Save**

or by running one of these lines:

``` r
save.image() # Creates a file named .RData in the project directory
save.image("12Feb24.RData") # Creates a file named 12Feb24.RData
```

At the start of the next session, if you want to load a specific
workspace, you can either use the menu:

**Session \> Load Workspace \> select the required RData file \> Open**

or run this line, replacing the text with your file name:

``` r
load("12Feb24.RData")
```

To get help on any function, including details on all the argument
options, run the function name preceded by a question mark, e.g.

``` r
?rem_estimate
```
