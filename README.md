camtrapDensity
================

This is a development package designed to interface with and ultimately
merge with
[camtraptor](https://github.com/inbo/camtraptor/blob/main/README.md),
and to interface seamlessly with
[camtrapDP](https://tdwg.github.io/camtrap-dp/data/) datapackages
generated in [Agouti](https://www.agouti.eu). The package currently
provides functions to run single species random encounter models to
estimate animal density. There is also some basic functionality to check
and correct the data.

## Installation

You can install camtrapDensity from
[GitHub](https://github.com/MarcusRowcliffe/camtrapDensity), as well as
camtraptor, by running this code (you only need to do this for the
inital install or to update the packages; if the devtools package is not
already installed, run the first line to do so):

``` r
install.packages("devtools")
devtools::install_github("inbo/camtraptor")
devtools::install_github("MarcusRowcliffe/camtrapDensity")
```

## Example usage

Load the required packages at the beginning of each session:

``` r
library(camtraptor)
library(camtrapDensity)
```

Having annotated your images in Agouti (including marking animal
positions and deployment calibration poles), exported the data, unzipped
the resulting download and moved the contents to a directory named data
in your working project directory, run this to load the data:

``` r
pkg <- camtrapDensity::read_camtrapDP("./datapackage_V1.0/datapackage.json")
```

This provides a visualisation of the deployment schedules:

``` r
plot_deployment_schedule(pkg)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

This creates a subset datapackage excluding a named location (S01) and
including only deployments in October 2017:

``` r
subpkg <- subset_deployments(pkg,
                             locationName != "S01" &
                               start >= as.POSIXct("2017-10-01", tz="UTC") &
                               end <= as.POSIXct("2017-10-31", tz="UTC"))
```

This creates a datapackage corrected for a mis-specified time timestamp
at one deployment:

`{r}package, depID=NULL, locName=NULL, wrongTime, rightTime pkg_corrected <- correct_time(pkg,                               depID = "c95a566f-e75e-4e7b-a905-0479c8770da3",                               wrongTime = "2017-01-01 00:00:00",                               rightTime = "2017-01-01 12:00:00")`

This allows you to inspect deployment calibration model diagnostic
plots, and derive a datapackage that records which deployments are
judged reliable, and whose position and speed data can therefore be used
in analyses:

``` r
pkg <- check_deployment_models(pkg)
```

## Estimating REM density

Density estimation can either be run as a single step, here without
specifiying species, in this case the function will present a table of
possible species and prompt you to choose which:

``` r
res <- rem_estimate(pkg, check_deployments=F)
```

Or one or more constituent models can be fitted separately, allowing
more flexibility in model choices, and passed to the rem_estimate
function; in this example all four models are fitted externally, based
on a pre-selected species, either with interactive choice:

``` r
sp <- select_species(pkg)
```

Or typed species name:

``` r
sp <- "Vulpes vulpes"
```

Then model fitting:

``` r
smod <- fit_speedmodel(pkg, species=sp)
pmod <- fit_actmodel(pkg, reps=100, species=sp)
rmod <- fit_detmodel(radius~1, pkg, order=0, species=sp)
amod <- fit_detmodel(angle~1, pkg, order=0, unit="radian", species=sp)
res <- rem_estimate(pkg, check_deployments=F, species=sp,
                    radius_model = rmod,
                    angle_model = amod,
                    speed_model = smod,
                    activity_model = pmod)
```

    ## Analysing Vulpes vulpes

## Understanding the output

The estimate result is a list with components:

- `estimates`: the density estimate and component parameters with their
  errors
- `species`: the species to which the estimates apply
- `data`: the trap rate data in the form of a table with counts and
  camera effort for each location
- `radius_model, angle_model, speed_model, activity_model`: the model
  objects from which estimates were derived. Access these list
  components like this:

``` r
res$estimates
```

    ##                  estimate         se         cv      lcl95      ucl95  n
    ## radius          5.5820537 0.42748750 0.07658248  4.7441782  6.4199292 15
    ## angle          43.6918974 8.76427859 0.20059277 26.5139113 60.8698834 15
    ## active_speed    1.5334081 0.70103906 0.45717709  0.1593716  2.9074447  7
    ## activity_level  0.2447423 0.05973084 0.24405604  0.1276699  0.3618147 15
    ## overall_speed   9.0069560 4.66777827 0.51824149 -0.1418894 18.1558014 NA
    ## trap_rate       0.4419098 0.06385799 0.14450456  0.2986509  0.5167405  3
    ## density         9.9953634 6.88079306 0.68839849  2.9488441 33.8801531 NA
    ##                   unit
    ## radius               m
    ## angle           degree
    ## active_speed   km/hour
    ## activity_level    none
    ## overall_speed   km/day
    ## trap_rate        n/day
    ## density          n/km2

These are the core results. Rows `radius` and `angle` are the effective
detection radius and angle respectively. The `active_speed` row refers
to the estimated speed while active. `activity_level` is the estimated
proportion of time spent active. The `overall_speed` row refers to the
product of `active_speed` and `activity_level`, giving the overall
average speed, or day range. The `trap_rate` row is the number of animal
observations per unit time. `density` is the estimated number of animals
per unit area. Columns contain the estimates, their standard errors
(se), coefficients of variation (cv - se as a proportion of the
estimate), lower and upper 95% confidence intervals (lcl95 and ucl95),
sample sizes (n), and units. Sample size are the numbers of observations
available for estimation, except for trap rate, where n is the number of
locations.

Inspecting the trap rate data shows the number of observations available
at each location (n), the amounts of camera effort and its unit, and the
species to which this applies.

``` r
res$data
```

    ## # A tibble: 3 × 5
    ##   locationName     n effort effort_unit scientificName
    ##   <chr>        <int>  <dbl> <chr>       <chr>         
    ## 1 S01              3  10.0  day         Vulpes vulpes 
    ## 2 S02              2   4.55 day         Vulpes vulpes 
    ## 3 S03             10  19.4  day         Vulpes vulpes

Component models can be evaluated by inspecting plots.

``` r
plot(res$activity_model)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(res$radius_model, pdf=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
plot(res$angle_model)
```

![](README_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->
