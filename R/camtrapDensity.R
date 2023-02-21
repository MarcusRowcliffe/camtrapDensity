#' Plot a deployment Gantt chart
#'
#' Plots a Gantt chart illustrating deployment times (black lines) and
#' the occurrence of observations within those deployments (red bars). Useful
#' for checking errors in specification of deployment start and end dates.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   plot_deployment_schedule(pkg)
#' @export
#'
plot_deployment_schedule <- function(package){
  depdat <- package$data$deployments
  obsdat <- package$data$observations

  rng <- range(c(depdat$start, depdat$end, obsdat$timestamp))
  n <- nrow(depdat)
  depdat <- depdat[gtools::mixedorder(depdat$locationName), ]

  plot(c(1,n+0.5), rng, type="n", xlab="Location name", ylab="Date", xaxt="n", las=1)
  axis(1, (1:n)+0.05, depdat$locationName, las=2, cex.axis=0.7)

  for(i in 1:n){
    dep <- depdat$deploymentID[i]
    obs <- subset(obsdat, deploymentID==dep)$timestamp
    lines(rep(i, 2), rng + difftime(rng[2], rng[1]) * c(-0.05, 0.05),
          col="grey90")
    points(rep(i+0.1, length(obs)), obs, pch="-",
           col="red")
    lines(rep(i-0.1, 2), depdat[i, c("start", "end")],
          lwd=2)
  }
}

#' Subset a camera trap datapackage by deployment
#'
#' Select a subset of deployments from a datapackage defined by
#' a choice based on columns in the deployments table.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param choice A logical expression using column names from the
#'  deployments table.
#' @return As for \code{\link[camtraptor]{read_camtrap_dp}}, with all
#'  data tables reduced according to the choice criteria at the deployment
#'  level.
#' @examples
#' # subset excluding a location and including only October 2017
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   subpkg <- subset_deployments(pkg,
#'                                locationName != "S01" &
#'                                start > as.POSIXct("2017-10-01", tz="UTC") &
#'                                end < as.POSIXct("2017-10-31", tz="UTC"))
#' @export
#'
subset_deployments <- function(package, choice){
  out <- package
  out$data$deployments <- dplyr::filter(out$data$deployments, {{choice}})
  usedeps <- out$data$deployments$deploymentID
  out$data$media <- dplyr::filter(package$data$media,
                                  deploymentID %in% usedeps)
  out$data$observations <- dplyr::filter(package$data$observations,
                                         deploymentID %in% usedeps)
  out
}

#' Correct times for a given deployment in a camera trap datapackage
#'
#' When a camera trap starts with the wrong time stamp, times
#' in all datapackage tables can be corrected given a reference time
#' recorded by the camera, the correct time for this reference time
#' and the ID of the deployment to correct.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param deploymentID A character value giving the deployment identifier,
#'  to be matched in package$data$deployments$deploymentID.
#' @param wrongTime A character or POSIX reference date-time recorded wrongly
#'  by the camera.
#' @param rightTime A character or POSIX value giving the correct date-time
#'  when the reference time was recorded.
#' @return As for    \code{\link[camtraptor]{read_camtrap_dp}}, with all
#'  date-times corrected by the difference between rightTime and wrongTime.
#' @examples
#' pkg_corrected <- correct_time(pkg,
#'                               deploymentID = "0d620d0e-5da8-42e6-bcf2-56c11fb3d08e",
#'                               wrongTime = "2017-01-01 00:00:00",
#'                               rightTime = "2018-09-01 11:04:00")
#' @export
#'
correct_time <- function(package, deploymentID, wrongTime, rightTime){
  tdiff <- as.POSIXct(rightTime, tz="UTC") - as.POSIXct(wrongTime, tz="UTC")
  package$data$deployments <- dplyr::mutate(package$data$deployments,
                                            start = start + tdiff,
                                            end = end + tdiff)
  package$data$observations <- dplyr::mutate(package$data$observations,
                                             timestamp = timestamp + tdiff)
  package$data$media <- dplyr::mutate(package$data$media,
                                      timestamp = timestamp + tdiff)
  package
}

#' Select a species name
#'
#' Presents a table of species names with observation count for each
#' and allows the user to interactively select one.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @return A character string, scientific species name.
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   species <- select_species(pkg)
#' @export
#'
select_species <- function(package){
  obs <- package$data$observations
  n <- table(obs$scientificName)
  tab <-  package %>%
    camtraptor::get_species() %>%
    dplyr::select(contains("Name")) %>%
    dplyr::filter(scientificName %in% names(n))
  tab$n_observations <- n
  if("useDeployment" %in% names(obs))
    obs[!obs$useDeployment, c("speed", "radius", "angle")] <- NA
  tab$n_speeds <- with(obs, tapply(speed, scientificName, function(x)
    sum(x>0.1 & x<10 & !is.na(x))))
  tab$n_radii <- with(obs, tapply(radius, scientificName, function(x)
    sum(x<10 & !is.na(x))))
  tab$n_angles <- with(obs, tapply(angle, scientificName, function(x)
    sum(!is.na(x))))


  print.data.frame(tab)
  i <- NA
  while(is.na(i) || i<1 || i>nrow(tab))
    i <- readline("Enter row number of species to select: ") %>%
    as.numeric() %>%
    suppressWarnings()
  as.character(tab$scientificName[i])
}

#' Check deployment calibration model diagnostic plots
#'
#' Displays deployment calibration model diagnostic plots and allows
#' users to record interactively whether each deployment is reliable.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @return The original package with logical column \code{useDeployment}`
#'  added to deployments and observations data.
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   pkg_checked <- check_deployment_models(pkg)
#' @export
#'
check_deployment_models <- function(package){
  plot_folder <- file.path(package$directory, "positioning_plots")
  if(!dir.exists(plot_folder)) stop("The specified folder does not exist.")
  plot_dirs <- list.dirs(plot_folder, recursive=FALSE)
  plots <- file.path(plot_dirs, "ratio.jpeg") %>%
    lapply(jpeg::readJPEG)
  names(plots) <- basename(plot_dirs)

  depdat <- package$data$deployments %>%
    dplyr::select(deploymentID, locationName)
  depdat$useDeployment <- FALSE

  for(i in 1:nrow(depdat)){
    dep <- depdat$deploymentID[i]
    if(dep %in% names(plots)){
      img <- plots[[dep]]
      imdim <- dim(img)
      p <- ggplot2::ggplot() +
        ggplot2::annotation_raster(img, 1, imdim[2], 1, imdim[1]) +
        ggplot2::xlim(1, imdim[2]) + ggplot2::ylim(1, imdim[1]) +
        ggplot2::theme_void() + ggplot2::ggtitle(depdat$locationName[i])
      print(p)
      answer <- NA
      while(is.na(answer) || !answer %in% c("y", "n"))
        answer <- readline("Use deployment? (y/n): ") %>% tolower()
      if(answer=="y") depdat$useDeployment[i] <- TRUE
    }
  }

  if("useDeployment" %in% names(package$data$observations))
    package$data$observations <- select(package$data$observations, -useDeployment)
  package$data$observations <- package$data$observations %>%
    dplyr::left_join(depdat, by="deploymentID")

  package$data$deployments$useDeployment <- depdat$useDeployment
  package
}

#' Estimate average animal speed
#'
#' Calculates harmonic mean and standard error of animal speed while active
#' from a data package.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs select_species to get user input.
#' @return List with elements:
#'  - \code{speed}: a one row dataframe containing columns \code{estimate}
#'    (mean) and \code{se} (standard error) speed while active.
#'  - \code{data}: a numeric vector of the data from which the estimate is derived
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   speed_model <- fit_speedmodel(pkg)
#'   speed_model$speed
#' @export
#'
fit_speedmodel <- function(package,
                           species=NULL,
                           distUnit=c("m", "km", "cm"),
                           timeUnit=c("second", "minute", "hour", "day")){
  distUnit <- match.arg(distUnit)
  timeUnit <- match.arg(timeUnit)

  if(is.null(species)) species <- select_species(package)
  obs <- package$data$observations %>%
    subset(scientificName==species & speed > 0.01 & speed < 10 & !is.na(speed))
  if("useDeployment" %in% names(obs)) obs <- subset(obs, useDeployment)
  if(nrow(obs) == 0) stop("There are no usable speed data")
  mn <- 1/mean(1/obs$speed, na.rm=FALSE)
  se <- mn^2 * sqrt(var(1/obs$speed, na.rm=FALSE)/nrow(obs))
  list(speed=data.frame(estimate=mn, se=se),
       data=obs$speed,
       distUnit=distUnit,
       timeUnit=timeUnit)
}

#' Fit an activity model
#'
#' Fits an activity model to data package data and estimates activity
#' level (proportion of time spent active).
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs select_species to get user input.
#' @param obsdef Observation definition, either individual or sequence.
#' @param reps Number of bootstrap replicates to run.
#' @param ... Arguments passed to \code{\link[activity]{fitact}}.
#' @return An `actmod` list.
#' @seealso \code{\link[activity]{fitact}}
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   act_model <- fit_actmodel(pkg, reps=100)
#'   act_model@@act
#' @export
#'
fit_actmodel <- function(package,
                         species=NULL,
                         reps=999,
                         obsdef=c("individual", "sequence"),
                         ...){
  obsdef <- match.arg(obsdef)
  if(is.null(species)) species <- select_species(package)
  deps <- package$data$deployments
  obs <- package$data$observations %>%
    dplyr::filter(scientificName==species) %>%
    dplyr::select(deploymentID, sequenceID, timestamp, count)
  i <- switch(obsdef,
              individual = rep(1:nrow(obs), obs$count),
              sequence = !duplicated(obs$sequenceID))
  obs <- obs[i, ]

  if(nrow(obs)>1){
    obs <- deps %>%
      dplyr::select(deploymentID, latitude, longitude) %>%
      dplyr::right_join(obs, by="deploymentID") %>%
      dplyr::select(-count)
    suntimes <- insol::daylength(obs$latitude, obs$longitude,
                                 insol::JD(obs$timestamp), 0)
    timeshift <- pi - mean(suntimes[, 1] + suntimes[,3]/2) * pi/12
    obs$solartime <- obs %>%
      with(activity::solartime(timestamp, latitude, longitude, 0)) %>%
      .$solar %>%
      + timeshift %>%
      activity::wrap()
      activity::fitact(obs$solartime,
                       adj = 1.5, sample = "data", reps = reps, ...)
  } else
    NULL
}

#' Fit a detection function model
#'
#' Fits a detection function to a data package and estimates effective
#' detection distance (EDD).
#'
#' @param formula A two sided formula relating radius or angle data
#'   to covariates.
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs select_species to get user input.
#' @param ... Arguments passed to \code{\link[Distance]{ds}}.
#' @return A \code{ddf} detection function model list, with additional elements:
#'   \code{edd}, a vector with estimated and standard error effective detection
#'     distance, or the \code{newdata} dataframe with EDD estimate and se
#'     columns added;
#'   \code{proportion_used}, the proportion of the observations used to fit the
#'     detection function in the case of truncation.
#' @details The type of detection function (line or point) is determined by
#'   the \code{unit} argument.
#' @seealso \code{\link[Distance]{ds}}
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   radius_model <- fit_detmodel(radius~1, pkg, order=0)
#'   angle_model <- fit_detmodel(angle~1, pkg, order=0)
#'   radius_model$edd
#'   angle_model$edd
#' @export
#'
fit_detmodel <- function(formula,
                         package,
                         species=NULL,
                         newdata=NULL,
                         unit=c("m", "km", "cm", "degree", "radian"),
                         ...){
  unit <- match.arg(unit)

  # get and check model variables
  allvars <- all.vars(formula)
  depvar <- allvars[1]
  covars <- tail(allvars, -1)
  data <- package$data$observations
  if(!all(allvars %in% names(data))) stop("Can't find all model variables in data")
  if("distance" %in% covars) stop("Cannot use \"distance\" as a covariate name - rename and try again")

  # set up data
  if(is.null(species)) species <- select_species(package)
  data <- data %>%
    subset(scientificName==species) %>%
    dplyr::select(all_of(allvars)) %>%
    tidyr::drop_na() %>%
    as.data.frame()
  if("useDeployment" %in% names(data)) data <- subset(data, useDeployment)
  if(nrow(data) == 0) stop("There are no usable position data")

  classes <- dplyr::summarise_all(data, class)
  if(classes[depvar]=="numeric"){
    data <- data %>%
      dplyr::rename(distance=all_of(depvar)) %>%
      dplyr::mutate(distance=abs(distance))
  } else{
    cats <- strsplit(as.character(dplyr::pull(data, depvar)), "-")
    data$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    data$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    data$distance <- (data$distbegin + data$distend) / 2
  }

  # model fitting
  type <- if(unit %in% c("m", "km", "cm")) "point" else "line"
  args <- c(data=list(data), formula=formula[-2], transect=type, list(...))
  mod <- suppressWarnings(suppressMessages(do.call(Distance::ds, args)$ddf))

  # esw prediction
  if(length(covars)==0)
    newdata <- data.frame(x=0) else{
      if(is.null(newdata)){
        newdata <- data %>% dplyr::select(all_of(covars)) %>%
          lapply(function(x)
            if(is.numeric(x)) mean(x, na.rm=T) else sort(unique(x)))  %>%
          expand.grid()
      } else{
        if(!all(covars %in% names(newdata))) stop("Can't find all model covariates in newdata")
      }}
  prdn <- predict(mod, newdata, esw=TRUE, se.fit=TRUE)
  if(mod$meta.data$point){
    prdn$se.fit <- 0.5 * prdn$se.fit / (pi * prdn$fitted)^0.5
    prdn$fitted <- sqrt(prdn$fitted/pi)
  }
  ed <- cbind(estimate=prdn$fitted, se=prdn$se.fit)
  if(length(covars)>=1) ed <- cbind(newdata, ed)
  mod$edd <- ed
  mod$unit <- unit
  mod$proportion_used <- nrow(mod$data) / nrow(data)
  mod
}

#' Get REM data from a camtrap-dp datapackage
#'
#' Extracts a data table of observation counts and effort for each
#' camera location in a camtrap-dp data package.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to extract
#'   data for; if NULL runs select_species to get user input.
#' @param unit The time unit in which to return camera effort.
#' @return A tibble with columns:
#'   - \code{locationName}: name of the camera location
#'   - \code{effort}: the camera time for the location
#'   - \code{unit}: the effort time unit
#'   - \code{scientificName}: the scientific name of the species data extracted
#'   - \code{n}: the observation counts
#'   - \code{stratumID}: stratum identifier (only if this column is present in
#'     \code{package$data$deployments})
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   remdata <- get_rem_data(pkg)
#' @export
#'
get_rem_data <- function(package, species=NULL,
                         unit=c("day", "hour", "minute", "second")){
  unit <- match.arg(unit)
  if(is.null(species)) species <- select_species(package)
  dep <- package$data$deployments
  eff <- package %>%
    camtraptor::get_effort(unit=unit) %>%
    dplyr::select(deploymentID, effort)
  res <- package %>%
    camtraptor::get_n_individuals(species=species) %>%
    suppressMessages() %>%
    dplyr::left_join(dep, by="deploymentID") %>%
    dplyr::left_join(eff, by="deploymentID") %>%
    dplyr::group_by(locationName) %>%
    dplyr::summarise(n = sum(n), effort=sum(effort))
  if("stratumID" %in% names(dep)){
    str <- dep %>%
      dplyr::group_by(locationName) %>%
      dplyr::summarise(stratumID = unique(stratumID), .groups="keep")
    if(any(table(str$locationName) > 1))
      stop("Some locations appear in more than one stratum in the deployments data") else
        res <- dplyr::left_join(res, str, by="locationName")
  }
  res$effort_unit <- unit
  res$species <- species
  res
}
#' Get average trap rate from REM data
#'
#' Calculates average trap rate and its bootstrapped error from a table of
#' per-location observation counts and camera time.
#'
#' @param data A dataframe containing (at least) columns \code{n} and
#'   \code{effort}, as returned by \code{\link{get_rem_data}}; if \code{strata}
#'   supplied for stratified calculation, must also have column \code{stratumID}.
#' @param strata A dataframe with one row per stratum, and columns
#'   \code{stratumID} and \code{area}.
#' @param reps The number of bootstrap replicates to run.
#' @return A dataframe with columns:
#'   - \code{estimate}: average trap rate
#'   - \code{se}: standard error
#'   - \code{cv}: proportional coefficient of variation
#'   - \code{lcl95}, \code{ucl95}: lower and upper 95% confidence limits
#'   - \code{n}: sample size (number of locations)
#'   - \code{unit}: the unit of the estimate
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   remdata <- get_rem_data(pkg)
#'   get_trap_rate(remdata)
#' @export
#'
get_trap_rate <- function(data, strata=NULL, reps=999){

  traprate <- function(data){
    if(is.null(strata)){
      sum(data$n) / sum(data$effort)
    } else{
      local_density <- sapply(strata$stratumID, function(stratum){
        i <- data$stratumID==stratum
        sum(data$n[i]) / sum(data$effort[i])
      })
      sum(local_density * strata$area) / sum(strata$area)
    }
  }

  sampled_traprate <- function(){
    i <- if(is.null(strata))
      sample(1:nrow(data), replace=TRUE) else
        as.vector(sapply(strata$stratumID, function(stratum){
          sample(which(data$stratumID==stratum), replace=TRUE)
        }))
    traprate(data[i, ])
  }

  if(!all(c("effort", "n") %in% names(data)))
    stop("data must contain (at least) columns effort and observations")
  if(!is.null(strata)){
    if(!"stratumID" %in% names(data))
      stop("data must contain column stratumID for stratified analysis")
    if(!all(c("stratumID", "area") %in% names(strata)))
      stop("strata must contain columns stratumID and area")
    if(!all(data$stratumID %in% strata$stratumID))
      stop("Not all strata in data are present in strata")
  }

  tr_sample <- replicate(reps, sampled_traprate())
  est <- traprate(data)
  se <- sd(tr_sample)
  cv <- se/est
  ci <- unname(quantile(tr_sample, c(0.025, 0.975)))
  data.frame(estimate = est,
             se = se,
             cv = cv,
             lcl95 = ci[1],
             ucl95 = ci[2],
             n = nrow(data),
             unit = paste("n", data$effort_unit[1], sep="/"),
             row.names = "trap_rate")
}

#' Log-normal confidence interval
#'
#' Calculates approximate log-normal confidence intervals given an estimate
#' and its standard error.
#'
#' @param estimate Numeric estimate value(s)
#' @param se Standard error(s) of the estimate
#' @return A dataframe with a row per estimate input, and columns \code{lcl}
#'   and \code{ucl} (lower and upper confidence limits).
#' @examples
#'   lnorm_confint(10.13, 3.57)
#' @export
#'
lnorm_confint <- function(estimate, se, percent=95){
  if(length(estimate) != length(se))
    stop("estimate and se must have the same number of values")
  z <- qt((1 - percent/100) / 2, Inf, lower.tail = FALSE)
  w <- exp(z * sqrt(log(1 + (se/estimate)^2)))
  data.frame(lcl=estimate/w, ucl=estimate*w)
}

#' Create a parameter table from a set of models
#'
#' Creates a table of REM parameters taken from models for detection radius,
#' detection angle, speed and activity level.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param radius_model A detection radius model fitted using \code{\link{fit_detmodel}}
#' @param angle_model A detection angle model fitted using \code{\link{fit_detmodel}}
#' @param speed_model A speed model fitted using \code{\link{fit_speedmodel}}
#' @param activity_model An activity  model fitted using \code{\link{fit_actmodel}}
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs select_species to get user input.
#' @param strata A dataframe of stratum information passed to \code{\link{get_trap_rate}}
#' @param reps Number of bootstrap replicates for estimating trap rate error
#'   (see \code{\link{get_trap_rate}})
#' @return A dataframe of unit-harmonised parameter estimates with seven rows:
#'   - \code{radius}: detection radius
#'   - \code{angle}: detection angle
#'   - \code{active_speed}: speed while active
#'   - \code{activity_level}: proportion of time spent active
#'   - \code{overall_speed}: long-term average speed (day range) - the product
#'     of \code{active_speed} and \code{activity_level}
#'   - \code{trap_rate}: number of camera trap records per unit time
#'  and seven columns:
#'   - \code{estimate}: parameter estimates
#'   - \code{se}: standard error
#'   - \code{cv}: proportional coefficient of variation
#'   - \code{lcl95}, \code{ucl95}: lower and upper 95% confidence limits
#'   - \code{n}: sample size
#'   - \code{unit}: the unit of the estimate
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   sp <- "Vulpes vulpes"
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   get_parameter_table(pkg, radmod, angmod, spdmod, actmod, sp)
#' @export
#'
get_parameter_table <- function(package,
                                radius_model,
                                angle_model,
                                speed_model,
                                activity_model,
                                species=NULL,
                                strata = NULL,
                                reps = 999){
  if(is.null(species)) species <- select_species(package)
  # Get parameters and SEs
  res <- data.frame(rbind(radius_model$edd,
                          angle_model$edd * 2,
                          speed_model$speed,
                          activity_model@act[1:2]))
  # Calculate overall speed (day range) and its SE
  ospd <- res[3,1] * res[4,1]
  se_ospd <- ospd * sqrt(sum((res[3:4, 2] / res[3:4, 1])^2))
  res <- rbind(res, c(ospd, se_ospd))
  res$cv <- res$se / res$estimate
  # Add confidence limits, sample sizes, units and row names
  res$lcl95 <- res$estimate - 1.96*res$se
  res$ucl95 <- res$estimate + 1.96*res$se
  res$n <- c(nrow(radius_model$data),
             nrow(angle_model$data),
             length(speed_model$data),
             length(activity_model@data),
             NA)
  speed_unit <- paste(speed_model$distUnit, speed_model$timeUnit, sep="/")
  res$unit <- c(radius_model$unit,
                angle_model$unit,
                speed_unit,
                "none",
                speed_unit)
  rownames(res) <- c("radius",
                     "angle",
                     "active_speed",
                     "activity_level",
                     "overall_speed")
  # Add trap rate data, including correction for truncation of radius model
  data <- get_rem_data(package, species)
  traprate <- get_trap_rate(data, strata, reps)
  j <- c("estimate", "se", "lcl95", "ucl95")
  traprate[, j] <- traprate[, j] * radius_model$proportion_used
  res <- rbind(res, traprate)

  convert_units(res)
}

#' Get a unit multiplier
#'
#' Returns a multiplier to convert a value from one unit to another in
#' one of three types: distance, time and angle.
#'
#' @param unitIN A text value giving the unit of the input; must be one of
#'   "cm", "m", "km" for distances, "second", "minute", "hour", "day" for
#'   times, "radian" "degree" for angles, or "n/ha", "n/km2" "n/100km2" for
#'   density.
#' @param unitOUT The same for output; must be of the same type (distance,
#'   time or density) as \code{unitIN}.
#' @return A number giving the amount by which to multipy input values
#'   to arrive a unit-converted output.
#' @examples
#'   get_multiplier("m", "km")
#' @export
#'
get_multiplier <- function(unitIN, unitOUT){
  dunits <- c("cm", "m", "km")
  dmult <- c(1, 1e2, 1e5)
  tunits <- c("second", "minute", "hour", "day")
  tmult <- c(1, 60, 60^2, 24*60^2)
  aunits <- c("radian", "degree")
  amult <- c(1, pi/180)
  areaUnits <- c("n/ha", "n/km2", "n/100km2")
  areaMult <- c(1, 1e2, 1e4)
  if(unitIN %in% dunits & unitOUT %in% dunits){
    u <- dunits
    m <- dmult
    n <- length(dunits)
  } else
    if(unitIN %in% tunits & unitOUT %in% tunits){
      u <- tunits
      m <- tmult
      n <- length(tunits)
    } else
      if(unitIN %in% aunits & unitOUT %in% aunits){
        u <- aunits
        m <- amult
        n <- length(aunits)
      } else
        if(unitIN %in% areaUnits & unitOUT %in% areaUnits){
          u <- areaUnits
          m <- areaMult
          n <- length(areaUnits)
        } else
          stop("Units not of the same type or not recognised")

  tab <- data.frame(from = rep(u, each=n),
                    to = rep(u, n),
                    mult = rep(m, each=n) / rep(m, n))
  tab$mult[tab$from==unitIN & tab$to==unitOUT]
}

#' Change the units of an REM parameter table
#'
#' Changes the units of parameters from their current setting to new
#' user-defined units.
#'
#' @param param An REM parameter dataframe as created with
#'   \code{\link{get_parameter_table}}. Must contain columns \code{estimate},
#'   \code{se}, \code{lcl95}, \code{ucl95} and \code{unit}, and at least one
#'   row named from \code{radius}, \code{angle}, \code{active_speed},
#'   \code{overall_speed}.
#' @param radius_unit A character string giving the output unit of radius.
#' @param angle_unit A character string giving the output unit of angle.
#' @param active_speed_unit A character string giving the output unit of
#'   speed while active.
#' @param overall_speed_unit A character string giving the output unit of
#'   day range.
#' @param density_unit A character string giving the output unit of density.
#' @return A replica of input dataframe \code{param} with \code{estimate},
#'   \code{se} and confidence limit values converted to output units.
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   sp <- "Vulpes vulpes"
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   param <- get_parameter_table(pkg, radmod, angmod, spdmod, actmod, sp)
#'   convert_units(param, radius_unit="m", angle_unit="degree")
#' @export
convert_units <- function(param,
                          radius_unit=c("km", "m", "cm"),
                          angle_unit=c("radian", "degree"),
                          active_speed_unit=c("km/day", "km/hour", "m/hour", "m/second"),
                          overall_speed_unit=c("km/day", "km/hour", "m/hour", "m/second"),
                          trap_rate_unit=c("day", "hour", "minute", "second"),
                          density_unit=c("n/km2", "n/ha", "n/100km2")){
  radius_unit <- match.arg(radius_unit)
  angle_unit <- match.arg(angle_unit)
  active_speed_unit <- match.arg(active_speed_unit)
  overall_speed_unit <- match.arg(overall_speed_unit)
  trap_rate_unit <- match.arg(trap_rate_unit)
  density_unit <- match.arg(density_unit)

  j <- c("estimate", "se", "lcl95", "ucl95")

  if("radius" %in% rownames(param)){
    m <- get_multiplier(param["radius", "unit"], radius_unit)
    param["radius", j] <- param["radius", j] * m
    param["radius", "unit"] <- radius_unit
  }

  if("angle" %in% rownames(param)){
    m <- get_multiplier(param["angle", "unit"], angle_unit)
    param["angle", j] <- param["angle", j] * m
    param["angle", "unit"] <- angle_unit
  }

  if("active_speed" %in% rownames(param)){
    unit_from <- unlist(strsplit(param["active_speed", "unit"], "/"))
    unit_to <- unlist(strsplit(active_speed_unit, "/"))
    dm <- get_multiplier(unit_from[1], unit_to[1])
    tm <- get_multiplier(unit_from[2], unit_to[2])
    param["active_speed", j] <- param["active_speed", j] * dm / tm
    param["active_speed", "unit"] <- paste(unit_to, collapse="/")
  }

  if("overall_speed" %in% rownames(param)){
    unit_from <- unlist(strsplit(param["overall_speed", "unit"], "/"))
    unit_to <- unlist(strsplit(overall_speed_unit, "/"))
    dm <- get_multiplier(unit_from[1], unit_to[1])
    tm <- get_multiplier(unit_from[2], unit_to[2])
    param["overall_speed", j] <- param["overall_speed", j] * dm / tm
    param["overall_speed", "unit"] <- paste(unit_to, collapse="/")
  }

  if("trap_rate" %in% rownames(param)){
    unit_from <- unlist(strsplit(param["trap_rate", "unit"], "/"))[2]
    m <- get_multiplier(unit_from, trap_rate_unit)
    param["trap_rate", j] <- param["trap_rate", j] / m
    param["trap_rate", "unit"] <- paste0("n/", trap_rate_unit)
  }

  if("density" %in% rownames(param)){
    m <- get_multiplier(param["density", "unit"], density_unit)
    param["density", j] <- param["density", j] / m
    param["density", "unit"] <- density_unit
  }

  param
}

#' Fit a random encounter model
#'
#' Estimates REM density given a dataframe of parameters and their errors.
#'
#' @param parameters A dataframe containing REM parameter estimates with
#'   (at least) rows:
#'   - \code{radius}: effective detection radius
#'   - \code{angle}: effective detection angle
#'   - \code{overall_speed}: average animal speed (day range)
#'   - \code{trap_rate}: animal observations per unit time
#'  and columns:
#'   - \code{estimate}: numeric parameter estimate
#'   - \code{se}: numeric parameter standard error
#'   - \code{unit}: character parameter units (see \code{\link{convert_units}}
#'     for allowable values)
#' @return A dataframe with the input parameters plus estimated density and
#'  its errors.
#' @examples
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   sp <- "Vulpes vulpes"
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   param <- get_parameter_table(pkg, radmod, angmod, spdmod, actmod, sp)
#'   rem(param)
#' @export
rem <- function(parameters){
  required_rows <- c("trap_rate", "overall_speed", "radius", "angle")
  required_cols <- c("estimate", "se", "unit")

  if(!all(required_rows %in% rownames(parameters)) |
     !all(required_cols %in% colnames(parameters)))
    stop(paste("parameters must have (at least) row names:",
               paste(required_rows, collapse=", "),
               ";\nand (at least) column names:",
               paste(required_cols, collapse=", ")))

  param <- convert_units(parameters)

  Es <- (param[required_rows, "estimate"] + c(0,0,0,2)) ^ c(1,-1,-1,-1)
  density <- pi * prod(Es)
  CVs <- param[required_rows, "cv"]
  CVs[4] <- param["angle", "se"] / Es[4]

  density_CV <- sqrt(sum(CVs^2))
  density_SE <- density_CV * density
  density_CI <- unname(lnorm_confint(density, density_SE))
  rbind(parameters, density=c(density,
                              density_SE,
                              density_CV,
                              density_CI[1],
                              density_CI[2],
                              NA,
                              "n/km2"))
}

#' Integrated random encounter model density estimate
#'
#' Estimates animal density for a given species given a camtrapDP datapackage.
#' Models for detection radius and angle, speed and/or activity level can be
#' fitted externally and provided as arguments, or are fitted internally if not
#' provided (\code{NULL} default). Input units are assumed to be distance in m
#' and time in seconds.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param check_deployments Logical indicating whether to check deployment
#'   calibration model diagnostic plots. If \code{TRUE} (default) runs
#'   \code{check_deployment_models}; radius, angle and speed data from any
#'   excluded deployments are then dropped from analysis. If \code{FALSE}
#'   all data are used.
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs \code{\link{select_species}} to get user input.
#' @param radius_model A detection function model for radii fitted using
#'   \code{\link{fit_detmodel}}.
#' @param angle_model A detection function model for angles fitted using
#'   \code{\link{fit_detmodel}} with \code{unit} argument "radian" or "degree".
#' @param speed_model A speed model fitted using \code{fit_speedmodel}.
#' @param activity_model An activity model fitted using
#'   \code{\link[activity]{fitact}} or \code{\link{fit_actmodel}}.
#' @param strata A dataframe of stratum areas, passed to \code{\link{get_trap_rate}}.
#' @param reps Number of bootstrap replicates for error estimation.
#' @return A dataframe containing estimates and their errors for density and
#'   all contributing parameters.
#' @examples
#'   # Load data
#'   pkg <- camtraptor::read_camtrap_dp("./data/datapackage.json")
#'   # Sense check deployment schedules
#'   plot_deployment_schedule(pkg)
#'   # Sense check deployment calibration model diagnostic plots
#'   pkg_checked <- check_deployment_models(pkg)
#'
#'   # Fully automated analysis (reps reduced for to limit run time)
#'   res <- rem_estimate(pkg_checked, check_deployments=FALSE, reps=100)
#'   res$estimates
#'
#'   # Analysis with one parameter model fitted separately
#'   sp <- "Vulpes vulpes"
#'   radmod <- fit_detmodel(radius~1, pkg_checked, species=sp, truncation=15, order=0)
#'   res <- rem_estimate(pkg_checked, check_deployments = FALSE, species = sp,
#'                       radius_model = radmod, reps=100)
#'   res$estimates
#' @export
#'
rem_estimate <- function(package,
                         check_deployments=TRUE,
                         species=NULL,
                         radius_model=NULL,
                         angle_model=NULL,
                         speed_model=NULL,
                         activity_model=NULL,
                         strata=NULL,
                         reps=999){

  if(check_deployments) package <- check_deployment_models(package)
  if(is.null(species)) species <- select_species(package)
  message(paste("Analysing", species))

  if(is.null(radius_model))
    radius_model <- fit_detmodel(radius~1, package, species,
                                 order=0, truncation=12)

  if(is.null(angle_model))
    angle_model <- fit_detmodel(angle~1, package, species,
                                order=0, unit="radian")

  if(is.null(speed_model))
    speed_model <- fit_speedmodel(package, species)

  if(is.null(activity_model))
    activity_model <- fit_actmodel(package, species, reps)

  parameters <- get_parameter_table(package,
                                    radius_model,
                                    angle_model,
                                    speed_model,
                                    activity_model,
                                    species,
                                    strata,
                                    reps)

  estimates <- rem(parameters) %>%
    convert_units(radius_unit = "m",
                  angle_unit = "degree",
                  active_speed_unit = "km/hour",
                  overall_speed_unit = "km/day")

  list(species=species, data=data, estimates=estimates,
       speed_model=speed_model, activity_model=activity_model,
       radius_model=radius_model, angle_model=angle_model)
}

