#'
#' Data and metadata from an example study exported from the Agouti camera trap
#' data management platform in camtrap-DP format. Metadata includes study name,
#' authors, location and other details. Data is held in element \code{data},
#' itself a list holding dataframes \code{deployments}, \code{media} and
#' \code{observations}. See \url{https://tdwg.github.io/camtrap-dp} for details.
#'
#' @format A list holding study data and metadata.
#' @name pkg
#' @docType data
NULL

#' Create a datapackage from csv files
#'
#' Reads csv data from a folder into a list that functions like a camtrap-dp
#' object for the purposes of density estimation.
#'
#' @param folder A character string giving the folder in which the csv files sit.
#' @param tryFormats A character string defining date-time format, passed to as.POSIXct.
#' @return As for \code{\link[camtraptor]{read_camtrap_dp}} but with reduced
#'  package metadata.
#' @details The folder must contain three csv files: deployments.csv, media.csv
#'  and observations.csv, each of which must contain at least the following fields:
#'  \itemize{
#'   \item{deployments: }{deploymentID, locationID, locationName, longitude, latitude,start,end}
#'   \item{media: }{mediaID, deploymentID, sequenceID, timestamp, filePath, fileName}
#'   \item{observations: }{observationID, deploymentID, sequenceID, mediaID, timestamp, scientificName, count, speed, radius, angle}
#' }
#' @examples
#'   \dontrun{pkg <- read_camtrap_dp_csv("./data")}
#' @export
#'
#'
read_camtrap_dp_csv <- function(folder,
                                tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                               "%Y/%m/%d %H:%M:%OS",
                                               "%Y:%m:%d %H:%M:%OS")){

  req_files <- c("deployments.csv", "observations.csv")
  dep_fields <- c("deploymentID","locationID","locationName",
                  "longitude","latitude","start","end")
  med_fields <- c("mediaID","deploymentID","sequenceID",
                  "timestamp","filePath","fileName")
  obs_fields <- c("observationID","deploymentID","sequenceID","mediaID",
                  "timestamp","scientificName","count","speed","radius","angle")

  full_files <- list.files(folder, ".csv", full.names=TRUE)
  csv_files <- basename(full_files)
  files <- tools::file_path_sans_ext(csv_files)
  if(!all(req_files %in% csv_files))
    stop("Folder must contain at least deployments and observations csv files")

  dat <- lapply(full_files, read.csv)
  names(dat) <- files
  deperr <- !all(dep_fields %in% names(dat$deployments))
  obserr <- !all(obs_fields %in% names(dat$observations))
  if("media" %in% files)
    mederr <- !all(med_fields %in% names(dat$media)) else
      mederr <- FALSE

  if(any(deperr, mederr, obserr)){
    if(deperr) message(paste("deployments.csv must contain at least these fields:/n"),
                       paste(dep_fields, collapse = ", "))
    if(mederr) message(paste("media.csv must contain at least these fields:/n"),
                       paste(med_fields, collapse = ", "))
    if(obserr) message(paste("observations.csv must contain at least these fields:/n"),
                       paste(obs_fields, collapse = ", "))
    stop("See above")
  }

  dat$deployments <- dat$deployments %>%
    dplyr::mutate(start = as.POSIXct(start, tz="UTC", tryFormats=tryFormats),
           end = as.POSIXct(end, tz="UTC", tryFormats=tryFormats))
  if("media" %in% files)
    dat$media <- dat$media %>%
      dplyr::mutate(timestamp = as.POSIXct(timestamp, tz="UTC", tryFormats=tryFormats))
  dat$observations <- dat$observations %>%
    dplyr::mutate(timestamp = as.POSIXct(timestamp, tz="UTC", tryFormats=tryFormats))

  spp <- unique(dat$observation$scientificName)

  list(name = basename(folder),
       directory = normalizePath(folder),
       taxonomic = lapply(spp, function(sp) list(taxonID=sp,
                                                 taxonIDReference="observations.csv",
                                                 scientificName=sp)),
       data = dat
  )
}

#' Read Camtrap DP data
#'
#' Reads data from a Camtrap DP datapackage Agouti export in either V0.1 or V1.
#' If data V0.1, uses camtraptor::read_camtrap_dp. If data are V1, effectively
#' down-versions to V0.1, and recalculates speeds. Positions are reordered
#' correctly before calculating speed (not always the case in raw output).
#' Observations with zero time difference (hence infinite speed) impute time
#' for speed calculation from average frame rate.
#'
#' @param file Path to a datapackage.json file.
#' @param resort Logical defining whether to re-sort sequences (see details).
#' @return As for \code{\link[camtraptor]{read_camtrap_dp}} with the addition
#'  of $data$positions table, containing the original media observations.
#' @details Occasionally, images may be mis-ordered within sequences when
#'  timestamps of adjacent images are equal, leading to mis-specification of
#'  speeds and initial positions. To fix this, \code{resort} can be set TRUE,
#'  and images will be rearranged according to original file name in the
#'  positions table, before recalculating speeds and initial positions in the
#'  observations table. Note that this only works if the fileName field of the
#'  $data$media table has preserved the original alphanumeric file name following
#'  the upload time suffix (e.g. "20231019073014-IMG0001.JPG").
#' @examples
#'   \dontrun{pkg <- read_camtrapDP("./data/datapackage.json")}
#' @export
#'
#'
read_camtrapDP <- function(file, resort=FALSE){

  convert_date <- function(x){
    x <- paste0(substr(x, 1, 22), substr(x, 24, 25))
    as.POSIXct(x, format="%FT%T%z", tz="UTC")
  }

  # Read json and add directory / taxonomicIDReference
  dir <- normalizePath(dirname(file), "/")
  res <- jsonlite::read_json(file)
  res$directory <- dir
  res$taxonomic <- lapply(res$taxonomic, function(x)
    c(x, taxonIDReference=dirname(x$taxonID)))

  # Check version and use camtraptor if V0, otherwise bespoke
  version <- substr(basename(dirname(res$profile)), 1, 1)
  if(version == "0"){
    res <- camtraptor::read_camtrap_dp(file)
  } else{
    # Read and modify csv files
    deployments <- read.csv(file.path(dir, "deployments.csv")) %>%
      dplyr::rename(start = deploymentStart, end = deploymentEnd) %>%
      dplyr::mutate(start = convert_date(start),
                    end = convert_date(end))
    gaps <- with(deployments,
                is.na(start) | is.na(end) | is.na(latitude) | is.na(longitude))
    if(any(gaps)) stop(
      paste(c("These deployments have missing dates and/or positions:",
              deployments$deploymentID[gaps]), collapse="\n"))

    media <- read.csv(file.path(dir, "media.csv")) %>%
      dplyr::arrange(deploymentID, timestamp) %>%
      dplyr::mutate(timestamp = convert_date(timestamp))

    observations <- read.csv(file.path(dir, "observations.csv")) %>%
      dplyr::rename(sequenceID = eventID) %>%
      dplyr::mutate(eventStart = convert_date(eventStart),
                    eventEnd = convert_date(eventEnd))

    # Extract media observations with fileName and timestamp added from media,
    # sort media chronologically within events if specified
    medobs <- observations %>%
      dplyr::filter(observationLevel == "media") %>%
      dplyr::left_join(dplyr::select(media, mediaID, fileName, timestamp), by="mediaID")
    if(resort){
      id <- medobs$individualID
      i <- c(0, cumsum(head(id, -1) != tail(id, -1)))
      filenm <- with(medobs, substr(fileName,
                                    1+regexpr("-", fileName),
                                    nchar(fileName)))
      medobs <- dplyr::arrange(medobs, i, filenm)
    }

    # Add stepwise distances, time differences and image counter
    r1 <- head(medobs$individualPositionRadius, -1)
    r2 <- tail(medobs$individualPositionRadius, -1)
    a1 <- head(medobs$individualPositionAngle, -1)
    a2 <- tail(medobs$individualPositionAngle, -1)
    t1 <- head(medobs$timestamp, -1)
    t2 <- tail(medobs$timestamp, -1)
    id1 <- head(medobs$individualID, -1)
    id2 <- tail(medobs$individualID, -1)
    dist <- sqrt(r1^2 + r2^2 - 2*r1*r2*cos(a2-a1))
    tdiff <- as.numeric(difftime(t2, t1, units="secs"))
    dist[id1!=id2] <- NA
    tdiff[id1!=id2] <- NA
    medobs$dist <- c(NA, dist)
    medobs$tdiff <- c(NA, tdiff)
    medobs$imgCount <- sequence(table(cumsum(c(0, id2!=id1))))

    # Summarise events for distance traveled, time difference,
    # number of steps, first position radius and angle
    evobs <- medobs %>%
      dplyr::group_by(individualID) %>%
      dplyr::summarise(dist = sum(dist, na.rm=TRUE),
                       tdiff = sum(tdiff, na.rm=TRUE),
                       steps = which(media$mediaID==mediaID[imgCount==max(imgCount)]) -
                         which(media$mediaID == mediaID[imgCount==1]),
                       radius = individualPositionRadius[imgCount==1],
                       angle = individualPositionAngle[imgCount==1])

    # Calculate speed, substituting mean step duration for zero time differences
    secs_per_img <- mean(evobs$tdiff / evobs$steps, na.rm=TRUE)
    evobs <- evobs %>%
      dplyr::mutate(tdiff = dplyr::if_else(tdiff==0, secs_per_img * steps, tdiff),
                    speed = dplyr::if_else(steps>0, dist/tdiff, NA))

    # Add newly calculated radius, angle, speed etc to event observations
    observations <- observations %>%
      dplyr::filter(observationLevel=="event") %>%
      dplyr::select(-individualPositionRadius, -individualPositionAngle, -individualSpeed) %>%
      dplyr::left_join(evobs, by="individualID") %>%
      dplyr::mutate(timestamp = eventStart)

    # Add data tables to output
    res$data <- list(deployments=deployments,
                     media=media,
                     observations=observations,
                     positions=medobs)
  }

  return(res)
}

#' Plot a map of deployments
#'
#' Creates an OpenStreetMap street or satellite map over-plotted with
#' deployment locations.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param basemap Basemap to plot, street (default) or satellite
#' @param ... Additional arguments passed to
#'   \code{\link[leaflet]{addCircleMarkers}}
#' @examples
#'   \dontrun{pkg <- read_camtrapDP("./data/datapackage.json")}
#'   data(pkg)
#'   map_deployments(pkg)
#' @export
#'
#'
map_deployments <- function(pkg, basemap=c("street", "satellite"), ...){
  basemap <- match.arg(basemap)
  map <- leaflet::leaflet(data = pkg$data$deployments)
  map <- if(basemap == "street")
    leaflet::addTiles(map) else
      leaflet::addProviderTiles(map,"Esri.WorldImagery")
  leaflet::addCircleMarkers(map, lng = ~longitude, lat = ~latitude,
                            popup = ~locationName, ...)
}


#' Plot a map of deployment trap rates
#'
#' Creates an OpenStreetMap street or satellite map over-plotted with
#' deployment locations, with points sized in proportion to trap rate for a
#' given species.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to analyse.
#'   Use scientific names. If NULL runs select_species to get user input;
#'   if all uses all data.
#' @param basemap Basemap to plot, street (default) or satellite
#' @param minSize Minimum point size to plot.
#' @param maxSize Maximum point size to plot.
#' @examples
#'   \dontrun{pkg <- read_camtrapDP("./data/datapackage.json")}
#'   data(pkg)
#'   map_traprates(pkg, species="Vulpes vulpes")
#' @export
#'
#'
map_traprates <- function(pkg, species=NULL, basemap=c("street", "satellite"),
                          maxSize=25, minSize=3){

  szfunc <- function(x, mxx)
    minSize + (maxSize-minSize) * x / mxx

  basemap <- match.arg(basemap)
  trdat <- pkg %>%
    get_traprate_data(species=species) %>%
    dplyr::mutate(tr = 100 * n/effort,
           sz = szfunc(tr, max(tr)),
           col = ifelse(tr==0, "red", "blue"))

  labels <- pretty(c(0,max(trdat$tr)), n=3)
  nlabs <- length(labels)
  cols <- c("red", rep("blue", nlabs-1))
  szs <- 2 * szfunc(labels, max(trdat$tr))
  cols <- paste0(cols, "; width:", szs, "px; height:", szs,
                 "px; border:0px solid ", cols,
                 "; border-radius:50%")

  map <- leaflet::leaflet(data = trdat)
  map <- if(basemap == "street")
    leaflet::addTiles(map) else
      leaflet::addProviderTiles(map,"Esri.WorldImagery")
  leaflet::addCircleMarkers(map, lng = ~longitude, lat = ~latitude,
                            popup = ~paste0(locationName, ": ",
                                            round(tr, 2-floor(log10(tr))),
                                            " (", n, " records)"),
                            radius = ~sz,
                            color= ~col,
                            weight=0,
                            fillOpacity = 1) %>%
    leaflet::addLegend(position = "topright",
              colors = cols,
              labels = labels,
              opacity = 1,
              title = "Records/100 days")
}

#' Plot a deployment Gantt chart
#'
#' Plots an interactive Gantt chart illustrating deployment times (black lines)
#' and the occurrence of observations within those deployments (orange bars).
#' Useful for checking errors in specification of deployment start and end
#' dates, and visualising spatiotemporal distribution of observations.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @examples
#'   \dontrun{pkg <- read_camtrapDP("./data/datapackage.json")}
#'   data(pkg)
#'   plot_deployment_schedule(pkg)
#' @export
#'

plot_deployment_schedule <- function(package){

  depdat <- package$data$deployments
  i <- gtools::mixedorder(depdat$locationName)
  depdat <- depdat[i,] %>%
    dplyr::mutate(deploymentID=factor(deploymentID, deploymentID))
  obsdat <- package$data$observations

  plt <- ggplot2::ggplot() +
    ggplot2::geom_point(data = obsdat,
                        mapping = ggplot2::aes(.data$deploymentID,
                                               .data$timestamp),
                        shape=45, color="tan2") +
    ggplot2::geom_segment(data = depdat,
                          mapping = ggplot2::aes(x = .data$deploymentID,
                                                 xend = .data$deploymentID,
                                                 y = .data$start,
                                                 yend = .data$end)) +
    ggplot2::scale_x_discrete(labels=depdat$locationName) +
    ggplot2::scale_y_datetime(date_labels="%Y/%m/%d") +
    ggplot2::labs(x="Location", y=ggplot2::element_blank()) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5))
  plotly::ggplotly(plt)
}

#' Subset a camera trap datapackage by deployment (deprecated)
#'
#' Select a subset of deployments from a datapackage defined by
#' a choice based on columns in the deployments table. Functionality
#' now provided in filter_camtrap_dp.
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
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   subpkg <- subset_deployments(pkg,
#'                                locationName != "S01" &
#'                                start >= as.POSIXct("2017-10-01", tz="UTC") &
#'                                end <= as.POSIXct("2017-10-31", tz="UTC"))
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


#' Take a time slice of a data package
#'
#' Discards any observations, media, or deployments that fall wholly outside
#' the time range defined by start and end. When start or end are not
#' specified, no slicing is applied to start or end points respectively.
#' Slicing applies to all deployments by default, or can be applied to only a
#' subset of deployments specified by depChoice. The fully default behaviour
#' for this function is therefore to do nothing (the datapackage is returned
#' unchanged).
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param start,end Single character or POSIXct values defining the
#'   time range within which to slice the package.
#' @param depChoice A logical expression using column names from the
#'  deployments table defining which deployments to slice.
#' @return As for \code{\link[camtraptor]{read_camtrap_dp}}, with all
#'  data tables reduced according to the choice criteria.
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#' # Slicing the whole package to mid October 2017
#'   subpkg <- slice_camtrap_dp(pkg,
#'                               start = "2017/10/10",
#'                               end = "2017/10/20")
#' # Slicing only deployments at location "S03" to a specific start time/date
#'   subpkg <- slice_camtrap_dp(pkg,
#'                               start = "2017/10/15 14:30:00",
#'                               depChoice = locationName=="S03")
#' @export
#'
slice_camtrap_dp <- function(package,
                             start = NULL,
                             end = NULL,
                             depChoice = NULL){

  if(missing(depChoice)) depChoice <- TRUE
  deps <- package$data$deployments %>%
    dplyr::filter({{depChoice}}) %>%
    dplyr::pull(deploymentID)

  startCut <- if(is.null(start))
    min(package$data$deployments$start) else
      as.POSIXct(start, tz="UTC")

  endCut <- if(is.null(end))
    max(package$data$deployments$end) else
      as.POSIXct(end, tz="UTC")

  package$data$deployments <- package$data$deployments %>%
    dplyr::mutate(start = dplyr::case_when(end<=startCut & {{depChoice}} ~ NA,
                                           start>=endCut & {{depChoice}} ~ NA,
                                           start<=startCut & {{depChoice}} ~ startCut,
                                           .default = start),
                  end = dplyr::case_when(end<=startCut & {{depChoice}} ~ NA,
                                         start>=endCut & {{depChoice}} ~ NA,
                                         end>=endCut & {{depChoice}} ~ endCut,
                                         .default = end)) %>%
    dplyr::filter(!is.na(start))

  package$data$observations <- package$data$observations %>%
    dplyr::filter(!deploymentID %in% deps |
                    (timestamp>=startCut &
                       timestamp<=endCut &
                       deploymentID %in% deps))

  if("media" %in% names(package$data)){
    package$data$media <- package$data$media %>%
      dplyr::filter(!deploymentID %in% deps |
                      (timestamp>=startCut &
                         timestamp<=endCut &
                         deploymentID %in% deps))
  }
  package
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
#' @param depID A character value giving the deployment ID,
#'  to be matched in package$data$deployments$deploymentID.
#' @param locName A character value giving the location name,
#'  to be matched in package$data$deployments$locationName.
#' @param wrongTime A character or POSIX reference date-time recorded wrongly
#'  by the camera.
#' @param rightTime A character or POSIX value giving the correct date-time
#'  when the reference time was recorded.
#' @return As for \code{\link[camtraptor]{read_camtrap_dp}}, with all
#'  date-times corrected by the difference between rightTime and wrongTime.
#' @details One, but not both, of depID and locName must be provided, as single
#'  text values. If locName is provided, the deployment associated with this
#'  in pkg$data$deployments is corrected, but if locName is associated with more
#'  than one deployment the function does not run.
#' @examples
#' \dontrun{
#'   pkg <- read_camtrapDP("./datapackage/datapackage.json")
#' }
#' data(pkg)
#' pkg_corrected <- correct_time(pkg,
#'                               locName = "S01",
#'                               wrongTime = "2017-10-02 08:06:43",
#'                               rightTime = "2017-09-01 10:36:00")
#' @export
#'
correct_time <- function(package, depID=NULL, locName=NULL, wrongTime, rightTime){
  nullsum <- sum(is.null(depID), is.null(locName))
  if(nullsum!=1)
    stop("One but not both of depID and locName must be provided")

  if(!is.null(depID)){
    if(length(depID) > 1)
      stop("depID must be a single value")
    if(!depID %in% package$data$deployments$deploymentID)
      stop(paste("Can't find depID", depID, "in package$data$deployments$deploymentID"))
  }

  if(!is.null(locName)){
    if(length(locName) > 1)
      stop("locName must be a single value")
    if(!locName %in% package$data$deployments$locationName)
      stop(paste("Can't find locName", locName, "in package$data$deployments$locationName"))
    depID <- package$data$deployments %>%
      dplyr::filter(locationName == locName) %>%
      dplyr::select(deploymentID) %>%
      dplyr::pull()
    if(length(depID) > 1)
      stop(paste("There is more than one deployment associated with locName", locName))
  }

  td <- difftime(rightTime, wrongTime, tz="UTC")
  package$data$deployments <- package$data$deployments %>%
    dplyr::mutate(start = dplyr::if_else(deploymentID==depID,
                                         start + td,
                                         start),
                  end = dplyr::if_else(deploymentID==depID,
                                       end + td,
                                       end))
  package$data$observations <- package$data$observations %>%
    dplyr::mutate(timestamp = dplyr::if_else(deploymentID==depID,
                                             timestamp + td,
                                             timestamp))
  if("media" %in% names(package$data))
    package$data$media <- package$data$media %>%
      dplyr::mutate(timestamp = dplyr::if_else(deploymentID==depID,
                                               timestamp + td,
                                               timestamp))
  package
}

#' Select a species name
#'
#' Presents a table of species names with observation count for each
#' and allows the user to interactively select one.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species NULL (default) to select interactively, or character vector
#'   giving either one or more valid species names found within
#'   \code{package$data$observations$scientificName}, or the string "all"
#'   to return all available species names.
#' @return A character vector of one or more scientific species names.
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./data/datapackage.json")
#'     select_species(pkg)
#'     # If provided, a valid species name is simply passed through
#'     select_species(pkg, "Vulpes vulpes")
#'     # Providing a species name that isn't found in the data throws an error
#'     select_species(pkg, "Vulpes vuppes")
#'   }
#' @export
#'
select_species <- function(package, species=NULL){
  obs <- package$data$observations

  if(is.null(species)){
    if("useDeployment" %in% names(obs))
      obs[!obs$useDeployment, c("speed", "radius", "angle")] <- NA
    tab <- obs %>%
      dplyr::group_by(scientificName) %>%
      dplyr::summarise(n_sequences = sum(!duplicated(sequenceID)),
                       n_individuals = sum(count),
                       n_speeds = sum(speed>0.01 & speed<10 & !is.na(speed)),
                       n_radii = sum(!is.na(radius)),
                       n_angles = sum(!is.na(angle))) %>%
      dplyr::filter(!is.na(scientificName) & scientificName!="")

    print.data.frame(tab)
    i <- NA
    while(is.na(i) || i<1 || i>nrow(tab))
      i <- readline("Enter row number of species to select: ") %>%
      as.numeric() %>%
      suppressWarnings()
    species <- as.character(tab$scientificName[i])
    message(paste(species, "selected"))
  } else{
    if(length(species)==1 & "all" %in% species)
      species <- unique(na.omit(obs$scientificName)) else
        if(!all(species %in% obs$scientificName))
          stop("Can't find species in scientificName field of observations data")
  }
  species
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
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     pkg_checked <- check_deployment_models(pkg)
#'   }
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
    package$data$observations <- package$data$observations %>%
      dplyr::select(-useDeployment)
  package$data$observations <- package$data$observations %>%
    dplyr::left_join(depdat, by="deploymentID")

  package$data$deployments$useDeployment <- depdat$useDeployment
  package
}


#' Gets a set Agouti sequences URLs.
#'
#' Obtains web addresses for sequences of selected observations, based
#' on criteria defined using fields in the observations table.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param obsChoice A logical expression using column names from the
#'  observations table defining which observations you want to inspect.
#' @return A dataframe of Agouti URLs.
#' @examples
#'   data(pkg)
#'   get_agouti_url(pkg, speed>1)
#'
get_agouti_url <- function(package, obsChoice){
  seqIDs <- package$data$observations %>%
    dplyr::filter({{obsChoice}}) %>%
    dplyr::select(sequenceID) %>%
    dplyr::pull()
  address <- file.path("https://www.agouti.eu/#/project",
                       package$project$id,
                       "annotate/sequence",
                       seqIDs)
  data.frame(address = unique(address))
}

#' Estimate average animal speed
#'
#' Calculates harmonic mean and standard error of animal speed while active
#' from a data package.
#'
#' @param package Camera trap data package object, as returned by
#'   \code{\link[camtraptor]{read_camtrap_dp}}.
#' @param species A character string indicating species subset to analyse;
#'   if NULL runs select_species to get user input; if all uses all data.
#' @param formula A formula with speed on the left and covariates on the right.
#' @param newdata A data frame of covariate values at which to predict speeds.
#' @param reps Number of random draws to use for standard calculation.
#' @param distUnit A character string indicating distance unit of speed observations.
#' @param timeUnit A character string indicating time unit of speed observations.
#' @param ... Other parameters passed to \code{sbm} for covariate modelling (see details).
#' @return List with elements:
#' \itemize{
#'   \item{\code{speed}: a dataframe containing columns \code{estimate}
#'    (mean) and \code{se} (standard error) speed while active.}
#'   \item{\code{data}: a numeric vector of the data from which the estimate is derived.}
#'   }
#' @details If a formula is provided, the model is fitted using the \code{sbm}
#'  (size biased model) function, which can be installed using:
#'  \code{devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/sbd/master/sbd.r")}
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'   }
#'   data(pkg)
#'   \dontrun{
#'     # With interactive species definition
#'     speed_model <- fit_speedmodel(pkg)
#'   }
#'   # With species predefined
#'   speed_model <- fit_speedmodel(pkg, species="Vulpes vulpes")
#'   speed_model$estimate
#'   hist(speed_model)
#' @export
#'
fit_speedmodel <- function(package,
                           species = NULL,
                           formula = speed ~ 1,
                           newdata = NULL,
                           reps = 1000,
                           distUnit = c("m", "km", "cm"),
                           timeUnit = c("second", "minute", "hour", "day"),
                           ...){
  distUnit <- match.arg(distUnit)
  timeUnit <- match.arg(timeUnit)
  varnms <- all.vars(formula)
  species <- select_species(package, species)
  obs <- package$data$observations %>%
    dplyr::select(dplyr::all_of(c("scientificName", varnms))) %>%
    dplyr::filter(scientificName %in% !!species & speed>0.01 & speed<10) %>%
    tidyr::drop_na()
  if("useDeployment" %in% names(obs))
    obs <- dplyr::filter(obs, useDeployment==TRUE)

  if(nrow(obs) == 0) stop("There are no usable speed data")

  res <- sbd::sbm(formula, obs, ...)
  res$unit <- paste(distUnit, timeUnit, sep="/")
  res
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
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'   }
#'   data(pkg)
#'   \dontrun{
#'     # With interactive species definition
#'     act_model <- fit_actmodel(pkg)
#'   }
#'   # With species predefined, reps reduced for speed
#'   act_model <- fit_actmodel(pkg, species="Vulpes vulpes", reps=100)
#'   act_model@@act
#' @export
#'
fit_actmodel <- function(package,
                         species=NULL,
                         reps=999,
                         obsdef=c("individual", "sequence"),
                         ...){
  obsdef <- match.arg(obsdef)
  species <- select_species(package, species)
  deps <- package$data$deployments
  obs <- package$data$observations %>%
    dplyr::filter(scientificName %in% !!species) %>%
    dplyr::select(deploymentID, sequenceID, timestamp, count)
  i <- switch(obsdef,
              individual = rep(1:nrow(obs), obs$count),
              sequence = !duplicated(obs$sequenceID))
  obs <- obs[i, ]

  if(nrow(obs)>1){
    obs <- deps %>%
      dplyr::select(deploymentID, latitude, longitude) %>%
      dplyr::right_join(obs, by="deploymentID", multiple="all") %>%
      dplyr::select(-count)

    suntimes <- activity::get_suntimes(obs$timestamp, obs$latitude, obs$longitude, 0)
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
#'   if NULL runs select_species to get user input; if "all" all data are used.
#' @param newdata A dataframe of covariate values at which to predict detection
#'   distance.
#' @param unit The units in which to return the result.
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
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   \dontrun{
#'     # With interactive species definition
#'     radius_model <- fit_detmodel(radius~1, pkg, order=0)
#'   }
#'   # With species predefined
#'   sp <- "Vulpes vulpes"
#'   radius_model <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angle_model <- fit_detmodel(angle~1, pkg, species=sp, order=0, unit="degree")
#'   radius_model$edd
#'   angle_model$edd
#'   plot(radius_model, pdf=TRUE)
#'   plot(angle_model)
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
  dat <- package$data$observations
  if(!all(allvars %in% names(dat))) stop("Can't find all model variables in data")
  if("distance" %in% covars) stop("Cannot use \"distance\" as a covariate name - rename and try again")

  # set up data
  species <- select_species(package, species)
  if("useDeployment" %in% names(dat)) dat <- subset(dat, useDeployment)
  dat <- dat %>%
    dplyr::filter(scientificName %in% !!species) %>%
    dplyr::select(dplyr::all_of(allvars)) %>%
    tidyr::drop_na() %>%
    as.data.frame()
  if(nrow(dat) == 0) stop("There are no usable position data")

  classes <- dplyr::summarise_all(dat, class)
  if(classes[depvar]=="numeric"){
    dat <- dat %>%
      dplyr::rename(distance=dplyr::all_of(depvar)) %>%
      dplyr::mutate(distance=abs(distance))
  } else{
    cats <- strsplit(as.character(dplyr::pull(dat, depvar)), "-")
    dat$distbegin <- unlist(lapply(cats, function(x) as.numeric(x[1])))
    dat$distend <- unlist(lapply(cats, function(x) as.numeric(x[2])))
    dat$distance <- (dat$distbegin + dat$distend) / 2
  }

  # model fitting
  type <- if(unit %in% c("m", "km", "cm")) "point" else "line"
  args <- c(data=list(dat), formula=formula[-2], transect=type, list(...))
  mod <- suppressWarnings(suppressMessages(do.call(Distance::ds, args)$ddf))

  # esw prediction
  if(length(covars)==0)
    newdata <- data.frame(x=0) else{
      if(is.null(newdata)){
        newdata <- dat %>% dplyr::select(dplyr::all_of(covars)) %>%
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
  mod$proportion_used <- nrow(mod$data) / nrow(dat)
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
#'   data for; if NULL runs acode{select_species} to get user input.
#' @param unit The time unit in which to return camera effort.
#' @return A tibble with columns:
#' \itemize{
#'   \item{\code{locationName}: name of the camera location}
#'   \item{\code{effort}: the camera time for the location}
#'   \item{\code{unit}: the effort time unit}
#'   \item{\code{scientificName}: the scientific name of the species data extracted}
#'   \item{\code{n}: the observation counts}
#'   \item{\code{stratumID}: stratum identifier (only if this column is present in
#'     \code{package$data$deployments})}
#' }
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   \dontrun{
#'     # With interactive species definition
#'     trdata <- get_traprate_data(pkg)
#'   }
#'   # With species predefined
#'   trdata <- get_traprate_data(pkg, species="Vulpes vulpes")
#' @export
#'
get_traprate_data <- function(package, species=NULL,
                         unit=c("day", "hour", "minute", "second")){
  unit <- match.arg(unit)
  species <- select_species(package, species)
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
    dplyr::summarise(latitude = mean(latitude),
                     longitude = mean(longitude),
                     n = sum(n),
                     effort=sum(effort)) %>%
    dplyr::mutate(effort_unit = unit,
                  scientificName = paste(species, collapse="|"))

  if("stratumID" %in% names(dep)){
    str <- dep %>%
      dplyr::group_by(locationName) %>%
      dplyr::summarise(stratumID = unique(stratumID), .groups="keep")
    if(any(table(str$locationName) > 1))
      stop("Some locations appear in more than one stratum in the deployments data") else
        res <- dplyr::left_join(res, str, by="locationName")
  }
  res
}


#' Get average trap rate from REM data
#'
#' Calculates average trap rate and its bootstrapped error from a table of
#' per-location observation counts and camera time.
#'
#' @param traprate_data A dataframe containing (at least) columns \code{n} and
#'   \code{effort}, as returned by \code{\link{get_traprate_data}}; if
#'   \code{strata} supplied for stratified calculation, must also have column
#'   \code{stratumID}.
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
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   trdata <- get_traprate_data(pkg, species="Vulpes vulpes")
#'   get_trap_rate(trdata)
#' @export
#'
get_trap_rate <- function(traprate_data, strata=NULL, reps=999){

  traprate <- function(dat){
    if(is.null(strata)){
      sum(dat$n) / sum(dat$effort)
    } else{
      local_density <- sapply(strata$stratumID, function(stratum){
        i <- dat$stratumID==stratum
        sum(dat$n[i]) / sum(dat$effort[i])
      })
      sum(local_density * strata$area) / sum(strata$area)
    }
  }

  sampled_traprate <- function(){
    i <- if(is.null(strata))
      sample(1:nrow(traprate_data), replace=TRUE) else
        unlist(sapply(strata$stratumID, function(stratum){
          sample(which(traprate_data$stratumID==stratum), replace=TRUE)
        }))
    traprate(traprate_data[i, ])
  }

  if(!all(c("effort", "n") %in% names(traprate_data)))
    stop("traprate_data must contain (at least) columns effort and observations")
  if(!is.null(strata)){
    if(!"stratumID" %in% names(traprate_data))
      stop("traprate_data must contain column stratumID for stratified analysis")
    if(!all(c("stratumID", "area") %in% names(strata)))
      stop("strata must contain columns stratumID and area")
    if(!all(traprate_data$stratumID %in% strata$stratumID))
      stop("Not all strata in traprate_data are present in strata")
  }

  tr_sample <- replicate(reps, sampled_traprate())
  est <- traprate(traprate_data)
  se <- sd(tr_sample)
  cv <- se/est
  ci <- unname(quantile(tr_sample, c(0.025, 0.975)))
  data.frame(estimate = est,
             se = se,
             cv = cv,
             lcl95 = ci[1],
             ucl95 = ci[2],
             n = nrow(traprate_data),
             unit = paste("n", traprate_data$effort_unit[1], sep="/"),
             row.names = "trap_rate")
}

#' Log-normal confidence interval
#'
#' Calculates approximate log-normal confidence intervals given estimates
#' and their standard errors.
#'
#' @param estimate Numeric estimate value(s)
#' @param se Standard error(s) of the estimate
#' @param percent Percentage confidence level
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
#' @param traprate_data A dataframe of trap rate data (counts and effort), as
#'   returned by \code{\link{get_traprate_data}}.
#' @param radius_model A detection radius model fitted using \code{\link{fit_detmodel}}
#' @param angle_model A detection angle model fitted using \code{\link{fit_detmodel}}
#' @param speed_model A speed model fitted using \code{\link{fit_speedmodel}}
#' @param activity_model An activity  model fitted using \code{\link{fit_actmodel}}
#' @param strata A dataframe of stratum information passed to \code{\link{get_trap_rate}}
#' @param reps Number of bootstrap replicates for estimating trap rate error
#'   (see \code{\link{get_trap_rate}})
#' @return A dataframe of unit-harmonised parameter estimates with rows:
#' \itemize{
#'   \item{\code{radius}: detection radius}
#'   \item{\code{angle}: detection angle}
#'   \item{\code{active_speed}: speed while active}
#'   \item{\code{activity_level}: proportion of time spent active}
#'   \item{\code{overall_speed}: long-term average speed (day range) - the product
#'     of \code{active_speed} and \code{activity_level}}
#'   \item{\code{trap_rate}: number of camera trap records per unit time}
#'  }
#'  and columns
#' \itemize{
#'   \item{\code{estimate}: parameter estimates}
#'   \item{\code{se}: standard error}
#'   \item{\code{cv}: proportional coefficient of variation}
#'   \item{\code{lcl95}, \code{ucl95}: lower and upper 95\% confidence limits}
#'   \item{\code{n}: sample size}
#'   \item{\code{unit}: the unit of the estimate}
#' }
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   sp <- "Vulpes vulpes"
#'   trdata <- get_traprate_data(pkg, species=sp)
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   get_parameter_table(trdata, radmod, angmod, spdmod, actmod)
#' @export
#'
get_parameter_table <- function(traprate_data,
                                radius_model,
                                angle_model,
                                speed_model,
                                activity_model,
                                strata = NULL,
                                reps = 999){

  # Get parameters and SEs
  rad <- radius_model$edd
  ang <- angle_model$edd * 2
  spd <- dplyr::select(speed_model$estimate, all_of(c("est", "se")))
  act <- activity_model@act[1:2]
  names(act) <- names(spd) <- dimnames(rad)[[2]]
  res <- data.frame(rbind(rad, ang, spd, act))

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
             nrow(speed_model$data),
             length(activity_model@data),
             NA)
  res$unit <- c(radius_model$unit,
                angle_model$unit,
                speed_model$unit,
                "none",
                speed_model$unit)
  rownames(res) <- c("radius",
                     "angle",
                     "active_speed",
                     "activity_level",
                     "overall_speed")

  # Add trap rate, including correction for any truncation of radius model
  traprate <- get_trap_rate(traprate_data, strata, reps)
  j <- c("estimate", "se", "lcl95", "ucl95")
  traprate[, j] <- traprate[, j] * radius_model$proportion_used
  res <- rbind(res, traprate)

  camtrapDensity::convert_units(res)
}

#' Get a unit multiplier
#'
#' Returns a multiplier to convert values from one unit to another, in
#' one of four categories: distance, time, angle, count.
#'
#' @param unitIN A character vector giving the units of input.
#' @param unitOUT A character vector giving the units of output,
#'   the same length as \code{unitIN},
#' @return A vector of numbers giving the amount by which to multiply input
#'   values to arrive at unit-converted values.
#' @details
#'  Possible \code{unitIN} and \code{unitOUT} values are "cm", "m", "km"
#'  for distances; "second", "minute", "hour", "day", "100day" for times;
#'  "radian", "degree" for angles; "n" for count; "none" for no units. Unit
#'  ratios are allowed for rates or densities. In this case, units should be
#'  separated with a forward slash (e.g. "n/day", "km/hour", "n/km2"). Input
#'  and output types must match.
#'
#' @examples
#'   get_multiplier(c("m", "m/second"), c("km", "km/day"))
#' @export
#'
get_multiplier <- function(unitIN, unitOUT){
  # lookup table for types and multipliers
  lookup <- data.frame(unit = c("cm", "m", "km",
                                "second", "minute", "hour", "day", "100day",
                                "radian", "degree",
                                "ha", "km2", "100km2",
                                "n", "none"),
                       mult = c(1, 1e2, 1e5,
                                1, 60, 60^2, 24*60^2, 2400*60^2,
                                1, pi/180,
                                1, 1e2, 1e4,
                                1, 1),
                       type = rep(c("distance", "time", "angle", "area", "unit"),
                                  c(3, 5, 2, 3, 2)))

  # recasts all input to x/y (x per y) format, adding "/n" where no denominator given
  recast <- function(u){
    unlist(strsplit(paste0(u, ifelse(grepl("/", u), "", "/n")), "/"))
  }

  if(length(unitIN) != length(unitOUT))
    stop("unitIN and unitOUT have different lengths")
  if(!all(grepl("/", unitIN) == grepl("/", unitOUT)))
    stop("unitIN and unitOUT have mismatched types")

  uIN <- recast(unitIN)
  uOUT <- recast(unitOUT)

  typeIN <- lookup$type[match(unlist(uIN), lookup$unit)]
  typeOUT <- lookup$type[match(unlist(uOUT), lookup$unit)]

  if(any(is.na(c(typeIN, typeOUT))))
    stop(paste("Units not recognised:",
               paste(c(uIN,uOUT)[is.na(c(typeIN, typeOUT))], collapse=", ")))
  if(!all(typeIN == typeOUT))
    stop("unitIN and unitOUT have mismatched types")

  m <- lookup$mult[match(uIN, lookup$unit)] / lookup$mult[match(uOUT, lookup$unit)]
  i <- 2 * (1:length(unitIN))
  m[i-1] / m[i]
}

#' Change the units of an REM parameter table
#'
#' Changes the units of parameters from their current setting to new
#' user-defined units.
#'
#' @param param An REM parameter dataframe (see details).
#' @param radius_unit A character string giving the output unit of radius.
#' @param angle_unit A character string giving the output unit of angle.
#' @param active_speed_unit A character string giving the output unit of
#'   speed while active.
#' @param overall_speed_unit A character string giving the output unit of
#'   day range.
#' @param trap_rate_unit A character string giving the output unit of trap rate.
#' @param density_unit A character string giving the output unit of density.
#' @return A replica of input dataframe \code{param} with \code{estimate},
#'   \code{se} and confidence limit values converted to output units.
#' @details
#'  Input dataframe param must contain field \code{unit}, and at least one
#'  field among \code{estimate}, \code{se}, \code{lcl95}, and \code{ucl95}.
#'  Row names must be among \code{radius}, \code{angle}, \code{activity_level},
#'  \code{active_speed}, \code{overall_speed}. Input is typically created with
#'  function \code{\link{get_parameter_table}}.
#'
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   sp <- "Vulpes vulpes"
#'   trdata <- get_traprate_data(pkg, species=sp)
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   param <- get_parameter_table(trdata, radmod, angmod, spdmod, actmod)
#'   convert_units(param, radius_unit="m", angle_unit="degree", active_speed_unit="m/second")
#' @export
convert_units <- function(param,
                          radius_unit=c("km", "m", "cm"),
                          angle_unit=c("radian", "degree"),
                          active_speed_unit=c("km/day", "km/hour", "m/hour", "m/second"),
                          overall_speed_unit=c("km/day", "km/hour", "m/hour", "m/second"),
                          trap_rate_unit=c("n/day", "n/100day", "n/hour", "n/minute", "n/second"),
                          density_unit=c("n/km2", "n/ha", "n/100km2")){
  uOUT <- c(radius = match.arg(radius_unit),
            angle = match.arg(angle_unit),
            activity_level = "none",
            active_speed = match.arg(active_speed_unit),
            overall_speed = match.arg(overall_speed_unit),
            trap_rate = match.arg(trap_rate_unit),
            density = match.arg(density_unit))

  convert_fields <- c("estimate", "se", "lcl95", "ucl95")
  j <- names(param) %in% convert_fields
  if(sum(j) == 0 | !"unit" %in% names(param))
    stop(paste("Fields in param dataframe must include unit and at least one of:\n",
               paste(head(convert_fields, -1), collapse=", ")))
  if(!all(rownames(param) %in% names(uOUT)))
    stop(paste("Row names in param dataframe must be among:\n",
               paste(names(uOUT), collapse=", ")))

  uOUT <- uOUT[rownames(param)]
  m <- get_multiplier(param$unit, uOUT)
  param[, j] <- m * param[, j]
  param$unit <- uOUT

  param
}

#' Fit a random encounter model
#'
#' Estimates REM density given a dataframe of parameters and their errors.
#'
#' @param parameters A dataframe containing REM parameter estimates with
#' (at least) rows:
#' \itemize{
#'   \item{\code{radius}: effective detection radius}
#'   \item{\code{angle}: effective detection angle}
#'   \item{\code{overall_speed}: average animal speed (day range)}
#'   \item{\code{trap_rate}: animal observations per unit time}
#' }
#' and columns:
#' \itemize{
#'   \item{\code{estimate}: numeric parameter estimate}
#'   \item{\code{se}: numeric parameter standard error}
#'   \item{\code{unit}: character parameter units (see \code{\link{convert_units}}
#'     for allowable values)}
#' }
#' @return A dataframe with the input parameters plus estimated density and
#'  its errors.
#' @examples
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   sp <- "Vulpes vulpes"
#'   trdata <- get_traprate_data(pkg, species=sp)
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, order=0)
#'   angmod <- fit_detmodel(angle~1, pkg, species=sp, unit="radian", order=0)
#'   spdmod <- fit_speedmodel(pkg, species=sp)
#'   actmod <- fit_actmodel(pkg, species=sp, reps=100)
#'   param <- get_parameter_table(trdata, radmod, angmod, spdmod, actmod)
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

  param <- convert_units(parameters[required_rows, ])
  wtd_est <- param$estimate + c(0,0,0,2)
  pwr_est <- wtd_est ^ c(1,-1,-1,-1)
  CVs <- param$se / wtd_est
  density <- pi * prod(pwr_est)
  cv <- sqrt(sum(CVs^2))
  se <- density * cv
  ci <- unname(lnorm_confint(density, se))

  parameters["density", "estimate"] <- density
  parameters["density", "se"] <- se
  if("cv" %in% names(parameters))
    parameters["density", "cv"] <- cv
  if("lcl95" %in% names(parameters))
    parameters["density", "lcl95"] <- ci[1]
  if("ucl95" %in% names(parameters))
    parameters["density", "ucl95"] <- ci[2]
  parameters["density", "unit"] <- "n/km2"

  parameters
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
#' @param speed_model A speed model fitted using \code{\link{fit_speedmodel}}.
#' @param activity_model An activity model fitted using
#'   \code{\link[activity]{fitact}} or \code{\link{fit_actmodel}}.
#' @param strata A dataframe of stratum areas, passed to \code{\link{get_trap_rate}}.
#' @param reps Number of bootstrap replicates for error estimation.
#' @return A dataframe containing estimates and their errors for density and
#'   all contributing parameters.
#' @examples
#'   # Load data
#'   \dontrun{
#'     pkg <- read_camtrapDP("./datapackage/datapackage.json")
#'     }
#'   data(pkg)
#'   # Sense check deployment schedules
#'   plot_deployment_schedule(pkg)
#'  \dontrun{
#'    # Sense check deployment calibration model diagnostic plots
#'    pkg_checked <- check_deployment_models(pkg)
#'    # Fully automated analysis (interactive species definition by default;
#'    # reps reduced to limit run time).
#'    res <- rem_estimate(pkg_checked, check_deployments=FALSE, reps=100)
#'   }
#'   # Automated analysis with species predefined and no deployment checking
#'   sp <- "Vulpes vulpes"
#'   res <- rem_estimate(pkg, species=sp, check_deployments=FALSE, reps=100)
#'   # Inspect results
#'   res$estimates
#'
#'   # Analysis with radius model fitted separately
#'   radmod <- fit_detmodel(radius~1, pkg, species=sp, truncation=15, order=0)
#'   res <- rem_estimate(pkg, check_deployments = FALSE, species = sp,
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
  species <- select_species(package, species)
  message(paste("Analysing", species))

  message("Fitting radius model...")
  if(is.null(radius_model))
    radius_model <- fit_detmodel(radius~1, package, species,
                                 order=0, truncation="5%")

  message("Fitting angle model...")
  if(is.null(angle_model))
    angle_model <- fit_detmodel(angle~1, package, species,
                                order=0, unit="radian")

  message("Fitting speed model...")
  if(is.null(speed_model))
    speed_model <- fit_speedmodel(package, species)

  message("Fitting activity model...")
  if(is.null(activity_model))
    activity_model <- fit_actmodel(package, species, reps)

  message("Calculating density...")
  trdat <- get_traprate_data(package, species)
  parameters <- get_parameter_table(trdat,
                                    radius_model,
                                    angle_model,
                                    speed_model,
                                    activity_model,
                                    strata,
                                    reps)

  estimates <- rem(parameters) %>%
    convert_units(radius_unit = "m",
                  angle_unit = "degree",
                  active_speed_unit = "km/hour",
                  overall_speed_unit = "km/day")

  message("DONE")
  list(project = package$project$title, datapackage = package$name,
       samplingDesign = package$project$samplingDesign,
       start = package$temporal$start, end = package$temporal$end,
       species=species, data=trdat, estimates=estimates,
       speed_model=speed_model, activity_model=activity_model,
       radius_model=radius_model, angle_model=angle_model)
}


#' Write REM results to csv file
#'
#' Writes one or more REM estimate tables to a single csv file, with
#' identifying columns added for project, datapackage, sampling design,
#' sampling effort, project location, project dates and species. Input
#' must be REM analysis object(s) created using \code{\link{rem_estimate}}.
#' The resulting file name is taken from the project and current date, and
#' the file is saved to the working directory.
#'
#' @param ... One or more REM analysis objects, separated by commas.
#' @return None - creates a csv file.
#' @examples
#'  \dontrun{
#'    foxREM <- rem_estimate(pkg_checked, check_deployments=FALSE, species="Vulpes vulpes")
#'    hhogREM <- rem_estimate(pkg_checked, check_deployments=FALSE, species="Erinaceus europaeus")
#'    write_rem_csv(foxREM, hhogREM)
#'    }
#' @export
#'
write_rem_csv <- function(...){
  remlist <- list(...)
  classes <- unlist(lapply(remlist, function(x)
    class(x)[1]))
  rqd <- c("project", "datapackage", "samplingDesign",
           "start", "end", "species", "estimates", "data")
  got_rqd <- unlist(
    lapply(remlist, function(x)
      all(rqd %in% names(x)))
  )

  if(!all(got_rqd) | !all(classes == "list"))
    stop("All objects passed to write_rem_csv must be rem analysis objects created using camtrapDensity::rem_estimate Vx.x or greater")

  get_table <- function(rem){
    rem_est <- tibble::rownames_to_column(rem$estimates, "parameter")
    cbind(projectName = rem$project,
          datapackage = rem$datapackage,
          samplingDesign = rem$samplingDesign,
          minLatitude = min(rem$data$latitude),
          maxLatitude = max(rem$data$latitude),
          minLongitude = min(rem$data$longitude),
          maxLongitude = max(rem$data$longitude),
          start = rem$start,
          end = rem$end,
          effort = sum(rem$data$effort),
          effortUnit = rem$data$effort_unit[1],
          species=rem$species,
          nObservations = sum(rem$data$n),
          rem_est)
  }
  est <- lapply(remlist, get_table) %>%
    dplyr::bind_rows()
  dt <- substr(gsub("\\D", "", Sys.time()), 1, 14)
  file <- unique(est$project) %>%
    paste(collapse="_")
  file <- gsub(" ", "-", file)
  file <- paste0(file, "_", dt, ".csv")
  write.csv(est, file, row.names = FALSE)
  print(paste("Data written to", normalizePath(file, "/")))
}
