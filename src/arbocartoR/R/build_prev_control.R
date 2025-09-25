#' Generate the list of preventive control to implement in the simulation
#'
#' @description Function used to generate the preventive control data.frame.
#'
#' Note that the distance unit of the buffer width parameter is meters if the CRS is (+proj=longlat), and in map units (typically also meters) if not.
#'
#' @param action string. 'K': Source reduction (removal or destruction of breeding sites); 'L': Chemical Larviciding; 'A': Fogging or Area Spraying (targets adult mosquitoes)
#' @param lon string. longitude of the control location
#' @param lat string. latitude of the control location
#' @param start date in '\%Y-\%m-\%d' format. Define the beginning of the implemented measure
#' @param end date in '\%Y-\%m-\%d' format. Define the end of the implemented measure
#' @param p number between 0 and 1. It is for the "K" action: the proportion of sites daily removed during the action ; for the "A" action: the additional daily mortality of adults due to action and for the "L" action: the additional daily mortality of larvae due to larvicide
#' @param SpatVec SpatVector or sf. (Required with buffer) Spatial vector of polygons representing patches. Polygons must include 'ID' attribute including the 'loc'.
#' @param prev_control data.frame. (optional) prev_control object to build on (add new measures)
#' @param buffer_width integer. (optional) Buffer around the location to implement the measure
#' @param plot_buffer logical. Display plot of the selected parcels in case of spatial buffer.
#'
#' @return events data.frame with preventive control measures
#'
#' @importFrom terra relate buffer vect is.related crs
#' @importFrom magrittr divide_by equals
#' @importFrom data.table rbindlist
#' @keywords events
#'
#' @examples
#'
#' f <- system.file("shape/SpatVec.shp", package = "arbocartoR")
#' SpatVec <- terra::vect(f)
#' build_prev_control(action = "K",
#'       lon = 1022397, lat = 6321347,
#'       start = "2022-01-01", end = "2022-01-05",
#'       p = 0.2,
#'       SpatVec = SpatVec,
#'       buffer_width = 100)
#'
#' @export

build_prev_control <- function(action,
                               lon,
                               lat,
                               start,
                               end,
                               p,
                               SpatVec,
                               prev_control = NULL,
                               buffer_width = NULL,
                               plot_buffer = F){

  if(is.null(prev_control))
  prev_control <- data.table(
    action = character(),
    loc = factor(),
    start = structure(numeric(0), class = "Date"),
    end = structure(numeric(0), class = "Date"),
    p = numeric()
  )
  #  fix me check the column of prev_control

  # CHECK: is the point into the study area
    if(is.related(SpatVec, vect(data.frame(lon = lon, lat = lat),
                                geom=c("lon", "lat"),
                                crs=crs(SpatVec)), "intersects") %>% sum %>% equals(0))
      stop("The point and buffer zone do not overlap with the study area.")

  if(!is.null(buffer_width)){
    buffer_ids <- relate(SpatVec,
                         buffer(vect(data.frame(lon = lon, lat = lat),
                                     geom=c("lon", "lat"),
                                     crs=crs(SpatVec)),
                                buffer_width),
                         "intersects", pairs=FALSE, na.rm=TRUE) %>% c %>% SpatVec[., ]
    if(plot_buffer)
      plot(buffer_ids)

    # For each intersected parcel, calculate the proportion of covered area

    p_covered_area <- sapply(seq(nrow(buffer_ids)), function(i)
     crop(buffer_ids[i,],
         buffer(vect(data.frame(lon = lon, lat = lat),
                     geom=c("lon", "lat"),
                     crs=crs(SpatVec)),
                buffer_width),
                ext=T) %>% expanse %>% divide_by(expanse(buffer_ids)[i]))

    loc <- buffer_ids$ID

    prev_control %<>% list(., data.table(
      action = action,
      loc = loc,
      start = as.Date(start),
      end = as.Date(end),
      p = p * p_covered_area)) %>% rbindlist(fill = TRUE)%>% unique

      } else
  prev_control %<>% list(., data.table(
    action = action,
    loc = relate(SpatVec,
                 vect(data.frame(lon = lon, lat = lat),
                      geom=c("lon", "lat"),
                      crs=crs(SpatVec)),
                 "intersects", pairs=FALSE, na.rm=TRUE) %>% c %>% SpatVec[.,] %>% .$ID,
    start = as.Date(start),
    end = as.Date(end),
    p = p)) %>% rbindlist(fill = TRUE)%>% unique

  return(prev_control)

}
