#' Estimate spatial carrying capacities
#'
#' @description Function used to estimate carrying capacity from soil occupation classes
#'
#' @param PARCELS_SHAPE SpatVector object with polygons of parcels
#' @param URBAN_ATLAS SpatVector object or list of SpatVector objects with polygons of soil occupation class by urban atlas
#'
#' @return SpatVector object
#'
#' @importFrom terra project expanse erase crop mask activeCat levels addCats zonal rast relate aggregate
#' @importFrom magrittr multiply_by is_in not
#'
#' @keywords carrying capacity
#'
#'
#' @examples
#' \dontrun{
#' PARCELS_SHAPE <- system.file("shape/SpatVec.shp", package = "arbocartoR") %>% vect
#' PARCELS_SHAPE_K_ESTIM <- estim_K(PARCELS_SHAPE, URBAN_ATLAS)
#' }
#'
#' @export

estim_K <- function(PARCELS_SHAPE,
                    URBAN_ATLAS = NULL){

  names_SHAPE <- names(PARCELS_SHAPE)

  if(!is.null(URBAN_ATLAS)){

    if(inherits(URBAN_ATLAS, 'SpatVector')){
      URBAN_ATLAS_list <- list(URBAN_ATLAS)
    } else URBAN_ATLAS_list <- URBAN_ATLAS

    for(i in seq_along(URBAN_ATLAS_list)){

      URBAN_ATLAS <- URBAN_ATLAS_list[[i]]

    ## HARMONIZE PROJECTIONS ##
    URBAN_ATLAS %<>% project(PARCELS_SHAPE)

    if(intersect(PARCELS_SHAPE, URBAN_ATLAS) %>% length() %>% magrittr::equals(0))
      next
      # stop("URBAN_ATLAS and PARCELS_SHAPE do not intersect, consider using another urban atlas shapefile or URBAN_ATLAS = FALSE")

    ## CROP URBAN ATLAS ##
    URBAN_ATLAS %<>% crop(., PARCELS_SHAPE, ext = 2)
    gc()

    ## CHECK IF PARCELS ARE FULLY COVERED BY URBAN ATLAS ##
    PARCELS_ARE_COVERED <- URBAN_ATLAS %>% aggregate %>% relate(PARCELS_SHAPE, ., relation ="coveredby") %>% is_in(FALSE, .) %>% not
    gc()

    ## CREATE A MASK ##
    URBAN_ATLAS %<>% mask(., PARCELS_SHAPE)
    gc()

    # SELECT COLUMNS
    URBAN_ATLAS %<>% .[, "code_2018"]
    gc()

    # BIND URBAN ATLAS WITH CC TABLE
    URBAN_ATLAS %<>% cbind(., configK[match(.$code_2018, CODE), ])
    gc()

    # ADD POLYGONS SURFACE
    URBAN_ATLAS$AREA <- expanse(URBAN_ATLAS, "ha")
    gc()

    # CALCULATE Kfix
    URBAN_ATLAS$Kfix <- URBAN_ATLAS$nbL_ha * URBAN_ATLAS$AREA * URBAN_ATLAS$p_gite_anthro
    # CALCULATE Kvar
    URBAN_ATLAS$Kvar <- URBAN_ATLAS$nbL_ha * URBAN_ATLAS$AREA * (1 - URBAN_ATLAS$p_gite_anthro)

    PARCELS_SHAPE %<>% project(URBAN_ATLAS)
    gc()

    if('Kfix' %in% names(PARCELS_SHAPE)  & 'Kvar' %in% names(PARCELS_SHAPE)){
    zonal_UA <- zonal(URBAN_ATLAS[,c("Kfix", "Kvar")], PARCELS_SHAPE[, -which(names(PARCELS_SHAPE) %in% c('Kfix', 'Kvar'))], fun = sum, na.rm=TRUE, as.polygons = T)
    gc()

    PARCELS_SHAPE$Kfix <- rowSums(data.frame(PARCELS_SHAPE$Kfix, zonal_UA$Kfix), na.rm = TRUE) * NA ^ (rowSums(!is.na(data.frame(PARCELS_SHAPE$Kfix, zonal_UA$Kfix))) == 0)
    PARCELS_SHAPE$Kvar <- rowSums(data.frame(PARCELS_SHAPE$Kvar, zonal_UA$Kvar), na.rm = TRUE) * NA ^ (rowSums(!is.na(data.frame(PARCELS_SHAPE$Kvar, zonal_UA$Kvar))) == 0)

    } else {
    PARCELS_SHAPE <- zonal(URBAN_ATLAS[,c("Kfix", "Kvar")], PARCELS_SHAPE,
                           fun = sum, na.rm=TRUE, as.polygons = T)
    }

    ## IF ALL PARCELS ARE NOT COVERED BY UA
    if(!PARCELS_ARE_COVERED){
      # EXTRACT AREAS NOT COVERED BY UA
      PARCELS_SHAPE_NO_UA <- PARCELS_SHAPE[is.na(PARCELS_SHAPE$Kfix),]#erase(PARCELS_SHAPE, URBAN_ATLAS)
      gc()
    } else PARCELS_SHAPE_NO_UA <- NULL
  }
    } else {
    PARCELS_SHAPE_NO_UA <- PARCELS_SHAPE
  }

  if(!is.null(PARCELS_SHAPE_NO_UA)){
    ## LOAD CLC
    CLC_raster <- system.file("clc/U2018_CLC2018_V2020_20u1.tif", package = "arbocartoR") %>% rast(., "CODE_18")

    # PROJECT
    PARCELS_SHAPE_NO_UA %<>% project(CLC_raster)

    # CROP & MASK
    CLC_raster %<>% crop(., PARCELS_SHAPE_NO_UA)
    CLC_raster %<>% mask(., PARCELS_SHAPE_NO_UA)

    suppressWarnings(CLC_raster %<>% project(PARCELS_SHAPE))

    # cats(CLC_raster)

    # SELECT THE CODE CATEGORY
    terra::activeCat(CLC_raster, layer=1) <- "CODE_18"

    # MERGE WITH THE CONFIGURATION TABLE
    table_raster_lev <- terra::levels(CLC_raster)[[1]]
    table_raster_lev$CODE_18 %<>% as.numeric %>% multiply_by(100)

    ## LOAD CARRYING CAPACITY X SOIL OCCUPATION TABLE
    # data("configCC")

    CLC_cats <- merge(table_raster_lev, configK, by.x = "CODE_18", by.y = "CODE", all.x = T) %>%
      replace(is.na(.), 0)

    CLC_cats %<>% .[, c(which(colnames(.) %in% c("Value", "CODE_18")),
                        which(!colnames(.) %in% c("Value", "CODE_18")))]

    CLC_raster %<>% addCats(.,
                            CLC_cats,
                            merge=F, layer=1)
    # cats(CLC_raster)

    terra::activeCat(CLC_raster, layer=1) <- "Klfix"
    PARCELS_SHAPE$CLC_Klfix <- zonal(CLC_raster, PARCELS_SHAPE, fun = sum, na.rm=TRUE)

    terra::activeCat(CLC_raster, layer=1) <- "Klvar"
    PARCELS_SHAPE$CLC_Klvar <- zonal(CLC_raster, PARCELS_SHAPE, fun = sum, na.rm=TRUE)

    if(!is.null(URBAN_ATLAS)){
      PARCELS_SHAPE$Kfix <- rowSums(cbind(PARCELS_SHAPE$CLC_Klfix,PARCELS_SHAPE$Kfix), na.rm=TRUE)
      PARCELS_SHAPE$Kvar <- rowSums(cbind(PARCELS_SHAPE$CLC_Klvar,PARCELS_SHAPE$Kvar), na.rm=TRUE)
    } else {
      PARCELS_SHAPE$Kfix <- PARCELS_SHAPE$CLC_Klfix
      PARCELS_SHAPE$Kvar <- PARCELS_SHAPE$CLC_Klvar
    }

  }

  PARCELS_SHAPE %<>% .[c(names_SHAPE, "Kfix", "Kvar")]

  return(PARCELS_SHAPE)
}
