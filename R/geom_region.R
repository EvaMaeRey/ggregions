st_crs_mod <- function(ref_data){

  crs <- sf::st_crs(ref_data)
  
  if(is.na(crs)){NULL}else{crs}

}


compute_panel_regions <- function (data, scales, ref_data, keep = NULL, drop = NULL, stamp = F) {
  
    ref_data$id <- ref_data[1][[1]]
    
    if (!is.null(keep)) {
        ref_data <- dplyr::filter(ref_data, id %in% keep)
    }
    
    if (!is.null(drop)) {
        ref_data <- dplyr::filter(ref_data, !(id %in% drop))
    }
    
    if (!stamp) {
    
          ref_data_long <- ref_data |> 
            mutate(across(-geometry, as.character)) |>
            pivot_longer(cols = -c(geometry, id), 
                         names_to = ".id_type", 
                         values_to = "region")

          # check unique in the future
          length(ref_data_long$region) == length(unique(ref_data_long$region))
    
          ref_data_long <- ref_data_long |> 
            ggplot2::StatSf$compute_panel(coord = ggplot2::CoordSf) |>
            ggplot2::StatSfCoordinates$compute_group(coord = ggplot2::CoordSf)
        
          out <- dplyr::inner_join(ref_data_long, data, by = join_by(region)) 
          
          ref_data |> sf::st_drop_geometry() |>
            inner_join(out, by = join_by(id))
          
    }
    
    else {
      
        ref_data |> 
            ggplot2::StatSf$compute_panel(coord = ggplot2::CoordSf) |>
            ggplot2::StatSfCoordinates$compute_group(coord = ggplot2::CoordSf)
      
    }
    
}


StatRegion <- ggplot2::ggproto("StatRegion", ggplot2::Stat,
                      compute_panel = compute_panel_regions,
                      required_aes = "region",
                      default_aes = ggplot2::aes(label = ggplot2::after_stat(id),
                                                 geometry = geometry))

StatRegionStamp <- ggplot2::ggproto("StatRegionStamp", ggplot2::Stat,
                      compute_panel = compute_panel_regions,
                      default_aes = ggplot2::aes(label = ggplot2::after_stat(id),
                                                 geometry = geometry))

#' @export
geom_region <- function (mapping = aes(), data = NULL, 
                         stat = StatRegion,
                         position = "identity", 
                         na.rm = FALSE, show.legend = NA, 
                         inherit.aes = TRUE, ref_data = getOption("ggregions.ref.regions", ref_data_us), ...){
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}


#' @export
stamp_region <- function (mapping = aes(), stat = StatRegionStamp, 
                          position = "identity", 
                          na.rm = FALSE, show.legend = NA, 
                          inherit.aes = FALSE, ref_data = getOption("ggregions.ref.regions", ref_data_us), ...) 
{
    c(layer_sf(geom = GeomSf, data = ref_data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, 
                                                         ref_data = ref_data, stamp = T,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}


GeomSfBorder <- ggplot2::ggproto("GeomSfBorder", ggplot2::GeomSf, 
                        default_aes = ggplot2::GeomSf$default_aes |>
                          modifyList(
                            ggplot2::aes(fill = "transparent", 
                                         linewidth = ggplot2::from_theme(linewidth*1.5)),
                            keep.null = T)) 

#' @export
geom_region_border <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data = getOption("ggregions.ref.regions", ref_data_us), ...) 
{
    c(layer_sf(geom =  GeomSfBorder, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, fill = "transparent",
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}


#' @export
stamp_region_border <- function (mapping = aes(), data = ref_data, 
                          stat = StatRegionStamp, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data = getOption("ggregions.ref.regions", ref_data_us), stamp = TRUE, ...) 
{
    c(layer_sf(geom = GeomSfBorder, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = stamp, fill = "transparent",
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}

#' @export
geom_region_text <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data = getOption("ggregions.ref.regions", ref_data_us), ...) 
{
    c(layer_sf(geom = GeomText, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}


#' @export
stamp_region_text <- function (mapping = aes(), data = ref_data, 
                          stat = StatRegionStamp, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data = getOption("ggregions.ref.regions", ref_data_us), stamp = TRUE, ...) 
{
    c(layer_sf(geom = GeomText, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = stamp,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}

#' @export
geom_region_label <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data = getOption("ggregions.ref.regions", ref_data_us), ...) 
{
    c(layer_sf(geom = GeomLabel, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}

#' @export
stamp_region_label <- function (mapping = aes(), data = ref_data, 
                          stat = StatRegionStamp, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data = getOption("ggregions.ref.regions", ref_data_us), stamp = TRUE, ...) 
{
    c(layer_sf(geom = GeomLabel, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = stamp,
            ...)), coord_sf(crs = st_crs_mod(ref_data))
      )
}

