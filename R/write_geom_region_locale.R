geom_region <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data, ...) 
{
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, 
            ...)), 
    coord_sf(crs = sf::st_crs(ref_data)))
}


#' @export
write_geom_region_locale <- function(ref_data = australia_state_ref){

  modified_fun <- geom_region

  formals(modified_fun)$ref_data <- substitute(ref_data)

  return(modified_fun)

}


# all the arguments should be passed
stamp_region <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data, ...) 
{
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = T,
            ...)), 
    coord_sf(crs = sf::st_crs(ref_data)))
}



#' @export
write_stamp_region_locale <- function(ref_data = australia_state_ref){

  modified_function <- stamp_region

formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}


# all the arguments should be passed
geom_region_text <- function (mapping = aes(), data = NULL, stat = StatRegion,
                              position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data, ...) 
{
    c(layer_sf(geom = GeomText, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, 
            ...)), 
    coord_sf(crs = sf::st_crs(ref_data)))
}



#' @export
write_geom_region_text_locale <- function(ref_data){

  modified_function <- geom_region_text

formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}



# all the arguments should be passed
# all the arguments should be passed
stamp_region_text <- function (mapping = aes(), data = NULL, stat = StatRegion,
                              position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data, ...) 
{
    c(layer_sf(geom = GeomText, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = T,
            ...)), 
    coord_sf(crs = sf::st_crs(ref_data)))
}


#' @export
write_stamp_region_text_locale <- function(ref_data){

  modified_function <- stamp_region_text

formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}
