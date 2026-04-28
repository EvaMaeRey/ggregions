# when the crs is NULL, st_crs unfortunately returns, na, but we would like it to return NULL
st_crs_mod <- function(ref_data){

  crs <- sf::st_crs(ref_data)
  
  if(is.na(crs)){NULL}else{crs}

}


geom_region <- function (mapping = aes(), data = NULL, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ref_data, ...) 
{
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, 
            ...)), 
    coord_sf(crs = st_crs_mod(ref_data)))
}


#' @export
write_geom_region <- function(ref_data = australia_state_ref, required_aes = NULL){

  modified_fun <- geom_region

  formals(modified_fun)$ref_data <- substitute(ref_data)

  return(modified_fun)

}


# all the arguments should be passed
stamp_region <- function (mapping = aes(), data = ref_data, stat = StatRegion, position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data, ...) 
{
    c(layer_sf(geom = GeomSf, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = T,
            ...)), 
    coord_sf(crs = st_crs_mod(ref_data)))
}



#' @export
write_stamp_region <- function(ref_data = australia_state_ref, required_aes = NULL){

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
    coord_sf(crs = st_crs_mod(ref_data)))
}



#' @export
write_geom_region_text <- function(ref_data, required_aes = NULL){

  modified_function <- geom_region_text

  formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}



# all the arguments should be passed
# all the arguments should be passed
stamp_region_text <- function (mapping = aes(), data = ref_data, stat = StatRegion,
                              position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = FALSE, ref_data, ...) 
{
    c(layer_sf(geom = GeomText, data = data, mapping = mapping, 
        stat = stat, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = rlang::list2(na.rm = na.rm, ref_data = ref_data, stamp = T,
            ...)), 
    coord_sf(crs = st_crs_mod(ref_data)))
}


#' @export
write_stamp_region_text <- function(ref_data, required_aes = NULL){

  modified_function <- stamp_region_text

  formals(modified_function)$ref_data <- substitute(ref_data)

  return(modified_function)

}
