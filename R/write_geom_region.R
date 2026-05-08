#' @export
write_geom_region <- function(ref_data){

  modified_fun <- geom_region

  formals(modified_fun)$ref_data <- substitute(ref_data)

  return(modified_fun)

}



#' @export
write_stamp_region <- function(ref_data){

  modified_function <- stamp_region

formals(modified_function)$ref_data <- substitute(ref_data)
# formals(modified_function)$data <- substitute(ref_data)


return(modified_function)

}


#' @export
write_geom_region_border <- function(ref_data){

  modified_fun <- geom_region_border

  formals(modified_fun)$ref_data <- substitute(ref_data)

  return(modified_fun)

}

#' @export
write_stamp_region_border <- function(ref_data){

  modified_fun <- geom_stamp_border

  formals(modified_fun)$ref_data <- substitute(ref_data)

  return(modified_fun)

}


#' @export
write_geom_region_text <- function(ref_data){

  modified_function <- geom_region_text

  formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}


#' @export
write_stamp_region_text <- function(ref_data){

  modified_function <- stamp_region_text

  formals(modified_function)$ref_data <- substitute(ref_data)

  return(modified_function)

}


#' @export
write_geom_region_label <- function(ref_data){

  modified_function <- geom_region_label

  formals(modified_function)$ref_data <- substitute(ref_data)

return(modified_function)

}


#' @export
write_stamp_region_label <- function(ref_data){

  modified_function <- stamp_region_label

  formals(modified_function)$ref_data <- substitute(ref_data)

  return(modified_function)

}
