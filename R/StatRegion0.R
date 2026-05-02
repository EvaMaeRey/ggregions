# compute just reproduced verbatim from above for packaging purposes
#' Compute panel regions
#'
#' @description
#' A short description...
#'
#' @param data A data frame.
#' @param scales Scales.
#' @param ref_data A reference data frame.
#' @param keep A character vector of IDs to keep. Optional.
#' @param drop A character vector of IDs to drop. Optional.
#' @param stamp A single logical value. Optional.
#'
#' @returns
#' A data frame. If `stamp` is `FALSE`, the processed `ref_data` inner-joined with `data`.
#' Otherwise, the processed `ref_data`.
#'
#' @export
compute_panel_regions0 <- function(data, scales, ref_data, keep = NULL, 
                                  drop = NULL, stamp = F){

  ref_data$id <- ref_data[1][[1]]

  if(!is.null(keep)){ref_data <- ref_data |> dplyr::filter(id %in% keep)}
  if(!is.null(drop)){ref_data <- ref_data |> dplyr::filter(!(id %in% drop))}

  ref_data <- ref_data |> 
    ggplot2::StatSf$compute_panel(coord = ggplot2::CoordSf) |>
    ggplot2::StatSfCoordinates$compute_group(coord = ggplot2::CoordSf)

  if(!stamp){ ref_data |> dplyr::inner_join(data) } else { ref_data }

}


StatRegion0 <- ggplot2::ggproto("StatRegion0",
                      ggplot2::Stat,
                      compute_panel = compute_panel_regions0,
                      default_aes = 
                        ggplot2::aes(label = ggplot2::after_stat(id)))
