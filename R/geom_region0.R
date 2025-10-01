geom_region0 <- ggplot2::make_constructor(ggplot2::GeomSf, stat = StatRegion) # no crs
stamp_region0 <- ggplot2::make_constructor(ggplot2::GeomSf, stat = StatRegion, stamp = T, inherit.aes = F) # no crs
geom_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion) # no crs
stamp_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion, stamp = T, inherit.aes = F) # no crs
