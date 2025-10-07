geom_region0 <- ggplot2::make_constructor(ggplot2::GeomSf, stat = StatRegion) 
stamp_region0 <- ggplot2::make_constructor(ggplot2::GeomSf, stat = StatRegion, stamp = T, inherit.aes = F)
geom_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion)
stamp_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion, stamp = T, inherit.aes = F)
