GeomSfStamp <- ggplot2::ggproto("GeomSfStamp", ggplot2::GeomSf,
                       default_aes = modifyList(ggplot2::GeomSf$default_aes, 
                                                ggplot2::aes(fill = ggplot2::from_theme(scales::col_mix(ink, paper, 0.8))), 
                                                keep.null = T))

geom_region0 <- ggplot2::make_constructor(ggplot2::GeomSf, stat = StatRegion) 
stamp_region0 <- ggplot2::make_constructor(GeomSfStamp, stat = StatRegion, stamp = T, inherit.aes = F)
geom_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion)
stamp_region_text0 <- ggplot2::make_constructor(ggplot2::GeomText, stat = StatRegion, stamp = T, inherit.aes = F)
