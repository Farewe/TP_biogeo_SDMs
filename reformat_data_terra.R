# Data preparation
# Copy paste baseline.tif, ipsl_ssp245_2070.tif & ipsl_ssp585_2070.tif
# from cours_sdms

current <- rast("baseline.tif")
current <- current[[c("bio2", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(current, "current.tif", overwrite = TRUE)
futuressp126 <- rast("ipsl_ssp245_2070.tif")
futuressp126 <- futuressp126[[c("bio2", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(futuressp126, "futuressp126.tif", overwrite = TRUE)
futuressp585 <- rast("ipsl_ssp585_2070.tif")
futuressp585 <- futuressp585[[c("bio2", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(futuressp585, "futuressp585.tif", overwrite = TRUE)


load("PresencePoints.RData") # Objet chargé : P.points
P.points

P_points  <- vect(P.points)
saveRDS(P_points, "P_points.RDS")

str(P.points)


## Ateliers
#PG

current <- rast(paste0("d:/r/Projects/Cours_SDMs/data_cours/wc2.1_10m_bio/",
                       list.files("d:/r/Projects/Cours_SDMs/data_cours/wc2.1_10m_bio")))
names(current) <- gsub("wc2.1_10m_", "", names(current))
names(current) <- gsub("_", "", names(current))
# current <- terra::aggregate(current[[c("bio2", "bio4", "bio6", "bio14")]],
#                             fact = 3)
writeRaster(current, 
            "Phelsumagrandis/current.tif",
            overwrite = TRUE)


future245 <- rast("d:/r/Projects/Cours_SDMs/data_cours/wc2.1_10m_bioc_IPSL-CM6A-LR_ssp245_2061-2080.tif")
names(future245) <-  paste0("bio", 1:19)
# future245 <- terra::aggregate(future245[[c("bio2", "bio4", "bio6", "bio14")]],
#                             fact = 3)
writeRaster(future245, 
            "future245.tif",
            overwrite = TRUE)

future585 <- rast("d:/r/Projects/Cours_SDMs/data_cours/wc2.1_10m_bioc_IPSL-CM6A-LR_ssp585_2061-2080.tif")
names(future585) <-  paste0("bio", 1:19)
# future585 <- terra::aggregate(future585[[c("bio2", "bio4", "bio6", "bio14")]],
#                               fact = 3)
writeRaster(future585, 
            "Phelsumagrandis/future585.tif",
            overwrite = TRUE)


load("Phelsumagrandis/PresencePoints.RData") # Objet chargé : P.points
P.points

P_points  <- vect(P.points)
# P <- rasterize(P_points,
#           current)
# P <- as.points(P)

saveRDS(P, "Phelsumagrandis/P_points.RDS")

# NV
current <- rast("baseline.tif")
current <- current[[c("bio1", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(current, "Neonvalentulus/current.tif", overwrite = TRUE)
future245 <- rast("ipsl_ssp245_2070.tif")
future245 <- future245[[c("bio1", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(future245, "Neonvalentulus/future245.tif", overwrite = TRUE)
future585 <- rast("ipsl_ssp585_2070.tif")
future585 <- future585[[c("bio1", "bio6", "bio7", "bio8", "bio15")]]
writeRaster(future585, "Neonvalentulus/future585.tif", overwrite = TRUE)


load("Neonvalentulus/PresencePoints.RData") # Objet chargé : P.points
P.points

P_points  <- vect(P.points)
saveRDS(P_points, "Neonvalentulus/P_points.RDS")


# DP
current <- rast("baseline.tif")
current <- current[[c("bio2", "bio5", "bio6", "bio7", "bio8")]]
writeRaster(current, "Dolomedesplantarius/current.tif", overwrite = TRUE)
future245 <- rast("ipsl_ssp245_2070.tif")
future245 <- future245[[c("bio2", "bio5", "bio6", "bio7", "bio8")]]
writeRaster(future245, "Dolomedesplantarius/future245.tif", overwrite = TRUE)
future585 <- rast("ipsl_ssp585_2070.tif")
future585 <- future585[[c("bio2", "bio5", "bio6", "bio7", "bio8")]]
writeRaster(future585, "Dolomedesplantarius/future585.tif", overwrite = TRUE)


load("Dolomedesplantarius/PresencePoints.RData") # Objet chargé : P.points
P.points

P_points  <- vect(P.points)
saveRDS(P_points, "Dolomedesplantarius/P_points.RDS")