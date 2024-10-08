# Atelier 3 - Phelsuma grandis
library(biomod2)
library(ggplot2)
library(geodata)
library(terra)

setwd("Phelsumagrandis/")

# Chargement des données climatiques
current <- rast("current.tif")
future245 <- rast("future245.tif")
future585 <- rast("future585.tif")

# Choix de variables adaptées 
current <- current[[c("bio4", "bio5", "bio12", "bio15")]]
future245 <- future245[[c("bio4", "bio5", "bio12", "bio15")]]
future585 <- future585[[c("bio4", "bio5", "bio12", "bio15")]]



# Chargement des données d'occurrence de l'espèce
P_points <- readRDS("P_points.RDS")
# Objet au format SpatVector (package terra)


# Faire une carte pour voir les données
wm <- geodata::world(path = ".")
plot(current[[1]])
plot(P_points, add = TRUE, col = "darkblue")
plot(wm, add = TRUE)



#### Etape 1 : preparation des donnees pour la modelisation ####
run_data <- BIOMOD_FormatingData(resp.name = "Phelsumagrandis", # Nom de l'espece
                                 resp.var = P_points, # Points de presence de l'espece
                                 expl.var = current, # Donnees environnemental de calibration : climat 1950-2000
                                 PA.nb.rep = 2, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = 1000, # Nombre de pseudo-absences echantillonnees a chaque tour
                                 PA.strategy = "random") # Stratégie d'échantillonnages des pseudo-absences

#### Etape 2 : calibration des modeles ####
model_runs <- BIOMOD_Modeling(run_data, # Objet preparatoire
                              models =  c('GLM', 'MARS', 'GBM'), # Modeles que l'on va faire tourner
                              CV.nb.rep = 2, # Nombre de runs d'evaluation
                              CV.perc = 0.8, # Quantite de donnees utilisees pour la validation croisee des modeles
                              # 80% pour la calibration, 20% pour la validation
                              CV.do.full.models = FALSE, # Faire les modèles complets 
                              metric.eval = "BOYCE", # Métrique d'évaluation, boyce est la seule possible en présence seule
                              prevalence = 0.5 # Faire en sorte que les poids des présences et des pseudoabsences soient équivalents
)

#### Etape 3 : Ensemble modelling ####
em_runs <- BIOMOD_EnsembleModeling(model_runs, # Objet issu de l'etape 2
                                   models.chosen = 'all', # Utiliser tous les modeles calibres
                                   em.by = 'all', # Combiner tous les modèles ensemble 
                                   em.algo = "EMmean", # Faire la moyenne des suitabilities individuelles
                                   metric.select = 'BOYCE', # Quelle métrique utiliser pour filtrer les mauvais modèles 
                                   metric.select.thresh = 0.5, # Quel seuil de filtration des mauvais modèles ? 
                                   metric.eval = "BOYCE" # Quelles métriques utiliser pour évaluer l'EM ?
)




#### Etape 4 : Projection des modeles individuels ####
##### 4.1 Projection dans le climat actuel #####
projection_current <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                        new.env = current, # Donnees climatiques pour la projection
                                        proj.name = "current", # Nom de la projection
                                        models.chosen = 'all', # On projette tous les modeles
                                        build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration

ef_current <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                         bm.proj = projection_current # Projections a rassembler pour l'ensemble forecasting
                                         )


##### Projection dans le climat futur, SSP 2 - 4.5 #####
projection_future245 <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                          new.env = future245, # Donnees climatiques pour la projection
                                          proj.name = "future245", # Nom de la projection
                                          models.chosen = 'all', # On projette tous les modeles
                                          build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration


ef_future245 <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                           bm.proj = projection_future245 # Projections a rassembler pour l'ensemble forecasting
                                           )


##### 4.3 Projection dans le climat futur, SSP 5 - 8.5 #####
projection_future585 <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                          new.env = future585, # Donnees climatiques pour la projection
                                          proj.name = "future585", # Nom de la projection
                                          models.chosen = 'all', # On projette tous les modeles
                                          build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration

ef_future585 <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                           bm.proj = projection_future585 # Projections a rassembler pour l'ensemble forecasting
                                           )


# Verification de la qualite des modeles
evals <- get_evaluations(model_runs)
ggplot(evals, aes(x = algo, y = validation)) + geom_boxplot() + facet_grid(metric.eval ~ .)
# Notez bien la qualité des évaluations. Qu'en déduire pour l'interprétation ?
# L'indice de Boyce est compris entre - 1 et 1
# -1 le modèle prédit l'opposé de la réalité
# 0 le modèle est équivalent à un modèle aléatoire
# 1 les prédictions du modèles sont parfaitement alignées sur les observations



# Cartes issues du modele d'ensemble (environmental suitability) : 
current_projection <- rast("./Phelsumagrandis/proj_current/proj_current_Phelsumagrandis_ensemble.tif")
plot(current_projection)
future245_projection <- rast("./Phelsumagrandis/proj_future245/proj_future245_Phelsumagrandis_ensemble.tif")
plot(future245_projection)
future585_projection <- rast("./Phelsumagrandis/proj_future585/proj_future585_Phelsumagrandis_ensemble.tif")
plot(future585_projection)

# Creation de raster stacks propres pour l'analyse
suitability <- c(current_projection, 
                 future245_projection,
                 future585_projection)
names(suitability) <- c("Current", "Future SSP 2 - 4.5", "Future SSP 5 - 8.5")


# Calcul de l'incertitude : ecart type des suitabilities du modele d'ensemble
current_all <- rast("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis.tif")
future245_all <- rast("Phelsumagrandis/proj_future245/proj_future245_Phelsumagrandis.tif")
future585_all <- rast("Phelsumagrandis/proj_future585/proj_future585_Phelsumagrandis.tif")
# N'hesitez pas a afficher ces stacks pour voir l'ensemble des modeles individuels

# On cree un stack dans lequel on calcule l'ecart type des suitabilities pour chaque projection
uncertainty <- c(app(current_all, sd), 
                 app(future245_all, sd),
                 app(future585_all, sd))
names(uncertainty) <- c("Current", "Future SSP 2 - 4.5", "Future SSP 5 - 8.5")



# Repartition globale predite
plot(suitability,
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")
plot(uncertainty, 
     col = viridis::plasma(10),
     type = "continuous")


# Repartition globale predite actuelle + points de présence connus
plot(suitability[[1]],
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")

plot(P_points, add = TRUE, pch = 16, col = "black", cex = .6)
plot(P_points, add = TRUE, col = "pink", cex = .5)

# Reunion
# x11()
plot(suitability, xlim = c(52, 58), ylim = c(-24, -18), 
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")

# Notez comme les projections futures continennent moins de pixels que les
# projections actuelles. C'est une limite des modèles pour les zones côtières,
# à expliquer à vos collègues...

plot(uncertainty, xlim = c(52, 58), ylim = c(-24, -18), 
     col = viridis::plasma(10),
     type = "continuous")

# Guadeloupe et Martinique
# x11()
plot(suitability, xlim = c(-66, -56), ylim = c(10, 20), 
     col = rev(viridis::viridis(10)),
     range = c(0, 1000),
     type = "continuous")
plot(uncertainty, xlim = c(-66, -56), ylim = c(10, 20), 
     col = rev(viridis::plasma(10)),
     type = "continuous")


# Mayotte
# x11()
plot(suitability, xlim = c(42, 48), ylim = c(-17, -10), 
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")
plot(uncertainty, xlim = c(42, 48), ylim = c(-17, -10), 
     col = viridis::plasma(10),
     type = "continuous")
# Nouvelle Caledonie
# x11()
plot(suitability, xlim = c(162, 172), ylim = c(-26, -16), 
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")
plot(uncertainty, xlim = c(162, 172), ylim = c(-26, -16), 
     col = viridis::plasma(10),
     type = "continuous")
# Polynesie Francaise
# x11()
plot(suitability, xlim = c(-154, -146), ylim = c(-20, -13), 
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")
plot(uncertainty, xlim = c(-154, -146), ylim = c(-20, -13), 
     col = viridis::plasma(10),
     type = "continuous")
