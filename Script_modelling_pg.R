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
                                 PA.nb.rep = 3, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = 1000, # Nombre de pseudo-absences echantillonnees a chaque tour
                                 PA.strategy = "random") # Stratégie d'échantillonnages des pseudo-absences
# On sauve l'objet a chaque fois :
save(run_data, file = "run_data")

#### Etape 2 : calibration des modeles ####
model_runs <- BIOMOD_Modeling(run_data, # Objet preparatoire
                              models =  c('GLM', 'MARS', 'GBM'), # Modeles que l'on va faire tourner
                              nb.rep = 2, # Nombre de runs d'evaluation
                              data.split.perc = 80, # Quantite de donnees utilisees pour la validation croisee des modeles
                              # 80% pour la calibration, 20% pour la validation
                              prevalence = 0.5,
                              do.full.models = FALSE
)
save(model_runs, file = "model_runs")

#### Etape 3 : Ensemble modelling ####
em_runs <- BIOMOD_EnsembleModeling(model_runs, # Objet issu de l'etape 2
                                   models.chosen = 'all', # Utiliser tous les modeles calibres
                                   em.by = 'all', # Combiner tous les modèles ensemble 
                                   em.algo = "EMmean", # Faire la moyenne des probabilités individuelles
                                   metric.select = 'TSS', # Quelle métrique utiliser pour filtrer les mauvais modèles 
                                   metric.select.thresh = 0.6, # Quel seuil de filtration des mauvais modèles ? 
                                   metric.eval = c("TSS", "ROC"), # Quelles métriques utiliser pour évaluer l'EM ?
)


save(em_runs, file = "em_runs")


#### Etape 4 : Projection des modeles individuels ####
##### 4.1 Projection dans le climat actuel #####
projection_current <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                        new.env = current, # Donnees climatiques pour la projection
                                        proj.name = "current", # Nom de la projection
                                        models.chosen = 'all', # On projette tous les modeles
                                        metric.binary = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                        build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection_current, file = "projection_current")

ef_current <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                         bm.proj = projection_current, # Projections a rassembler pour l'ensemble forecasting
                                         metric.binary = "TSS")
save(ef_current, file = "ef_current")

##### Projection dans le climat futur, SSP 2 - 4.5 #####
projection_future245 <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                          new.env = future245, # Donnees climatiques pour la projection
                                          proj.name = "future245", # Nom de la projection
                                          models.chosen = 'all', # On projette tous les modeles
                                          metric.binary = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                          metric.filter = "TSS", # Metrique utilisee pour filtrer les 'mauvais' modeles
                                          build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection_future245, file = "future245")

ef_future245 <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                           bm.proj = projection_future245, # Projections a rassembler pour l'ensemble forecasting
                                           metric.binary = "TSS")
save(ef_future245, file = "ef_future245")

##### 4.3 Projection dans le climat futur, SSP 5 - 8.5 #####
projection_future585 <- BIOMOD_Projection(bm.mod = model_runs, # Objet issu de l'etape 4 (calibration des modeles individuels)
                                          new.env = future585, # Donnees climatiques pour la projection
                                          proj.name = "future585", # Nom de la projection
                                          models.chosen = 'all', # On projette tous les modeles
                                          metric.binary = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                          metric.filter = "TSS", # Metrique utilisee pour filtrer les 'mauvais' modeles
                                          build.clamping.mask = TRUE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection_future585, file = "projection_future585")

ef_future585 <- BIOMOD_EnsembleForecasting(bm.em = em_runs,  # Objet issu de l'etape 5 (ensemble modelling)
                                           bm.proj = projection_future585, # Projections a rassembler pour l'ensemble forecasting
                                           metric.binary = "TSS")
save(ef_future585, file = "ef_future585")


# Verification de la qualite des modeles
evals <- get_evaluations(model_runs)
ggplot(evals, aes(x = algo, y = validation)) + geom_boxplot() + facet_grid(metric.eval ~ .)

# Voir le seuil de conversion de proba vers presence-absence :
seuil <- get_evaluations(em_runs)[, "cutoff"]
seuil


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


# Calcul de l'incertitude : ecart type des probabilitEs de presences du modele d'ensemble
current_all <- rast("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis.tif")
future245_all <- rast("Phelsumagrandis/proj_future245/proj_future245_Phelsumagrandis.tif")
future585_all <- rast("Phelsumagrandis/proj_future585/proj_future585_Phelsumagrandis.tif")
# N'hesitez pas a afficher ces stacks pour voir l'ensemble des modeles individuels

# On cree un stack dans lequel on calcule l'ecart type des probas de presence pour chaque projection
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
