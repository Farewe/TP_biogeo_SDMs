# Atelier 2 - Neon valentulus
library(biomod2)
library(ggplot2)
library(geodata)
library(terra)

setwd("Neonvalentulus/")

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
run_data <- BIOMOD_FormatingData(resp.name = "Neonvalentulus", # Nom de l'espece
                                 resp.var = P_points, # Points de presence de l'espece
                                 expl.var = current, # Donnees environnemental de calibration : climat 1950-2000
                                 PA.nb.rep = 3, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = 1000, # Nombre de pseudo-absences echantillonnees a chaque tour
                                 PA.strategy = "random") # Stratégie d'échantillonnages des pseudo-absences

#### Etape 2 : calibration des modeles ####
model_runs <- BIOMOD_Modeling(run_data, # Objet preparatoire
                              models =  c('GLM', 'MARS', 'RF'), # Modeles que l'on va faire tourner
                              CV.nb.rep = 2, # Nombre de runs d'evaluation
                              CV.perc = 0.80, # Quantite de donnees utilisees pour la validation croisee des modeles
                              # 80% pour la calibration, 20% pour la validation
                              CV.do.full.models = FALSE,
                              metric.eval = "BOYCE", # Métrique d'évaluation, boyce est la seule possible en présence seule
                              prevalence = 0.5,
)

#### Etape 3 : Ensemble modelling ####
em_runs <- BIOMOD_EnsembleModeling(model_runs, # Objet issu de l'etape 2
                                   models.chosen = 'all', # Utiliser tous les modeles calibres
                                   em.by = 'all', # Combiner tous les modèles ensemble 
                                   em.algo = "EMmean", # Faire la moyenne des suitabilities
                                   metric.select = 'BOYCE', # Quelle métrique utiliser pour filtrer les mauvais modèles 
                                   metric.select.thresh = 0.5, # Quel seuil de filtration des mauvais modèles ? 
                                   metric.eval = "BOYCE")


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


##### 4.2 Projection dans le climat futur, SSP 2 - 4.5 #####
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
current_projection <- rast("./Neonvalentulus/proj_current/proj_current_Neonvalentulus_ensemble.tif")
plot(current_projection)
future245_projection <- rast("./Neonvalentulus/proj_future245/proj_future245_Neonvalentulus_ensemble.tif")
plot(future245_projection)
future585_projection <- rast("./Neonvalentulus/proj_future585/proj_future585_Neonvalentulus_ensemble.tif")
plot(future585_projection)

# Creation de raster stacks propres pour l'analyse
suitability <- c(current_projection, 
                 future245_projection,
                 future585_projection)
names(suitability) <- c("Current", "Future SSP 2 - 4.5", "Future SSP 5 - 8.5")


# Calcul de l'incertitude : ecart type des suitabilities du modele d'ensemble
current_all <- rast("Neonvalentulus/proj_current/proj_current_Neonvalentulus.tif")
future245_all <- rast("Neonvalentulus/proj_future245/proj_future245_Neonvalentulus.tif")
future585_all <- rast("Neonvalentulus/proj_future585/proj_future585_Neonvalentulus.tif")
# N'hesitez pas a afficher ces stacks pour voir l'ensemble des modeles individuels

# On cree un stack dans lequel on calcule l'ecart type des suitabilities pour chaque projection
uncertainty <- c(app(current_all, sd), 
                 app(future245_all, sd),
                 app(future585_all, sd))
names(uncertainty) <- c("Current", "Future SSP 2 - 4.5", "Future SSP 5 - 8.5")


#### Cartes ####
# 1. suitabilities
plot(suitability,
     col = viridis::viridis(10),
     range = c(0, 1000),
     type = "continuous")

# 2. Incertitude
plot(uncertainty, 
     col = viridis::plasma(10),
     type = "continuous")

# Notez l'ampleur de l'incertitude sur les projections futures

#### Donnees France ####
pops_fra <- read.table("pops_france.txt", sep = "\t", h = T)
par(mfrow = c(1, 3))
plot(suitability[[1]], main = "Current", xlim = c(-6, 12), ylim = c(40, 52),
     col = viridis::viridis(10))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[2]], main = "Future SSP 2 - 4.5", xlim = c(-6, 12), ylim = c(40, 52),
     col = viridis::viridis(10))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[3]], main = "Future SSP 5 - 8.5", xlim = c(-6, 12), ylim = c(40, 52),
     col = viridis::viridis(10))
points(pops_fra$y ~ pops_fra$x)

#### Resultats par population ####
# Preparation des donnees pour plotter avec ggplot2
## Tableau contenant les suitabilities
suitability_fra <- extract(suitability, pops_fra[, c("x", "y")], ID = FALSE)
suitability_fra$Lieu <- pops_fra[, 1]
suitability_fra <- reshape2::melt(suitability_fra)

## Tableau contenant les incertitudes
uncertainty_fra <- extract(uncertainty, pops_fra[, c("x", "y")], ID = FALSE)
uncertainty_fra$Lieu <- pops_fra[, 1]
uncertainty_fra <- reshape2::melt(uncertainty_fra)
 
suitability_fra$inf <- suitability_fra$value - uncertainty_fra$value # Borne inferieure : moyenne issue de l'ensemble modelling - ecart-type
suitability_fra$sup <- suitability_fra$value + uncertainty_fra$value # Borne superieure : moyenne issue de l'ensemble modelling + ecart-type


# On plotte un graphique avec les valeurs par population, et les ecarts-types
ggplot(suitability_fra, aes(x = variable, y = value, col = Lieu)) + 
  geom_point(size = 3) + 
  facet_wrap(~Lieu) + # On separe le graphe par lieu
  ylab("Favorabilité") + xlab ("Scenario") +
  geom_errorbar(aes(ymin = inf, ymax = sup)) # Et les barres d'erreur autour de la moyenne issue de l'ensemble modelling

