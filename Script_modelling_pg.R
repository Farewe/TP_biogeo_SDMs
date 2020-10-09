# Atelier 3 - Phelsuma grandis
library(biomod2)
library(ggplot2)
library(rworldmap)
library(raster)
library(reshape2)

# Attention a bien ecrire le repertoire de travail dans la ligne suivante !
wd <- "./Phelsumagrandis/"

setwd(wd)

# Chargement des donnees climatiques
current <- stack("current")
future2.6 <- stack("future2_6")
future8.5 <- stack("future8_5")


# Chargement des donnees d'occurrence de l'espece
load("PresencePoints.RData") # Objet charge : P.points


wm <- getMap(resolution = "low")
plot(current[[1]])
plot(P.points, add = TRUE)
plot(wm, add = TRUE)



#### Etape 1 : preparation des donnees pour la modelisation ####
run.data <- BIOMOD_FormatingData(resp.var = P.points, # Points de presence de l'espece
                                 expl.var = current, # Donnees environnemental de calibration : climat 1950-2000
                                 resp.name = "Phelsumagrandis", # Nom de l'espece
                                 PA.nb.rep = 2, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = length(P.points)) # Nombre de pseudo-absences echantillonnees e chaque tour
# On sauve l'objet a chaque fois :
save(run.data, file = "run.data")

#### Etape 2 : calibration des modeles ####
model.runs <- BIOMOD_Modeling(run.data, # Objet preparatoire
                              models =  c('GLM', 'RF', 'GBM'), # Modeles que l'on va faire tourner
                              NbRunEval = 2, # Nombre de runs d'evaluation
                              DataSplit = 80, # Quantite de donnees utilisees pour la validation croisee des modeles
                                              # 80% pour la calibration, 20% pour la validation
                              SaveObj = T, # Sauver les objets de modelisation sur le disque dur
                              Prevalence = 0.5, # Pour donner un poids egal aux points de presence et pseudo-absences
                              do.full.models = FALSE) # Ne pas faire une calibration sur 100% des donnees
save(model.runs, file = "model.runs")

#### Etape 3 : Ensemble modelling ####
em.runs <- BIOMOD_EnsembleModeling(model.runs, # Objet issu de l'etape 2
                                   chosen.models = 'all', # Utiliser tous les modeles calibres
                                   em.by = 'all', # Utiliser tous les modeles calibres
                                   eval.metric = 'TSS', # Metrique de qualite utilisee pour supprimer les 'mauvais' modeles de l'EM
                                   eval.metric.quality.threshold = .6, # Seuil de suppression des 'mauvais' modeles
                                   prob.mean = TRUE, # Moyenne des probas de presence issues des modeles
                                   prob.cv = FALSE, # Coefficient de variation des probas de presence issues des modeles
                                   prob.ci = TRUE, # Intervalle de confiance autour de la moyenne
                                   prob.ci.alpha = 0.05, # Seuil de l'IC (0.05 pour avoir l'IC 95%)
                                   prob.median = FALSE, # Mediane des probas
                                   committee.averaging = FALSE, # % de modeles predisant presence
                                   prob.mean.weight = FALSE # Moyenne ponderee par les evals des modeles
)


#### Etape 4 : Projection des modeles individuels ####
##### 4.1 Projection dans le climat actuel #####
projection.current <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                        new.env = current, # Donnees climatiques pour la projection
                                        proj.name = "current", # Nom de la projection
                                        selected.models = 'all', # On projette tous les modeles
                                        binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                        filtered.meth = "TSS", # Metrique utilisee pour filtrer les 'mauvais' modeles
                                        do.stack = TRUE, # Creer un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.current, file = "projection.current")

ef.current <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                         projection.output = projection.current, # Projections e rassembler pour l'ensemble forecasting
                                         binary.meth = "TSS")
save(ef.current, file = "ef.current")

##### 4.2 Projection dans le climat futur, RCP 2.6 #####
projection.future2.6 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                        new.env = future2.6, # Donnees climatiques pour la projection
                                        proj.name = "future2.6", # Nom de la projection
                                        selected.models = 'all', # On projette tous les modeles
                                        binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                        filtered.meth = "TSS", # Metrique utilisee pour filtrer les 'mauvais' modeles
                                        do.stack = TRUE, # Creer un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.future2.6, file = "projection.future2.6")

ef.future2.6 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                         projection.output = projection.future2.6, # Projections a rassembler pour l'ensemble forecasting
                                         binary.meth = "TSS")
save(ef.future2.6, file = "ef.future2.6")

##### 4.3 Projection dans le climat futur, RCP 8.5 #####
projection.future8.5 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                        new.env = future8.5, # Donnees climatiques pour la projection
                                        proj.name = "future8.5", # Nom de la projection
                                        selected.models = 'all', # On projette tous les modeles
                                        binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                        filtered.meth = "TSS", # Metrique utilisee pour filtrer les 'mauvais' modeles
                                        do.stack = TRUE, # Creer un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.future8.5, file = "projection.future8.5")

ef.future8.5 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                           projection.output = projection.future8.5, # Projections a rassembler pour l'ensemble forecasting
                                           binary.meth = "TSS")
save(ef.future8.5, file = "ef.future8.5")


# Verification de la qualite des modeles
evals <- melt(get_evaluations(model.runs))
colnames(evals) <- c("Metric", "Variable", "Model", "CV.Run", "PA", "value")
evals <- evals[which(evals$Metric %in% c("TSS", "ROC") & evals$Variable == "Testing.data"), ]
ggplot(evals, aes(x = Model, y = value)) + geom_boxplot() + facet_grid(Metric ~ .)

# Voir le seuil de conversion de proba vers presence-absence :
get_evaluations(em.runs)[[1]]["TSS", "Cutoff"]


# Cartes issues du modele d'ensemble (environmental suitability) : 
current.projection <- stack("./Phelsumagrandis/proj_current/proj_current_Phelsumagrandis_ensemble.grd")
plot(current.projection)
future2.6.projection <- stack("./Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis_ensemble.grd")
plot(future2.6.projection)
future8.5.projection <- stack("./Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis_ensemble.grd")
plot(future8.5.projection)

# Creation de raster stacks propres pour l'analyse
suitability <- stack(current.projection[[1]], 
                     future2.6.projection[[1]],
                     future8.5.projection[[1]])
names(suitability) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")


# Cartes binaires (1/0) calculees a partir de la probabilite moyenne du modele d'ensemble 
current.binary <- stack("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis_ensemble_TSSbin")
future2.6.binary <- stack("Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis_ensemble_TSSbin")
future8.5.binary <- stack("Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis_ensemble_TSSbin")

# Creation de stacks propres pour l'analyse
pa <- stack(current.binary[[1]], # Notez qu'on ne garde que la premiere couche (moyenne du modele d'ensemble)  
            future2.6.binary[[1]],
            future8.5.binary[[1]])
names(pa) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")

# Calcul de l'incertitude : ecart type des probabilitEs de presences du modele d'ensemble
current.all <- stack("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis.grd")
future2.6.all <- stack("Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis.grd")
future8.5.all <- stack("Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis.grd")
# N'hesitez pas a afficher ces stacks pour voir l'ensemble des modeles individuels

# On cree un stack dans lequel on calcule l'ecart type des probas de presence pour chaque projection
uncertainty <- stack(calc(current.all, sd), 
                     calc(future2.6.all, sd),
                     calc(future8.5.all, sd))
names(uncertainty) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")



# Repartition globale predite
plot(suitability)
plot(pa)
plot(uncertainty)

# Reunion
# x11()
plot(suitability, xlim = c(52, 58), ylim = c(-24, -18), zlim = c(0, 1000))
plot(uncertainty, xlim = c(52, 58), ylim = c(-24, -18), zlim = c(0, 1000))
plot(pa, xlim = c(52, 58), ylim = c(-24, -18), zlim = c(0, 1))


# Guadeloupe et Martinique
# x11()
plot(suitability, xlim = c(-66, -56), ylim = c(10, 20), zlim = c(0, 1000))
plot(uncertainty, xlim = c(-66, -56), ylim = c(10, 20), zlim = c(0, 1000))
plot(pa, xlim = c(-66, -56), ylim = c(10, 20), zlim = c(0, 1))

# Mayotte
# x11()
plot(suitability, xlim = c(42, 48), ylim = c(-17, -10), zlim = c(0, 1000))
plot(uncertainty, xlim = c(42, 48), ylim = c(-17, -10), zlim = c(0, 1000))
plot(pa, xlim = c(42, 48), ylim = c(-17, -10), zlim = c(0, 1))

# Nouvelle Caledonie
# x11()
plot(suitability, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1000))
plot(uncertainty, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1000))
plot(pa, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1))

# Polynesie Francaise
# x11()
plot(suitability, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1000))
plot(uncertainty, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1000))
plot(pa, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1))
