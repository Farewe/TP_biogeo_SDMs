# Atelier 1 - Dolomedes plantarius
library(biomod2)
library(ggplot2)
library(rworldmap)
library(raster)
library(reshape2)


# Attention a bien ecrire le repertoire de travail dans la ligne suivante !
wd <- "./Dolomedesplantarius/"

setwd(wd)

# Chargement des donn?es climatiques
current <- stack("current")
future2.6 <- stack("future2_6")
future8.5 <- stack("future8_5")


# Chargement des donn?es d'occurrence de l'esp?ce
load("PresencePoints.RData") # Objet charg? : P.points



wm <- getMap(resolution = "low")
plot(current[[1]])
plot(P.points, add = TRUE)
plot(wm, add = TRUE)



#### Etape 1 : preparation des donnees pour la modelisation ####
run.data <- BIOMOD_FormatingData(resp.var = P.points, # Points de presence de l'esp?ce
                                 expl.var = current, # Donnees environnemental de calibration : climat 1950-2000
                                 resp.name = "Dolomedesplantarius", # Nom de l'esp?ce
                                 PA.nb.rep = 2, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = length(P.points)) # Nombre de pseudo-absences echantillonnees a chaque tour
# On sauve l'objet a chaque fois :
save(run.data, file = "run.data")

#### Etape 2 : calibration des modeles ####
model.runs <- BIOMOD_Modeling(run.data, # Objet preparatoire
                              models =  c('GLM', 'RF', 'GBM'), # Mod?es que l'on va faire tourner
                              NbRunEval = 2, # Nombre de runs d'?valuation
                              DataSplit = 80, # Quantit? de donn?es utilisees pour la validation croisee des modeles
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

save(em.runs, file = "em.runs")

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
                                         projection.output = projection.current, # Projections a rassembler pour l'ensemble forecasting
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
seuil <- get_evaluations(em.runs)[[1]]["TSS", "Cutoff"]
seuil

# Cartes issues du mod?le d'ensemble (environmental suitability) : 
current.projection <- stack("./Dolomedesplantarius/proj_current/proj_current_Dolomedesplantarius_ensemble.grd")
plot(current.projection)
future2.6.projection <- stack("./Dolomedesplantarius/proj_future2.6/proj_future2.6_Dolomedesplantarius_ensemble.grd")
plot(future2.6.projection)
future8.5.projection <- stack("./Dolomedesplantarius/proj_future8.5/proj_future8.5_Dolomedesplantarius_ensemble.grd")
plot(future8.5.projection)

# Cr?ation de raster stacks propres pour l'analyse
suitability <- stack(current.projection[[1]], # Notez qu'on ne garde que la premi?re couche (moyenne du mod?le d'ensemble) 
                     future2.6.projection[[1]],
                     future8.5.projection[[1]])
names(suitability) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")


# Cartes binaires (1/0) calcul?es ? partir de la probabilit? moyenne du mod?le d'ensemble 
current.binary <- stack("Dolomedesplantarius/proj_current/proj_current_Dolomedesplantarius_ensemble_TSSbin")
future2.6.binary <- stack("Dolomedesplantarius/proj_future2.6/proj_future2.6_Dolomedesplantarius_ensemble_TSSbin")
future8.5.binary <- stack("Dolomedesplantarius/proj_future8.5/proj_future8.5_Dolomedesplantarius_ensemble_TSSbin")

# Cr?ation de stacks propres pour l'analyse
pa <- stack(current.binary[[1]], 
            future2.6.binary[[1]],
            future8.5.binary[[1]])
names(pa) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")

# Calcul de l'incertitude : ?cart type des probabilit?s s du mod?le d'ensemble
current.all <- stack("Dolomedesplantarius/proj_current/proj_current_Dolomedesplantarius.grd")
future2.6.all <- stack("Dolomedesplantarius/proj_future2.6/proj_future2.6_Dolomedesplantarius.grd")
future8.5.all <- stack("Dolomedesplantarius/proj_future8.5/proj_future8.5_Dolomedesplantarius.grd")
# N'h?sitez pas ? afficher ces stacks pour voir l'ensemble des mod?les individuels

# On cr?e un stack dans lequel on calcule l'?cart type des probas de pr?sence pour chaque projection
uncertainty <- stack(calc(current.all, sd), 
                     calc(future2.6.all, sd),
                     calc(future8.5.all, sd))
names(uncertainty) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")


#### Cartes ####
# 1. Probabilit? de pr?sence
plot(suitability)

# 2. Presence-absence
plot(pa)

# 3. Incertitude
plot(uncertainty)


#### Donnees France ####
pops_fra <- read.table("pops_france.txt", sep = "\t", h = T)
par(mfrow = c(1, 3))
plot(suitability[[1]], main = "Current", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[2]], main = "2070 RCP 2.6", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[3]], main = "2070 RCP 8.5", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)

#### Resultats par population ####
# Preparation des donnees pour plotter avec ggplot2
## Tableau contenant les probas de presence
suitability_fra <- extract(suitability, pops_fra[, c("x", "y")])
rownames(suitability_fra) <- pops_fra[, 1]
suitability_fra <- as.data.frame(melt(suitability_fra))
colnames(suitability_fra) <- c("Lieu", "Variable", "Value")

## Tableau contenant les incertitudes
uncertainty_fra <- extract(uncertainty, pops_fra[, c("x", "y")])
rownames(uncertainty_fra) <- pops_fra[, 1]
uncertainty_fra <- as.data.frame(melt(uncertainty_fra))
colnames(uncertainty_fra) <- c("Lieu", "Variable", "Value")


suitability_fra$inf <- suitability_fra$Value - uncertainty_fra$Value # Borne inferieure : moyenne issue de l'ensemble modelling - ecart-type
suitability_fra$sup <- suitability_fra$Value + uncertainty_fra$Value # Borne superieure : moyenne issue de l'ensemble modelling + ecart-type


# On plotte un graphique avec les valeurs par population, et les ecarts-types
ggplot(suitability_fra, aes(x = Variable, y = Value, col = Lieu)) + 
  geom_point(size = 3) + 
  facet_wrap(~Lieu) + # On separe le graphe par lieu
  geom_hline(aes(yintercept = seuil), alpha=.5, linetype = 2) + # On affiche le seuil de conversion en presence-absence
  ylab("Probabilite de presence") + xlab ("Scenario") +
  geom_errorbar(aes(ymin = inf, ymax = sup)) # Et les barres d'erreur autour de la moyenne issue de l'ensemble modelling
