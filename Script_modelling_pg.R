# Atelier 3 - Phelsuma grandis
library(biomod2)
library(rworldmap)
library(ggplot2)


# Attention a bien ecrire le repertoire de travail dans la ligne suivante !
wd <- "./Phelsumagrandis/"

setwd(wd)

load("PresencePoints.RData") # Objet chargé : P.points

wm <- getMap(resolution = "low")
plot(current[[1]])
plot(P.points, add = TRUE)
plot(wm, add = TRUE)


current <- stack("current")
future2.6 <- stack("future2_6")
future8.5 <- stack("future8_5")


#### Etape 1 : pr?paration des donn?es pour la mod?lisation ####
run.data <- BIOMOD_FormatingData(resp.var = P.points, # Points de pr?sence de l'esp?ce
                                 expl.var = current, # Donn?es environnemental de calibration : climat 1950-2000
                                 resp.name = "Phelsumagrandis", # Nom de l'esp?ce
                                 PA.nb.rep = 2, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = 1000) # Nombre de pseudo-absences ?chantillonn?es ? chaque tour
# On sauve l'objet ? chaque fois :
save(run.data, file = "run.data")

#### Etape 2 : calibration des mod?les ####
model.runs <- BIOMOD_Modeling(run.data, # Objet pr?paratoire
                              models =  c('GLM', 'RF', 'GBM'), # Mod?les que l'on va faire tourner
                              NbRunEval = 2, # Nombre de runs d'?valuation
                              DataSplit = 80, # Quantit? de donn?es utilis?es pour la validation crois?e des mod?les
                                              # 80% pour la calibration, 20% pour la validation
                              SaveObj = T, # Sauver les objets de mod?lisation sur le disque dur
                              Prevalence = 0.5, # Pour donner un poids ?gal aux points de pr?sence et pseudo-absences
                              do.full.models = FALSE) # Ne pas faire une calibration sur 100% des donn?es
save(model.runs, file = "model.runs")

#### Etape 3 : Ensemble modelling ####
em.runs <- BIOMOD_EnsembleModeling(model.runs, # Objet issu de l'?tape 2
                                   chosen.models = 'all', # Utiliser tous les mod?les calibr?s
                                   em.by = 'all', # Utiliser tous les mod?les calibr?s
                                   eval.metric = 'TSS', # M?trique de qualit? utilis?e pour supprimer les 'mauvais' mod?les de l'EM
                                   eval.metric.quality.threshold = .6, # Seuil de suppression des 'mauvais' mod?les
                                   prob.mean = TRUE, # Moyenne des probas de pr?sence issues des mod?les
                                   prob.cv = FALSE, # Coefficient de variation des probas de pr?sence issues des mod?les
                                   prob.ci = TRUE, # Intervalle de confiance autour de la moyenne
                                   prob.ci.alpha = 0.05, # Seuil de l'IC (0.05 pour avoir l'IC 95%)
                                   prob.median = FALSE, # M?diane des probas
                                   committee.averaging = FALSE, # % de mod?les pr?disant pr?sence
                                   prob.mean.weight = FALSE # Moyenne pond?r?e par les ?vals des mod?les
)


#### Etape 4 : Projection des mod?les individuels ####
##### 4.1 Projection dans le climat actuel #####
projection.current <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'?tape 2 (calibration des mod?les individuels)
                                        new.env = current, # Donn?es climatiques pour la projection
                                        proj.name = "current", # Nom de la projection
                                        selected.models = 'all', # On projette tous les mod?les
                                        binary.meth = "TSS", # M?trique utilis?e pour transformer la proba de pr?sence en pr?sence-absence
                                        filtered.meth = "TSS", # M?trique utilis?e pour filtrer les 'mauvais' mod?les
                                        do.stack = TRUE, # Cr?er un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones o? le climat est tr?s diff?rent du climat  utilis? lors de la calibration
save(projection.current, file = "projection.current")

ef.current <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'?tape 3 (ensemble modelling)
                                         projection.output = projection.current, # Projections ? rassembler pour l'ensemble forecasting
                                         binary.meth = "TSS")
save(ef.current, file = "ef.current")

##### 4.2 Projection dans le climat futur, RCP 2.6 #####
projection.future2.6 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'?tape 2 (calibration des mod?les individuels)
                                        new.env = future2.6, # Donn?es climatiques pour la projection
                                        proj.name = "future2.6", # Nom de la projection
                                        selected.models = 'all', # On projette tous les mod?les
                                        binary.meth = "TSS", # M?trique utilis?e pour transformer la proba de pr?sence en pr?sence-absence
                                        filtered.meth = "TSS", # M?trique utilis?e pour filtrer les 'mauvais' mod?les
                                        do.stack = TRUE, # Cr?er un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones o? le climat est tr?s diff?rent du climat  utilis? lors de la calibration
save(projection.future2.6, file = "projection.future2.6")

ef.future2.6 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'?tape 3 (ensemble modelling)
                                         projection.output = projection.future2.6, # Projections ? rassembler pour l'ensemble forecasting
                                         binary.meth = "TSS")
save(ef.future2.6, file = "ef.future2.6")

##### 4.3 Projection dans le climat futur, RCP 8.5 #####
projection.future8.5 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'?tape 2 (calibration des mod?les individuels)
                                        new.env = future8.5, # Donn?es climatiques pour la projection
                                        proj.name = "future8.5", # Nom de la projection
                                        selected.models = 'all', # On projette tous les mod?les
                                        binary.meth = "TSS", # M?trique utilis?e pour transformer la proba de pr?sence en pr?sence-absence
                                        filtered.meth = "TSS", # M?trique utilis?e pour filtrer les 'mauvais' mod?les
                                        do.stack = TRUE, # Cr?er un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones o? le climat est tr?s diff?rent du climat  utilis? lors de la calibration
save(projection.future8.5, file = "projection.future8.5")

ef.future8.5 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'?tape 3 (ensemble modelling)
                                           projection.output = projection.future8.5, # Projections ? rassembler pour l'ensemble forecasting
                                           binary.meth = "TSS")
save(ef.future8.5, file = "ef.future8.5")


# V?rification de la qualit? des mod?les
evals <- melt(get_evaluations(model.runs))
colnames(evals) <- c("Metric", "Variable", "Model", "CV.Run", "PA", "value")
evals <- evals[which(evals$Metric %in% c("TSS", "ROC") & evals$Variable == "Testing.data"), ]
ggplot(evals, aes(x = Model, y = value)) + geom_boxplot() + facet_grid(Metric ~ .)

# Voir le seuil de conversion de proba vers pr?sence-absence :
get_evaluations(em.runs)[[1]]["TSS", "Cutoff"]


# Cartes issues du modèle d'ensemble (environmental suitability) : 
current.projection <- stack("./Phelsumagrandis/proj_current/proj_current_Phelsumagrandis_ensemble.grd")
plot(current.projection)
future2.6.projection <- stack("./Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis_ensemble.grd")
plot(future2.6.projection)
future8.5.projection <- stack("./Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis_ensemble.grd")
plot(future8.5.projection)

# Création de raster stacks propres pour l'analyse
suitability <- stack(current.projection[[1]], 
                     future2.6.projection[[1]],
                     future8.5.projection[[1]])
names(suitability) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")


# Cartes binaires (1/0) calculées à partir de la probabilité moyenne du modèle d'ensemble 
current.binary <- stack("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis_ensemble_TSSbin")
future2.6.binary <- stack("Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis_ensemble_TSSbin")
future8.5.binary <- stack("Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis_ensemble_TSSbin")

# Création de stacks propres pour l'analyse
pa <- stack(current.binary[[1]], # Notez qu'on ne garde que la première couche (moyenne du modèle d'ensemble)  
            future2.6.binary[[1]],
            future8.5.binary[[1]])
names(pa) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")

# Calcul de l'incertitude : écart type des probabilités de présences du modèle d'ensemble
current.all <- stack("Phelsumagrandis/proj_current/proj_current_Phelsumagrandis.grd")
future2.6.all <- stack("Phelsumagrandis/proj_future2.6/proj_future2.6_Phelsumagrandis.grd")
future8.5.all <- stack("Phelsumagrandis/proj_future8.5/proj_future8.5_Phelsumagrandis.grd")
# N'hésitez pas à afficher ces stacks pour voir l'ensemble des modèles individuels

# On crée un stack dans lequel on calcule l'écart type des probas de présence pour chaque projection
uncertainty <- stack(calc(current.all, sd), 
                     calc(future2.6.all, sd),
                     calc(future8.5.all, sd))
names(uncertainty) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")



# R?partition globale pr?dite
plot(suitability)
plot(pa)
plot(uncertainty)

# R?union
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

# Nouvelle Cal?donie
# x11()
plot(suitability, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1000))
plot(uncertainty, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1000))
plot(pa, xlim = c(162, 172), ylim = c(-26, -16), zlim = c(0, 1))

# Polyn?sie Fran?aise
# x11()
plot(suitability, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1000))
plot(uncertainty, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1000))
plot(pa, xlim = c(-154, -146), ylim = c(-20, -13), zlim = c(0, 1))
