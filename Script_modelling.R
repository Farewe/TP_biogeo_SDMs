library(biomod2)
library(ggplot2)
library(rworldmap)

# Attention a bien ecrire le repertoire de travail dans la ligne suivante !
wd <- "./Dolomedesplantarius/"

setwd(wd)

# 1. 
current <- stack("current")
future2.6 <- stack("future2_6")
future8.5 <- stack("future8_5")

# 2. 
load("PresencePoints.RData") # Objet chargé : P.points

wm <- getMap(resolution = "low")
plot(current[[1]])
plot(P.points, add = TRUE)
plot(wm, add = TRUE)

# 3. 
run.data <- BIOMOD_FormatingData(resp.var = P.points, # Points de presence de l'espece
                                 expl.var = current, # Donnees environnemental de calibration : climat 1950-2000
                                 resp.name = "Dolomedesplantarius", # Nom de l'espece
                                 PA.nb.rep = 2, # Nombre de runs de pseudo-absences
                                 PA.nb.absences = 1000) # Nombre de pseudo-absences echantillonnees a chaque tour
# On sauve l'objet a chaque fois :
save(run.data, file = "run.data.RData")

# 4.
model.runs <- BIOMOD_Modeling(run.data, # Objet pr?paratoire
                              models =  c('GLM', 'RF', 'GBM'), # Modeles que l'on va faire tourner
                              NbRunEval = 2, # Nombre de runs d'evaluation
                              DataSplit = 80, # Quantite de donnees utilisees pour la calibration des modeles
                              # 80% pour la calibration, 20% pour la validation
                              SaveObj = T, # Sauver les objets de modelisation sur le disque dur
                              Prevalence = 0.5, # Pour donner un poids egal aux points de présence et pseudo-absences
                              do.full.models = FALSE) # Ne pas faire une calibration sur 100% des données
save(model.runs, file = "model.runs")

# 5.
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

# 6.
##### 6.1 Projection dans le climat actuel #####
projection.current <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                        new.env = current, # Donnees climatiques pour la projection
                                        proj.name = "current", # Nom de la projection
                                        selected.models = 'all', # On projette tous les modeles
                                        binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                        do.stack = TRUE, # Creer un stack avec les projections
                                        keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                        build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.current, file = "projection.current")

ef.current <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                         projection.output = projection.current, # Projections a rassembler pour l'ensemble forecasting
                                         binary.meth = "TSS")
save(ef.current, file = "ef.current")

##### 6.2 Projection dans le climat futur, RCP 2.6 #####
projection.future2.6 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                          new.env = future2.6, # Donnees climatiques pour la projection
                                          proj.name = "future2.6", # Nom de la projection
                                          selected.models = 'all', # On projette tous les modeles
                                          binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                          do.stack = TRUE, # Creer un stack avec les projections
                                          keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                          build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.future2.6, file = "projection.future2.6")

ef.future2.6 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                           projection.output = projection.future2.6, # Projections a rassembler pour l'ensemble forecasting
                                           binary.meth = "TSS")
save(ef.future2.6, file = "ef.future2.6")

##### 6.3 Projection dans le climat futur, RCP 8.5 #####
projection.future8.5 <- BIOMOD_Projection(modeling.output = model.runs, # Objet issu de l'etape 2 (calibration des modeles individuels)
                                          new.env = future8.5, # Donnees climatiques pour la projection
                                          proj.name = "future8.5", # Nom de la projection
                                          selected.models = 'all', # On projette tous les modeles
                                          binary.meth = "TSS", # Metrique utilisee pour transformer la proba de presence en presence-absence
                                          do.stack = TRUE, # Creer un stack avec les projections
                                          keep.in.memory = F, # Pour ne pas saturer la RAM quand on charge l'objet
                                          build.clamping.mask = FALSE) # Pour identifier les zones ou le climat est tres different du climat  utilise lors de la calibration
save(projection.future8.5, file = "projection.future8.5")

ef.future8.5 <- BIOMOD_EnsembleForecasting(EM.output = em.runs,  # Objet issu de l'etape 3 (ensemble modelling)
                                           projection.output = projection.future8.5, # Projections a rassembler pour l'ensemble forecasting
                                           binary.meth = "TSS")
save(ef.future8.5, file = "ef.future8.5")


# 7
evals <- melt(get_evaluations(model.runs))
colnames(evals) <- c("Metric", "Variable", "Model", "CV.Run", "PA", "value")
evals <- evals[which(evals$Metric %in% c("TSS", "ROC") & evals$Variable == "Testing.data"), ]
ggplot(evals, aes(x = Model, y = value)) + 
  geom_boxplot() + 
  facet_grid(Metric ~ .)

# Voir le seuil de conversion de proba vers presence-absence :
seuil <- get_evaluations(em.runs)[[1]]["TSS", "Cutoff"]
seuil

# 8
plot(ef.current)
plot(ef.future2.6)
plot(ef.future8.5)

current.projection <- stack("./Dolomedesplantarius/proj_current/proj_current_Dolomedesplantarius_ensemble.grd")
plot(current.projection)
future2.6.projection <- stack("./Dolomedesplantarius/proj_future2.6/proj_future2.6_Dolomedesplantarius_ensemble.grd")
plot(future2.6.projection)
future8.5.projection <- stack("./Dolomedesplantarius/proj_future8.5/proj_future8.5_Dolomedesplantarius_ensemble.grd")
plot(future8.5.projection)


# 9
current.binary <- stack("Dolomedesplantarius/proj_current/proj_current_Dolomedesplantarius_ensemble_TSSbin")
plot(current.binary)
future2.6.binary <- stack("Dolomedesplantarius/proj_future2.6/proj_future2.6_Dolomedesplantarius_ensemble_TSSbin")
plot(future2.6.binary)
future8.5.binary <- stack("Dolomedesplantarius/proj_future8.5/proj_future8.5_Dolomedesplantarius_ensemble_TSSbin")
plot(future2.6.binary)

# 10
suitability <- stack(current.projection[[1]], 
                     future2.6.projection[[1]],
                     future8.5.projection[[1]])
names(suitability) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")
plot(suitability)


# 11
SRC <- BIOMOD_RangeSize(current.binary[[1]], 
                        future8.5.binary[[1]])
plot(SRC$Diff.By.Pixel,
     col = c("#FF4100", "#3016B0", "#F2F2F2FF", "#2DD700"))
legend(x = 75, y = 65, pch = 15, 
        col = c("#F2F2F2FF", "#FF4100", "#3016B0", "#2DD700"), 
        legend = c("Unsuitable", "Lost", "Kept", "New"),
        bty = "n", border = "white", cex = 1, y.intersp = 1,
        adj = c(0, .2), title = "Range change", xpd = NA)
