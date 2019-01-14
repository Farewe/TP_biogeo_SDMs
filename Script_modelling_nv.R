# Atelier 3 - Neon valentulus
library(biomod2)
library(ggplot2)
library(rworldmap)


# Attention a bien ecrire le repertoire de travail dans la ligne suivante !
wd <- "./Neonvalentulus/"

setwd(wd)

load("PresencePoints.RData") # Objet charg? : P.points

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
                                 resp.name = "Neonvalentulus", # Nom de l'esp?ce
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


save(em.runs, file = "em.runs")

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
seuil <- get_evaluations(em.runs)[[1]]["TSS", "Cutoff"]
seuil

# Cartes issues du modèle d'ensemble (environmental suitability) : 
current.projection <- stack("./Neonvalentulus/proj_current/proj_current_Neonvalentulus_ensemble.grd")
plot(current.projection)
future2.6.projection <- stack("./Neonvalentulus/proj_future2.6/proj_future2.6_Neonvalentulus_ensemble.grd")
plot(future2.6.projection)
future8.5.projection <- stack("./Neonvalentulus/proj_future8.5/proj_future8.5_Neonvalentulus_ensemble.grd")
plot(future8.5.projection)

# Création de raster stacks propres pour l'analyse
suitability <- stack(current.projection[[1]], # Notez qu'on ne garde que la première couche (moyenne du modèle d'ensemble) 
                     future2.6.projection[[1]],
                     future8.5.projection[[1]])
names(suitability) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")



# Cartes binaires (1/0) calculées à partir de la probabilité moyenne du modèle d'ensemble 
current.binary <- stack("Neonvalentulus/proj_current/proj_current_Neonvalentulus_ensemble_TSSbin")
future2.6.binary <- stack("Neonvalentulus/proj_future2.6/proj_future2.6_Neonvalentulus_ensemble_TSSbin")
future8.5.binary <- stack("Neonvalentulus/proj_future8.5/proj_future8.5_Neonvalentulus_ensemble_TSSbin")

# Création de stacks propres pour l'analyse
pa <- stack(current.binary[[1]], 
            future2.6.binary[[1]],
            future8.5.binary[[1]])
names(pa) <- c("Current", "Future RCP 2.6", "Future RCP 8.5")

# Calcul de l'incertitude : écart type des probabilités de présences du modèle d'ensemble
current.all <- stack("Neonvalentulus/proj_current/proj_current_Neonvalentulus.grd")
future2.6.all <- stack("Neonvalentulus/proj_future2.6/proj_future2.6_Neonvalentulus.grd")
future8.5.all <- stack("Neonvalentulus/proj_future8.5/proj_future8.5_Neonvalentulus.grd")
# N'hésitez pas à afficher ces stacks pour voir l'ensemble des modèles individuels

# On crée un stack dans lequel on calcule l'écart type des probas de présence pour chaque projection
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


#### Donn?es France ####
pops_fra <- read.table("pops_france.txt", sep = "\t", h = T)
par(mfrow = c(1, 3))
plot(suitability[[1]], main = "Current", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[2]], main = "2070 RCP 2.6", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)
plot(suitability[[3]], main = "2070 RCP 8.5", xlim = c(-6, 12), ylim = c(40, 52))
points(pops_fra$y ~ pops_fra$x)

#### R?sultats par population ####
# Pr?paration des donn?es pour plotter avec ggplot2
## Tableau contenant les probas de pr?sence
suitability_fra <- extract(suitability, pops_fra[, c("x", "y")])
rownames(suitability_fra) <- pops_fra[, 1]
suitability_fra <- as.data.frame(melt(suitability_fra))
colnames(suitability_fra) <- c("Lieu", "Variable", "Value")

## Tableau contenant les incertitudes
uncertainty_fra <- extract(uncertainty, pops_fra[, c("x", "y")])
rownames(uncertainty_fra) <- pops_fra[, 1]
uncertainty_fra <- as.data.frame(melt(uncertainty_fra))
colnames(uncertainty_fra) <- c("Lieu", "Variable", "Value")
 
suitability_fra$inf <- suitability_fra$Value - uncertainty_fra$Value # Borne inf?rieure : moyenne issue de l'ensemble modelling - ?cart-type
suitability_fra$sup <- suitability_fra$Value + uncertainty_fra$Value # Borne sup?rieure : moyenne issue de l'ensemble modelling + ?cart-type


# On plotte un graphique avec les valeurs par population, et les ?carts-types
ggplot(suitability_fra, aes(x = Variable, y = Value, col = Lieu)) + 
  geom_point(size = 3) + 
  facet_wrap(~Lieu) + # On s?pare le graphe par lieu
  geom_hline(aes(yintercept = seuil), alpha=.5, linetype = 2) + # On affiche le seuil de conversion en pr?sence-absence
  ylab("Probabilit? de pr?sence") + xlab ("Scenario") +
  geom_errorbar(aes(ymin = inf, ymax = sup)) # Et les barres d'erreur autour de la moyenne issue de l'ensemble modelling

