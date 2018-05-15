#' Identify some representative time series of an ANTARES project
#' 
#' @description 
#' Identify some representative time series of an ANTARES project containing a significant amount of Monte-Carlo years.
#' Usually based on France (= \code{mainAreas}) load monotonous.
#' Europe can also be imported as \code{extraAreas} to take energy imports and exports into account.
#' 
#' @param mainAreas
#'   Vector containing the names of the areas on which the clustering algorithm will be based.
#'   Usually \code{mainAreas} = \code{"fr"}.
#' @param extraAreas
#'   Vector containing the names of the additional areas on which the clustering algorithm will be based.
#'   Usually it contains areas that need to be taken into account in the algorithm but in a smaller scale than the ones in \code{mainAreas}.
#'   If \code{mainAreas} = \code{"fr"}, \code{extraAreas} must not contain \code{"fr"} again to be relevant enough.
#'   If \code{NULL}, no extra area is imported.
#' @param selection
#'   Numeric representing the amount of Monte-Carlo years to get after the function.
#' @param MCYears
#'   Index of the Monte-Carlo years to import. 
#'   If \code{"all"}, every MC years are read, else the specified Monte-Carlo simulations are imported. 
#' @param weightMain
#'   Numeric giving the weighting of the load monotonous for the main areas into the clustering algorithm choices.
#'   If \code{0}, no importance is given to this criteria. If \code{1}, the algorithm will be based only on this criteria.
#' @param weightPeakMean
#'   Numeric giving the weighting of the peak period (20 most crucial hours) for the main areas into the clustering algorithm choices. 
#'   If \code{0}, no importance is given to this criteria. If \code{1}, the algorithm will be based only on this criteria.
#' @param weightExtra
#'   Numeric giving the weighting of the load monotonous for the additional areas into the clustering algorithm choices.
#'   It is usually lower than \code{WeightExtra}. 
#'   If \code{0}, no importance is given to this criteria. If \code{1}, the algorithm will be based only on this criteria.
#' @param weightPeakExtra
#'   Numeric giving the weighting of the peak period (20 most crucial hours) for the additional areas into the clustering algorithm choices. 
#'   It is usually lower than \code{WeightPeakMain}. 
#'   If \code{0}, no importance is given to this criteria. If \code{1}, the algorithm will be based only on this criteria.
#' @param subtractUnavoidableEnergyMain
#'   If \code{TRUE}, unavoidable energy (Solar, Wind, etc.) is subtracted from LOAD of the main areas.
#' @param subtractUnavoidableEnergyExtra
#'   If \code{TRUE}, unavoidable energy (Solar, Wind, etc.) is subtracted from LOAD of the additional areas.
#' @param subtractNuclearAvailabilityMain
#'   If \code{TRUE}, nuclear availability is subtracted from LOAD of the main areas.
#' @param subtractNuclearAvailabilityExtra
#'   If \code{TRUE}, nuclear availability is subtracted from LOAD of the additional areas.   
#' @param displayCurves
#'   If \code{TRUE}, the function displays a load monotonous curve analysis
#' @param displayTable
#'   If \code{TRUE}, the function displays a cost analysis (OP. COST, LOLD, UNSP. ENRG)   
#' @param opts
#'   List of simulation parameters returned by the function \code{antaresRead::setSimulationPath}
#'
#' @details 
#' When \code{subtractUnavoidableEnergyMain}, \code{subtractUnavoidableEnergyExtra}, \code{subtractNuclearAvailabilityMain} and \code{subtractNuclearAvailabilityExtra} are all \code{TRUE}, the function may crash because of insufficient memory. 
#' In such a case, it is necessary to reduce the size of the input.
#' Different strategies are available depending on your objective : reduce the number of Monte-Carlo years, do not subtract nuclear availability (especially on extra areas), take fewer areas, increase the memory (with setRam), etc.
#'
#' @return
#' If \code{displayCurves} and \code{displayTable} are both \code{FALSE}, only identities and weightings of the selected Monte-Carlo years are displayed.
#' Else, curves of load monotonous for imported areas will be also plotted (for every MC years, clusters and average curves) and a cost analysis will be given (based on OP. COST, LOLD and UNSP. ENRG).
#' 
#' @examples 
#' # Import simulation
#' setSimulationPath()
#' 
#' # Find 5 Monte-Carlo year clusters for the simulation
#' # Study France as the main area and europe as the secondary area
#' # Base algorithm on :
#' # 40% for the load monotonous in France
#' # 40% for the peak period in France
#' # 20% for the load monotonous in all europe
#' # 0% for the peak period in all europe
#' europe <- c("at", "be", "ch", "de", "es", "gb", "ie", "it", "lu_be", "lu_de", "ni", "nl", "pt")
#' select_years(mainAreas = "fr", extraAreas = europe, weightMain = 0.4, weightPeakMain = 0.4, weightExtra = 0.2)
#' 
#' @import data.table
#' @importFrom dplyr filter group_by summarise arrange
#' @importFrom cluster pam
#' @importFrom antaresRead simOptions readClusterDesc
#' @export

select_years <- function(mainAreas = "fr", extraAreas = NULL, selection = 5, MCYears = "all", weightMain = 0.5, weightPeakMain = 0.5, weightExtra = 0, weightPeakExtra = 0, subtractUnavoidableEnergyMain = TRUE, subtractUnavoidableEnergyExtra = TRUE, subtractNuclearAvailabilityMain = TRUE, subtractNuclearAvailabilityExtra = FALSE, displayCurves = TRUE, displayTable = TRUE, opts = antaresRead::simOptions())
{
  ##### INITIALIZATION #####
  
  # Definition of the random draw
  set.seed(1)

  
  
  
  ##### CREATION OF FUNCTIONS #####
  
  # Function allowing to extract values of total annual load (before subtracting renewables) to analyse it after the clustering
  extractLoad <- function(antaresDataList_areas) {
    antares_load <- antaresDataList_areas[,.(mcYear, LOAD)]
    antares_load <- group_by(antares_load, mcYear)
    antares_load <- summarise(antares_load, TOTLOAD = sum(LOAD))
    return(antares_load)
  }
  
  # Function that defines Net load = Load - UndispatchableEnergy (without mustRun)
  addNetLoadnoMustRun <- function(antaresDataList_areas) {
    antaresDataList_areas$LOAD <- (antaresDataList_areas$LOAD - antaresDataList_areas$`ROW BAL.` - antaresDataList_areas$PSP - antaresDataList_areas$`MISC. NDG` - antaresDataList_areas$`H. ROR` - antaresDataList_areas$WIND - antaresDataList_areas$SOLAR)
    return(antaresDataList_areas)
  }
  
  # Function subtracting the nuclear availability from the load
  subtractNucAvailability <- function(antaresDataList, areas) {
    # Identifying clusters dealing with nuclear availability with the right areas
    liste_clusters <- readClusterDesc()
    liste_clusters <- filter(liste_clusters, area %in% areas & group == "nuclear")
    # Adaptation of data.table "clusters" to the format of data.table "areas"
    # Filtering nuclear, grouping data and summing the right areas
    antaresDataList$clusters <- filter(antaresDataList$clusters, cluster %in% liste_clusters$cluster)
    antaresDataList$clusters <- group_by(antaresDataList$clusters, area, timeId, mcYear)
    antaresDataList$clusters <- summarise(antaresDataList$clusters, nuclearAvailability = sum(thermalAvailability))
    # Subtracting nuclear availability
    if ( nrow(antaresDataList$clusters) == nrow(antaresDataList$areas) ) {
      antaresDataList$areas$LOAD <- antaresDataList$areas$LOAD - antaresDataList$clusters$nuclearAvailability
    }
    return(antaresDataList)
  }
  
  # Function creating a load matrix in a descendent way for every MC year (in order to study load duration curves)
  loadDurationMatrix <- function(antaresDataList_areas) {
    antaresDataList_areas <- group_by(antaresDataList_areas, mcYear, timeId)
    antaresDataList_areas <- summarise(antaresDataList_areas, LOAD = sum(LOAD))
    antaresDataList_areas <- arrange(antaresDataList_areas, mcYear, desc(LOAD))
    matrix_conso <- matrix(data = NA, ncol = max(antaresDataList_areas$mcYear), nrow = max(antaresDataList_areas$timeId))
    for (MC_year in 1 : max(antaresDataList_areas$mcYear)) {
      matrix_conso[, MC_year] <- antaresDataList_areas[antaresDataList_areas$mcYear == MC_year, ]$LOAD
    }
    return(matrix_conso)
  }
  
  # Function creating a reference matrix (re-sampled) for a batch of load duration curves
  completeLoadDuration <- function(loadMatrix) {
    # Initialisation of the vector containing every load data
    conso_globale_full <- as.vector(loadMatrix)
    conso_globale_full <- sort(conso_globale_full, decreasing = TRUE)
    conso_globale <- rep(NA, times = nrow(loadMatrix))
    # To re-sample, we only keep the following years 1, 1001, 2001, 4001, etc. (if there is 1000 load duration curves)
    for (time in 1 : nrow(loadMatrix)) {
      conso_globale[time] <- conso_globale_full[ncol(loadMatrix)*(time-1)+1]
    }
    return(conso_globale)
  }
  
  # Function creating a vector of L3 distance between every load duration curves and the reference load duration one 
  # L3 distance has been chosen to stress big deviations and to take into account the sign (-) for the position, contrary to a L2 distance 
  l3DistanceRef <- function(matrix_conso, completeLoadData) {
    matrix_ecart_ref <- matrix(data = NA, ncol = ncol(matrix_conso), nrow = nrow(matrix_conso))
    dist_reference <- rep(NA, times = ncol(matrix_conso))
    for (MC_year in 1 : ncol(matrix_conso)) {
      for (time in 1 : nrow(matrix_conso)) {
        matrix_ecart_ref[time, MC_year] <- (matrix_conso[time,MC_year] - completeLoadData[time]) ^ 3
      }
      dist_reference[MC_year] <- sum(matrix_ecart_ref[,MC_year])
    }
    return(dist_reference)
  }
  
  # Fonction permettant de renvoyer un vecteur de distance L3 entre chaque monotone et la monotone de référence sur la pointe (écart calculé sur les 20 premières heures)
  l3PeakDistanceRef <- function(matrix_conso, completeMonotonous) {
    # Initialisation de la matrice des écarts de consommation avec la monotone de référence
    matrix_peak_ecart_ref <- matrix(data = NA, ncol = ncol(matrix_conso), nrow = 20)
    # Initialisation du vecteur de distance avec la monotone de référence
    dist_peak_reference <- rep(NA, times = ncol(matrix_conso))
    # Définition des distances (on prends le cube des distances pour pouvoir favoriser l'étude des grands écarts tout en gardant la notion de signe)
    for (MC_year in 1 : ncol(matrix_conso)) {
      for (time in 1 : 20) {
        matrix_peak_ecart_ref[time, MC_year] <- (matrix_conso[time,MC_year] - completeMonotonous[time]) ^ 3
      }
      dist_peak_reference[MC_year] <- sum(matrix_peak_ecart_ref[,MC_year])
    }
    return(dist_peak_reference)
  }
  
  # Fonction permettant l'agrégation des indicateurs (normalisés) pour l'algorithme de clustering
  aggregateIndicators <- function(dist_reference_main, dist_reference_peak_main, dist_reference_extra = rep(0, length(dist_reference_main)), dist_reference_peak_extra = rep(0, length(dist_reference_main)), nuc_info_main = matrix(0, length(dist_reference_main), 2)) {
    cluster_indicators <- matrix(c(dist_reference_main, dist_reference_peak_main, dist_reference_extra, dist_reference_peak_extra, nuc_info_main), nrow = length(dist_reference_main), ncol = 6)
    cluster_indicators <- as.data.frame(scale(cluster_indicators))
    cluster_indicators[,1] <- cluster_indicators[,1] * weightMain
    cluster_indicators[,2] <- cluster_indicators[,2] * weightPeakMain
    cluster_indicators[,3] <- cluster_indicators[,3] * weightExtra
    cluster_indicators[,4] <- cluster_indicators[,4] * weightPeakExtra
    return(cluster_indicators)
  }
  
  # Fonction permettant la visualisation de toutes ces monotones
  plotMonotonous <- function(title, matrix_conso, matrix_conso_clusters, complete_conso, complete_conso_clusters, x_lim = NULL, y_lim = NULL) {
    matplot(matrix_conso, type = "l", lty = 3, xlab = "Operating time (hours)", ylab = "Net load (W)", col = "grey", xlim = x_lim, ylim = y_lim, main = title)
    matlines(matrix_conso_clusters, col = 3:(3+ncol(matrix_conso_clusters)), lty = 2, lwd = 2)
    matlines(complete_conso, col = "black", lwd = 2)
    matlines(complete_conso_clusters, col = "red", lwd = 2)
    legend("topright", legend = c("All load monotonous", "Reference monotonous", "Weighted mean monotonous of the clusters", paste("Cluster : MC year ", info_clusters$`Selected years`, "- Weighting : ", info_clusters$Weighting*100/ncol(matrix_conso), "%")), col = c("grey", "black", "red", 3:(3+ncol(matrix_conso_clusters))), pch = 1)
  }
  
  # Fonction permettant la creation d'un tableau de comparaison de valeurs cles : LOLD, OP. COST, UNSP ENRG
  costAnalysis <- function(antaresDataList_areas, antares_load, clusterList) {
    # Conversion des données initiales en donnees annuelles
    antaresDataList_areas <- changeTimeStep(antaresDataList_areas, "annual")
    # Création du vecteur nom
    name <- c("MEAN OF ALL MC YEARS", "WEIGHTED MEAN OF CLUSTERS", "DIFFERENCE", "RELATIVE DIFFERENCE (in %)")
    # Création du vecteur ANNUAL LOAD
    load_all <- mean(antares_load$`TOTLOAD`)
    load_cluster <- antares_load[antares_load$mcYear %in% clusterList$`Selected years`,]$`TOTLOAD`
    load_cluster_pondere <- sum(load_cluster * clusterList$Weighting)/max(antares_load$mcYear)
    load_diff <- load_all - load_cluster_pondere
    load_rel <- (load_all - load_cluster_pondere)*100/load_all
    load <- c(load_all, load_cluster_pondere, load_diff, load_rel)
    # Création du vecteur OP. COST 
    opcost_all <- mean(antaresDataList_areas$`OP. COST`)
    opcost_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`OP. COST`
    opcost_cluster_pondere <- sum(opcost_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    opcost_diff <- opcost_all - opcost_cluster_pondere
    opcost_rel <- (opcost_all - opcost_cluster_pondere)*100/opcost_all
    opcost <- c(opcost_all, opcost_cluster_pondere, opcost_diff, opcost_rel)
    # Creation du vectebur LOLD
    lold_all <- mean(antaresDataList_areas$`LOLD`)
    lold_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`LOLD`
    lold_cluster_pondere <- sum(lold_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    lold_diff <- lold_all - lold_cluster_pondere
    lold_rel <- (lold_all - lold_cluster_pondere)*100/lold_all
    lold <- c(lold_all, lold_cluster_pondere, lold_diff, lold_rel)
    # Creation du vecteur UNSP. ENRG
    unsp_all <- mean(antaresDataList_areas$`UNSP. ENRG`)
    unsp_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`UNSP. ENRG`
    unsp_cluster_pondere <- sum(unsp_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    unsp_diff <- unsp_all - unsp_cluster_pondere
    unsp_rel <- (unsp_all - unsp_cluster_pondere)*100/unsp_all
    unsp <- c(unsp_all, unsp_cluster_pondere, unsp_diff, unsp_rel)
    # Création du data.table
    costTable <- data.table("NAME" = name, "ANNUAL LOAD (W)" = load, "OP. COST (euros)" = opcost, "LOLD (hours)" = lold, "UNSP. ENRG (MWh)" = unsp)
    return(costTable)
  }
  
  
  
  
  
  ##### TRAITEMENT DES DONNEES SUR LA ZONE PRINCIPALE #####
    
  # Lecture de l'étude Antares et création des jeux de données sur la zone principale
  cat("Processing data from the main areas :", mainAreas, ":\n")
  data_etude_main <- readAntares(areas = mainAreas, clusters = mainAreas,  mcYears = MCYears, thermalAvailabilities = subtractNuclearAvailabilityMain, select = c("LOAD", "OP. COST","LOLD", "UNSP. ENRG", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND", "SOLAR"))
  
  # Extracting the column of real load data (before subtracting renawables) in order to compare values after the clustering
  data_load_main <- extractLoad(data_etude_main$areas)
  
  # Ajout de la consommation nette définie comme NETLOAD = LOAD - PRODUCTION FATALE - DISPO NUCLEAIRE
  if (subtractUnavoidableEnergyMain == TRUE) {
    data_etude_main$areas <- addNetLoadnoMustRun(data_etude_main$areas)
  }
  if (subtractNuclearAvailabilityMain == TRUE) {
    data_etude_main <- subtractNucAvailability(data_etude_main, mainAreas)
  } 
  
  # Création d'une matrice de consommation, classée par ordre décroissant pour pouvoir étudier la monotone, selon les années MC
  matrix_conso_main <- loadMonotonousMatrix(data_etude_main$areas)
  
  # Création de la monotone globale des 1000 années MC (ré-échantillonée)
  complete_conso_main <- completeLoadMonotonous(matrix_conso_main)
  
  # Création du vecteur de distance entre toutes les monotones et la monotone de référence (= montonone globale ré-échantillonée)
  l3_dist_main <- l3DistanceRef(matrix_conso_main, complete_conso_main)
  
  # Création du vecteur de distance entre toutes les monotones et la monotone de référence sur la pointe
  l3_peak_dist_main <- l3PeakDistanceRef(matrix_conso_main, complete_conso_main)
  
 
  
  
  
  
  ##### TRAITEMENT DES DONNEES SUR LA ZONE SECONDAIRE #####

  if (is.null(extraAreas) == FALSE) {
    
    # Lecture de l'étude Antares et création des jeux de données sur la zone principale
    cat("\nProcessing data from the extra areas :", extraAreas, ":\n")
    data_etude_extra <- readAntares(areas = extraAreas, mcYears = MCYears, thermalAvailabilities = subtractNuclearAvailabilityExtra, select = c("LOAD", "OP. COST","LOLD", "UNSP. ENRG", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND", "SOLAR"))
    
    # Ajout de la consommation nette définie comme NETLOAD = LOAD - PRODUCTION FATALE - DISPO NUCLEAIRE
    if (subtractUnavoidableEnergyExtra == TRUE) {
      data_etude_extra <- addNetLoadnoMustRun(data_etude_extra)
    }
    if (subtractNuclearAvailabilityExtra == TRUE) {
      data_etude_extra <- subtractNucAvailability(data_etude_extra, extraAreas)
    }
    
    # Création d'une matrice de consommation, classée par ordre décroissant pour pouvoir étudier la monotone, selon les années MC
    matrix_conso_extra <- loadMonotonousMatrix(data_etude_extra)
    
    # Création de la monotone globale des 1000 années MC (ré-échantillonée)
    complete_conso_extra <- completeLoadMonotonous(matrix_conso_extra)
    
    # Création du vecteur de distance entre toutes les monotones et la monotone de référence (= montonone globale ré-échantillonée)
    l3_dist_extra <- l3DistanceRef(matrix_conso_extra, complete_conso_extra)
    
    # Création du vecteur de distance entre toutes les monotones et la monotone de référence sur la pointe
    l3_peak_dist_extra <- l3PeakDistanceRef(matrix_conso_extra, complete_conso_extra)
  }  
    

  
  
  
  
  
  ##### AGREGATION DES DEUX ZONES ET CLUSTERING #####
  
  # Création d'un data.frame normalisé regroupant tous les indicateurs selon leur poids (attention à l'ordre)
  if (is.null(extraAreas) == FALSE) {
    cluster_indicators <- aggregateIndicators(l3_dist_main, l3_peak_dist_main, l3_dist_extra, l3_peak_dist_extra)
  } else {
    cluster_indicators <- aggregateIndicators(l3_dist_main, l3_peak_dist_main)
  }
   
  # Algorithme des k-medoids
  kmed_data <- pam(cluster_indicators, selection)
  info_clusters <- data.table("Selected years" = kmed_data$id.med, "Weighting" = kmed_data$clusinfo[,"size"])
  info_clusters <- arrange(info_clusters, `Selected years`)
  
  
  
  
  
  ##### ANALYSE DES RESULTATS PAR LES MONOTONES #####
  if (displayCurves == TRUE) {
    # Création des monotones des années MC sélectionnées après clustering
    matrix_conso_clusters_main <- matrix_conso_main[,info_clusters$`Selected years`]
    
    # Création des monotones globales des années MC sélectionnées après clustering (ré-échantillonée)
    complete_conso_clusters_main <- completeLoadMonotonous(matrix_conso_main[, rep(info_clusters$`Selected years`, info_clusters$Weighting)])
    
    # Visualisation de toutes ces monotones
    plotMonotonous(title = paste(c("Load monotonous on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main)
    
    # Visualisation de toutes ces monotones sur le pic de consommation
    plotMonotonous(title = paste(c("Load peak on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main, x_lim=c(0,60), y_lim=c(mean(matrix_conso_main),max(matrix_conso_main)*1.05))
    
    # Visualisation de toutes ces monotones sur le creux de consommation
    plotMonotonous(title = paste(c("Load troughs on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main, x_lim = c(8680,8740), y_lim = c(min(matrix_conso_main)*1.05,mean(matrix_conso_main)))
    
    # # Impression de l'écart entre la monotone de référence et la monotone ré-échantillonée de l'ensemble des clusters (pour comparer la sensibité aux paramètres d'entrée : selection, poids, etc.)
    # print("Ecart L2 entre la monotone de reference et la monotone globale re-echantillonee sur la zone principale :")
    # print(sum((complete_conso_main-complete_conso_clusters_main)^2))
    
    if(is.null(extraAreas) == FALSE) {
      # Création des monotones des années MC sélectionnées après clustering
      matrix_conso_clusters_extra <- matrix_conso_extra[,info_clusters$`Selected years`]
      
      # Création des monotones globales des années MC sélectionnées après clustering (ré-échantillonée)
      complete_conso_clusters_extra <- completeLoadMonotonous(matrix_conso_extra[, rep(info_clusters$`Selected years`, info_clusters$Weighting)])
      
      # Visualisation de toutes ces monotones
      plotMonotonous(title = paste(c("Load monotonous on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra)
      
      # Visualisation de toutes ces monotones sur le pic de consommation
       plotMonotonous(title = paste(c("Load peak on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra, x_lim = c(0,60), y_lim = c(mean(matrix_conso_extra),max(matrix_conso_extra)*1.05))
      
      # Visualisation de toutes ces monotones sur le creux de consommation
      plotMonotonous(title = paste(c("Load troughs on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra, x_lim = c(8680,8740), y_lim = c(min(matrix_conso_extra)*1.05,mean(matrix_conso_extra)))
      
      # # Impression de l'écart entre la monotone de référence et la monotone ré-échantillonée de l'ensemble des clusters (pour comparer la sensibité aux paramètres d'entrée : selection, poids, etc.)
      # print("Ecart L2 entre la monotone de reference et la monotone globale re-echantillonee sur la zone secondaire :")
      # print(sum((complete_conso_extra-complete_conso_clusters_extra)^2))
    }
  }
  
  
  
  
  
  
  
  
  ##### ANALYSE DES RESULTATS PAR LES COUTS #####
  if (displayTable == TRUE) {
    # Création d'un tableau de valeurs et de comparaison sur l'operating cost et le loss of load duration entre la moyenne des 1000 années et la moyenne pondérée des clusters
    data_comparison_main <- costAnalysis(data_etude_main$areas, data_load_main, info_clusters)
    
    # Impression des tableaux
    cat("\n Cost analysis on the main areas :", mainAreas, ":\n")
    print(data_comparison_main)
    cat("\n")
  }
  
  
  
  
  
  ##### FIN DE FONCTION #####
  return(info_clusters)
  
}