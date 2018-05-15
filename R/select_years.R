#' Identify some representative time series of an ANTARES project
#' 
#' @description 
#' Identify some representative time series of an ANTARES project containing a significant amount of Monte-Carlo years.
#' Usually based on France (= \code{mainAreas}) load duration curves.
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
#' @param subtractUndispatchableEnergyMain
#'   If \code{TRUE}, undispatchable energy (Solar, Wind, etc.) is subtracted from LOAD of the main areas.
#' @param subtractUndispatchableEnergyExtra
#'   If \code{TRUE}, undispatchable energy (Solar, Wind, etc.) is subtracted from LOAD of the additional areas.
#' @param subtractNuclearAvailabilityMain
#'   If \code{TRUE}, nuclear availability is subtracted from LOAD of the main areas.
#' @param subtractNuclearAvailabilityExtra
#'   If \code{TRUE}, nuclear availability is subtracted from LOAD of the additional areas.   
#' @param displayCurves
#'   If \code{TRUE}, the function displays a load monotonous curve analysis
#' @param displayTable
#'   If \code{TRUE}, the function displays a cost analysis (LOAD, OP. COST, LOLD, UNSP. ENRG)   
#' @param opts
#'   List of simulation parameters returned by the function \code{antaresRead::setSimulationPath}
#'
#' @details 
#' When \code{subtractUndispatchableEnergyMain}, \code{subtractUndispatchableEnergyExtra}, \code{subtractNuclearAvailabilityMain} and \code{subtractNuclearAvailabilityExtra} are all \code{TRUE}, the function may crash because of insufficient memory. 
#' In such a case, it is necessary to reduce the size of the input.
#' Different strategies are available depending on your objective : reduce the number of Monte-Carlo years, do not subtract nuclear availability (especially on extra areas), take fewer areas, increase the memory (with setRam), etc.
#'
#' @return
#' If \code{displayCurves} and \code{displayTable} are both \code{FALSE}, only identities and weightings of the selected Monte-Carlo years are displayed.
#' Else, load duration curves for imported areas will be also plotted (for every MC years, clusters and average curves) and a cost analysis will be given (based on ANNUAL LOAD, OP. COST, LOLD and UNSP. ENRG).
#' 
#' @examples 
#' # Import simulation
#' setSimulationPath()
#' 
#' # Find 5 Monte-Carlo year clusters for the simulation
#' # Study France as the main area and europe as the secondary area
#' # Base algorithm on :
#' # 40% for the load duration curve in France
#' # 40% for the peak period in France
#' # 20% for the load duration curve in all europe
#' # 0% for the peak period in all europe
#' europe <- c("at", "be", "ch", "de", "es", "gb", "ie", "it", "lu_be", "lu_de", "ni", "nl", "pt")
#' select_years(mainAreas = "fr", extraAreas = europe, weightMain = 0.4, weightPeakMain = 0.4, weightExtra = 0.2)
#' 
#' @import data.table
#' @importFrom dplyr filter group_by summarise arrange
#' @importFrom cluster pam
#' @importFrom antaresRead simOptions readClusterDesc
#' @export

select_years <- function(mainAreas = "fr", extraAreas = NULL, selection = 5, MCYears = "all", weightMain = 0.5, weightPeakMain = 0.5, weightExtra = 0, weightPeakExtra = 0, subtractUndispatchableEnergyMain = TRUE, subtractUndispatchableEnergyExtra = TRUE, subtractNuclearAvailabilityMain = TRUE, subtractNuclearAvailabilityExtra = FALSE, displayCurves = TRUE, displayTable = TRUE, opts = antaresRead::simOptions())
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
  
  # Function creating a vector of L3 distance between every load duration curves and the reference load duration one on the peak period (first 20 hours)
  # L3 distance has been chosen to stress big deviations and to take into account the sign (-) for the position, contrary to a L2 distance 
  l3PeakDistanceRef <- function(matrix_conso, completeLoadData) {
    matrix_peak_ecart_ref <- matrix(data = NA, ncol = ncol(matrix_conso), nrow = 20)
    dist_peak_reference <- rep(NA, times = ncol(matrix_conso))
     for (MC_year in 1 : ncol(matrix_conso)) {
      for (time in 1 : 20) {
        matrix_peak_ecart_ref[time, MC_year] <- (matrix_conso[time,MC_year] - completeLoadData[time]) ^ 3
      }
      dist_peak_reference[MC_year] <- sum(matrix_peak_ecart_ref[,MC_year])
    }
    return(dist_peak_reference)
  }
  
  # Function aggregating indicators (scaled) for the clustering algorithm
  aggregateIndicators <- function(dist_reference_main, dist_reference_peak_main, dist_reference_extra = NULL, dist_reference_peak_extra = NULL) {
    cluster_indicators <- matrix(c(dist_reference_main, dist_reference_peak_main, dist_reference_extra, dist_reference_peak_extra), nrow = length(dist_reference_main), ncol = 4)
    cluster_indicators <- as.data.frame(scale(cluster_indicators))
    cluster_indicators[,1] <- cluster_indicators[,1] * weightMain
    cluster_indicators[,2] <- cluster_indicators[,2] * weightPeakMain
    cluster_indicators[,3] <- cluster_indicators[,3] * weightExtra
    cluster_indicators[,4] <- cluster_indicators[,4] * weightPeakExtra
    return(cluster_indicators)
  }
  
  # Function plotting load duration curves
  plotMonotonous <- function(title, matrix_conso, matrix_conso_clusters, complete_conso, complete_conso_clusters, x_lim = NULL, y_lim = NULL) {
    matplot(matrix_conso, type = "l", lty = 3, xlab = "Operating time (hours)", ylab = "Net load (W)", col = "grey", xlim = x_lim, ylim = y_lim, main = title)
    matlines(matrix_conso_clusters, col = 3:(3+ncol(matrix_conso_clusters)), lty = 2, lwd = 2)
    matlines(complete_conso, col = "black", lwd = 2)
    matlines(complete_conso_clusters, col = "red", lwd = 2)
    legend("topright", legend = c("All load monotonous", "Reference monotonous", "Weighted mean monotonous of the clusters", paste("Cluster : MC year ", info_clusters$`Selected years`, "- Weighting : ", info_clusters$Weighting*100/ncol(matrix_conso), "%")), col = c("grey", "black", "red", 3:(3+ncol(matrix_conso_clusters))), pch = 1)
  }
  
  # Function creating a table that compares some key values : ANNUAL LOAD, LOLD, OP. COST, UNSP ENRG
  costAnalysis <- function(antaresDataList_areas, antares_load, clusterList) {
    # Converting initial data in annual mode
    antaresDataList_areas <- changeTimeStep(antaresDataList_areas, "annual")
    # Creating vector NAME
    name <- c("MEAN OF ALL MC YEARS", "WEIGHTED MEAN OF CLUSTERS", "DIFFERENCE", "RELATIVE DIFFERENCE (in %)")
    # Creation vector ANNUAL LOAD
    load_all <- mean(antares_load$`TOTLOAD`)
    load_cluster <- antares_load[antares_load$mcYear %in% clusterList$`Selected years`,]$`TOTLOAD`
    load_cluster_pondere <- sum(load_cluster * clusterList$Weighting)/max(antares_load$mcYear)
    load_diff <- load_all - load_cluster_pondere
    load_rel <- (load_all - load_cluster_pondere)*100/load_all
    load <- c(load_all, load_cluster_pondere, load_diff, load_rel)
    # Creating vector OP. COST 
    opcost_all <- mean(antaresDataList_areas$`OP. COST`)
    opcost_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`OP. COST`
    opcost_cluster_pondere <- sum(opcost_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    opcost_diff <- opcost_all - opcost_cluster_pondere
    opcost_rel <- (opcost_all - opcost_cluster_pondere)*100/opcost_all
    opcost <- c(opcost_all, opcost_cluster_pondere, opcost_diff, opcost_rel)
    # Creating vector LOLD
    lold_all <- mean(antaresDataList_areas$`LOLD`)
    lold_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`LOLD`
    lold_cluster_pondere <- sum(lold_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    lold_diff <- lold_all - lold_cluster_pondere
    lold_rel <- (lold_all - lold_cluster_pondere)*100/lold_all
    lold <- c(lold_all, lold_cluster_pondere, lold_diff, lold_rel)
    # Creating vector UNSP. ENRG
    unsp_all <- mean(antaresDataList_areas$`UNSP. ENRG`)
    unsp_cluster <- antaresDataList_areas[mcYear %in% clusterList$`Selected years`,]$`UNSP. ENRG`
    unsp_cluster_pondere <- sum(unsp_cluster * clusterList$Weighting)/max(antaresDataList_areas$mcYear)
    unsp_diff <- unsp_all - unsp_cluster_pondere
    unsp_rel <- (unsp_all - unsp_cluster_pondere)*100/unsp_all
    unsp <- c(unsp_all, unsp_cluster_pondere, unsp_diff, unsp_rel)
    # Creating final data.table
    costTable <- data.table("NAME" = name, "ANNUAL LOAD (W)" = load, "OP. COST (euros)" = opcost, "LOLD (hours)" = lold, "UNSP. ENRG (MWh)" = unsp)
    return(costTable)
  }
  
  
  
  
  
  ##### DATA PROCESSING ON THE MAIN AREAS #####
    
  # Reading Antares data on the main areas
  cat("Processing data from the main areas :", mainAreas, ":\n")
  data_etude_main <- readAntares(areas = mainAreas, clusters = mainAreas,  mcYears = MCYears, thermalAvailabilities = subtractNuclearAvailabilityMain, select = c("LOAD", "OP. COST","LOLD", "UNSP. ENRG", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND", "SOLAR"))
  
  # Extracting the column of real load data (before subtracting renawables) in order to compare values after the clustering
  data_load_main <- extractLoad(data_etude_main$areas)
  
  # Defining the net load
  if (subtractUndispatchableEnergyMain == TRUE) {
    data_etude_main$areas <- addNetLoadnoMustRun(data_etude_main$areas)
  }
  if (subtractNuclearAvailabilityMain == TRUE) {
    data_etude_main <- subtractNucAvailability(data_etude_main, mainAreas)
  } 
  
  # Creation the load matrix on the main areas
  matrix_conso_main <- loadDurationMatrix(data_etude_main$areas)
  
  # Creation of the reference load matrix on the main areas
  complete_conso_main <- completeLoadDuration(matrix_conso_main)
  
  # Creation the distance vector between every load duration curve and the reference one on the main areas
  l3_dist_main <- l3DistanceRef(matrix_conso_main, complete_conso_main)
  
  # Creation the distance vector between every load duration curve and the reference one on the peak period on the main areas
  l3_peak_dist_main <- l3PeakDistanceRef(matrix_conso_main, complete_conso_main)
  
 
  
  
  
  
  ##### DATA PROCESSING ON THE EXTRA AREAS #####

  if (is.null(extraAreas) == FALSE) {
    
    # Reading Antares data on the extra areas
    cat("\nProcessing data from the extra areas :", extraAreas, ":\n")
    data_etude_extra <- readAntares(areas = extraAreas, mcYears = MCYears, thermalAvailabilities = subtractNuclearAvailabilityExtra, select = c("LOAD", "OP. COST","LOLD", "UNSP. ENRG", "ROW BAL.", "PSP", "MISC. NDG", "H. ROR", "WIND", "SOLAR"))
    
    # Defining the net load
    if (subtractUndispatchableEnergyExtra == TRUE) {
      data_etude_extra <- addNetLoadnoMustRun(data_etude_extra)
    }
    if (subtractNuclearAvailabilityExtra == TRUE) {
      data_etude_extra <- subtractNucAvailability(data_etude_extra, extraAreas)
    }
    
    # Creation the load matrix on the extra areas
    matrix_conso_extra <- loadDurationMatrix(data_etude_extra)
    
    # Creation of the reference load matrix on the extra areas
    complete_conso_extra <- completeLoadDuration(matrix_conso_extra)
    
    # Creation the distance vector between every load duration curve and the reference one on the extra areas
    l3_dist_extra <- l3DistanceRef(matrix_conso_extra, complete_conso_extra)
    
    # Creation the distance vector between every load duration curve and the reference one on the peak period on the extra areas
    l3_peak_dist_extra <- l3PeakDistanceRef(matrix_conso_extra, complete_conso_extra)
  }  
    

  
  
  
  
  
  ##### AGGREGATING BOTH AREAS DATA AND CLUSTERING METHOD #####
  
  # Creation a scaled data.frame containing every indicators (/!\ mind the order of the indicators)
  if (is.null(extraAreas) == FALSE) {
    cluster_indicators <- aggregateIndicators(l3_dist_main, l3_peak_dist_main, l3_dist_extra, l3_peak_dist_extra)
  } else {
    cluster_indicators <- aggregateIndicators(l3_dist_main, l3_peak_dist_main)
  }
   
  # K-medoids algorithm
  kmed_data <- pam(cluster_indicators, selection)
  info_clusters <- data.table("Selected years" = kmed_data$id.med, "Weighting" = kmed_data$clusinfo[,"size"])
  info_clusters <- arrange(info_clusters, `Selected years`)
  
  
  
  
  
  ##### ANALYSING RESULTS THROUGH LOAD DURATION CURVES #####
  
  if (displayCurves == TRUE) {
    # Creation of the load duration matrix of the selected years after clustering on the main areas
    matrix_conso_clusters_main <- matrix_conso_main[,info_clusters$`Selected years`]
    
    # Creation of the general load duration matrix of the selected years after clustering (re-sampled) on the main areas
    complete_conso_clusters_main <- completeLoadMonotonous(matrix_conso_main[, rep(info_clusters$`Selected years`, info_clusters$Weighting)])
    
    # Displaying curves of the main areas
    plotMonotonous(title = paste(c("Load monotonous on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main)
    
    # Displaying curves on the peak period of the main areas
    plotMonotonous(title = paste(c("Load peak on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main, x_lim=c(0,60), y_lim=c(mean(matrix_conso_main),max(matrix_conso_main)*1.05))
    
    # Displaying curves on the trough period of the main areas
    plotMonotonous(title = paste(c("Load troughs on main areas :", mainAreas), collapse = " "), matrix_conso_main, matrix_conso_clusters_main, complete_conso_main, complete_conso_clusters_main, x_lim = c(8680,8740), y_lim = c(min(matrix_conso_main)*1.05,mean(matrix_conso_main)))
    
    if(is.null(extraAreas) == FALSE) {
      # Creation of the load duration matrix of the selected years after clustering on the main areas
      matrix_conso_clusters_extra <- matrix_conso_extra[,info_clusters$`Selected years`]
      
      # Creation of the general load duration matrix of the selected years after clustering (re-sampled) on the extra areas
      complete_conso_clusters_extra <- completeLoadMonotonous(matrix_conso_extra[, rep(info_clusters$`Selected years`, info_clusters$Weighting)])
      
      # Displaying curves of the main areas
      plotMonotonous(title = paste(c("Load monotonous on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra)
      
      # Displaying curves on the peak period of the main areas
       plotMonotonous(title = paste(c("Load peak on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra, x_lim = c(0,60), y_lim = c(mean(matrix_conso_extra),max(matrix_conso_extra)*1.05))
      
      # Displaying curves on the trough period of the main areas
      plotMonotonous(title = paste(c("Load troughs on extra areas :", extraAreas), collapse = " "), matrix_conso_extra, matrix_conso_clusters_extra, complete_conso_extra, complete_conso_clusters_extra, x_lim = c(8680,8740), y_lim = c(min(matrix_conso_extra)*1.05,mean(matrix_conso_extra)))
    }
  }
  
  
  
  
  
  
  
  
  ##### ANALYSING RESULTS THROUGH KEY DATA #####
  if (displayTable == TRUE) {
    # Creating a table to compare key values for the selected years
    data_comparison_main <- costAnalysis(data_etude_main$areas, data_load_main, info_clusters)
    
    # Printing the table
    cat("\n Cost analysis on the main areas :", mainAreas, ":\n")
    print(data_comparison_main)
    cat("\n")
  }
  
  
  
  
  
  ##### FIN DE FONCTION #####
  return(info_clusters)
  
}