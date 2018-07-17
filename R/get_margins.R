#' Compute generation margin in a given area on an Antares Study.
#' 
#'   - A positive generation margin means that the generation fleet of the area is over-sized 
#'   and the LOLD (Loss Of Load Duration) is below the adequacy criteria (typically 3h/y). In that
#'   case, the margin corresponds to the extra constant load that the generation fleet can still supply while
#'   respecting strictly the adequacy criteria.
#'   
#'   - A negative generation margin means that the generation fleet is under-sized and the LOLD
#'   is above the adequacy criteria. In that case, the margin corresponds to the extra "perfect" production
#'   capacity required to meet the adequacy criteria. A "perfect" capacity is a capacity which is available
#'   all the time (i.e. without maintenance/outages).
#' 
#' @param area
#'   Area in which the margin is computed
#' @param cluster_name
#'   Name of the complementary cluster if the margin is negative (lack of production).
#'   The cluster can already exist, otherwise it will be created.
#'   (example : If cluster_name = "gas_pcomp_peak" and area = "fr", the final name of the cluster will be "fr_gas_pcomp_peak")
#' @param LOLD
#'   Adequacy criteria of the area in terms of Loss of Load Duration (given in h/year) 
#' @param tolerance
#'    Algorithmic tolerance about the adequacy criteria. Used as stop criterion. 
#'   (example : if tolerance = 0.5 and LOLD = 3.0, the aimed range of LOLD is between 2h30 and 3h30)
#' @param unit_size
#'   Minimal step of the generation margin. Given in MW.
#' @param abaque
#'   Function abaque : marge = f(LOLE)
#' @param initial_margin
#'   First margin tested in the function. Given in MW.
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param display
#'   Logical. If \code{TRUE} the advancement of the function
#'   if displayed in the console
#' @param clean
#'   Logical. If \code{TRUE} the output of the ANTARES simulations run by the
#'   package will be deleted (except for the output of the simulation which brings
#'   to the best solution).
#' @param parallel
#'   Logical. If \code{TRUE} the ANTARES simulations will be run in parallel mode (Work
#'   only with ANTARES v6.0.0 or more). In that case, the number of cores used by the simulation
#'   is the one set in advanced_settings/simulation_cores (see ANTARES interface).
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions setSimulationPath getAreas readClusterDesc readInputTS
#' @importFrom rmarkdown render
#' @importFrom utils packageVersion tail
#' @importFrom antaresEditObject setPlaylist getPlaylist createCluster writeIni 
#' @export
#' 

get_margins <- function(area = "fr", cluster_name = "gas_pcomp_peak", LOLD = 3.00, tolerance = 0.01,  unit_size = 100,
                        abaque = function(c){return(-3627*log(c)+3723.4)}, initial_margin = 0, path_solver, display = TRUE, clean = TRUE, 
                        parallel = TRUE, opts = antaresRead::simOptions())
{
  # ---- 0. initialize  ----
  
  LOLE <- LOLD
  # reload study
  opts <- setSimulationPath(opts$studyPath, simulation = 0)

  # save current settings of the ANTARES study into a temporary file
  save_general_settings(opts)
  # reset options of the ANTARES study to their initial values when the function ends
  on.exit(restore_general_settings(opts))
  
  
  #    set week and initial day
  #    we need to ensure the consistency between the weekly optimisation and the weekly
  #    aggregation of the output
  
  month_name <- c("january", "december", "november", "october", "september", "august", "july", "june", "may", "april", "march", "february")
  day_per_month <- c(0, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 28)
  
  day_name <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  if (opts$parameters$general$leapyear)
  {
    day_per_month[12] <- 29
  }
  month_id <- which(month_name == opts$parameters$general$"first-month-in-year")
  assertthat::assert_that(length(month_id) == 1)
  n_day <- (-sum(day_per_month[1:month_id]) + opts$parameters$general$simulation.start - 1) %% 7
  
  first_day_week <- day_name[((which(day_name == opts$parameters$general$january.1st) + n_day - 1) %% 7 ) +1]
  antaresEditObject::updateGeneralSettings(first.weekday = first_day_week)
  
  # adjust abacus
  abaque_t <- abaque
  a_0 <- uniroot(abaque_t, interval = c(0.01, 10))$root 
  abaque <- function(x){abaque_t(x * a_0 / LOLE)}
  
  # initiate a few parameters
  current_it <- 0
  antaresEditObject::updateGeneralSettings(filtering = "true", year.by.year = "true", opts = opts)
  filter_output_areas(areas = antaresRead::getAreas(opts = opts), filter = c("annual", "weekly", "hourly"), type = c("year-by-year","synthesis"), opts = opts)
  filter_output_links(links = antaresRead::getLinks(opts = opts), filter = c("annual"), type = c("synthesis"), opts = opts)
  unique_key <- paste(sample(c(0:9, letters), size = 3, replace = TRUE),collapse = "") # unique key used in output names
  rowbalance_file_name <- paste0(opts$inputPath, "/misc-gen/miscgen-", area, ".txt", sep = "")
  assertthat::assert_that(file.exists(rowbalance_file_name))
  loop <- FALSE
  if (file.info(rowbalance_file_name)$size != 0)
  {
    param_data <- read.table(rowbalance_file_name)
    initial_row_balance <- param_data[, 8]  
    if (abs(sum(initial_row_balance)) > 0) 
    {
      cat("Warning : initial row balance of the area ", area," is not null ! \n", sep="") 
    }
  }
  else {initial_row_balance <- rep(0,8760)}
  list_margin <- data.frame(row.names = c("it", "margin", "LOLE"))
  
  if(display){cat("------ Iteration 0 ------\n", sep="")}
  
  # setplaylist and weeks
  first_sim_week <- 1 + ceiling((opts$parameters$general$simulation.start - 1)/7)
  n_w <- floor((opts$parameters$general$simulation.end - opts$parameters$general$simulation.start + 1)/7) # number of weeks 
  weeks <- initial_weeks <- first_sim_week:(first_sim_week + n_w - 1) # identifier of weeks to simulate for all expansion planning optimisation
  playlist <- initial_playlist <- getPlaylist(opts) # identifier of mc years to simulate for all expansion planning optimisation

  # set margin 
  set_margins_in_antares(margin = initial_margin, area, cluster_name, row_balance_init = initial_row_balance, opts = antaresRead::simOptions())

    
   # ---- 1. Simulate with Antares once at the beginning : ---- 

  simulation_name <- paste0("get-margins-", unique_key, "-it", current_it, "-margin_", initial_margin,"mw")
  if(display){  cat("   ANTARES simulation running ", sep="")}
  if(display){  cat("[",length(weeks)*length(playlist)," simulated weeks] ... ", sep = "")}
  run_simulation(simulation_name, mode = "economy",
                 path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
  if(display){  cat("[done] \n", sep="")}
  
  output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
  output_LOLE    <- antaresRead::readAntares(areas = area, links = NULL, mcYears = "all", 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = c("LOLD"))

  
  # ---- 2. Define first LOLE : ----   
  new_LOLE <- mean(output_LOLE$LOLD)
  if(display){ cat("   margin = ", initial_margin, " MW\n", sep="")}
  if(display){ cat("   LOLD = ", new_LOLE ," h\n", sep="")}

  list_margin <- rbind(list_margin,
                       data.frame(it = current_it,
                                  margin = initial_margin,
                                  LOLE = new_LOLE))
  
  # if LOLE is above the target, report years and weeks which acutally have unsupplied energy
  if(new_LOLE > LOLE)
  {
    ooo <- get_years_and_weeks_with_LOLD(area = area, opts = output_antares)
    playlist <- intersect(playlist, ooo$mc_years)
    weeks <- intersect(weeks, ooo$weeks)
  }
  
  # No zero with log !
  new_LOLE_init <- new_LOLE
  current_it <- 1
  if(new_LOLE >= LOLE-tolerance & new_LOLE <= LOLE + tolerance )
  {
    if(display){ cat("The area ", area, " is already balanced with margin = ", initial_margin,"  MW \n")}
  }
  
 
  
  
   # ---- 3. Evaluate the fisrt margin with the abacus : ----   
  else
  {
    if (new_LOLE==0){margin  <- 10000}
    else if (new_LOLE > LOLE) {margin <- abaque(new_LOLE)}
    else if (new_LOLE < LOLE) {margin <- margin * 1.15}
    margin <- initial_margin + margin
    margin <- floor(margin/unit_size)*unit_size
    if(display){ cat("   First margin evaluated with the abacus : ", margin," MW\n", sep="")}

    
    
  # ---- 5. iterate until convergence : ----  
    

  has_converged <- FALSE
  while(!has_converged)
  {
    if(display){cat("\n------ Iteration ", current_it, " ------\n", sep="")}
    
    set_margins_in_antares(margin, area, cluster_name, row_balance_init = initial_row_balance, opts = antaresRead::simOptions()) 
    
    # ----------- change playlist and simulated weeks -------------
    
    set_simulation_period(weeks, opts)
    antaresEditObject::setPlaylist(playlist, opts)
    

    # ----------- Simulate ----------- 
    antaresEditObject::updateGeneralSettings(filtering = "true", year.by.year = "true", opts = opts)
    simulation_name <- paste0("get-margins-", unique_key, "-it", current_it, "-margin_", margin, "mw")
    if(display){  cat("   ANTARES simulation running ", sep="")}
    if(display){  cat("[",length(weeks)*length(playlist)," simulated weeks] ... ", sep = "")}
    
    run_simulation(simulation_name, mode = "economy",
                   path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
    if(display){  cat("[done] \n", sep="")}
    
    output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
    output_LOLE    <- antaresRead::readAntares(areas = area, links = NULL, mcYears = "all", 
                                               timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                               select = c("LOLD"))

    new_LOLE <- sum(output_LOLE$LOLD)/length(initial_playlist)
    
    if(display){ cat("   margin = " ,margin , " MW\n", sep="" )}
    if(display){ cat("   LOLD = " ,new_LOLE , " h\n", sep="" )}
    list_margin <- rbind(list_margin,
                         data.frame(it = current_it,
                                    margin = margin,
                                    LOLE = new_LOLE))
  
    # ---- update step ----
    
    # if(min(abs(new_LOLE - LOLE + tolerance), abs(new_LOLE - LOLE - tolerance))>1.0){ step <- 8*unit_size}
    # else{
    #   if(min(abs(new_LOLE - LOLE + tolerance), abs(new_LOLE - LOLE - tolerance))>0.75){step <- 4*unit_size}
    #   else{
    #     if(min(abs(new_LOLE - LOLE + tolerance), abs(new_LOLE - LOLE - tolerance))>0.5){step <- 2*unit_size}
    #     else{step <- unit_size}
    #   }
    # }
    
    if (new_LOLE==0){step <- 10000}
    else {step <- abaque(new_LOLE)}
    if(step > 0 && new_LOLE > LOLE){step <- -step}
    if(step < 0 && new_LOLE < LOLE){step <- -step}
    if(abs(step) < unit_size) {step <- sign(step)*unit_size}
    else { step <- round(step/unit_size)*unit_size  }      

    
    # ----------- Update : Lack of production ----------- 
    if (new_LOLE > LOLE+tolerance)
    { 
      margin <- margin  + step
      # If we had already had this margin previously
      if(!is.null(list_margin$margin[list_margin$margin < margin - step]))
      {
      if (any(margin <= list_margin$margin[list_margin$margin < margin - step])) {
        loop <- TRUE
        # Get the nearest LOLE of the aimed range
        ind_inf <- which(list_margin$LOLE< LOLE - tolerance)
        ind_inf <- ind_inf[which.max(list_margin$margin[ind_inf])]
        ind_sup <-  which(list_margin$LOLE> LOLE + tolerance)
        ind_sup <- ind_sup[which.min(list_margin$margin[ind_sup])]
        if(abs(list_margin$margin[ind_inf]-list_margin$margin[ind_sup])==unit_size)
        {
          if(display){ cat("\n Units of ",unit_size," MW are too big to adapt to the tolerance \n")}
          has_converged <- TRUE
          if(display){ cat("Convergence with margin = ",list_margin$margin[ind_inf]," MW or margin = ",list_margin$margin[ind_sup], " MW \n",  sep="")}
        }
        else{
          margin <- floor((list_margin$margin[ind_inf]+list_margin$margin[ind_sup])/2/unit_size)*unit_size
        }
      }
      }

    }
  
    # ----------- Update : Lack of consumption
    else if (new_LOLE < LOLE - tolerance)
    {
      margin <- margin  + step
      # If we had already had this margin previously
      if(!is.null(list_margin$margin[list_margin$margin > margin - step]))
      {
        if (any(margin >= list_margin$margin[list_margin$margin > margin - step])) {
          loop <- TRUE
        # Get the nearest LOLE of the aimed range
        ind_inf <- which(list_margin$LOLE< LOLE - tolerance)
        ind_inf <- ind_inf[which.max(list_margin$margin[ind_inf])]
        ind_sup <-  which(list_margin$LOLE> LOLE + tolerance)
        ind_sup <- ind_sup[which.min(list_margin$margin[ind_sup])]
        if(abs(list_margin$margin[ind_inf]-list_margin$margin[ind_sup])==unit_size)
        {
          if(display){ cat("\n Units of ",unit_size, " MW are too big to adapt to the tolerance \n")}
          has_converged <- TRUE
          if(display){ cat("Convergence with margin = ",list_margin$margin[ind_inf]," MW or margin = ",list_margin$margin[ind_sup], " MW \n",  sep="")}
        }
        else{
          margin <- floor((list_margin$margin[ind_inf]+list_margin$margin[ind_sup])/2/unit_size)*unit_size
        }
        }
      }       
    }
    else
    { 
      if(display){ cat("\n --- Area ", area, " has been balanced with margin = ", margin, " MW \n", sep = "")}
      has_converged <- TRUE        
    }
    
    # if LOLE is above the target, report years and weeks which acutally have unsupplied energy
    if(new_LOLE > LOLE)
    {
      ooo <- get_years_and_weeks_with_LOLD(area, output_antares)
      playlist <- intersect(playlist, ooo$mc_years)
      weeks <- intersect(weeks, ooo$weeks)
    }
    
    #---- 9. Clean ANTARES output ----
    if(clean) { clean_output_benders(current_it, unique_key, output_name = "get-margins-", opts)}
     
    current_it<-current_it+1
  
  
  }
    if(display){ cat("\n Summary :  \n")}
    if(display){ print(list_margin)}
  }
}



#' Change the input of an Antares study with a given margin (positive or negative value 
#' in MW) to be set in one area.
#'   - if the margin is positive (implicitly, the generation fleet is over-sized and the LOLE 
#'   is below the adequacy criteria), ROW BALANCE is removed from the given area. In that case,
#'   ROW BALANCE = row_balance_init - margin.
#'   - if the margin is negative (implicitly, the generation fleet is under-sized and the LOLE
#'   is above the adequacy criteria), a cluster is enabled and its installed capacity is set to
#'   the amount of the margin. The cluster is "perfect", i.e. without any outages or maintenance
#'   stops.
#' 
#'
#' @param margin
#'   given margin (positive or negative) to set in an Antares study.
#' @param area
#'   area in which this margin should be set
#' @param cluster_name
#'   Name of the complementary cluster if the margin is negative (lack of production).
#'   The cluster can already exist, otherwise it will be created.
#'   Example : if cluster_name = "gas_pcomp_peak" and area = "fr", the final name of the cluster will be "fr_gas_pcomp_peak".
#' @param row_balance_init
#'   Initial row_balance in the given area. Vector of size 8760 or 1 (if constant).
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Nothing. Input file of the antares study are therefore modified.
#' 
#' @noRd
#' @importFrom antaresRead readClusterDesc simOptions
#' @importFrom antaresEditObject createCluster readIniFile writeIni
#' @importFrom utils modifyList read.table write.table
#' @importFrom assertthat assert_that
#' 
#' 
#' 
set_margins_in_antares <- function(margin, area, cluster_name, row_balance_init, opts = antaresRead::simOptions())
{
  # 1.check cluster existence
  list_cluster <- antaresRead::readClusterDesc(opts = opts)
  list_cluster <- list_cluster[area == area]
  extended_cluster_name <- paste(area, cluster_name, sep = "_")
  
  cluster_existence <- extended_cluster_name %in% list_cluster$cluster
  
  if(!cluster_existence)
  {
    # create the cluster 
    antaresEditObject::createCluster(area = area, 
                                     cluster_name = cluster_name,
                                     group = "gas",
                                     unitcount = as.integer(1),
                                     add_prefix = TRUE,
                                     nominalcapacity = unit_size,
                                     enabled = FALSE,
                                     `marginal-cost` = 300,
                                     `spread-cost` = 0,
                                    `market-bid-cost` = 300,
                                     time_series = rep(0,8760),
                                     opts = opts)
    warning("As it was not already present in the study, the cluster ", area, "_", cluster_name, " has been automatically created with default values (e.g. cost = 300€/MWh).")
  }
   
  
  
  # 2. If margins >=0, disable cluster + modify ROW Balance
  
  if (margin >= 0)
  {
    # disable cluster
    path_clusters_ini <- file.path(opts$inputPath, "thermal", "clusters", area, "list.ini")
    previous_params <- antaresEditObject::readIniFile(file = path_clusters_ini)
    params_cluster <- list(enabled = "false")
    ind_cluster <- which(tolower(names(previous_params)) %in% tolower(extended_cluster_name))[1]
    previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
    antaresEditObject::writeIni(listData = previous_params,pathIni = path_clusters_ini,overwrite = TRUE)
    
    # modify ROW Balance
    update_row_balance(row_balance_init - margin, area = area, opts = opts)
    
  }
  
  # 3. if margins <0, enable cluster with good capacity + desactivate ROW Balance
  else if (margin < 0)
  {
    # enable cluster and put its capacity equal to the margin
    path_clusters_ini <- file.path(opts$inputPath, "thermal", "clusters", area, "list.ini")
    previous_params <- antaresEditObject::readIniFile(file = path_clusters_ini)
    params_cluster <- list(enabled = "true", unitcount = as.integer(1), nominalcapacity = - margin)
    ind_cluster <- which(tolower(names(previous_params)) %in% tolower(extended_cluster_name))[1]
    previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
    antaresEditObject::writeIni(listData = previous_params,pathIni = path_clusters_ini,overwrite = TRUE)
    
    # modify the time series of this cluster
    ts_file_name <- paste0(opts$inputPath,"/thermal/series/" ,area,"/", extended_cluster_name,"/","series.txt",sep="")
    assertthat::assert_that(file.exists(ts_file_name))
    utils::write.table(rep(-margin, 8760),ts_file_name,sep = "\t",col.names = FALSE,row.names = FALSE)
    
    # reset ROW Balance
    update_row_balance(row_balance_init, area = area, opts = opts)
    
  }
}




#' update ROW Balance with new given value
#' 
#' @param row_balance
#'   New row_balance. Vector of size 8760 or 1 (if constant).
#' @param area
#'   area in which this margin should be set
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' Nothing. Input file of the antares study are therefore modified.
#' 
#' @noRd
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject createCluster readIniFile writeIni
#' @importFrom utils read.table write.table
#' @importFrom assertthat assert_that
update_row_balance <- function(row_balance, area, opts = antaresRead::simOptions())
{

  assertthat::assert_that(length(row_balance) %in% c(1, 8760))
  
  
  rowbalance_file_name <- paste0(opts$inputPath,"/misc-gen/miscgen-",area,".txt",sep="")
  assertthat::assert_that(file.exists(rowbalance_file_name))
  
  if (file.info(rowbalance_file_name)$size != 0)
  {
    # read file
    param_data <- utils::read.table(rowbalance_file_name)
    
    # update column with new value and write it 
    param_data[,8] = row_balance
    utils::write.table(param_data, rowbalance_file_name, sep="\t", col.names = FALSE, row.names = FALSE)
  }
  else if (file.info(rowbalance_file_name)$size ==0)
  {
    # The file exists but is empty : i.e. all column contains default value
    # file is built from scratch
    param_data <- as.table(matrix(0,8760,8))
    
    # update column with new value and write it 
    param_data[,8] = row_balance
    utils::write.table(param_data, link_file_name, sep="\t", col.names = FALSE, row.names = FALSE)
  }      
}



#' Returns list of mc_years and weeks with LOLD
#'
#' @param area
#'   area in which LOLD is analyzed
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return 
#' A list with two vectors, one with the MC years with LOLD and 
#' on with the weeks with LOLD.
#' 
#' @noRd
#' @importFrom antaresRead simOptions readAntares
get_years_and_weeks_with_LOLD <- function(area, opts = antaresRead::simOptions())
{
   LOLD <- antaresRead::readAntares(areas = area, links = NULL, mcYears = "all", 
                           timeStep = "weekly", opts = opts, showProgress = FALSE,
                           select = c("LOLD"))
  
   # get mc-years and weeks with LOLD
   mc_years <- unique(LOLD[LOLD > 0, mcYear])
   weeks <- unique(LOLD[LOLD > 0, timeId])
   
   
   # look for consecutive weeks
   c_weeks <- min(weeks):max(weeks)
   
   # build output
   out <- list()
   out$mc_years <- mc_years
   out$weeks <- c_weeks
   return(out)
}


