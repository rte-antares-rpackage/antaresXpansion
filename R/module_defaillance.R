#' Launch LOLE module
#' 
#'
#' @param area
#'   area 
#' @param LOLE
#'   LOLE 
#' @param tolerance
#'   tolerance (example : if tolerance = 0.5 and LOLE = 3.0, the aimed range of LOLE is between 2h30 and 3h30 )
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param display
#'   Logical. If \code{TRUE} the advancement of the defaillance module
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
#' @param abaque
#'   Function abaque : marge = f(LOLE)
#' @param cluster_name
#'   Name of the complementary cluster if the margin is negative (lack of production).
#'   The cluster can already exist, otherwise it will be created.
#'   If cluster_name = "gas_pcomp_peak" and area = "fr", the final name of the cluster will be "fr_gas_pcomp_peak".
#'   
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
#' 
#' 
#' 
#' tolerance
get_margins <- function(area = "fr", LOLE = 3.00, tolerance = 0.5, path_solver, display = TRUE, clean = TRUE, parallel = TRUE, opts = antaresRead::simOptions(), abaque = function(c){return(-3627*log(c)+3723.4)}, cluster_name = "gas_pcomp_peak" )
{
  # ---- 0. initialize  ----
  

  # save current settings of the ANTARES study into a temporary file
  save_general_settings(opts)
  # reset options of the ANTARES study to their initial values when the function ends
  on.exit(restore_general_settings(opts))
  
  
  # initiate a few parameters
  current_it <- 1
  antaresEditObject::updateGeneralSettings(filtering = "true", year.by.year = "true", opts = opts)
  filter_output_areas(areas = antaresRead::getAreas(opts = opts), filter = c("annual"), type = c("year-by-year","synthesis"), opts = opts)
  filter_output_links(links = antaresRead::getLinks(opts = opts), filter = c("annual"), type = c("synthesis"), opts = opts)

  # ---- 1. Check that the cluster already exists : ----
  # Baptiste : c'est maintenant fait dans set_margins_in_antares
  
  # list_cluster <- antaresRead::readClusterDesc()
  # list_cluster <- list_cluster[area == area ]
  # clustername <- paste(area, cluster_name, sep = "_")
  # cluster_existence <- clustername %in% list_cluster$cluster
  # if(display){cat("------ INITIALIZATION ------\n", sep="")}
  # cat("The cluster already exits : ", cluster_existence," (thermal series must be ready-made !) \n", sep="")
  # if (cluster_existence)
  # {
  #   # On vérifie que le cluster est à Enabled = FALSE :
  #   path_clusters_ini <- file.path(opts$inputPath, "thermal", "clusters", area, "list.ini")
  #   previous_params <- antaresEditObject::readIniFile(file = path_clusters_ini)
  #   params_cluster <- list(enabled = "false")
  #   ind_cluster <- which(tolower(names(previous_params)) %in% tolower(clustername))[1]
  #   previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
  #   antaresEditObject::writeIni(listData = previous_params,pathIni = path_clusters_ini,overwrite = TRUE)  
  # }
  
  initial_row_balance <- antaresRead::readInputTS(misc = area)$ROW_Balance
  # AJOUTER warning if(any(initial_row_balance) != 0)
  
  
  set_margins_in_antares(margin = 0, ...)
  
   # ---- 1. Simulate with Antares once at the beginning : ---- 

  cat("If the cluster already exists, the parameter will be initialized to ENABLED = FALSE at the beginning.\n")
  simulation_name <- paste0("get-margins", "-", "0")
  if(display){  cat("   ANTARES simulation running ... ", sep="")}
  run_simulation(simulation_name, mode = "economy",
                 path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
  if(display){  cat("[done] \n", sep="")}
  
  output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
  output_LOLE    <- antaresRead::readAntares(areas = area, links = NULL, mcYears = "all", 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = c("LOLD"))

  
  # ---- 2. Define first LOLE : ----   
  new_LOLE <- mean(output_LOLE$LOLD)
  cat("      Initial LOLE : ", new_LOLE ,"\n", sep="")
  # No zero with log !
  if (new_LOLE==0){new_LOLE  <- 0.001}
  new_LOLE_init <- new_LOLE
  
  if(new_LOLE >= LOLE-tolerance & new_LOLE <= LOLE + tolerance )
  {
    cat("The range is already satisfied. \n")
  }
  
 
  
  
   # ---- 3. Evaluate the fisrt margin with the abacus : ----   
  else
  {
    new_value <- abaque(new_LOLE)
    cat("      First margin evaluated with the abacus : ", new_value," MW.\n", sep="")
    new_value_init <- new_value

    
    
  # ---- 5. iterate until convergence : ----  
    

  has_converged <- FALSE
  while(!has_converged)
  {
    if(display){cat("\n------ ITERATION ", current_it,"------\n", sep="")}
    
    # -----------  In case the margin is negative (lack of production) :----------- 
    if (new_LOLE > LOLE+tolerance)
    {
      nb_unit <- floor( -1 *new_value/100)
      new_value <-  nb_unit*100  # (positive)
      if(current_it == 1){last_margin <-new_value }else{last_margin <-100}
      

      # # write the series of thermal availability :
      # n_mc <- opts$parameters$general$nbyears
      # param_data <- as.table(matrix(0, 8760, n_mc))
      # param_data[, 1:n_mc] = new_value
      # 
      
      
      
    #   if (cluster_existence)
    #   { 
    #       # update serie with new values and write it
    #       Pcomp_file_name <- paste0(opts$inputPath,"/thermal/series/" ,area,"/", clustername,"/","series.txt",sep="")
    #       assertthat::assert_that(file.exists(Pcomp_file_name))
    #       utils::write.table(param_data,Pcomp_file_name,sep = "\t",col.names = FALSE,row.names = FALSE)
    # 
    #       # On vérifie que le cluster est à Enabled = TRUE :
    #       path_clusters_ini <- file.path(opts$inputPath, "thermal", "clusters", area, "list.ini")
    #       previous_params <- antaresEditObject::readIniFile(file = path_clusters_ini)
    #       # Bug Antares when we modifie the unitCount of the cluster :
    #       params_cluster <- list(enabled = "true")
    #       ind_cluster <- which(tolower(names(previous_params)) %in% tolower(clustername))[1]
    #       previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
    #       antaresEditObject::writeIni(listData = previous_params,pathIni = path_clusters_ini,overwrite = TRUE)
    #     
    #   }
    # 
    #   else
    #   {
    #     # create the cluster 
    #     antaresEditObject::createCluster(
    #       area = area, 
    #       cluster_name = cluster_name,
    #       group = "gas",
    #       unitcount = as.integer(1),
    #       add_prefix = TRUE,
    #       nominalcapacity = 100,
    #       enabled = TRUE,
    #       `marginal-cost` = 35.760000,
    #       `spread-cost` = 0.400000,
    #       `market-bid-cost` = 35.760000,
    #       time_series = param_data)
    #   }
    # }
    
    # -----------  In case the margin is positive (lack of consumption) :----------- 
    else
    { 
      new_value <- floor(new_value/100)*100 
      new_value <- -1*new_value # (negative)
      if(current_it == 1){last_margin <-new_value }else{last_margin <-100}
      
      # #load file with miscellanous generation of the area
      # rowbalance_file_name <- paste0(opts$inputPath,"/misc-gen/miscgen-",area,".txt",sep="")
      # 
      # # check that this file exists
      # assertthat::assert_that(file.exists(rowbalance_file_name))
      # 
      # if (file.info(rowbalance_file_name)$size != 0)
      # {
      #   # read file
      #   param_data <- read.table(rowbalance_file_name)
      #   
      #   # update column with new value and write it 
      #   if(current_it == 1){rowbalance_init <-param_data[,8]}
      #   
      #   param_data[,8] = rowbalance_init + new_value
      #   utils::write.table(param_data, rowbalance_file_name, sep="\t", col.names = FALSE, row.names = FALSE)
      # }
      # else if (file.info(rowbalance_file_name)$size ==0)
      # {
      #   # The file exists but is empty : i.e. all column contains default value
      #   # file is built from scratch
      #   
      #   param_data <- as.table(matrix(0,8760,8))
      # 
      #   # update column with new value and write it 
      #   param_data[,8] = new_value
      #   utils::write.table(param_data, link_file_name, sep="\t", col.names = FALSE, row.names = FALSE)
      # }      
      # 
    }

    # ----------- Simulate ----------- 
    antaresEditObject::updateGeneralSettings(filtering = "true", year.by.year = "true", opts = opts)
    filter_output_areas(areas = antaresRead::getAreas(opts = opts), filter = c("annual"), type = c("year-by-year","synthesis"), opts = opts)
    filter_output_links(links = antaresRead::getLinks(opts = opts), filter = c("annual"), type = c("synthesis"), opts = opts)
    simulation_name <- paste0("get-margins", "-", current_it)
    if(display){  cat("   ANTARES simulation running ... ", sep="")}
    run_simulation(simulation_name, mode = "economy",
                   path_solver, wait = TRUE, show_output_on_console = FALSE, parallel = parallel, opts)
    if(display){  cat("[done] \n", sep="")}
    
    output_antares <- antaresRead::setSimulationPath(paste0(opts$studyPath, "/output/", get_whole_simulation_name(simulation_name, opts)))
    output_LOLE    <- antaresRead::readAntares(areas = area, links = NULL, mcYears = "all", 
                                               timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                               select = c("LOLD"))


    new_LOLE <- mean(output_LOLE$LOLD)
    cat("      New LOLE : " ,new_LOLE , " \n", sep="" )

    
    # ----------- Update : Lack of production ----------- 
    if (new_LOLE > LOLE+tolerance)
    { 
      # Initially in lack of production
      if(new_value_init<0)
      {
        new_value <-  new_value + 100
        cat("      New Pcomp = ", new_value, " MW \n", sep="")
        nb_unit   <-  nb_unit + 1
      }
      # Previously in lack of consumption
      else
      {   
          if(last_margin ==100)
          {
            cat("Units of 100MW are too big to adapt to the range. ")
            has_converged <- TRUE
            cat("Convergence with Pcomp = -", new_value, " MW \n")
          }
          else
          {
            cat("      Rowbalance was to big with the abacus. ")
            new_value <-  new_value/2
            cat(" New Pcomp = ",new_value," MW \n",  sep="")
            new_LOLE <- new_LOLE_init
          }
       }
    }
    # Lack of consumption
    else if (new_LOLE < LOLE - tolerance)
    {
      # # Initially in lack of consumption
      if(new_value_init > 0) #initially positive
      {
        new_value <-  new_value - 100 #negative
        cat("      New Pcomp = ",new_value, " MW  \n",  sep="")
      }      
      # Previously in lack of production
      else
      {
        if(last_margin ==100)
        {
          cat("Units of 100MW are too big to adapt to the range. ")
          has_converged <- TRUE
          cat("Convergence with Pcomp = ",new_value," MW \n",  sep="")
        }
        else
        {
          cat("      Pcomp was to big with the abacus.")
          new_value <- floor( new_value/2/100)*100         
          cat(" New Pcomp = ",new_value, " MW \n", sep="")
          nb_unit <- floor(new_value/100)
          new_LOLE <- new_LOLE_init
        }         
        
      }
    }
    else
    { 
      cat("\n --- CONVERGENCE with Pcomp = ", new_value, " MW \n", sep = "")
      has_converged <- TRUE        
    }

    new_value <- -1 * new_value

    current_it<-current_it+1
    
    
    
     #---- 9. Clean ANTARES output ----
     #if(clean) { clean_output_benders(best_solution, unique_key, opts)}
     

  
  
  
   }
    
  }
}



#' Change the input of an Antares study with a given margin (positive or negative value 
#' in MW) to be set in one area.
#'   - if the margin is positive (implicitly, the generation fleet is over-sized and the LOLE 
#'   is below the adequacy criteria), ROW BALANCE is removed from the given area. In that case,
#'   ROW BALANCE = row_balance_init - margin.
#'   - if the margin is negative (impliictly, the generation fleet is under-sized and the LOLE
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
#' @importFrom asserthat assert_that
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
                                     nominalcapacity = 100,
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
#' @importFrom asserthat assert_that
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


