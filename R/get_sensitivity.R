#'  This function examines how the optimal installed capacities of an ANTARES study will be impacted by solutions in the neighborhood of the optimal cost.
#' 
#'   - Launch this function after you launched benders Xpansion a first time
#'   - The range given for each candidate means that alternate solution of installed capacity are include in this range.
#' 

#' @param epsilon
#'   Epsilon (need to be taller than the optimality gap)
#' @param display
#'   Logical. If \code{TRUE} the advancement of the function
#'   if displayed in the console
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @return 
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions setSimulationPath getAreas readClusterDesc readInputTS
#' @importFrom utils packageVersion tail
#' @export

get_sensitivity <- function(epsilon = 1000000, opts = antaresRead::simOptions())
{

  # ---- 0. Assert that a benders calculation has been previously perform :  ----  
  tmp_folder <- paste(opts$studyPath,"/user/expansion/temp",sep="")   # temporary folder
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/data_for_report.RDS")))
  assertthat::assert_that(file.exists(paste0(tmp_folder, "/out_underestimator.txt")))
  assertthat::assert_that(file.info(paste0(tmp_folder, "/out_underestimator.txt"))$size !=0)
  
  
  # ---- 1. Print informations of the last benders calculation :  ----
  benders_out <- readRDS(file = paste0(tmp_folder, "/data_for_report.RDS"))
  cat("\n Sentivity analysis performs on this benders Xpansion study :\n", sep="")
  cat(" - Study : ",benders_out$study_options$studyPath, "\n", sep="")
  date_rds <- file.mtime(path = paste0(tmp_folder, "/data_for_report.RDS"))
  cat(" - Date of simulation : ", as.character(date_rds), "\n", sep="")
  cat(" - Number of iterations : ",length(benders_out$iterations), "\n", sep="")
  best_it <- benders_out$costs[which(benders_out$costs$overall_costs == min(benders_out$costs$overall_costs, na.rm = TRUE)),"it"]
  best_it <- best_it[1]
  ubcost_init <- unlist(read.table(paste0(tmp_folder,"/in_ubcosts.txt"), header = FALSE))
  max_underestimator <- max(unname(unlist(read.table(paste0(tmp_folder,"/out_underestimator.txt"), header = FALSE))))
  cat(" - Optimality gap : ", ubcost_init - max_underestimator, " euros \n" , sep="")
  cat(" - Best solution : \n", sep="")
  print(benders_out$invested_capacities[which(benders_out$invested_capacities$it == best_it),])

  
  # ---- 2. Clean the file "in_out_capacitybounds.txt" :  ----
  write(c(), file = paste0(tmp_folder, "/in_out_capacitybounds.txt"), append = FALSE )
  
  
  # ---- 3. Change option of the master:  ---- 
  change_option_master(option_name = c("solve_bounds","solve_master"), option_value = c(1,0), opts = opts)
  
  
  # ---- 4. Change the value of epsilon :  ---- 
  change_option_master(option_name = "epsilon", option_value = epsilon, opts = opts)

  
  # ---- 5. Check if optimality gap is negative :  ----  
  if(ubcost_init - max_underestimator < 0 ){
    write(max_underestimator, file = paste0(tmp_folder, "/in_ubcosts.txt"), append = FALSE )
  }
  
  
  # ---- 6. Solve bounds problem :  ----  
  if(benders_out$expansion_options$master == "relaxed") relaxed_p <- TRUE
  else relaxed_p <- FALSE
  solve_master(opts = opts, relax_integrality = relaxed_p)
  sensitivity <-  read.table(paste0(tmp_folder,"/in_out_capacitybounds.txt"), sep =" ", col.names = c("candidate", "min_capacity", "max_capacity"))
 
  write(ubcost_init, file = paste0(tmp_folder, "/in_ubcosts.txt"), append = FALSE )

  # ---- 7. Return results :  ----  
  cat(" \n Sensitivity analysis for epsilon = ",epsilon," euros : \n", sep="")    
  return(sensitivity)
}