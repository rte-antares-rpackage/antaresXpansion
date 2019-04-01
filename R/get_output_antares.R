#' Get antares output
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param cut_type
#'   cut_type parameter : average, yearly or weekly
#' @return 
#' vector output yearly weekly annual
#' 
#' @importFrom antaresRead readAntares 
#' @noRd


extract_output <- function(output_antares, current_it, candidates, cut_type = "average")
{  
  output_link_s     <- data.table(NULL) # mcYear "annual" with_no_profile
  output_link_h_s   <- data.table(NULL) # mcYear "hourly" with_profile_unique
  output_link_w_s   <- data.table(NULL) # mcYear "weekly" with_profile_unique
  output_link_y_s   <- data.table(NULL) # mcYear "annual" with_profile_unique
  output_link_h_s_i <- data.table(NULL) # year-by-year "hourly" with_profile_indirect
  output_link_w_s_i <- data.table(NULL) # year-by-year "weekly" with_profile_indirect 
  output_link_y_s_i <- data.table(NULL) # year-by-year "annual" with_profile_indirect


  # hourly results (for candidates with link profile only)
  if (length(with_profile(candidates)) > 0 )
  {
    
    if (length(with_profile_unique(candidates)) > 0 )
    {
      if (cut_type == "average") 
      {
        output_link_h_s <- readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = NULL, 
                                 timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                 select = "MARG. COST")
        output_link_w_s <- readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = NULL, 
                                 timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                 select = "MARG. COST")
        output_link_y_s <- readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = NULL, 
                                 timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                 select = "MARG. COST") 
      }

      else
      {        
        
        output_link_h_s <- readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = current_it$mc_years, 
                                      timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                      select = "MARG. COST")
        if (cut_type == "yearly") 
        {
          output_link_w_s <- readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = current_it$mc_years, 
                                         timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                         select = "MARG. COST")
        } 
      }
    }
    
    if (length(with_profile_indirect(candidates)) > 0 )
    {         
       output_link_h_s_i <- readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                     select = c("MARG. COST", "FLOW LIN."))  
      if (cut_type == "average") 
      {      

        output_link_w_s_i <- readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                      timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                      select = "MARG. COST")
        output_link_y_s_i <- readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                      timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                      select = "MARG. COST")        
      }
       if (cut_type == "yearly") 
       {      
         
         output_link_w_s_i <- readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                          timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                          select = "MARG. COST")
       }  
      
      
      # distinction between rentability in direct and indirect way (in "average" cut_type, it's necessary to come back to year_by_year value and averaged the rentability only at the end) :
      
      output_link_h_s_i$`MARG. COST DIRECT`<- output_link_h_s_i$"MARG. COST"
      output_link_h_s_i$sens_direct <- 0        
      output_link_h_s_i$`MARG. COST INDIRECT`<- output_link_h_s_i$"MARG. COST"
      output_link_h_s_i$sens_indirect <- 0        
      output_link_h_s_i$"MARG. COST DIRECT"[which(output_link_h_s_i$"FLOW LIN." <= 0)]= 0
      output_link_h_s_i$sens_direct[which(output_link_h_s_i$"FLOW LIN." > 0)]= 1
      output_link_h_s_i$"MARG. COST INDIRECT"[which(output_link_h_s_i$"FLOW LIN." >= 0)]= 0
      output_link_h_s_i$sens_indirect[which(output_link_h_s_i$"FLOW LIN." < 0)]= 1
      
      # when FLOW LIN == 0 : "MARG. COST" can be different from 0 if (x$invested_capacities == 0) or if (x$invested_capacities !=0 and link_profile[direct or indirect] = 0) :
      # - if x$invested_capacities == 0 : "MARG. COST" is counted twice in both direct way and indirect way
      # - if x$invested_capacities !=0 and link_profile[direct or indirect] == 0 : "MARG. COST" is set to 0
      # output_link_h_s_i <-  merge(output_link_h_s_i, capa_zero , by = "link", all.x = TRUE)
      # output_link_h_s_i$sens_direct[which(output_link_h_s_i$"capa" == 0 )] = 1
      # output_link_h_s_i$sens_indirect[which(output_link_h_s_i$"capa" == 0 )] = 1
      # output_link_h_s_i$"MARG. COST DIRECT"[which(output_link_h_s_i$"FLOW LIN." == 0 & output_link_h_s_i$"capa"!=0)]= 0
      # output_link_h_s_i$sens_direct[which(output_link_h_s_i$"FLOW LIN." == 0 & output_link_h_s_i$"capa"!=0 & output_link_h_s_i$"MARG. COST" == 0)]= 1
      # output_link_h_s_i$"MARG. COST INDIRECT"[which(output_link_h_s_i$"FLOW LIN." == 0 & output_link_h_s_i$"capa"!=0)]= 0
      # output_link_h_s_i$sens_indirect[which(output_link_h_s_i$"FLOW LIN." == 0 & output_link_h_s_i$"capa"!=0 & output_link_h_s_i$"MARG. COST" == 0)]= 1


      
    }
  }
  # synthetic results for other candidates 
  if (length(without_profile(candidates)) > 0 )
  {
    output_link_s <- antaresRead::readAntares(areas = NULL, links = without_profile(candidates), mcYears = NULL, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = "MARG. COST")
  }
  
  output <- list("output_link_s" = output_link_s,"output_link_h_s" = output_link_h_s, "output_link_w_s" = output_link_w_s, "output_link_y_s" = output_link_y_s,"output_link_h_s_i" = output_link_h_s_i, "output_link_w_s_i" = output_link_w_s_i, "output_link_y_s_i" = output_link_y_s_i)
  return(output) 
}  
  