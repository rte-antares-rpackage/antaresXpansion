#' Write files which are necessary for master problem (in_iterations,
#' in_z0, in_costs and in _cuts)
#' 
#' @param folder
#'   folder where to write master files
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param exp_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param x
#'   benders data structure
#' @param n_w
#'   number of weeks in the study
#' @return 
#' nothing
#' 
#' @importFrom antaresRead readAntares  readOptimCriteria
#' @importFrom assertthat assert_that
#' @noRd

write_master_files <- function(folder, output_antares, current_it, candidates, exp_options, x, n_w)
{
  # 0. check : 
  assert_that(dir.exists(folder))
  
  # 1. update iteration file
  write(current_it$id, file = paste0(folder, "/in_iterations.txt"), append = TRUE)  
  
  # 2. write current invested capacity in in_z0.txt
  n_candidates <- length(candidates)
  script <-  ""
  capa_zero <- data.frame(link = 0, capa = 0 )
  for (c in 1:n_candidates)
  {
    capacite <- antaresXpansion:::get_capacity(x$invested_capacities, candidate = candidates[[c]]$name, it = current_it$n)
    script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", capacite)
    if (c != n_candidates) {script <- paste0(script, "\n")}
    temp <- data.frame(link = candidates[[c]]$link, capa = capacite)
    capa_zero <- rbind(capa_zero, temp)
  }
  capa_zero <- capa_zero[-1,]
  write(script, file = paste0(folder, "/in_z0.txt"), append = TRUE )  
  
  # 3 and 4. write costs and cuts files 
  #      average cuts and cost file
  if(current_it$cut_type == "average")
  {
    # make sense only if the iteration if "full"
    assert_that(current_it$full)

    # write in_avgcuts.txt file 
    script  <-  paste0(current_it$id, " ", subset(x$costs, it == current_it$n)$overall_costs)
    write(script, file = paste0(folder, "/in_avgcuts.txt"), append = TRUE )      
    
    # read Antares data
    if (length(with_profile(candidates)) > 0 )
    {
      if (length(with_profile_unique(candidates)) > 0 )
      {
        
        output_link_h_s = readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = NULL, 
                                      timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                      select = "MARG. COST")
      }       
      
      
      if (length(with_profile_indirect(candidates)) > 0 )
      {
        
        output_link_h_s_i = readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                        timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                        select = "MARG. COST")
        
        # distinction between rentability in direct and indirect way (in "average" cut_type, it's necessary to come back to year_by_year value and averaged the rentability only at the end) :
        output_link_h_s_flux = readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                           timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                           select = "FLOW LIN.")
        output_link_h_s_direct <- output_link_h_s_i
        output_link_h_s_indirect <- output_link_h_s_i
        output_link_h_s_direct$"MARG. COST"[which(output_link_h_s_flux$"FLOW LIN." < 0)]= 0
        output_link_h_s_indirect$"MARG. COST"[which(output_link_h_s_flux$"FLOW LIN." > 0)]= 0
        
        # when FLOW LIN == 0 : "MARG. COST" can be different from 0 if (x$invested_capacities == 0) or if (x$invested_capacities !=0 and link_profile[direct or indirect] = 0) :
        # - if x$invested_capacities == 0 : "MARG. COST" is counted twice in both direct way and indirect way
        # - if x$invested_capacities !=0 and link_profile[direct or indirect] == 0 : "MARG. COST" is set to 0 
        output_link_h_s_direct <-  merge( output_link_h_s_direct, capa_zero , by = "link")
        output_link_h_s_direct$"MARG. COST"[which(output_link_h_s_flux$"FLOW LIN." == 0 & output_link_h_s_direct$"capa"!=0)]= 0
        output_link_h_s_indirect$"MARG. COST"[which(output_link_h_s_flux$"FLOW LIN." == 0 & output_link_h_s_direct$"capa"!=0)]= 0
        # average rentability over the mc_years :
        output_link_h_s_direct <- output_link_h_s_direct[, mean(`MARG. COST`), by = c("link","timeId","time","day","month","hour")]
        colnames(output_link_h_s_direct )[7] <- "MARG. COST"
        output_link_h_s_indirect <- output_link_h_s_indirect[, mean(`MARG. COST`), by = c("link","timeId","time","day","month","hour")]
        colnames(output_link_h_s_indirect )[7] <- "MARG. COST"
      
      }
    }
    if (length(without_profile(candidates)) > 0 )
    {
      output_link_s = antaresRead::readAntares(areas = NULL, links = without_profile(candidates), mcYears = NULL, 
                                               timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                               select = "MARG. COST")
    }
    # write in_avgrentability.txt file
    script  <-  ""
    for (c in 1:n_candidates)
    {
      if((candidates[[c]]$has_link_profile)&(!candidates[[c]]$has_link_profile_indirect))
      {
        len = length(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")
        tmp_rentability <- sum(as.numeric(subset(output_link_h_s, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile[1:len])
        - candidates[[c]]$cost * n_w / 52
      }      
      
      else if(candidates[[c]]$has_link_profile_indirect)
      {
        len = length(subset(output_link_h_s_direct, link == candidates[[c]]$link)$"MARG. COST")
        tmp_rentability <- sum(as.numeric(subset(output_link_h_s_direct, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile[1:len]+as.numeric(subset(output_link_h_s_indirect, link == candidates[[c]]$link)$"MARG. COST")*candidates[[c]]$link_profile_indirect[1:len]) - candidates[[c]]$cost * n_w / 52
              }
      
      else
      {
        tmp_rentability <- sum(as.numeric(subset(output_link_s, link == candidates[[c]]$link)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
      }
      
      script <- paste0(script, current_it$id, " ", candidates[[c]]$name, " ", tmp_rentability)
      if (c != n_candidates) { script <- paste0(script, "\n")}
    }
    write(script, file = paste0(folder, "/in_avgrentability.txt"), append = TRUE )
  }
  
  #      yearly cuts and cost file
  if(current_it$cut_type == "yearly")
  {
    # make sense only if all the weeks are simulated
    #assert_that(length(current_it$weeks) == length(weeks))
    #assert_that(all(current_it$weeks == weeks))
    
    # compute a few intermediate variables
    n_candidates <- length(candidates)
    last_y <- current_it$mc_years[length(current_it$mc_years)]
    
    # initiate scripts
    script_rentability  <-  ""
    script_cost <- ""
    
    # read antares data
    if (length(antaresXpansion:::with_profile(candidates)) > 0 )
    {
      if (length(antaresXpansion:::with_profile_unique(candidates)) > 0 )
      {
        
        output_link_h = readAntares(areas = NULL, links = antaresXpansion:::with_profile_unique(candidates), mcYears = current_it$mc_years, 
                                      timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                      select = "MARG. COST")
      }       
      
      
      if (length(antaresXpansion:::with_profile_indirect(candidates)) > 0 )
      {
        
        output_link_h_i = readAntares(areas = NULL, links = antaresXpansion:::with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                        timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                        select = "MARG. COST")

        # distinction between rentability in direct and indirect way :
        output_link_h_flux = readAntares(areas = NULL, links = antaresXpansion:::with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                           timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                           select = "FLOW LIN.")
        
        output_link_h_direct <- output_link_h_i
        output_link_h_indirect <- output_link_h_i
        output_link_h_direct$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." < 0)]= 0
        output_link_h_indirect$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." > 0)]= 0
        
        # when FLOW LIN == 0 : "MARG. COST" can be different from 0 if (x$invested_capacities == 0) or if (x$invested_capacities !=0 and link_profile[direct or indirect] = 0) :
        # - if x$invested_capacities == 0 : "MARG. COST" is counted twice in both direct way and indirect way
        # - if x$invested_capacities !=0 and link_profile[direct or indirect] == 0 : "MARG. COST" is set to 0 
        output_link_h_direct <-  merge( output_link_h_direct, capa_zero , by = "link")
        output_link_h_direct$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." == 0 & output_link_h_direct$capa !=0)]= 0
        output_link_h_indirect$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." == 0 & output_link_h_direct$capa !=0)]= 0
        
      }
    }
  
    output_link_y = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                               timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                               select = c("MARG. COST", "HURDLE COST"))
    
    output_area_y = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = c("OV. COST", "NP COST"))
    
    # load cost in expansion_accurate mode
    if(exp_options$uc_type == "expansion_accurate")
    {
      criterion <- antaresRead::readOptimCriteria(opts = output_antares)
    }
    
    # for every mc year
    for(y in current_it$mc_years)
    {
      if(exp_options$uc_type == "expansion_fast")
      {
        # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
        # part of the ANTARES optimization  
        y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) -
          sum(as.numeric(subset(output_area_y, mcYear == y)$"NP COST")) +
          subset(x$costs, it == current_it$n)$investment_costs
      }
      else if(exp_options$uc_type == "expansion_accurate")
      {
        # in that case, the considered cost is the criterion of the optimization problem (not yet post-treated)
        y_cost <- sum(as.numeric(subset(criterion, mcYear == y)$"criterion1")) + subset(x$costs, it == current_it$n)$investment_costs
      }
      else
      {
        # old "fast" and "accurate" mode which shouldn't be used anymore
        y_cost <- sum(as.numeric(subset(output_area_y, mcYear == y)$"OV. COST")) +
          sum(as.numeric(subset(output_link_y, mcYear == y)$"HURDLE COST")) +
          subset(x$costs, it == current_it$n)$investment_costs
      }
      
      
      script_cost <- paste0(script_cost, current_it$id, " ", y , " ",y_cost)
      if (y != last_y) {script_cost <- paste0(script_cost, "\n")}
      
      # for every candidate
      for(c in 1:n_candidates)
      {
        if((candidates[[c]]$has_link_profile)&(!candidates[[c]]$has_link_profile_indirect))
        {
          # (!!!!) carreful : will no work if number of simulated week is not 52 (???)
          len = length(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")
          tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile[1:len]) - candidates[[c]]$cost * n_w / 52
        }      
        
        else if(candidates[[c]]$has_link_profile_indirect)
        {
          len = length(subset(output_link_h_direct, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")
          tmp_rentability <- sum(as.numeric(subset(output_link_h_direct, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile[1:len]+as.numeric(subset(output_link_h_indirect, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")*candidates[[c]]$link_profile_indirect[1:len])- candidates[[c]]$cost * n_w / 52
        }

        else
        {
          tmp_rentability <- sum(as.numeric(subset(output_link_y, link == candidates[[c]]$link & mcYear == y)$"MARG. COST")) - candidates[[c]]$cost * n_w / 52
        }
        
        script_rentability <- paste0(script_rentability, current_it$id, " ", y, " ", candidates[[c]]$name, " ", tmp_rentability)
        if (c != n_candidates || y != last_y)
        {
          script_rentability <- paste0(script_rentability, "\n")
        }
      }
    }
    write(script_rentability, file = paste0(folder, "/in_yearlyrentability.txt"), append = TRUE )
    write(script_cost, file = paste0(folder, "/in_yearlycuts.txt"), append = TRUE )
    
  }
  
  
  #      weekly cuts and cost file
  
  if(current_it$cut_type == "weekly")
  {
    
    # compute a few intermediate variables
    last_w <- current_it$weeks[length(current_it$weeks)]
    last_y <- current_it$mc_years[length(current_it$mc_years)]
    
    # initiate scripts
    script_rentability  <-  ""
    script_cost <- ""
    
    # read antares data
    if (length(with_profile(candidates)) > 0 )
    {
      if (length(with_profile_unique(candidates)) > 0 )
      {
        
        output_link_h = readAntares(areas = NULL, links = with_profile_unique(candidates), mcYears = current_it$mc_years, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                    select = "MARG. COST")
      }       
    
    
     if (length(with_profile_indirect(candidates)) > 0 )
     {
      
       output_link_h_i = readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                    timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                    select = "MARG. COST")
       
       # distinction between rentability in direct and indirect way :      
       output_link_h_flux = readAntares(areas = NULL, links = with_profile_indirect(candidates), mcYears = current_it$mc_years, 
                                       timeStep = "hourly", opts = output_antares, showProgress = FALSE,
                                       select = "FLOW LIN.")
      
       output_link_h_direct <- output_link_h_i
       output_link_h_indirect <- output_link_h_i
       output_link_h_direct$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." < 0)]= 0
       output_link_h_indirect$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." > 0)]= 0
       
       # when FLOW LIN == 0 : "MARG. COST" can be different from 0 if (x$invested_capacities == 0) or if (x$invested_capacities !=0 and link_profile[direct or indirect] = 0) :
       # - if x$invested_capacities == 0 : "MARG. COST" is counted twice in both direct way and indirect way
       # - if x$invested_capacities !=0 and link_profile[direct or indirect] == 0 : "MARG. COST" is set to 0 
       output_link_h_direct <-  merge( output_link_h_direct, capa_zero , by = "link")
       output_link_h_direct$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." == 0 & output_link_h_direct$"capa"!=0)]= 0
       output_link_h_indirect$"MARG. COST"[which(output_link_h_flux$"FLOW LIN." == 0 & output_link_h_direct$"capa"!=0)]= 0
     }
    }
    
    output_link_w = antaresRead::readAntares(areas = NULL, links = "all", mcYears = current_it$mc_years, 
                                             timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                             select = c("MARG. COST", "HURDLE COST"))
    
    output_area_w = antaresRead::readAntares(areas = "all", links = NULL, mcYears = current_it$mc_years, 
                                             timeStep = "weekly", opts = output_antares, showProgress = FALSE,
                                             select = c("OV. COST", "NP COST"))
    
    
    
    # load cost in expansion mode
    if(exp_options$uc_type == "expansion_accurate")
    {
      criterion <- antaresRead::readOptimCriteria(opts = output_antares)
    }
    
    # for every mc years and every week
    for(y in current_it$mc_years)
    {
      for(w in current_it$weeks)
        
      {
        
        if(exp_options$uc_type == "expansion_fast")
        {
          # in that case, non-linear cost has to be removed because they are computed in a post-processing and are not
          # part of the ANTARES optimization  
          w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) -
            sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"NP COST")) +
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        else if(exp_options$uc_type == "expansion_accurate")
        {
          # in that case, the considered cost is the criterion of the optimization problem (not yet post-treated)
          w_cost <- sum(as.numeric(subset(criterion, mcYear == y & timeId == w)$"criterion1")) + 
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        else
        {
          # old "fast" and "accurate" mode which shouldn't be used anymore
          w_cost <- sum(as.numeric(subset(output_area_w, mcYear == y & timeId == w)$"OV. COST")) +
            sum(as.numeric(subset(output_link_w, mcYear == y & timeId == w)$"HURDLE COST")) +
            subset(x$costs, it == current_it$n)$investment_costs /n_w
        }
        
        script_cost <- paste0(script_cost, current_it$id, " ", y , " ", w, " ", w_cost)
        if (y != last_y || w != last_w) {script_cost <- paste0(script_cost, "\n")}
        
        # for every candidate
        for(c in 1:n_candidates)
        {
          if((candidates[[c]]$has_link_profile)&(!candidates[[c]]$has_link_profile_indirect))
          {
            # (!!!!) carreful : will no work if number of simulated week is not 52 (???)
            first_h <- 7*24*(w-1)+1
            last_h <- 7*24*w
            tmp_rentability <- sum(as.numeric(subset(output_link_h, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")* candidates[[c]]$link_profile[first_h:last_h]) - candidates[[c]]$cost /52
          }      
          
          else if(candidates[[c]]$has_link_profile_indirect)
          {
            # (!!!!) carreful : will no work if number of simulated week is not 52 (???)
            first_h <- 7*24*(w-1)+1
            last_h <- 7*24*w
            tmp_rentability <- sum(as.numeric(subset(output_link_h_direct, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")*candidates[[c]]$link_profile[first_h:last_h]+as.numeric(subset(output_link_h_indirect, link == candidates[[c]]$link & mcYear == y & timeId >= first_h & timeId <= last_h)$"MARG. COST")*candidates[[c]]$link_profile_indirect[first_h:last_h])-candidates[[c]]$cost  / 52 
          }
          else
          {
            tmp_rentability <- sum(as.numeric(subset(output_link_w, link == candidates[[c]]$link & mcYear == y & timeId == w)$"MARG. COST")) - candidates[[c]]$cost /52
          }
          
          script_rentability <- paste0(script_rentability, current_it$id, " ", y , " ", w , " ", candidates[[c]]$name, " ", tmp_rentability)
          if (c != n_candidates || y != last_y || w != last_w)
          {
            script_rentability <- paste0(script_rentability, "\n")
          }
        }
      }
    }
    write(script_rentability, file = paste0(folder, "/in_weeklyrentability.txt"), append = TRUE )
    write(script_cost, file = paste0(folder, "/in_weeklycuts.txt"), append = TRUE )
  }
}