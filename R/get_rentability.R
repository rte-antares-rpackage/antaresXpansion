#' Get rentability of investments candidates from an antares output
#' 
#' @param output_antares
#'   output from an Antares study, as obtained by \code{antaresRead::setSimulationPath}
#' @param current_it
#'   current iteration of the benders problem with its associated characteristics
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param n_w
#'   number of weeks in the study
#' @param x
#'   data structure of the benders decomposition
#' @return 
#' vector of rentability
#' 
#' @importFrom antaresRead readAntares 
#' @noRd

get_expected_rentability <- function(output_antares, current_it, candidates, n_w, x)
{
  n_candidates <- length(candidates)
  capa_zero <- data.frame(link = 0, capa = 0 )
  for (c in 1:n_candidates)
  {
    capacite <- antaresXpansion:::get_capacity(x$invested_capacities, candidate = candidates[[c]]$name, it = current_it$n)
    temp <- data.frame(link = candidates[[c]]$link, capa = capacite)
    capa_zero <- rbind(capa_zero, temp)
  }
  capa_zero <- capa_zero[-1,]  

  if(!current_it$full)
  {
    # in case of a partial iteration, expected rentability over all weeks 
    # of all mc years cannot be computed
    return(rep(NA, n_candidates))
  }
  else
  {
    # first, read antares outputs 
    # hourly results (for candidates with link profile only)
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
    # synthetic results for other candidates 
    if (length(without_profile(candidates)) > 0 )
    {
      output_link_s = antaresRead::readAntares(areas = NULL, links = without_profile(candidates), mcYears = NULL, 
                                             timeStep = "annual", opts = output_antares, showProgress = FALSE,
                                             select = "MARG. COST")
    }
    
    # second, get aggregated rentability for those links
    get_aggr_rentability <- function(c)
    {
      # carreful : not sure the following line works in case of a partial iteration
      if((c$has_link_profile)&(!c$has_link_profile_indirect))
      {
        return(sum(as.numeric(subset(output_link_h_s,   link == c$link)$"MARG. COST")*c$link_profile[1:8736]) - c$cost * n_w / 52)         
      }

      else if(c$has_link_profile_indirect)
      {        
        return(sum(as.numeric(subset(output_link_h_s_direct,   link == c$link)$"MARG. COST")*c$link_profile[1:8736]+ as.numeric(subset(output_link_h_s_indirect, link == c$link)$"MARG. COST")*c$link_profile_indirect[1:8736])- c$cost * n_w / 52) 
      }
      else 
      {
        return(sum(as.numeric(subset(output_link_s, link == c$link)$"MARG. COST")) - c$cost * n_w / 52)
      }
    }
    average_rentability <- sapply(candidates, FUN = get_aggr_rentability)
    return(average_rentability)
  }
    
}