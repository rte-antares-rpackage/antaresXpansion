#' Set ANTARES study options related to the current benders decomposition
#' 
#' 
#' @param benders_options
#'   list of benders decomposition options, as returned by
#'   \code{\link{read_options}}.
#' @param candidates
#'   list of investment candidates, as returned by
#'   \code{\link{read_candidates}}
#' @param opts
#'   list of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return nothing
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions getLinks getAreas
#' 
set_antares_options <- function(benders_options, candidates, opts = antaresRead::simOptions())
{
  # 1 - set output filtering options
  enable_custom_filtering(TRUE, opts)
  enable_year_by_year(TRUE, opts)
  filter_output_areas(areas = antaresRead::getAreas(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  filter_output_links(links = antaresRead::getLinks(opts = opts), filter = c("weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  if (length(with_profile(candidates)) > 0 )
  {
    filter_output_links(links = with_profile(candidates), filter = c("hourly", "weekly", "annual"), type = c("year-by-year", "synthesis"), opts = opts)
  }
  # 2 - set unit-commitment mode
  if(benders_options$uc_type == "accurate")
  {
    set_uc_mode(mode = "accurate", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(benders_options$uc_type == "expansion_accurate")
  {
    set_uc_mode(mode = "accurate", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(benders_options$uc_type == "fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = TRUE, opts = opts)
  }
  if(benders_options$uc_type == "expansion_fast")
  {
    set_uc_mode(mode = "fast", opts = opts)
    enable_uc_heuristic(enable = FALSE, opts = opts)
  }
  
  # 3 - set week and initial day
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
  set_week(first_day = first_day_week, opts)
}
