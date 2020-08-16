################################################################################
# Author: Paco Marquez
# Purpose: Documenting Data Files
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get National Squads Function
#' This function pulls all national squads. Currently only pulling data from 2010 to 2019.
#' @description This function outputs a dataframe with players per year per squad with url needed for
#' future functions.
#' @examples
#' pull_national_squads()
#' @export
pull_national_squads <- function(){

  national_squads <- purrr::map_df(seq(1:6), get_national_squads) %>%
    unique()
  temp_squad_year <- national_squads %>% tidyr::expand(nation,c(2010:2019)) %>% dplyr::rename(year = `c(2010:2019)`) %>%
    dplyr::left_join(national_squads, by = c('nation'))
  nat_squads_year<-purrr::map2_dfr(temp_squad_year %>% dplyr::pull(url), temp_squad_year %>% dplyr::pull(year), get_squad_list)
  return(nat_squads_year)

}


#' Get National Squads Function
#' This function pulls data for all national squads
#' @description This function calls multiple functions from from this package.
#' calls the player url function and outputs market value of each player for each time they
#' updated their Market Value.
#' @examples
#' get_all_player_mv()
#' @export
get_all_player_mv <- function(){
  nat_squads_year <- pull_national_squads()

  unique_player<-nat_squads_year %>%
    dplyr::select(name,url) %>%
    dplyr::distinct()

  future::plan(future::multiprocess)
  future::plan(future::multisession(workers = availableCores()))
  squad_mv_df<-furrr::future_map2_dfr(unique_player %>%
                                        dplyr::pull(url),unique_player %>%
                                        dplyr::pull(name),get_player_historic_market_value, .progress = TRUE)

  return(squad_mv_df)

}
