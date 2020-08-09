##################################################################
### Documenting the data files                                 ###
# Author: Francisco Marquez                                      #
# Code Style Guide: Google R Format                              #
##################################################################


#' Get National Squads Function
#'
#' This function pulls data for all national squads
#' @param none ad
#' @keywords none
#' @export
#' @examples
#' pull_national_squads()
#'
pull_national_squads <- function(){

  national_squads <- purrr::map_df(seq(1:6), get_national_squads) %>%
    unique()
  temp_squad_year <- national_squads %>% tidyr::expand(nation,c(2010:2019)) %>% dplyr::rename(year = `c(2010:2019)`) %>%
    dplyr::left_join(national_squads, by = c('nation'))
  nat_squads_year<-purrr::map2_dfr(temp_squad_year %>% dplyr::pull(url), temp_squad_year %>% dplyr::pull(year), get_squad_list)
  return(nat_squads_year)

}


#' Get National Squads Function
#'
#' This function pulls data for all national squads
#' @param none ad
#' @keywords none
#' @export
#' @examples
#' get_all_player_mv()
#'
get_all_player_mv <- function(){
  nat_squads_year <- pull_national_squads()

  unique_player<-nat_squads_year %>%
    dplyr::select(name,url) %>%
    dplyr::distinct()

  future::plan(multiprocess)
  future::plan(multisession(workers = availableCores()))
  squad_mv_df<-furrr::future_map2_dfr(unique_player %>%
                                        dplyr::pull(url),unique_player %>%
                                        dplyr::pull(name),get_player_historic_market_value, .progress = TRUE)

  return(squad_mv_df)

}

# unique_player<-nat_squads_year %>%
#   dplyr::select(name,url) %>%
#   dplyr::distinct()
