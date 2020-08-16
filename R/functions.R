################################################################################
# Author: Paco Marquez
# Purpose: Documenting Data Files
# Code Style Guide: styler::tidyverse_style()
################################################################################

#' Get National Squads Function
#'
#' This function pulls data for all national squads
#' @param num_of_page
#' @keywords none
#' @export
#' @examples
#' get_national_squads()
#'

get_national_squads <- function(n){
  #fifa_ranking <- "https://www.transfermarkt.us/wettbewerbe/fifa/wettbewerbe?plus="
  #no fancy details (top url doesnt always work)
  fifa_ranking <- "https://www.transfermarkt.us/wettbewerbe/fifa?page="
  nations <- xml2::read_html(paste0(fifa_ranking,n))

  nations_url_odd <- nations %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble::tibble() %>%
    unique()

  colnames(nations_url_odd)<-c('url')
  nations_url_odd<-nations_url_odd %>%
    dplyr::mutate(url = paste0("https://www.transfermarkt.us",url,"plus/1/galerie/0?saison_id="))

  nations_url_even<-nations %>%
    rvest::html_nodes('.even') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble::tibble() %>%
    unique()

  colnames(nations_url_even)<-c('url')
  nations_url_even<-nations_url_even %>%
    dplyr::mutate(url = paste0("https://www.transfermarkt.us",url,"plus/1/galerie/0?saison_id="))

  nations_url <-rbind(nations_url_odd,nations_url_even)

  nation_odd<-nations %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_text() %>%
    tibble::tibble()

  colnames(nation_odd)<-"nation"
  nation_odd <- nation_odd %>% dplyr::filter(nation !="")

  nation_even<-nations %>%
    rvest::html_nodes('.even') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_text() %>%
    tibble::tibble()

  colnames(nation_even)<-"nation"
  nation_even <- nation_even %>% dplyr::filter(nation!="")

  nations <-rbind(nation_odd,nation_even) %>%
    unique()

  data <- cbind(nations,nations_url)
  return(data)

}


#' Get Squad Players in List
#'
#' This function pulls data for all national squads
#' @param team_url year_ofsquad
#' @keywords none
#' @export
#' @examples
#' get_squad_list()
#'
get_squad_list <-function(url, year){
  # Returns a dataframe with player name and URL for national Team Squad for a specific year
  print(paste0(url,year))

  ##
  tryCatch(game <- paste0(url,year) %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html(),
           error = function(e) {
             message('Timout.. sleep and retry')
             Sys.sleep(45)
             game <- paste0(url,year) %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html() },
           finally = {game <- paste0(url,year) %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html()})

  ##
  #game <- xml2::read_html(paste0(url,year))


  country<-game %>%
    rvest::html_nodes('.dataName') %>%
    rvest::html_node('h1') %>%
    rvest::html_node('span') %>%
    rvest::html_text()

  # player url - odd
  url_odd<-game %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble::tibble()

  name_odd<-game %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('title') %>%
    tibble::tibble()

  url_even<-game %>%
    rvest::html_nodes('.even') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble::tibble()

  name_even<-game %>%
    rvest::html_nodes('.even') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('title') %>%
    tibble::tibble()

  odd <- cbind(url = url_odd,name = name_odd)
  even <- cbind(url = url_even, name = name_even)

  squad_urls<-rbind(odd,even)
  colnames(squad_urls) <- c('url','name')

  squad_urls <- squad_urls %>%
    dplyr::mutate(url = paste0("https://www.transfermarkt.us",url),
           url = stringr::str_replace_all(url, 'profil','marktwertverlauf'),
           nation = country,
           squad_year = year) %>%
    dplyr::select(nation, squad_year, name, url)
  return(squad_urls)
}



#' Get Historic Market Value for Player
#' This function pulls data for all national squads
#' @params url_of_player, name
#' @keywords none
#' @examples
#' get_player_historic_market_value()
#' @export
get_player_historic_market_value <- function(url,name){
  print(name)
  print(url)

  tryCatch(mv_url <- url %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html(),
           error = function(e) {
             message('Timout.. sleep and retry')
             Sys.sleep(10)
             mv_url <- url %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html() },
           finally = {mv_url <- url %>% httr::GET(., httr::timeout(60)) %>% xml2::read_html()})

  flag <- 0
  test <- mv_url %>%
    rvest::html_nodes('script') %>%
    length()
  test<-ifelse(test<=42,1,0)


  if(test==1){
    print('Test ==1')
    print(test)
    return(data.frame(contract_date = NA, mv = NA, age = NA,club = NA,dob = NA,citizenship = NA, position = NA, name = name))
  }

  t<-mv_url %>%
    rvest::html_nodes('script') %>%
    .[43] %>%
    rvest::html_text()

  summary_df<-mv_url %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    data.frame()
  colnames(summary_df)<-c('raw_txt')

  #Get dim value metrics for player

  summary_df<-mv_url %>%
    rvest::html_nodes("p") %>%
    rvest::html_text() %>%
    data.frame()
  colnames(summary_df)<-c('raw_txt')



  #Get dim value metrics for player
  player_dim <-summary_df%>%
    dplyr::mutate(txt = stringr::str_remove_all(raw_txt,'\t'),
                  txt = stringr::str_remove_all(txt,'\n'),
                  txt = stringr::str_remove_all(txt,'/Age'),
                  #d_flag = str_detect( txt,"Date of birth"),
                  dob = dplyr::case_when(
                    stringr::str_detect( txt,"Date of birth") == TRUE ~ sub (".*Date of birth: *(.*?) *\\(.*","\\1", txt),
                    TRUE ~ ''),
                  citizenship = dplyr::case_when(
                    stringr::str_detect( txt,"Citizenship") == TRUE ~ sub (".*Citizenship: *(.*?) * *","\\1", txt),
                    TRUE ~ ''),
                  position = dplyr::case_when(
                    stringr::str_detect( txt,"Position") == TRUE ~ sub (".*Position: *(.*?) * *","\\1", txt),
                    TRUE ~ ''),
                  position = stringr::str_remove(position,"                                                    ")

    ) %>%
    dplyr::select(dob,citizenship,position) %>%
    dplyr::group_by("f") %>%
    dplyr::mutate(dob=paste0(dob, collapse = ""),
                  citizenship =paste0(citizenship, collapse = ""),
                  position = paste0(position, collapse = "")) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::select(-'"f"') %>%
    dplyr::mutate(dob = as.Date(dob, format = '%b %d, %Y'))



  #Market Value
  t1<-strsplit(t,"e','data':")[[1]][4]
  t2<-strsplit(t1,"}],'credits'")[[1]][1]
  t3<-stringr::str_remove_all(t2,'\\[')
  t3<-stringr::str_remove_all(t3,'\\]')
  t3<-stringr::str_remove_all(t3,'x20')
  t4<-stringr::str_split(t3,"\\}\\},\\{", simplify = TRUE)
  a<-strsplit(t3,"\\}\\},\\{")

  mv_df<-data.frame(raw=a)
  colnames(mv_df)<-c('raw')

  contracts_df <- mv_df %>%
    dplyr::mutate(
      mv = sub(".*'y': *(.*?) *,'verein'.*","\\1", raw),
      contract_date = sub(".*'datum_mw':' *(.*?) *','x':.*","\\1", raw),
      club = sub(".*,'verein':' *(.*?) *','age'.*","\\1", raw),
      age = sub(".*'age': *(.*?) *,'mw'.*","\\1", raw),
      contract_date = stringr::str_replace_all(contract_date, "\\\\",' '),
      club = stringr::str_replace_all(club, "\\\\",' '),
      contract_date = as.Date(contract_date, format = '%b %d, %Y')
    ) %>%
    dplyr::select(contract_date,mv,age,club)

  player_mv_df<-cbind(contracts_df,player_dim) %>%
    dplyr::mutate(name = name)
  #print(url)
  return(player_mv_df)
}

