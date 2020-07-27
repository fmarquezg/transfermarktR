library(tidyverse)
library(rvest)
library(transfermarketR)


#get_squads_sites
get_national_squads <- function(n){
  fifa_ranking <- "https://www.transfermarkt.us/wettbewerbe/fifa/wettbewerbe?plus="
  nations <- xml2::read_html(paste0(fifa_ranking,n))

  nations_url_odd<-nations %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble() %>%
    unique()
  colnames(nations_url_odd)<-c('url')
  nations_url_odd<-nations_url_odd %>%
    mutate(url = paste0("https://www.transfermarkt.us",url,"plus/1/galerie/0?saison_id="))

  nations_url_even<-nations %>%
    rvest::html_nodes('.even') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble() %>%
    unique()
  colnames(nations_url_even)<-c('url')
  nations_url_even<-nations_url_even %>%
    mutate(url = paste0("https://www.transfermarkt.us",url,"plus/1/galerie/0?saison_id="))

  nations_url <-rbind(nations_url_odd,nations_url_even)


  nation_odd<-nations %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_text() %>%
    tibble()
  colnames(nation_odd)<-'nation'
  nation_odd<-nation_odd %>% filter(nation!="")

  nation_even<-nations %>%
    rvest::html_nodes('.even') %>%
    rvest::html_nodes('.hauptlink') %>%
    rvest::html_nodes('a') %>%
    rvest::html_text() %>%
    tibble()
  colnames(nation_even)<-'nation'
  nation_even<-nation_even %>% filter(nation!="")

  nations <-rbind(nation_odd,nation_even) %>%
    unique()

  data <- cbind(nations,nations_url)
  return(data)
}


#squad <- "https://www.transfermarkt.us/mexico/kader/verein/6303/plus/1/galerie/0?saison_id="

get_squad_list <-function(url, year){
  # Returns a dataframe with player name and URL for national Team Squad for a specific year
  game <- xml2::read_html(paste0(squad,y))


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
    tibble()

  name_odd<-game %>%
    rvest::html_nodes('.odd') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('title') %>%
    tibble()

  url_even<-game %>%
    rvest::html_nodes('.even') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('href') %>%
    tibble()

  name_even<-game %>%
    rvest::html_nodes('.even') %>%
    rvest::html_node('span') %>%
    rvest::html_nodes('a') %>%
    rvest::html_attr('title') %>%
    tibble()

  odd <- cbind(url = url_odd,name = name_odd)
  even <- cbind(url = url_even, name = name_even)

  squad_urls<-rbind(odd,even)
  colnames(squad_urls) <- c('url','name')

  squad_urls <- squad_urls %>%
    mutate(url = paste0("https://www.transfermarkt.us",url),
           url = str_replace_all(url, 'profil','marktwertverlauf'),
           nation = country,
           squad_year = year) %>%
    select(nation, squad_year, name, url)
  return(squad_urls)
}



get_player_historic_market_value <- function(url,name){
  print(name)

  mv_url <- xml2::read_html(url)
  flag <- 0
  test<-tryCatch({mv_url %>%
      html_nodes('script') %>%
      .[42] %>%
      html_text()
  }, error = function(cond){
    message('No MV')
    message(cond)
    #colnames <- c('contract_date','mv','age','club','dob','citizenship','position')
    flag = 1
    return(flag)
  }
  )
  if(test==1){
    return(data.frame(contract_date = NA, mv = NA, age = NA,club = NA,dob = NA,citizenship = NA, position = NA, name = name))
  }

  t<-mv_url %>%
    html_nodes('script') %>%
    .[42] %>%
    html_text()

  summary_df<-mv_url %>%
    html_nodes("p") %>%
    html_text() %>%
    data.frame()
  colnames(summary_df)<-c('raw_txt')

  #Get dim value metrics for player

  summary_df<-mv_url %>%
    html_nodes("p") %>%
    html_text() %>%
    data.frame()
  colnames(summary_df)<-c('raw_txt')



  #Get dim value metrics for player
  player_dim <-summary_df%>%
    mutate(txt = str_remove_all(raw_txt,'\t'),
           txt = str_remove_all(txt,'\n'),
           txt = str_remove_all(txt,'/Age'),
           #d_flag = str_detect( txt,"Date of birth"),
           dob = case_when(
             str_detect( txt,"Date of birth") == TRUE ~ sub (".*Date of birth: *(.*?) *\\(.*","\\1", txt),
             TRUE ~ ''),
           citizenship = case_when(
             str_detect( txt,"Citizenship") == TRUE ~ sub (".*Citizenship: *(.*?) * *","\\1", txt),
             TRUE ~ ''),
           position = case_when(
             str_detect( txt,"Position") == TRUE ~ sub (".*Position: *(.*?) * *","\\1", txt),
             TRUE ~ ''),
           position = str_remove(position,"                                                    ")

    ) %>%
    select(dob,citizenship,position) %>%
    group_by("f") %>%
    mutate(dob=paste0(dob, collapse = ""),
           citizenship =paste0(citizenship, collapse = ""),
           position = paste0(position, collapse = "")) %>%
    distinct() %>% ungroup() %>%
    select(-'"f"') %>%
    mutate(dob = as.Date(dob, format = '%b %d, %Y'))



  #Market Value
  t1<-strsplit(t,"e','data':")[[1]][4]
  t2<-strsplit(t1,"}],'credits'")[[1]][1]
  t3<-str_remove_all(t2,'\\[')
  t3<-str_remove_all(t3,'\\]')
  t3<-str_remove_all(t3,'x20')
  t4<-str_split(t3,"\\}\\},\\{", simplify = TRUE)
  a<-strsplit(t3,"\\}\\},\\{")

  mv_df<-data.frame(raw=a)
  colnames(mv_df)<-c('raw')

  contracts_df <- mv_df %>%
    mutate(
      mv = sub(".*'y': *(.*?) *,'verein'.*","\\1", raw),
      contract_date = sub(".*'datum_mw':' *(.*?) *','x':.*","\\1", raw),
      club = sub(".*,'verein':' *(.*?) *','age'.*","\\1", raw),
      age = sub(".*'age': *(.*?) *,'mw'.*","\\1", raw),
      contract_date = str_replace_all(contract_date, "\\\\",' '),
      club = str_replace_all(club, "\\\\",' '),
      contract_date = as.Date(contract_date, format = '%b %d, %Y')
    ) %>%
    select(contract_date,mv,age,club)

  player_mv_df<-cbind(contracts_df,player_dim) %>%
    mutate(name = name)
  print(url)
  return(player_mv_df)

}
