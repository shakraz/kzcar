library(rvest)
library(rjson)

krisha <- data.frame()

regions <- c(
  #'almaty',
  #'nur-sultan',
  #'aktau', 
  #'aktobe',
  #'atyrau', 
  #'karaganda',
  #'kokshetau', 
  #'kostanaj', 
  #'kyzylorda',  
  #'pavlodar',
  #'petropavlovsk', 
  #'semej', 
  #'taldykorgan', 
  #'taraz', 
  #'turkestan',
  #'ust-kamenogorsk',
  #'shymkent')


for (region in regions){
  region_page <- read_html(paste0("https://krisha.kz/prodazha/kvartiry/", region))
  pages <- html_nodes(region_page, 'a.paginator__btn')
  region_content <- tail(pages[1: length(pages)-1],n=1)
  last_page_number <- as.integer(html_text(region_content))-1
  

for (page in 1:last_page_number){

  search_url <- paste0('https://krisha.kz/prodazha/kvartiry/',region,'/?page=', page)
  krisha_page <- read_html(search_url)
  items <- html_nodes(krisha_page, '.main-col .a-card')
  

for(i in 1:length(items)){
  flat <- items[i]
  id <- html_attr(flat, 'data-id')
  flat_url <- paste0('https://krisha.kz/a/show/', id)
  flat_page <- read_html(flat_url)
  price <- html_text(html_node(flat_page, 'div.offer__price'), trim=T)
  title <- html_text(html_node(flat_page, 'div.offer__advert-title h1'), trim = T)
  owner <- html_text(html_node(flat_page, "div.owners__label"), trim = T)
  desc <- html_nodes(flat_page, '.offer__info-item')
  col_names <- sapply(desc, function(x) {html_text(html_nodes(x, 'div')[1], trim=T)})
  values <- sapply(desc, function(x) {html_text(html_nodes(x, 'div')[3], trim = T)})
  desc_df <- data.frame(matrix(values, nrow=1))
  names(desc_df) <- col_names
  
  param_names <-
    html_text(html_nodes(flat_page, 'div.offer__parameters dl dt'))
  
  param_values <-
    html_text(html_nodes(flat_page, 'div.offer__parameters dl dd'))
  param_df <- data.frame(matrix(param_values, nrow=1))
  names(param_df) <- param_names
  
  df <- bind_cols(desc_df, param_df)
  
  geo_content <- html_node(flat_page, '#jsdata')
  text <- html_text(geo_content, trim = T)
  text <- gsub(pattern = "\\\"", "", text)
  lat <- regmatches(text,regexpr("(lat:)[0-9|.]+", text))
  lon <- regmatches(text,regexpr("(lon:)[0-9|.]+", text))
  df$lat <- ifelse(length(lat),lat,NA )
  df$lon <- ifelse(length(lon),lon,NA )
  df$id <- id
  df$url <- flat_url
  df$price <- price
  df$title <- title
  df$owner <- owner
  if(nrow(krisha)==0){
    krisha <- df
  } else{
    krisha <- bind_rows(krisha, df)
  }
}
  
  print(paste0("region ",region, ' page ', page," out of ",last_page_number, " has been processed ",Sys.time() ))

}

}
krisha <- data.frame(lapply(krisha, function(x) gsub("\n", "", trimws(x) )))


write.csv(krisha,'krisha.csv', quote = T, row.names = F)
