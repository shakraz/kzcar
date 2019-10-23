library(dplyr)
library(help=reshape2)


kolesa <- data.frame()
regions <- c('aktau', 'aktobe', 'almaty', 'atyrau', 'jezkazgan', 'karaganda',
             'kokshetau', 'kostanai', 'kyzylorda', 'nur-sultan', 'pavlodar',
             'petropavlovsk', 'semei', 'taldykorgan', 'taraz', 'turkestan',
             'ust-kamenogorsk','shymkent')

for (city in regions){

url  <- paste0('https://kolesa.kz/cars/', city)

region_page <- read_html(url)
region_content <- tail(html_nodes(region_page, 'div.pager li'),n=1)

last_page_number <- as.integer(html_text(region_content, "span"))

for (page_number in 1:last_page_number) {
  
  main_url <- paste0('https://kolesa.kz/cars/',city,'/?page=', page_number)
  page <- read_html(main_url)
  content <- html_node(page, '#results')
  cars <- html_nodes(content, '.list-item')[1:20]
  
  for (i in 1:length(cars)) {
    car = cars[i]
    id <- html_attr(car, 'data-id')
    img <- html_attr(html_node(car, "div.pictures-list img"), 'src')
    date <- html_text(html_node(car, 'span.date'))
    price <- html_text(html_node(car, 'span.price'))
    
    car_page <- read_html(paste0("https://kolesa.kz/a/show/", id))
    car_content <- html_node(car_page, 'div.row')
    header <- html_nodes(car_content, 'h1.offer__title span')
    brand <- html_text(header[1])
    model <- html_text(header[2])
    year <- html_text(header[3])
    
    
    col_names <-
      html_attr(html_nodes(car_content, 'div.offer__parameters dl dt'),
                "title")
    values <-
      html_text(html_nodes(car_content, 'div.offer__parameters dl dd'),
                ".value")
    views <-
      html_text(html_node(car_content, "div.offer__info-views span"),
                'strong')
    desc_df <- data.frame(matrix(values, nrow=1))
    names(desc_df) <- col_names
    
    
    df <- data.frame("id"=id, "img"=img, "date"=date,  "price"=price,"brand"=brand,
                     "model"=model, "year"=year)
    df <- bind_cols(df, desc_df)  
    
    if(
      which(regions==city)==1 &&
      i==1 && page_number==1){
      kolesa <- df
    }else{
      kolesa <- bind_rows(kolesa, df)
    }
  
  }
  
  print (paste0("region: ", city, " page ", page_number," out of ",last_page_number, " has been processed"))
}

}

kolesa_df <- kolesa[names(kolesa)[!is.na(names(kolesa))]]
write.csv(kolesa_df,'kolesa.csv', row.names = F)

names(kolesa_df) <- c('id', 'img', 'create_date', 'price', 'brand', 'model', 
                      'year', 'region', 'type', 'engine_volume', 'mileage',
                      'gear', 'wheel','color', 'drive_type', 'legalized', 'vin','engine')

kolesa_df <- data.frame(lapply(kolesa_df, function(x) gsub("\n", "", trimws(x) )))
