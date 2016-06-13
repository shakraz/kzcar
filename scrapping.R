library(rvest)

kolesa_df2<-NULL

for (i in 2166:5350) {
  
  link<-paste0('http://kolesa.kz/cars/?page=',i)
  
  print(paste0('The ', i,' is processing now'))
  page<-read_html(link)
  
  content<-html_node(page, xpath='//*[contains(@class,"result-block col-sm-8")]')
  
  
  items<-html_nodes(content, xpath='//*[contains(@class,"list-item")]')[1:20]
  
    for (j in 1: length(items)){
      item=items[j]
      title<-html_text(html_node(item, '.list-title .list-link'),trim=T)
      
      year<-tryCatch({getNumeric(html_text(html_node(item, '.list-extra-info .year'),trim=T))},error=function(err){year<-NA})
      
      price<-tryCatch({getNumeric(html_text(html_node(item, '.price-in-list .price'),trim=T))},error=function(err){price<-NA})
      
      region<-html_text(html_node(item, '.list-region'),trim=T)
      
      views<-getNumeric(html_text(html_node(item, '.list-views-comments span'),trim=T))
      
      desc<-html_text(html_node(item, '.description'),trim=T)
      
      type<-sapply(desc, function(x)   unlist(strsplit(x,','))[1])
      
      volume2<- getNumeric(sapply(desc, function(x)   getNumeric(unlist(strsplit(x,','))[2])))
      
      url<-html_attr(html_node(item, '.photo a'), 'href')
      
      df<-data.frame(title, price,year, region, views, type, volume2, url)
      
      if (i==2166){
        kolesa_df2<-df
      }else{
        kolesa_df2<-rbind(kolesa_df2, df)
      }
    
    }
}






getNumeric<-function(text_list){ 
  as.numeric(
    gsub('[^\\d+]','',text_list, perl=T))
}
kolesa_df<-read.csv('C:\\Users\\shakraz\\Documents\\KnnR\\kolesa_df.csv', encoding = 'UTF-8')

kolesa_df<-rbind(kolesa_df, kolesa_df2)
#write.csv(kolesa_df, paste0(getwd(),'/kolesa_df.csv'), row.names = F)

