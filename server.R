
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(rdrop2)
library(dplyr)
library(leaflet)
kolesa_df<-drop_read_csv('kolesa_df.csv',encoding = 'UTF-8')
sim_model<-drop_read_csv('sim_model.csv',encoding = 'UTF-8')

colnames(sim_model)<-gsub('X', '', colnames(sim_model), perl=T)

kolesa_df<-unique(kolesa_df)
kolesa_df$type<-as.character(kolesa_df$type)
kolesa_df$region<-as.character(kolesa_df$region)
kolesa_df$car_id<-regmatches(kolesa_df$url, regexpr('\\d+', kolesa_df$url,perl=T))
type_id<-kolesa_df[grep('\\d+', kolesa_df$type,perl=T),]$car_id
t<-head(kolesa_df[order(kolesa_df$price, decreasing = T),]$car_id,10)
t1<-head(kolesa_df[order(kolesa_df$price, decreasing = F),]$car_id,10)

kolesa_df<-filter(kolesa_df, !car_id %in% c(t,t1,type_id))


Sys.setlocale(category = "LC_ALL", locale = "russian")

shinyServer(function(input, output) {
  

  set_filter<-reactive({
    if (is.null(input$reg)| is.null(input$car_type) ) {
      return(NULL)
    }  
    if(input$reg!='All'){
      kolesa_df<-filter(kolesa_df,region==input$reg)
    }
    if (input$car_type!='All'){
      kolesa_df<-filter(kolesa_df, type==input$car_type)
    }
    kolesa_df
  })
  
  
  cars<-reactive({
    
    if (is.null(input$reg)| is.null(input$car_type) ) {
      return(NULL)
    }    
    if(input$reg!='All'){
      kolesa_df<-filter(kolesa_df,region==input$reg)
    }
    if (input$car_type!='All'){
      kolesa_df<-filter(kolesa_df, type==input$car_type)
    }
    
    kolesa_df<-filter(kolesa_df,
                      year>=input$year_range[1],
                      year<=input$year_range[2],
                      price>=input$price_range[1],
                      price<=input$price_range[2]
                      )
  })

  
  recommend<-reactive({
    
    index<-input$table_rows_selected[length(input$table_rows_selected)]
    if(is.null(index)){
      return(NULL)
    }
    carId<-kolesa_df[index,c('car_id')]
    t<-sim_model[,c(carId, 'id')]
    names(t)<-c('sim', 'id')
    t<-filter(t, id!=carId)
    t<-t[order(t$sim, decreasing=F),]
    ids<-t[1:input$n,c('id')]
    kolesa_df<-filter(kolesa_df, car_id %in% ids)
    
  })
  

  output$outSliderYear<-renderUI({
    min<-min(set_filter()$year,na.rm=T)
    max<-max(set_filter()$year,na.rm=T)
    sliderInput('year_range', 'Год выпуска' ,min=min, max=max, c(min,max) ,sep = '',step=1)
  })
  
  output$outSelectRegion<-renderUI({
    selectInput('reg','Регион',multiple =T, c('All', as.character(unique(kolesa_df$region))), selected = 'Костанай')
  })
  
  
  output$outSliderPrice<-renderUI({
    min<-min(set_filter()$price,na.rm=T)
    max<-max(set_filter()$price,na.rm=T)
    sliderInput('price_range','Цена',min=min,max=max, c(min,max))
  })
  
  output$outSelectType<-renderUI({
    selectInput('car_type','Кузов', c('All', as.character(unique(kolesa_df$type))), selected = 'All')
  })
  
  output$table <- DT::renderDataTable(DT::datatable({

    cars()[,c('title','price','year','region','type','volume2')]

  }, selection = 'single',rownames = T,colnames = c('rn',"Название","Цена","Год","Регион" ,"Кузов","Объем"),options=list(pageLength=10)))
  
  output$sim_cars<-DT::renderDataTable(DT::datatable({
    
    recommend()
  },rownames = F,colnames = c("Модель",   "Цена",   "Год",    "Регион" , "Просмотров",   "Кузов",    "Объем")))
  
  output$priceDist<-renderPlot({
    ggplot(data=cars(), aes(x=type, y=price))+
      geom_boxplot()+
      theme(axis.text.x=element_text(angle=90))+
      scale_y_continuous(labels = scales::comma)+
      labs(x='Кузов',y='Цена (млн.тг)')
    
    })

  output$regDist<-renderPlot({
 ggplot(data=cars(), aes(x=region, y=price))+
      geom_boxplot()+
      theme(axis.text.x=element_text(angle=90))+
      scale_y_continuous(labels = scales::comma)+
      labs(x='Регион',y='Цена (млн.тг)')
   
      
  })
  
  output$outPlotType<-renderUI({
    selectInput('plot_type', 'Выберите чарт', c('По регионам','По кузову'))
  })
  
output$selCars<-DT::renderDataTable(DT::datatable({
  brushedPoints(cars(), input$kz_brush, allRows=F)
},rownames = F,colnames = c("Модель",   "Цена",   "Год",    "Регион" , "Просмотров",   "Кузов",    "Объем")))




})

#rsconnect::deployApp(getwd(), appName = "kzcar")

exp(-12)
