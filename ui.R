
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
shinyUI(
  
  fluidPage(theme='bootstrap.css',
                         

  # Application title
  tags$head(HTML('<meta property="og:image" content="https://gringo.shinyapps.io/kzcar/ggplotGuide-007.png">
                  <title>CF with Shiny and R</title>
                  <meta name="description" content="Yet another CF example based on kolesa.kz data">
                 <script src="likely.js"></script>
                 <link rel="stylesheet" href="likely.css">')),

  # Sidebar with a slider input for number of bins
  navbarPage('',
             tabPanel('Машины',
  sidebarLayout(
    sidebarPanel(
      uiOutput('outSelectRegion'),
      uiOutput('outSliderYear'),
      uiOutput('outSliderPrice'),
      conditionalPanel('input.tabs=="Raw data"',selectInput('n','Количество рекомендаций',c(5,10,15))),
      conditionalPanel('input.tabs=="Plots"',uiOutput('outPlotType')),
      uiOutput('outSelectType'),
      tags$div(style="vertical-align: top",
      tags$div(style="float:left;margin-right:5px",HTML('<script src="//platform.linkedin.com/in.js" type="text/javascript"> lang: en_US</script>
<script type="IN/Share"></script>')),
      
    tags$div(class="likely", checked=NA,
             tags$div(class='twitter',""),
             tags$div(class='facebook',""),
             tags$div(class='gplus',""),
             tags$div(class='vkontakte',"")
            ))
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='tabs',
        tabPanel('Raw data',DT::dataTableOutput("table"),
                 tags$div(style="background-color:lightblue",  
                          tags$h3('Похожие машины')),
                 DT::dataTableOutput('sim_cars')),
        
        tabPanel('Plots', 
                 conditionalPanel('input.plot_type=="По кузову"',plotOutput('priceDist',brush="kz_brush")),
                 conditionalPanel('input.plot_type=="По регионам"',plotOutput('regDist',brush="kz_brush")),
                 DT::dataTableOutput('selCars')
                 )
      )
      
      
      
    ))),
  tabPanel('Квартиры','we are still working on it.'),
  navbarMenu('More',
             tabPanel('Contact', "shakirov.oraz@gmail.com"),
             tabPanel('Info',
                      tags$p(
                        "R+Shiny+DropBox"
                      )))
  )
))