#for the sake of simplicity and ease to read, I have used <block> tag to 
#start a block of code to execute a certain job
# title attribute of tag <block> stands for the description of what the 
#block of code is intended to do
#Enjoy


#<block title="install packages">

#install.packages("shiny")
#install.packages("shinyFiles")
#install.packages("shinydashboard")
#install.packages("threejs")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("knitr")
#install.packages("stringr")
#install.packages("DT")
#install.packages("extrafont")
#install.packages("treemap")
#install.packages("arules")
#install.packages("arulesViz")
#install.packages("plotly")
#install.packages("tools")
#install.packages("visNetwork")
#</block>

#<block title="load libraries">
library(shinydashboard)
library(shiny)
library(arules)
library(arulesViz)
library(threejs)
library(data.table)
library(dplyr)
library(ggplot2)
require(gridExtra)
library(knitr)
library(stringr)
library(DT)
library(extrafont)
library(scales)
library(treemap)
library(plotly)
library(visNetwork)
library(shinyFiles)
#library(shinyjs)

#font_import()
fonttable()
loadfonts(device = "win")
windowsFonts()
#</block>

#<block title="function for Human readable number to be used for formatting numbers for ease of readability">
# ref on github https://github.com/fdryan/R/blob/master/ggplot2_formatter.r
human_numbers <- function(x = NULL, smbl ="",  signif = 1  ){
  humanity <- function(y){
    
    if (!is.na(y)){
      tn <- round(abs(y) / 1e12, signif)
      b <- round(abs(y) / 1e9, signif)
      m <- round(abs(y) / 1e6, signif)
      k <- round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "k")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"m")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"bn")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "tn")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}

#' Human versions of large currency numbers - extensible via smbl

human_usd   <- function(x){human_numbers(x, smbl = "$")}
human_num   <- function(x){human_numbers(x, smbl = "")} 



#</block>

#<block title="set options of the Shiny Dashboard session">
options(shiny.launch.browser=T, shiny.minified=F, shiny.port = 4601, shiny.maxRequestSize=3000*1024^2)
#</block>

ui <- function(request) {
  dashboardPage(
    skin='green',
    dashboardHeader(title = "", titleWidth = 0),
    dashboardSidebar(
      uiOutput("sidebarControls"),
      sidebarMenuOutput("menu")
    ),
    dashboardBody(
      tags$head(tags$style(HTML('
        /* logo */
                                .skin-green .main-header .logo {
                                background-color: #1ABC9C;
                                }

                                /* logo when hovered */
                                .skin-green .main-header .logo:hover {
                                background-color: #1ABC9C;
                                }

                                /* navbar (rest of the header) */
                                .skin-green .main-header .navbar {
                                background-color: #1ABC9C;
                                }

                                .box.box-primary {
                                  border-top-color: #73C6B6;
                              }
                                 .shiny-output-error{color: ecf0f5;}                              
                                
                               
                                    '))),
      tabItems(
        tabItem("front",
               
         h3(tags$b("Welcome to Instacart's Market Basket Analysis Dashboard")),
         HTML('<p><img src="instacart.jpg", width="100%", height="70%" /></p>')
          
        )
        

          ,
        tabItem("upload",
                h3(tags$b("Uploading Tables..."))
        ),
        tabItem("tables",
                
                fluidRow(
                
                  box(
                    title = tags$b("Have a glance at the tables")
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                    ,width=12
                    ,h4(tags$b(textOutput("selected_table")))
                    ,tags$b(textOutput("txttbl"))
                    ,tags$head(tags$style("#txttbl{color: red;
                                 
                                 }"
                    )
                    )
                    ,dataTableOutput("tbl")
                    ,h6(tags$i("Maximum of 1,000 rows are displayed to avoid 'ERROR: [_parse_http_data] too many header bytes seen; overflow detected'"))
                  )
                )
        ),
        tabItem("tabApriori",
         
          
          h3(tags$b("Apriori")),
          uiOutput("tb")
          
        ),
       
        tabItem("tabPopular",
          fluidRow(
            
              box(
                title = tags$b("Popular Order Times")
                ,status = "primary"
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,h5(tags$b("What time of day do our customers order the most?"))
                ,h6(tags$i("Our Customers order most between 9:00AM and 5:00PM"))
                ,plotOutput("pltPopularOrderHrs", height = "300px")
              )
              
              ,box(
                title = tags$b("Popular Order Days")
              
                ,status = "primary"
                ,solidHeader = FALSE 
                ,collapsible = TRUE 
                ,h5(tags$b("Which day of week do our customers like to shop the most?"))
                ,h6(tags$i("People love grocery shopping on Sundays and Mondays"))
                ,plotOutput("pltPopularOrderDays", height = "300px")
              ) 
            
              )
            ),
        tabItem("tabReorder",
                fluidRow(
                  
                  box(
                    title = tags$b("Percentage of Reorders")
                  #  ,style='padding:0px;'
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                  #  ,h5(tags$b("How often do people order the same items again?"))
                    ,h6(tags$i("on Average, 60% of items are reordered"))
                    ,plotOutput("pltReorderSameItem", height = "130px")
                  )
                  
                  ,box(
                    title = tags$b("Period to Reorder")
                  #  ,style='padding:0px;'
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                  #  ,h5(tags$b("How Long does it take for a customer to reorder?"))
                    ,h6(tags$i("Most of our customers order again either in less than 10 days or once a month"))
                    ,plotOutput("pltPeriodToReorder", height = "130px")
                  ) 
                  
                ),
                fluidRow(
                  box(
                    title = tags$b("Most Often Ordered Items")
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                  #  ,h5(tags$b("How often do people order the same items again?"))
                   # ,h6(tags$i("on Average, 60% of items are ordered"))
                    ,plotOutput("pltOftenOrdered", height = "250px")
                  )
                  ,box(
                    title = tags$b("Probability of Reordering")
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                    ,h5(tags$b("Association between time of last order and probability of reorder"))
                     ,h6(tags$i("Customers who order frequently, buy more of the same items"))
                    ,plotOutput("pltAssocRecencyReorder", height = "200px")
                  )
                 
                  
                )
                
                
                
                
        ),
        tabItem("tabCustomerBehavior",
                fluidRow(
                  
                  box(
                    title = tags$b("Number of Items per Order")
                    #  ,style='padding:0px;'
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                  #  ,h5(tags$b("How Many items do people usually buy"))
                   # ,h6(tags$i("on Average, 60% of items are ordered"))
                    ,plotOutput("pltNumberofItems", height = "200px")
                  )
                  
                  ,box(
                    title = tags$b("Best Selling Products")
                    #  ,style='padding:0px;'
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                 #   ,h5(tags$b("What are our Bestselling Products?"))
                  #  ,h6(tags$i("Most of our customers order again either in less than 10 days or once a month"))
                    ,plotOutput("pltBestsellingProducts", height = "200px")
                  ) 
                           
                ),
                fluidRow(
                  box(
                    title = tags$b("Items put first into Cart")
                    ,status = "primary"
                    ,solidHeader = FALSE 
                    ,collapsible = TRUE 
                    #  ,h5(tags$b("How often do people order the same items again?"))
                    # ,h6(tags$i("on Average, 60% of items are ordered"))
                    ,plotOutput("pltFirsttoCart", height = "250px")
                  )
                 
                  
                )),
                tabItem("tabProductPortfolio",
                        fluidRow(
                          box(
                            title = tags$b("Product Portfolio")
                            #  ,style='padding:0px;'
                            ,status = "primary"
                            ,solidHeader = FALSE 
                            ,collapsible = TRUE
                            ,width=12
                            #  ,h5(tags$b("How Many items do people usually buy"))
                            ,plotOutput("pltTreemap", height = "600px", width = "100%")
                            ,h6(tags$i("The size of the boxes shows the volume of sales."))
                          )
                        )
                
                
                )     
                
        )
       
      )
    )
  
}


server <- function(input, output, session) {
  
  #<block title="create a button to choose the folder from which the csv files will be uploaded > "
  #each csv is read to a dataframe and at the same time basic data cleaning is done to convert Character variables to factors
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  orders<-reactive({
    setwd(global$datapath)
    file <- 'orders.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('orders.csv')
    tbl<- tbl %>%mutate(order_hour_of_day= as.numeric(order_hour_of_day),eval_set=as.factor(eval_set))
   
    return(tbl)
  })
  aisles<-reactive({
    setwd(global$datapath)
    file <- 'aisles.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('aisles.csv')
    tbl<- tbl %>%mutate(aisle=as.factor(aisle))
    return(tbl)
  })
  products<-reactive({
    setwd(global$datapath)
    file <- 'products.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('products.csv')
    tbl<- tbl %>%mutate(product_name=as.factor(product_name))
   
    return(tbl)
  })
  order_products<-reactive({
    setwd(global$datapath)
    file <- 'order_products__train.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('order_products__train.csv')
   
    return(tbl)
  })
  order_products_prior<-reactive({
    setwd(global$datapath)
    file <- 'order_products__prior.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('order_products__prior.csv')
    return(tbl)
  })
  departments<-reactive({
    setwd(global$datapath)
    file <- 'departments.csv'
    if(!file.exists(file)){
      return (NULL) 
    }
    tbl<-fread('departments.csv')
    tbl <- tbl %>%mutate(department=as.factor(department))
   
    return(tbl)
  })


  
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
               })
  #</block>  
  #<block title = "defining menu items" and customizing speicific controls to show on a couple of menu items">
  output$menu <- renderMenu({
    sidebarMenu(id = "smenu",
      menuItem(tags$b("Instacart"), tabName = "front", icon = icon("home", lib = "font-awesome")),
      menuItem(tags$b("Upload Tables"), tabName = "upload", icon = icon("upload", lib = "font-awesome")),
      menuItem(tags$b("Tables"), tabName = "tables", icon = icon("table", lib = "font-awesome")),
     
      menuItem(tags$b("Descriptive Data Analysics"), tabName="DDA", icon = icon("chart-bar", lib = "font-awesome"),
      #  helpText(""),
        menuSubItem("Popular Order Times", "tabPopular"),
        menuSubItem("All About Reorders", "tabReorder"),
        menuSubItem("Purchasing Behaviour", "tabCustomerBehavior"),
        menuSubItem("Product Portfolio", "tabProductPortfolio")
      ),
      menuItem("Prescriptive Data Analytics", tabName="PDA", icon = icon("brain", lib = "font-awesome"),
       menuSubItem("Apriori", "tabApriori")
               
      )
      
    )
  })
  output$sidebarControls <- renderUI({
    req(input$smenu)
    
   if (input$smenu == "tables") {
      tagList(
        selectInput("displaytable", "Choose Table to display", c("orders","products","order_products","order_products_prior","aisles","departments"), selected = "aisles")

      )
   } else if (input$smenu =="upload"){
     tagList(
       helpText(" Choose the folder of the csv files"),
       shinyDirButton("dir", "Choose Folder", "Upload")
    
      
     )
      
    } else NULL
  })
  


#</block>

  
#<block  title= "processing Apriori Algorithm">
transactiondownload <- reactive ({
  tmpproduct <- products() %>% group_by(department_id, aisle_id) %>% 
                left_join(departments(),by="department_id")
  tmpproduct <- tmpproduct %>% left_join(aisles(),by="aisle_id")
  
  tmp <-order_products() %>% 
    group_by(product_id) %>% 
    left_join(tmpproduct,by="product_id")
 
  write.csv(tmp, file = "transactions.csv")
})
  rules<- reactive ({
   
   # tmpproduct <- products() %>% group_by(department_id, aisle_id) %>% 
   #               left_join(departments(),by="department_id")
   # tmpproduct <- tmpproduct %>% left_join(aisles(),by="aisle_id")
   # 
   # tmp <-order_products() %>% 
   #   group_by(product_id) %>% 
   #   left_join(tmpproduct,by="product_id")
  #
   # write.csv(tmp, file = "transactions.csv")
    req(input$level)
    if(input$level=="Products"){
      transactions<-read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,6)) 
    }else if (input$level=="Aisles"){
      transactions<-read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,9)) 
    }else {
      transactions<-read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,10)) 
    }
    groceryrules <- apriori(transactions, parameter = list(support = input$supp, confidence = input$conf, minlen=2, maxlen=input$maxL))
    
  })
  #to get top n input 
  nR <- reactive({
    nRule <- input$Topn
  })
  output$AprioriAlgorithm<-renderPrint({
   ar<-rules()
  })
  output$AprioriAlgorithmSummary<-renderPrint({
    ar<-rules()
    summary(ar)
  })
  
  output$AssociationRules<-renderPrint({
    ar<-rules()
    inspect(sort(ar, by=input$sort)[1:nR()])
  })
  

  
  output$pltAssociationRules <-renderPlotly  ({
    ar<-rules()
    plot (ar,  method = "scatter" , engine = "htmlwidget",colors = c("#C70039", "#DAF7A6"))
      
  })
  output$pltAssociationRulesGraph <-renderVisNetwork ({
    ar<-rules()
    topnrules<- sort(ar, by=input$sort)[1:nR()]
      plot (topnrules,  method = "graph" , interactive=T, engine = "htmlwidget")
    
  })
  
  output$marketinginsights<-renderUI({
    ar<-rules()
    topnrules<- sort(ar, by=input$sort)[1:nR()]
    df<-data.frame(lhs=labels(lhs(topnrules)), rhs=labels(rhs(topnrules)), text=paste("People who usually buy <b>", labels(lhs(topnrules)), "</b> also buy <b>", rhs=labels(rhs(topnrules)), "</b>" ))
   # topnrules <- mutate(topnrules, text = paste("This is test" , labels(lhs(topnrules))))
    HTML(paste(df[,3], collapse="<br/><br/>"))
    
  })
# <block>
  
  
#<block title="Produce ggplots for the descriptive data analytics part">

  output$pltPopularOrderHrs<- renderPlot({
    PopularOrderHrs<- orders() %>%
      ggplot(aes(x=order_hour_of_day)) +
      geom_histogram(stat="count", fill="#158799") +
      theme_classic() +
      labs(title ="", x="Hour of Day", y="") 
    
    PopularOrderHrs<- PopularOrderHrs + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
     # plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
     # plot.background = element_rect(fill="#fff", color="#fff"),
      #panel.background = element_rect(fill="#fff", color="#fff")
      ##ecf0f5
    )
    PopularOrderHrs<- PopularOrderHrs + scale_y_continuous(labels = human_num)
    PopularOrderHrs
  })

  output$pltPopularOrderDays<-renderPlot({
    PopularOrderDays<- orders() %>%
      ggplot(aes(x=order_dow)) +
      geom_histogram(stat="count", fill="#F97F59") +
      theme_classic() +
      labs(title ="", x="day of week", y="") 
    
    PopularOrderDays<- PopularOrderDays + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 8),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
    #  plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
   #   plot.background = element_rect(fill="#fff", color="#fff"),
    #  panel.background = element_rect(fill="#fff", color="#fff")
    )
    PopularOrderDays<- PopularOrderDays + scale_y_continuous(labels = human_num) + 
      scale_x_continuous(breaks=seq(0,6,1),labels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
    PopularOrderDays
  })
  output$pltPeriodToReorder<-renderPlot({
    PeriodtoReorder<- orders() %>%
      ggplot(aes(x=days_since_prior_order)) +
      geom_histogram(stat="count", fill="#EA4F5C") +
      theme_classic() +
      labs(title ="", x="Number of days", y="") 
    
    PeriodtoReorder<- PeriodtoReorder + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
   #   plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    PeriodtoReorder<- PeriodtoReorder + scale_y_continuous(labels = human_num) 
    PeriodtoReorder
  })
  output$pltReorderSameItem<-renderPlot({
    table_reordersameitem <- order_products() %>% 
      group_by(reordered) %>% 
      summarize(count = n()) %>% 
      mutate(reordered = as.factor(reordered)) %>%
      mutate(proportion = count/sum(count))
    levels(table_reordersameitem$reordered)[levels(table_reordersameitem$reordered)==0] <- "No"
    levels(table_reordersameitem$reordered)[levels(table_reordersameitem$reordered)==1] <- "Yes"
    kable(table_reordersameitem)
    
    
    Reordersameitem<- table_reordersameitem %>% 
      ggplot(aes(x="",y=count,fill=reordered))+
      geom_bar( stat="identity")+
      coord_polar("y", start=0) +
      theme_void()+
      geom_text(aes(label=percent(proportion)),position = position_stack(vjust = 0.5),family="calibri", face="bold", color="#404040", size=3) +
      labs(title ="", y="", x="")+
      scale_fill_manual(values = c("#EA4F5C" , "#A4E55E"))
    
    Reordersameitem
    Reordersameitem<- Reordersameitem + theme(
     # plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0),
      legend.text = element_text(family="calibri", face="bold", color="#404040", size=11, hjust=0),
      legend.title = element_text(family="calibri", face="bold", color="#404040", size=12, hjust=0)
      
    )
    
    Reordersameitem
  })
  output$pltOftenOrdered<-renderPlot({
    table_oftenordered <-order_products() %>% 
      group_by(product_id) %>% 
      summarize(proportion_reordered = mean(reordered), n=n()) %>% 
      filter(n>40) %>% 
      top_n(10,wt=proportion_reordered) %>% 
      arrange(desc(proportion_reordered)) %>% 
      left_join(products(),by="product_id")
    
   # kable(table_oftenordered)
    
    OftenOrdered<- table_oftenordered %>% 
      ggplot(aes(x=reorder(product_name,proportion_reordered), y=proportion_reordered))+
      geom_bar(position="dodge", stat="identity",fill="#A4E55E")+
      coord_flip() +
      theme_classic() +
      labs(title ="", y="% of average item reorder", x="")
    
    OftenOrdered
    OftenOrdered<- OftenOrdered + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
   #   plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    OftenOrdered<- OftenOrdered + scale_y_continuous(labels = function(x) paste0(x*100, "%"))  
    OftenOrdered
  })
  
  output$pltAssocRecencyReorder<-renderPlot({
    AssocRecencyReorder<- order_products() %>% 
      left_join(orders(),by="order_id") %>% 
      group_by(days_since_prior_order) %>%
      summarize(mean_reorder = mean(reordered)) %>%
      ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
      geom_bar(stat="identity",fill="#EA4F5C")+
      theme_classic() +
      labs(title ="", y="Average Reorder Ratio", x="Number of Days between two orders")
    
    AssocRecencyReorder
    AssocRecencyReorder<- AssocRecencyReorder + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0),
      axis.title.y = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
  #    plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    #AssocRecencyReorder<- AssocRecencyReorder + scale_y_continuous(labels = function(x) paste0(x*100, "%"))  
    AssocRecencyReorder
    
    
  })
  output$pltNumberofItems<-renderPlot({ 
    order_products_all<-rbind(order_products(), order_products_prior())
    NumberofItems<- order_products_all %>%
      group_by(order_id) %>%
      summarize(n_items = last(add_to_cart_order)) %>%
      ggplot(aes(x=n_items)) +
      geom_histogram(stat="count", fill="#A4E55E") +
      theme_classic() +
      labs(title ="", x="Number of items", y="") 
    
    NumberofItems<- NumberofItems + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
    #  plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    NumberofItems<- NumberofItems + scale_y_continuous(labels = human_num)   + scale_x_continuous(breaks=seq(0,60,10), limits = c(0, 60)) 
    NumberofItems
  })
  
  output$pltBestsellingProducts<-renderPlot({ 
    table_BestSellingProducts <- order_products() %>% 
      group_by(product_id) %>% 
      summarize(count = n()) %>% 
      top_n(10, wt = count) %>%
      left_join(select(products(),product_id,product_name),by="product_id") %>%
      arrange(desc(count)) 
    kable(table_BestSellingProducts)
    
    
    
    BestsellingProducts<- table_BestSellingProducts %>%
      ggplot(aes(x=reorder(product_name,count), y=count))+
      geom_bar(position="dodge", stat="identity",fill="#F9CF59")+
      coord_flip() +
      theme_classic() +
      labs(title ="", y="Number of units sold", x="") 
    
    BestsellingProducts<- BestsellingProducts + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
    #  plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    BestsellingProducts<- BestsellingProducts + scale_y_continuous(labels = human_num)    
    BestsellingProducts
  })
  
  output$pltFirsttoCart<-renderPlot({ 
    table_FirsttoCart <- order_products() %>% 
      group_by(product_id, add_to_cart_order) %>% 
      summarize(count = n()) %>% mutate(pct=count/sum(count)) %>% 
      filter(add_to_cart_order == 1, count>10) %>% 
      arrange(desc(pct)) %>% 
      left_join(products(),by="product_id") %>% 
      select(product_name, pct, count) %>% 
      ungroup() %>% 
      top_n(10, wt=pct)
    
   # kable(table_FirsttoCart)
    
    
    FirsttoCart<- table_FirsttoCart %>% 
      ggplot(aes(x=reorder(product_name,pct), y=pct))+
      geom_bar(position="dodge", stat="identity",fill="#EA4F5C")+
      coord_flip() +
      theme_classic() +
      labs(title ="", y="% of times item added First to Cart", x="")
    
    FirsttoCart
    FirsttoCart<- FirsttoCart + theme(
      axis.text.x = element_text(family="calibri", face = "bold", color = "#747474", size = 12),
      axis.text.y = element_text(family="calibri", face = "bold", color = "#747474", size = 10),
      axis.line.y = element_line( linetype = "blank"),
      axis.line.x = element_line( linetype = "solid", color="#747474"),
      axis.title.x = element_text(family="calibri", face="italic", color= "#747474", size =10, hjust=1, vjust=0)
 #     plot.title= element_text(family="calibri", face="bold", color="#404040", size=15, hjust=0)
    )
    FirsttoCart<- FirsttoCart + scale_y_continuous(labels = function(x) paste0(x*100, "%"))  
    FirsttoCart
    
  }) 
 
  output$pltTreemap<-renderPlot({ 
    tmp <- products() %>% group_by(department_id, aisle_id) %>% summarize(n=n())
    tmp <-  tmp %>% left_join(departments(),by="department_id")
    tmp <- tmp %>% left_join(aisles(),by="aisle_id")
    
    tmp2<-order_products() %>% 
      group_by(product_id) %>% 
      summarize(count=n()) %>% 
      left_join(products(),by="product_id") %>% 
      ungroup() %>% 
      group_by(department_id,aisle_id) %>% 
      summarize(sumcount = sum(count)) %>% 
      left_join(tmp, by = c("department_id", "aisle_id")) %>% 
      mutate(onesize = 1)
    #The size of the boxes shows the number of products in each category.
    #  treemap(tmp,index=c("department","aisle"),vSize="n",title="",palette="Set2",border.col="#FFFFFF")
    #The size of the boxes shows the number of sales.
    treemap(tmp2,index=c("department","aisle"),vSize="sumcount",title="",palette="Set3",border.col="#FFFFFF")
  })
# </block> 
  
#<block title="Displaying tables">
  #To display the name of the table chosen to be displayed
  output$selected_table <- renderText({
   #  paste0(toupper(substr(input$displaytable, 1, 1)), tolower(substring(input$displaytable, 2)))
    paste0( gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(gsub("\\_", " ", input$displaytable)), perl=TRUE))
  })
 
  #displaying the table
  output$tbl <- renderDT(
    #tried alot to handle the error in case data is not uploaded, couldn't find the right solution
    
    head(get(input$displaytable)(),1000),
    class = "display nowrap compact", # style
    filter = "top"
    
  )
  #output text in case the chosen file doesn't contain the csv files we are searching for
  output$txttbl <-renderText({
    if(is.null(get(input$displaytable)()))
      paste0("DANGER!! Please make sure to first upload files!!!!")
  })
    
#</block>    
  
#<block title="Apriori tabPanel is generated here"> 
 
  
 output$tb <- renderUI({
    fluidRow(  
      box(
        title = tags$b("Algorithm Controls ")
        ,style='margin:10px;'
        ,status = "primary"
        ,solidHeader = FALSE 
        ,collapsible = FALSE 
        ,width=3
        ,sidebarPanel(
         
          width="100%",
         # radioButtons('samp', label='Rules to display', choices=c('All Rules', 'Top N'), inline=T, selected="All Rules"),
          numericInput(inputId = "Topn", label="Top N Rules", value=10, min=2, width="100px"),
          selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support'), width="120px"), hr(),
          selectInput('level', label='Association by:', choices = c('Products', 'Aisles', 'Departments'), width="120px"), hr(),
          sliderInput("supp", "Support:", min = 0, max = 1, value = 0.001 , step = 1/10000), 
          sliderInput("conf", "Confidence:", min = 0, max = 1, value = 0.25 , step = 1/10000),
          sliderInput("maxL", "Max Items per set:", min=2, max=4, value=2, step = 1), 
  
          br()
        )),
      box(
        title = tags$b("Algorithm  ")
        ,style='margin:10px;'
        ,status = "primary"
        ,solidHeader = FALSE 
        ,collapsible = TRUE 
        ,width=9
       ,tabsetPanel(
         tabPanel("Marketing Insights", 
                  htmlOutput("marketinginsights")
         ),
        tabPanel("Association Rules Scatterplot", 
               plotlyOutput("pltAssociationRules", height = "400px")
                 ),
       tabPanel("Association Rules Graph", 
               
                visNetworkOutput("pltAssociationRulesGraph", height = "400px")
                ),
     #  tabPanel("Transaction Type Dataform", verbatimTextOutput("vertabimApriori"), verbatimTextOutput("verbatimTransactionFirst3")),
     #  tabPanel("Apriori", verbatimTextOutput("AprioriAlgorithm"), verbatimTextOutput("AprioriAlgorithmSummary")),
       tabPanel("Association Rules", verbatimTextOutput("AssociationRules"))
       
     ))
       )
 })
#</block>
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
#save transactions.csv once user goes to Apriori tab
  observeEvent(input$smenu,
               {
                 if(input$smenu=='tabApriori')
                 {
                   transactiondownload()
                 }
                 
               })
  

}

enableBookmarking("url")
shinyApp(ui, server)
