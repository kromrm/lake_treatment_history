
# SHINY: Plot of species treated by year ----

ploty_base <- readRDS("ploty_base.rds")


library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridis)

ploty_base <- ploty_base %>% mutate(
  MDNRID=as.character(MDNRID),
  EGLE_WB_ID=as.character(EGLE_WB_ID))

ui <- fluidPage(
  tags$script(HTML("$(document).on('click','#clear',function(){location.reload();});")),
  titlePanel("Aquatic Plant Treatment by Waterbody"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county","County:",NULL),
      selectInput("lake","Waterbody:",NULL),
      selectInput("mdnrid","MDNRID:",NULL),
      selectInput("egle","EGLE WB ID:",NULL),
      actionButton("clear","Clear Filters")),
    mainPanel(plotlyOutput("treated_plot",height="700px"))))

server <- function(input,output,session) {
  
  base <- function() list(
    county=sort(unique(na.omit(ploty_base$County))),
    lake=sort(unique(na.omit(ploty_base$Waterbody_TrimmedName))),
    mdnrid=sort(unique(na.omit(ploty_base$MDNRID))),
    egle=sort(unique(na.omit(ploty_base$EGLE_WB_ID))))
  
  selected <- function(x) !is.null(x) && length(x)==1 && !is.na(x) && nzchar(x)
  
  observeEvent(TRUE,{
    x <- base()
    for(id in names(x))
      updateSelectInput(session,id,choices=c("All"="",x[[id]]),selected="")
  },once=TRUE)
  
  filter_data <- function(exclude=NULL) {
    d <- ploty_base
    if(!identical(exclude,"county") && selected(input$county))
      d <- d[d$County==input$county,]
    if(!identical(exclude,"lake") && selected(input$lake))
      d <- d[d$Waterbody_TrimmedName==input$lake,]
    if(!identical(exclude,"mdnrid") && selected(input$mdnrid))
      d <- d[d$MDNRID==input$mdnrid,]
    if(!identical(exclude,"egle") && selected(input$egle))
      d <- d[d$EGLE_WB_ID==input$egle,]
    d
  }
  
  update_field <- function(id,col,exclude) {
    d <- filter_data(exclude)
    x <- sort(unique(na.omit(d[[col]])))
    old <- isolate(input[[id]])
    sel <- if(selected(old) && old %in% x) old else if(length(x)==1) x else ""
    updateSelectInput(session,id,choices=c("All"="",x),selected=sel)
  }
  
  observeEvent(input$county,{
    update_field("lake","Waterbody_TrimmedName","lake")
    update_field("mdnrid","MDNRID","mdnrid")
    update_field("egle","EGLE_WB_ID","egle")
  },ignoreInit=TRUE)
  
  observeEvent(input$lake,{
    update_field("county","County","county")
    update_field("mdnrid","MDNRID","mdnrid")
    update_field("egle","EGLE_WB_ID","egle")
  },ignoreInit=TRUE)
  
  observeEvent(input$mdnrid,{
    update_field("county","County","county")
    update_field("lake","Waterbody_TrimmedName","lake")
    update_field("egle","EGLE_WB_ID","egle")
  },ignoreInit=TRUE)
  
  observeEvent(input$egle,{
    update_field("county","County","county")
    update_field("lake","Waterbody_TrimmedName","lake")
    update_field("mdnrid","MDNRID","mdnrid")
  },ignoreInit=TRUE)
  
  output$treated_plot <- renderPlotly({
    req(selected(input$county),selected(input$lake),
        selected(input$mdnrid),selected(input$egle))
    
    d <- filter_data()
    
    validate(need(nrow(d)>0,"No treatment data match the selected filters."))
    
    p <- ggplot(d,aes(
      Year,Annual_Acres,color=Treated_Species,group=Treated_Species,
      text=paste0(
        "Species: ",Treated_Species,
        "<br>Year: ",Year,
        "<br>Annual Acres: ",Annual_Acres)))+
      geom_line(linewidth=.8)+
      geom_point(size=2.5)+
      scale_color_viridis_d(option="D")+
      scale_x_continuous(breaks=sort(unique(d$Year)))+
      labs(
        x="Year",
        y="Annual Acres Treated",
        color="Treated Species",
        title=paste(unique(d$Waterbody_TrimmedName), "Lake,", unique(d$County),"County: Species Treated by Acres per Year"))+
      theme(
        legend.position="right",
        plot.title=element_text(hjust=.5,size=18),
        axis.text.x=element_text(angle=45,hjust=1))
    
    ggplotly(p,tooltip="text")
  })
}

shinyApp(ui,server)
