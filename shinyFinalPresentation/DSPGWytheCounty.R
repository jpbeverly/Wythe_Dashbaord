library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(readr)
library(collapsibleTree)
library(tidyverse)
library(viridis)

source("theme.R")
Tree<-read_csv("Tree")
Tree_Ed<-read_csv("Tree_Ed")
Wythe_long<-read_csv("Wythe_long")


shinyApp(
  ui = dashboardPagePlus(
    title = "DashboardPage",
    header = dashboardHeaderPlus(
      title = "DSPG 2020"
    ),
    
    # SIDEBAR (LEFT) ----------------------------------------------------------
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(
          tabName = "overview",
          text = "Project Overview",
          icon = icon("info circle")
        ),
        menuItem(
          tabName = "capital",
          text = "Human and Built Capital",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "jobs",
          text = "Jobs of Tomorrow",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "both",
          text = "Accessibility",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "data",
          text = "Regional Comparisons",
          icon = icon("database")
        ),
        menuItem(
          tabName = "data",
          text = "Data & Methodology",
          icon = icon("database")
        ),
        menuItem(
          tabName = "findings",
          text = "Findings",
          icon = icon("chart-pie")
        ),
        menuItem(
          tabName = "team",
          text = "Team",
          icon = icon("user-friends")
        )
      )
    ),
    
    # BODY --------------------------------------------------------------------
    body = dashboardBody(
      customTheme,
      fluidPage(
        tabItems(
          tabItem(tabName = "overview",
                  fluidRow(
                    boxPlus(
                      title = "Project Overview",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h1("Industry and Workforce Attraction and Retention in Wythe County"),
                      h2("Project Description"),
                      p("The VT-DSPG used publicly available data to identify and classify the unique community, work force, and industry amenities that support economic development initiatives in Wythe county."),
                      img(src = 'Wythe.png', height = "150", width = "300", style="display: block; margin-left: auto; margin-right: auto;"),
                      h2("Project Goals"),
                      p("Contextualize industry and workforce factors at levels that are actionable for stakeholders and that promote informed policy and investment in Wythe"),
                      h2("Our Approach"),
                      p("Our research team used the community capital framework to identify the built and human capital available in Wythe
                        and surrounding counties that directly relate to industry location factors. We created composite industry and 
                        workforce attractiveness measures to compare Wythe County to other counties in the region."),
                      img(src = 'Approach.png', height = "300", width = "500", style="display: block; margin-left: auto; margin-right: auto;")
                      )
                    )),
          tabItem(tabName = "capital",
                  fluidRow(
                    boxPlus(
                      title = "Wythe County",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = NULL,
                      enable_dropdown = TRUE,
                      dropdown_icon = "",
                      dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                      plotOutput("myplot")
                    ),
                    p("Add facts/figures, graphs, maps to represent capital in other ways.  Would this be a place for Afrina's location data? Or should that have its own tab?"),
                    br()
                  )),
          tabItem(tabName = "jobs",
                  fluidRow(
                    boxPlus(
                      title = "Industries and Knowledge Areas",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = "100%",
                      enable_sidebar = FALSE,
                      enable_dropdown = TRUE,
                      dropdown_icon = "",
                      dropdown_menu = tagList(selectInput("var1","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                      collapsibleTreeOutput("mytree",width = "100%")
                    ),
                    p("This graph maps the occupations with highest projected openings (2018-2028) onto required areas of knowledge."),
                    br()
                  )),
          tabItem(tabName = "both",
                  fluidRow(
                    boxPlus(
                      title = "Interactive Graph",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 6,
                      enable_sidebar = TRUE,
                      sidebar_width = 25,
                      sidebar_start_open = TRUE,
                      sidebar_content = tagList(sliderInput(
                        "obs2",
                        "Number of observations:",
                        min = 0,
                        max = 1000,
                        value = 500
                      )
                      ),
                      plotOutput("distPlot2"),
                      footer = "Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."
                    ),
                    boxPlus(
                      title = "Interactive Map",
                      closable = FALSE,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 6,
                      enable_sidebar = TRUE,
                      sidebar_width = 25,
                      sidebar_start_open = TRUE,
                      sidebar_content = tagList(
                        p(),
                        actionButton("recalc2", "Click Me!")
                      ),
                      leafletOutput("mymap2"),
                      footer = "Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."
                    )
                  )),
          tabItem(tabName = "data",
                  fluidRow(
                    boxPlus(
                      title = "Data & Methodology",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("Data Sources"),
                      img(src = "data_sets.png", width = "450px", align = "right"),
                      h3("Data Source 1"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Source 2"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Source 3"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h2("Methodology"),
                      h3("Data Preparation"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Data Modeling"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
                    )
                  )),
          tabItem(tabName = "findings",
                  fluidRow(
                    boxPlus(
                      title = "Findings",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("Summary of Findings"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section One"),
                      img(src = "irrational_venn_diagram.png", width = "360px", align = "right"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section Two"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                      h3("Results Section Three"),
                      img(src = "food_reality_chart.png", width = "400px", align = "right"),
                      p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
                    )
                  )),
          tabItem(tabName = "team",
                  fluidRow(
                    boxPlus(
                      title = "Findings",
                      closable = FALSE,
                      width = NULL,
                      status = "warning",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      h2("DSPG Team Members"),
                      img(src = 'Josh.Beverly.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Dylan.Glover.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Afrina.Tabassum.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Adam.Wells.VT.jpg', height = "150", width = "140", align = "center"),
                      p("Josh Beverly, Fellow (Ph.D. Student at Virginia Tech, Agricultural and Applied Economics)"),
                      p("Dylan Glover, Intern (Undergraduate Student at Virginia Tech, Mathematics)"),
                      p("Afrina Tabassum, Intern (Ph.D. Student at Virginia Tech, Computer Science)"),
                      p("Adam Wells, Intern (Master Student at Virginia Tech, Data Analysis and Applied Statistics)"),
                      h2("Virginia Tech Faculty Team Members"),
                      img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Conaway.Haskins.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Matt.Holt.VT.jpg', height = "150", width = "140", align = "center"),
                      img(src = 'Ford.Ramsey.VT.jpg', height = "150", width = "140", align = "center"),
                      p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
                      p("Conaway Haskins (Extensions Specialist, Rural & Regional Development)"),
                      p("Matt Holt (Department Head, Professor, Agribusiness, Applied Econometrics, Principal Investigator)"),
                      p("Ford Ramsey (Assistant Professor, Agribusiness, DSPG Project Co-Lead)"),
                      h2("Project Sponsors"),
                      img(src = 'VCE.Logo.png', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                      p("Matthew Miller (Unit Coordinator and Extension Agent, Agriculture and Natural Resources - Farm Business Management)"),
                      
                      h2("Acknowledgements"),
                      p("We would like to thank:"),
                      p("Stephen Bear (Wythe County Administrator),"),
                      p("David Manely (Joint Industrial Development Authority of Wythe County)"),
                      p("John Matthews (Economic Developer & Associate Director at the Joint IDA of Wythe County)")
                    )
                  ))
          )
                  ))
    ),
  
  
  # SERVER ------------------------------------------------------------------
  server = function(input, output) {
    # Render Plot 1
    output$mytree <- renderCollapsibleTree({
      if(input$var1%in%"Skills"){
        Tree%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
      }
      
      
      else if(input$var1%in%"Education"){
        Tree_Ed%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else if(input$var1%in%"Experience Needed"){
        Tree_Ex%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else if(input$var1%in%"On-Site Training"){
        Tree_Site%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
      else {
        Tree_Job%>%
          filter(Job_Openings>5300)%>%
          mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
          filter(Importance>=2.88)%>%
          group_by(Occupation)%>%
          collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Category_Description"),
                          root="Industries",
                          attribute = "Job_Openings",
                          width=1800,
                          zoomable=F)
        
      }
    })
    
    
    # Render Plot 2
    output$myplot <- renderPlot({
      if(input$var%in%"Household Size"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=name,y=value,fill=name))+
          geom_col()+
          scale_fill_viridis(discrete = T)+
          labs (title="Household Size",y="Households",x="")+
          theme_minimal()+
          theme(legend.position = "none")
      }
      else if (input$var%in%"Level of Education"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_col()+
          scale_fill_viridis(discrete = T)+
          labs (title="Highest Level of Education (Age > 25)",y="Population",x="")+
          theme_minimal()+
          theme(legend.position = "none")
      }
      else if (input$var%in%"Household Income"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Household Income",y="Households",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
      else if (input$var%in%"Home Values"){
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Home Values",y="Number of Homes",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
      else{
        Wythe_long%>%filter(Variable%in%input$var)%>%
          ggplot(mapping=aes(x=name,y=value,fill=name))+
          geom_bar(stat = "identity")+
          scale_fill_viridis(discrete = T)+
          labs (title="Employment by Industry",y="Population",x="")+
          theme_minimal()+
          theme(legend.position = "none",axis.text.x =element_text(angle=45,hjust=1,vjust=1))
      }
    })
    
    # Create Map Points 1
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 1
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points())
    })
    
    # Create Map Points 2
    points2 <- eventReactive(input$recalc2, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 2
    output$mymap2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points2())
    })
    
  }
  )
