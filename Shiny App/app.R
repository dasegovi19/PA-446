source("library.R")






ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Life Expectancy in Chicago 2019",
                  titleWidth = 2000),  # close header
  
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Intro + map", tabName = "map_tab", icon = icon("map")),
      menuItem("Plots", tabName = "plots_tab", icon = icon("tablet")),
      menuItem("Bargraph and Table", tabName = "bargraph_tab", icon = icon("tablet")),
      menuItem("Conclusion", tabName = "conclusion_tab", icon = icon("tablet"))
    ) # close side bar Menu
  ), # Close SideBar
  
  dashboardBody(
    tabItems(
      ## set up the individual tab Items
      
      ### map starts
      tabItem(
        tabName = "map_tab",
        titlePanel("Hi Chicago resident! Thank you for visiting this page. Are you interested to learn more about why some communities have higher life expectancies than others? Check out this app."),
        fluidRow(
          # main diagram
          box(
            title = "Please select your community area to get started!",
            status = "primary",
            width = 12,
            selectInput(inputId = "community_input",
                        label = "Where do you live?",
                        choices = name),
          ), # closes 1st box
          box(
            title = "Your community's life expectancy(years)",
            width = 12,
            valueBoxOutput("community_life_exp")
          ), # closes 2nd box
          box(
            width = 12,
            verbatimTextOutput("text1")
          )# closes 3rd box
        ), # closes 1st fluid Row
        
        fluidRow(
          box(title = "Chicago's Life Expectancy",
              width = 12,
              valueBoxOutput("chicago_life_exp")
          ) # closes box
        ), # closes 2nd fluid row
        
        
        fluidRow(
          box(
            title = "Now we have 2 maps. This map displays life expectancy among the 77 Chicago community areas while the map below lets you choose the different health indicators or demographic variables to compare. Higher colors means higher values. See the disparities?",
            width = 12,
            tmapOutput("lifeexpectancymap")
          ) # closes box
        ), # closes fluid row
        
        fluidRow(
          box(
            title = "Select Variable",
            status = "primary",
            width = 4,
            selectInput(inputId = "choices", 
                        label = "Select community health/Demographic variable", 
                        choices = choices,
                        selected = NULL)
          ), # closes 1st box
          box(
            title = "What variables explain the disparities in Life Expectancy? ",
            width = 8,
            tmapOutput("map")
          )# closest 2nd box
        ) # closes 2nd fluid row
        
        
      ), # close maptab Item
      
      
      
      
      
      
      ### plot tab starts
      tabItem(
        tabName = "plots_tab", 
        titlePanel("Below is an interactive scatterplot, you can select from the health indicators or demographic variables and see the correlation with life expectancy in Chicago. You can also look at the regions by selecting the individual plots."),
        fluidRow(
          box(
            title = "Select Options",
            status = "primary",
            width = 3,
            selectInput(inputId = "choice",
                        label = "Select Community Health/Demographic Variable",
                        choices = choices,
                        selected = healthdata$smoking_pregnancypercent)# close input
          ), # closes Box
          
          box(
            title = "What Variables correlate with life expectancy?",
            width = 9,
            plotlyOutput("plot")
          )# closes 2nd box
        ) # closest fluid row
      ), # closest plot tab
      
      
      
      
      
      ### bargraph starts
      
      tabItem(
        tabName = "bargraph_tab",
        titlePanel("Community Characteristics"),
        fluidRow(
          box(
            title = "Select Region", 
            status = "primary", 
            width = 3,
            selectInput(inputId = "region", 
                        label = "Select region", 
                        choices = healthdata$region %>% 
                          as.list(),
                        selected = healthdata$region)
          ), # close 1st box
          
          box(
            title = "Life Expectancy across Regions",
            width = 9,
            plotOutput("bargraph")
          ) # close 2nd box
          
        ), #close fluid row
        
        fluidRow(
          box(
            title = "Select Region",
            status = "primary",
            width = 3,
            selectInput(
              inputId= "region_id",
              label= "Select region",
              choices = healthdata$region %>% 
                as.list(),
              selected = healthdata$region) # close input
          ), # close 1st box
          box(
            title = "table",
            width = 20,
            tableOutput("table")
          ) # close 2nd box
        ) # close 2nd fluid row
      ), # close bargraph tab
      
      
      ### conclusion tab
      
      tabItem(
        tabName = "conclusion_tab",
        titlePanel("What did you learn?"),
        fluidRow(
          box(
            title = " Which community health/demographic factor do you think best explains the disparities in Life Expectancy in Chicago?",
            status = "primary",
            width = 12,
            selectInput(inputId = "choices", 
                        label = "Select community health/Demographic variable", 
                        choices = choices,
                        selected = NULL)
            
          ), #closes box
          
          box(
            textAreaInput("", "What is one thing that you can do to increase life expectancy in your neighborhood? (e.g. I can tell my friends or family to stop drinking so much soda)", rows = 2),
            textAreaInput("", "What is one policy that you think  City Council should enact to combat this problem? (e.g. increase accessibility to fruits and vegetables in low-income communities)", rows = 2),
            
          ) # closes 2nd box
          
        ) # closes fluid row
      )# closes conclusion tab Item
      
      
    ) # closes tab Items
  ) # closes dashboard Body
) # closes dashboard Page






### SERVER

server <- function(input, output, session) {
  
  ### value box
  
  output$text1 <- renderText({
    "Is this lower or higher than you expected? How does it compare to the average Life Expectancy in Chicago? Check out the maps below and the next tabs that will help explain what factors may explain your community's life expectancy."})
  
  
  area_1 <- reactive(healthdata%>%
                       filter(Name==input$community_input) %>%
                       select(Name, lifeexpectancy2019_years))
  
  output$community_life_exp <- renderValueBox({
    valueBox(
      paste(input$community_input),
      area_1(),
      width = 12,
      color = "red",
    )
    
  })
  
  
  
  output$chicago_life_exp <- renderValueBox({
    valueBox(
      value = "77",
      subtitle = "Chicago's Life Expectancy(years)")
    
  })
  
  ### end value box
  
  
  
  ### start plot
  
  
  
  output$plot <- renderPlotly({
    
    plot_ly(healthdata, x= ~get(input$choice), y = ~lifeexpectancy2019_years, color= ~region, type = 'scatter', mode = 'markers') %>% 
      
      
      layout(xaxis = list(title = 'Community Health/Demographic Variable'), 
             yaxis = list(title = 'Average Life Expectancy in 2019 (years) in Chicago'))
    
  })
  ### end plot
  
  
  ### maps
  
  ## 1st map
  output$lifeexpectancymap <- renderTmap({
    tm_shape(healthmap) + 
      tm_polygons("lifeexpectancy2019_years", id="Name", palette= "OrRd", title='Total life expectancy',
                  border.col='grey27', alpha=.9) + tm_layout(legend.position = c("right", "top"), title= 'Total life expectancy by Community Area', title.position = c('right', 'top')) + tmap_mode("view")
  })
  
  
  ## 2nd map
  output$map <- renderTmap({
    tm_shape(healthmap) +
      tm_polygons(choices[1], zindex = 401)
  })
  
  observe({
    choices <- input$choices
    tmapProxy("map", session, {
      tm_remove_layer(401) +
        tm_shape(healthmap) +
        tm_polygons(choices, id="Name", zindex = 401)
    })
  })
  
  
  ### end maps
  
  
  
  
  
  
  
  ### end map
  
  ### table
  output$table <- function() {
    df <- healthdata %>%
      filter(region == input$region_id) %>%
      select(Name, white_percent, black_percent, hispanic_latino_percent, bingedrinking_percent, physicalinactivity_percent, smoking_percent, smoking_pregnancypercent, accessFruitsVegetables_percent, fruitVegetableServings_percent, sodaconsumption_percent, neighborhoodsafety_percent) 
    
    
    knitr::kable(df, 
                 col.names = c("Chicago Community Area", "% that are white", "% that are  African American", "% that are Hispanic/Latino", " % that binge drink", "% that are physically inactive", "% that smoke", "% that smoke during pregnancy", "% that have access to Fruits and vegetables", "% that have fruit and vegetable in their servings", "% that consume soda", "% of neighborhood safety")) %>%
      kableExtra::kable_styling(fixed_thead = TRUE)
  }
  
  
  ### end table
  
  ### bargraph 
  data <- reactive({
    req(input$region)
    df <- healthdata %>% filter(region == input$region) %>% select(Name, lifeexpectancy2019_years, region)
  })
  
  output$bargraph <- renderPlot({
    ggplot(data(), aes( y = lifeexpectancy2019_years, x = Name, fill = Name )) + geom_bar(stat = "identity") + geom_text(aes(x = Name, y = lifeexpectancy2019_years,
                                                                                                                             label = round(lifeexpectancy2019_years, 0)), size = 3, hjust=0 , vjust= -0.5 , family= "Arial") + coord_flip() + 
      
      labs( x= "Commmunity Area", 
            y= "Average Life Expectancy in 2019 (years)",
            title = "Life Expectancy by community area in 2019") +
      
      theme_classic() +
      theme( 
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(face = "italic", size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
    ### end bargraph
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
