library(shiny)


Race_Levels<-levels(as.factor(mi_sample$MI_RACE))
Race_Levels<-append("NONE", Race_Levels)
Age_Levels<-levels(as.factor(mi_sample$MI_AGE_RANGE))
Age_Levels<-append("NONE", Age_Levels)

haystack_issues<-colnames(mi_sample[25:64])



ui <- fluidPage(
  tabsetPanel(
    
    tabPanel( "Dat Table",
  
      # Sidebar panel for inputs ----
      sidebarPanel(
        selectInput(inputId = "Race",
                    label = "Demographic Filter",
                    choices = Race_Levels,),
        
        selectInput(inputId = "Age",
                    label = "Age Filter",
                    choices = Age_Levels,),
        
        
        selectInput(inputId = "flag",
                    label = "Choose a Issue:",
                    choices = haystack_issues),
        
        selectInput(inputId = "Level_1", 
                    label="Choose a Flag",
                    choices = c('A','B') ),
        
        selectInput(inputId = "flag2",
                    label = "Choose another Issue:",
                    choices = haystack_issues),
        
        selectInput(inputId = "Level_2", 
                    label="Choose a Flag",
                    choices = c('A','B') ),
    
        actionButton("button", "RUN"),
        actionButton("button2", "Make Venn")
      ),
    
      mainPanel(h1("Various tables"),dataTableOutput("table"), h2('Conditional Probabilities'), dataTableOutput("table2"), plotOutput("venn"), h2('Flags with the most overlap'), dataTableOutput('table3'))

    ),

    tabPanel("Area",
             
             sidebarPanel(
               actionButton("button3", "Get Zip Data"),
               
               sliderInput("Scat_Slide", label = "Select Voters in Zipcode", min= 0, max= 100, value = 0 , step = 1)
               
               
               
             ),
             
             
             
           mainPanel(dataTableOutput("table4"), plotOutput("scatter"))
           
           
    )
    
  ),



)

server <- function(input, output, session) 
  {
    
    Race<-reactive(input$Race)
    Age<-reactive({input$Age})
    issueB<-reactive({input$flag2})
    issueA<-reactive({input$flag})
    Level_1<-reactive({input$Level_1})
    Level_2<-reactive({input$Level_2})
    Min_Pop<-reactive({input$Scat_Slide})
    
    
    observe(
    {
    
      updateSelectInput(session, "Level_2",
                        label = paste("Select Flag"),
                        choices = unique(mi_sample[colnames(mi_sample)==issueB()]))
      updateSelectInput(session, "Level_1",
                        label = paste("Select Flag"),
                        choices = unique(mi_sample[colnames(mi_sample)==issueA()]))
    
    })
    
    
      
    observeEvent(input$button,
    {
      issueA<-input$flag
      issueB<-input$flag2
      issues<-as.data.frame(c(issueA,issueB))
      
      # Filters Out Population based on the demographic filters 
      if(Race() != "NONE" || Age() != "NONE")
      {
        population<-Pop_Cut(Race(), Age())
      }
      else{
        population<-Michigan
        #population<-sample_n(Michigan, size = 100000)
      }
      
      Forjac<-Get_Counts(issues,population)
      DT2<-Con_Probs(Forjac[2], population, issues)
      DT<-D_Jac(as.data.frame(Forjac[1]),as.data.frame(Forjac[2]), input_names_GLO, population)
      output$table <- renderDataTable(DT)
      output$table2 <- renderDataTable(DT2)
    })
      
      
      
    observeEvent(input$button2, 
    {
      if(Race() != "NONE" || Age() != "NONE")
      {
        population<-Pop_Cut(Race(), Age())
      }
      else
      {
        population<-Michigan
        #population<-sample_n(Michigan, size = 100000)
      }
      
      output$venn <- renderPlot(
      {
      
        display_venn(issueA(), issueB(), Level_1(), Level_2(), population)
      })
 
    
      output$table3 <- renderDataTable(Most_Similar_Flag(JAC_WITH_TAGS, issueA(), Level_1()))
    })
    
    
    
    
    observeEvent(input$button3,
    {          
      if(Race() != "NONE" || Age() != "NONE")
      {
        population<-Pop_Cut(Race(), Age())
      }
      else
      {
        population<-Michigan
        #population<-sample_n(Michigan, size = 100000)
      }
      
                Zip_Tab<-Zip_Codes(population, issueA(), issueB(), Level_1(), Level_2(), 0)
                O_Scat<-Zip_Codes(population, issueA(), issueB(), Level_1(), Level_2(), 1)
                
                output$table4<-renderDataTable(Zip_Tab)
                output$scatter<-renderPlot(O_Scat)
                updateSliderInput(session, "Scat_Slide", min = min(Zip_Tab[2]), max=max(Zip_Tab[2]))
     }
        )
    observeEvent(input$Scat_Slide,
    {
      
      if(Min_Pop()!=0)
      {
        
        if(Race() != "NONE" || Age() != "NONE")
        {
          population<-Pop_Cut(Race(), Age())
        }
        else
        {
          population<-Michigan
          #population<-sample_n(Michigan, size = 100000)
        }
        
      Zip_Tab<-Zip_Codes(population, issueA(), issueB(), Level_1(), Level_2(), 0)
      Zip_Tab_Filter<- Zip_Tab %>% filter(Total_Voters > Min_Pop()) 
      output$table4<-renderDataTable(Zip_Tab_Filter)
      }
      
      
    }
    )
    
    
  
  }
  

shinyApp(ui, server)
