library(plyr)
library(dplyr)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinydashboard)
library(RODBC)
library(DT)
library(shinyjs)
library(stringr)
library(gridExtra)
library(grid)
library(lattice)
library(shinyWidgets)
library(plotly)

source("helper_functions.R")
source("create_graphs.R")
result<-NULL

#build a look up table of Major_Codes / Major names
major_lookup = getActivePrograms()

dbHeader <- dashboardHeader(title=tags$a(href = "https://www.elac.edu/facultyStaff/oie/index.htm","ELAC"),dropdownMenuOutput('dropdown'))



ui = fluidPage(includeCSS("theme.css"),
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }",
                          "a {color: #333;}"
               ),
               
               dashboardPage(skin = "black",title="Program Completions App",
                             
                             # Application title
                             dbHeader,
                             dashboardSidebar(width="300px",collapsed=TRUE,
                                              
                                              sidebarMenu(id="tabs",
                                                          menuItem("Tutorial",tabName="tutorial"),
                                                          menuItem("App",tabName="main_app",selected=T)
                                                          
                                                          
                                              )),
                             
                             # Sidebar with a slider input for the number of bins
                             dashboardBody(useShinyjs(), tags$script(HTML("function clickFunction(link){ 
                                                                          Shiny.onInputChange('linkClicked',link);}")),
                                           tabItems(tabItem(tabName = "main_app",
                                                            fluidRow(
                                                              box(title="Programs",
                                                                  selectInput(inputId = "programs", label=NULL, choices = levels(major_lookup$Major), multiple = FALSE),
                                                                  width=5
                                                              ),br(),br(),br(),br(),br(),br(),br(),br(),
                                                              
                                                              div(id = "time-scale-box",box(title="Time Scale",solidHeader=T, status="primary",
                                                                                            radioButtons(inputId = "time_scale",label = NULL,
                                                                                                         choices = c("Academic Year" = "academic_year", "Semester" = "semester")),
                                                                                            width= 2,height=150
                                                              )),
                                                              div(id = "plot-style-box",box(solidHeader=T, status="primary",title="Plot Style",
                                                                                            radioButtons(inputId = "plot_style",label = NULL,
                                                                                                         choices = c("Histogram" = "histogram", "Density" = "density")),
                                                                                            width=  3,height=150
                                                              )),
                                                              div(id = "completer-groups-box",box(solidHeader=T, status="primary",title="Completers Group",
                                                                                                  radioButtons(inputId = "completer_groups",label = NULL,
                                                                                                               choices = c("All completers (6 years)" = "all_completers", "Six-year cohort" = "cohort"),
                                                                                                               selected = c("All completers (6 years)"="all_completers")),
                                                                                                  width=  2,height=150
                                                              )),
                                                              div(id = "gender-box",box(solidHeader=T, status="primary",title="Gender",
                                                                                        checkboxGroupInput(inputId = "gender_choice",label = NULL,
                                                                                                           choices = c("Male" = "Male","Female" = "Female")), width=1,height=150)),
                                                              div(id = "ethnicity-box",box(solidHeader=T, status="primary",title="Ethnicity",
                                                                                           checkboxGroupInput(inputId = "ethnicity_choice",label = NULL, inline = TRUE,
                                                                                                              choices = c("Asian" = "Asian","Black/African American" = "Black/African American",
                                                                                                                          "Hawaiian/Pacific Islander", "Hispanic/Latino","Native American",
                                                                                                                          "Two or more races","Unknown","White")),width = 4,height=150)),
                                                              div(id = "sgec-box",box(solidHeader=T, status="primary",title="Proportion of Units at SGEC",
                                                                                      sliderInput(inputId = "SGEC_slider", label=NULL,0,100,60,5),
                                                                                      width=4,height=150
                                                              )),
                                                              div(id = "online-box",box(solidHeader=T, status="primary",title="Proportion of Online Units",
                                                                                        sliderInput(inputId = "Online_slider", label=NULL,0,100,60,5),
                                                                                        width=4,height=150
                                                              )),
                                                              div(id = "chart-style-box",box(solidHeader=T, status="primary",title="Chart Style",
                                                                                             radioButtons(inputId = "chart_style",label=NULL,choices = c("Count" = "count","Proportion" = "proportion"),selected = c("Count" = "count")),
                                                                                             width=1,height=150
                                                              )),
                                                              div(id = "term-selection-box",box(solidHeader=T, status="primary",title="Terms",
                                                                                                checkboxGroupInput(inputId = "term_choice", label=NULL, inline = TRUE, choices = c("Summer","Fall","Winter","Spring"),
                                                                                                                   selected = c("Summer","Fall","Winter","Spring")),
                                                                                                width=2,height=150
                                                              )),
                                                              
                                                              div(id = "award-type-selection-box",box(solidHeader=T, status="primary",title="Award Type",
                                                                                                      uiOutput("award_type_selection_radio"),
                                                                                                      width=2
                                                              )),
                                                              
                                                              div(id = "award-type-check-box",box(solidHeader=T, status="primary",title="Award Type",
                                                                                                  uiOutput("award_type_selection_check"),
                                                                                                  width=1
                                                              )),
                                                              column(width=4,div(id = "course-proportion-box",box(solidHeader=T, status="primary",title="Minimum Proportion of Students",
                                                                                                                  sliderInput(inputId = "course_proportion_slider", label=NULL,0,100,50,5),
                                                                                                                  width=12,height=150
                                                              ))),br(),br(),br(),br(),br(),br(),br(),br(),
                                                              column(width=1,offset=11,
                                                                     actionBttn(inputId = "submit", label = "Submit", 
                                                                                style = "gradient", color = "primary"))
                                                            ),
                                                            fluidRow(
                                                              tabBox(
                                                                id = "tabCompleters",width=12,
                                                                tabPanel("Completers",
                                                                         uiOutput("completers_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("completersPlot"),uiOutput("cohortCompletersPlot"), 
                                                                         dataTableOutput("completersTable")
                                                                ),
                                                                tabPanel("Time to Complete Award",
                                                                         uiOutput("time_to_complete_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("timeToCompletionPlot"),
                                                                         uiOutput("cohortTimeToCompletionPlot"),
                                                                         dataTableOutput("timeToCompleteTable")),
                                                                tabPanel("Completers (SGEC Students)",
                                                                         uiOutput("sgec_completers_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("sgecPlot"),
                                                                         uiOutput("cohortSgecPlot"), 
                                                                         dataTableOutput("sgecTable")),
                                                                tabPanel("Completers (Online Students)",
                                                                         uiOutput("online_completers_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("onlinePlot"),
                                                                         uiOutput("cohortOnlinePlot"),
                                                                         dataTableOutput("onlineTable")),
                                                                tabPanel("Change in Completers over Time",
                                                                         uiOutput("change_completers_time_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("changePlot"),
                                                                         dataTableOutput("changeTable")),
                                                                tabPanel("Change in Efficiency over Time",
                                                                         uiOutput("change_efficiency_time_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("efficiencyPlot"), 
                                                                         dataTableOutput("efficiencyTable")),
                                                                tabPanel("Change in Time to Complete Award",
                                                                         uiOutput("change_time_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("changeTimePlot"),
                                                                         dataTableOutput("changeTimeTable")),
                                                                tabPanel("Course-taking Patterns", 
                                                                         uiOutput("course_patterns_box"),br(),br(),br(),br(),br(),
                                                                         uiOutput("coursePatterns"))
                                                              )
                                                            )
                                           ),#mainapp tab,
                                           
                                           tabItem(tabName = "tutorial",
                                                   actionBttn(inputId = "tutorial_go_main",label="Go Back",style="stretch",color="primary",icon=icon("arrow-left"),size="sm",no_outline = FALSE),
                                                   
                                                   includeHTML("tutorial.txt")))#,tutorial tab, tab items
                             )))#dashboardPage,FluidPage

server = function(input, output,session) 
{
  addClass(selector = "body", class = "sidebar-collapse")
  output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                           badgeStatus = NULL,headerText="Help",get_noti())})
  observeEvent(input$linkClicked,{
    
    if(grepl("Tutorial",input$linkClicked)){
      updateTabItems(session,"tabs",selected = "tutorial")
      output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                               badgeStatus = NULL,headerText="Help",get_noti())})
    }
    
  })
  
  observeEvent(input$tutorial_go_main,{
    updateTabItems(session, "tabs","main_app")
  })
  options(warn = -1) 
  
  show_no_awards_warning<-reactiveVal(value=FALSE)
  
  awards = reactive({
    getAwardsByMajor(major_lookup$Major_Code[major_lookup$Major == input$programs])
  })
  
  #hide stuff based of of tab content needed
  observeEvent(input$tabCompleters,{
    if(input$tabCompleters %in% c("Completers"))
    {
      show(id = "time-scale-box")
      show(id = "gender-box")
      show(id = "ethnicity-box")
      show(id = "completer-groups-box")
      show(id = "chart-style-box")
      show(id = "award-type-check-box")
      
      hide(id = "award-type-selection-box")
      hide(id = "course-proportion-box")
      hide(id = "plot-style-box")
      hide(id = "sgec-box")
      hide(id = "online-box")
      hide(id = "term-selection-box")
    }
    
    if(input$tabCompleters == "Time to Complete Award")
    {
      show(id = "plot-style-box")
      show(id = "gender-box")
      show(id = "ethnicity-box")
      show(id = "completer-groups-box")
      show(id = "award-type-check-box")
      
      hide(id = "award-type-selection-box")
      hide(id = "course-proportion-box")
      hide(id = "sgec-box")
      hide(id = "online-box")
      hide(id = "time-scale-box")
      hide(id = "chart-style-box")
      hide(id = "term-selection-box")
    }
    
    
    if(input$tabCompleters == "Completers (SGEC Students)")
    {
      show(id = "sgec-box")
      show(id = "time-scale-box")
      show(id = "completer-groups-box")
      show(id = "award-type-check-box")
      show(id = "chart-style-box")
      
      hide(id = "course-proportion-box")
      hide(id = "award-type-selection-box")
      hide(id = "plot-style-box")
      hide(id = "gender-box")
      hide(id = "online-box")
      hide(id = "ethnicity-box")
      hide(id = "term-selection-box")
    }
    
    if(input$tabCompleters == "Completers (Online Students)")
    {
      show(id = "online-box")
      show(id = "time-scale-box")
      show(id = "completer-groups-box")
      show(id = "award-type-check-box")
      show(id = "chart-style-box")
      
      hide(id = "course-proportion-box")
      hide(id = "award-type-selection-box")
      hide(id = "plot-style-box")
      hide(id = "gender-box")
      hide(id = "plot-style-box")
      hide(id = "ethnicity-box")
      hide(id = "sgec-box")
      hide(id = "term-selection-box")
    }
    
    if(input$tabCompleters == "Change in Completers over Time")
    {
      show(id = "gender-box")
      show(id = "ethnicity-box")
      show(id = "chart-style-box")
      show(id = "term-selection-box")
      show(id = "award-type-check-box")
      
      hide(id = "course-proportion-box")
      hide(id = "award-type-selection-box")
      hide(id = "plot-style-box")
      hide(id = "time-scale-box")
      hide(id = "sgec-box")
      hide(id = "completer-groups-box")
      hide(id = "online-box")
    }
    if(input$tabCompleters == "Change in Efficiency over Time")
    {
      show(id = "term-selection-box")
      show(id = "award-type-check-box")
      
      hide(id = "course-proportion-box")
      hide(id = "award-type-selection-box")
      hide(id = "gender-box")
      hide(id = "ethnicity-box")
      hide(id = "chart-style-box")
      hide(id = "plot-style-box")
      hide(id = "time-scale-box")
      hide(id = "sgec-box")
      hide(id = "completer-groups-box")
      hide(id = "online-box")
    }
    if(input$tabCompleters == "Change in Time to Complete Award")
    {
      show(id = "term-selection-box")
      show(id = "gender-box")
      show(id = "ethnicity-box")
      show(id = "award-type-selection-box")
      
      hide(id = "course-proportion-box")
      hide(id = "award-type-check-box")
      hide(id = "chart-style-box")
      hide(id = "plot-style-box")
      hide(id = "time-scale-box")
      hide(id = "sgec-box")
      hide(id = "completer-groups-box")
      hide(id = "online-box")
    }
    
    if(input$tabCompleters == "Course-taking Patterns")
    {
      show(id = "gender-box")
      show(id = "ethnicity-box")
      show(id = "award-type-selection-box")
      show(id = "course-proportion-box")
      
      hide(id = "award-type-check-box")
      hide(id = "term-selection-box")
      hide(id = "chart-style-box")
      hide(id = "plot-style-box")
      hide(id = "time-scale-box")
      hide(id = "sgec-box")
      hide(id = "completer-groups-box")
      hide(id = "online-box")
    }
  })
  
  #update award type in last tab
  cohort_awards = reactive({
    getAwardsByMajor(major_lookup$Major_Code[major_lookup$Major == input$programs],mode="cohort")
  })
  
  change_data = reactive({
    getStratifiedCohorts(major_lookup$Major_Code[major_lookup$Major == input$programs],
                         mode = "completers")
  })
  
  efficiency_data = reactive({
    getStratifiedCohorts(major_lookup$Major_Code[major_lookup$Major == input$programs],
                         mode = "efficiency")
  })
  
  courses_data = reactive({
    getCourseDistributions(major_lookup$Major_Code[major_lookup$Major == input$programs])
  })
  
  
  output$award_type_selection_radio = renderUI({
    if(input$tabCompleters %in% c("Course-taking Patterns") || input$tabCompleters %in% c("Change in Time to Complete Award"))
    {
      options = unique(as.character(cohort_awards()$Award_Type) )
      
      radioButtons("award_type_selection", label=NULL, options)
    }
  })
  
  
  #update award_typpe_check_box
  output$award_type_selection_check = renderUI({
    options = unique(as.character(awards()$Award_Type) )
    if(!identical(options,character(0))){
      show_no_awards_warning(FALSE)
      checkboxGroupInput("award_type_selection_many", "Award Type", options, selected = options)
    }else{
      show_no_awards_warning(TRUE)
      sendSweetAlert(
        session=session,
        title = "Oops, no award data available",
        text = "No awards have been given within the last 6 years for the program chosen",
        type = "error"
      )
      return(checkboxGroupInput("award_type_selection_many", "Award Type", choices=NULL))
      
    }
    
  })
  
  observe(if(show_no_awards_warning()){
    shinyjs::hide("submit")
    
  }else{
    shinyjs::show("submit")
  })
  
  #when submit is pressed
  observe(if(input$submit>0){
    #variables for graphs
    all_completers_graph = NULL
    cohort_graph = NULL
    table=NULL
    data=NULL
    cohort_data=NULL
    
    
    #if in completers tab
    #------------------------------------------------------Completers Tab------------------------------------------------------------
    if(isolate(input$tabCompleters) %in% c("Completers","Time to Complete Award","Completers (SGEC Students)","Completers (Online Students)")){
      if("cohort" %in% isolate(input$completer_groups)){
        cohort_data=isolate({generateAwardsPlot(cohort_awards(), "Cohort Awards",input$tabCompleters, input$time_scale, 
                                                input$plot_style, input$gender_choice, input$ethnicity_choice,
                                                input$SGEC_slider, input$Online_slider, input$chart_style, input$award_type_selection_many,text="Cohort culprit")})
        
        
      }
      else if("all_completers" %in% isolate(input$completer_groups)){
        data=isolate({generateAwardsPlot(awards(),"All Awards",input$tabCompleters,input$time_scale, 
                                         input$plot_style, input$gender_choice, input$ethnicity_choice,
                                         input$SGEC_slider, input$Online_slider, input$chart_style, input$award_type_selection_many,text="All awards culprit")})
        
        
      }
      
      if((is.null(data) && ("all_completers" %in% isolate(input$completer_groups))) ||
         (is.null(cohort_data) && ("cohort" %in% isolate(input$completer_groups)))
      ){#no data because disaggregated too much
        sendSweetAlert(
          title = "Oops, no award data available for chosen filters",
          text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
          type = "error"
        )
        return()
      }
      #--------------Deciphering what y you will use--------------
      #Note: Time to complete award does not include chart_style
      if(isolate(input$chart_style)=="count")
        y="Count"
      else{
        y="Proportion"
      } 
      
      #Note: Time to complete award does not include chart_style
      if(isolate(input$time_scale)=="academic_year")
        x="Academic_Year"
      else
        x="Year_Semester"
      
      #if ethnicity was choosen
      if(isolate(input$tabCompleters)!="Completers (SGEC Students)" && 
         isolate(input$tabCompleters)!="Completers (Online Students)" &&
         isolate(input$tabCompleters)!="Change in Efficiency over Time"){
        if(!is.null(isolate(input$ethnicity_choice)) && !is.null(isolate(input$gender_choice)))
          y="Ethnicity_Gender"
        #if gender was chosen
        else if(!is.null(isolate(input$gender_choice)))
          y="Gender"
        else if(!is.null(isolate(input$ethnicity_choice)))
          y="Ethnicity"
      }
      
      
      #--------------- Time to Complete Award or Everythin Else--------------------
      if(isolate(input$tabCompleters) %in% c("Time to Complete Award")){
        
        #reset variables
        all_completers_graph<-NULL
        cohort_graph<-NULL
        table=isolate( time_to_comple_award_proportion(data,cohort_data,input$completer_groups, input$gender_choice, input$ethnicity_choice))
        
        if(is.null(table)){
          sendSweetAlert(
            title = "Oops, no award data available",
            text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
            type = "error"
          )
          return()
        }
        
        #Note: histogram uses count, density uses proportion
        
        #Divinding terms into 4 to get years
        temp<- vector(mode="numeric", length=length(testing$Terms_to_Complete))
        temp=4
        data$Terms_to_Complete=data$Terms_to_Complete/ temp
        
        #must determin if using ethinicity, gender, or both
        mode=NULL
        
        if(isolate(length(input$gender_choice))>0 && isolate(length(input$ethnicity_choice))> 0 )
          mode="Ethnicity_Gender"
        else if(isolate(length(input$gender_choice))>0)
          mode="Gender"
        else if(isolate(length(input$ethnicity_choice))>0)
          mode="Ethnicity"
        if("cohort" %in% isolate(input$completer_groups))
          data=cohort_data
        
        if(isolate(input$plot_style)=="density"){
          graph_type="Proportion"
          all_completers_graph <- isolate({create_graph(data,
                                                        input$tabCompleters,
                                                        x= "Time_to_Complete",
                                                        y="Proportion",
                                                        chart_style="density",
                                                        mode= mode,
                                                        genderChoice=input$gender_choice,
                                                        ethnicityChoice=input$ethnicity_choice)})
        }else{
          graph_type="Count"
          #TIME_TO_COMPLETE_AWARD no chart style necessary
          all_completers_graph <- isolate({create_graph(data,
                                                        input$tabCompleters,
                                                        x= "Terms_to_Complete",
                                                        y=NULL,
                                                        chart_style="histogram",
                                                        mode=mode,
                                                        genderChoice=input$gender_choice,
                                                        ethnicityChoice=input$ethnicity_choice)})
          
        }
        
        
        #TIME_TO_COMPLETE_AWARD no chart style necessary
        # graph is histogram and desity chart
        #------------All Completers Graph--------------------------
        if(is.null(all_completers_graph)){#did not chose all completers
          output$timeToCompletionPlot<-renderUI({NULL})
        }else{
          output$graph_all_time<-renderPlotly({all_completers_graph})
          output$timeToCompletionPlot<-renderUI({plotlyOutput("graph_all_time",height="600px")})
        }
        #------------Cohort Graph--------------------------
        if(is.null(cohort_graph)){#did not chose all completers
          output$cohortTimeToCompletionPlot<-renderUI({NULL})
        }else{
          output$graph_cohort_time<-renderPlotly({cohort_graph})
          output$cohortTimeToCompletionPlot<-renderUI({plotlyOutput("graph_cohort_time",height="600px")})
        }
        #--------------Info box---------------------------------
        output$time_to_complete_box <- renderUI({
          infoBox(title ="Filter",width= 12,
                  value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                            paste(input$award_type_selection_many, sep="' '", collapse=", ")))}),
                  icon = icon("filter"),
                  color = "black"
          )
        })
        
        
        colnames(table)=gsub("_"," ",colnames(table))
        output$timeToCompleteTable = renderDataTable({table})
        print("----------------------------------------------------------------------------")
        
        return()
      }
      
      else{
        
        #get the table results with the proportions added to it for all completers anf cohorts
        if("all_completers" %in% isolate(input$completer_groups)){
          data=isolate({get_proportions(data,NULL,input$tabCompleters,c("all_completers"),input$gender_choice,input$ethnicity_choice,input$time_scale,text="all_completers culprit")})
          if(is.null(data)){
            sendSweetAlert(
              title = "Oops, no award data available",
              text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
              type = "error"
            )
            return()
          }
          
          all_completers_graph = isolate({create_graph(data,
                                                       input$tabCompleters,
                                                       x,
                                                       y,
                                                       input$chart_style)})
        }
        
        if(("cohort" %in% isolate(input$completer_groups)) && !is.null(cohort_data)){ #if want to see cohort
          cohort_data=isolate({get_proportions(NULL,cohort_data,input$tabCompleters,c("cohort"),input$gender_choice,input$ethnicity_choice,input$time_scale,text="cohort culprit")})
          if(is.null(cohort_data)){
            sendSweetAlert(
              title = "Oops, no award data available",
              text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
              type = "error"
            )
            return()
          }
          cohort_graph = isolate({create_graph(cohort_data,
                                               input$tabCompleters,
                                               x,
                                               y,
                                               input$chart_style,
                                               main_title="Cohort Awards")})
        }
        else if(("cohort" %in% isolate(input$completer_groups)) && is.null(cohort_data)){#cohort might not have data if chosen
          sendSweetAlert(
            
            title = "Oops, no cohort data available",
            text = "No awards have been given to this cohort within the last 6 years for the program chosen",
            type = "error"
          )
          return()
        }
        
        if( ("cohort" %in% isolate(input$completer_groups)) || ("all_completers" %in% isolate(input$completer_groups))  ){
          
          if(!is.null(data) || !is.null(cohort_data)){#there needs to be data
            
            table=isolate({get_proportions(data,cohort_data,input$tabCompleters,input$completer_groups,input$gender_choice,input$ethnicity_choice,input$time_scale,text="table culprit")})
            table[[".group"]]<-NULL
          }
        }
        
        
        #------------COMPLETERS TAB-----------------------------------
        if(isolate(input$tabCompleters)=="Completers"){
          #value box
          
          output$completers_box <- renderUI({
            infoBox(title ="Filter",width= 12,
                    value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                              paste(input$award_type_selection_many, sep="' '", collapse=", ")))}),
                    icon = icon("filter"),
                    color = "black"
            )
          })
          
          #All Completers graph
          if(is.null(all_completers_graph)){output$completersPlot<-renderUI({NULL})}#dont see all completers graph
          else{#chose to see it
            output$graph_all<-renderPlotly({all_completers_graph})
            output$completersPlot<-renderUI({plotlyOutput("graph_all",height="600px")})}
          
          #Cohort Graph
          if(is.null(cohort_graph)){output$cohortCompletersPlot<-renderUI({NULL})}#dont see cohort graph
          else{#chose to see it
            output$graph_cohort_sgec<-renderPlotly({cohort_graph})
            output$cohortCompletersPlot<-renderUI({plotlyOutput("graph_cohort_sgec",height="600px")})
          }
          
          #table
          colnames(table)=gsub("_"," ",colnames(table))
          if(isolate(input$tabCompleters)=="Completers"){output$completersTable = renderDataTable({table})}
          
        }#end of Completers Tab
        
        #-----------------------SGEC TAB----------------------------------
        else if (isolate(input$tabCompleters)=="Completers (SGEC Students)"){
          #All completers graph
          if(is.null(all_completers_graph)){output$sgecPlot<-renderUI({NULL}) }#dont see all completers graph
          else{#chose to see it
            output$graph_all_sgec<-renderPlotly({all_completers_graph})
            output$sgecPlot<-renderUI({plotlyOutput("graph_all_sgec",height="600px")}) }
          
          #info box
          output$sgec_completers_box <- renderUI({
            infoBox(title ="Filter",width= 12,
                    value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                              paste(input$award_type_selection_many, sep="' '", collapse=", "),
                                              br(),"Proportion: ",input$SGEC_slider))}),
                    icon = icon("filter"),
                    color = "black"
            )
          })
          
          #Cohort Graph
          if(is.null(cohort_graph)){output$cohortSgecPlot<-renderUI({NULL})}#dont see cohort graph
          else{#chose to see it
            output$graph_cohort_sgec<-renderPlotly({cohort_graph})
            output$cohortSgecPlot<-renderUI({plotlyOutput("graph_cohort_sgec",height="600px")})}
          
          #table
          colnames(table)=gsub("_"," ",colnames(table))
          output$sgecTable = renderDataTable({table})
          
        }
        
        #-----------------------ONLINE TAB--------------------------------
        else if(isolate(input$tabCompleters)=="Completers (Online Students)"){
          #All completers grpah
          if(is.null(all_completers_graph)){output$onlinePlot<-renderUI({NULL}) }#dont see all completers graph
          else{#chose to see it
            output$graph_all_online<-renderPlotly({all_completers_graph})
            output$onlinePlot<-renderUI({plotlyOutput("graph_all_online",height="600px")}) }
          
          
          #info box
          output$online_completers_box <- renderUI({
            infoBox(title ="Filter",width= 12,
                    value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                              paste(input$award_type_selection_many, sep="' '", collapse=", "),
                                              br(),"Proportion: ",input$Online_slider))}),
                    icon = icon("filter"),
                    color = "black"
            )
          })
          
          #Cohort Graph
          if(is.null(cohort_graph)){output$cohortOnlinePlot<-renderUI({NULL})}#dont see cohort graph
          else{#chose to see it
            output$graph_cohort_online<-renderPlotly({cohort_graph})
            output$cohortOnlinePlot<-renderUI({plotlyOutput("graph_cohort_online",height="600px")})}
          
          #table
          colnames(table)=gsub("_"," ",colnames(table))
          output$onlineTable = renderDataTable({table})
          
        }
        
        
        
        print("----------------------------------------------------------------------------")
        return()
      }
      
      
      
      # Note:
      #   Had to use uiOutput because plotly does not accept NULL values
      #   So I had to made the output$some_graph and render the plotly graph and then add
      #   it to the renderUI with the name of "some graph" as plotlyOutput
      #   If no graph was needed then renderUI will result in NULL.
      #   Attempted to use plotly_empty with renderPlotly but still got errors
      
    }
    
    #----------------------------------------------------Change in Completers Over Time----------------------------------------------
    else if(isolate(input$tabCompleters) %in% c("Change in Completers over Time")){
      data=isolate(generateChangePlot(change_data(),input$tabCompleters,input$gender_choice, 
                                      input$ethnicity_choice, input$chart_style, input$term_choice, input$award_type_selection_many))
      
      data=isolate(change_in_completers_propotions(data,input$tabCompleters,input$gender_choice,input$ethnicity_choice))
      
      if(is.null(data)){
        sendSweetAlert(
          
          title = "Oops, no award data available",
          text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
          type = "error"
        )
        return()
      }
      #Algorithm: 
      #1) Determin if count or proportion
      if(isolate(input$chart_style)=="Count" || isolate(input$chart_style)=="count"){
        y="Count"
        chart_style="Count"
      }
      else{
        y="Proportion"
        chart_style="Proportion"
      } 
      #2) Determine if filter was applied
      #if no filter applied proportion will not be calculated. So everything will be set to 100
      if(!is.null(isolate(input$ethnicity_choice)) && !is.null(isolate(input$gender_choice)))
        y="Ethnicity_Gender"
      #if gender was chosen
      else if(!is.null(isolate(input$gender_choice)))
        y="Gender"
      else if(!is.null(isolate(input$ethnicity_choice)))
        y="Ethnicity"
      else{
        data$Proportion="100 %"
      }
      
      #filter by award type and filters
      #using y to identify the filter however chart_style is used to identify between Count/Proportion
      all_completers_graph <- isolate({create_graph(data,
                                                    input$tabCompleters,
                                                    x= "Completion_Cohort",
                                                    y= y,
                                                    chart_style=chart_style,
                                                    genderChoice=input$gender_choice,
                                                    ethnicityChoice=input$ethnicity_choice)})
      #render table
      data[[".group"]]<-NULL  #remove .group column 
      colnames(data)=gsub("_"," ",colnames(data)) #remove any underscore in column names
      output$changeTable = renderDataTable({data}) 
      
      #render graph
      output$graph_all_change<-renderPlotly({all_completers_graph})
      output$changePlot<-renderUI({plotlyOutput("graph_all_change",height="600px")})
      
      
      #info box
      output$change_completers_time_box <- renderUI({
        infoBox(title ="Filter",width= 12,
                value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                          paste(input$award_type_selection_many, sep="' '", collapse=", ")))}),
                icon = icon("filter"),
                color = "black"
        )
      })
    }
    
    #----------------------------------------------Chnage in Efficieny over Time-----------------------------------------------------
    else if(isolate(input$tabCompleters) %in% c("Change in Efficiency over Time")){
      i=1
      withProgress(message = 'Creating plot',detail = 'This may take a while...', value = 4, {
        i=i+1
        incProgress((i)/4, detail = paste("Getting data..."))
        
        data=isolate(generateChangePlot(efficiency_data(),input$tabCompleters,input$gender_choice, 
                                        input$ethnicity_choice, input$chart_style, input$term_choice, input$award_type_selection_many))
        
        i=i+1
        incProgress((i)/4, detail = paste("Getting proportions..."))
        
        data=change_in_efficiency_proportion(data) 
      })
      
      #in case there is no data for the award
      if(length(data)==0){
        sendSweetAlert(
          
          title = "Oops, no award data available",
          text = "No awards have been given within the last 6 years for the program chosen",
          type = "error"
        )
      }
      
      withProgress(message = 'Creating plot',detail = 'This may take a while...', value = 4, {
        i=i+1
        incProgress((i)/4, detail = paste("Making graph..."))
        all_completers_graph <- isolate({create_graph(data,
                                                      input$tabCompleters,
                                                      x= "Completion_Cohort",
                                                      y="Efficiency")})
        
        
        i=i+1
        incProgress((i)/4, detail = paste("Rendering Graph..."))
        #info box
        output$change_efficiency_time_box <- renderUI({
          infoBox(title ="Filter",width= 12,
                  value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                            paste(input$award_type_selection_many, sep="' '", collapse=", ")))}),
                  icon = icon("filter"),
                  color = "black"
          )
        })
        
        #make the graph
        output$graph_all_efficiency<-renderPlotly({all_completers_graph})
        output$efficiencyPlot<-renderUI({plotlyOutput("graph_all_efficiency",height="600px")})
        
        output$efficiencyTable = renderDataTable({data})
        
        
        
      })
    }
    
    #---------------------------------------------Chnage in Time to Complete Award---------------------------------------------------
    else if(isolate(input$tabCompleters) %in% c("Change in Time to Complete Award")){
      data=isolate(generateChangePlot(change_data(),input$tabCompleters,input$gender_choice, 
                                      input$ethnicity_choice, input$chart_style, input$term_choice,input$award_type_selection))
      
      
      table=isolate(change_time_to_complete_award_proportions(data,input$tabCompleters,input$gender_choice,input$ethnicity_choice))
      
      
      if(nrow(table)==0){
        sendSweetAlert(
          
          title = "Oops, no award data available",
          text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
          type = "error"
        )
        return()
      }
      
      y=NULL
      
      #Determine if filter was applied
      #if no filter applied proportion will not be calculated. So everything will be set to 100
      if(!is.null(isolate(input$ethnicity_choice)) && !is.null(isolate(input$gender_choice)))
        y="Ethnicity_Gender"
      #if gender was chosen
      else if(!is.null(isolate(input$gender_choice)))
        y="Gender"
      else if(!is.null(isolate(input$ethnicity_choice)))
        y="Ethnicity"
      
      
      all_completers_graph<-  isolate({create_graph(data,
                                                    input$tabCompleters,
                                                    x= "Completion_Cohort",
                                                    y= y,
                                                    genderChoice=input$gender_choice,
                                                    ethnicityChoice=input$ethnicity_choice)})
      
      #info box
      output$change_time_box <- renderUI({
        infoBox(title ="Filter",width= 12,
                value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                          paste(input$award_type_selection, sep="' '", collapse=", ")))}),
                icon = icon("filter"),
                color = "black"
        )
      })
      
      #make the graph
      output$graph_all_changeTime<-renderPlotly({all_completers_graph})
      output$changeTimePlot<-renderUI({plotlyOutput("graph_all_changeTime",height="600px")})
      
      output$changeTimeTable = renderDataTable({table})
    }
    
    #---------------------------------------------Course Taking Patters -------------------------------------------------------------
    else if(isolate(input$tabCompleters) %in% c("Course-taking Patterns")){
      
      data = isolate(generateCourseDistributionPlot(courses_data(),cohort_awards(),input$award_type_selection,
                                                    genderChoice = input$gender_choice, ethnicityChoice = input$ethnicity_choice,
                                                    proportionCutoff = 0.01*input$course_proportion_slider))
      
      if(nrow(data)==0){
        sendSweetAlert(
          
          title = "Oops, no award data available",
          text = "No awards have been given within the last 6 years for the filters specified. Try disagragating by something else.",
          type = "error"
        )
        return()
      }
      course_taking_graph<-isolate(generateCourseDistributionPlot(courses_data(),cohort_awards(),input$award_type_selection,
                                                                  genderChoice = input$gender_choice, ethnicityChoice = input$ethnicity_choice,
                                                                  proportionCutoff = 0.01*input$course_proportion_slider,graph=T))
      
      
      
      #info box
      output$course_patterns_box <- renderUI({
        infoBox(title ="Filter",width= 12,
                value=isolate({HTML(paste("Program: ",input$programs, br(),"Awards: ",
                                          paste(input$award_type_selection, sep="' '", collapse=", "),
                                          br(),"Proportion: ",input$course_proportion_slider))}),
                icon = icon("filter"),
                color = "black"
        )
      })
      
      #graph
      output$graph_all_course<-renderPlotly({course_taking_graph})
      
      output$coursePatterns<-renderUI({plotlyOutput("graph_all_course",height="800px")})
      
    }
    
    
  })
  
  
}

shinyApp(server = server, ui = ui)