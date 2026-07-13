library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(DiagrammeR)

#source helper files
source("load_data.r")
source("summary.r")
source("plots.r")
#UI
ui <- dashboardPage(
#Header
dashboardHeader(
    title="Clinical Eligibility Dashboard",
    titleWidth = 320
),
#SideBar
dashboardSidebar(
    width=260, 
    sidebarMenu(
    id="tabs",

    menuItem("Study Overview", tabName="summary", icon=icon("dashboard")),
    
    menuItem("Time Plot", tabName="clinical", icon=icon("chart-bar")),

    menuItem("Patient Data", tabName="listing", icon=icon("table"))
    ),
    hr(),
    #filters
    h4("Filters"),

    pickerInput(inputId= "country", label="Country", choices=c("All", sort(unique(tnm_data$Country))), selected="All", multiple=FALSE),

    pickerInput(inputId="site", label="Site", choices=c("All", sort(unique(tnm_data$Site))), selected="All", multiple=FALSE),
    
    pickerInput(inputId="tumor", label="Tumor Location", choices=c("All", sort(unique(tnm_data$PTUMLOC))), selected="All", multiple=FALSE),

    pickerInput(inputId="eligibility", label="Eligibility Status", choices=c("All", sort(unique(tnm_data$Eligibility))), selected="All", multiple=FALSE),
    
    dateRangeInput(
  inputId = "enrollment_date",
  label = "Enrollment Date",
  start = min(tnm_data$EnrollmentDate),
  end   = max(tnm_data$EnrollmentDate),
  min   = min(tnm_data$EnrollmentDate),
  max   = max(tnm_data$EnrollmentDate),
  format = "yyyy-mm-dd"
),
actionButton(
  inputId = "reset_filters",
  label = "Reset Filters",
  icon = icon("undo"),
  width = "150px",
  class = "btn-primary"
)

),
dashboardBody(

  tags$head(
tags$style(HTML("
.small-box{
  min-height:70px !important;
  height:70px !important;
}

.small-box>.inner{
  padding:8px 10px !important;
}

.small-box h3{
  font-size:22px !important;
  margin:0 !important;
  line-height:1.1;
}

.small-box p{
  font-size:13px !important;
  margin-top:3px !important;
}

.small-box .icon{
  font-size:40px !important;
  top:12px !important;
  right:10px !important;
}
.btn-primary{
  background-color:#2E86C1;
  border-color:#2E86C1;
  font-weight:bold;
}

.btn-primary:hover{
  background-color:#21618C;
}
"))
),

  tabItems(
        #Summary Tab
        tabItem(
            tabName="summary",
           fluidRow(
  valueBoxOutput("total", width = 2),
  valueBoxOutput("eligible", width = 2),
  valueBoxOutput("ineligible", width = 2),
  valueBoxOutput("missing", width = 2),
  valueBoxOutput("inconsistent", width = 2)
),
                fluidRow(

box(
  width = 4,
  title = "Patient Flow",
  status = "primary",
  solidHeader = TRUE,
  grVizOutput("flowchart", height = "320px")
),

box(
width=8,
title="Country-wise Distribution",
status="primary",
solidHeader=TRUE,
plotlyOutput("countryPlot",height=350)
)

),

fluidRow(

box(
width=12,
title="Site-wise Distribution",
status="primary",
solidHeader=TRUE,
plotlyOutput("sitePlot",height=350)
)

)
            ),
    #Clinical Insights tab
    tabItem(
        tabName="clinical",
        fluidRow(

box(
  width = 12,
  title = "Cumulative Enrollment by Eligibility Status",
  status = "primary",
  solidHeader = TRUE,
  plotlyOutput("cumulativePlot", height = 420)
)
),

fluidRow(

box(
  width = 12,
  title = "Quarterly Enrollment by Eligibility Status",
  status = "primary",
  solidHeader = TRUE,
  plotlyOutput("randomizationPlot", height = 400)
)

),
fluidRow(
box(
  width = 12,
  title = "Enrollment vs Randomization Trend",
  status = "primary",
  solidHeader = TRUE,
  plotlyOutput("enrollmentRandomizationPlot", height = 420)
)),

    ),
    tabItem(
        tabName="listing",
        fluidRow(
            box(width=12, title="Patient Level Dataset", status="primary", solidHeader=TRUE, downloadButton("downloadData", "Download CSV"),
            br(),
            br(),
            DTOutput("subjectTable"))
        )
    )
    )
    )
)

##########################################################
#                  SERVER 
##########################################################
server <- function(input, output, session){
###################################################
## Filtered Dataset
###################################################
valid_location <- c(
  "bladder",
  "renal pelvis",
  "ureter",
  "urethra"
)

filtered_data <- reactive({
    data <- tnm_data
    

    if (!is.null(input$enrollment_date)) {

  data <- data %>%
    filter(
      EnrollmentDate >= input$enrollment_date[1],
      EnrollmentDate <= input$enrollment_date[2]
    )

}
    #country filter
    if(input$country != "All"){
        data <- data %>% filter(Country==input$country)
    }
    #Site Filter
    if(input$site != "All"){
        data <- data %>% filter(Site==input$site)
    }
    #Tumor Location filter
    if(input$tumor != "All"){
        data <- data %>% filter(PTUMLOC==input$tumor)
    }
    #Eligibility filter
    if(input$eligibility != "All"){
        data <- data %>% filter(Eligibility == input$eligibility)
    }
    data <- data %>%
  mutate(
    Tumor_Flag = ifelse(
      tolower(PTUMLOC) %in% valid_location,
      "Valid",
      "Invalid"
    )
  )

    data
})
observeEvent(input$reset_filters, {

  updatePickerInput(
    session,
    "country",
    selected = "All"
  )

  updatePickerInput(
    session,
    "site",
    selected = "All"
  )

  updatePickerInput(
    session,
    "tumor",
    selected = "All"
  )

  updatePickerInput(
    session,
    "eligibility",
    selected = "All"
  )

  updateDateRangeInput(
    session,
    "enrollment_date",
    start = min(tnm_data$EnrollmentDate),
    end   = max(tnm_data$EnrollmentDate)
  )

})
#########################################
## Summary Statistics
#########################################
stats <- reactive({
    summary_stats(filtered_data())
})
output$flowchart <- renderGrViz({

  total <- nrow(filtered_data())

  eligible_data <- filtered_data() %>%
    dplyr::filter(Eligibility == "Eligible")

  eligible <- nrow(eligible_data)

  randomized <- eligible_data %>%
    dplyr::filter(!is.na(randomization_date)) %>%
    nrow()

  not_randomized <- eligible - randomized

  grViz(paste0("

digraph patientflow {

graph [

layout = dot,

rankdir = TB,

nodesep = 0.25,

ranksep = 0.45,

bgcolor='white'

]

node [

shape = box,

style='rounded,filled',

fillcolor='#D6EEF8',

color='#3C8DBC',

fontname='Arial',

fontsize=13,

width=1.6,

height=0.55,

margin='0.12,0.08'

]

edge [

color='#3C3C3C',

arrowsize=0.6,

penwidth=1.2

]

A [label=<
<B>Screened</B><BR/>
<FONT POINT-SIZE='12'>", comma(total), "</FONT>
>]

B [label=<
<B>Eligible</B><BR/>
<FONT POINT-SIZE='12'>", comma(eligible), "</FONT>
>]

C [label=<
<B>Randomized</B><BR/>
<FONT POINT-SIZE='12'>", comma(randomized), "</FONT>
>]

D [label=<
<B>Not Randomized</B><BR/>
<FONT POINT-SIZE='12'>", comma(not_randomized), "</FONT>
>]

A -> B

B -> C

B -> D

}

"))
})

output$total <- renderValueBox({

  valueBox(
    value = HTML(
      paste0(
        stats()$total_subjects,
        " <span style='color:white; font-size:14px;'>(100%)</span>"
      )
    ),
    subtitle = "Total Subjects",
    color = "blue"
  )

})
output$eligible <- renderValueBox({

  valueBox(
    value = HTML(
      paste0(
        "<span style='font-size:20px;font-weight:bold;'>",
        format(stats()$eligible, big.mark = ","),
        "</span>",
        " ",
        "<span style='font-size:14px;font-weight:600;color:white;'>(",
        stats()$eligible_pct,
        "%)</span>"
      )
    ),
    subtitle = "Eligible",
    color = "green"
  )

})
output$ineligible <- renderValueBox({
    valueBox( value = HTML(
      paste0(
        "<span style='font-size:20px;font-weight:bold;'>",
        format(stats()$ineligible, big.mark = ","),
        "</span>",
        " ",
        "<span style='font-size:14px;font-weight:600;color:white;'>(",
        stats()$ineligible_pct,
        "%)</span>"
      )
    ), subtitle="Ineligible", color="red")
})
output$missing <- renderValueBox({
    valueBox( value=HTML(
      paste0(
        "<span style='font-size:20px;font-weight:bold;'>",
        format(stats()$missing, big.mark = ","),
        "</span>",
        " ",
        "<span style='font-size:14px;font-weight:600;color:white;'>(",
        stats()$missing_pct,
        "%)</span>"
      )
    ), subtitle="Missing Data", color="yellow")
})
output$inconsistent <- renderValueBox({ 
    valueBox(value=HTML(
      paste0(
        "<span style='font-size:20px;font-weight:bold;'>",
        format(stats()$inconsistent, big.mark = ","),
        "</span>",
        " ",
        "<span style='font-size:14px;font-weight:600;color:white;'>(",
        stats()$inconsistent_pct,
        "%)</span>"
      )
    ), subtitle="Inconsistent", color="purple")
})
output$eligibilityRate <- renderValueBox({
    valueBox(value=paste0(eligibility_rate(filtered_data()), "%"), subtitle="Eligibility Rate", color="teal")
})


#Eligibility Distribution
output$eligibilityPlot <- renderPlotly({

  ggplotly(
    eligibility_plot(filtered_data()),
    tooltip = "text"
  ) %>%
    config(displayModeBar = FALSE)

})
#country plot
output$countryPlot <- renderPlotly({

  ggplotly(
    country_plot(filtered_data()),
    tooltip="text"
  ) %>%
    config(displayModeBar=FALSE)

})
#site plot
output$sitePlot <- renderPlotly({

  ggplotly(
    site_plot(filtered_data()),
    tooltip="text"
  ) %>%
    config(displayModeBar=FALSE)

})
#tumor plot
output$tumorPlot <- renderPlotly({
    ggplotly(tumor_plot(tumor_summary(filtered_data())))
})
#missing plot
output$missingPlot <- renderPlotly({
    ggplotly(missing_plot(missing_summary(filtered_data())))
})
#delay plot
output$delayPlot <- renderPlotly({
    ggplotly(delay_plot(filtered_data()))
})
# Cumulative Enrollment Trend
output$cumulativePlot <- renderPlotly({

  ggplotly(
    plot_cumulative(filtered_data()),
    tooltip = "text"
  ) %>%
    config(displayModeBar = FALSE)

})


# Randomization Date vs Eligibility
output$randomizationPlot <- renderPlotly({

  ggplotly(
    plot_enrollment_status(filtered_data()),
    tooltip = "text"
  ) %>%
    layout(
      legend = list(
        orientation = "h",
        x = 0.2,
        y = -0.25
      )
    ) %>%
    config(displayModeBar = FALSE)

})
output$enrollmentRandomizationPlot <- renderPlotly({

  ggplotly(
    plot_enrollment_vs_randomization(filtered_data()),
    tooltip = "text"
  ) %>%
    config(displayModeBar = FALSE)

})

###############################################
## SUBJECT TABLE
################################################
output$subjectTable <- renderDT({

  table_data <- filtered_data() %>%
    rename(
      Subject = subject,
      `Enrollment Date` = enrollment_date,
      `Randomization Date` = randomization_date,
      `Tumor Location` = PTUMLOC,
      `TNM T` = TNM_T,
      `TNM N` = TNM_N,
      `TNM M` = TNM_M
    ) %>%
    select(
      Country,
      Site,
      Subject,
      `Enrollment Date`,
      `Randomization Date`,
      `Tumor Location`,
      `TNM T`,
      `TNM N`,
      `TNM M`,
      Eligibility,
      Reason,
      Tumor_Flag
    )

  datatable(
    table_data,
    extensions = "Buttons",
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      dom = "Bfrtip",
      buttons = c("copy","excel","print"),

      columnDefs = list(
        list(
          targets = which(names(table_data) == "Tumor_Flag") - 1,
          visible = FALSE
        )
      )
    ),
    rownames = FALSE
  )

    

})
###########################################
## DOWNLOAD CSV
#########################################
output$downloadData <- downloadHandler(
    filename=function(){ paste0("Clinical_Eligibility_", Sys.Date(), ".csv")
    },
    content=function(file){
        write.csv(filtered_data(), file, row.names=FALSE)
    }
)
}


##############################################
## RUN APP
#############################################
shinyApp(ui=ui, server=server)