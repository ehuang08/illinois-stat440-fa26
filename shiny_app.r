library(tidyverse)
library(shiny)
library(DT)
library(DBI)
library(reactable)
library(leaflet)
library(plotly)

# https://catalog.data.gov/dataset/chicago-public-schools-school-progress-reports-sy2425

df_schools <- read_csv('export.csv')
df_schools1 <- df_schools |>
    select(
        School_ID, Short_Name, Long_Name, School_Type,
        Primary_Category, Address, City, State, Zip, Phone,
        Student_Attainment_Rating, Culture_Climate_Rating,
        School_Survey_Student_Response_Rate_Avg_Pct,
        Creative_School_Certification, School_Survey_Safety,
        Chronic_Truancy_Pct
    )
df_schools2 <- df_schools |>
    select(School_ID, Short_Name, Long_Name,
           Blue_Ribbon_Award_Year, Excelerate_Award_Gold_Year,
           Spot_Light_Award_Year, Improvement_Award_Year, 
           School_Type, Primary_Category, Address, City, State, Zip)
df_schools2 <- df_schools2 |>
    group_by(School_Type) |>
    filter(!is.na(Blue_Ribbon_Award_Year) | !is.na(Excelerate_Award_Gold_Year) |
           !is.na(Spot_Light_Award_Year) | !is.na(Improvement_Award_Year))
df_schools1$School_Survey_Safety <- factor(
    df_schools1$School_Survey_Safety,
    levels = c(
        "VERY WEAK",
        "WEAK",
        "NEUTRAL",
        "STRONG",
        "VERY STRONG",
        "NOT ENOUGH DATA"
    )
)



ui <- fluidPage(
    titlePanel("Chicago Public School Explorer"),

    tabsetPanel(
        tabPanel("Overview",
                h3("Purpose of this app"),
                p("This app can help parents find public schools in Chicago, focusing on location, school's safety, 
                culture climate ratings, award counts, attendance rate, and student attainmenent ratings."),
                p("This interactive map displays different public schools in Chicago."),
                leafletOutput("map", height = "650px")),


        tabPanel("Data Explorer",
                h3("List of all schools"),
                DTOutput("school_table"),
                h3("Schools with awards"),
                DTOutput("award_school_table")
        ),


        tabPanel("Results/Findings",
                h3("School safety ratings by school category"),
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput(
                            "school_type",
                            "Select School Types(s):",
                            choices = sort(unique(na.omit(df_schools1$School_Type))),
                            selected = sort(unique(na.omit(df_schools1$School_Type))),
                            multiple = TRUE,
                            options = list(
                                placeholder = "Choose school types: ",
                                plugins = list("remove_button")
                            )
                        ),
                        sliderInput(
                            "max_truancy",
                            "Maximum Chronic Truancy Rate (%):",
                            min = 0,
                            max = max(df_schools1$Chronic_Truancy_Pct, na.rm = TRUE),
                            value = max(df_schools1$Chronic_Truancy_Pct, na.rm = TRUE),
                            step = 0.1
                        )
                    ),
                    mainPanel(
                        plotlyOutput("barplot1", height = "500px")
                    )
                )
        ),


        tabPanel("About the Project",
        )
    )
)




server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(data = df_schools) %>%
            addTiles() %>%
            addCircleMarkers(
                lng = ~School_Longitude,
                lat = ~School_Latitude,
                radius = 5,
                popup = ~paste(
                    "<b>School ID:</b> ", School_ID,
                    "<br><b>School Name:</b> ", Long_Name,
                    "<br><b>Adress:</b> ", Address,
                    "<br><b>City:</b> ", City,
                    "<br><b>State:</b> ", State,
                    "<br><b>Phone:</b> ", Phone,
                    "<br><b>School Type:</b> ", School_Type
                )
            )
    })


    output$school_table <- renderDT({
        datatable(
            df_schools1,
            filter = "none",
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE
            ),
            rownames = FALSE
        )
    })
    output$award_school_table <- renderDT({
        datatable(
            df_schools2,
            filter = "none",
            options = list(
                pageLength = 10,
                scrollX = TRUE,
                autoWidth = TRUE
            ),
            rownames = FALSE
        )
    })


    output$barplot1 <- renderPlotly({
        req(input$school_type)
        filtered_data <- df_schools1 %>%
            filter(
                !is.na(Chronic_Truancy_Pct),
                Chronic_Truancy_Pct <= input$max_truancy,
                Primary_Category %in% c("ES", "MS", "HS"),
                School_Type %in% input$school_type
            )

        ggplot(
            filtered_data,
            aes(
                x = School_Survey_Safety,
                fill = Primary_Category
            )
        ) +
            geom_bar(position = "dodge") +
            scale_fill_manual(
                values = c(
                    "ES" = "skyblue",
                    "MS" = "lightpink",
                    "HS" = "lightgreen"
                )
            ) +
            labs(
                title = "School safety ratings by school category",
                x = "Safety Rating",
                y = "Number of Schools",
                fill = "School Category"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 30, hjust =1)
            )
    })
}

shinyApp(ui = ui, server = server)

# Outline/plan

# Entry count, Cylinders, and Search feature
# Show School ID, Short Name, Long Name, School Type,
# primary category, address, city, state, zip, Phone,
# Student_Attainment_Rating, Culture_Climate_Rating,
# School_Survey_Student_Response_Rate_Avg_Pct,
# Creative_School_Certification, School_Survey_Safety
# Suspensions_Per_100_Students_Avg_Pct
# Student_Attendance_Avg_Pct

# Group by school type, collapse/expand, and search feature
# Only schools with Blue ribbon awards, excelerate award gold,
# spot light award year, improvement award year, excellence award year
# School ID, Short Name, Long Name, School Type,
# primary category, Adress, City, State, Zip, Phone

# Box plot of each primary category (ES, MS, HS)
# Show the student attendance avg pct
# filter of school type drop down, slider for maximum suspensions avg pct
# dynamic summaries (min, max, 25th, median, 75th)

# Bar chart of student attainment rating
# filter by primary category drop down, school type dropdown
# dynamic summaries (counts of each category)

# map of schools. can hover on each location and see
# school, school type, priamry category, phone, school survey safety

