library(tidyverse)
library(shiny)
library(DT)
library(DBI)
library(reactable)
library(leaflet)
library(plotly)
library(shinyWidgets)

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
    select(
        School_ID, Short_Name, Long_Name,
        Blue_Ribbon_Award_Year,
        Excelerate_Award_Gold_Year,
        Spot_Light_Award_Year,
        Improvement_Award_Year,
        School_Type, Primary_Category,
        Address, City, State, Zip, Phone)
df_schools2 <- df_schools2 |>
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
                reactableOutput("award_school_table")
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
                ),

                hr(),

                h3("Distribution of Student Attainment Ratings"),
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput(
                            "attainment_school_type",
                            "Select School Types(s):",
                            choices = sort(unique(na.omit(df_schools1$School_Type))),
                            selected = sort(unique(na.omit(df_schools1$School_Type))),
                            multiple = TRUE,
                            options = list(
                                plugins = list("remove_button"),
                                placeholder = "Choose school types: "
                            )
                        ),
                        pickerInput(
                            inputId = "attainment_culture",
                            label = "Select Culture Climate Rating(s):",
                            choices = sort(unique(na.omit(df_schools1$Culture_Climate_Rating))),
                            selected = sort(unique(na.omit(df_schools1$Culture_Climate_Rating))),
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE,
                                `live-search` = TRUE,
                                `selected-text-format` = "count > 2",
                                size = 8
                            )
                        )
                    ),
                    mainPanel(
                        plotlyOutput("attainment_plot", height = "500px"),
                        h4("Explore Schools by Attainment Rating"),
                        uiOutput("selected_rating_ui"),
                        DTOutput("selected_schools")
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
    output$award_school_table <- reactable::renderReactable({
        reactable::reactable(
            df_schools2,
            groupBy = "School_Type",
            defaultExpanded = FALSE,
            searchable = TRUE
        )
        
    })


    output$barplot1 <- renderPlotly({
        req(input$school_type)
        filtered_data <- df_schools1 %>%
            filter(
                !is.na(Chronic_Truancy_Pct),
                !is.na(School_Survey_Safety),
                Chronic_Truancy_Pct <= input$max_truancy,
                Primary_Category %in% c("ES", "MS", "HS"),
                School_Type %in% input$school_type
            ) %>%
            count(School_Survey_Safety, Primary_Category, name = "Number_of_Schools")

        plot <- ggplot(
            filtered_data,
            aes(
                x = School_Survey_Safety,
                y = Number_of_Schools,
                fill = Primary_Category,
                text = paste(
                    "Safety Rating: ", School_Survey_Safety,
                    "<br>School Category: ", Primary_Category,
                    "<br>Number of Schools: ", Number_of_Schools
                )
            )
        ) +
            geom_col(position = "dodge") +
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

            ggplotly(plot, tooltip = "text")
    })
    output$attainment_plot <- renderPlotly({
        req(input$attainment_school_type)
        req(input$attainment_culture)

        filtered_data <- df_schools1 %>%
            filter(
                !is.na(Student_Attainment_Rating),
                !is.na(Culture_Climate_Rating),
                Primary_Category == "HS",
                School_Type %in% input$attainment_school_type,
                Culture_Climate_Rating %in% input$attainment_culture
            ) %>%
            count(Student_Attainment_Rating, name = "Number_of_Schools") %>%
            mutate(
                Student_Attainment_Rating = factor(
                    Student_Attainment_Rating,
                    levels = c(
                        "NO DATA AVAILABLE",
                        "FAR BELOW EXPECTATIONS",
                        "BELOW EXPECTATIONS",
                        "MET EXPECTATIONS",
                        "ABOVE EXPECTATIONS",
                        "FAR ABOVE EXPECTATIONS"
                    )
                )
            )

        plot <- ggplot(
            filtered_data,
            aes(
                x = Student_Attainment_Rating,
                y = Number_of_Schools,
                fill = Student_Attainment_Rating,
                key = as.character(Student_Attainment_Rating),
                text = paste(
                    "Attainment Rating:",
                    Student_Attainment_Rating,
                    "<br>Number of Schools:",
                    Number_of_Schools
                )
            )
        ) +
        geom_col() +
        labs(
            title = "Distribution of Student Attainment (HS)",
            x = "Student Attainment Rating",
            y= "Number of Schools",
            fill = "Attainment Rating"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        )

        ggplotly(plot, tooltip = "text", source = "attainment") %>%
            event_register("plotly_click")

    })
    selected_rating <- reactive({
        click <- event_data(
            "plotly_click",
            source= "attainment"
        )
        if(is.null(click)) {
            return(NULL)
        }
        as.character(click$key)
    })
    output$selected_schools <- renderDT({
        rating <- selected_rating()
        req(rating)
        selected_data <- df_schools1 %>%
            filter(
                Student_Attainment_Rating == rating,
                Primary_Category == "HS",
                School_Type %in% input$attainment_school_type,
                Culture_Climate_Rating %in% input$attainment_culture
            ) %>%
            select(
                School_ID, Short_Name, School_Type, Student_Attainment_Rating, Culture_Climate_Rating
            )
        datatable(
            selected_data,
            options = list(
                pageLength= 10,
                scrollX = TRUE
            ),
            rownames = FALSE
        )
    })
    output$selected_rating_ui <- renderUI({
        rating <- selected_rating()
        if(is.null(rating)) {
            return(
                ("Click a bar to see school in selected attainment rating.")
            )
        }
        h4(paste("Schools rated: ", rating))
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

# Bar plot of each primary category (ES, MS, HS)
# filter of school type drop down, slider for maximum truancy rate
# dynamic summaries by showing count of each bar

# Bar chart of student attainment rating
# filter by primary category drop down, school type dropdown
# dynamic summaries (counts of each category)

# map of schools. can hover on each location and see
# school, school type, priamry category, phone, school survey safety

