# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(leaflet)

# Load data --------------------------------------------------------------------

cake_data_raw <- read.csv("../data/2025/scores_sheet_season_2_with_coords.tsv", sep = "\t", header = T)

# Prepare data --------------------------------------------------------------------

cake_data_filt <- cake_data_raw %>%
    mutate(cake_type = 
            ifelse(Chocolate == "yes" & Berries == "no" & Cheesecake == "no", "Chocolate",
                ifelse(Berries == "yes" & Chocolate == "no" & Cheesecake == "no", "Berries",
                    ifelse(Cheesecake == "yes" & Chocolate == "no" & Berries == "no", "Cheesecake", "Other"))))

cake_data_clean <- cake_data_filt %>%
    mutate(Job_group = ifelse(Job_group == "TA", "Teaching assistant", Job_group))

cake_data_clean$cake_type <- factor(cake_data_clean$cake_type, levels = c("Berries", "Cheesecake", "Chocolate", "Other"))

# Prepare data for value boxes --------------------------------------------------------------------

n_tot_cake <- nrow(cake_data_filt)

female_cake <- cake_data_filt %>%
                filter(Gender == "Female") %>% 
                nrow()

male_cake <- cake_data_filt %>%
                filter(Gender == "Male") %>% 
                nrow()

n_cheesecake <- cake_data_filt %>%
                filter(cake_type == "Cheesecake") %>% 
                nrow()

n_chocolate <- cake_data_filt %>%
                filter(cake_type == "Chocolate") %>% 
                nrow()

n_berries <- cake_data_filt %>%
                filter(cake_type == "Berries") %>% 
                nrow()

n_other_cake <- cake_data_filt %>%
                filter(cake_type == "Other") %>% 
                nrow()

average_score <- cake_data_filt %>%
    summarise(avg = round(mean(Score, na.rm = TRUE), 1)) %>%
    pull(avg)

average_participation <- cake_data_filt %>%
    summarise(avg = round(mean(Participation, na.rm = TRUE), 0)) %>%
    pull(avg)

first <- cake_data_filt %>%
    filter(Cake == "Lemon Cheesecake") %>%
    pull(Cake)

second <- cake_data_filt %>%
    filter(Cake == "cheesecake") %>%
    pull(Cake)

third <- cake_data_filt %>%
    filter(Cake == "Nutella Semifreddo") %>%
    pull(Cake)

# Define UI --------------------------------------------------------------------

ui <- page_navbar(
    title = "Immunotechnology Cake Club",
    sidebar = sidebar(
        selectizeInput(
            inputId = "Job_group",
            label = "Select a job title:",
            multiple = TRUE,
            choices = c("NBIS", "PhD student", "Student", "Teaching assistant"),
            selected = "Teaching assistant",
            options = list(plugins = "remove_button")
        )
    ), 
    nav_spacer(),
    nav_panel("Overview", 
        layout_columns(
            value_box(
              title = "Total number of cakes",
              value = n_tot_cake,
              showcase = bsicons::bs_icon("cake2"), 
              theme = value_box_theme(bg = "#DDC3E3", fg = "#000000")
            ),
            value_box(
              title = "Average score",
              value = average_score,
              showcase = bsicons::bs_icon("cake2-fill"), 
              theme = value_box_theme(bg = "#c9e3c3", fg = "#000000")
            ),
            value_box(
              title = "Average participation",
              value = average_participation,
              showcase = bsicons::bs_icon("calendar-week"),
              theme = value_box_theme(bg = "#c3c9e3", fg = "#000000")
            )
        ),
        plotOutput("score_vs_job_group")
    ), 
    nav_panel("Bakers", 
        layout_columns(
            value_box(
              title = "Female bakers",
              value = female_cake,
              showcase = bsicons::bs_icon("person-standing-dress"), 
              theme = value_box_theme(bg = "#e3ddc3", fg = "#000000")
            ),
            value_box(
              title = "Male bakers",
              value = male_cake,
              showcase = bsicons::bs_icon("person-standing"), 
              theme = value_box_theme(bg = "#c3e3dd", fg = "#000000")
            )
        ),
        layout_columns(
            plotOutput("score_vs_gender"),
            plotOutput("bakers_by_job_gender")
        )
    ), 
    nav_panel("Cake types", 
        layout_columns(
            value_box(
              title = "Cake types",
              value = "4",
              showcase = bsicons::bs_icon("list-ol"), 
              theme = value_box_theme(bg = "#ffd4a9", fg = "#000000")
            ),
            value_box(
              title = "Berry cakes",
              value = n_berries,
              showcase = bsicons::bs_icon("cake2-fill"), 
              theme = value_box_theme(bg = "#C94C4C", fg = "#000000")
            ),
            value_box(
              title = "Cheesecakes",
              value = n_cheesecake,
              showcase = bsicons::bs_icon("cake2"),
              theme = value_box_theme(bg = "#F5E8E1", fg = "#000000")
            ), 
            value_box(
              title = "Chocolate cakes",
              value = n_chocolate,
              showcase = bsicons::bs_icon("cake2-fill"),
              theme = value_box_theme(bg = "#ae7549ff", fg = "#000000")
            ),
            value_box(
              title = "Other cakes",
              value = n_other_cake,
              showcase = bsicons::bs_icon("cake2"),
              theme = value_box_theme(bg = "#b0aaaaff", fg = "#000000")
            )
        ),
        layout_columns(
            plotOutput("cake_type_pie"),
            plotOutput("score_vs_type")
        )
    ),
    nav_panel("Top 3", 
        card(
          card_header("1. place - Lemon Cheesecake"),
          img(src = "../assets/images/2025/Baker_8.jpg")), 
        card(
          card_header("2. place - Cheesecake"),
          img(src = "../assets/images/2025/Baker_17.jpg")), 
        card(
          card_header("3. place - Nutella Semifreddo"),
          img(src = "../assets/images/2025/Baker_13.jpg"))), 
    nav_panel("Geography", leafletOutput('map1'))
)

# Define server function -------------------------------------------------------

server <- function(input, output) { 
    selected_data <- reactive({
        req(input$Job_group)
        cake_data_clean %>%
            filter(
                Job_group %in% input$Job_group
            )
    })

    plot1 <- reactive({
        ggplot(selected_data(), aes(x = Job_group, y = Score)) +
            geom_violin(alpha = 0.7, trim = FALSE) +
            geom_jitter(shape=16, position=position_jitter(0.2)) +
            theme_bw() +
            labs(x = "Job title", y = "Score") +
            theme(
                axis.text = element_text(size = 20),
                axis.text.x = element_text(size = 20, angle = 45, hjust = 1.0),
                axis.title = element_text(size = 20),
                legend.position = "none"
            )
    })

    output$score_vs_job_group <- renderPlot({
        plot1()
    })


    plot2 <- reactive({
    ggplot(selected_data(), aes(x = Gender, y = Score, fill = Gender)) +
        geom_boxplot(alpha = 0.7) +
        theme_bw() +
        scale_fill_manual(values = c("Female" = "#e3ddc3", "Male" = "#c3e3dd")) +
        labs(x = "Gender", y = "Score") +
        theme(
            axis.text = element_text(size = 25),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20)
        )
    })

    output$score_vs_gender <- renderPlot({
        plot2()
    })

    plot3 <- reactive({
    selected_data() %>%
        group_by(Job_group, Gender) %>%
        summarise(count = n(), .groups = "drop") %>%
        ggplot(aes(x = Job_group, y = count, fill = Gender)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            theme_bw() +
            labs(x = "Job title", y = "Number of bakers", fill = "Gender") +
            scale_fill_manual(values = c("Female" = "#e3ddc3", "Male" = "#c3e3dd")) +
            theme(
                axis.text = element_text(size = 20),
                axis.text.x = element_text(size = 20, angle = 45, hjust = 1.0),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20)
            )
})

    output$bakers_by_job_gender <- renderPlot({
        plot3()
    })

    plot4 <- reactive({
    selected_data() %>%
        group_by(Job_group, cake_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        ggplot(aes(x = Job_group, y = count, fill = cake_type)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_fill_manual(
                values = c("Berries" = "#C94C4C", "Cheesecake" = "#F5E8E1", "Chocolate" = "#ae7549ff", "Other" = "#b0aaaaff"),
                labels = c("Berries", "Cheesecake", "Chocolate", "Other")
            ) +
            labs(x = "Job title", y = "Number of cakes", fill = "Cake type") +
            theme_bw() +
            theme(
                axis.text = element_text(size = 20),
                axis.text.x = element_text(size = 20, angle = 45, hjust = 1.0),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20)
            )
    })

    output$cake_type_pie <- renderPlot({
        plot4()
    })

    plot5 <- reactive({
    ggplot(selected_data(), aes(x = cake_type, y = Score, fill = cake_type)) +
        geom_boxplot(alpha = 0.7) +
        theme_bw() +
        labs(x = "Cake type", y = "Score", fill = "Cake type") +
        scale_fill_manual(values = c("Berries" = "#C94C4C", "Cheesecake" = "#F5E8E1", "Chocolate" = "#ae7549ff", "Other" = "#b0aaaaff"), labels = c("Berries", "Cheesecake", "Chocolate", "Other")) +
        theme(
            axis.text = element_text(size = 20),
            axis.text.x = element_text(size = 20, angle = 45, hjust = 1.0),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 20)
        )
    })

    output$score_vs_type <- renderPlot({
        plot5()
    })

    icons_list <- makeAwesomeIcon(
    icon = 'birthday-cake',
    iconColor = 'white',
    library = 'fa',
    markerColor = "purple"
)

    output$map1 <- renderLeaflet({ 
        leaflet(selected_data()) %>%
        addTiles() %>% 
        addAwesomeMarkers(
        lng = ~Longitude_average, lat = ~Latitude_average, popup = ~Nationality_of_the_cake, icon=icons_list) %>%
        addMiniMap(width = 150, height = 150)
        })
}

# Create the Shiny app object -------------------------------------------------------------

shinyApp(ui, server)
