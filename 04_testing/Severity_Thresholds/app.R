library(shiny)
library(shinythemes)
library(tidyverse)
library(extrafont)

########################################
######## Threshold calculation #########
########################################
################################################################################
## Run this commented code first then make it a comment again to make it work ##
################################################################################
# source(here::here("99_helpers", "helpers.R"))
# 
# file_paths <- get_paths_analysis()
# 
# df <- read_csv(file.path(file_paths$agg_dir,
#                          "2022_sectoral_sev.csv"))  %>%
#     mutate(disag = paste0(
#         adm1_name,
#         adm2_name,
#         adm3_name,
#         population_group,
#         administration
#     )) %>%
#     group_by(adm0_name,
#              disag) %>%
#     mutate(intersectoral = ifelse(
#                                 is.null(severity[sector == 'Intersectoral']), 
#                                 NA_real_,
#                                 severity[sector == 'Intersectoral'])) %>%
#     filter(sector_general != "intersectoral")
# 
# df_calc <- df %>%
#     group_by(
#         adm0_name,
#         disag
#     ) %>%
#     summarize(
#         quartile_3 = quantile(severity, 0.75),
#         n_5 = sum(severity == 5, na.rm = TRUE),
#         n_4 = sum(severity == 4, na.rm = TRUE),
#         n_3 = sum(severity == 3, na.rm = TRUE),
#         n_2 = sum(severity == 2, na.rm = TRUE),
#         n_1 = sum(severity == 1, na.rm = TRUE),
#         n_sectors = n(),
#         mean_sectors = mean(severity),
#         mean = mean(severity),
#         max_sectors = max(severity, na.rm = TRUE),
#         intersectoral = mean(intersectoral)
#     ) %>%
#     group_by(adm0_name) %>%
#     mutate(
#         country_sectors = max(n_sectors)
#     ) %>%
#     ungroup() %>%
#     filter(n_sectors > 3 & !is.na(intersectoral)) %>%
#     mutate(
#         optionA = case_when(
#             n_5 >= 1 & (n_5 + n_4 >= 4 | (n_5 + n_4) / n_sectors >= 0.5)  ~ 5,
#             n_4 + n_5 >= 4 | (n_4 + n_5) / n_sectors >= 0.5 ~ 4,
#             n_3 + n_4 + n_5 >= 4 |
#                 (n_3 + n_4 + n_5) / n_sectors >= 0.5 ~ 3,
#             n_3 + n_4 + n_5 + n_2 >= 4 |
#                 (n_3 + n_4 + n_5 + n_2) / n_sectors >= 0.5 ~ 2,
#             n_2 <= 3 | n_1 >= 1 ~ 1
#         ),
#         optionA2 = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 4 ~ 5,
#             n_4 + n_5 >= 4 ~ 4,
#             n_3 + n_4 + n_5 >= 4 ~ 3,
#             n_3 + n_4 + n_5 + n_2 >= 4 ~ 2,
#             n_2 <= 3 ~ 1
#         ),
#         optionB = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 3 ~ 5,
#             n_5 + n_4 >= 2 & n_5 + n_4 + n_3 >= 3 ~ 4,
#             n_5 + n_4 + n_3 >= 2 & n_5 + n_4 + n_3 + n_2 >= 3 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 2 ~ 2,
#             n_5 + n_4 + n_3 + n_2 < 2 | n_1 >= 1 ~ 1
#         ),
#         optionC = case_when(
#             n_5 >= 1 & n_5 + n_4 >= 3 ~ 5,
#             n_5 + n_4 >= 1 & n_5 + n_4 + n_3 >= 3 ~ 4,
#             n_5 + n_4 + n_3 >= 1 & n_5 + n_4 + n_3 + n_2 >= 3 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 1 ~ 2,
#             n_1 == n_sectors ~ 1
#         ),
#         optionD = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 4 ~ 5,
#             n_5 + n_4 >= 2 & n_5 + n_4 + n_3 >= 4 ~ 4,
#             n_5 + n_4 + n_3 >= 2 & n_5 + n_4 + n_3 + n_2 >= 4 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 2 ~ 2,
#             n_5 + n_4 + n_3 + n_2 < 2 | n_1 >= 1 ~ 1
#         ),
#         quartile_3 = round(quartile_3),
#         adm0_name = ifelse(adm0_name == "Cameroon", "Cameroon (6 Sectors)",
#                            paste0(adm0_name, " (", country_sectors, ")")),
#         flag1 = case_when(n_5 > 0 ~ "1 or more sectors in phase 5"),
#         flag2 = case_when(
#           n_4 >= 5 | n_4/n_sectors >= 0.75 ~ "5 sectors or more in phase 4"),
#         mean_sectors = round(mean_sectors)
#     ) %>%
#     pivot_longer(
#         cols = c(
#             quartile_3,
#             mean_sectors,
#             intersectoral,
#             max_sectors,
#             optionA,
#             optionA2,
#             optionB,
#             optionC,
#             optionD
#         ),
#         names_to = "calculation",
#         values_to = "severity"
#     ) %>%
#     mutate(
#         flag3 = case_when(
#           severity - mean >= 2 ~ "Preliminary is 2 phases away from average")
#     )
# 
# flags <- df_calc %>%
#     pivot_longer(
#         cols = c(flag1, flag2, flag3)
#     ) %>%
#     group_by(
#         adm0_name,
#         calculation,
#         severity,
#         value
#     ) %>%
#     summarise(
#         n_flags = n()
#     ) %>% filter(!is.na(value))
# 
# df_calc %>%
#     group_by(adm0_name,
#              calculation,
#              severity) %>%
#     summarize(n = n(),
#               .groups = "drop") %>%
#     left_join(flags,
#               by = c("adm0_name", "calculation", "severity")) %>%
#     mutate(
#         calculation = case_when(
#             calculation == "max_sectors" ~ "Max of sectoral severity",
#             calculation == "quartile_3" ~ "3rd Quartile of sectoral severity",
#             calculation == "mean_sectors" ~ "Mean of sectoral severity",
#             calculation == "intersectoral" ~ "Intersectoral (JIAF 1.1)",
#             calculation == "optionA" ~ "Option A (Proposed by MTWG)",
#             calculation == "optionA2" ~ "Option A2",
#             calculation == "optionB" ~ "Option B",
#             calculation == "optionC" ~ "Option C",
#             calculation == "optionD" ~ "Option D"
#         )
# 
#     ) %>%
#     saveRDS("Severity_Thresholds/data.RDS")
# 
# df_perm <- data.frame(s1 = 1:5) %>%
#     expand(
#         nesting(s1),
#         s2 = 1:5,
#         s3 = 1:5,
#         s4 = 1:5,
#         s5 = 1:5,
#         s6 = 1:5,
#         s7 = 1:5,
#         s8 = 1:5
#     ) %>%
#     mutate(
#         id = 1:nrow(.)) %>%
#     pivot_longer(
#         cols = matches("s[1-8]"),
#         values_to = "severity",
#         names_to = "sector"
#     ) %>%
#     group_by(id) %>%
#     summarize(
#         quartile_3 = quantile(severity, 0.75),
#         n_5 = sum(severity == 5, na.rm = TRUE),
#         n_4 = sum(severity == 4, na.rm = TRUE),
#         n_3 = sum(severity == 3, na.rm = TRUE),
#         n_2 = sum(severity == 2, na.rm = TRUE),
#         n_1 = sum(severity == 1, na.rm = TRUE),
#         mean_sectors = mean(severity),
#         mean = mean(severity),
#         max_sectors = max(severity, na.rm = TRUE),
#         .groups = "drop"
#     ) %>%
#     mutate(
#         optionA = case_when(
#             n_5 >= 1 & (n_5 + n_4 >= 4 | (n_5 + n_4) / 8 >= 0.5)  ~ 5,
#             n_4 + n_5 >= 4 | (n_4 + n_5) / 8 >= 0.5 ~ 4,
#             n_3 + n_4 + n_5 >= 4 |
#                 (n_3 + n_4 + n_5) / 8 >= 0.5 ~ 3,
#             n_3 + n_4 + n_5 + n_2 >= 4 |
#                 (n_3 + n_4 + n_5 + n_2) / 8 >= 0.5 ~ 2,
#             n_2 <= 3 | n_1 >= 1 ~ 1
#         ),
#         optionA2 = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 4 ~ 5,
#             n_4 + n_5 >= 4 ~ 4,
#             n_3 + n_4 + n_5 >= 4 ~ 3,
#             n_3 + n_4 + n_5 + n_2 >= 4 ~ 2,
#             n_2 <= 3 ~ 1
#         ),
#         optionB = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 3 ~ 5,
#             n_5 + n_4 >= 2 & n_5 + n_4 + n_3 >= 3 ~ 4,
#             n_5 + n_4 + n_3 >= 2 & n_5 + n_4 + n_3 + n_2 >= 3 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 2 ~ 2,
#             n_5 + n_4 + n_3 + n_2 < 2 | n_1 >= 1 ~ 1
#         ),
#         optionC = case_when(
#             n_5 >= 1 & n_5 + n_4 >= 3 ~ 5,
#             n_5 + n_4 >= 1 & n_5 + n_4 + n_3 >= 3 ~ 4,
#             n_5 + n_4 + n_3 >= 1 & n_5 + n_4 + n_3 + n_2 >= 3 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 1 ~ 2,
#             n_1 == 8 ~ 1
#         ),
#         optionD = case_when(
#             n_5 >= 2 & n_5 + n_4 >= 4 ~ 5,
#             n_5 + n_4 >= 2 & n_5 + n_4 + n_3 >= 4 ~ 4,
#             n_5 + n_4 + n_3 >= 2 & n_5 + n_4 + n_3 + n_2 >= 4 ~ 3,
#             n_5 + n_4 + n_3 + n_2 >= 2 ~ 2,
#             n_5 + n_4 + n_3 + n_2 < 2 | n_1 >= 1 ~ 1
#         ),
#         quartile_3 = round(quartile_3),
#         perm = paste(n_5 ,n_4 , n_3 , n_2 , n_1),
#         flag1 = case_when(n_5 > 0 ~ "1 or more sectors in phase 5"),
#         flag2 = case_when(
#           n_4 >= 5 | n_4/8 >= 0.75 ~ "5 sectors or more in phase 4"
#         ),
#         mean_sectors = round(mean_sectors)
#     ) %>%
#     pivot_longer(
#         cols = c(
#             quartile_3,
#             mean_sectors,
#             max_sectors,
#             optionA,
#             optionA2,
#             optionB,
#             optionC,
#             optionD
#         ),
#         names_to = "calculation",
#         values_to = "severity"
#     ) %>%
#     mutate(
#         flag3 = case_when(
#           severity - mean >= 2 ~ "Preliminary is 2 phases away from average"
#         )
#     )
# 
# df_perm_unique <- distinct(df_perm, 
#                            paste(perm, calculation), 
#                            .keep_all = TRUE)
# 
# flags <- df_perm_unique %>%
#     pivot_longer(
#         cols = c(flag1, flag2, flag3)
#     ) %>%
#     group_by(
#         calculation,
#         severity,
#         value
#     ) %>%
#     summarise(
#         n_flags = n()
#     ) %>% filter(!is.na(value))
# 
# df_perm_unique %>%
#     group_by(
#          calculation,
#          severity) %>%
#     summarize(n = n(),
#               .groups = "drop") %>%
#     left_join(flags,
#               by = c("calculation", "severity")) %>%
#     mutate(
#         calculation = case_when(
#             calculation == "max_sectors" ~ "Max of sectoral severity",
#             calculation == "quartile_3" ~ "3rd Quartile of sectoral severity",
#             calculation == "mean_sectors" ~ "Mean of sectoral severity",
#             calculation == "intersectoral" ~ "Intersectoral (JIAF 1.1)",
#             calculation == "optionA" ~ "Option A (Proposed by MTWG)",
#             calculation == "optionA2" ~ "Option A2",
#             calculation == "optionB" ~ "Option B",
#             calculation == "optionC" ~ "Option C",
#             calculation == "optionD" ~ "Option D"
#         )
# 
#     ) %>%
#     saveRDS("Severity_Thresholds/8_sectors_permutes.RDS")

#########################
####### Read Data #######
#########################

df_overlap <- readRDS("data.RDS")
df_permutes <- readRDS("8_sectors_permutes.RDS")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                title = "Severity Thresholds Test",
                sidebarLayout(
                    sidebarPanel(
                        style = paste("justify-content: space-evenly;",
                                      "padding:5px;align-items:center")
                        ,
                        checkboxGroupInput(
                            inputId = "methods",
                            label = "Select methods: ",
                            choices = c(
                                # "Max of sectoral severity",
                                # "3rd Quartile of sectoral severity",
                                "Mean of sectoral severity",
                                "Intersectoral (JIAF 1.1)",
                                "Option A (Proposed by MTWG)",
                                "Option A2",
                                "Option B",
                                "Option C",
                                "Option D"
                            ),
                            inline = FALSE,
                            selected = "Mean of sectoral severity"
                        ),
                        shiny::img(src = "Thresholds.png", 
                                   alt = "thresholds", 
                                   width = "100%", 
                                   height = "500px")
                    ),
                    
                    mainPanel(
                        tabsetPanel(
                            tabPanel(title = "Countries Data",
                                     plotOutput(
                                         outputId = "countryData",
                                         height = "600px",
                                         width = "100%"
                                     )),
                            tabPanel(title = "8 Sectors Permutations",
                                     plotOutput(
                                         outputId = "permutesData",
                                         height = "600px",
                                         width = "100%"
                                         
                                     ))
                        )
                    )
                ))

# Define server function
server <- function(input, output) {
    selected_countryMethods <- reactive({
        req(input$methods)
        df_overlap %>% filter(calculation %in% input$methods)
    })
    
    selected_permutesMethods <- reactive({
        req(input$methods)
        df_permutes %>% filter(calculation %in% input$methods)
    })
    
    output$countryData <- renderPlot({
        selected_countryMethods() %>%
            ggplot(                
                aes(
                x = severity,
                y = n,
                label = n,
                fill = factor(calculation,
                              levels = input$methods)
            )) +
            geom_col(
                position = position_dodge(width = 0.9),
                width = 0.9
            ) +
            geom_line(aes(y = n_flags, 
                          alpha = 0.8, 
                          linetype = value, 
                          color = factor(calculation, levels = input$methods)),
                      stat = "identity",
                      size = 2) +
            geom_point(aes(y = n_flags,
                           alpha = 0.8, 
                           shape = value, 
                           color = factor(calculation, 
                                          levels = input$methods)),
                       size = 3
                      ) +
            geom_text(
                size = 3,
                hjust = .5,
                vjust = -0.5,
                position = position_dodge(0.9),
                inherit.aes = TRUE
            ) +
            scale_fill_manual(
                values = c(
                    # "Max of sectoral severity" = "#C25048",
                    # "3rd Quartile of sectoral severity" = "#66B0EC",
                    "Mean of sectoral severity" = "#66B0EC",
                    "Intersectoral (JIAF 1.1)" = "#F2645A",
                    "Option A (Proposed by MTWG)" = "#1EBFB3",
                    "Option A2" = "#FFBF00",
                    "Option B" = "#D989B5",
                    "Option C" = "#BCE29E",
                    "Option D" = "#98A8F8"
                )
            ) +
            scale_color_manual(
                values = c(
                    # "Max of sectoral severity" = "#C25048",
                    # "3rd Quartile of sectoral severity" = "#66B0EC",
                    "Mean of sectoral severity" = "#0C6A8C",
                    "Intersectoral (JIAF 1.1)" = "#B7281F",
                    "Option A (Proposed by MTWG)" = "#217770",
                    "Option A2" = "#A68215",
                    "Option B" = "#984070",
                    "Option C" = "#80A066",
                    "Option D" = "#505DA0"
                )
            ) +
            facet_wrap( ~ adm0_name,
                        scales = "free_y") +
            labs(
                x = "Severity",
                title = paste(
                    "Distribution of severity scores",
                    "calculated in various methods over the units of analysis."
                ),
                y = "# of unit of analysis",
                fill = "",
                shape = "Flags",
                color = ""
            ) +
            scale_y_continuous(expand = c(0.15, 0)) +
            scale_shape_discrete(na.translate = F) +
            guides(alpha = "none", 
                   linetype = "none", 
                   shape = guide_legend(nrow = 2)
                  ) +
            theme_minimal() +
            theme(
                plot.title = element_text(
                    face = "bold",
                    size = 18,
                    margin = margin(10, 10, 10, 10, "pt"),
                    family = "Roboto",
                    hjust = .5
                ),
                plot.background = element_rect(fill = "white"),
                axis.text = element_text(
                    size = 10,
                    family = "Roboto",
                    margin = margin(r = 10, unit = "pt")
                ),
                axis.title.y = element_text(
                    face = "bold",
                    size = 14,
                    family = "Roboto",
                    margin = margin(r = 20)
                ),
                axis.title.x = element_text(
                    face = "bold",
                    size = 14,
                    family = "Roboto",
                    margin = margin(t = 20)
                ),
                legend.text = element_text(size = 12,
                                           family = "Roboto"),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill = "transparent"),
                legend.box.background = element_rect(fill = "transparent"),
                strip.text = element_text(size = 16,
                                          family = "Roboto")
            )
    })
    
    output$permutesData <- renderPlot({
        selected_permutesMethods() %>%            
            ggplot(                
            aes(
                x = severity,
                y = n,
                label = n,
                fill = factor(calculation,
                              levels = input$methods)
            )) +
            geom_col(
                position = position_dodge(width = 0.9),
                width = 0.9
            ) +
            geom_line(aes(y = n_flags,
                          alpha = 0.8,
                          linetype = value, 
                          color = factor(calculation, levels = input$methods)),
                      stat = "identity",
                      size = 2) +
            geom_point(aes(y = n_flags, 
                           alpha = 0.8, 
                           shape = value, 
                           color = factor(calculation, levels = input$methods)),
                       size = 3
                      ) +
            geom_text(
                size = 3,
                hjust = .5,
                vjust = -0.5,
                position = position_dodge(0.9),
                inherit.aes = TRUE
            ) +
            scale_fill_manual(
                values = c(
                    # "Max of sectoral severity" = "#C25048",
                    # "3rd Quartile of sectoral severity" = "#66B0EC",
                    "Mean of sectoral severity" = "#66B0EC",
                    "Intersectoral (JIAF 1.1)" = "#F2645A",
                    "Option A (Proposed by MTWG)" = "#1EBFB3",
                    "Option A2" = "#FFBF00",
                    "Option B" = "#D989B5",
                    "Option C" = "#BCE29E",
                    "Option D" = "#98A8F8"
                )
            ) +
            scale_color_manual(
                values = c(
                    # "Max of sectoral severity" = "#C25048",
                    # "3rd Quartile of sectoral severity" = "#66B0EC",
                    "Mean of sectoral severity" = "#0C6A8C",
                    "Intersectoral (JIAF 1.1)" = "#B7281F",
                    "Option A (Proposed by MTWG)" = "#217770",
                    "Option A2" = "#A68215",
                    "Option B" = "#984070",
                    "Option C" = "#80A066",
                    "Option D" = "#505DA0"
                )
            ) +
            labs(
                x = "Severity",
                title = paste(
                  "Distribution of severity scores calculated in various",
                  "methods over possible cases when considering only 8 sectors."
                ),
                y = "# of possible cases",
                fill = "",
                shape = "Flags",
                color = ""
            ) +
            scale_y_continuous(expand = c(0.15, 0)) +
            scale_shape_discrete(na.translate = F) +
            guides(alpha = "none", 
                   linetype = "none",
                   shape = guide_legend(nrow = 2)
                  ) +
            theme_minimal() +
            theme(
                plot.title = element_text(
                    face = "bold",
                    size = 18,
                    margin = margin(10, 10, 10, 10, "pt"),
                    family = "Roboto",
                    hjust = .5
                ),
                plot.background = element_rect(fill = "white"),
                axis.text = element_text(
                    size = 10,
                    family = "Roboto",
                    margin = margin(r = 10, unit = "pt")
                ),
                axis.title.y = element_text(
                    face = "bold",
                    size = 14,
                    family = "Roboto",
                    margin = margin(r = 20)
                ),
                axis.title.x = element_text(
                    face = "bold",
                    size = 14,
                    family = "Roboto",
                    margin = margin(t = 20)
                ),
                legend.text = element_text(size = 12,
                                           family = "Roboto"),
                legend.position = "bottom",
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill = "transparent"),
                legend.box.background = element_rect(fill = "transparent"),
                strip.text = element_text(size = 16,
                                          family = "Roboto")
            )
    })
}

shinyApp(ui = ui, server = server)
