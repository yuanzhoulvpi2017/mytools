library(tidyverse)
library(shiny)
# library(DT)
if (!require("DT")) {install.packages("DT")}
if (!require("ggrepel")) {install.packages("ggrepel")}

# data --------------------------------------------------------------------


all_data <- data.frame("factors" = c(rep("type1", 10), rep("type2", 20), rep("type3", 15)),
                       "value" = c(rnorm(10, mean = 2, sd = 2),
                                   rnorm(20, mean = 3, sd = 1),
                                   rnorm(15, mean = 1.8, sd = 1)))




# my shiny part -----------------------------------------------------------


temp = ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']

ui <- fluidPage(
  
  titlePanel("one way anova"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h1("choose a data"),
      selectInput(inputId = "all_data", label = "choose a data to analysis", choices = temp),
      hr(),
      h1("raw data boxplot"),
      numericInput(inputId = "boxplot_line", label = "boxplot line width", value = 1, min = 0, max = 10, step = 0.1),
      numericInput(inputId = "boxplot_alpha", label = "boxplot alpha", value = 1, min = 0, max = 1, step = 0.05),
      textInput(inputId = "boxplot_title_text", label = "boxplot title", value = "this is a boxplot title"),
      sliderInput(inputId = "boxplot_title_hjust", label = "boxplot title hjust", value = 0.5, min = 0, max = 1),
      hr(),
      h1("axis text"),
      numericInput(inputId = "axisx_text_angle", label = "axisx_text_angle", value = 0, min = 0, max = 360, step = 20),
      numericInput(inputId = "axisx_text_hjust", label = "axisx_text_hjust", value = 0.5, min = 0, max = 1, step = 0.1),
      # textInput(inputId = "boxplot_title_mean_text", label = "boxplot title", value = "this is a boxplot title"),
      # sliderInput(inputId = "boxplot_title_mean_hjust", label = "boxplot title hjust", value = 0.5, min = 0, max = 1),
      
    ),
    mainPanel = mainPanel(
      h1("descirbe data"),
      DTOutput(outputId = "describe_data"),
      h1("raw data boxplot"),
      plotOutput(outputId = "plot_boxplot"),
      h1("mean boxplot"),
      plotOutput(outputId = "plot_mean_boxplot"),
      h1("summary text"),
      verbatimTextOutput(outputId = "summary_resaov"),
      h1("summary text to data"),
      DTOutput(outputId = "order_summary_data"),
      h1("residual plot"),
      plotOutput(outputId = "residuals_plot"),
      h1("tukey data"),
      DTOutput(outputId = "order_tkudey_data")
      
      
    )
  )
)

server <- function(input, output, session) {

#展示描述性统计 ---------------------------------------------------------------
  desc_data <- reactive({
    req(input$all_data)
    desc_data <- get(input$all_data) %>% group_by(factors) %>% 
      summarise(n = dplyr::n(), mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE))
    desc_data
  })
  output$describe_data <- renderDT(desc_data())
  

#画图boxplot ------------------------------------------------------------


  plot_boxplot <- reactive({
    req(input$all_data)
    req(input$boxplot_line)
    req(input$boxplot_alpha)
    req(input$boxplot_title_text)
    req(input$boxplot_title_hjust)
    req(input$axisx_text_angle)
    req(input$axisx_text_hjust)
    # cat("test boxplot")
    
    p <- ggplot(data = get(input$all_data), aes(x = factors, y = value, color = factors, fill = factors)) + 
      geom_boxplot(alpha = input$boxplot_alpha) +
      geom_jitter() + 
      theme_classic() + 
      labs(x = "my factors", y = "values", title = paste0("raw_data: ", input$boxplot_title_text)) + 
      theme(axis.text.x = element_text(hjust = input$axisx_text_hjust, angle = input$axisx_text_angle),
            plot.title = element_text(hjust = input$boxplot_title_hjust))
    p
  })
  
  output$plot_boxplot <- renderPlot(plot_boxplot())
  

# mean part ---------------------------------------------------------------

  plot_mean_boxplot <- reactive({
    req(input$boxplot_line)
    req(input$boxplot_alpha)
    req(input$boxplot_title_text)
    req(input$boxplot_title_hjust)
    req(input$axisx_text_angle)
    req(input$axisx_text_hjust)
    
    p <- ggplot() + 
      geom_errorbar(data = desc_data(), 
                    aes(x = factors, y = mean, ymin = mean - sd/sqrt(n), ymax = mean + sd/sqrt(n),
                        color = factors), width=0.1, size = 1) +
      geom_line(data = desc_data(), 
                aes(x = factors, y = mean, group = 1)) + 
      geom_jitter(data = get(input$all_data), aes(x = factors, y = value, color = factors), width = 0.3) +
      theme_classic() + 
      labs(x = "my factors", y = "values", title = paste0("mean: ", input$boxplot_title_text)) + 
      theme(axis.text.x = element_text(hjust = input$axisx_text_hjust, angle = input$axisx_text_angle),
            plot.title = element_text(hjust = input$boxplot_title_hjust))
    p 
  })
  output$plot_mean_boxplot <- renderPlot(plot_mean_boxplot())
  


# cal one anova  ----------------------------------------------------------
  
  one_anova_model <- reactive({
    req(input$all_data)
    
    res.aov <- aov(value ~ factors, data = get(input$all_data))
    # Summary of the analysis
    summary_resaov <- summary(res.aov)
    tukeyhsd_resaov <- TukeyHSD(res.aov)
    
    residuals_data <- data.frame("residuals" = res.aov[["residuals"]], "fitted_values" = res.aov[["fitted.values"]])
    
    all_result <- list("summary_resaov" = summary_resaov,
                       # "tukeyhsd_resaov" = tukeyhsd_resaov,
                       "residuals_data" = residuals_data,
                       "order_tukeyhsd_data" = tukeyhsd_resaov[["factors"]],
                       "order_sumamry_resaov" = summary_resaov[[1]])
    all_result
  })
  
  output$summary_resaov <- renderPrint({
    one_anova_model()[['summary_resaov']]
  })
  output$order_summary_data <- renderDT({
    one_anova_model()[['order_sumamry_resaov']]
  })
  
  output$residuals_plot <- renderPlot({
    p <- one_anova_model()[['residuals_data']] %>% bind_cols(get(input$all_data) %>% dplyr::select(factors)) %>% 
      mutate(n = 1:dplyr::n()) %>% 
      ggplot(aes(x = fitted_values, y = residuals)) + 
      geom_point(aes(color = factors)) +
      geom_hline(aes(yintercept = 0), linetype="dashed") +
      geom_text_repel(aes(label = n, color = factors), show.legend = FALSE) + 
      theme_classic() + 
      labs(x = "fitted_values", y = "residuals", title = "this is a title fitted_values and residuals") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    p
  })
  
  output$order_tkudey_data <- renderDT({
    one_anova_model()[['order_tukeyhsd_data']]
  })
  
  
}

# copy from :
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
shinyApp(ui, server)