# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(tools)
library(shinythemes)
library(corrplot)

# Load data --------------------------------------------------------------------

#diabetes <- read.csv(file = "https://raw.githubusercontent.com/KengTan/S3729C_Shiny/main/diabetes.csv", header = TRUE, sep = ",")
diabetes <- read.csv(file = "https://raw.githubusercontent.com/KengTan/S3729C_Shiny/main/diabetes_rev.csv", header = TRUE, sep = ",")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("slate"),
  
  # App title ----
  titlePanel(HTML("Diabetes Dataset")),
  
  # Navbar Panel with two tabs
  navbarPage(
    title = "Diabetes Dataset",
    tabPanel("Home",
             
             sidebarLayout(
               
               # Inputs: Select variables to plot
               sidebarPanel(
                 
                 # Select variable for y-axis
                 selectInput(
                   inputId = "y",
                   label = "Y-axis:",
                   choices = c(
                     "Systolic Blood Pressure" = "SBP",
                     "Diastolic Blood Pressure" = "DBP",
                     "Fasting Plasma Glucose" = "FPG",
                     "Cholesterol" = "Chol",
                     "Triglyceride" = "Tri",
                     "High-Density Lipoprotein" = "HDL",
                     "Low-Density Lipoprotein" = "LDL",
                     "Alanine Aminotransferase" = "ALT",
                     "Blood urea nitrogen" = "BUN",
                     "Creatinine Clearance" = "CCR",
                     "Final Fasting Plasma Glucose" = "FFPG"
                   ),
                   selected = "HDL"
                 ),
                 
                 # Select variable for x-axis
                 selectInput(
                   inputId = "x",
                   label = "X-axis:",
                   choices = c(
                     "Age" = "Age",
                     "Body Mass Index" = "BMI"
                   ),
                   selected = "Age"
                 ),
                 
                 # Select variable for color
                 selectInput(
                   inputId = "z",
                   label = "Color by:",
                   choices = c(
                     "Gender" = "Gender",
                     "Smoking" = "Smoking",
                     "Drinking" = "Drinking",
                     "Family History" = "family_history",
                     "Diabetes" = "Diabetes"
                   ),
                   selected = "smoking"
                 )
               ),
               
               # Output: Show scatterplot
               mainPanel(
                 plotOutput(outputId = "scatterplot"),
                 textOutput(outputId = "avg_x"), # avg of x
                 textOutput(outputId = "avg_y"), # avg of y
                 verbatimTextOutput(outputId = "lmoutput") # regression output
               )
             )
    ),
    
    tabPanel("Descriptive",
             p(HTML("

            <b>Histogram</b><br>
            <ul><li>To view a two-dimensional data such as below Age versus Diabetes</li></ul><br><br>")),
             
             plotOutput(outputId = "histogram"),
             
             p(HTML("
            
            <b>Boxplot</b><br>
            <ul><li>To view the distribution of the numeric attributes</li></ul><br><br>")),
             
             plotOutput(outputId = "boxplot")
    ),
    
    tabPanel("Knowledge Check",
             p(HTML("

<b>Dependent and Independent Variables</b><br>
<ul><li>In health research there are generally two types of variables. Independent variables are what we expect will influence dependent variables. A dependent variable is what happens as a result of the independent variable. Generally, the dependent variable is the disease or outcome of interest for the study, and the independent variables are the factors that may influence the outcome. For example, if we want to explore whether high concentrations of vehicle exhaust impact incidence of asthma in children, the concentration of vehicle exhaust is the independent variable (on the x-axis) while asthma incidence is the dependent variable (on the y-axis).</li></ul><br><br>

<b>p-value</b><br>
<ul><li>A p-value is the measure of probability that the null hypothesis was rejected when in fact the null hypothesis is true. When thinking about the standard normal distribution (bell curve), the p-value corresponds to the area under the curve where extreme values are not likely to be the result of chance. The p-value can be calculated using a calculator with the test statistic (z-test or t-test).

The lower the p-value, the smaller the chance we mistakenly rejected the null when the null was true.
The p-value is related to the significance level. If the critical alpha value is 0.05, then the p-value must be smaller than 0.05 for the test to have a statistically significant result. If the p-value is greater than the critical alpha value, then the test does not have a statistically significant result.</li></ul><br><br>

Credit the source:<br>
<li>https://www.nlm.nih.gov/oet/ed/stats/02-200.html</li>"))
    ),
    
    tabPanel("About",
             p(HTML("Diabetes is the fourth leading cause of death in the world and one of the most common endocrine disorders. According to studies, Type 2 diabetes kills thousands of people around the world every year and imposes huge costs on societies in the form of surgeries and other treatment programs, as well as controlling complications and disability. Therefore, predicting and early diagnosis of this disease can greatly help governments and patients.

This dataset is the output of a Chinese research study conducted in 2016. It includes 1304 samples of patients who tested positive for diabetes, and the age of the participants ranges from 21 to 99 years old. The dataset was collected according to the indicators and standards of the World Health Organization, making it a reliable source for building diabetes diagnosis models. Researchers and healthcare professionals can use this dataset to train and test machine learning models to predict and diagnose diabetes in patients.
<br><br>

Features of Dataset:<br>
<ul><li>Age</li><li>Gender</li><li>BMI (Body Mass Index)</li><li>SBP (Systolic Blood Pressure)</li>
<li>DBP (Diastolic Blood Pressure)</li><li>FPG (Fasting Plasma Glucose)</li><li>FFPG (Final Fasting Plasma Glucose)</li><li>Cholesterol</li>
<li>Triglyceride</li><li>HDL (High-Density Lipoprotein)</li><li>LDL (Low-Density Lipoprotein)</li><li>ALT (Alanine Aminotransferase)</li>
<li>BUN (Blood urea nitrogen)</li><li>CCR (Creatinine Clearance)</li><li>Smoking Status</li><li>Drinking Status</li>
<li>Drinking Status</li><li>Diabetes</li></ul><br><br>

Credit the source:<br>
<li>https://www.kaggle.com/datasets/pkdarabi/diabetes-dataset-with-18-features</li>"))
    )
  )      
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = diabetes, aes_string(x = input$x, y = input$y, 
                                       color = input$z)) +
      geom_jitter(alpha = 0.5, width = 0.5, height = 0.5) +
      stat_smooth(method = lm, level = 0.99)
  })
  
  output$avg_x <- renderText({
    avg_x <- diabetes %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, "=", avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- diabetes %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })
  
  output$lmoutput <- renderPrint({
    x <- diabetes %>% pull(input$x)
    y <- diabetes %>% pull(input$y)
    print(summary(lm(y ~ x, data = diabetes)), digits = 3, signif.stars = FALSE)
  })
  
  output$histogram <- renderPlot({
    ggplot(data = diabetes, mapping = aes(x = Age, fill = Diabetes)) +
      stat_bin(bins = 30, color = "white", fill = "steelblue")
  })
  
  output$boxplot <- renderPlot({
    ggplot(diabetes, aes(x = factor(Age), y = SBP)) +
      geom_boxplot(colour = "blue")
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)