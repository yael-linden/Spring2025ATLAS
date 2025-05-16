#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(bslib)
library(shiny)
library(forcats)
library(ggplot2)
library(randomForest)
library(dplyr)
library(MASS)
library(iml)
library(counterfactuals)
library(modelr)
set.seed(123)
#######################################################
 # DATA WRANGLING

obesity <- read.csv("obesity_dataset.csv")

obesity_levels <- c("Insufficient_Weight", 
                    "Normal_Weight", 
                    "Overweight_Level_I", 
                    "Overweight_Level_II",
                    "Obesity_Type_I",
                    "Obesity_Type_II",
                    "Obesity_Type_III"
)

freq_levels <- c(
  "no",
  "Sometimes",
  "Frequently",
  "Always"
)

yes_no_levels <- c("no", "yes")

ob_fct_default <- obesity %>%
  mutate(NObeyesdad = factor(obesity$NObeyesdad, levels= obesity_levels)) %>%
  mutate(CAEC = factor(CAEC, levels = freq_levels)) %>%
  mutate(CALC = factor(CALC, levels= freq_levels)) %>%
  dplyr::select(-c("Height", "Weight", "MTRANS", "Gender", "SMOKE", "SCC", "NCP", "TUE")) %>% 
  rename(family_history = family_history_with_overweight) %>% 
  rename(high_calorie = FAVC) %>% 
  rename(alcohol = CALC) %>% 
  rename(physical_activity = FAF) %>% 
  rename(veggies = FCVC) %>% 
  rename(water = CH2O) %>% 
  rename(age = Age) %>% 
  rename(eating_between_meals = CAEC) %>% 
  mutate(NObeyesdad = fct_collapse(NObeyesdad, Overweight = c("Overweight_Level_I", "Overweight_Level_II")))

########################################################

# BUILD MODEL

ob_fct_default <- tibble::rowid_to_column(ob_fct_default, "ID")

ob_fct_d <- ob_fct_default

ob_fct_d <- ob_fct_d[sample(1:nrow(ob_fct_d)), ] # shuffling the rows

train_d <- ob_fct_d %>% dplyr::sample_frac(0.75)
test_d  <- dplyr::anti_join(ob_fct_d, train_d, by = 'ID')

well_sample <- rbind(
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Insufficient_Weight"), 200, replace = T),
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Normal_Weight"), 200, replace = T),
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Overweight"), 200, replace = T),
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Obesity_Type_I"), 200, replace = T),
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Obesity_Type_II"), 200, replace = T),
  dplyr::sample_n(train_d %>% dplyr::filter(NObeyesdad == "Obesity_Type_III"), 200, replace = T)
)

RandomForestPredictor <- randomForest(x = well_sample %>% dplyr::select(-NObeyesdad, -ID), 
                        y = well_sample$NObeyesdad, 
                        ntree = 700, 
                        replace = TRUE,
                        importance = TRUE
)

newdata_d = test_d %>% dplyr::select(-ID, -NObeyesdad)

########################################################

# PREDICT PROB AND COUNTERFACTUALS

predictor <- iml::Predictor$new(RandomForestPredictor, type = "prob", data = newdata_d)
predictor$predict(newdata_d)

moc_classif <- MOCClassif$new(
  predictor, epsilon = 0, fixed_features = c("age", "family_history"),
  quiet = TRUE, termination_crit = "genstag", n_generations = 10L
)

predict_prob <- function(newdata) {
  predictor$predict(newdata)
}

extract_meaningful_vars <- function(df) {

  vars <- c("age", "family_history", "high_calorie", "veggies", "eating_between_meals", "water", "physical_activity", "alcohol");
  important_vars <- c()

  for(var in vars) {
    if( (sum(is.na(df %>% dplyr::select(all_of(var)))) / nrow(df)) < 0.70 ) {
      important_vars <- append(important_vars, var)
    }
  }
  return(important_vars)
}

extract_recommendation <- function(df, vars) {
  recommendations <- c()
  for(var in vars) {
    if(typeof( unlist( na.omit(df %>% dplyr::select(all_of(var))) )) == "double" ) {
      t <- unlist( na.omit(df %>% dplyr::select(all_of(var))) )
      ifelse(mean(t) > 0,
             recommendations <- append(recommendations, sprintf("increase %s", var)),
             recommendations <- append(recommendations, sprintf("decrease %s", var)))
    } else {
      q <- unlist( na.omit(df %>% dplyr::select(all_of(var))))
      recommendations <- append(recommendations, sprintf("change %s to %s", var, typical(unlist(q))))
    }
  }
  return(recommendations)
}

extract_counterfactuals <- function(newdata) {
  moc_classif <- MOCClassif$new(
    predictor, epsilon = 0, fixed_features = c("age", "family_history"),
    quiet = TRUE, termination_crit = "genstag", n_generations = 10L
  )
  cfactuals <- moc_classif$find_counterfactuals(
    newdata, desired_class = "Normal_Weight", desired_prob = c(0.6, 1)
  )
  cfactuals$subset_to_valid()
  change <- cfactuals$evaluate(show_diff = TRUE,
                               measures = c("dist_x_interest", "dist_target", "no_changed", "dist_train"))
  return(extract_recommendation(change, extract_meaningful_vars(change)))

}

extract_counterfactuals1 <- function(newdata) {
  moc_classif <- MOCClassif$new(
    predictor, epsilon = 0, fixed_features = c("age", "family_history"),
    quiet = TRUE, termination_crit = "genstag", n_generations = 10L
  )
  cfactuals <- moc_classif$find_counterfactuals(
    newdata, desired_class = "Normal_Weight", desired_prob = c(0.6, 1)
  )
  cfactuals$subset_to_valid()
  change <- cfactuals$evaluate(show_diff = TRUE,
                               measures = c("dist_x_interest", "dist_target", "no_changed", "dist_train"))
  pred <- as.character( last(predict(RandomForestPredictor, newdata = rbind(newdata_d, newdata))), n = 1 )
  plot <- cfactuals$plot_freq_of_feature_changes(subset_zero = TRUE)
  recs <- extract_recommendation(change, extract_meaningful_vars(change))
  return(list(pred = pred, plot = plot, recs = recs))
}


extract_vars_plot <- function(newdata) {
  cfactuals <- moc_classif$find_counterfactuals(
    newdata, desired_class = "Normal_Weight", desired_prob = c(0.6, 1)
  )
  cfactuals$subset_to_valid()
  return(cfactuals$plot_freq_of_feature_changes(subset_zero = TRUE))
}
########################################################

class <- c("Insufficient_Weight", "Normal_Weight", "Overweight", "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III")
BMI <- c("BMI under 18.5 kg/m^2", 
         "BMI between to 18.5 and 24.9 kg/m^2", 
         "BMI between 25 and 29.9 kg/m^2", 
         "BMI between 30 and 34.9 kg/m^2", 
         "BMI between 35 and 39.9 kg/m^2", 
         "BMI greater than or equal to 40 kg/m^2")
df_bmiclass <- data.frame(class = class, BMI = BMI)

# Define UI for application that draws a histogram
ui <- page_navbar(
  title = "Tabs",
  tabPanel(
    
    title = "Weight Risk Level",
    
    sidebarLayout(
      
      sidebarPanel(
        
        sliderInput("age", "Please move the slider to indicate your age:", min = 15, max = 60, value = 35),
        selectInput("history", "Does your family have a history of obesity?", choices = c("yes", "no")),
        selectInput("high_cal", "Do you eat high caloric food frequently?", choices = c("yes", "no")),
        selectInput("eat_between", "Do you eat between meals?", choices = freq_levels),
        selectInput("veggies", "Rate your vegetable consumption from 1 to 3 (1 being the lowest)", choices = 1:3),
        selectInput("water", "Rate your water consumption from 1 to 3 (1 being the lowest)", choices = 1:3),
        selectInput("alcohol", "How frequently do you consume alcohol?", choices = freq_levels),
        selectInput("phys", "Rate your physical activity from 0 to 3 (0 being the lowest)", choices = 0:3),
        
        # radioButtons("ptype", "Select model type", choices = c("Random Forest", "XGBoost")),
        submitButton(text = "Update Input"),
        
      ), # sidebar panel
      mainPanel(
        verbatimTextOutput("prediction"),
        textOutput("accuracy"),
        plotOutput("graphics"),
        verbatimTextOutput("recs"),
        tableOutput("bmiclass")
      ) # main panel
      
    ) # sidebar layout
    
  ), # tab panel
  tabPanel(
    title = "About",
    fluidPage(
      titlePanel("About the Obesity Risk Dataset and Prediction Models"),
      p("This app calculates your weight risk classification using a combination of eating habits, lifestyle habits, and family history."),
      p("The predictions range from Underweight, up to Type III Obesity."),
      p("These predictions are derived from an original dataset containing 2111 observations, along with lifestyle habits and family history, as well as obesity levels. 
        The original dataset can be seen at:", 
        a(href = "https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition", 
          "https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition")),
      p("After data cleaning, this dataset contains 10 columns: Age, family history of Obesity, vegetable consumption, water consumption, high-calorie food consumption, eating in between meals, alcohol consumption, and physical activity"),
      p("To predict your weight risk, this website uses the Random Forest model, and the XGBoost model."),
      p("Both are decision tree-based models, which construct a prediction based on past observations of obesity levels."),
      p("These models are far from perfect, so please take their predictions very lightly!")
    ) # end of fluid page
    
  ) # end of about tab panel
) # ui 

# Define server logic required to draw a histogram
server <- function(input, output) {
  # saving inputs
  age <- reactive(input$age)
  high_cal <- reactive(input$high_cal)
  history <- reactive(input$history)
  alcohol <- reactive(input$alcohol)
  eat_between <- reactive(input$eat_between)
  veggies <- reactive(input$veggies)
  water <- reactive(input$water)
  phys <- reactive(input$phys)
  
  output$prediction <- renderPrint({

      # prepare user output
      # 
      cat("Random Forest Prediction:", "\n")
      cat(
      as.character( last(predict(RandomForestPredictor, newdata = rbind(newdata_d, data.frame(
        age = age(),
        family_history = history(),
        high_calorie = high_cal(),
        veggies = veggies(),
        eating_between_meals = factor(eat_between(), levels = freq_levels),
        water = water(),
        physical_activity = phys(),
        alcohol = factor(alcohol(), levels = freq_levels))
        ))),
        n = 1 ))
  }) # renderText prediction
  
  output$recs <- renderPrint({
    cat("Actions likely to lead to 'Normal_Weight' classification by the model:", "\n")
    if (as.character( last(predict(RandomForestPredictor, newdata = rbind(newdata_d, data.frame(
      age = age(),
      family_history = history(),
      high_calorie = high_cal(),
      veggies = veggies(),
      eating_between_meals = factor(eat_between(), levels = freq_levels),
      water = water(),
      physical_activity = phys(),
      alcohol = factor(alcohol(), levels = freq_levels))
    )))) == "Normal_Weight") {
      cat("Your'e already at the 'Normal_Weight classification. No action necessary!")
    }
    else {
      writeLines(sprintf(extract_counterfactuals(newdata = data.frame(
        age = as.double(age()),
        family_history = as.character(history()),
        high_calorie = as.character(high_cal()),
        veggies = as.double(veggies()),
        eating_between_meals = factor(eat_between(), levels = freq_levels),
        water = as.double(water()),
        physical_activity = as.double(phys()),
        alcohol = factor(alcohol(), levels = freq_levels)))))
    }
  })
  
  output$graphics <- renderPlot({
    
    if (as.character( last(predict(RandomForestPredictor, newdata = rbind(newdata_d, data.frame(
      age = age(),
      family_history = history(),
      high_calorie = high_cal(),
      veggies = veggies(),
      eating_between_meals = factor(eat_between(), levels = freq_levels),
      water = water(),
      physical_activity = phys(),
      alcohol = factor(alcohol(), levels = freq_levels))
    )))) == "Normal_Weight") {
      varImpPlot(RandomForestPredictor, type=1)
    }
    else {
      extract_vars_plot(newdata = data.frame(
        age = as.double(age()),
        family_history = as.character(history()),
        high_calorie = as.character(high_cal()),
        veggies = as.double(veggies()),
        eating_between_meals = factor(eat_between(), levels = freq_levels),
        water = as.double(water()),
        physical_activity = as.double(phys()),
        alcohol = factor(alcohol(), levels = freq_levels)))
    }

  }) # renderPlot
  
  output$bmiclass <- renderTable({
    df_bmiclass
    
  })
  
  output$accuracy <- renderText({
    "This graph displays the features which most contributed to your classification, in order of importance."
  }) # renderText description
}

# Run the application 
shinyApp(ui = ui, server = server)
