library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(e1071)
library(caret)
library(Rtsne) # For t-SNE visualization
library(arrow)
library(readr)
library(caret)

# Run Canine vs Feline SVM before application loads
run_cvf_svm <- function() {
  print("Running SVM...")
  
  data<-read.csv("svm_output_filter.csv")
  
  # Factor species (CANINE OR FELINE)
  data$species <- as.factor(data$species)
  
  # Transform susceptibility test results to values instead of characters
  value_mapping <- c("S" = 5, "R" = 4, "I" = 3, "TF" = 2, "N/I" = 1, "None" = 0)
  
  columns_to_transform <- c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)
  
  for (col in columns_to_transform) {
    data[[col]] <- ifelse(data[[col]] %in% names(value_mapping),
                          value_mapping[data[[col]]],
                          data[[col]])
  }
  
  data[columns_to_transform] <- lapply(data[columns_to_transform], as.numeric)
  
  # 80/20 split
  # Randomly select 6767 training data points
  # Randomly select 1692 testing data points
  random.training.data<-sample(seq(1, dim(data)[1]), size=6767, replace=FALSE)
  random.testing.data<-sample(seq(1, dim(data[-random.training.data])[1]), size=1692, replace=FALSE)

  # Select the columns that we need (species + drugs up to T2 since anything after that are just none values)
  svm.train.data<-data[random.training.data, c(7, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)]
  svm.testing.data<-data[random.testing.data, c(7, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)]
  
  # Train the SVM
  svm1<-svm(factor(species)~., data=svm.train.data, kernel="linear", cost=1, scale=FALSE, type="C")
  
  # Generate predictions
  predictions <- predict(svm1, svm.testing.data)
  
  # Generate confusion matrix based on prediction results
  conf_matrix <- confusionMatrix(predictions, svm.testing.data$species)

  # Add predictions into original testing data
  svm.testing.data$predicted_species <- predictions
  
  # Extracting features (excluding the 'species' and 'predicted_species' columns)
  features <- select(svm.testing.data, -c(species, predicted_species))
  
  # Perform t-SNE for dimensionality reduction
  tsne_result <- Rtsne(as.matrix(features), dims = 2, perplexity = 30, verbose = TRUE, check_duplicates = FALSE)
  
  # Combine the t-SNE results with species and predicted species labels
  tsne_df <- as.data.frame(tsne_result$Y)
  tsne_df$species <- svm.testing.data$species
  tsne_df$predicted_species <- svm.testing.data$predicted_species

  print("SVM completed!")
  
  results <- list(
    confusion_matrix = conf_matrix,
    tsne_data = tsne_df
  )
  
  return(results)
}

add_regions <- function(dataframe) {
  dataframe$region = NA
  for (i in 1:nrow(dataframe)) {
    dataframe$region[i] = switch(
      dataframe$county[i],
      "New York" = "Urban",
      "Nassau" = "Urban",
      "Suffolk" = "Urban",
      "Kings" = "Urban",
      "Westchester" = "Urban",
      "Albany" = "Rural",
      "Monroe" = "Rural",
      "Onondaga" = "Rural",
      "Erie" = "Rural",
      "Queens" = "Rural",
      "Richmond" = "Urban",
      "Saratoga" = "Rural",
      "Dutchess" = "Rural",
      "Orange" = "Rural",
      "Rockland" = "Rural",
      "Sullivan" = "Rural",
      "Oneida" = "Rural",
      "Schenectady" = "Rural",
      "Ulster" = "Rural",
      "Bronx" = "Urban",
      "Niagara" = "Rural",
      "Chemung" = "Rural",
      "Broome" = "Rural",
      "Rensselaer" = "Rural",
      "Putnam" = "Rural",
      "Montgomery" = "Rural",
      "Clinton" = "Rural",
      "Columbia" = "Rural",
      "Tompkins" = "Rural",
      "Wayne" = "Rural",
      "Chenango" = "Rural",
      "Oswego" = "Rural",
      "Orleans" = "Rural",
      "Steuben" = "Rural",
      "Otsego" = "Rural",
      "Chautauqua" = "Rural",
      "Ontario" = "Rural",
      "Cayuga" = "Rural",
      "Madison" = "Rural",
      "Schoharie" = "Rural",
      "Livingston" = "Rural",
      "Saint Lawrence" = "Rural",
      "Warren" = "Rural",
      "Essex" = "Rural",
      "Genesee" = "Rural",
      "Herkimer" = "Rural",
      "Greene" = "Rural",
      "Seneca" = "Rural",
      "Washington" = "Rural",
      "Fulton" = "Rural",
      "Tioga" = "Rural",
      "Delaware" = "Rural",
      "Yates" = "Rural",
      "Allegany" = "Rural",
      "Jefferson" = "Rural",
      "Wyoming" = "Rural",
      "Schuyler" = "Rural",
      "Franklin" = "Rural",
      "Cattaraugus" = "Rural",
      "Lewis" = "Rural"
    )
  }
  
  value_mapping <- c("S" = 5, "R" = 4, "I" = 3, "TF" = 2, "N/I" = 1, "None" = 0)
  
  columns_to_transform <- c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45)
  
  for (col in columns_to_transform) {
    dataframe[[col]] <- ifelse(dataframe[[col]] %in% names(value_mapping),
                            value_mapping[dataframe[[col]]],
                            dataframe[[col]])
  }
  
  dataframe[columns_to_transform] <- lapply(dataframe[columns_to_transform], as.numeric)
  
  return(dataframe)
}

# Run Urban vs Rural Logistic Regression before application loads
run_region_lr <- function() {
  print("Running Logistic Regression...")
  amr_df <- read_parquet("amr_data.parquet")
  
  # Add urban or rural region to region column
  amr_df <- add_regions(amr_df)
  amr_df$region <- as.factor(amr_df$region)
  
  # Split data into training and testing sets with an 80/20 proportion
  set.seed(531)
  split <- initial_split(amr_df, prop = 0.8, strata = NULL)
  
  train <- split %>% 
    training()
  test <- split %>% 
    testing()
  
  # Train the model
  model <- glm(region ~ species + R1 + H1 + W1 + N5 + N3 + I2 + E1 + O1 + N4 + N6 + N2 + B1 + Z1 + W2 + Z2 + F1 + M2 + T1 + K1 + S1 + D3 + B2 + L1 + W4 + H3 + H4 + X1, family = binomial, data = train)
  
  # Make predictions
  predicted_probabilities <- predict(model, newdata = test, type="response")
  Predictions <- ifelse(predicted_probabilities > 0.5, "Urban", "Rural")
  
  # Generate confusion matrix based on prediction results
  conf_matrix <- table(Predictions, test$region)
  confusionMatrix(conf_matrix)
  
  print("Logistic Regression Completed!")
  result <- confusionMatrix(conf_matrix)
  return(result)
}

function(input, output, session) {
  # CANINE VS FELINE CSV FILE LOADING
  cvf_svm_data <- read.csv("svm_urine.csv")
  
  # URBAN VS RURAL PARQUET FILE LOADING
  region_lr_data <- read_parquet("amr_data.parquet")
  region_lr_data <- add_regions(region_lr_data)
  region_lr_data <- region_lr_data %>% select(id, state, county, region, everything())
  region_lr_data <- region_lr_data %>% select(-Z4, -U1, -Z3, -W5, -J1, -O3, -V1, -I1, -G1, -A1, -R3, -"FUSIDIC ACID", -B3, -D4, -H2, -CEFOPERAZONE, -N1, -C1, -R2, -P1, -D2, -W3, -Q1, -H5, -M1, -Y1)

  # Reactive value to track if preloading is complete
  preloading_done <- reactiveVal(FALSE)
  
  # Canine vs Feline SVM confusion matrix and scatter plot results
  cvf_svm_confusion_matrix <- reactiveVal(NULL)
  cvf_svm_tsne_data<- reactiveVal(NULL)
  
  # Urban vs Rural Logistic Regression confusion matrix
  region_lr_confusion_matrix <- reactiveVal(NULL)
  
  # When the applications starts, execute any preload functions
  observe({
    results <- run_cvf_svm()
    cvf_svm_confusion_matrix(results$confusion_matrix)
    cvf_svm_tsne_data(results$tsne_data)
    
    # Urban vs Rural logistic regression
    region_lr_confusion_matrix(run_region_lr())
    
    # Set preloading_done to TRUE when finished all preloading
    preloading_done(TRUE)
  })
  
  # Loading indicator
  # NOTE: LOADING TEXT DOES NOT SHOW UP
  output$loading_indicator <- renderText({
    if (!preloading_done()) {
      return("Loading...")
    } else {
      return(NULL)
    }
  })
  
  # Canine vs Feline
  output$cvf_method_output <- renderUI({
    selected_method <- input$cvf_method
    
    if (selected_method == "svm") {
      return (
        div(
          HTML('
          <p><b>Support Vector Machine (SVM)</b></p>
          <p>A class of supervised machine-learning algorithms used to aid in analyzing and making predictions based on the dataset.</p>
          <p>As one of the most commonly used algorithms in AMR prediction, SVMs can be trained with hopes to create a model that can provide us information on if a clear distinction between canine and feline bladder infections exist.</p>
          <p>In this case, the SVM was used to attempt to classify a bladder infection as canine or feline based on the susceptibility test results it was trained on.</p>
          <p>Note: A new prediction will be created on reload of the application
          '),
          hr(),
          radioButtons("cvf_svm_radio", NULL, c("Available Data" = "avail", "Confusion Matrix" = "conf", "Scatter Plot" = "scatter")),
        )
      )
    } else {
      return (
        HTML('
          <p><b>Logistic Regression</b></p>   
        ')
      )
    }
  })
  
  # Canine vs Feline SVM radio button
  observeEvent(input$cvf_svm_radio, {
    selected_option <- input$cvf_svm_radio
    
    print(paste("Selected option:", selected_option))
  })
  
  # Canine vs Feline main panel output
  output$cvf_conditional_output <- renderUI({
    selected_method <- input$cvf_method
    selected_option <- input$cvf_svm_radio
    
    if (selected_method == "svm") {
      if (selected_option == "avail") {
        return (
          list(
            HTML('
              <h4><b>Available Data</b></h4>
              <hr/>
              <p>The original dataset consisted of 111,969 infections identified in canines and felines.</p>
              <p>In order to answer the research question, the dataset was filtered down to include only those infections identified from urine samples:</p>
              <ul>
                <li>Total cases: <b>8459</b></li>
                <li>Canine cases: <b>5836</b></li>
                <li>Feline cases: <b>2623</b></li>
              </ul>
              <p>Susceptibility test results for the various drugs were also converted to a numerical value before using the SVM model:</p>
              <ul>
                <li>S (Susceptible) = 5</li>
                <li>R (Resistant) = 4</li>
                <li>I (Intermediate) = 3</li>
                <li>TF (To Follow) = 2</li>
                <li>N/I (Not interpretable) = 1</li>
                <li>None (Empty) = 0</li>
              </ul>
              <p>When dividing the data into training data and testing data, an 80%/20% split was used. In this case, 6767 random cases were used to train the model and the 1692 remaining cases were used to test the model. Each new run of the SVM model will randomly select a different set of training/testing data.</p>
              <p>After filtering of the original dataset, this is a small example of how the available data for the SVM looks like</p>
              <p>Note: The drugs only include R1 - T2, as any drug after those have no test results in any cases</p>
            '),
            dataTableOutput("cvf_svm_data_table")
          )
        )
      } else if (selected_option == "conf") {
        return (
          list(
            HTML('
              <h4><b>Confusion Matrix and Statistics</b></h4>
              <hr/>
            '),
            verbatimTextOutput("cvf_svm_confusion_matrix_output"),
            HTML('
              <p>Shown above is the generated confusion matrix and statistics from the trained SVM model on this particular run.</p>
              <p>As the training and testing data is randomly selected each run, the values will almost never be the same on consecutive runs.</p>
              <p>To determine the total number of CANINE and FELINE within the testing data that was used, add up the values in their respective columns.</p>
              <p>Here is some insight on how the interpret these values:</p>
              <p><b>Confusion Matrix: </b>The two rows represent the CANINE/FELINE predictions from the trained SVM model. The two columns represent the actual number of CANINE/FELINE in the testing data. For example, the top-left value shows the amount of times the model predicted CANINE and the actual testing data was CANINE. On the other hand, for the top-right value, it shows the amount of times the model predicted CANINE but the actual testing data was FELINE. If the row and column name are the same (e.g. CANINE-CANINE or FELINE-FELINE), the value in that cell indicates how many predictions the model got correct.</p>
              <p><b>Accuracy: </b>This value represents the overall accuracy of the model in correctly predicting CANINE or FELINE</p>
              <p><b>Kappa: </b>Measures the agreement between classification and truth values. A value closer to 1 indicates good agreement, while a value closer to 0 indicates no agreement.</p>
              <p><b>Sensitivity: </b>The proportion of actual positives (canines) that were correctly identified by the model</p>
              <p><b>Specificity: </b>The proportion of actual negatives (felines) that were correctly identified by the model</p>
              <p><b>Prevalence: </b>The prevalence/proportion of CANINE within the testing data</p>
              <p><b>Balanced Accuracy: </b>The determined average of sensitivity and specificity. May be useful for imbalanced datasets</p>
              <br/>
              <p><b>Confusion Matrix and Statistics Conclusion</b></p>
              <p>After multiple runs of training and testing the SVM model, we have come to the same conclusion every time when analyzing the confusion matrix and statistics.</p>
              <p>The model always results in high sensitivity and very low specificity, indicating it performs well in identifying canine cases but poorly in identifying feline cases. This could be due to a fairly large amount of canine infections compared to feline infections within the original dataset. This may result in a higher prevalence of canines within the training data, resulting in more canine predictions within the testing data.</p>
              <p>Overall, based on the results from the confusion matrix and generated statistics, there might be a slight indication of differences between canines and felines while looking at their susceptibility test results. However, due to a few possible limitations (e.g., proportion of canines vs felines, insufficient features, etc), there can be no definitive conclusion based solely on the antimicrobial resistance patterns.</p>
            ')
          )
        )
      } else {
        return (
          list(
            HTML('
              <h4><b>Scatter Plot</b></h4>
              <hr/>   
            '),
            plotOutput("cvf_svm_tsne_plot"),
            HTML('
              <p>Shown above is a scatter plot of the predicted species from the SVM model with the actual species from the testing data.</p>
              <p>As the original data used for training and testing included 1 dependent variable (species) and various predictor variables (the antibiotics used), this resulted in difficult to normally use a scatter plot with a set X and Y axis. Therefore, the t-SNE method had to be applied, which is a non-linear dimensionality reduction technique, allowing the high-dimensional data to be reduced into just 2 dimensions for visualization.</p>
              <p>Here is how the above scatter plot can be interpreted:</p>
              <p><b>Shapes: </b>The shapes represent the predicted species from the SVM model. Circle represents a canine prediction, while a triangle represents a feline prediction</p>
              <p><b>Colour: </b>The colour of the shapes represent the actual species, where blue represents canine and red represents feline. For example, for one point that is a red circle, it means that the model predicted a canine but the actual species was a feline.</p>
              <br/>
              <p><b>Scatter Plot Conclusion</b></p>
              <p>When observing the graph after multiple iterations of training and testing, we can always see numerous groups of points, scattered around the graph.</p>
              <p>Within all these small clusters, they include both canine and feline species. As the dimensionality has been reduced using t-SNE, there can be no direct interpretation of what these groups of clusters and their location on the graph could mean. However, a possibility for a cluster of points could indicate similar antimicrobial resistance patterns based on their susceptibility test results. As these clusters include both canine and feline species, it could represent similarities in AMR patterns in canines and felines.</p>
              <p>In conclusion, based on the results of both the confusion matrix and scatter plot, there is <b>no clear distinction</b> between antimicrobial resistance patterns found in canine and feline bladder infections within New York.</p>
            ')
          )
        )
      }
    } else {
      # When the method is not "svm", do not render any textOutput
      NULL
    }
  })
  
  # Render the Canine vs Feline SVM confusion matrix
  output$cvf_svm_confusion_matrix_output <- renderPrint({
    # Get the confusion matrix from reactive value
    confusion_matrix <- cvf_svm_confusion_matrix()
    if (!is.null(confusion_matrix)) {
      print(confusion_matrix)
    } else {
      "Confusion matrix not available yet."
    }
  })
  
  # Render the Canine vs Feline SVM TSNE plot
  output$cvf_svm_tsne_plot <- renderPlot({
    cvf_svm_tsne_data_df <- cvf_svm_tsne_data()
    
    ggplot(cvf_svm_tsne_data_df, aes(x = V1, y = V2, color = species, shape = predicted_species)) +
      geom_point(size = 3) +
      labs(x = "Dimension 1", y = "Dimension 2", color = "Actual Species", shape = "Predicted Species") +
      scale_color_manual(values = c("CANINE" = "blue", "FELINE" = "red")) +
      scale_shape_manual(values = c("CANINE" = 1, "FELINE" = 2)) +
      theme_minimal()
  })
  
  # Render the SVM available data table within the conditional output
  output$cvf_svm_data_table <- renderDataTable({
    cvf_svm_data[1:50, ]
  }, options = list(
    scrollX = TRUE
  ))
  
  # Urban vs Rural
  output$region_method_output <- renderUI({
    selected_method <- input$region_method
    
    if (selected_method == "lr") {
      return (
        div(
          HTML('
          <p><b>Logistic Regression</b></p>
          <p>A class of supervised machine-learning algorithms used to predict categorical dependent variables given predictor variables.</p>
          <p>Logistic regression models are common in AMR problems, they can be trained to provide us information on if there is a distinction between AMR patterns between urban and rural regions.</p>
          <p>In this case, the logistic regression model was used to attempt to classify whether antimicrobial resistant bacteria found in canines and felines differ between urban and rural regions based on susceptibility test results the model was trained on.</p>
          '),
          hr(),
          radioButtons("region_lr_radio", NULL, c("Available Data" = "avail", "Confusion Matrix" = "conf")),
        )
      )
    } else {
      return (
        HTML('
          <p><b>K-Means Clustering</b></p>   
        ')
      )
    }
  })
  
  # Urban vs Rural logistic regression radio button
  observeEvent(input$region_lr_radio, {
    selected_option <- input$region_lr_radio
    
    print(paste("Selected option:", selected_option))
  })
  
  output$region_conditional_output <- renderUI({
    selected_method <- input$region_method
    selected_option <- input$region_lr_radio
    
    if (selected_method == "lr") {
      if (selected_option == "avail") {
        print(region_lr_confusion_matrix())
        return (
          list(
            HTML('
              <h4><b>Available Data</b></h4>
              <hr/>
              <p>The original dataset consisted of 111,969 infections identified in canines and felines.</p>
              <p>There was some preliminary data analysis that had to be conducted to the existing dataset to identify whether a specific county was in a rural or urban region.</p>
              <p>To indentify whether a county was in a rural or urban region, we referenced the following:</p>
              <p><i>New York State Rural Counties and Towns - New York State Department of ... New York State Department of Health. (2021, August). <a href="https://www.health.ny.gov/funding/soi/20283/docs/attachment7.pdf">https://www.health.ny.gov/funding/soi/20283/docs/attachment7.pdf</a></i></p>
              <p>A new feature was added to the dataset - "region" - which identifies whether the county was in a rural or urban region.</p>
              <p>Susceptibility test results for the various drugs were also converted to a numerical value before using the logistic regression model:</p>
              <ul>
                <li>S (Susceptible) = 5</li>
                <li>R (Resistant) = 4</li>
                <li>I (Intermediate) = 3</li>
                <li>TF (To Follow) = 2</li>
                <li>N/I (Not interpretable) = 1</li>
                <li>None (Empty) = blank</li>
              </ul>
              <p>When dividing the data into training data and testing data, an 80%/20% split was used.</p>
              <p>After including the dataset to add the "region" feature, this is a small example of how the available data for the logistic regression model looks like.</p>
              <p>Note: The drugs only include R1 - T2, as any drug after those have no test results in any cases</p>
            '),
            dataTableOutput("region_lr_data_table")
          )
        )
      } else if (selected_option == "conf") {
        return (
          list(
            HTML('
              <h4><b>Confusion Matrix and Statistics</b></h4>
              <hr/>
            '),
            verbatimTextOutput("region_lr_confusion_matrix_output"),
            HTML('
              <p>Shown above is the generated confusion matrix and statistics from the trained logistic regression model on this particular run.</p>
              <p>As the training and testing data is randomly selected each run, the values will almost never be the same on consecutive runs.</p>
              <p>Here is some insight on how the interpret these values:</p>
              <p><b>Confusion Matrix: </b>The two rows represent the Urban/Rural predictions from the trained logistic regression model. The two columns represent the actual number of Urban/Rural regions in the testing data. For example, the top-left value shows the amount of times the model predicted rural and the actual testing data was rural. On the other hand, for the top-right value, it shows the amount of times the model predicted urban but the actual testing data was urban. If the row and column name are the same (e.g. Urban-Urban or Rural-Rural), the value in that cell indicates how many predictions the model got correct.</p>
              <p><b>Accuracy: </b>This value represents the overall accuracy of the model in correctly predicting whether the county is in an Urban or Rural region based on the susceptibility tests.</p>
              <p><b>Kappa: </b>Measures the agreement between classification and truth values. A value closer to 1 indicates good agreement, while a value closer to 0 indicates no agreement.</p>
              <p><b>Sensitivity: </b>The proportion of actual positives (Rural) correctly predicted.</p>
              <p><b>Specificity: </b>The proportion of actual negatives (Urban) correctly predicted.</p>
              <p><b>Prevalence: </b>The proportion of actual positives (Rural) in the dataset. </p>
              <p><b>Balanced Accuracy: </b>The determined average of sensitivity and specificity. May be useful for imbalanced datasets</p>
              <br/>
              <p><b>Confusion Matrix and Statistics Conclusion</b></p>
              <p>After multiple runs of training and testing the logistic regression model, we have come to the same conclusion every time when analyzing the confusion matrix and statistics.</p>
              <p>The overall accuracy is around chance level, and it is difficult to conclude whether the types of antibiotic-resistant bacteria found in canines and felines differ between urban and rural regions.</p>
              <p>However, the high sensitivity of the logistic regression model meants that it is relatively good at correctly identifying instances from rural regions when given susceptibility tests from rural regions. But, the low specificity for the urban class suggests that the model struggles to correctly identify instances from urban regions.</p>
            ')
          )
        )
      } 
    } else {
      NULL
    }
  })
  
  # Render the Urban vs Rural logistic regression confusion matrix
  output$region_lr_confusion_matrix_output<- renderPrint({
    # Get the confusion matrix from reactive value
    confusion_matrix <- region_lr_confusion_matrix()
    if (!is.null(confusion_matrix)) {
      print(confusion_matrix)
    } else {
      "Confusion matrix not available yet."
    }
  })
  
  # Render the logistic regression available data table within the conditional output
  output$region_lr_data_table <- renderDataTable({
    region_lr_data[1:50, ]
  }, options = list(
    scrollX = TRUE,
    pageLength = 5
    
  ))
}
