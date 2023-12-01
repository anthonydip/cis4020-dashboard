library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(e1071)
library(reshape2)
library(rsample)
library(caret)
library(Rtsne) # For t-SNE visualization
library(arrow)
library(readr)
library(caret)

# Run Canine vs Feline SVM
run_cvf_svm <- function() {
  print("Running Canine vs Feline SVM...")
  
  data<-read.csv("cvf_output_filter.csv")
  
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

  print("Canine vs Feline SVM completed!")
  
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

# Run Canine vs Feline Logistic Regression
run_cvf_lr <- function() {
  print("Running Canine vs Feline LR...")
  data<-read.csv("cvf_output_filter.csv")
  
  columns_to_exclude <- c("id", "state", "order_month", "county", "zip_3_level", "order_year", "panel_name", "assay_name", "remark", "site", "source", "org_standard", "age_year")
  
  data <- data[, !colnames(data) %in% columns_to_exclude]
  
  drugs_to_exclude <- 33:100
  
  data <- data[, -drugs_to_exclude]
  
  # Transform susceptibility test results to values instead of characters
  value_mapping <- c("S" = 5, "R" = 4, "I" = 3, "TF" = 2, "N/I" = 1, "None" = 0)
  
  columns_to_transform <- c(2:32)
  
  for (col in columns_to_transform) {
    data[[col]] <- ifelse(data[[col]] %in% names(value_mapping),
                          value_mapping[data[[col]]],
                          data[[col]])
  }
  
  data[columns_to_transform] <- lapply(data[columns_to_transform], as.numeric)
  
  drug_columns <- colnames(data)[2:32]
  
  # Factor species (CANINE OR FELINE)
  data$species <- as.factor(data$species)
  
  # Factor drug columns
  data[, drug_columns] <- lapply(data[, drug_columns], factor)
  
  #str(data$species)
  
  #predictor_formula <- as.formula(paste("species ~", paste(drug_columns, collapse = " + ")))
  
  #logistic_model_combined <- glm(predictor_formula, data = training_data, family = "binomial")
  
  # use stepwise regression to explore "better" models
  #step.aic<-step(logistic_model_combined)
  
  final_model <- glm(species~R1 + W1 + Z1 + S1 + H3 + X1, data = data, family = "binomial")
  summary(final_model)
  
  # determine predictions based on the final model
  pred<-predict(final_model, type="response")
  
  # View the distribution of predictions
  #hist(pred)
  
  # Convert predicted probabilities to class predictions (assigning to the most probable class)
  predicted_classes <- ifelse(pred > 0.5, "FELINE", "CANINE")  # Threshold 0.5: if > 0.5, predict "FELINE", otherwise "CANINE"
  
  # Extract actual species from the dataset
  actual_species <- as.character(data$species)
  
  # Generate the confusion matrix
  conf_matrix <- table(predicted_classes, actual_species)
  
  print("Canine vs Feline LR Completed!")
  
  results <- list(
    predictions = pred,
    confusion_matrix = conf_matrix
  )
  
  return(results)
}

function(input, output, session) {
  # CANINE VS FELINE CSV FILE LOADING
  cvf_svm_data <- read.csv("cvf_urine.csv")
  
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
  
  # Canine vs Feline LR prediction and confusion matrix results
  cvf_lr_confusion_matrix <- reactiveVal(NULL)
  cvf_lr_pred <- reactiveVal(NULL)
  
  # When the applications starts, execute any preload functions
  observe({
    cvf_svm_results <- run_cvf_svm()
    cvf_svm_confusion_matrix(cvf_svm_results$confusion_matrix)
    cvf_svm_tsne_data(cvf_svm_results$tsne_data)
    
    # Urban vs Rural logistic regression
    region_lr_confusion_matrix(run_region_lr())
    
    cvf_lr_results <- run_cvf_lr()
    cvf_lr_confusion_matrix(cvf_lr_results$confusion_matrix)
    cvf_lr_pred(cvf_lr_results$predictions)

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
        div(
          HTML('
            <p><b>Logistic Regression</b></p>
            <p>A statistical model used to observe the probability of a certain outcome, based on given multiple input variables.</p>
            <p>As logistic regression models are among one of the most commonly used algorithms in AMR prediction, we can use it in our case to model a binary outcome; whether a species is canine or feline based on the inputted susceptibility tests.</p>
          '),
          hr(),
          radioButtons("cvf_lr_radio", NULL, c("Available Data" = "avail", "Confusion Matrix" = "conf", "Histogram" = "hist"))
        )
      )
    }
  })
  
  # Canine vs Feline main panel output
  output$cvf_conditional_output <- renderUI({
    selected_method <- input$cvf_method
    selected_svm_option <- input$cvf_svm_radio
    selected_lr_option <- input$cvf_lr_radio
    
    if (selected_method == "svm") {
      if (selected_svm_option == "avail") {
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
      } else if (selected_svm_option == "conf") {
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
      # When the method is "Logistic Regression" for Canine vs Feline
      if (selected_lr_option == "avail") {
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
              <p>Susceptibility test results for the various drugs were also converted to a numerical value before using the Logistic Regression:</p>
              <ul>
                <li>S (Susceptible) = 5</li>
                <li>R (Resistant) = 4</li>
                <li>I (Intermediate) = 3</li>
                <li>TF (To Follow) = 2</li>
                <li>N/I (Not interpretable) = 1</li>
                <li>None (Empty) = 0</li>
              </ul>
              <p>After filtering of the original dataset, this is a small example of how the available data for the SVM looks like</p>
              <p>Note: The drugs only include R1 - T2, as any drug after those have no test results in any cases</p>
            '),
            dataTableOutput("cvf_svm_data_table")
          )
        )
      } else if (selected_lr_option == "conf") {
        conf_matrix <- cvf_lr_confusion_matrix()
        
        return (
          list(
            HTML('
              <h4><b>Confusion Matrix and Statistics</b></h4>
              <hr/>
            '),
            verbatimTextOutput("cvf_lr_confusion_matrix_output"),
            HTML('
              <p>Shown above is the generated confusion matrix from the <b>final</b> logistic regression model.</p>
              <p>Note: Stepwise regression was used to explore "better" models, and was concluded that the combination of R1, W1, Z1, S1, H3, X1 provided the "best" results in terms of balance and predictions. Therefore, the final logistic regression model only used these drugs in order to determine the probabilities of the outcome species.</p>
              <p>Here are a few calculated statistics to help us interpret what confusion matrix means:</p>
              <ul>
                <li>5825 cases were correctly predicted as CANINE (True Positives)</li>
                <li>13 cases were correctly predicted as FELINE (True Negatives)</li>
                <li>2610 cases were incorrectly predicted as CANINE when they were FELINE (False Positives)</li>
                <li>11 cases were incorrectly predicated as FELINE when they were CANINE (False Negatives)</li>
              </ul>
              <p><b>Accuracy: 69.02%: </b>The ratio of correct predictions of both CANINE and FELINE over the total number of observations</p>
              <p><b>Sensitivity: 99.81%: </b>The ratio of correct CANINE predictions over the total number of CANINE in the data</p>
              <p><b>Specificity: 00.50%: </b>The ratio of correct FELINE predictions over the total number of FELINE in the data</p>
              <br/>
              <p><b>Confusion Matrix and Statistics Conclusion</b></p>
              <p>Showing similar results to the SVM model, the results from logistic regression show a very high sensitivity and very low specificity. Again, indicating it is able to correctly classify almost all canine cases, but incorrectly classifies almost all feline cases.</p>
              <p>With a decent accuracy of approximately 69.02%, this however does not indicate a good logistic regression model, as it is not good at classifying feline species. </p>
              <p>The overall results are inconclusive, as many limitations with the model may exist. Limitations such as the large amount of canine infections compared to feline cases and the fact that we are only utilizing susceptibility test results may not be enough to distinctly classify the species.</p>
            ')
          )
        )
      } else {
        return (
          list(
            HTML('
              <h4><b>Histogram of Predictions</b></h4>
              <hr/>
            '),
            plotOutput("cvf_lr_hist_plot"),
            HTML('
              <p>The histogram shown above shows the probabilities of predictions from the logistic regression model.</p>
              <p>On the x-axis, it shows a range of probabilities from 0.0 to 1.0.</p>
              <p>On the y-axis, it shows a range of frequency from 0 to 5000. This indicates the number of occurences of that probability during the predictions.</p>
              <p>Here is a closer look on the probabilities of predictions for the first few observations in the dataset:</p>
            '),
            verbatimTextOutput("cvf_lr_pred_head_output"),
            HTML('
              <p>The first number <b>(0.2947158)</b> represents the predicted probability for the first observation in the data.</p>
              <p>The second number <b>(0.2817432)</b> represents the predicted probability for the first observation in the data.</p>
              <p>And so on for subsequent observations up to the total number of urine infections within the filtered dataset.</p>
              <p>Each probability indicates the confidence of the logistic regression model that the current observation is classified as FELINE.</p>
              <p>Based on the way the model is setup, a probability closer to 0.0 means it would be classified as a CANINE, while a probability closer to 1.0 means it would be classified as FELNIE</p>
              <br/>
              <p><b>Histogram Conclusion</b></p>
              <p>Based on the histogram above and observed probabilities, it is evident that the most frequent probability is within the range of 0.36, showing around 5000 occurrences. We can see that a significantly large amount of the probabilities occur under the 0.4 range, showing a large bias towards CANINE classification.</p>
              <p>From the results of the model, it appears that based on solely the susceptibility tests, the probability that our response variable (species in this case) takes on the value CANINE is more prevalent.</p>
              <p>In conclusion, this could indicate slight differences between the antimicrobial patterns between canines and felines, perhaps canines showing more distinct patterns. However, this could also be due to the existence of more canine infections within the dataset.</p>
            ')
          )
        )
      }
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
  
  # Render the text for the Canine vs Feline LR histogram predictions
  output$cvf_lr_pred_head_output <- renderPrint({
    # Get the predictions
    predictions <- cvf_lr_pred()
    if (!is.null(predictions)) {
      print(head(predictions))
    } else {
      "Histogram not available yet."
    }
  })
  
  # Render the Canine vs Feline LR confusion matrix
  output$cvf_lr_confusion_matrix_output <- renderPrint({
    # Get the confusion matrix from reactive value
    confusion_matrix <- cvf_lr_confusion_matrix()
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
  
  # Render the Canine vs Feline LR histogram
  output$cvf_lr_hist_plot <- renderPlot({
    predictions <- cvf_lr_pred()
    
    hist(predictions)
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
  
  
  # Most Common Bacteria
  output$cb_method_output <- renderUI({
    return (
      div(
        HTML('
        <p><b>Descriptive Statistics</b></p>
        <p>A branch of statistics that includes the collection and analysis of data.</p>
        <p>One of the most common and powerful statistical methods for summarizing and analyzing large datasets, offering various approaches to answering the question.
        <p>Descriptive statistics includes measures of central tendency, measures of dispersion, frequency distributions, etc.</p>
        <p>For this question, utilizing frequency distributions can be used to accurately analyze the data. This approach provides powerful insights into the bacteria most commonly identified. Furthermore, by splitting the data by variables such as species, frequency distributions can be analyzed on each subset, therefore identifying common bacteria associated with those variables.
        '),
        hr(),
        radioButtons("cb_ds_radio", NULL, c("All" = "All", "Canine" = "Canine", "Feline" = "Feline")),
      )
    )
  })
  
  # Common Bacteria Descriptive Statistics Radio
  output$cb_conditional_output <- renderUI({
    selected_option <- input$cb_ds_radio
    
    if (selected_option == "All") {
      return (
        div(
          plotOutput("all_species_plot", width = "100%", height = "600px"),
          uiOutput("cb_ds_description")
        )
      )
    } else if (selected_option == "Canine") {
      return (
        div(
          plotOutput("canine_plot", width = "100%", height = "600px"),
          uiOutput("cb_ds_description")
        )
      )
    } else if (selected_option == "Feline") {
      return (
        div(
          plotOutput("feline_plot", width = "100%", height = "600px"),
          uiOutput("cb_ds_description")
        )
      )
    }
    else {
      return (
        p("")
      )
    }
  })
  
  # Common Bacteria Canine + Feline Plot
  output$all_species_plot <- renderPlot({
    data <- data.frame(
      Bacteria = c("E COLI", "STAPHYLOCOCCUS PSEUDINTERMEDIUS", "ENTEROCOCCUS FAECALIS", "PROTEUS MIRABILIS", "STAPHYLOCOCCUS SCHLEIFERI", "PSEUDOMONAS AERUGINOSA", "BETAHAEMOLYTIC STREPTOCOCCUS", "KLEBSIELLA SP", "STAPHYLOCOCCUS FELIS", "DROP", "CORYNEBACTERIUM SP", "STAPHYLOCOCCUS AUREUS", "ENTEROCOCCUS FAECIUM", "ENTEROCOCCUS SP", "KLEBSIELLA PNEUMONIAE"),
      Freq = c(29142, 20586, 10581, 9674, 5935, 5633, 2473, 2202, 1841, 1717, 1677, 1594, 1587, 983, 917)
    )
    
    data$Bacteria <- factor(data$Bacteria, levels=unique(data$Bacteria))
    data_sorted <- data[order(-data$Freq),]
    data_sorted$Bacteria <- factor(data_sorted$Bacteria, levels=data_sorted$Bacteria)
    
    ggplot(data_sorted, aes(x=Bacteria, y=Freq, fill=Bacteria)) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(title="Canine and Feline Bacterial Species Frequency", x="Bacterial Species", y="Frequency")
  })
  
  # Common Bacteria Canine Plot
  output$canine_plot <- renderPlot({
    data <- data.frame(
      Bacteria = org_standard_column <- c("E COLI", "STAPHYLOCOCCUS PSEUDINTERMEDIUS", "PROTEUS MIRABILIS", "ENTEROCOCCUS FAECALIS", "STAPHYLOCOCCUS SCHLEIFERI", "PSEUDOMONAS AERUGINOSA", "BETAHAEMOLYTIC STREPTOCOCCUS", "KLEBSIELLA SP", "CORYNEBACTERIUM SP", "DROP", "ENTEROCOCCUS FAECIUM", "STAPHYLOCOCCUS AUREUS", "ENTEROCOCCUS SP", "KLEBSIELLA PNEUMONIAE", "BACILLUS SP"),
      Freq <- c(21859, 20013, 9263, 7372, 5762, 4985, 2337, 1957, 1559, 1513, 1125, 886, 824, 787, 712)
    )
    
    data$Bacteria <- factor(data$Bacteria, levels=unique(data$Bacteria))
    data_sorted <- data[order(-data$Freq),]
    data_sorted$Bacteria <- factor(data_sorted$Bacteria, levels=data_sorted$Bacteria)
    
    ggplot(data_sorted, aes(x=Bacteria, y=Freq, fill=Bacteria)) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(title="Canine Bacterial Species Frequency", x="Bacterial Species", y="Frequency")
  })
  
  # Common Bacteria Feline Plot
  output$feline_plot <- renderPlot({
    data <- data.frame(
      Bacteria = org_standard_column <- c("E COLI", "ENTEROCOCCUS FAECALIS", "STAPHYLOCOCCUS FELIS", "STAPHYLOCOCCUS AUREUS", "PSEUDOMONAS AERUGINOSA", "STAPHYLOCOCCUS PSEUDINTERMEDIUS", "ENTEROCOCCUS FAECIUM", "PROTEUS MIRABILIS", "KLEBSIELLA SP", "PASTEURELLA SP", "DROP", "STAPHYLOCOCCUS SCHLEIFERI", "ENTEROCOCCUS SP", "ENTEROBACTER CLOACAE", "STAPHYLOCOCCUS EPIDERMIDIS"),
      Freq <- c(7283, 3209, 1805, 708, 648, 573, 462, 411, 245, 227, 204, 173, 159, 158, 155)
    )
    
    data$Bacteria <- factor(data$Bacteria, levels=unique(data$Bacteria))
    data_sorted <- data[order(-data$Freq),]
    data_sorted$Bacteria <- factor(data_sorted$Bacteria, levels=data_sorted$Bacteria)
    
    ggplot(data_sorted, aes(x=Bacteria, y=Freq, fill=Bacteria)) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      labs(title="Feline Bacterial Species Frequency", x="Bacterial Species", y="Frequency")
  })
  
  # Common Bacteria Description
  output$cb_ds_description <- renderUI({
    return(
      HTML('
           <div style="margin-left:5px">
             <br/>
             <b>General Description</b>
             <p>
                The bar graphs represent the commonly identified bacteria for the different species (all/canine/feline) based on the data.
                <br/>
                To change the graphs, please choose one of the options (all/canine/feline) to render the respective graph.
             </p>
             <br/>
             <b>Interpretation</b>
             <p>
                Interpretation of the bar graph is quite simple. Each bar denotes a different species of identified bacteria while the height of the bar corresponds to the occurrence of that species 
                <br/>
                in the data. The graph is ordered from the most occurring bacteria to the least occurring, allowing for easy visualization of the most common bacteria among the canine and feline species.
                <br/>
                Each bar has a corresponding colour, which can be matched with the legend on the right for the identified species of bacteria.
                <br/>
                For example, if we interpret the bar graph for both canine and feline represented by "All", we can see that E.Coli is the most prevalent bacteria, followed by Staphylococcus P.
             </p>
             <br/>
             <b>Conclusion and Results</b>
             <p>
                <b>Note:</b> It is important to note that only the 15 most frequent bacteria were displayed as the remaining bacteria were less relevant and we can still reach a 
                <br/>
                clear conclusion for the question.
                <br/>
                <br/>
                Bacteria that is more common can be exposed to higher amounts of antibiotics as a result of being treated more often. Thus, it is important to not only focus on 
                <br/>
                antimicrobial resistances, but analyze the most common bacteria. Understanding the most common bacteria can aid researchers and scientists in predicting bacteria that are 
                <br/>
                more susceptible to developing resistances.
                <br/>
                <br/>
                Reflecting on the results from the bar graph, it is apparent for both species that E.Coli is the most common bacteria, followed by Staphylococcus P., then Enterococcus F.
                <br/>
                As mentioned, bacteria that is more common can be exposed to higher amounts of antibiotics, increasing the likelihood of those bacteria developing antimicrobial resistances.
                <br/>
                Since E.Coli is highly prevalent according to the data, it signifies the greater chances of developing antimicrobial resistances, especially if treatment is sought after.
                <br/>
                This concept can be applied to the different commonly identified bacteria, providing researchers and scientist insights into which bacteria is more likely to develop antimicrobial resistances.
                <br/>
                Furthermore, looking at the individual species, they seem to have differing common bacteria. Although E.Coli is the most common bacteria for both canine and feline,
                <br/>
                their following most common species are different. Canines second most common bacteria is Staphylococcus P., while felines second most common bacteria is Enterococcus F. 
                <br/>
                This suggests the animal species has an influence on the most common types of bacteria caught.
                <br/>
                <br/>
                Descriptive statistics proved to be a strong approach when addressing the underlying question of the most common bacteria. The method accurately summarized the data into
                <br/>
                an easily interpretable form to answer the question. The bar graphs were created using frequency methods from the data, as such the most commonly identified bacteria in the 
                <br/>
                dataset will be towards the left.
                <br/>
                <br/>
             </p>
           </div>
           ')
    )
  })
}
