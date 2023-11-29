library(shiny)

function(input, output, session) {
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
          <p>Note: A new prediction will be created each re-render
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
  
  output$cvf_conditional_output <- renderUI({
    selected_method <- input$cvf_method
    selected_option <- input$cvf_svm_radio
    
    if (selected_method == "svm") {
      if (selected_option == "avail") {
        return (
          p("selected avail")
        )
      } else if (selected_option == "conf") {
        return (
          p("selected conf")
        )
      } else {
        return (
          p("selected scatter")
        )
      }
    } else {
      # When the method is not "svm", do not render any textOutput
      NULL
    }
  })
  
}