library(shiny)
library(markdown)

navbarPage("Visualizing AMR",
  tabPanel("Home",
    fluidPage(
      HTML('
        <div class="container">
          <h2>Antimicrobial Resistance (?)</h2>
          <hr/>
          <h4><b>Authors</b></h4>
          <p>Anthony Dip, Branden Phan, Michael Lam</p>
          <br/>
          <h4><b>The Problem</b></h4>
          <p>As the implications of antimicrobial resistance found within bacteria are on the rise, it becomes increasingly difficult and expensive to correctly treat various infections.</p>
          <p>Due to antimicrobial susceptibility testing becoming more costly, treatment choices often rely on past success, recommendations from senior veterinarians, and antimicrobial stewardship guidelines.</p>
          <p>In order to combat the persistence of antimicrobial resistance, it is crucial that veterinarians utilize precise antimicrobial treatment decisions.</p>
          <p>We aim to provide veterinarians a tool based on previous susceptibility tests, allowing for data-driven decisions, potentially reducing the need for further testing.</p>
          <br/>
          <h4><b>Questions to Be Answered</b></h4>
          <ul>
            <li><b>Do antimicrobial resistance patterns differ between canine and feline bladder infections within New York?</b></li>
            <br/>
            <li><b>Do the types of antibiotic-resistant bacteria commonly found in canines and felines differ between urban and rural regions?</b></li>
            <br/>
            <li><b>What antimicrobial bacteria is most common in canines and felines within New York?</b></li>
          </ul>
          <br/>
          <p>As the large dataset utilized within this application is solely based on infections observed in New York state, this tool is directed towards veterinarians based in New York.</p>
          <br/>
          <h4><b>Literature Review</b></h4>
        </div>
      ')
    )
  ),
  tabPanel("Canine vs Feline",
           
  ),
  tabPanel("Urban vs Rural",
           
  ),
  tabPanel("Common Bacteria",
        
  )

)