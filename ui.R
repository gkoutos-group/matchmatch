library(shiny)

ui <- fluidPage(
  titlePanel("MatchMatch"),
  
  span(
    textOutput("dataset_info"),
    style = "color:blue",
    align = 'right'
  ),
  
  tabsetPanel(
    type = "tabs",
    tabPanel("Main",
       sidebarLayout(
         sidebarPanel(
           helpText("Dataset file"),
           fileInput(
             "dataset",
             "Choose CSV/XLS(X) File",
             multiple = FALSE,
             accept = c("text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".xls",
                        ".xlsx")
           ),
           textInput("dataset_sheet", label = "If XLSX file, add sheet name:", value=""),
  
           helpText("Dataset types definition"),
           fileInput(
             "column_description",
             "Choose CSV/XLS(X) File",
             multiple = FALSE,
             accept = c("text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv",
                        ".xls",
                        ".xlsx")
           ),
           textInput("column_description_sheet", label = "If XLSX file, add sheet name:", value=""),
           checkboxInput("guess_column_types", "Guess column types", value = TRUE),
           span(textOutput("column_descrition_red"),
                style="color:red"),
           
           hr(),
           textInput("match_group", label = "Match groups defined on:", value="group"),
           span(textOutput("match_group_red"),
                style = "color:red"),
  
           textInput("match_distance", label = "Match distance:", value="mahalanobis"),
           
           textInput("match_method", label = "Match method:", value="nearest"),
           
           hr(),
           helpText("Advanced options"),
           numericInput(
             "ratio_numeric",
             label = "Ratio for numeric column identification",
             value = 0.1, min = 0, max = 1,
           ),
           numericInput(
             "ratio_factor",
             label = "Ratio for categorical column identification",
             value = 0.2, min = 0, max = 1
           )
         ),
         
         mainPanel(
           helpText("Data types specification"),
           tableOutput("table_column_description"),
           downloadButton("download_table_column_description", "Download"),
           
           helpText("Matching on these variables:"),
           verbatimTextOutput("variables_used"),
           
           helpText("The complete data has these number of samples on each group:"),
           textOutput("complete_set_table"),
           
           helpText("The matched data has these number of samples on each group:"),
           textOutput("complete_set_matched"),
           
           helpText("Categorical features matching performance"),
           tableOutput("table_categorical"),
           downloadButton("download_table_categorical", "Download"),
           
           helpText("Numerical features matching performance"),
           tableOutput("table_numerical"),
           downloadButton("download_table_numerical", "Download"),
           
           helpText("Download matched data"),
           downloadButton("download_matched_data", "Download")
         )
       )
    ),
    tabPanel("Help",
             HTML("Matching of data is made using MatchIt package. [1]<br/>
Matching method can be one of: nearest, optimal, full, genetic, cem, exact and subclass<br/>
Matching distance can be one of: glm, gam, rpart, randomforest, nnet, cbps, bart, mahalanobis<br/>
<br/><br/>
If you see no output, be sure that the variables being matched are not exclusively in one group!<br/>
A match with less than 10% standardised difference (stddiff) is usually considered good enough.<br/>
<br/>
References:<br/>
[1] Ho, D. E., Imai, K., King, G., & Stuart, E. A. (2011). MatchIt: Nonparametric Preprocessing for Parametric Causal Inference. Journal of Statistical Software, 42(8). doi: 10.18637/jss.v042.i08
<br/>
"))
  ),
  hr(),
  HTML('<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is private on <a href="https://github.com/gkoutos-group/tableone/">https://github.com/gkoutos-group/tableone/</a>. For details contact <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.')
)