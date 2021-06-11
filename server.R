
library(shiny)
library(readxl)

VERSION = "matchmatch_0.1"

if(!exists(".match_loaded", mode='function')) source(here::here('match.R'))

## infer column type - copied from describe_table.R (tableone)
infer_column_type <- function(df, c, ratio_factor, ratio_numeric) {
  n_op <- length(unique(df[[c]]))
  
  if(n_op <= nrow(df) * ratio_factor) {
    if(n_op == 2) {
      return("binary")
    } else {
      return("factor")
    }
  } else if(n_op == nrow(df)) {
    return("skip")
  } else if(n_op >= nrow(df) * ratio_numeric) {
    return("numeric")
  }
  return("skip")
}

server <- function(input, output, session) {
  ######################################## this code is copied and pasted on the tableone project
  load_dataset <- reactive({
    if (is.null(input$dataset$datapath)) {
      tdf <- read_excel("sample.xlsx", sheet="data")
    } else if(grepl(".xls$|.xlsx$", input$dataset$datapath)) {
      tdf <- read_excel(input$dataset$datapath, sheet=ifelse(input$dataset_sheet == "", 1, input$dataset_sheet))
    } else {
      tdf <- read.csv(input$dataset$datapath)
    }
    
    return(tdf)
  })
  
  # read the dtypes file
  load_dtypes <- reactive({
    if (is.null(input$column_description$datapath)) {
      tdf <- read_excel("sample.xlsx", sheet="description")
    } else if(grepl(".xls$|.xlsx$", input$column_description$datapath)) {
      tdf <- read_excel(input$column_description$datapath, sheet=ifelse(input$column_description_sheet == "", 1, input$column_description_sheet))
    } else {
      tdf <- read.csv(input$column_description$datapath)
    }
    return(tdf)
  })
  
  # from the dtypes file process other info
  process_dtypes <- reactive({
    df <- load_dataset()
    dtypes <- load_dtypes()
    
    column <- vector()
    mode <- vector()
    type <- vector()
    countable <- vector()
    
    if(!is.null(dtypes)) {
      column <- as.vector(dtypes$column)
      mode <- as.vector(dtypes$mode)
      type <- as.vector(dtypes$type)
      countable <- as.vector(dtypes$countable)
    }
    
    if(input$guess_column_types) {
      # check the others
      for(c in colnames(df)) {
        if(c %in% column) { #already specified
          next
        }
        
        m <- 'automatic'
        t <- infer_column_type(df, c, input$ratio_factor, input$ratio_numeric)
        
        column <- append(column, c)
        mode <- append(mode, m)
        type <- append(type, t)
        countable <- append(countable, NA)
      }
    }
    
    dtypes_n <- as.data.frame(list(column=column, mode=mode, type=type, countable=countable))
    return(dtypes_n)
  })
  
  get_valid_columns <- function(cols) {
    if(length(cols) == 0) {
      return(cols)
    }
    df <- load_dataset()
    return(cols[sapply(cols, function(x) {(x %in% colnames(df))})])
  }
  
  gimme_numerical <- reactive({
    dtypes <- process_dtypes()
    candidates <- unique(dtypes[(tolower(dtypes$type) == 'numeric') | (tolower(dtypes$type) == 'int') | (tolower(dtypes$type) == 'float'), ]$column)
    return(get_valid_columns(candidates))
  })
  
  gimme_categorical <- reactive({
    dtypes <- process_dtypes()
    candidates <- unique(dtypes[(tolower(dtypes$type) == 'factor') | (tolower(dtypes$type) == 'categorical') | (tolower(dtypes$type) == 'binary'), ]$column)
    return(get_valid_columns(candidates))
  })
  
  process_df <- reactive({
    df <- load_dataset()
    dtypes <- process_dtypes()
    
    for(c in gimme_numerical()) {
      df[[c]] <- as.numeric(df[[c]])
    }
    
    for(c in gimme_categorical()) {
      df[[c]] <- as.factor(as.character(df[[c]]))
    }
    
    df$.all_samples <- 1
    
    return(df)
  })
  
  complete_df <- reactive({
    df <- process_df()
    df[complete.cases(df[, .matchvariables()]), ]
  })
  
  loaded_file_output <- function() {
    if (is.null(input$dataset$datapath)) {
      paste0("Using sample data.")
    } else {
      if(is.null(input$column_description$datapath)) {
        paste0("Using uploaded file `", input$dataset$name, "` without column definition file.")
      } else {
        paste0("Using uploaded file `", input$dataset$name, "` with column definition file `", input$column_description$name, "`.")
      }
    }
  }
  
  output$dataset_info <- renderText({
    loaded_file_output()
  })
  
  output$column_descrition_red <- renderText({
    dtypes <- load_dtypes()
    if(is.null(dtypes) & !input$guess_column_types) {
      "Either specify to guess the column types or insert description file"
    }
  })
  
  output$match_group_red <- renderText({
    if(!(input$match_group %in% colnames(process_df()))) {
      "Match variable not present and it is required!"
    }
  })
  
  # based download name
  base_download_name <- reactive({
    dfn <- "sample"
    if(!is.null(input$dataset$datapath)) {
      dfn <- input$dataset$name
    }
    
    dfd <- "none"
    if(!is.null(input$column_description$datapath)) {
      dfd <- input$column_description$name
    }
    
    return(paste(format(Sys.time(), "%Y%m%d"), dfn, dfd, VERSION, sep="_"))
  })
  
  
  .matchvariables <- reactive({
    data <- process_df()
    dtypes <- process_dtypes()
    setdiff(unique(dtypes[(dtypes$type != 'skip') & (!is.na(dtypes$column)), ]$column), c('.all_samples', input$match_group))
  })
  
  output$complete_set_table <- reactive({
    table(complete_df()[[input$match_group]])
  })
  
  output$variables_used <- renderPrint({
    .matchvariables()
  })
  
  output$table_column_description <- renderTable({
    process_dtypes()
  }, rownames = FALSE)
  
  output$download_table_column_description <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_datatypes.csv")
    },
    content = function(file) {
      write.csv(process_dtypes(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  .matched_data <- reactive({
    matched_data <- match_model(complete_df(), input$match_group, .matchvariables(), input$match_distance, input$match_method)
    matched_data[[input$match_group]] <- as.factor(as.character(matched_data[[input$match_group]]))
    matched_data
  })
  
  output$complete_set_matched <- reactive({
    table(.matched_data()[[input$match_group]])
  })
  
  .std_cat <- reactive({
    std_cat(.matched_data(), input$match_group, intersect(gimme_categorical(), .matchvariables()))
  })
  
  output$table_categorical <- renderTable({
    .std_cat()
  }, digits=3, rownames=T)
  
  output$download_table_categorical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_categorical.csv")
    },
    content = function(file) {
      write.csv(.std_cat(), file, row.names = T)
    },
    contentType = "text/csv"
  )
  
  .std_num <- reactive({
    std_num(.matched_data(), input$match_group, intersect(gimme_numerical(), .matchvariables()))
  })
  
  output$table_numerical <- renderTable({
    .std_num()
  }, digits=3, rownames=T)
  
  output$download_table_numerical <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_numerical.csv")
    },
    content = function(file) {
      write.csv(.std_num(), file, row.names = T)
    },
    contentType = "text/csv"
  )
  
  output$download_matched_data <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_matched_data.csv")
    },
    content = function(file) {
      write.csv(.matched_data(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}