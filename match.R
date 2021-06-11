
library(MatchIt)
library(stddiff)

.match_loaded <- function() {
  return(TRUE)
}


match_model <- function(df, subgroup_variable, matching_variables, distance='mahalanobis', method='nearest') {
  tryCatch({
    mod_match <- matchit(as.formula(paste(subgroup_variable, paste(matching_variables, 
                                                                   collapse=' + '), 
                                          sep=' ~ ')),
                         method = method, 
                         distance = distance,
                         data = df)
  }, error=function(e){
    print(e)
    mod_match <- NULL
  })
  
  
  return(as.data.frame(match.data(mod_match)))
}

std_num <- function(matched_data, subgroup_variable, matching_numeric) {
  if(length(matching_numeric) > 0) {
    g_var <- which(subgroup_variable == colnames(matched_data))[[1]]
    stdn1 <- as.data.frame(stddiff::stddiff.numeric(matched_data, 
                                                    gcol=g_var, 
                                                    vcol=which(colnames(matched_data) %in% matching_numeric) ))
  } else {
    stdn1 <- NULL
  }
  return(stdn1)
}

std_cat <- function(matched_data, subgroup_variable, matching_categoric) {
  if(length(matching_categoric) > 0) {
    for(i in matching_categoric) {
      if(length(levels(factor(matched_data[[i]]))) <= 1) {
        matching_categoric <- setdiff(matching_categoric, c(i))
      }
    }
    
    g_var <- which(subgroup_variable == colnames(matched_data))[[1]]
    stdc1 <- as.data.frame(stddiff::stddiff.category(matched_data, 
                                                     gcol=g_var, 
                                                     vcol=which(colnames(matched_data) %in% matching_categoric)))
  } else {
    stdc1 <- NULL
  }
  return(stdc1)
}

