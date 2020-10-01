correct_missing_star <- function(function_strings){
  # Handle when input does not contain multiplication sign before x, i.e. 2+4x
  gsub(pattern = "([0-9]+)(x)", replacement = "\\1*\\2",x = function_strings)
}

correct_decimal_comma <- function(function_strings){
  # Handle when civilized users using decimal commas in formulas
  gsub(pattern = "\\,", replacement = "\\.", x = function_strings)
}

remove_empty <- function(function_strings){
  function_strings[function_strings != "" & !is.null(function_strings) & !is.na(function_strings)]
}

evaluate_functions <- function(function_strings, x){
  # Evaluates a vector of function strings over an input space x
  as.data.table(cbind(x, sapply(function_strings, function(func){eval(parse(text = func))})))
}


