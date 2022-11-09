AUTO_DETECT_NEWVAR <- FALSE

script_results_identical <- function(result_name) {
  # Get e
  e <- get('e', parent.frame())
  # Get user's result from global
  if(exists(result_name, globalenv())) {
    user_res <- get(result_name, globalenv())
  } else {
    return(FALSE)
  }
  # Source correct result in new env and get result
  tempenv <- new.env()
  # Capture output to avoid double printing
  temp <- capture.output(
    local(
      try(
        source(e$correct_script_temp_path, local = TRUE),
        silent = TRUE
      ),
      envir = tempenv
    )
  )
  correct_res <- get(result_name, tempenv)
  # Compare results
  identical(user_res, correct_res)
}

# Multiple expression version of expr_creates_var
multi_expr_creates_var <- function(correctName=NULL){
  e <- get("e", parent.frame())
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  delta <- if(!customTests$AUTO_DETECT_NEWVAR){
    safeEval(e$expr, e)
  } else {
    e$delta
  }
  if(is.null(correctName)){
    passed <- length(delta) > 0
  } else {
    passed <- correctName %in% names(delta)
  }
  passed <- isTRUE(passed)
  if(passed){
    e$newVar <- e$val
    e$newVarName <- names(delta)[1]
    e$delta <- mergeLists(delta, e$delta)
  }
  return(passed)
}

# Check that the output/value produced by a script is correct
script_vals_identical <- function() {
  # Get e
  e <- get('e', parent.frame())
  # Get value produced from user's script
  user_val <- capture.output(
    local(
      try(
        # Must use eval-parse combo, not source, if we don't force user
        # to print result
        eval(e$expr),
        silent = TRUE
      )
    )
  )
  # Get value produced from correct script
  correct_val <- capture.output(
    local(
      try(
        # Must use eval-parse combo, not source, if we don't force user
        # to print result
        eval(parse(file = e$correct_script_temp_path)),
        silent = TRUE
      )
    )
  )
  # Compare values
  identical(user_val, correct_val)
}

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
  getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
  getState()$expr
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}
start_timer <- function() {
  e <- get('e', parent.frame())
  e$`__lesson_start_time` <- now()
  TRUE
}

stop_timer <- function() {
  e <- get('e', parent.frame())
  start_time <- e$`__lesson_start_time`
  stop_time <- now()
  to_print <- as.period(interval(start_time, stop_time))
  e$`__elapsed` <- as.ts(interval(start_time, stop_time))[1]
  print(e$`__elapsed`)
  print(to_print)
  TRUE
}

notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "No") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    message("\nMake sure you're connected to the internet\n")
    
    ID <- readline_clean("What is your ID? ")
    
    # Repeat back to them
    message("\nDoes everything look good?\n")
    message("Your ID: ", ID, "\n")
    
    yn <- select.list(c("Yes", "No"), graphics = FALSE)
    if(yn == "Yes") good <- TRUE
  }
  
  # Get course and lesson names

  lesson_name <- gsub(" ","+", attr(e$les, "lesson_name"))
  start_time <- gsub(" ","+",e$`__lesson_start_time`)
  time_elapsed <- e$`__elapsed`

  library(httr)
  baseLink <- "https://docs.google.com/forms/d/e/1FAIpQLSdU_q_QhNIoKdxpCBgDdiMbD_Okt3AldPqVk2IqRjcnAjb6dQ/formResponse?&submit=Submit?usp=pp_url"

  idAttr <- "&entry.826582333="
  lessonNameAttr <- "&entry.248191379="
  startTimeAttr <- "&entry.267874381="
  timeElapsed <- "&entry.840687692="

  toPost <- paste0(baseLink, idAttr, ID, lessonNameAttr, lesson_name, startTimeAttr, start_time, timeElapsed, time_elapsed)

  response <- POST(toPost) 
  if (response['status_code'] == 200) {
    message("\n Successfully Submited")
  } else {
    message("\n Please screen-shot and contact your instructor")
  }

  TRUE
}