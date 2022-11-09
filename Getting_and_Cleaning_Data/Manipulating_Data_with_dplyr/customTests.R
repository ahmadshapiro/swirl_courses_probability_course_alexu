# Turn off double evaluation to make things faster
AUTO_DETECT_NEWVAR <- FALSE

match_call <- function(correct_call = NULL) {
  e <- get("e", parent.frame())
  # Trivial case
  if(is.null(correct_call)) return(TRUE)
  # Get full correct call
  full_correct_call <- expand_call(correct_call)  
  # Expand user's expression
  expr <- deparse(e$expr)
  full_user_expr <- expand_call(expr)
  # Compare function calls with full arg names
  identical(full_correct_call, full_user_expr)
}

# Utility function for match_call answer test
# Fills out a function call with full argument names
expand_call <- function(call_string) {
  # Quote expression
  qcall <- parse(text=call_string)[[1]]
  # If expression is not greater than length 1...
  if(length(qcall) <= 1) return(qcall)
  # See if it's an assignment
  is_assign <- is(qcall, "<-")
  # If assignment, process righthandside
  if(is_assign) {
    # Get righthand side
    rhs <- qcall[[3]]
    # If righthand side is not a call, can't use match.fun()
    if(!is.call(rhs)) return(qcall)
    # Get function from function name
    fun <- match.fun(rhs[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Get expanded call
    full_rhs <- match.call(fun, rhs)
    # Full call
    qcall[[3]] <- full_rhs
  } else { # If not assignment, process whole thing
    # Get function from function name
    fun <- match.fun(qcall[[1]])
    # match.call() does not support primitive functions
    if(is.primitive(fun)) return(qcall)
    # Full call
    qcall <- match.call(fun, qcall)
  } 
  # Return expanded function call
  qcall
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