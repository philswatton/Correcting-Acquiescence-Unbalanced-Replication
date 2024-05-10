# Functions to construct lavaan formulas ----

# Helper function for repeat operation
formula_constructor <- function(factor, vars, beta=NULL) return(str_c(factor, " =~ ", str_c(str_c(beta, vars), collapse=" + ")))

# Main builder function
build_formula <- function(identifier = NULL, type) {

  # 1) Setup

  # Core variables always used
  lr <- str_c("lr", 1:5)
  al <- str_c("al", 1:5)

  # If identifier is NULL, we are building the comparison model
  if (is.null(identifier)) word <- "Uncorrected"
  else word <- "Corrected"


  # 2) Create main part of formula

  # Create core of formula
  f <- str_c(formula_constructor(str_c("Left", word), lr),
             "\n",
             formula_constructor(str_c("Auth", word), al))


  # 3) Prepare and add identifier to model (OR return if comparison model)

  # If identifier is NULL, return
  if (is.null(identifier)) return(f)

  # Otherwise, create the other required components
  if (identifier == "Zero") {
    name <- "Z"
    vars <- str_c("zero", c(7,1,4,11,5,9))
  }
  if (identifier == "Empathy") {
    name <- "E"
    vars <- str_c("em", 1:10)
  }

  # Add additional factor for identifier
  f <- str_c(f, "\n", formula_constructor(name, vars))


  # 4) Add acquiescence to model, conditional on type
  # type 1: Identify by constraining acq effect to 1, freeing variance
  # type 2: Identify by constraining to same effect, constraining variance to 1

  # Make selections based on type
  if (type == 1) {
    beta <- 1
    add_variances <- T
  }
  if (type == 2) {
    beta <- "a"
    add_variances <- F
  }

  # Add acquiescence
  f <- str_c(f, "\n", formula_constructor("Acq", c(lr, al, vars), str_c(beta, "*")))

  # If type 1, add variances
  if (add_variances) {
    f <- str_c(f,
               "\n",
               str_c(name, "~~1*", name),
               "\nLeftCorrected ~~ 1*LeftCorrected
               AuthCorrected ~~ 1*AuthCorrected
               Acq~~Acq")
  }

  # Return
  return(f)

}


# Functions to run models ----

# Wrapper functions to lavaan::cfa (for type 1) and lavaan::lavaan (for type 2)
cfa_wrapper <- function(f, df, ...) cfa(f, df, std.lv=T, ...)
lav_wrapper <- function(f, df, ...) lavaan(f, df, auto.var=T, int.ov.free = T, ...)



# Formulas for running regressions ----

m1 <- RightUncorrected ~ edlevel
m2 <- RightCorrected ~ edlevel
m3 <- AuthUncorrected ~ edlevel
m4 <- AuthCorrected ~ edlevel

models <- list(m1,m2,m3,m4)




# Pipeline ----

pipe <- function(data, identifier, type, ordered) {
  # Prepare model formulas
  comp_model <- build_formula(identifier=NULL, type=NULL)
  full_model <- build_formula(identifier=identifier, type=type)

  # Select estimator based on whether ordered or not
  if (ordered) estimator <- "ULSMV"
  else estimator <- "MLR"

  # Select fit fn for main model based on whether type 1 or 2
  if (type == 1) fit_fn <- lav_wrapper
  if (type == 2) fit_fn <- cfa_wrapper

  # Fit models conditional on whether ordered and type
  comp_fit <- cfa_wrapper(comp_model, data, ordered=ordered, estimator=estimator)
  full_fit <- fit_fn(full_model, data, ordered=ordered, estimator=estimator)

  # Predict factor scores
  comp_factors <- as.data.frame(lavPredict(comp_fit))
  full_factors <- as.data.frame(lavPredict(full_fit))

  # Add predicted factor scores to df
  out_data <- data %>%
    cbind(comp_factors, full_factors) %>%
    mutate(RightUncorrected = -LeftUncorrected,
           RightCorrected = -LeftCorrected)

  # Run regressions
  regression_results <- map(models, ~lm(.x, out_data))

  # Output list
  out_list <- list(
    df = out_data,
    comp = comp_fit,
    fit = full_fit,
    regList = regression_results
  )

  # Return
  return(out_list)

}



