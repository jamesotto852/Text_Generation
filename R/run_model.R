run_model <- function(seed, author, steps = 250, bootstrap = TRUE) {
  model_1 <- load_model_hdf5(here("Models", author, "model_1.h5"))
  model_5 <- load_model_hdf5(here("Models", author, "model_5.h5"))
  model_10 <- load_model_hdf5(here("Models", author, "model_10.h5"))
  model_30 <- load_model_hdf5(here("Models", author, "model_30.h5"))
  
  data <- read_rds(here("Data/Training_Data/", author, "data.RDS"))
  
  make_pred <- function(X, k) {
    if (k == 1) return(predict(model_1, X))
    if (k == 5) return(predict(model_5, X))
    if (k == 10) return(predict(model_10, X))
    if (k == 30) return(predict(model_30, X))
  }
  
  if (!bootstrap) {
    seed <- str_pad(seed, 30, side = "left") 
  }

  seed <- str_extract_all(seed, ".") |>
    unlist() |>
    data$encoder()
  
  seed <- seed - 1 # Off-by-one between R and Python
  
  for (i in 1:steps) {
    seq_length <- length(seed)
    mod_index <- max(which(seq_length - c(1, 5, 10, 30) >= 0))
    k <- c(1, 5, 10, 30)[mod_index]
    
    X <- rev(seed)[1:k]
    X <- rev(X)
    X <- matrix(X, ncol = k)

    probs <- make_pred(X, k) |>
      as_vector()

    choices <- 1:(length(probs)) - 1 # Off-by-one between R and Python
    pred <- sample(x = choices, size = 1, prob = probs)

    seed <- c(seed, pred)
  }
  
  seed <- seed + 1 # Off-by-one between R and Python
  
  map_chr(seed, data$decoder) |>
    paste(collapse = "") |>
    str_remove("^\\s*(?=\\S)")
}


