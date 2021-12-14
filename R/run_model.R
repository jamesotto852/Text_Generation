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

    choices <- 1:(length(probs))
    pred <- sample(x = choices, size = 1, prob = probs) - 1 # Off-by-one between R and Python

    seed <- c(seed, pred)
  }
  
  seed <- seed + 1 # Off-by-one between R and Python
  
  map_chr(seed, data$decoder) |>
    paste(collapse = "")
}


run_model("Frankensteins monster was", "Merry Shelley", 100, bootstrap = FALSE)
run_model("Frankensteins monster was", "Merry Shelley", 1000, bootstrap = TRUE)

# 8 epoch output:
# "Frankensteins monster wascing miget, but lithles, I was lokt tometakn of the wood of might compased theirfk 
# farigled to acco, them, bean, and my materrepaid, that country. I stone. Now amasprede hively the fext on they in; 
# understant sad expleded buincered!_ Chapter 2n cersed of the bittlrive dayscersant his collinedy, ‘of convicted to happiness. 
# I aghirly. She wo chingle progaced that I mappected. If ipparmation fars of arvoured same, the mack to thes of a couctry I had condecned. 
# Deered, they returning I gould coons monnts vistom.” there of cheeced. She was hoaruagly beeatien of whith had precepve and sade us 
# if enlowed which ly shis, Clerstarching Vittor, appeared this encmuricable id. The lyought conceent lemint and calmen. 
# He is husistirely and pleasons which fave the youth of the fiefes me to relieving hen dount of ly the rave towards, 
# devated the cours of the viccur. How very ridd I strubled had recoingres. That I had connequenced the atconcemed aptrigitad my mand fictety.” 
# As he had not hide among aich"