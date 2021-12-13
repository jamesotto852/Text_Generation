fit_model <- function(author, k, n_epochs) {
  df <- read_csv(here("Data/Training_Data", author, paste0("df_seq_", k, ".csv")))

  X_mat <- df |>
    select(starts_with("X")) |>
    as.matrix()
  
  Y_mat <- df |>
    select(Y) |>
    as.matrix()
  
  n_target_nodes <- length(unique(df$Y)) 

  # fix off-by-one errors:
  X_mat <- X_mat - 1
  Y_mat <- Y_mat - 1

  # NN with embedding and RNN
  model <- keras_model_sequential() |>
    layer_embedding(input_dim = n_target_nodes, output_dim = 16) |>
    layer_simple_rnn(units = 1000, activation = "relu") |>
    # layer_dense(units = 100, activation = "relu") |>
    layer_dense(units = n_target_nodes, activation = "softmax")

  model |> compile(loss = "sparse_categorical_crossentropy", optimizer = "adam", metrics = "accuracy")

  model |> fit(
      X_mat, Y_mat,
      batch_size = 1000, 
      epochs = n_epochs)
  
  if(!dir.exists(here("Models", author))) {
    dir.create(here("Models", author), recursive = TRUE)
  }
  
  save_model_hdf5(model, here("Models", author, paste0("model_", k, ".h5")))
  
  NULL
}