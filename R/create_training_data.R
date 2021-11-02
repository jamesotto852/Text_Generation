# Function factories for vectorized encoders and decoders:
encoder_fun <- function(char_table) {
  encoder <- function(char) {
    if (length(char) > 1) return(map_dbl(char, encoder))
    which(char_table == char)
  } 
  
  encoder
}

decoder_fun <- function(char_table) {
  decoder <- function(i) {
    if (length(i) > 1) return(map_chr(i, decoder))
    char_table[i]
  } 
  
  decoder
}

# Bundle up all of authors works into list
create_training_data <- function(author) {
  # Get list of books corresponding to author
  files <- list.files(here("Data/Gutenberg/cleaned", author))
  
  # Concatenate books into singular corpus
  books <- map(here("Data/Gutenberg/cleaned", author, files), \(x) read_lines(x, skip_empty_rows = FALSE))
  books <- unlist(books)
  
  # Empirical frequency distribution for encoder/decoder
  char_table <- table(books) |>
    sort(decreasing = TRUE) |>
    names()
  
  encoder <- encoder_fun(char_table)
  decoder <- decoder_fun(char_table)
  
  # Encode corpus
  books <- encoder(books)
  
  list(
    author = author,
    books = books,
    encoder = encoder, 
    decoder = decoder
  )
}

# Helper function, make sequences into tibble rows
make_row <- function(index, books, batch) {
  lift_dl(tibble_row, .name_repair = "minimal")(
    books[index:(index + batch)]
  ) |>
    `names<-`(c(paste0("X", 1:batch), "Y"))
}

# Create df of sequences of specified batch length
write_seq_df <- function(batch, data) {
  map_dfr(1:(length(data$books) - batch), make_row, data$books, batch) |>
    write_csv(here("Data/Training_Data", data$author, paste0("df_seq_", batch, ".csv")))
  
  NULL # Don't want to return anything -- these boots are made for walkin'
}

# Create directory with all files necessary for training model and decoding output
write_train_data <- function(author, batch_max, batch_min = 1) {
  if(!dir.exists(here("Data/Training_Data", author))) {
    dir.create(here("Data/Training_Data", author), recursive = TRUE)
  }
  
  data <- create_training_data(author)
  saveRDS(data, here("Data/Training_Data", author, "data.RDS"))
  walk(batch_min:batch_max, write_seq_df, data)
}
