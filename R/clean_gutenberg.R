clean_gutenberg <- function(author, file) {
  if (length(file) > 1) return(walk(file, \(x) clean_gutenberg(author, x)))
  
  book <- read_lines(file = here("Data/Gutenberg/raw", author, file), skip_empty_rows = FALSE)

  # Project Gutenberg indicates original text with "***"
  text_between <- which(str_detect(book, "\\*{3}"))
  book <- book[(text_between[1] + 1) : (text_between[2] - 1)]
  
  # Remove blank lines
  book <- book[book != ""]

  # Want vector of characters, not words
  book <- map_chr(book, \(x) paste0(x, " ")) |>
    str_extract_all(boundary("character")) |>
    unlist()

  # Write the cleaned file to disk
  if(!dir.exists(here("Data/Gutenberg/cleaned", author))) {
    dir.create(here("Data/Gutenberg/cleaned", author), recursive = TRUE)
  }
  
  write(book, here("Data/Gutenberg/cleaned", author, file))
}
