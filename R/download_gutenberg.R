download_gutenberg <- function(url, author, title) {
  ROOT_DIR <- "Data/Gutenberg/raw"

  if(!dir.exists(here(ROOT_DIR, author))) dir.create(here(ROOT_DIR, author), recursive = TRUE)
  download.file(url, here(ROOT_DIR, author, paste0(title, ".txt")))
}
