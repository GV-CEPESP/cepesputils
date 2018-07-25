#' @export
cpp_titulo <- function(titulo){
  while(stringr::str_length(titulo) < 12){
    titulo <- paste0("0",titulo)
  }
  return(titulo)
}

#' @export
cpp_numeric_group <- function(str){
  str <- as.character(str)
  count = 0
  i = 1
  new_ <- vector(mode = "character")
  while(count < nchar(str)){
    subs <- substr(str, start = nchar(str) - count, stop = nchar(str) - count)
    if(count %% 3 == 0 & count > 0){
      new_[[i]] <- "."
      i = i + 1
    }
    new_[[i]] <- subs
    count = count + 1
    i = i + 1
  }
  paste0(rev(new_), collapse = "")
}
