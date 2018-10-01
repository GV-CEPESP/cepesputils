#' @export
cpp_titulo <- function(titulos){
  while(any(nchar(titulos) < 12, na.rm = TRUE)){
    titulos[(nchar(titulos) < 12) & !is.na(titulos)] <- paste0("0", titulos[(nchar(titulos) < 12) & !is.na(titulos)])
  }
  return(titulos)
}

#' @export
cpp_cpf <- function(cpfs){
  while(any(nchar(cpfs) < 11, na.rm = TRUE)){
    cpfs[(nchar(cpfs) < 11)  & !is.na(cpfs)] <- paste0("0", cpfs[(nchar(cpfs) < 11) & !is.na(cpfs)])
  }
  return(cpfs)
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
