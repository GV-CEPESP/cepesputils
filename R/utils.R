#' @export
cpp_titulo <- function(titulos){
  for(i in seq_along(titulos)){
    if(is.na(titulos[[i]])){
      return(NA_character_)
    }
    while(stringr::str_length(titulos[[i]]) < 12){
      titulos[[i]] <- paste0("0",titulos[[i]])
    }
  }
  return(titulos)
}

#' @export
cpp_cpf <- function(cpfs){
  for(i in seq_along(cpfs)){
    if(is.na(cpfs[[i]])){
      return(NA_character_)
    }
    while(stringr::str_length(cpfs[[i]]) < 11){
      cpfs[[i]] <- paste0("0",cpfs[[i]])
    }
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
