#' @import rlang
#' @import tibble
#' @import stringr
#'
#' @export

cpp_check_cpf <- function(data, cpfs){

  if(!is.character(cpfs)){
    stop("cpfs must be character vector.")
  }

  ## Split
  split <- stringr::str_split_fixed(cpfs, "", 11)
  split <- apply(split, 2, function(x) as.numeric(x))

  ## First digit
  weights <- seq(10, 2)
  fd <- sweep(split[,1:9], MARGIN = 2, weights, `*`)
  sum <- rowSums(fd)
  remainder <- sum %% 11
  first_digit <- 11 - remainder
  first_digit <- ifelse(first_digit > 9, 0, first_digit)

  ## Second digit
  split_fd <- cbind(split, first_digit, deparse.level = 0)
  weights <- c(11, weights)
  sd <- sweep(split_fd[,c(1:9, 12)], MARGIN = 2, weights, `*`)
  sums <- rowSums(sd)
  remainders <- sums %% 11
  second_digit <- 11 - remainders
  second_digit <- ifelse(second_digit > 9, 0, second_digit)

  ## Validade
  cod_validate <- cbind(first_digit, second_digit, deparse.level = 0)
  cod_disponivel <-  split[,10:11]
  valid_code <- ifelse(cod_validate == cod_disponivel, 1, 0)
  colnames(valid_code) <- c("first_digit_valid", "second_digit_valid")
  valid_code <- tibble::as_tibble(valid_code)

  return(tibble::as_tibble(cbind(data, valid_code)))
}

