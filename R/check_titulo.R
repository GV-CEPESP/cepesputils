#' @import rlang
#' @import tibble
#'
#' @export

cpp_check_titulos <- function(data, titulo, ufs){

  titulos <- rlang::enquo(titulo)

  uf <- rlang::enquo(ufs)

    if(nrow(data) != length(titulos)){
    stop("data and titulos must have the same length.")
  }

  ## Check 12 digits
  teste_digitos <- ifelse(nchar(titulos) == 12, 1, 0)

  ## Check UF
  if(!is.null(uf)){
    if(length(uf) != length(titulos)){
      stop("uf and titulos must have the same length.")
    }

    teste_uf <- dplyr::case_when(uf == "SP" & substr(titulos, 9, 10) == "01" ~ 1,
                                 uf == "MG" & substr(titulos, 9, 10) == "02" ~ 1,
                                 uf == "RJ" & substr(titulos, 9, 10) == "03" ~ 1,
                                 uf == "RS" & substr(titulos, 9, 10) == "04" ~ 1,
                                 uf == "BA" & substr(titulos, 9, 10) == "05" ~ 1,
                                 uf == "PR" & substr(titulos, 9, 10) == "06" ~ 1,
                                 uf == "CE" & substr(titulos, 9, 10) == "07" ~ 1,
                                 uf == "PE" & substr(titulos, 9, 10) == "08" ~ 1,
                                 uf == "SC" & substr(titulos, 9, 10) == "09" ~ 1,
                                 uf == "GO" & substr(titulos, 9, 10) == "10" ~ 1,
                                 uf == "MA" & substr(titulos, 9, 10) == "11" ~ 1,
                                 uf == "PB" & substr(titulos, 9, 10) == "12" ~ 1,
                                 uf == "PA" & substr(titulos, 9, 10) == "13" ~ 1,
                                 uf == "ES" & substr(titulos, 9, 10) == "14" ~ 1,
                                 uf == "PI" & substr(titulos, 9, 10) == "15" ~ 1,
                                 uf == "RN" & substr(titulos, 9, 10) == "16" ~ 1,
                                 uf == "AL" & substr(titulos, 9, 10) == "17" ~ 1,
                                 uf == "MT" & substr(titulos, 9, 10) == "18" ~ 1,
                                 uf == "MS" & substr(titulos, 9, 10) == "19" ~ 1,
                                 uf == "DF" & substr(titulos, 9, 10) == "20" ~ 1,
                                 uf == "SE" & substr(titulos, 9, 10) == "21" ~ 1,
                                 uf == "AM" & substr(titulos, 9, 10) == "22" ~ 1,
                                 uf == "RO" & substr(titulos, 9, 10) == "23" ~ 1,
                                 uf == "AC" & substr(titulos, 9, 10) == "24" ~ 1,
                                 uf == "AP" & substr(titulos, 9, 10) == "25" ~ 1,
                                 uf == "RR" & substr(titulos, 9, 10) == "26" ~ 1,
                                 uf == "TO" & substr(titulos, 9, 10) == "27" ~ 1,
                                 uf == "ZZ" & substr(titulos, 9, 10) == "28" ~ 1,
                                 T                                           ~ 0)
  } else {
    teste_uf <- NA
  }

  # Digito verificador
  x1 <- 2 * as.numeric(substr(titulos, 1, 1))
  x2 <- 3 * as.numeric(substr(titulos, 2, 2))
  x3 <- 4 * as.numeric(substr(titulos, 3, 3))
  x4 <- 5 * as.numeric(substr(titulos, 4, 4))
  x5 <- 6 * as.numeric(substr(titulos, 5, 5))
  x6 <- 7 * as.numeric(substr(titulos, 6, 6))
  x7 <- 8 * as.numeric(substr(titulos, 7, 7))
  x8 <- 9 * as.numeric(substr(titulos, 8, 8))
  temp <- data.frame(x1,x2,x3,x4,x5,x6,x7,x8)

  v1 <- rowSums(temp[,c("x1","x2","x3","x4","x5","x6","x7","x8")]) %% 11

  v1 <- ifelse(v1 == 10, 0, v1)

  x9 <- 7 * as.numeric(substr(titulos, 9, 9))
  x10 <- 8 * as.numeric(substr(titulos, 10, 10))
  x11 <- 9 * v1
  temp2 <- data.frame(x9,x10,x11)
  v2 <- rowSums(temp2[,c("x9","x10","x11")]) %% 11
  v2 <- ifelse(v2 == 10, 0, v2)

  verif <- paste0(v1, v2)

  teste_verif <- ifelse(verif == substr(titulos, 11, 12), 1, 0)

  tibble::as_tibble(cbind(data, teste_digitos, teste_uf, teste_verif))
}
