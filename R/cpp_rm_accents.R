#' @import stringr
#'
#' @export

cpp_rm_accents <- function(x, encoding = "latin1")
{
  if (.Platform$OS.type == "unix") {
    stringr::str_replace_all(iconv(x, to = "ASCII//TRANSLIT"),
                             "[`'\"^~]", "")
  }
  else {
    gsub("`", "", iconv(x, from = encoding, to = "ASCII//TRANSLIT"))
  }
}
