#' @import stringr
#'
#' @export

cpp_rm_accents <- function(x)
{
  if (.Platform$OS.type == "unix") {
    stringr::str_replace_all(iconv(x, to = "ASCII//TRANSLIT"),
                             "[`'\"^~]", "")
  }
  else {
    gsub("`", "", iconv(x, from = "latin1", to = "ASCII//TRANSLIT"))
  }
}
