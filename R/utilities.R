# HELPERS

#' Colour Scheme Picker
#'
#' @param palette A [`character`] string specifying the palette name.
#' @param aesthetic A [`character`] string specifying the name of the aesthetic
#'  that this scale works with. It must be one of "`colour`" or "`fill`".
#' @keywords internal
#' @noRd
scale_picker <- function(palette, aesthetic = "colour") {
  palette <- gsub(pattern = "[[:blank:]]", replacement = "", x = palette)
  fun <- sprintf("khroma::scale_%s_%s()", aesthetic, palette)
  eval(parse(text = fun))
}
