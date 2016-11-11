#' Available trees for measure and compute index
#' 
#' @export
#' 
available_tree <- function(x, y, border) {
  ifelse(
    '&'(
      '&'(
        x <= max(x)-border,
        x >= min(x)+border
      ),
      '&'(
        y <= max(y)-border,
        y >= min(y)+border
      )
    ),
    TRUE, FALSE
  )
}