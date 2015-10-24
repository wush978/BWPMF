#'@export
deserialize_cookie <- function(src) {
  UseMethod("deserialize_cookie")
}

#'@export
deserialize_hostname <- function(src) {
  UseMethod("deserialize_hostname")
}

#'@export
deserialize_history <- function(src) {
  UseMethod("deserialize_history")
}

#'@export
encode <- function(x, user_visit_lower_bound = 0L, ...) {
  UseMethod("encode")
}

#'@export
encode_data <- function(x, ...) {
  UseMethod("encode_data")
}