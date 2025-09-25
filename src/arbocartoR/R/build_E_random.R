#' Generate a list of introduction events
#'
#' @description Function used to generate a random list of schedule introduction.
#' The introduction events generated can occur in different locations and at different dates in a given period.
#' The number of individuals introduced is randomly distributed over all the events.
#' If the number of events is not provided, location and date are randomly sampled for each introduced individual.
#' The epidemiological stage can be either deterministic (unique value provided) or random (randomly selected in a vector of values).
#'
#' @param period_start Date in '\%Y-\%m-\%d' format. Defines the beginning of the period for introduction.
#' @param period_end Date in '\%Y-\%m-\%d' format. Defines the end of the period for introduction.
#' @param n_events Numeric value. Number of introduction events generated.
#' @param n_ind Numeric value. Number of individuals introduced.
#' @param stage Character vector. Epidemiological stage of introduced individuals.
#' @param loc Character or numeric vector. List of patches to seed random introductions.
#'
#' @return Data frame with scheduled introductions.
#'
#' @importFrom stats aggregate rmultinom
#'
#' @keywords events
#'
#' @examples
#' \dontrun{
#' build_E_random(period_start = as.Date("2020/03/10"),
#' period_end = as.Date("2020/09/30"),
#' n_ind = NULL, n_events = 10,
#' stage = "Eh", loc = LETTERS[1:3])
#' }
#'
#' @export

build_E_random <- function(period_start,
                           period_end = period_start,
                           n_events = NULL,
                           n_ind = NULL,
                           stage = "Eh",
                           loc) {

  ###
  ### Checks
  ###
  if (!inherits(period_start, "Date") || !inherits(period_end, "Date")) {
    stop("start and end dates must be dates ('Date' class)")
  }

  if (is.null(n_events) & is.null(n_ind)) stop("n_ind or n_events must be provided")

  if (is.null(n_ind)) n_ind <- n_events

  if (!is.null(n_events)) {
    if (!inherits(n_events, c("numeric", "integer")) || length(n_events) != 1) {
      stop("n_events must be a unique numeric value")
    }

    if (n_ind < n_events) {
      stop("n_ind must be greater than or equal to n_events")
    }
  }

  if (!inherits(n_ind, c("integer", "numeric")) || length(n_ind) != 1) {
    stop("n_ind must be numeric")
  }

  list_compartments <- list_compartments()$u0

  if (!inherits(stage, "character") || sum(is.na(match(stage, list_compartments))) > 0) {
    stop(paste(c("stage must be a character vector. Authorized entries:", list_compartments), collapse = " "))
  }

  ###
  period <- seq(period_start, period_end, by = "day")
  ###
  ###

  # sample time, location, and number of individuals
  # every possible introduction
  sampling <- expand.grid(period, loc, 1, stage)

  # sample date and loc based on the number of events
  if (is.null(n_events)) {
    sampling <- sampling[sample(seq(nrow(sampling)), n_ind, replace = TRUE), ]
    sampling <- aggregate(Var3 ~ ., data = sampling, sum)
  } else if (n_events == n_ind) {
    if (nrow(sampling) < n_events) {
      stop("The number of events cannot exceed the number of days in the period times the number of possible locations for introduction")
    }
    sampling <- sampling[sample(seq(nrow(sampling)), n_ind, replace = FALSE), ]
  } else {
    if (nrow(sampling) < n_events) {
      stop("The number of events cannot exceed the number of days in the period times the number of possible locations for introduction")
    }
    sampling <- sampling[sample(seq(nrow(sampling)), n_events, replace = FALSE), ]

    # distribute individuals over events
    distribution <- rmultinom(n = 1, size = n_ind, prob = rep(1 / n_events, n_events))
    while (0 %in% distribution) distribution <- rmultinom(n = 1, size = n_ind, prob = rep(1 / n_events, n_events))
    sampling$Var3 <- distribution
  }

  events <- data.frame(
    time = sampling$Var1,   # The time that the event happens
    dest = sampling$Var2,   # In which node does the event occur
    n = sampling$Var3,      # How many individuals are moved
    select = sampling$Var4  # Use the 4th column in the model select matrix
  )

  return(events)
}
