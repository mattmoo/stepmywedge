#' An interface for getting a value for a patient in a simulated context. Must
#' implement the get.value function.
#'
#'
#'@export
I.Sampler = setRefClass(
  'I.Sampler',
  methods = list(
    get.value = function(time = NA, size = 1, ...) {
      "Get a value. If you provide a time, values and weights will only be applied if time>=time.begin and <time.end. If no time is provided, all values will be used. Can also add arguments for base::sample()"
    }
  )
)
