#' A Reference Class to represent a site in a stepped-wedge cluster-randomised
#' trial.
#'
#' @field name The name of the site.
#'
#' @field sim.ppt.per.unit.time For simulation purposes, the number of
#'   participants generated per unit time.
#'
#' @field sim.normal.preintervention.mean For simulation purposes, the mean
#'   value of participants at the site before intervention.
#' @field sim.normal.postintervention.mean For simulation purposes, the mean
#'   value of participants at the site after intervention.
#' @field sim.normal.sd For simulation purposes, the standard deviation of
#'   participants at the site.
#' @field sim.normal.effect.per.unit.time For simulation purposes, the effect of
#'   time on outcomes.
#'
#' @field timed.sampling.dist A Timed.Sampling.Dist that can be used to simulate
#'   non-normal data.
#' @export
SW.Site = setRefClass(
  "SW.Site",
  fields = list(name = "character",

                sim.ppt.per.unit.time = "numeric",

                sim.normal.preintervention.mean = "numeric",
                sim.normal.postintervention.mean = "numeric",
                sim.normal.sd = "numeric",
                sim.normal.effect.per.unit.time = "numeric",

                timed.sampling.dist = "I.Sampler",

                randomisation.group = "numeric"),
  methods = list(
    initialize = function(name = 'Site', randomisation.group = NA_integer_) {
      "Initialize site with a name."
      name <<- name
      randomisation.group <<- randomisation.group
    },
    get.name = function() {
      return(name)
    },
    set.timed.sampling.dist = function(new.tsd) {
      timed.sampling.dist <<- new.tsd
    },
    get.timed.sampling.dist = function() {
      return(timed.sampling.dist)
    },
    set.sim.ppt.per.unit.time = function(sim.ppt.per.unit.time) {
      sim.ppt.per.unit.time <<- sim.ppt.per.unit.time
    }
  )
)
