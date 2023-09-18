#' A Reference Class to represent a cluster in a stepped-wedge cluster-randomised trial.
#'
#' @field name The name of the site.
#' @field cluster.start.time When data from the cluster starts being collected (usually start of study).
#' @field transition.start.time When the transition period starts (i.e. intervention started rolling out, but not finished).
#' @field intervention.start.time When the intervention is considered implemented.
#' @field cluster.end.time After which data is not included.
#' @field sites List of SW.Site included in the cluster.
#' @field cluster.dt A data.table with all sites.
#' @export
SW.Cluster = setRefClass(
  "SW.Cluster",
  fields = list(name = "character",
                cluster.start.time = "numeric",
                transition.start.time = "numeric",
                intervention.start.time = "numeric",
                cluster.end.time = "numeric",
                sites = "list",
                cluster.dt = "data.table"),

  methods = list(
    initialize = function(name = 'Cluster',
                          cluster.start.time = NaN,
                          transition.start.time = NaN,
                          intervention.start.time = NaN,
                          cluster.end.time = NaN) {
      "Initialize cluster with a name."
      name <<- name
      cluster.start.time <<- cluster.start.time
      transition.start.time <<- transition.start.time
      intervention.start.time <<- intervention.start.time
      cluster.end.time <<- cluster.end.time
    },

    get.name = function() {
      "Get cluster name."
      return(name)
    },
    get.sites = function() {
      "Get sites in the cluster."
      return(sites)
    },
    get.site.names = function() {
      "Get sites in the cluster."
      unlist(lapply(X = get.sites(), function(x) x$get.name()))
    },
    get.site.by.name = function(site.name) {
      "Get a site given it's name."
      return(sites[get.site.names() == site.name])
    },

    set.sites = function(sites) {
      "Replace sites in the cluster, sites should be provided as a SW.Site object."
      siteList = sites
      if (!is.list(siteList)) siteList = as.list(siteList)
      sites <<- siteList
    },
    add.sites = function(sites.to.add) {
      "Add sites to the cluster, sites should be provided as a SW.Site object."
      set.sites(c(sites, sites.to.add))
    },

    generate.site.dt = function() {
      # cluster.dt <<- data.table::data.table(
      #   cluster = name,
      #   site = get.site.names()
      # )
      #
      cluster.dt <<-
        rbindlist(lapply(
          X = sites,
          FUN = function(x)
            data.table(
              cluster = name,
              site = x$name,
              randomisation.group = x$randomisation.group
            )
        ))
    }

  )
)
