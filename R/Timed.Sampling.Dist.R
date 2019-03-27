#'A Reference Class to represent a sampling distribution that depends on a time
#'that is provided.
#'
#'@field sampling.dt A data.table with three columns time.begin/time.end (time when the
#'  sampling distribution starts(>=)/stops(<), NA means ignore), weight
#'  (probabilities), and value (the value to be chosen).
#'@field legal.col.names A vector of names that are allowed as column names for
#'  sampling.dt
#' @import methods
#' @export Timed.Sampling.Dist
#' @exportClass Timed.Sampling.Dist
Timed.Sampling.Dist = setRefClass(
  "Timed.Sampling.Dist",
  contains = 'I.Sampler',
  fields = list(sampling.dt = "data.table",
                legal.col.names = "character"),
  methods = list(
    initialize = function(sampling.dt = NULL) {
      if (!is.null(sampling.dt)) {
        set.sampling.dt(sampling.dt)
      } else {
        sampling.dt <<- data.table()
      }
      legal.col.names <<- c("time.begin","time.end","weights","values")
    },

    #TODO: Make it a bit more flexible so that columns will be autofilled if absent.
    set.sampling.dt = function(input.sampling.dt) {
      "Sets entire sampling data.table, remember to give it the right columns!"
      #Ensure that the provided table has the right columns.
      if (!all(colnames(input.sampling.dt) %in% legal.col.names)) {
        stop(paste('Column names of sampling.dt should be in', paste(legal.col.names, collapse = ', ')))
      }
      sampling.dt <<- input.sampling.dt
      sort.sampling.dt()
      normalise.sampling.dt()
    },

    add.sampling.dist = function(values, weights = c(), time.begin = NA_real_, time.end = NA_real_) {
      "Adds a sampling distribution at the specified time. If either time limit is NA, there is no limit. Weights and values should be vectors of same length."
      #Set all weights to equal if none provided
      if (length(weights) == 0) {
        weights = rep(1,length(values))
      }
      #Ensure there are as many weights as values.
      if (length(values) != length(weights)) {
        error(paste0('length(values) != length(weights) [values: ',length(values),', weights: ', length(weights)))
      }
      new.weight.dt = data.table(values = values,
                                 weights = weights,
                                 time.begin = time.begin,
                                 time.end = time.end)
      sampling.dt <<- rbind(sampling.dt, new.weight.dt)
      sort.sampling.dt()
      normalise.sampling.dt()
    },

    sort.sampling.dt = function() {
      "Sorts sampling dt by time"
      sampling.dt <<- sampling.dt[order(time.begin)]
    },

    normalise.sampling.dt = function(weights.sum = 1000) {
      "Makes the weights add to a certain value."
      norm.dt = sampling.dt[,.(norm.factor = 1000/sum(weights)), by = .(time.begin, time.end)]
      temp.sampling.dt = data.table:::merge.data.table(sampling.dt, norm.dt)
      data.table::set(x = temp.sampling.dt,
                      j = 'weights',
                      value = temp.sampling.dt$weights * temp.sampling.dt$norm.factor)
      sampling.dt <<- temp.sampling.dt[,.(values, weights, time.begin, time.end)]

    },

    get.value = function(time = NA, size = NA, ...) {
      "Get a value. If you provide a time, values and weights will only be applied if time>=time.begin and <time.end. If no time is provided, all values will be used. Can also add arguments for base::sample()"
      #This is a nasty bodge job to get it working. Problem is that the last time can be buggered by all the splitting and such.
      min.time = sampling.dt[,min(time.begin)]
      max.time = sampling.dt[,max(time.end)]-1
      #There might be no time.
      if (length(time)==1 & is.na(time[1])) {
        return(sample(
          x = sampling.dt$values,
          prob = sampling.dt$weights,
          size = size,
          replace = T,
          ...
        ))
      } else {

        #Set keys for sampling.dt to prepare for overlaps.
        data.table::setkey(sampling.dt, time.begin, time.end)

        #Find overlaps, need to put requested times in a data.table with a second
        #identical time column. Also get the order for returning values in original
        #order.
        overlap.dt = foverlaps(data.table(time = time, time2 = time, order = 1:length(time)),
                               unique(sampling.dt[,.(time.begin, time.end)]),
                               by.x = c('time', 'time2'),
                               by.y = c('time.begin', 'time.end'))

        overlap.dt[,time2 := NULL]

        #Order the requested times
        setkey(overlap.dt, time)
        #Get the number of requested times in each time period.
        sum.dt = overlap.dt[,.N, by=.(t1 = time.begin, t2 = time.end)]
        #Sample the number from each time period, append the previously calculated
        #original order, and order by that.
        value.dt = sum.dt[,.(value = sample(x = sampling.dt[time.begin == t1 & time.end == t2, values],
                                            prob = sampling.dt[time.begin == t1 & time.end == t2, weights],
                                            replace = T,
                                            size = N)), by = .(t1, t2)][
                                              ,order := overlap.dt$order][
                                                order(order)
                                                ]
        return(value.dt$value)
      }
    },
    split.period = function(time.split, silent = F) {
      "Split the distribution at a certain time, resulting in two identical distributions either side, the second starting at time.split (unless you choose to split at an existing split, in which case nothing happens"
      #Check it's not already split there, or that's not the end.
      if (time.split %in% c(unique(sampling.dt$time.begin))) {
        if (!silent) {
          message(paste0('Split already existed at ', time.split))
        }
        return()
      } else {
        #Get the time split before the requested one.
        last.prev.start.time = max(unique(sampling.dt$time.begin)[time.split>unique(sampling.dt$time.begin)])
        #Get parameters for new distribution.
        old.dist = sampling.dt[time.begin == last.prev.start.time, ]
        #Modify timing of old distribution
        data.table::set(x = sampling.dt,
                        i = which(sampling.dt$time.begin == last.prev.start.time),
                        j = 'time.end',
                        value = time.split-1)
        #Add a sampling distribution like that.
        add.sampling.dist(values = old.dist$values,
                          weights = old.dist$weights,
                          time.begin = time.split,
                          time.end = old.dist$time.end)
      }

    },

    merge.periods.between =function(merge.time.begin, merge.time.end) {
      "Merges all periods between time.begin (default begin first period) and time.end (default end last period). Weights will be average of composing periods."
      #Split periods if required.
      split.period(merge.time.begin, silent = T)
      split.period(merge.time.end+1, silent = T)
      #Get periods from requested times.
      merge.period.sampling.dt = sampling.dt[merge.time.begin <= time.begin & merge.time.end >= time.end,]
      #Modify sampling.dt so that it doesn't include the merge period.
      sampling.dt <<- sampling.dt[!(merge.time.begin <= time.begin & merge.time.end >= time.end),]
      #Set all timings in merge period to the same.
      data.table::set(x = merge.period.sampling.dt,
                      j = 'time.begin',
                      value = merge.time.begin)
      data.table::set(x = merge.period.sampling.dt,
                      j = 'time.end',
                      value = merge.time.end)
      #Get mean of the merge periods (uses data.table syntax)
      merge.period.sampling.dt = merge.period.sampling.dt[, .(weights = mean(weights)), by = .(values, time.begin, time.end)]
      #Add newly merged period to sampling dt
      sampling.dt <<- rbind(sampling.dt, merge.period.sampling.dt)
      #Sort and normalise.
      sort.sampling.dt()
      normalise.sampling.dt()
    },

    get.quantile.dt = function(at.quantile = 0.5) {
      "Gives the quantiles at all time periods, at at.quantile (e.g. 0.5 for median)."
      result = sampling.dt[,
                           daohtools:::quantile.dist(x = .SD[,values],
                                                     prob = .SD[,weights],
                                                     quantile = at.quantile),
                           by=.(time.begin,time.end)][order(time.begin)]
      data.table::setnames(result, old = 'V1', new = paste0('q',at.quantile*100))
      return(result)
    }
  )
)
