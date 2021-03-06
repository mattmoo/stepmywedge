#' Construct a statistic distribution generated by permZuting sites to different
#' clusters.
#'
#' Statistic is Wilcoxon-Mann-Whitney at the moment.
#'
#' @param max.r How many permutations?
#' @param outcome.col.name Name of the column containing the outcome
#' @param intervention.col.name NAme of the column containing the intervention
#'   (i.e. has levels "control" and "intervention")
#' @param stat.per.site If TRUE, the statistic will be calculated separately for
#'   each site on each permutation, if not site will be ignored. MAy not be a
#'   good idea to use TRUE
#' @param statistic Can be WMWU or ANOVA.
#' @param sort.input Will sort the input by outcome (by reference), which slightly speeds ranking/
#' @param ... Passed on to the test
#' @return A data.table with the statistic value at each permutation (with zero
#'   as the unpermuted comparison). May be divided by site if requested.
#' @export
generate.stat.dt = function(max.r,
                            outcome.col.name,
                            intervention.col.name,
                            data.dt,
                            cluster.dt,
                            study.dt,
                            perm.dt = NULL,
                            stat.per.site = F,
                            statistic = 'WMWU',
                            other.predictors = NULL,
                            sort.input = T,
                            progress.bar = T,
                            ...) {

  #It will probably streamline the calculations if I precalculate the intervention
  #and control groups for each site for each cluster (i.e. sequence).
  #This could fail for very large data sets.
  hypothetical.data.dt = data.table::data.table(expand.grid(site = study.dt[,levels(site)],cluster = study.dt[,levels(cluster)]))
  #This could be vectorised, but won't be the most compute-heavy part anyway.

  #Iterate through the different assignments of site to clusters, making tables of
  #what the groups would look like.

  #No per site calculations for ANOVA
  #It could probably be implemented, but it might not be a good idea.
  if (statistic == 'ANOVA') {
    stat.per.site = F
  }

  if (sort.input == T) {
    setorder(data.dt, outcome)
  }

  #Make a wee progress bar.
  message(paste0("Calculating ", nrow(hypothetical.data.dt), " hypothetical site data tables..."))
  if (progress.bar == T) {
    pb = txtProgressBar(style = 3, char = '|')
  }
  t = Sys.time()

  for (row.ind in 1:nrow(hypothetical.data.dt)) {
    #Make a table of what the groups would look like if that site was in that cluster.
    site.cluster.dt = data.dt[site == hypothetical.data.dt[row.ind, site],c("time","site",outcome.col.name), with=F]
    data.table::set(x = site.cluster.dt,
                    j = "cluster",
                    value = hypothetical.data.dt[row.ind, cluster])
    #Chuck on the time markers.
    site.cluster.dt = data.table:::merge.data.table(site.cluster.dt, cluster.dt, by = "cluster")
    #Calculate which group the subject is in.
    data.table::set(x = site.cluster.dt,
                    j = intervention.col.name,
                    value = "control")
    data.table::set(x = site.cluster.dt,
                    i = which(site.cluster.dt[,time>=transition.start.time]),
                    j = intervention.col.name,
                    value = "transition")
    data.table::set(x = site.cluster.dt,
                    i = which(site.cluster.dt[,time>=intervention.start.time]),
                    j = intervention.col.name,
                    value = "intervention")
    data.table::set(x = site.cluster.dt,
                    j = intervention.col.name,
                    value = factor(site.cluster.dt[,get(intervention.col.name)],
                                   levels = c("control",
                                              "transition",
                                              "intervention")))

    #Add that table to the hypothetical data.
    data.table::set(x = hypothetical.data.dt,
                    i = row.ind,
                    j = "data.dt",
                    value = list(list(site.cluster.dt)))

    #Update progress
    if (progress.bar == T) {
      setTxtProgressBar(pb, row.ind/nrow(hypothetical.data.dt))
    }
  }

  if (progress.bar == T) {
    close(pb)
  }
  elapsed = Sys.time()-t
  message(paste("Time elapsed: "),hms::as.hms(elapsed))
  message("")

  #Now let's figure out how to permute things.
  if (is.null(perm.dt)) {
    perm.dt = generate.perm.dt(study.dt, max.r = max.r)
  } else if ((ncol(perm.dt)-1) != max.r) {
    message(paste0("Number of permutations in perm.dt: ", (ncol(perm.dt)-1)))
    message(paste0("Number of requested permutations: ", max.r))
    if ((ncol(perm.dt)-1) > max.r) {
      message(paste0("First ", max.r, " permutations will be used."))
    } else {
      message(paste0("max.r changed to "), (ncol(perm.dt)-1))
      max.r = (ncol(perm.dt)-1)
    }
  }
  #Set up a table for stats from all permutations, differs in dimension
  #depending on whether you want one stat per site or not.
  if (!stat.per.site) {
    stat.dt = data.table::data.table(perm.num = 0:max.r,
                                     stat = NA_real_)
  } else {
    stat.dt = data.table::data.table(perm.num = rep(0:max.r,
                                                    times = 1,
                                                    each = nrow(study.dt)),
                                     site = rep(study.dt$site, times = max.r+1),
                                     stat = NA_real_)
  }

  #Flag original data in permutations
  data.table::set(x = stat.dt,
                  j = 'permuted',
                  value = factor(stat.dt$perm.num != 0))

  #Make a wee progress bar.
  message(paste0("Calculating ", max.r, " permutations..."))

  if (progress.bar == T) {
    pb = txtProgressBar(style = 3, char = '|')
  }
  next.progress = 0
  t = Sys.time()

  #Formula for the stats
  # form = as.formula(paste(outcome.col.name,intervention.col.name, sep = ' ~ '))
  form = as.formula(paste(outcome.col.name,'~', paste(c(intervention.col.name, other.predictors), collapse = '+')))

  # for (perm.ind in 0:max.r) {
  perform.permutation.step = function(perm.ind) {
    t2 = Sys.time()
    #Permutation 0 is the actual data.
    if (perm.ind>0) {
      #Rename the relevant permutation's column to pick it out.
      old.col.name = names(perm.dt)[perm.ind+1]
      data.table::setnames(x = perm.dt,
                           old = perm.ind+1,
                           new = "cluster.perm")
      #Shuffle the clusters between sites.
      perm.data.dt = data.table::rbindlist(
        data.table:::merge.data.table(
          x = perm.dt[, .(site, cluster.perm)],
          y = hypothetical.data.dt,
          by.x = c("site", "cluster.perm"),
          by.y = c("site", "cluster")
        )$data.dt
        #Make sure to remove transition!
      )[get(intervention.col.name) != "transition"]
    } else {
      perm.data.dt = data.dt[get(intervention.col.name) != "transition"]
    }
    # message(c('t1_d = ', Sys.time() - t2))

    #Put the statistic in the table we setup.
    if (statistic == 'ANOVA') {
      stat = anova(test.wilcoxon.ANOVA.form(data.dt = perm.data.dt, form = form))$`F value`[1]
      # data.table::set(x = stat.dt,
      #                 i = which(stat.dt[,perm.num == perm.ind]),
      #                 j = "stat",
      #                 value = anova(test.wilcoxon.ANOVA.form(data.dt = perm.data.dt, form = form))$`F value`[1])

    } else if (statistic == 'WMWU') { #WMWMU
      if (!stat.per.site) {
        # message(c('t2_d = ', Sys.time() - t2))
        # data.table::set(x = stat.dt,
        #                 i = which(stat.dt[,perm.num == perm.ind]),
        #                 j = "stat",
        #                 value = coin::statistic(coin::wilcox_test(form, data = perm.data.dt)))
        stat = coin::statistic(coin::wilcox_test(form, data = perm.data.dt))
        # message(c('t3_d = ', Sys.time() - t2))
      } else {
        # data.table::set(x = stat.dt,
        #                 i = which(stat.dt[,perm.num == perm.ind]),
        #                 j = "stat",
        #                 value = perm.data.dt[,.(stat = coin::statistic(coin::wilcox_test(form, .SD, exact=F))), by = site][,stat])
        stat = perm.data.dt[,.(stat = coin::statistic(coin::wilcox_test(form, .SD, exact=F))), by = site][,stat]
      }
    } else if (statistic == 'WMWU.DT') { #WMWMU
      # if (!stat.per.site) {
        # message(c('t2_d = ', Sys.time() - t2))

      # data.table::set(x = stat.dt,
      #                   i = which(stat.dt[,perm.num == perm.ind]),
      #                   j = "stat",
      #                   value =  test.wilcox.dt(perm.data.dt)$z)
      stat = test.wilcox.dt(perm.data.dt)$z
        # message(c('t3_d = ', Sys.time() - t2))
      # }
    } else {
      stop(paste(statistic, 'is not a supported statistic'))
    }


    #Permutation 0 is the actual data.
    if (perm.ind>0) {
      #Reset name to what it was before
      data.table::setnames(x = perm.dt,
                           old = perm.ind+1,
                           new = old.col.name)
    }

    # message(c('t4_d = ', Sys.time() - t2))

    #Update progress (max 100 times)

    if (progress.bar == T) {
      if (max.r<=100) {
        setTxtProgressBar(pb, perm.ind/max.r)
      } else {
        if (perm.ind/max.r >= next.progress) {
          setTxtProgressBar(pb, perm.ind/max.r)
          next.progress = next.progress + 0.01
        }
      }
    }

    return(stat)
  }

  # result = lapply(0L:max.r, perform.permutation.step)
  # print(result)
  data.table::set(x = stat.dt,
                  j = "stat",
                  value = unlist(mclapply(0L:max.r, perform.permutation.step)))

  # stat.dt = cbind(stat.dt, rbindlist(result))

  if (progress.bar == T) {
    close(pb)
  }
  elapsed = Sys.time()-t
  message(paste("Time elapsed: "),hms::as.hms(elapsed))
  message("")

  #Also give it a z-transform
  data.table::set(x = stat.dt,
                  j = "z",
                  value = scale(stat.dt$stat, center = T, scale = T))

  return(stat.dt)
}

#' Do a wilcox test on a data.table.
#'
#' @param data.dt data.table with two-level grouping factor 'group' and outcome 'outcome''
#' @param two.sided Do a two sided test?
#' @param tie.correction Apply tie correction (slightly slower, but sometimes necessary). If NULL, tie correction will be applied if there are.
#' @param sort.input Sorts the input by reference, this enables faster ranking (i.e. speeds permutation test)
#' @return A data.table containing statistics, including z score and theoretical p value (e.g. z.dt$z).
#'
#' @export
test.wilcox.dt = function(data.dt,
                          two.sided = T,
                          tie.correction = NULL,
                          sort.input = F) {

  #What proportion of values should be unique to apply tie correction (if not
  #explicitly enabled/disabled)
  tie.correction.threshold = .9

  t = Sys.time()

  if (sort.input) {
    setorder(data.dt, outcome)
  }
  if (is.null(tie.correction)) {
    tie.correction = length(data.dt[,unique(outcome)]) < nrow(data.dt)*tie.correction.threshold
  }

  # message(paste('t0 = ', Sys.time()-t ))

  #Do WMW
  #Assign ranks to outcomes
  data.dt[, rank := frank(outcome, ties.method = 'average')]


  #Make a data.table with rank sums and n per group.
  wmw.dt = data.dt[,.(R = sum(rank), n = .N), by = group]


  #Calculate U for each group
  data.table::set(x = wmw.dt,
                  j = 'u',
                  value = wmw.dt[,R - (n*(n+1))/2])


  #Get minimum U, n for each group, and also theoretical mean.
  z.dt = cbind(wmw.dt[u == min(u), .(n.a=as.numeric(n), u = u)],
               wmw.dt[u == max(u), .(n.b=as.numeric(n))],
               wmw.dt[,.(m.u = prod(n)/2)])

  #Calculate theoretical SD, correcting for errors if requested.
  if (tie.correction == F) {
    z.dt[,sd.u := wmw.dt[,sqrt(prod(n)*(sum(n)+1)/12)]]
  } else {
    z.dt[,sd.u := sqrt((n.a*n.b/((n.a+n.b)*(n.a+n.b-1))) * ((((n.a+n.b)^3-(n.a+n.b))/12)-sum(data.dt[,(.N^3-.N)/12, by = outcome])))]
  }


  #Calculate z score for U
  data.table::set(x = z.dt,
                  j = 'z',
                  value = z.dt[,(u - m.u)/sd.u])


  data.table::set(x = z.dt,
                  j = 'p',
                  value = z.dt[,pnorm(z) * (1+two.sided)])

  return(z.dt)
}

#' Generate a table that holds a number of ways in which to permute sites to
#' different clusters.
#'
#' @param study.dt A study info data.table, containing site, cluster, and timing info.
#' @param max.r Number of permutations.
#' @return A data.table containing all the permutations of site to cluster.
#'
#' @export
generate.perm.dt = function(study.dt, max.r = 1000) {

  #Find out roughly how many computations are possible.
  max.poss.r = factorial(nrow(study.dt))

  #Adjust max iterations if necessary.
  if (max.poss.r < max.r) {
    max.r = max.poss.r
  }

  #If there aren't heaps more possible iterations than the number requested,
  if (max.r*10 > max.poss.r) {
    #Get all permutations
    perm.dt = transpose(do.call(data.table,combinat::permn(study.dt$cluster)))
    #Get a random sample of the number that you really want.
    perm.dt = perm.dt[sample(x = 1:nrow(perm.dt), size = max.r)]
  } else {
    #If there are heaps of possible permutations, get a random sample without ensuring that they're unique, it's probably good enough.
    perm.dt = transpose(do.call(data.table,lapply(rep(1,max.r), function(n) sample(study.dt$cluster))))
  }

  return(cbind(data.table(site = study.dt$site), transpose(perm.dt)))
}

#' Applies a few tests to a statistic table, generated by generate.stat.dt I
#' don't think this is as good as an overall wilcoxon
#'
#' @param stat.dt A data.table of the statistic with minimally columns for stat,
#'   permuted, and I guess site if you want that in the plot
#' @return A list with a few tests.
#'
#' @export
test.stat.table.per.site = function(stat.dt) {

  plot = ggplot2::ggplot(data=stat.dt,ggplot2::aes(x=stat, y = ..density..))


  if ('site' %in% colnames(stat.dt)) {
    plot = plot +
      ggplot2::geom_density(ggplot2::aes(fill=factor(site)), adjust=1.5, alpha = 0.5, position = "stack")

  }

  tests = list(
    tt = t.test(stat ~ permuted, stat.dt),
    it = coin::independence_test(stat ~ permuted, stat.dt, distribution = "asymptotic"),
    wt = coin::wilcox_test(stat ~ permuted, stat.dt),
    ot = coin::oneway_test(stat ~ permuted, stat.dt))

  return(list(tests = tests,
              plot = plot))
}

#' Applies a few tests to a statistic table, generated by generate.stat.dt
#'
#' @param stat.dt A data.table of the statistic with minimally columns for stat,
#'   permuted.
#' @return A list with only a p-value currently.
#'
#' @export
test.stat.table = function(stat.dt) {

  result = list(
    p = which(stat.dt[order(-abs(stat)),stat] == stat.dt[1,stat])/nrow(stat.dt)
  )

}


#' Applies a two-way ANOVA to Wilcoxon ranks
#'
#' @param data.dt A data.table with results from each participant, an outcome, a
#'   group (i.e. effect of interest), and a time
#' @return Results of the two-way ANOVA.
#'
#' @export
test.wilcoxon.ANOVA = function(data.dt, outcome.col.name = 'outcome', intervention.col.name = 'group', other.predictors = 'time') {

  form = as.formula(paste(outcome.col.name,'~',intervention.col.name, ' + ', paste(other.predictors, collapse = '+')))
  return(test.wilcoxon.ANOVA.form(data = data.dt, form = form))

}

#' Applies a two-way ANOVA to Wilcoxon ranks
#'
#' @param data.dt A data.table with results from each participant, an outcome, a
#'   group (i.e. effect of interest), and a time
#' @return Results of the two-way ANOVA.
#'
#' @export
test.wilcoxon.ANOVA.form = function(data.dt, form) {

  return(aov(form, data = data.dt))

}
