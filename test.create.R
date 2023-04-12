# library(data.table)
# library(ggplot2)
# library(caTools)
#
# source('R/I.Sampler.R')
# source('R/Timed.Sampling.Dist.R')
# source('R/SW.Cluster.R')
# source('R/SW.Site.R')
# source('R/SW.Trial.R')

# smw.dir = '.'
# devtools::document(pkg = smw.dir)
# devtools::install(pkg = smw.dir, dependencies = F, reload = T)
library(stepmywedge)

set.seed(1)

#Testing a timed sampling distribution
# tsd = new('Timed.Sampling.Dist')
# testDist = data.table(values = c(1,2,3),
#                       weights = c(2,1,1),
#                       time.begin = 1,
#                       time.end = 3)
# tsd$set.sampling.dt(testDist)
#
# tsd$add.sampling.dist(values = c(4,5,6),
#                       weights = c(2,1,1),
#                       time.begin = 2,
#                       time.end = 4)
# tsd$get.value()

intervention.col.name = 'group'
outcome.col.name = 'outcome'


trial01 = generate.trial(nClusters = 5,
                         sitesPerCluster = 2,
                         timePerStep = 90,
                         transitionDuration = 15)
trial01$generate.study.dt()
trial01$set.sim.parameters(sim.ppt.per.unit.time.mean = 1.7,
                           sim.ppt.per.unit.time.sd = .2,
                           sim.normal.preintervention.mean = 15,
                           sim.normal.intervention.effect.mean = .75,
                           sim.normal.intervention.effect.sd = .6,
                           sim.site.effect.mean = 0,
                           sim.site.effect.sd = 2,
                           sim.time.effect.per.unit.mean = 0,
                           sim.time.effect.per.unit.sd = 0.005,
                           sim.site.effect.force.sign = c(-1,0,+1)[3],
                           sim.individual.noise.mean = .5,
                           sim.individual.noise.sd = .1)
trial01$generate.site.sim.parameters()
trial01$generate.sim.data.normal(save.intermediates = FALSE)

# a = trial01$data.dt[group != 'transition',.(stat = coin::statistic(coin::wilcox_test(outcome ~ group, .SD, exact=F))), by = site][,stat]
# b = coin::statistic(coin::wilcox_test(outcome ~ group, data = trial01$data.dt[group != 'transition']))

# perm.data.dt = trial01$data.dt[group != 'transition']

# c = perm.data.dt[, mean(get(outcome.col.name)), by = c('site', intervention.col.name)
#                  ][, diff(V1), by = site
#                    ][, V1]
# c = perm.data.dt[group != 'transition',.(stat = coin::statistic(coin::wilcox_test(outcome ~ group, .SD, exact=F))), by = site][,stat]
# b = coin::statistic(coin::wilcox_test(outcome ~ group, data = trial01$data.dt[group != 'transition']))
trial01$generate.stat.dt(1000, statistic = 'mean_diff')
trial01$generate.stat.dt(1000, statistic = 'WMWU')


p = trial01$stat.dt[permuted == TRUE, mean(stat <= trial01$stat.dt[permuted == 'FALSE', stat])]

# t = test.stat.table.per.site(trial01$stat.dt)


# p = trial01$faceted.line.plot(outcome.name = 'outcome')
#
# plot(p)

# swt.test(dataDT = trial01$data.dt,
#          studyDT = trial01$study.dt,
#          max.r = 10,
#          outcomeColName = "outcome",
#          interventionColName = "group")


# data.dt = copy(trial01$data.dt)
# study.dt = copy(trial01$study.dt)
# cluster.dt = copy(trial01$cluster.dt)
# max.r = 50
# outcome.col.name = "outcome"
# intervention.col.name = "group"
# stat.per.site = F
#
#
# perm.dt = generate.perm.table(study.dt, max.r = max.r)
#
#
# stat.dt = construct.stat.dt(max.r = max.r,
#                             outcome.col.name = outcome.col.name,
#                             intervention.col.name = intervention.col.name,
#                             stat.per.site = F,
#                             perm.dt = perm.dt)
# # tests2 = perform.stat.table.stats.per.site(stat.dt = stat.dt)
# tests = perform.stat.table.stats(stat.dt)
# # wilcox_test()


