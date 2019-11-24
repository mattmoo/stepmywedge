#' Create a stepped wedge experiment structure.
#'
#' @param nClusters How many clusters?
#' @param sitesPerCluster SHow many sites per cluster?
#' @param timePerStep How much time per step?
#' @param transitionDuration Study structure represented using classes from this package
#' @return Stepped wedge experiment
#' @export
generate.trial = function(nClusters, sitesPerCluster, timePerStep, transitionDuration = 0) {
  result = SW.Study()
  siteNumber = 1
  interventionStep = 1

  nSteps = nClusters + 1
  cluster.end.time = nSteps * timePerStep

  for (clusterInd in 1:nClusters) {
    fmat = paste0('%0',floor(log10(nClusters))+1,'d')
    #Make cluster
    cluster = SW.Cluster(name = paste0('cluster',sprintf(fmat,clusterInd)),
                         cluster.start.time = 0,
                         transition.start.time = clusterInd * timePerStep,
                         intervention.start.time = clusterInd * timePerStep + transitionDuration,
                         cluster.end.time = cluster.end.time)

    #Make sites associated with a cluster
    for (siteInd in 1:sitesPerCluster) {
      fmat = paste0('%0',floor(log10(nClusters*sitesPerCluster))+1,'d')
      cluster$add.sites(SW.Site(name = paste0('site',sprintf(fmat,siteNumber))))
      siteNumber = siteNumber + 1
    }

    #Add sites to cluster
    result$add.clusters(cluster)
  }
  return(result)
}
