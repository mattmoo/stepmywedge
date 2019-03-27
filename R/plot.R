#' Plot a stepped wedge design.
#'
#' Has facets for sites and clusters.
#'
#' @param dataDT Data from the SW study
#' @param studyDT Data about the SW study
#' @param ylims Limits of y-axis
#' @param outcomeName What outcome to plot
#' @param dotSize Size of points in scatter
#' @param scatterColVar What variable to colour the points on.
#' @return The plot
#' @export
faceted.line.plot.dt = function(dataDT,
                               studyDT,
                               studyParams = NULL,
                               ylims = NULL,
                               outcomeName = 'outcome.05',
                               dotSize = 2,
                               scatterColVar = c('site','day')[[1]]) {

  # p1 = ggplot2::ggplot(data=dataDT, aes_string(x='day', y=outcomeName, color='site')) +
  #   scale_fill_brewer(palette="Set1")

  # print(studyParams)

  if (scatterColVar == 'site') {
    p1 = ggplot2::ggplot(data=dataDT, aes_string(x='day', y=outcomeName, color='site')) +
      scale_fill_brewer(palette="Set1")
  } else if (scatterColVar == 'day') {
    p1 = ggplot2::ggplot(data=dataDT, aes_string(x='day', y=outcomeName, color='day')) +
      ggplot2::scale_colour_gradient2(low = '#ac402a', mid = '#269c38', high='#2a2aac', midpoint=540)
  } else {
    error(paste(scatterColVar, 'is not a valid variable for faceting (should be site or day)'))
  }
  p1 = p1 +
    ggplot2::geom_point(alpha=.3, size = dotSize) +
    ggplot2::facet_grid(sequence ~ .) +
    ggplot2::geom_line(aes(y=runquantile(dataDT[,get(outcomeName)], k = 1.5*studyParams$stepLengthDays, probs=.5)), colour='black',alpha=0.5) +
    ggplot2::scale_y_continuous(limits = ylims, breaks = seq(from = 0, to = 100, by = 5), minor_breaks = NULL) +
    ggplot2::theme(legend.position="none",
          panel.grid.minor = element_blank()) +
    ggplot2::labs(x = 'Day', y='Patient outcome')

  if (!is.null(studyParams)) {
    p1 = p1 +
      ggplot2::geom_vline(aes(xintercept = interveneAtDay), data=studyDT, colour = 'red', alpha = .4) +
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = studyParams$nSteps*studyParams$stepLengthDays, by = stepLengthDays), minor_breaks = NULL)

  }

  return(p1)
}
