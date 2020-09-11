##' @title Plot the repositories data in glamorous way!
##' @author Marcin Kierczak <\email{marcin.kierczak@@scilifelab.se}>
##'
##' @description Plots the historical and the current data using tibble returned by the update_repos_data function.
##' Style of the plot is inspired by William Chase's lecture 'Glamour of graphics' at rstdio::rconf2020.
##' @param data -- long-format tibble as returned by the update_repos_data function.
##' @return a ggplot2 plot object.
##' @export

plot_repos_data <- function(data) {
  require(magrittr)
  require(ggplot2)
  jojo1 <- c('#c0b3aa', '#2b2117', '#9a8268', '#d3a677', '#e1d0ba')
  jojo2 <- c('#221c17', '#947e66', '#da9869', '#e0c6a4', '#c1b3ab')

  cols <- c('bioconductor' = jojo2[2], 'cran' = jojo2[3], 'github' = jojo2[1], 'rforge' = jojo2[5])
  colors = c('black', cols[2], 'black', cols[4], 'black', cols[1], 'black', cols[3], 'black')

  title = glue::glue('Log(number) of R packages available at',
             ' <span style="color:{cols[2]}">**CRAN**</span>,',
             ' <span style="color:{cols[4]}">**R-Forge**</span>,',
             ' <span style="color:{cols[1]}">**Bioconductor**</span>',
             ' and',
             ' <span style="color:{cols[3]}">**GitHub**</span>',
             ' over time.')

  gg <- ggplot(data, mapping = aes(x = date, y = log(value), col = repo)) +
    geom_point(show.legend = F) +
    geom_line(show.legend = F) +
    ggrepel::geom_text_repel(aes(label = scales::comma(value)), direction = 'y', vjust = 0.5, show.legend = F) +
    scale_colour_manual(values = cols) +
    labs(title = title, x = '', y = '') +
    theme_minimal() +
    theme(panel.grid.major = element_line(colour = '#EEEEEE', size = 0.25),
        plot.background = element_rect(fill = '#FFFDF8', colour = '#FFFDF8'),
        axis.title = element_text(colour = jojo2[5]),
        axis.text = element_text(colour = jojo2[5], size = 10),
        axis.title.y = element_blank(),
        plot.title = ggtext::element_markdown(lineheight = 1.5, size = 12),
        plot.title.position = 'plot')
  return(gg)
}
