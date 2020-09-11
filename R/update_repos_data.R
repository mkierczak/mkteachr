##' @title Update the dataset with the number of available packages.
##' @author Marcin Kierczak <\email{marcin.kierczak@@scilifelab.se}>
##' @description Queries all repos and pulls the current number of packages.
##' Then loads the old data and adds the new numbers to it. If the path to a dataset is specified,
##' the old data is loaded from that file and next, file is overwritten so that the new data is added.
##' Otherwise the built-in dataset is used as the historical data and the new data will not be saved.
##' @param path_to_dataset - specifies the path to the file where the historical data is stored.
##' If not given, internal package dataset will be used but it will not be updated.
##' @return tibble (long-format) containing historical data and the new figures from today.
##' @export
update_repos_data <- function(path_to_dataset = NULL) {
  require(magrittr)
  write_flag = T
  if (is.null(path_to_dataset)) {
    path_to_dataset <- system.file("extdata", "num_pkgs_data.csv", package = "mkteachr", mustWork = TRUE)
    write_flag = F
  }
  prev_data <- readr::read_csv(path_to_dataset)
  today <- interrogate_repos()
  if (dplyr::last(prev_data$date) < today[1]) {
    prev_data <- rbind(prev_data, today)
    if (write_flag) {
      print(paste0('Writing updated dataset to ', path_to_dataset, '...'))
      readr::write_csv(x = prev_data, path_to_dataset)
    }
  }
  # Transform the data to the long format
  data <- prev_data %>%
          tidyr::pivot_longer(cols = c(cran, rforge, bioconductor, github),
                       names_to = 'repo') %>%
          dplyr::mutate(value = as.integer(value))
return(data)
}
