##' @title Pull the curent number of R projects from CRAN, RForge, Bioconductor and GitHub.
##'
##' @description Using curl queries and parsing, the function pulls the current number of R projects from the 4 main repositories: CRAN, RForge, Bioconductor and GitHub.
##' @author Marcin Kierczak <\email{marcin.kierczak@@scilifelab.se}>
##' @return Named vector with the date and the number of packages in all 4 repos.

interrogate_repos <- function() {
  # Interrogate CRAN
  cran_con <- curl::curl(url = 'https://cran.r-project.org/web/packages/index.html')
  cran <- paste0(readLines(con = cran_con, n = 20), collapse = '')
  cran_pkgs <- stringr::str_match(cran, 'repository features ([0-9]{1,}) available packages')[2]

  # Interrogate R-Forge
  rforge_con <- curl::curl(url = 'https://r-forge.r-project.org')
  rforge <- paste0(readLines(con = rforge_con, n = 200), collapse = '')
  rforge_pkgs <- stringr::str_replace(stringr::str_match(rforge, 'Projects: <strong>([0-9]{1,},[0-9]{1,})</strong>')[2],
                                      ',',
                                      '')

  # Interrogate Bioconductor
  bioconductor_con <- curl::curl(url = 'https://bioconductor.org')
  bioconductor <- paste0(readLines(con = bioconductor_con, n = 200), collapse = '')
  bioconductor_pkgs <- stringr::str_match(bioconductor, 'Software\">([0-9]{1,}).*software packages.')[2]

  # Interrogate GitHub
  github_con <- curl::curl(url = 'https://github.com/search?l=R&q=R+package&type=Repositories')
  github <- paste0(readLines(con = github_con, n = 2400, ), collapse = '')
  github_pkgs <- stringr::str_replace(stringr::str_match(github, '([0-9]{1,},[0-9]{1,}) repository results')[2],
                                      ',',
                                      '')

  day <- format(Sys.time(), "%Y-%m-%d")
  result <- c(day, cran_pkgs, rforge_pkgs, bioconductor_pkgs, github_pkgs)
  names(result) <- c('date', 'CRAN', 'R-Forge', 'Bioconductor', 'GitHub')
  return(result)
}
