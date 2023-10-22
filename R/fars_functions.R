#' @importFrom readr read_csv
#' @importFrom magrittr "%>%"

#' Check filename and load data
#'
#' This function check if a csv table exist. If the file exists it saves it
#' as a data.frame. The file will be loaded using the \code{\link{read_csv}}
#' function from the \code{readr} package. In addition it shows a progress bar
#' due to the \code{progress} argument in \code{\link{read_csv}}
#'
#' @param filename A path to a csv file.
#'
#' @return This function returns a \code{\link{tibble}}
#'
#' @examples
#' fars_read("foo.csv")
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv2(filename, progress = FALSE)
  })
  tibble::tibble(data)
}

#' Make a file's name from a year
#'
#' This is an internal function to make a file's name with a year in the current
#' working directory.
#'
#' @param year a year in 4-digit format.
#'
#' @return This function returns a chracter value that correspond with the prefix
#' 'accident_' the code{year}, and the extension '.csv.bz2'
#'
#' @examples
#' make_filename(2013)
#'
#' @export
#'
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}

#' Validate the years
#'
#' This function check if each element in a vector of years makes a valid filename. A valid
#' name is one that existis in the current working directory.
#'
#' @details To check the filename, this function tries to make a simple \code{\link{tibble}} using
#' tools from the dplyr package. It is important to check the extension of the file that needs to correspond with
#' the one that is created with the \code{\link{make_filename}} function.
#'
#' @param years a vector or list of years in a 4-digit format. Each one of the years will be
#' used to create a file name with the \code{\link{make_filename}} function. Then it will be tested
#' to see if it is a valid filename.
#'
#' @return This function returns a list with NULL values and a warning in case of finding
#' an invalid year.
#'
#' @examples
#' fars_read_years(c(1999, 2013, 2022))
#'
#' @export

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarise many years
#'
#' This functions takes each year in a vector and makes a summary of the number of observations
#' for every month of that year.
#'
#' @param years a vector or list of years in a 4-digit format. Then it will be tested
#' to see if it is a valid filename with the \code{\link{fars_read_years}} function and then
#' make a summary table iwth all of them.
#'
#' @return This function \code{\link{tibble}} where every row is a month of the year and from
#' the second to the last column are the years. The cells are the frequency.
#'
#' @examples
#' fars_summarize_years(c(1999, 2013, 2022))
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    tidyr::spread(year, n)
}

#' Plot the accident cases in a map
#'
#' This generates a map where it shows every accident case in a state. To do this, the function
#' uses a year to load a table with accidents and check if there were accidents in that state and
#' year.
#'
#' @param state.num The number of a state that must exist in the data of the year selected in the param
#' \code{year}. If not the function will throw an error.
#'
#' @param year A year to load the accident data from that year. This file must existe in the current
#' working directory.
#'
#' @return This function returns a map of the selected State with points on the places where an accident
#' occur.
#'
#' @examples
#' fars_map_state(1, 2013)
#'
#'\dontrun{
#' # This does not work because the state 3 is not existent in the data.
#'fars_map_state(3, 2013)
#'}
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE))) stop("invalid STATE number: ", state.num)

  data.sub <- dplyr::filter(data, STATE == state.num)

  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90

  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}


