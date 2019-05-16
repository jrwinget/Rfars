#' Read a delimited file
#'
#' This function reads a file in a tibble format and creates a data frame from
#' it. In the data frame, cases correspond to rows and variables to columns in
#' the file.
#'
#' @param filename A character string giving the path to a file.
#'
#' @details First, the function will check if the file named by the argument
#'    exists. If it does not, an error will be thrown indicating the specified
#'    file does not exist. If the file name is valid, the function reads the
#'    data and converts it into tibble format.
#'
#' @return This function returns a tibble of the specified data file. Using a
#'    missing or invalid value for a file or path name will result in an error.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(system.file("extdata", filename, package = "Rfars")))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(system.file("extdata", filename, package = "Rfars"))
  })
  dplyr::tbl_df(data)
}

#' Create a file name
#'
#' This function makes a file name by entering a year. You can specify the year
#' by using the \code{year} argument.
#'
#' @param year A character or integer vector specifying the year(s)
#'    (YYYY format).
#'
#' @details First, the function turns the year input into a integer, creating a
#'    numerical vector. The function then returns a character vector containing
#'    a formatted combination of text and variable values to specific a valid
#'    data file name for the desired year.
#'
#' @return This function returns a character string with the correct file name
#'    for the specified year.
#'
#' @examples
#' make_filename(2013)
#' make_filename(c(2014, 2015))
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read data for specified year(s)
#'
#' This function reads a file for a specified year, uses the year to create a
#' column name, selects the year and month columns, and returns a list of the
#' resulting data frames.
#'
#' @param years A vector specifying the year(s) (YYYY format).
#'
#' @details This function takes the year input and iterates it over another
#'    function that selects and wrangles the data for a given year.
#'    Inside this second function, \link{make_filename} is called to create a
#'    data file name for the desired year. Then, \link{fars_read} is called to
#'    import the data, which is then processed such that the year of the
#'    associated data is given to the year column and only that column and month
#'    are selected. If a year is chosen that is not correspond to a valid file
#'    name, an error will be thrown indicating an invalid year was specified.
#'
#' @return This function returns a list of the data and selects the month and
#'    year columns.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2014, 2015))
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

#' Read data for specified year(s) and combine into a data frame
#'
#' This function reads a file for a specified year, uses the year to create a
#' column name, selects the year and month columns, and converts the list into
#' a single data frame in tibble format.
#'
#' @inheritParams fars_read_years
#'
#' @details This function takes the year input, reads in the data using
#'    \link{fars_read_years}, and stores the resulting data frames into a list.
#'    It then combines those data frames into a single tibble, the number of
#'    cases there are of each.
#'
#' @return This function returns a tibble for the data and selects the month and
#'    year columns.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years(c(2014, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot the geographic location of US highway fatalities
#'
#' This function plots the geographic location of US highway fatalities for a
#' given year and state. You can adjust the plot by state (using the
#' \code{state.num} argument) and year (using the \code{year} argument).
#'
#' @param state.num A character string giving the text the function will print.
#' @param year A character string or integer specifying a year (YYYY format).
#'
#' @details First, the function calls \link{make_filename} to create a valid
#'    file name format for the given year and then calls \link{fars_read} to
#'    read in the data based on the created file name. The function then checks
#'    to see if the state number selects is in the data. If it is not, an error
#'    is thrown indicating an invalid STATE number was entered. If the state
#'    number is valid but there are no rows of data for that state number, then
#'    a message stating there are "no accidents to report" is returned. If there
#'    are data for the valid state number, the function then creates a plot of
#'    the state selected and plots the highway fatalities as points on the map.
#'
#' @return This function returns a graph of the state with points representing
#'    the geographic location of highway fatalities.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(1, 2015)
#' fars_map_state(5, 2013)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
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
