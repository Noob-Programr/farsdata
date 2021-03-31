
#' @title Read a Accidents Data File.
#'
#' @description The data files feature US National Highway Traffic Safety Administration's Fatality Analysis
#'              Reporting System, which is a nationwide census providing the American public yearly data
#'              regarding fatal injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename The filename of the dataset to be imported.
#'
#' @return A dataframe or object of class `tbl_df`.
#' @note An error is thrown if a file with the given filename doesn't exists.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' fars_read(accident_2015.csv.bz2)
#'
#' fars_read(accident_2020.csv.bz2) # shows an error as such a file doesn't exists.
#'
#' }
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' @title Create a Filename.
#'
#' @description A function that reproduces the name of the file that consists of the data on accidents for the given year.
#'
#' @param year The year for which the records are desired.
#'
#' @return A string that denotes the name of the data file.
#'
#' @note It is recommended to take the input as 2013, 2014 or 2015 since data beyond these 3 years is unavailble.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' make_filename(2013)  # Input is numeric
#' make_filename("2015")  # Input is a string
#' make_filename(as.double(2018))  # Input is of `double` data type.
#' }
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title Extract Month and Year of Accidents Data.
#'
#' @description The function actually takes data of all the concerned years into account and extracts the
#'              month and year of each accident, collects them and stores them in a dataframe
#'
#' @param years The list or vector of years.
#'
#' @return An object of class `data.frame` or `tibble` or `tbl_df`.
#'
#' @note An error message is displayed if there is no data file corressponding to some input year.
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' fars_read_years(list(2013, 2014, 2015))  # Input is a list
#'
#' fars_read_years(c(2013, 2014, 2015))   # Input is a vector or a list
#'
#' }
#'
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


#' @title Summarize the Data by Month and Year.
#'
#' @description A function to show the distribution of fatalities in motor vehicle accidents over month and year
#'              of occurrence in USA in a tabulated format.
#'
#' @param years A vector or list of years.
#'
#' @return An object of class `data.frame` or `tibble` or `tbl_df`.
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' fars_summarize_years(list(2013, 2014, 2015))  # Input is a list
#'
#' fars_summarize_years(c(2013, 2014, 2015))   # Input is a vector or a list
#'
#' fars_summarize_years(list(2013, 2014, 2020)) # Error raised as 2020 is not a valid year for input.
#'
#' }
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title Plot the Accidents Data.
#'
#' @description The function takes a particular state and year into account and plots the corresponding locations
#'              of casualties on a map of USA. The `state.num` feature of the data ranges from 1 to 56.
#'
#' @param state.num An integer denoting the state to plot for.
#' @param year The year for which the plot is required.
#'
#' @return Plot of the cleaned data on a US map.
#'
#' @note An error is raised if the state idenification number (`state.num`) is invalid.
#' @note No plot is returned if number of accidents is zero in the specified month and year.
#'
#' @import maps
#' @import graphics
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(48, 2013)
#' fars_map_state(56, 2014)
#' fars_map_state(82, 2013) # error is raised as no data for the specified state number exists.
#' }
#'
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
