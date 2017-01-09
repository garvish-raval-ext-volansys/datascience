
#' Imports CSV file and convert it to A Dataframe table
#'
#' This is simple function to import csv data and return dataframe
#' table for data operations. Function will fail if filename does't
#' exist in working directory.
#'
#' @param filename A charecter string to provide filename
#'
#' @return This function returns Dataframe table of imported CSV data
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates filename based on provided Year
#'
#' This function is used to generate filename from provided year in
#' argument. Accident Data is organized per year and compressed in different
#' files with specific pattern. This function simplifized user input by just
#' providing Year and compose filename.
#'
#' @param year A integer or character string of Year
#'
#' @return
#' This functio nreturns charecter string of Filename
#'
#' @examples
#' make_filename("2015")
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Reads data from file for provided years data
#' and retrun array of data table for each year.
#'
#' This is simple function to create dataset for single or multiple
#' year for which data file is available. Data table columns are
#' for Month of Year and Year for individual row in data set of Year.
#'
#' @param years A charector string or Integer or vector of Years to get dataset
#'
#' @return Array of Datatable for provided years in arguments.
#'  array element will represent individual year's data in data table.
#'  Function will return NULL in case of year datafile not found
#'  in working environment or error in processing datafile.
#'
#' @examples
#' fars_read_years(2013)
#' fars_read_years("2013")
#' fars_read_years(c(2013,2014,2015))
#' fars_read_years(c("2013","2014","2015"))
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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

#' Sumurize number of accidents per month for all provided years in data table.
#'
#' This function is used to sumarize number of rows per month for individual
#' year as column and month as row format data table. This function will read
#' data table for each year in array and then bind them all togather and converts
#' multiple data frame into single data table and provides summary for each month.
#'
#' @param years A charector string or Integer or vector of Years to get dataset
#'
#' @return Returs data table for number of accidents for each year where yeras are
#'  columns and row represents month number. Function will throw warning if data
#'  for individual year not found or error in processing individual year's data.
#'
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years("2013")
#' fars_summarize_years(c(2013,2014,2015))
#' fars_summarize_years(c("2013","2014","2015"))
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plots number of accidents as points for selected State and Year
#'
#' This function draws accident data for state and accidents are
#' represented as points and state boundry is displayed by line.
#' Location of accident is identified by range of Longitude and latitude but NA
#' values are ignored to pricise points of accidents.
#'
#' @param state.num A charecter string or Integer of state number of USA
#' @param year A character string or Integer of Year
#'
#' @return Plots graph displaying accidents as points for selected state and year
#'  Function will throw error incase of state number not found in dataset for
#'  selected year.
#'
#' @examples
#' fars_map_state(5,2015)
#' fars_map_state(5,"2015")
#' fars_map_state("5",2015)
#' fars_map_state("5","2015")
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
