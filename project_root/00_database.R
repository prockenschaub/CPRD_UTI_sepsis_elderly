#--------------------------------------------------------------------------
#
# Program: 00_database.R
# Author:  Patrick Rockenschaub
# Date:    14/11/2017
#
# Purpose: Create functions that deal with database interaction silently
#          in the background using credentials from a .txt file
#
#--------------------------------------------------------------------------

load_if_missing <- function(packages, lib.loc = NULL){
  # Load packages if they have not been loaded before
  #
  # Args:
  #   packages - the packages to load
  #   lib.loc  - the location of the packages, passed on unaltered to
  #              require()
  #
  # Result:
  #   A named logical vector indicating whether the package could be 
  #   loaded.
  
  missing <- which(!vapply(packages, isNamespaceLoaded, logical(1)))
  vapply(names(missing), require, logical(1), 
         character.only = TRUE, lib.loc = lib.loc)
}


init_db <- function(config_file, lib.loc = NULL){
  # Load all libraries and parameters needed to connect to database
  #
  # Args:
  #   config_file - .txt file containing the database connection parameters
  #     For example:  driver={SQL Server}
  #                   server=sqlserver.idhs.ucl.ac.uk
  #                   database=DATABASE123
  #                   trusted_connection=true
  #   lib.loc     - path to database packages folder. If NULL, the default
  #                 package search path will be used. The default path is 
  #                 always used for general packages (magrittr, tibble,
  #                 dplyr, readr). Preload these packages if they are located
  #                 in a different location.
  #
  # Result:
  #   NULL
  
  # Load the required libraries for database connections
  db_libs <- c("Rcpp", "odbc")
  load_if_missing(db_libs, lib.loc = lib.loc)
  
  # Load other libraries needed if they have not been loaded already
  other_libs <- c("tibble", "magrittr", "dplyr", "readr")
  load_if_missing(other_libs)
  
  # Import the configuration file
  config_file %>% 
    read_tsv(col_names = "params", col_types = "c") %$% 
    params %>% 
    assign(x = ".db_params", envir = globalenv())
}


connect_db <- function(){
  # Connect to the database using parameters pre-loaded by init_db().
  #
  # Args:
  #
  # Result:
  #   NULL
  
  if(!exists(".db_params", envir = globalenv())){
    stop("No database parameter have been specified. Run init_db() first.")
  }
  
  get(".db_params", envir = globalenv()) %>% 
    paste(collapse = ";") %>% 
    dbConnect(odbc::odbc(), .connection_string = .) %>% 
    assign(x = ".conn", envir = globalenv())
  
  assign(as.numeric(Sys.time()), x = ".conn_id", envir = globalenv())
}


disconnect_db <- function(){
  # Disconnect the current database connection
  #
  # Args:
  #
  # Result:
  #   NULL
  
  .conn_id <- NULL
  
  dbDisconnect(get_db_conn())
}


get_db_conn <- function(){
  # Return a previously established database connection object
  #
  # Args:
  #
  # Result:
  #   OdbcConnection object with an open connection
  
  if(!exists(".conn", envir = globalenv())){
    stop("No connection has been established yet. Run connect_db() first.")
  }
  
  get(".conn", envir = globalenv())
}


get_db_conn_id <- function(){
  # Get a unique id for the current connection.
  # 
  # Args:
  #
  # Result:
  #   An identifier number for the current connection
  
  if(!exists(".conn", envir = globalenv())){
    stop("No connection has been established yet. Run connect_db() first.")
  }
  
  get(".conn_id", envir = globalenv())
}


connected <- function(){
  # Return true if a valid database connection is established
  #
  # Args:
  #
  # Result:
  #   Logical vector
  
  odbc::dbIsValid(get_db_conn())
}


check_connection <- function(){
  # Throw an error if no active database connection exists.
  #
  # Args:
  #
  # Result:
  #
  
  if(!connected()){
    stop(bl("No valid database connection is established.", "
            Run connect_db() first"))
  }
}


remove_all_tables <- function(pattern){
  # Drop all database tables whose name matches the pattern
  #
  # Args:
  #   pattern - a regex defining the tables to be dropped
  #
  # Result:
  #   NULL
  
  odbc::dbListTables(get_db_conn()) %>% 
    .[grep(pattern, ., ignore.case = TRUE)] %>% 
    walk(odbc::dbRemoveTable, conn = get_db_conn())
}


is_date_char <- function(x){
  # Check whether a character vector contains dates. This is needed,
  # for `collect_dt()`, as the Microsoft SQL driver returns dates as
  # characters.
  #
  # Args: 
  #   x - any vector
  #
  # Result:
  #   TRUE if the input was a character vector and contained dates
  
  if(!is.character(x)){
    return(FALSE)
  }
    
  tryCatch(suppressWarnings(is.Date(as_date(x))),
           error = function(err) { FALSE })
}


collect_dt <- function(..., convert = FALSE){
  # Wrapper for collect() that returns a data.table instead of a tbl.
  #
  # Args:
  #   ...     - passed on to collect()
  #   convert - if TRUE, auto-convert all dates from character to date
  #             Uses the first 1000 rows to guess dates
  #
  # Result:
  #   the results of collect converted to data.table
  
  dt <- as.data.table(collect(...))
  
  # Conversion needed for some because dates are returned as characters
  if(convert){
    dates <- which(map_lgl(dt, ~is_date_char(.[1:min(nrow(dt), 10000)])))
    
    # Convert all dates to date
    dt[, (dates) := lapply(.SD, ymd), .SDcols = dates] 
  }
  
  dt[]
}



collect_mem <- function(sql_obj){
  # Wrapper for collect_dt() that stores the query results in a local
  # cache and loads the results from cache if the SQL query has not 
  # since changed. 
  #
  # Args: 
  #   sql_obj - a tbl_sql object containing the query to collect
  #
  # Result:
  #   a new or cached result from collect_dt()
  
  if(!inherits(sql_obj, "tbl_sql")){
    stop("sql_obj must have class tbl_sql.")
  }

  # dbplyr adds random temporary table names for its subqueries,
  # which have to be removed to create a persisent hash name
  unq_query <- 
    as.character(sql_render(sql_obj)) %>% 
    str_replace_all("\\) \"[a-z]{10}\"", "\\)")
  cache_name <- digest::digest(unq_query)
  
  simpleCache::simpleCache(cache_name, { collect_dt(sql_obj) })
}


compute_nm <- function(x, ...){
  # wrapper for compute to use the passed object's name as the 
  # temporary table name in the database
  #
  # See compute for parameters
  
  
  nm <- deparse(substitute(x))
  compute(x, nm, ...)
}





# Convenience functions ---------------------------------------------------

escape_date <- function(x){
  # Escape R dates for database use
  #
  # Args:
  #   x - an R vector
  #
  # Result:
  #   a character vector with escaped dates 
  
  ifelse(is.Date(x), paste0("'", x, "'"), x)
}


iif_db <- function(x, y, operator){
  # Return the string representing an IIF comparison in TRANSACT SQL
  #
  # Args:
  #   x - vector of length one
  #   y - vector of lenght one
  #   operator - the comparison to be carried out
  #
  # Result:
  #   The SQL command as a string
  
  x <- escape_date(x)
  y <- escape_date(y)
  
  sprintf("IIF(%s %s %s, %s, %s)", x, operator, y, x, y)
}


max_db <- function(...){
  # Return the string representing a two or more column MAX comparison
  #
  # Args:
  #   ... - vectors of length 1 containing the dates
  #
  # Result:
  #   The SQL command as a string
  
  args <- list(...)
  reduce(args, iif_db, operator = ">")
}


min_db <- function(...){
  # Return the string representing a two or more column MIN comparison
  #
  # Args:
  #   ... - vectors of length 1 containing the dates
  #
  # Result:
  #   The SQL command as a string
  
  args <- list(...)
  reduce(args, iif_db, operator = "<")
}


year_add_db <- function(date, increment){
  # Add a certain number of years to a date in the database
  #
  # Args:
  #   date - a date to increment
  #   increment - the number of years to add
  #
  # Result:
  #   sql code for the added date as string (TRANSACT SQL)
  
  sprintf("DATEADD(year, %d, %s)", increment, escape_date(date))
}


day_add_db <- function(date, increment){
  # Add a certain number of days to a date in the database
  #
  # Args:
  #   date - a date to increment
  #   increment - the number of days to add
  #
  # Result:
  #   sql code for the added date as string (TRANSACT SQL)
  
  sprintf("DATEADD(day, %d, %s)", increment, escape_date(date))
}




