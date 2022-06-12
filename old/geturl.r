library(httr)

sm_coreurl <- function(page, logon, start, extent) {
  # internal helper function
  # start <- formatStart(start)
  extent <- formatExtent(extent)
  url <- paste("https://supermag.jhuapl.edu/services/", page, "?start=", start, "&logon=", logon, "&extent=", extent, sep = "")

  # print("debug:",urlstr)

  return(url)
}

formatExtent <- function(extent) {
  while (nchar(extent) < 12) {
    extent <- paste("0", extent, sep = "")
  }
  return(extent)
}
# url <- generateUrl("data-api.php", "2003-10-29T00:00", "3600", "SoonerThanLater_", "VIC")
# print(url)

sm_GetUrl <- function(fetchurl) {

  # internal helper function
  # returned data choices are 'raw' or 'json', default is 'raw'
  # converts an http bytestream into a python list (raw) or list of dict (json)
  # 'stations' should be 'raw' and returns a list
  # 'data' should be 'json', returns a list (which converts to a dataframe)
  # 'indices' should be 'json', returns a list (which converts to a dataframe)
  success <- 0 # gets changed to 1 good data is fetched
  mydata <- "ERROR: Unknown error" # prepare for the worst
  # print("debug: url trying ",fetch,"is",fetchurl)
  # If the url object throws an error it will be caught here
  tryCatch(
    {
      print(fetchurl)
      mydata <- content(GET(fetchurl), as = "parsed")
      success <- 1
    },
    error = function(e) {
      print(e)
      mydata <- e$message
      success <- 0
    },
    warning = function(w) {
      print(w)
      mydata <- w$message
      success <- 0
    }
  )
  # print("debug: function return type is:",type(mydata),".")
  return(list("status" = success, "data" = mydata))
}

SuperMAGGetData <- function(logon, start, extent, flagstring, station) {
  # One of the core 3 functions
  # optional options for 'data':
  # ALL=&mlt&mag&geo&decl&sza
  # MLT=&mlt,MAG=&mag,GEO=&geo,DECL=&decl,SZA=&sza,
  # DELTA='start',BASELINE='none/yearly'
  # e.g. can pass  MLT=1,MAG=1  and they will be evaluated.  Full set checked: ALL, MLT, MAG, GEO, DECL, SZA, also values for DELTA, BASELINE
  # also arg FORMAT='list', otherwise defaults to FORMAT='dataframe'  NOT YET DONE!!!!

  # default FORMAT='dataframe', alt is FORMAT='list'

  url <- sm_coreurl("data-api.php", logon, start, extent)
  # indices <- sm_keycheck_data(flagstring)
  # url <- paste(url, indices, "&station=", station.upper())
  url <- paste(url, "&station=", station, sep = "")

  result <- sm_GetUrl(url)

  status <- result$status
  data_list <- result$data
  print(status)
  print(data_list)
}

# SuperMAGGetData("SoonerThanLater_","2003-10-29T00:00", "3600", "", "VIC")