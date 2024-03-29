# based on supermag_api.py by S. Antunes
# Created by Hongyi (Ethan) Hu from The Overlake School.
# Created on: May 11, 2022
# Last modified: July 31, 2022
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# The best way to report issues would be on the GitHub repository. see
# <https://github.com/PythonOrC/SuperMAG-R-API/issues>

# TODO: 3. better documentation
# TODO: 4. add support for other data types
# TODO: 5. better error handling
# TODO: 6. add more potential user input error checking

library(jsonlite)
library(httr)

sm_coreurl <- function(page, logon, start, extent) {
    # internal helper
    # forms a url with page, start, logon, and extent
    # extent <- formatExtent(extent)
    url <- paste("https://supermag.jhuapl.edu/services/", page, "?start=", start, "&logon=", logon, "&extent=", extent, sep = "")
    # print("debug:",urlstr)
    return(url)
}

formatExtent <- function(extent) {
    # internal helper method that converts extent to 12 digit string
    while (nchar(extent) < 12) {
        extent <- paste("0", extent, sep = "")
    }
    return(extent)
}

sm_csvitem_to_list <- function(myarr) {
    # ! untested, but should work
    # internal helper method that converts a csv item to a list
    # myarr is a list of strings
    # returns a list of lists
    # print("debug:",myarr)
    mylist <- list()
    for (i in 1:length(myarr)) {
        mylist[[i]] <- strsplit(myarr[[i]], ",")[[1]]
    }
    return(mylist)
}

sm_csvitem_to_dict <- function(myarr) {
    # ! untested, currently same as sm_csvitem_to_list
    # internal helper method that converts a csv item to a dictionary
    # myarr is a list of strings
    # returns a list of dictionaries
    # print("debug:",myarr)
    mydict <- list()
    for (i in 1:length(myarr)) {
        mydict[[i]] <- strsplit(myarr[[i]], ",")[[1]]
    }
    return(mydict)
}

sm_parsestart <- function(start) {
    # internal helper method that converts a list to string
    if (typeof(start) == "list") {
        for (i in 2:length(start)) {
            if (nchar(start[[i]]) != 2) {
                start[[i]] <- paste("0", start[[i]], sep = "")
            }
        }
        parsedstart <- paste(start[[1]], "-", start[[2]], "-", start[[3]], "T", start[[4]], ":", start[[5]], sep = "")
    }
    return(parsedstart)
}

sm_dateToYMDHMS <- function(tval, yr, mo, dy, hr, mt, sc) {
    #* unused
}

sm_keycheck_data <- function(flagstring) {
    # internal helper method that checks the flagstring for valid optional keys
    # returns a parsed string of keys
    keys <- list("mlt", "mag", "geo", "decl", "sza")
    user_keys <- strsplit(flagstring, ",")[[1]]
    parsed_keys <- str("")

    # check if keyword "all" exist in flagstring
    # if so, add everything
    # if not, check for individual keywords
    if (is.element("all", user_keys)) {
        parsed_keys <- paste("&", keys, sep = "", collapse = "&")
    } else {
        for (i in 1:length(user_keys)) {
            if (is.element(user_keys[[i]], keys)) {
                parsed_keys <- paste(parsed_keys, user_keys[[i]], sep = "&")
            }
        }
    }
    # check if keyword "baseline" is in flagstring
    if (is.element("baseline=yearly", user_keys)) {
        parsed_keys <- paste(parsed_keys, "&baseline=yearly", sep = "")
    } else if (is.element("baseline=none", user_keys)) {
        parsed_keys <- paste(parsed_keys, "&baseline=none", sep = "")
    }
    # check if keyword "delta" is in flagstring
    if (is.element("delta=start", user_keys)) {
        parsed_keys <- paste(parsed_keys, "&delta=start", sep = "")
    }
    return(parsed_keys)
}

sm_keycheck_indices <- function(flagstring) {
    # internal helper method that checks the flagstring for valid indices keyword
    # returns a parsed string of indices
    user_keys <- strsplit(flagstring, ",")[[1]]
    basekey <- list("sme", "sml", "smu", "mlat", "mlt", "glat", "glon", "stid", "num")
    pluskey <- list("smr", "ltsmr", "ltnum", "nsmr")
    swi <- list("pdyn", "epsilon", "newell", "clockgse", "clockgsm", "density")
    imf <- list("bgse", "bgsm", "vgse", "vgsm")
    indices <- "&indices="
    swi <- "&swi="
    imf <- "&imf="
    # check if keyword "all" exist in flagstring
    # if so, add everything
    # if not, check for individual keywords
    if ("all" %in% user_keys | "indicesall" %in% user_keys) {
        indices <- paste(indices, "all,", sep = "")
    } else {
        # add individually
        # check for each category
        if ("baseall" %in% user_keys) {
            indices <- paste(indices, paste(basekey, sep = "", collapse = ","), ",", sep = "")
        }
        if ("plusall" %in% user_keys) {
            indices <- paste(indices, paste(pluskey, sep = "", collapse = ","), ",", sep = "")
        }
        if ("sunall" %in% user_keys) {
            indices <- paste(indices, paste(basekey, sep = "", collapse = "s,"), "s,", sep = "")
        }
        if ("darkall" %in% user_keys) {
            indices <- paste(indices, paste(basekey, sep = "", collapse = "d,"), "d,", sep = "")
        }
        if ("regall" %in% user_keys | "regionalall" %in% user_keys) {
            indices <- paste(indices, paste(basekey, sep = "", collapse = "r,"), "r,", sep = "")
        }
        for (u in user_keys) {
            if (u %in% pluskey) {
                indices <- paste(indices, u, ",", sep = "")
                next
            }
            for (b in basekey) {
                if (u == b) {
                    indices <- paste(indices, b, ",", sep = "")
                } else if (u == paste(b, "s", sep = "") | u == paste("sun", b, sep = "")) {
                    indices <- paste(indices, b, "s,", sep = "")
                } else if (u == paste(b, "d", sep = "") | u == paste("dark", b, sep = "")) {
                    indices <- paste(indices, b, "d,", sep = "")
                } else if (u %in% paste(b, "r", sep = "") | u %in% paste(list("regional", "reg"), b, sep = "")) {
                    indices <- paste(indices, b, "r,", sep = "")
                }
            }
        }
    }

    # check if keyword "swi" exist in flagstring
    if ("swiall" %in% user_keys) {
        swi <- paste(swi, "all,", sep = "")
    } else {
        for (u in user_keys) {
            if (u %in% swi) {
                swi <- paste(swi, u, ",", sep = "")
            }
        }
    }


    # check if keyword "imf" exist in flagstring
    if ("imfall" %in% user_keys) {
        imf <- paste(imf, "all,", sep = "")
    } else {
        for (u in user_keys) {
            if (u %in% imf) {
                imf <- paste(imf, u, ",", sep = "")
            }
        }
    }


    # clean up the indices
    if (indices == "&indices=") {
        indices <- ""
    } else {
        indices <- substr(indices, 1, nchar(indices) - 1)
    }

    if (swi == "&swi=") {
        swi <- ""
    } else {
        swi <- substr(swi, 1, nchar(swi) - 1)
    }

    if (imf == "&imf=") {
        imf <- ""
    } else {
        imf <- substr(imf, 1, nchar(imf) - 1)
    }

    return(paste(indices, swi, imf, sep = ""))
}

sm_geturl <- function(fetchurl, fetch) {
    if (missing(fetch)) {
        fetch <- "raw"
    }

    success <- 0 # gets changed to 1 good data is fetched
    mydata <- "ERROR: Unknown error" # prepare for the worst
    # If the url object throws an error it will be caught here
    tryCatch(
        {
            # print(fetchurl)
            fetched <- content(GET(fetchurl))

            if (fetch == "raw") {
                mydata <- strsplit(fetched, "\n")[[1]]

                mydata <- mydata[-2]
            } else if (fetch == "json") {
                mydata <- format_to_json(fetched)
                # } else if (fetch == "xml") {
                #     mydata <- fromXML(fetched)
            } else {
                mydata <- "ERROR: Unknown fetch type"
            }
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

format_to_json <- function(raw) {

    # remove the "OK" and "[]" from the start of the string
    data <- substr(raw, 5, nchar(raw) - 2)

    # split the long list of json into individual json strings
    data <- strsplit(data, ",\n")[[1]]
    parsed_data <- unlist(fromJSON(data[1]))
    for (i in 2:length(data)) {
        parsed_data <- Map(c, parsed_data, unlist(fromJSON(data[i])))
    }
    return(parsed_data)
}

SuperMAGGetInventory <- function(logon, start, extent) {
    url <- sm_coreurl("inventory.php", logon, start, extent)
    content <- sm_geturl(url)
    status <- content[1]
    stations <- content[2]

    # print(status)
    if (status == 1) {
        # print(length(stations[[1]]))
        if (length(stations[[1]]) > 1) {
            stations <- stations[[1]]

            stations <- stations[2:length(stations)]
            print(stations)
            return(list("status" = 1, "stations" = stations))
        }
    }
    return(list("status" = 0, "stations" = "No stations found"))
}

SuperMAGGetIndices <- function(logon, start, extent, flagstring) {
    indices <- sm_keycheck_indices(flagstring)
    url <- paste(sm_coreurl("indices.php", logon, start, extent), indices, sep = "")
    content <- sm_geturl(url, "json")
    status <- content[1]
    data <- content[[2]]
    if (status == 1) {
        return(list("status" = 1, "indices" = data))
    }
    return(list("status" = 0, "indices" = "No data found"))
}

SuperMAGGetData <- function(logon, start, extent, flagstring, station) {
    optional_flag <- sm_keycheck_data(flagstring)
    url <- paste(sm_coreurl("data-api.php", logon, start, extent), "&station=", station, optional_flag, sep = "")
    content <- sm_geturl(url, "json")
    status <- content[1]
    data <- content[2]
    # print(data)
    if (status == 1) {
        return(content)
    }
    return(list("status" = status, "data" = "No data found"))
}


# The Following are a few examnples uses of the functions above
# print(sm_coreurl('inventory.php', "", "2003-10-29T00:00", "3600"))
# time <- list(2003, 10, 29, 0, 0)
# print(sm_parsestart(time))
# print(sm_keycheck_data("all,baseline=none,delta=start"))
# sm_keycheck_indices("all,imfall,swiall")
# print(sm_geturl("https://supermag.jhuapl.edu/services/data-api.php?logon=&station=VIC&start=2003-10-29T00:00&extent=3600", fetch = "json"))
# print(SuperMAGGetData("USERNAME", "2003-10-29T00:00", "3600", "all,baseline=none,delta=start", "VIC"))
# print(SuperMAGGetIndices("USERNAME", "2003-10-29T00:00", "3600", "baseall,imfall,swiall"))
# content <- SuperMAGGetInventory("USERNAME", "2003-10-29T00:00", "3600")
# print(content$stations)
# status = 0
# data = ""
# list(status, data) <- content