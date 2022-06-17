# based on supermag_api.py by S. Antunes
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
    # !requires testing, finished but untested
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
    # ! requires testing as supermag is down when the time of writing
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


    if (status == 1) {
        # print(length(stations[[1]]))
        if (length(stations[[1]]) > 1) {
            stations <- stations[[1]]
            stations <- stations[2:length(stations)]
            return(list("status" = 1, "data" = stations))
        }
    }
    return(list("status" = 0, "data" = "No stations found"))
}

SuperMAGGetIndices <- function(logon, start, extent, flagstring) {
    indices <- sm_keycheck_indices(flagstring)
    url <- paste(sm_coreurl("indices.php", logon, start, extent), indices, sep = "")
    content <- sm_geturl(url, "json")
    status <- content[1]
    data <- content[2]
    if (status == 1) {
        return(list("status" = 1, "data" = data))
    }
    return(list("status" = 0, "data" = "No data found"))
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

# print(sm_coreurl('inventory.php', "SoonerThanLater", "2003-10-29T00:00", "3600"))
# time <- list(2003, 10, 29, 0, 0)
# print(sm_parsestart(time))
# print(sm_keycheck_data("all,baseline=none,delta=start"))
# sm_keycheck_indices("all,imfall,swiall")
# print(sm_geturl("https://supermag.jhuapl.edu/services/data-api.php?logon=SoonerThanLater_&station=VIC&start=2003-10-29T00:00&extent=3600", fetch = "json"))
# print(format_to_json("OK\n[{\"tval\":1067385600.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -32.032387, \"geo\": -31.587579}, \"E\": {\"nez\": 3.914707, \"geo\": -6.604813}, \"Z\": {\"nez\": 30.236118, \"geo\": 30.236118}},\n{\"tval\":1067385660.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -29.978230, \"geo\": -29.734411}, \"E\": {\"nez\": 4.199440, \"geo\": -5.674006}, \"Z\": {\"nez\": 31.153442, \"geo\": 31.153442}},\n{\"tval\":1067385720.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -26.973866, \"geo\": -26.878542}, \"E\": {\"nez\": 4.163980, \"geo\": -4.740480}, \"Z\": {\"nez\": 32.070774, \"geo\": 32.070774}},\n{\"tval\":1067385780.000000, \"ext\": 60.000000, \"iaga\": \"VIC\", \"N\": {\"nez\": -22.710659, \"geo\": -22.015141}, \"E\": {\"nez\": 1.594514, \"geo\": -5.800867}, \"Z\": {\"nez\": 33.988182, \"geo\": 33.988182}}]\n"))
# print(SuperMAGGetData("SoonerThanLater_", "2003-10-29T00:00", "3600", "all,baseline=none,delta=start", "VIC"))
# print(SuperMAGGetIndices("SoonerThanLater_", "2003-10-29T00:00", "3600", "baseall,imfall,swiall"))
# print(SuperMAGGetInventory("SoonerThanLater_", "2003-10-29T00:00", "3600"))