library(jsonlite)

sm_coreurl <- function(page, logon, start, extent) {
    # internal helper
    # forms a url with page, start, logon, and extent
    extent <- formatExtent(extent)
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
    if (missing(fetch)) {
        fetch <- "raw"
    }

    success <- 0 # gets changed to 1 good data is fetched
    mydata <- "ERROR: Unknown error" # prepare for the worst
    # If the url object throws an error it will be caught here
    tryCatch(
        {
            print(fetchurl)
            fetched <- content(GET(fetchurl))
            
            if (fetch == "raw") {
                mydata <- fetched
            } else if (fetch == "json") {
                mydata <- fromJSON(fetched)
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



# print(sm_coreurl('inventory.php', "SoonerThanLater", "2003-10-29T00:00", "3600"))
# time <- list(2003, 10, 29, 0, 0)
# print(sm_parsestart(time))
# print(sm_keycheck_data("all,baseline=none,delta=start"))
# sm_keycheck_indices("all,imfall,swiall")
print(sm_geturl("https://supermag.jhuapl.edu/services/data-api.php?start=2003-10-29T00:00&logon=SoonerThanLater_&extent=000000003600&station=VIC", fetch = "raw"))