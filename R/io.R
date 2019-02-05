get_temp <- function() {
    return (tempfile())
}

get_temp_jkl <- function() {
    return (get_temp(".jkl"))
}

get_temp_dat <- function() {
    return (get_temp(".dat"))
}

get_temp_arff <- function() {
    return (get_temp(".arff"))
}

get_temp_res <- function() {
    return (get_temp(".res"))
}

get_temp_net <- function() {
    return (get_temp(".net"))
}


get_temp <- function(ext) {
    return (tempfile(fileext = ext))
}


#' Read a Jkl file (parent sets cache) 
#' @export
read.jkl <- function(path, data) {
    check_data(data)

#     print(path)
    con  <- file(path, open = "r")
    ln <- readLines(con, n=1)
    while (grepl("^[#]", ln, perl=T))
        ln <- readLines(con, n=1)
    n <- strtoi(ln)
    ln <- readLines(con, n=1)
    while (grepl("^[#]", ln, perl=T))
        ln <- readLines(con, n=1)
    jkl <- list()
    for (i in 1:n) {
        ln <- strsplit(ln, " ", fixed=T)[[1]]
        np <- strtoi(ln[2])
        psets <- character(np)
        for (j in 1:np) {
            ln <- readLines(con, n=1)
            ln <- strsplit(ln, " ", fixed=T)[[1]]
            sc = ln[1]
            sz = ln[2]
            pset = ln[-c(1,2)]
            au <- paste(sapply(pset, function(z) colnames(data)[strtoi(z)+1]), collapse=", ")
            au <- paste(c(sc, " # ", au, ""), collapse="")
            psets[j] <- au[1]
        }
       jkl[[i]] <- list(name=colnames(data)[i], psets=psets)
        ln <- readLines(con, n=1)
    }

close(con)
    return(jkl)
}

#' Write a Jkl file (parent sets cache) 
#' @export
write.jkl <- function(path, jkl, data) {
    check_data(data)

    con  <- file(path, open = "w")
    n <- length(jkl)
    cnames <- colnames(data)
    writeLines(as.character(n), con)
    for (i in 1:n) {
        psets <- jkl[[i]]$psets
        np <- length(psets)
        writeLines(paste(c(i-1, np), collapse=" "), con)
        
        for (j in 1:np) {
            ps <- psets[j]
            ln <- strsplit(ps, " # ", fixed=T)[[1]]
            if (!is.na(ln[2])) {
            pars <- strsplit(ln[2], ", ", fixed=T)[[1]]
            npars <- length(pars)            
            au <- match(pars, cnames)
            au <- lapply(au, function(x) x-1)
        } else {
            npars <- 0
         au  <- ""
     }
            writeLines(paste(c(ln[1], npars, au), collapse=" "), con)
        }
    }

close(con)
}


write.jkl.names <- function(path, jkl) {

   con  <- file(path, open = "w")
    n <- length(jkl)

    writeLines(as.character(n), con)
    for (i in 1:n) {
        nm <- jkl[[i]]$name
        psets <- jkl[[i]]$psets
        np <- length(psets)
        writeLines(paste(nm, np, sep=" "), con)
        
        for (j in 1:np) {
            ps <- psets[j]
            writeLines(ps, con)
        }
    }

close(con)
}


#' Read a res file for bnlearn loading
#' @export
read.str <- function(path, data) {
    check_data(data)

    con <- file(path, open = "r")
    n <- ncol(data)
    
    str <- ""
    for (i in 1:n) {
        ln <- readLines(con, n=1)
        ln <- strsplit(ln, ":", fixed=T)[[1]]
        v <- strtoi(ln[1])
        str <- paste(str, "[",colnames(data)[v+1], sep="")

        ps <- strsplit(ln[2], "(", fixed=T)[[1]]

        if (length(ps) > 1) {
            str <- paste(str, "|", sep="")
            ps <- strsplit(ps[2], ")", fixed=T)[[1]]
            ps <- strsplit(ps[1], ",", fixed=T)[[1]]

            c <- sapply (ps, function(x) colnames(data)[strtoi(x)+1])

            c <- paste(c, sep="", collapse=":")
            
            str <- paste(str, c, sep="")
        }

        str <- paste(str, "]", sep="")
    }

close(con)
    return(str)
}

