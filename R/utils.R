cache <- new.env()

#' Bayesian Learning Package - Main function.
#'
#' Used by most of the functions in the r.blip binding, provides access to the included jar file. 
#'
#' The arguments vector is formatted in a system call to the included jar file. 
#' Should not be called directly by the user, unless you know exactly what you are doing. 
#' In that case, call directly the blip jar. 
#' @param args Vector of arguments to be passed to the jar
blip <- function(args){

    if (!exists('blip_path', envir=cache)){
        assign('blip_path', find_blip(), envir=cache)
    }
    
    # print( find_blip())
    # print(args)

    system2("java", c("-jar", get('blip_path', envir=cache), args), wait=T)

}

find_blip <- function(){

    for (l in .libPaths()) {
        if(grepl('r.blip$', l)) {
            return (comb_path(l))
        }
    
        path <- file.path(l, "r.blip")
        if (dir.exists(path)) {
            return (comb_path(path))
        }
    }
}

comb_path <- function(path) {

    path <- file.path(path, "java")
    path <- file.path(path, "blip.jar")
    return(path)
}


isSingleString <- function(input) {
    is.character(input) & length(input) == 1
}

alphanumeric <- function(x) {
 if (!isSingleString(x))
    return(T);
 return(grepl("^[A-Za-z0-9 -;]+$", x, perl=T))
}

illegal <- function(x) {
 if (!isSingleString(x))
    return(T);
 return(grepl("[,]", x, perl=T))
}

#' @export      
check_data <- function(x) {

 if(!is.data.frame(x))
    stop("The argument is required to be a data.frame.")

if(nrow(x) == 0)
    stop("The argument data frame is empty.")   
    
    x[] <- lapply(x, factor)      

f <- sapply(colnames(x), illegal)
 if (any(f == T))  
    stop("One of the col names of the given data frame contains illegal characters (usually a comma). ") 
    
 for(xr in sapply(x, levels)) {
    f <- sapply(xr, illegal)
 if (any(f == T))  
    stop("One of the value of the given data frame contains illegal characters (usually a comma). ") 
}
    
 f <- sapply(x, is.infinite)
 if (any(f == T))  
    stop("No 'Inf' values are allowed in the argument data frame.")         
    
#f <- sapply(colnames(x), alphanumeric)
# if (any(f == F))  
#    stop("Only alphanumerical characters are allowed in the row names of the argument data frame.") 

# f <- apply(x, c(1, 2), alphanumeric)
# if (any(f == F))  
#    stop("Only alphanumerical characters are allowed in the values of the argument data frame.")       

# f <- sapply(x, is.numeric)
# if (any(f == T))  
#    stop("No numeric columns are allowed in the argument data frame.")    
 
# f <- sapply(x, is.na)
# if (any(f == T))  
#    stop("No missing data are allowed in the argument data frame.")   

}

check_arg <- function (x, vals) {

 if(!(x %in% vals)) 
    stop("No valid value for the given argument.")

}

check_int <- function (x, min, max) {
    
 if (!is.numeric(x))
    stop("The given argument is not a number.")

 if (x%%1!=0)
    stop("The given argument is not an integer.")
    
 if (x < min || x > max)
    stop("No valid value for the given argument.")

}

check_float <- function (x, min, max) {
    
 if (!is.numeric(x))
    stop("The given argument is not a number.")
    
 if (x < min || x > max)
    stop("No valid value for the given argument.")

}
