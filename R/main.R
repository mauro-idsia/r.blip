#' Learns a BN
#'
#' Fully learns a Bayesian networks. 
#'
#' The input data is required to be complete and discrete. 
#' Accordingly missing values in the input data.frame will be ignored, and all numeric values will be converted to integers. 
#' @param dat dataframe from which to learn the parent sets.(required)
#' @param scorer.method Method to be used for scoring the parent sets. Possible values: "is" (independence selection), "sq" (sequential selection). (default: is)
#' @param solver.method Method to be used for structure exploration. Possible values: "winasobs", "winobs", "asobs", "obs". (default: winasobs)
#' @param indeg Maximum number of parents (default: 6)
#' @param time Execution time (default: 3600)
#' @param allocated Percentage of the total execution time dedicated to parent set exploration (default: 80)
#' @param scorefunction Chosen score function. Possible choices: BIC, BDeu (default: bic)
#' @param alpha (if BDeu is chosen) equivalent sample size parameter (default: 1.0)
#' @param cores Number of machine cores to use. If 0, all are used. (default: 1)
#' @param verbose Verbose level (default: 0)
#' @return The learned Bayesian network in the bnlearn format.
#' @export
blip.learn <- function(dat, scorer.method="is", solver.method="winasobs", indeg=6, time=3600, allocated=80, scorefunction="bic", alpha=1.0, cores=1, verbose=0) {

    # CHECK ARGS
    check_data(dat)    

    check_arg(scorer.method, c("is", "sq"))

    check_arg(solver.method, c("winasobs", "winobs", "asobs", "obs"))
    
    check_int(indeg, 1, 99)
    
    check_int(time, 1, 999999)

    check_int(allocated, 10, 90)
    
    check_arg(scorefunction, c("bic", "bdeu", "k2"))
    
    check_int(cores, 0, 999999)
    
    check_int(verbose, 0, 5)

    # WRITE INPUT
    dat_path <- get_temp_arff()
    write.arff(dat, dat_path)
     cat("... writing arff file (temp file:", dat_path, ")\n")

    # EXECUTE PSE
    scorer.time = floor(time * allocated / 100.0 )
    jkl_path <- blip.scorer.int(dat_path, scorer.method, indeg, scorer.time, scorefunction, alpha, cores, verbose)
    
    jkl <- read.jkl(jkl_path, dat)
    loc <- get_temp_jkl()
    cat("... writing jkl complete (temp file:", loc, ")\n")
    write.jkl.names(loc, jkl)

    # EXECUTE SO
    solver.time = time - scorer.time
    res_path <- blip.solver.int(jkl_path, solver.method, solver.time, cores, verbose)

    # EXECUTE PL
    res <- read.str(res_path, dat)
    bn <- blip.parle(res, dat, alpha)

    return(bn)
        
}

#' Learns a BN with a treewidth bound
#'
#' Fully learns a Bayesian networks with a treewidth bound. 
#'
#' The input data is required to be complete and discrete. 
#' Accordingly missing values in the input data.frame will be ignored, and all numeric values will be converted to integers. 
#' @param dat dataframe from which to learn the parent sets.(required)
#' @param scorer.method Method to be used for scoring the parent sets. Possible values: "is" (independence selection), "sq" (sequential selection). (default: is)
#' @param solver.method Method to be used for bounded-treewidth structure exploration. Possible values: "kmax", "kg", "ka". (default: kmax)
#' @param treewidth Maximum treewidth (default: 4)
#' @param time Execution time (default: 3600)
#' @param allocated Percentage of the total execution time dedicated to parent set exploration (default: 80)
#' @param scorefunction Chosen score function. Possible choices: BIC, BDeu (default: bic)
#' @param alpha (if BDeu is chosen) equivalent sample size parameter (default: 1.0)
#' @param cores Number of machine cores to use. If 0, all are used. (default: 1)
#' @param verbose Verbose level (default: 0)
#' @return The learned Bayesian network in the bnlearn format.
#' @export
blip.learn.tw <- function(dat, scorer.method="is", solver.method="kmax", treewidth=4, time=3600, allocated=80, scorefunction="bic", alpha=1.0, cores=1, verbose=0) {

    # CHECK ARGS
    check_data(dat)    

    check_arg(scorer.method, c("is", "sq"))

    check_arg(solver.method, c("kmax", "kg", "ka"))
    
    check_int(treewidth, 1, 99)
    
    check_int(time, 1, 999999)

    check_int(allocated, 10, 90)
    
    check_arg(scorefunction, c("bic", "bdeu", "k2"))
    
    check_int(cores, 0, 999999)
    
    check_int(verbose, 0, 5)
    
    # WRITE INPUT
    dat_path <- get_temp_arff()
    write.arff(dat, dat_path)

    # EXECUTE PSE
    scorer.time = floor(time * allocated / 100.0 )
    jkl_path <- blip.scorer.int(dat_path, scorer.method, treewidth, scorer.time, scorefunction, alpha, cores, verbose)

    # EXECUTE SO
    solver.time = time - scorer.time
    res_path <- blip.solver.tw.int(jkl_path, solver.method, solver.time, treewidth, cores, verbose)

    # EXECUTE PL
    res <- read.str(res_path, dat)
    bn <- blip.parle(res, dat, alpha)

    return(bn)
        
}

#' Parent set exploration 
#'
#' Generates the cache of parent sets from a given data source
#'
#' Usually the first step in the learning of a Bayesian network.
#'
#' The input data is required to be complete and discrete. 
#' Accordingly missing values in the input data.frame will be ignored, and all numeric values will be converted to integers. 
#' @param dat dataframe from which to learn the parent sets.(required)
#' @param method Method to be used. Possible values: "is" (independence selection), "sq" (sequential selection). (default: is)
#' @param indeg Maximum number of parents (default: 6)
#' @param time Maximum Execution time (default: 3600)
#' @param scorefunction Chosen score function. Possible choices: BIC, BDeu (default: bic)
#' @param alpha (if BDeu is chosen) equivalent sample size parameter (default: 1.0)
#' @param cores Number of machine cores to use. If 0, all are used. (default: 1)
#' @param verbose Verbose level (default: 0)
#' @return Cache of parent sets 
#' @export
blip.scorer <- function(dat, method="is", indeg=6, time=3600, scorefunction="bic", alpha=1.0, cores=1, verbose=1) {

    # CHECK ARGS
    check_data(dat)    

    check_arg(method, c("is", "sq"))
    
    check_int(indeg, 1, 20)
    
    check_int(time, 1, 999999)
    
    check_arg(scorefunction, c("bic", "bdeu", "k2"))

    check_float(alpha, -100, 100)
    
    check_int(cores, 0, 999999)
    
    check_int(verbose, 0, 5)
    
    # WRITE INPUT
    dat_path <- get_temp_arff()

    write.arff(dat, dat_path)

    # EXECUTE PSE
    jkl_path <- blip.scorer.int(dat_path, method, indeg, time, scorefunction, alpha, cores, verbose)
    jkl <- read.jkl(jkl_path, dat)

    return(jkl)
}

#' @export
blip.scorer.int <- function(dat_path, method, indeg, time, scorefunction, alpha, cores, verbose) {

    jkl_path <- get_temp_jkl()

    cat("... executing PSE (temp file:", jkl_path, ")\n")

    args <- c( paste("scorer", method, sep="."),
        "-d", dat_path,
        "-j", jkl_path,
        "-n", indeg,
        "-t", time,
        "-c", scorefunction,
        "-a", alpha,
        "-b", cores,
        "-v", verbose
    )
    
    blip(args)
    
    return(jkl_path)
}

#' Structure Optimization
#'
#' Find an optimal structure from the cache of parent sets
#'
#' The input data is required to be complete and discrete. 
#' Accordingly missing values in the input data.frame will be ignored, and all numeric values will be converted to integers. 
#' @param jkl cache of pre-computed parent sets.(required)
#' @param method Method to be used. Possible values: "winasobs", "winobs", "asobs", "obs". (default: winasobs)
#' @param time Maximum Execution time (default: 3600)
#' @param cores Number of machine cores to use. If 0, all are used. (default: 1)
#' @param verbose Verbose level (default: 0)
#' @return Structure
#' @export
blip.solver <- function(jkl, method="winasobs", time=3600, cores=1, verbose=1) {

    # CHECK ARGS
    check_jkl(jkl)    

    check_arg(method, c("winasobs", "winobs", "asobs", "obs"))
    
    check_int(time, 1, 999999)
    
    check_int(cores, 0, 999999)
    
    check_int(verbose, 0, 5)
    
    # WRITE INPUT
    jkl_path <- get_temp_jkl()  

    write.jkl(jkl, jkl_path)

    # EXECUTE SO
    res_path <- blip.solver.int(jkl_path, method, time, cores, verbose)
    
    # READ OUTPUT
    res <- read.str(res_path)
    
    return(res)
}

blip.solver.int <- function(jkl_path, method, time, cores, verbose) {

    res_path <- get_temp_res()

    cat("... executing SO (temp file:", res_path, ")\n")

    args <- c( paste("solver", method, sep="."),
        "-j", jkl_path,
        "-r", res_path, 
        "-t", time,
        "-b", cores,
        "-v", verbose
    )
    
    blip(args)
    
    return(res_path)
}

#' Structure Optimization - treewidth bound
#'
#' Find an optimal structure from the cache of parent sets
#'
#' The input data is required to be complete and discrete. 
#' Accordingly missing values in the input data.frame will be ignored, and all numeric values will be converted to integers. 
#' @param jkl cache of pre-computed parent sets.(required)
#' @param method Method to be used. Possible values: "kmax", "kg", "ka". (default: kmax)
#' @param time Maximum Execution time (default: 3600)
#' @param treewidth Maximum treewidth (default: 4)
#' @param cores Number of machine cores to use. If 0, all are used. (default: 1)
#' @param verbose Verbose level (default: 0)
#' @return Structure
#' @export
blip.solver.tw <- function(jkl, method="kmax", treewidth=4, time=3600, cores=1, verbose=1) {

    # CHECK ARGS
    check_jkl(jkl)    

    check_arg(method, c("kmax", "kg", "ka"))
    
    check_int(time, 1, 999999)
    
    check_int(cores, 0, 999999)
    
    check_int(verbose, 0, 5)
    
    # WRITE INPUT
    jkl_path <- get_temp_jkl()  

    write.jkl(jkl, jkl_path)

    # EXECUTE SO
    res_path <- blip.solver.tw.int(jkl_path, method, time, treewidth, cores, verbose)
    
    # READ OUTPUT
    res <- read.str (res_path)
    
    return(res)
}

blip.solver.tw.int <- function(jkl_path, method, time, treewidth, cores, verbose) {

    res_path <- get_temp_res()

    cat("... executing SO TW (temp file:", res_path, ")\n")

    args <- c( paste("solver", method, sep="."),
        "-j", jkl_path,
        "-r", res_path, 
        "-t", time,
        "-w", treewidth,
        "-b", cores,
        "-v", verbose
    )
    
    blip(args)
    
    return(res_path)
}

blip.parle <- function(res, dat, alpha) {

    # CREATE BNLEARN DAG
    #dag1 = model2network("[A][B][F][C|B][E|B][D|A:B:C]") 
    dag = model2network(res)

    # FIT BNLEARN DAG
    fitted = bn.fit(dag, dat, method="bayes", iss=alpha)

    return (fitted)
}


