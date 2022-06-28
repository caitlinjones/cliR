#' export
parse_cl <- function(cli){

    args <- commandArgs(trailingOnly = TRUE)

    if(length(args) == 0 || '-h' %in% args || '--help' %in% args){
        print_usage(cli)
        q(save = 'no', status = 0)
    }

    args <- structure_arg_list(args, cli)
    
    validate_args(cli, args)

    cli <<- NULL
    
    args
}


#' export
add_arg <- function(name, type = 'character', doc = '', nargs = 1, req = FALSE, short = NULL, 
                     options = NULL, default = NULL){
    if(name %in% names(cli$all_args)){
        warning(paste0("argument ", name, " already added. OVERWRITING!")) 
    }
    arg <- list(name = name, 
                doc = doc,
                type = type,
                nargs = nargs,
                req = req,
                short = short,
                options = options,
                default = default)

    if(req){
        cli$req_args[[name]] <<- arg
    } else {
        cli$opt_args[[name]] <<- arg
    }
}

#' export
add_required_choice <- function(choice_id, arg_choices){
    cli$req_choices[[choice_id]] <<- arg_choices
}

# store the requirement that when argNotNull is NOT NULL, req_arg becomes a required arg
#' export
add_dependent_req <- function(argNotNull, req_arg){
    cli$dep_req[[argNotNull]] <<- req_arg
}

#' export
configure_cli <- function(){
    list(req_args = list(),
         req_choices = list(),
         dep_req = list(),
         opt_args = list())
}


build_arg_help <- function(arg, longest_arg = NULL){
    x <- c("    ")
    nm <- c("--", arg$name)
    shrt <- ifelse(is.null(arg$short), '', paste0('-', arg$short, ', '))
    x <- c(x, shrt, nm)
    if(is.null(longest_arg)){
        longest_arg = nchar(arg$name) + nchar(shrt)
    }
    spcs <- (longest_arg - (nchar(arg$name) + nchar(shrt)) + 1)
    x <- c(x, rep(" ", spcs))
    
    doc_width <- 80 - (spcs + 6 + nchar(arg$name))
    doc_str1 <- strwrap(arg$doc, width = doc_width, indent = 0)
    x <- c(x, doc_str1[1])
    x <- c(x, "\n")

    if(length(doc_str1) > 1){
        doc_str2 <- strwrap(gsub(doc_str1[1], "", arg$doc), width = 80, indent = 80 - doc_width, exdent = 80 - doc_width)
        x <- c(x, paste0(doc_str2, collapse = "\n"))
        x <- c(x, "\n")
    }
    if(!is.null(arg$default)){
        def_str <- strwrap(paste0("[default = ", paste0(arg$default, collapse = ","), "]"), 
                            width = 80, indent = 80 - doc_width, exdent = 80 - doc_width)
        x <- c(x, paste0(def_str, collapse = "\n"))
        x <- c(x, "\n")
    }
    paste(x, collapse = "")
}

get_longest_str <- function(strings){
    max(
      unlist(
        lapply(strings, function(x){
            nchar(x)
        })
      )
    )
}

print_usage <- function(cli){
    summ <- usage_summary(cli)
    det <- usage_detail(cli)
    cat(paste0("\n", paste(summ, collapse = " "), "\n"))
    dn <- lapply(det, cat)
    cat("\n")
}

usage_summary <- function(cli){
    usage <- c("  Usage: script.R ")
    if(length(cli$req_args) > 0){
        tmp <- lapply(cli$req_args, function(x){
                   paste0("--", x$name, ' ', toupper(gsub(" ", "_", x$doc)))
               }) 
        usage <- c(usage, unlist(tmp))
    }
    if(length(cli$req_choices) > 0){
        tmp <- lapply(names(cli$req_choices), function(x){
                   paste0("{", paste(paste0("--", cli$req_choices[[x]]), collapse = "|"), ' ', toupper(x), "}")
               })
        usage <- c(usage, unlist(tmp))
    }
    if(length(cli$opt_args) > 0){
        opt_args <- setdiff(names(cli$opt_args), unlist(cli$req_choices))
        tmp <- paste0("[--", opt_args, "]")
        usage <- c(usage, tmp)
    }
    usage
}


usage_detail <- function(cli){
    help <- c()
    longest <- get_longest_str(c(names(cli$req_args), names(cli$opt_args))) + 4 #4 spaces for short version of arg name
    if(length(cli$req_args) > 0){
        help <- c("\n [REQUIRED]\n")
        tmp <- lapply(cli$req_args, function(x){
                   build_arg_help(x, longest_arg = longest)
               })
        help <- c(help, unlist(tmp))
    }
 
    if(length(cli$req_choices)){
        help <- c(help, "\n [REQUIRED CHOICE] \n")
        tmp <- lapply(names(cli$req_choices), function(x){
                   c_help <- c("    ", toupper(x), "\n")
                   for(choice in cli$req_choices[[x]]){
                       if(choice != cli$req_choices[[x]][1]){
                           c_help <- c(c_help, "          OR\n")
                       }
                       c_help <- c(c_help, build_arg_help(cli$opt_args[[choice]], longest_arg = longest))
                   }
                   c_help
               })
        help <- c(help, unlist(tmp))
    }
 
    if(length(cli$opt_args) > 0){
        opt <- setdiff(names(cli$opt_args), unlist(cli$req_choices))
        help <- c(help, "\n [OPTIONS]\n")
        tmp <- lapply(cli$opt_args[opt], function(x){
                   build_arg_help(x, longest_arg = longest)
               })
        help <- c(help, unlist(tmp))
    }

    help
}


check_required_args <- function(cli, args){
    if(!all(names(cli$req_args) %in% names(args))){
        print_usage(cli)
        cat(paste0("\nMissing required arguments: ", paste(setdiff(names(cli$req_args), names(args)), collapse = ', ')))
        cat("\n")
        q(save = 'no', status = 1)
    }
}

check_required_choices <- function(cli, args){
    errs <- unlist(
              sapply(names(cli$req_choices), function(x){
                inc <- which(cli$req_choices[[x]] %in% names(args))
                if(length(inc) != 1){
                    return(paste0("  Must choose one argument as ", x, ": ", 
                           paste0(cli$req_choices[[x]], collapse = "|")))
                }
              })
            )
    if(length(errs) > 0){
        print_usage(cli)
        for(err in errs){
            cat(paste0("\nERROR: ", err))
        }
        cat("\n")
        q(save = 'no', status = 1)
    }
}

check_dependent_requirements <- function(cli, args){
    if(length(cli$dep_req) == 0){
        return(NULL)
    }
    errs <- unlist(
              sapply(names(cli$dep_req), function(x){
                if(x %in% names(args) && !is.null(args[[x]])){
                    for(nm in cli$dep_req[[x]]){
                        if(!nm %in% names(args)){
                            return(paste0("  Argument '", nm, "' required when using '--", x, "'."))
                        }
                    }
                }
              })
            )
    if(length(errs) > 0){
        print_usage(cli)
        for(err in errs){
            cat(paste0("\nERROR", err, "\n"))
        }
        q(save = 'no', status = 1)
    }
}

map_arg_names <- function(cli){
    nameMap <- c()
    if(!is.null(cli$req_args) && length(cli$req_args) > 0){
        req <- unlist(
                   lapply(1:length(cli$req_args), function(x){
                       arg <- cli$req_args[[x]]
                       if(is.null(arg$short)){ return(NULL) }
                       long <- c(arg$name)
                       names(long) <- arg$short
                       long
                   })
               )
        nameMap <- c(nameMap, req)
    }
    if(!is.null(cli$opt_args) && length(cli$opt_args) > 0){
        opt <- unlist(
                   lapply(1:length(cli$opt_args), function(x){
                       arg <- cli$opt_args[[x]]
                       if(is.null(arg$short)){ return(NULL) }
                       long <- c(arg$name)
                       names(long) <- arg$short
                       long
                   })
               )
        nameMap <- c(nameMap, opt)
    }
    nameMap
}

fill_in_defaults <- function(arglist, cli){
    complete_list <- arglist
    def <- setdiff(names(cli$opt_args), names(arglist))
    for(rq in def){
        complete_list[[rq]] <- cli$opt_args[[rq]]$default
    }
    complete_list
}

structure_arg_list <- function(args, cli){
    ## map short names to long
    nmMap <- map_arg_names(cli)

    namepos <- grep("^-", args)
    arglist <- lapply(1:length(namepos), function(i){

                 pos <- namepos[i]
                 arg <- args[pos]
                 argname <- gsub('\\-', '', arg)

                 if(!grepl("^--", arg)){
                     if(is.null(nmMap) || !argname %in% names(nmMap)){ 
                         cat(paste0("Unrecognized argument: ", argname, "\n")) 
                         q(save = 'no', status = 1)
                     }
                     argname <- nmMap[argname]
                 }
                 last_val <- ifelse(i == length(namepos), length(args), namepos[i + 1] - 1)
                 val_idxs <- seq(pos + 1, last_val)        

                 if(length(val_idxs) > 1 && val_idxs[1] > val_idxs[2] || length(val_idxs) == 0){
                     val <- list(TRUE)
                 } else {
                     val <- list(c(type.convert(args[val_idxs], as.is = TRUE)))
                 }
                 names(val) <- argname
                 val
              })
    arglist <- unlist(arglist, recursive = F)
    arglist <- fill_in_defaults(arglist, cli)
    arglist
}

validate_arg <- function(cli_arg, arg_val){
    ## check type
    if(typeof(arg_val) != cli_arg$type){
        cat(paste0("\nERROR: ", cli_arg$name, " should be of type ", toupper(cli_arg$type), ", not ", typeof(arg_val), "\n\n"))
        q(save = 'no', status = 1)
    }    

    ## check length
    if(cli_arg$nargs != '+' && length(arg_val) != cli_arg$nargs){
        cat(paste0("\nERROR: ", length(arg_val), " values given for arg ", 
                    cli_arg$name, ". Number allowed: ", cli_arg$nargs, "\n\n")) 
        q(save = 'no', status = 1)
    }

    ## check for valid option(s)
    if(!is.null(cli_arg$options) && !all(arg_val %in% cli_arg$options)){
        cat(paste0("\nERROR: Invalid option given for ", cli_arg$name, ": ", 
                     paste0(setdiff(arg_val, cli_arg$options), collapse = ", "), "\n\n"))
        q(save = 'no', status = 1)
    }
}

validate_args <- function(cli, args){
    check_required_args(cli, args)
    check_dependent_requirements(cli, args)
    check_required_choices(cli, args)

    dvnl <- lapply(names(args), function(x){
        arg_type <- ifelse(x %in% names(cli$req_args), 'req_args', 'opt_args')
        validate_arg(cli[[arg_type]][[x]], args[[x]])
    })          
}

