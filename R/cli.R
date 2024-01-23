library(logger)
CLI_LOG <- structure(450L, level = 'CLI', class = c('loglevel', 'integer'))

#' Parse and validate command-line input
#'
#' Create an argument list and validate according to specifications in each CLI 
#' argument object. 
#'
#' @param cli   command-line interface configuration, built with any
#'              combination of argument objects 
#' @param log   logical; when TRUE, final arguments will be logged in the 
#'              console
#' @param nest  logical; when TRUE, arguments with names including a '.' will
#'              be nested accordingly; e.g., 'arg_group1.color = red' will be
#'              in final argument list as arg_group1$color = red
#'
#' @return named list of all valid arguments 
#' 
#' @export
parse_cl <- function(cli, log = FALSE, nest = TRUE){

    args <- commandArgs(trailingOnly = TRUE)

    if(length(args) == 0 || '-h' %in% args || '--help' %in% args){
        print_usage(cli)
        q(save = 'no', status = 0)
    }

    args <- structure_arg_list(args, cli)
    
    validate_args(cli, args)

    if(log){
        for(arg in names(args)){
            logger::log_level(CLI_LOG, 
                              paste0(arg, ": ", 
                                     paste0(args[[arg]], collapse = ", "), 
                                     "\n"))
        }
    }

    if(nest){
        args <- args_to_nested_list(args)
    }

    cli <<- NULL
    
    args
}


#' Add a command line argument to configuration
#'
#' Create an argument object with desired settings and add to CLI configuration
#'
#' @param name    argument name to be used on command line and in final 
#                 argument list. to allow for nesting of multiple arguments, 
#'                use '.' in n ame to indicate the nest levels. e.g., 
#'                'arg_group1.color' will be structured as arg_group1$color. 
#'                there is no limit to how many levels can be specified.
#' @param type    data type or class of argument, or 'file' or 'path'. If type
#'                is 'file' or 'path', existence of such file or path will be
#'                tested. hence, not to be used for files to be created (output) 
#' @param doc     help string
#' @param nargs   number of expected arguments. use '+' for unknown number.
#'                default = 1
#' @param req     logical; when TRUE, argument will be required and if not
#'                provided, script will fail. default = FALSE
#' @param short   short name. short character string to use as an alternative
#'                argument identifier on command line. default = NULL
#' @param options vector of valid options for this argument. script will fail 
#'                if value provided does not match any of these options. 
#'                default = NULL (argument value may be any value satisfying 
#                 other restrictions)
#' @param default default argument value; default = NULL
#'  
#' @export
add_arg <- function(name, type = 'character', doc = '', nargs = 1, req = FALSE, 
                    short  = NULL, options = NULL, default = NULL){
    if(name %in% names(cli$all_args)){
        warning(paste0("argument ", name, " already added. OVERWRITING!")) 
    }
    arg <- list(name = name, 
                type = type,
                doc = doc,
                nargs = nargs,
                req = req,
                short = short,
                options = options,
                default = default)

    if(grepl("\\.", arg$name)){
        arg$group <- gsub("\\..*", "", arg$name)
    }

    if(req){
        cli$req_args[[name]] <<- arg
    } else {
        cli$opt_args[[name]] <<- arg
    }
}

#' Add a requirement to CLI object for user to choose between two or more
#' arguments or sets of arguments
#'
#' After adding all individual arguments to CLI object, set a requirement for
#' user to use one option or set of options OR the other. 
#' 
#' @param choice_id    character; ID for the required choice. e.g., 'INPUT'
#' @param arg_choices  list with two items, where each item is a vector of one
#'                     or more arguments and the user must provide either ALL
#'                     arguments in list item 1 or ALL items in list argument 2
#' 
#' @export
add_required_choice <- function(choice_id, arg_choices){
    cli$req_choices[[choice_id]] <<- arg_choices
}

#' Add a dependent or conditional requirement to an argument object in CLI
#'
#' Add a requirement to CLI object that an argument is only required when 
#' a certain other argument is NOT NULL. 
#' 
#' @param arg_not_null  name of argument to check for null to determine 
#'                      whether \code{req_arg} is required
#' @param req_arg       vector of name(s) of argument(s) to set as required when 
#'                      \code{arg_not_null} is not null
#'
#' @export
add_dependent_req <- function(arg_not_null, req_arg){
    cli$dep_req[[arg_not_null]] <<- req_arg
}

#' Initialize CLI configuration object
#' 
#' Create CLI object to hold required and optional arguments, required choices,
#' and depending requirements 
#'  
#' @export
configure_cli <- function(){
    list(req_args = list(),
         req_choices = list(),
         dep_req = list(),
         opt_args = list())
}

#' Internal function to parse argument name and create nested list using '.'
#' character as an indication of list levels
#'
#' @param arg  list containing argument value and named by arg name
nest_arg <- function(arg){
    levels <- rev(unlist(strsplit(names(arg), "\\.")))
    arglist <- arg[[1]]
    for(lvl in levels){
        arglist <- list(arglist)
        names(arglist) <- lvl        
    }
    arglist
}

#' Merge two lists
#' 
#' Recursively combine two lists, returning the union of all list levels
#' 
#' @param list1  the first list to be merged
#' @param list2  the second list to be merged
merge_lists <- function(list1, list2){
    fin <- list1
    for(nm in names(list2)){
        if(is.null(list1[[nm]])){
            fin[[nm]] <- list2[[nm]]
            next
        } 
        if(!identical(list1[[nm]], list2[[nm]])){
            ## if one is list but not the other, not sure 
            ## what to do because one has names and the other does not
            if(!all(is.list(list1), is.list(list2)) & 
                (is.list(list1) | is.list(list2))){
                stop(paste0("ERROR: List within item named ", nm, 
                           " can not be merged with non-list item.\n"))
            }
            if(!is.list(list2[[nm]])){
                fin[[nm]] <- c(fin[[nm]], list2[[nm]]) 
            } else {
                fin[[nm]] <- merge_lists(list1[[nm]], list2[[nm]])
            }
        }
    }
    fin
}


#' Create nested list from flat command line argument list
#' 
#' Parse argument list names to determine how list should be nested. Each '.'
#' character in an argument name indicates that an inner list should be 
#' created. e.g., list item name 'arg_group1.color.hot' will result in an
#' inner list structured \code{arg_group1$color$hot}
#' 
#' @param args  single-level, named list of command line arguments
#' 
#' @return nested list
args_to_nested_list <- function(args){
    nargs <- list() 
    for(arg_name in names(args)){
        nlist <- nest_arg(args[arg_name])
        aname <- names(nlist)[1]
        if(!aname %in% names(nargs)){
            nargs <- c(nargs, nlist)
        } else {
            nargs[[aname]] <- merge_lists(nargs[[aname]],  nlist[[aname]])
        }
    } 
    nargs
}

#' Format argument doc string for help
#' 
#' @param arg          argument object
#' @param longest_arg  integer; length of the longest argument name, used for
#'                     justification; default = NULL
#' 
#' @return formatted argument doc string
build_arg_help <- function(arg, longest_arg = NULL){
    page_width <- 80
    x <- c("    ")
    nm <- c("--", arg$name)
    shrt <- ifelse(is.null(arg$short), "", paste0("-", arg$short, ", "))
    x <- c(x, shrt, nm)

    if(is.null(longest_arg)){
        longest_arg = nchar(arg$name) + nchar(shrt)
    }
    spcs <- (longest_arg - (nchar(arg$name) + nchar(shrt)) + 1)
    x <- c(x, rep(" ", spcs))

    doc_str <- arg$doc
    if(!is.null(arg$options)){
        opt_str <- paste0("[", paste(arg$options, collapse = "|"), "]")
        doc_str <- paste(doc_str, opt_str, sep = " ")
    }

    doc_width <- page_width - (spcs + 6 + nchar(arg$name))
    doc_str1 <- strwrap(doc_str, width = doc_width, indent = 0)
    x <- c(x, doc_str1[1])
    x <- c(x, "\n")

    if(length(doc_str1) > 1){
        doc_str2 <- strwrap(trimws(substring(doc_str, nchar(doc_str1[1]) + 1)),
                            width = page_width, 
                            indent = page_width - doc_width, 
                            exdent = page_width - doc_width)
        x <- c(x, paste0(doc_str2, collapse = "\n"))
        x <- c(x, "\n")
    }

    other_str = ""
    if(!is.null(arg$default)){
        other_str <- paste0("[default = ", 
                            paste0(arg$default, collapse = ","), 
                            "] ")
    }
    if(!is.null(arg$group)){
        other_str <- paste0(other_str, " [group = ", arg$group, "]")
    }
    if(other_str != ""){
        other_str <- strwrap(other_str,
                             width = page_width, 
                             indent = page_width - doc_width, 
                             exdent = page_width - doc_width)
        x <- c(x, paste0(other_str, collapse = "\n"))
        x <- c(x, "\n")
    }
    paste(x, collapse = "")
}

#' Get longest string in vector of strings
#'
#' @param strings vector of character strings
#' 
#' @return length of longest character string in vector
get_longest_str <- function(strings){
    max(
      unlist(
        lapply(strings, function(x){
            nchar(x)
        })
      )
    )
}

#' Print usage
#' 
#' Print full usage and help page
#' 
#' @param cli  CLI object
print_usage <- function(cli){
    summ <- usage_summary(cli)
    det <- usage_detail(cli)
    cat(paste0("\n", paste(summ, collapse = " "), "\n"))
    dn <- lapply(det, cat)
    cat("\n")
}

#' Build usage summary
#' 
#' Construct single-line script usage summary
#' 
#' @param cli  CLI object
#' 
#' @return character string
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
                   paste0("{", 
                          paste(paste0("--", cli$req_choices[[x]]), 
                                collapse = "|"), 
                          ' ', 
                          toupper(x), 
                          "}")
               })
        usage <- c(usage, unlist(tmp))
    }
    if(length(cli$opt_args) > 0){
        tmp <- "[OPTIONS...]"
        usage <- c(usage, tmp)
    }
    usage
}

#' Build full usage including all argument documentation
#' 
#' Compile usage summary AND all argument documentation
#' 
#' @param cli CLI object
#' 
#' @return full usage string
usage_detail <- function(cli){
    help <- c()
    ## add 4 spaces to the longest string to allow for short versions of 
    ## arg names
    longest <- get_longest_str(c(names(cli$req_args), names(cli$opt_args))) + 4
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
                   for(i in seq(cli$req_choices[[x]])){
                       choice <- cli$req_choices[[x]][[i]]
                       if(i > 1){
                           c_help <- c(c_help, "          OR\n")
                       }
                       for(ch in choice){
                           arg_help <- build_arg_help(cli$opt_args[[ch]], 
                                                      longest_arg = longest)
                           c_help <- c(c_help, arg_help) 
                       }
                   }
                   c_help
               })
        help <- c(help, unlist(tmp))
    }

    opts <- setdiff(names(cli$opt_args), unlist(cli$req_choices))
    if(length(opts) > 0){
        help <- c(help, "\n [OPTIONS]\n")
        tmp <- lapply(cli$opt_args[opts], function(x){
                   build_arg_help(x, longest_arg = longest)
               })
        help <- c(help, unlist(tmp))
    }

    help
}

#' Check for existence of required arguments
#'
#' If any required arguments are missing from parsed argument list, 
#' print error message and quit with non-zero exit status
#'
#' @param cli   CLI object
#' @param args  flat (not nested) argument list provided on command line
check_required_args <- function(cli, args){
    if(!all(names(cli$req_args) %in% names(args))){
        print_usage(cli)
        stop(paste0("\nMissing required arguments: ", 
                   paste(setdiff(names(cli$req_args), names(args)), 
                         collapse = ', '), "\n"))
    }
}

#' Check that required choices are made properly
#'
#' For each item in \code{cli$req_choices}, make sure that one of the two 
#' sets of choices is provided. If not, print error message and quit with non-
#' zero exit status
#'
#' @param cli   CLI object
#' @param args  named, flat list of command line arguments
check_required_choices <- function(cli, args){
    errs <- unlist(
              sapply(names(cli$req_choices), function(x){
                inc <- which(sapply(cli$req_choices[[x]], 
                                    function(i){ 
                                        all(i %in% names(args)) 
                                    }
                                   )
                            )
                if(length(inc) != 1){
                    return(paste0("  Must choose one option as ", x, ": ", 
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
        stop()
    }
}

#' Check that dependent requirements are satisfied
#' 
#' If an argument is required when another one provided and said argument is
#' not also provided, print error message and exit with non-zero exit status
#' 
#' @param cli   CLI object                                                      
#' @param args  named, flat list of command line arguments                      
check_dependent_requirements <- function(cli, args){
    if(length(cli$dep_req) == 0){
        return(NULL)
    }
    errs <- unlist(
              sapply(names(cli$dep_req), function(x){
                if(x %in% names(args) && !is.null(args[[x]])){
                    for(nm in cli$dep_req[[x]]){
                        if(!nm %in% names(args)){
                            return(paste0("  Argument '", nm, 
                                          "' required when using '--", 
                                          x, "'."))
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
        stop()
    }
}


#' Create a map of full argument names to their short alternatives
#'
#' @param cli  CLI object
#' 
#' @return list of full argument names, named by their short names
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


#' Fill in argument list with defaults for arguments not provided on
#' command line
#'
#' @param arglist  named, flat list of command line arguments                      
#' @param cli      CLI object                                                      
fill_in_defaults <- function(arglist, cli){
    complete_list <- arglist
    def <- setdiff(names(cli$opt_args), names(arglist))
    for(rq in def){
        complete_list[[rq]] <- cli$opt_args[[rq]]$default
    }
    complete_list
}


#' Parse command line arguments and build one-level argument list
#'
#' TODO: LOOK AT THIS FUNCTION MORE CLOSELY TO GET BETTER DESCRIPTION
#' 
#' @param args  list of arguments read directly from command line
#' @param cli CLI object
#'
#' @return structured argument linst 
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
                         stop(paste0("Unrecognized argument: ", argname, "\n")) 
                     }
                     argname <- nmMap[argname]
                 }
                 last_val <- ifelse(i == length(namepos), 
                                    length(args), 
                                    namepos[i + 1] - 1)
                 val_idxs <- seq(pos + 1, last_val)        

                 if(length(val_idxs) > 1 && val_idxs[1] > val_idxs[2] || 
                     length(val_idxs) == 0){
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


#' Validate provided argument using its corresponding CLI argument object
#'
#' Use specifications in CLI argument object to validate provided argument
#'
#' @param cli_arg  argument object from CLI object
#' @param arg_val  value provided for argument
validate_arg <- function(cli_arg, arg_val){

    ## check type
    if(cli_arg$type == "file"){
        files_exist <- sapply(arg_val, file.exists)
        if(!all(files_exist)){
            stop(paste0("\nERROR: file(s) do not exist:\n", 
                       paste(arg_val[!files_exist], collapse = "\n"), 
                       "\n"))
        }
        return(NULL)
    }
    if(cli_arg$type == "path"){
        dirs_exist <- sapply(arg_val, dir.exists)
        if(!all(dirs_exist)){
            stop(paste0("\nERROR: path(s) do not exist:\n", 
                       paste(arg_val[!dirs_exist], collapse = "\n"), 
                       "\n"))
        }
    }
    if(cli_arg$type == "integer" && !is.integer(arg_val)){
        arg_val <- tryCatch({
                       as.integer(arg_val)
                    }, error = function(){ 
                       stop(paste0("ERROR: ", cli_arg$name, 
                                  " should be of type INTEGER"))
                   })
    }
    if(!cli_arg$type %in% c(typeof(arg_val), class(arg_val), "file", "path")){  
        stop(paste0("\nERROR: ", cli_arg$name, " should be of type ", 
                   toupper(cli_arg$type), ", not ", typeof(arg_val), "\n\n"))
    }    

    ## check length
    if(cli_arg$nargs != "+" && length(arg_val) != cli_arg$nargs){
        stop(paste0("\nERROR: ", length(arg_val), " values given for arg ", 
                    cli_arg$name, ". Number allowed: ", cli_arg$nargs, "\n\n")) 
    }

    ## check for valid option(s)
    if(!is.null(cli_arg$options) && !all(arg_val %in% cli_arg$options)){
        stop(paste0("\nERROR: Invalid option given for ", cli_arg$name, ": ", 
                   paste0(setdiff(arg_val, cli_arg$options), collapse = ", "), 
                   "\n\n"))
    }
}

#' Validate all arguments
#'
#' Check all arguments provided against the specifications in the corresponding
#' CLI object argument
#'
#' @param cli  CLI object
#' @param args flat, named list of arguments provided on command line
validate_args <- function(cli, args){
    check_required_args(cli, args)
    check_dependent_requirements(cli, args)
    check_required_choices(cli, args)

    dvnl <- lapply(names(args), function(x){
        if(!x %in% cli$req_args & !x %in% cli$opt_args){
            stop(paste0("\nERROR: Unrecognized argument '", x, ".' See usage for valid arguments\n"))
        }
        arg_type <- ifelse(x %in% names(cli$req_args), 'req_args', 'opt_args')
        validate_arg(cli[[arg_type]][[x]], args[[x]])
    })          
}

cli <<- configure_cli()

