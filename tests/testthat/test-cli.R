default_arg <- list(name = NULL,
                    type = 'character',
                    doc = '',
                    nargs = 1,
                    req = FALSE,
                    short = NULL,
                    options = NULL,
                    default = NULL)

test_that("global CLI object is initialized on package load", {
    exp_obj <- list(req_args = list(),
                    req_choices = list(),
                    dep_req = list(),
                    opt_args = list())
    expect_identical(cli, exp_obj)
})

test_that("required argument is added to req_args list of global CLI object", {
    add_arg("test_req_arg", req = T)
    exp_req_arg_list <- list(test_req_arg = default_arg)
    exp_req_arg_list$test_req_arg$name <- "test_req_arg"
    exp_req_arg_list$test_req_arg$req <- T
    expect_identical(cli$req_args, exp_req_arg_list)
})

test_that("optional argument is added to opt_args list of global CLI object", {
    add_arg("test_opt_arg")

    ## make sure req_args hasn't changed
    exp_req_arg_list <- list(test_req_arg = default_arg)
    exp_req_arg_list$test_req_arg$name <- "test_req_arg"
    exp_req_arg_list$test_req_arg$req <- T

    ## make sure opt_args now has one item
    exp_opt_arg_list <- list(test_opt_arg = default_arg)
    exp_opt_arg_list$test_opt_arg$name <- "test_opt_arg"
    
    exp_cli <- list(req_args = exp_req_arg_list,
                    req_choices = list(),
                    dep_req = list(),
                    opt_args = exp_opt_arg_list)

    expect_identical(cli, exp_cli)
})

test_that("required choice is added to the req_choices list of global CLI object", {
    add_required_choice('TEST', c('test_req_arg', 'test_opt_arg'))

    ## make sure req_args hasn't changed
    exp_req_arg_list <- list(test_req_arg = default_arg)
    exp_req_arg_list$test_req_arg$name <- "test_req_arg"
    exp_req_arg_list$test_req_arg$req <- T

    ## make sure opt_args now has one item
    exp_opt_arg_list <- list(test_opt_arg = default_arg)
    exp_opt_arg_list$test_opt_arg$name <- "test_opt_arg"

    req_choices_list <- list('TEST' = c('test_req_arg', 'test_opt_arg'))

    exp_cli <- list(req_args = exp_req_arg_list,
                    req_choices = req_choices_list, 
                    dep_req = list(),
                    opt_args = exp_opt_arg_list)

    expect_identical(cli, exp_cli)
})

test_that("dependent requirements are added to dep_req list of global CLI object", {

    add_dependent_req('test_opt_arg', 'test_dep_req_arg')

    ## make sure req_args hasn't changed
    exp_req_arg_list <- list(test_req_arg = default_arg)
    exp_req_arg_list$test_req_arg$name <- "test_req_arg"
    exp_req_arg_list$test_req_arg$req <- T

    ## make sure opt_args now has one item
    exp_opt_arg_list <- list(test_opt_arg = default_arg)
    exp_opt_arg_list$test_opt_arg$name <- "test_opt_arg"

    req_choices_list <- list('TEST' = c('test_req_arg', 'test_opt_arg'))
    dep_req_list <- list('test_opt_arg' = 'test_dep_req_arg')

    exp_cli <- list(req_args = exp_req_arg_list,
                    req_choices = req_choices_list,
                    dep_req = dep_req_list,
                    opt_args = exp_opt_arg_list)

    expect_identical(cli, exp_cli)

})

test_that("lists within global CLI object are appended to properly", {

    add_arg('test_req_arg2', req = T)
    add_arg('test_opt_arg2')
    add_required_choice('TEST2', c('test_opt_arg', 'test_opt_arg2'))
    add_dependent_req('test_opt_arg2', 'test_opt_arg')

    ## make sure req_args hasn't changed
    exp_req_arg_list <- list(test_req_arg = default_arg, 
                             test_req_arg2 = default_arg)
    exp_req_arg_list$test_req_arg$name <- "test_req_arg"
    exp_req_arg_list$test_req_arg$req <- T
    exp_req_arg_list$test_req_arg2$name <- "test_req_arg2"
    exp_req_arg_list$test_req_arg2$req <- T

    ## make sure opt_args now has one item
    exp_opt_arg_list <- list(test_opt_arg = default_arg, 
                             test_opt_arg2 = default_arg)
    exp_opt_arg_list$test_opt_arg$name <- "test_opt_arg"    
    exp_opt_arg_list$test_opt_arg2$name <- "test_opt_arg2"

    req_choices_list <- list(TEST = c('test_req_arg', 'test_opt_arg'),
                             TEST2 = c('test_opt_arg', 'test_opt_arg2'))
    dep_req_list <- list(test_opt_arg = 'test_dep_req_arg',
                         test_opt_arg2 = 'test_opt_arg')

    exp_cli <- list(req_args = exp_req_arg_list,
                    req_choices = req_choices_list,
                    dep_req = dep_req_list,
                    opt_args = exp_opt_arg_list)

    expect_identical(cli, exp_cli)

})

test_that("arguments are properly nested", {

    arglist <- list(arg1 = 'independent arg 1',
                    arg2 = 'independent arg 1',
                    arg_group1.arg1 = 'group 1 arg 1',
                    #arg_group1.arg2 = 'group1  arg 2',
                    arg_group1.arg2.argA = 'arg A in arg2 of group 1',
                    #arg_group2.arg1 = 'group 2 arg 1',
                    arg_group2.arg1.argA = 'arg A in arg2 of group 2',
                    arg_3 = 'independent arg 3')    
  
    exp_nested <- list(arg1 = 'independent arg 1',
                       arg2 = 'independent arg 1',
                       arg_group1 = list(arg1 = 'group 1 arg 1',
                                         arg2 = list(argA = 'arg A in arg2 of group 1')),
                       arg_group2 = list(arg1 = list(argA = 'arg A in arg2 of group 2')),
                       arg_3 = 'independent arg 3')

    expect_equal(args_to_nested_list(arglist), exp_nested)

    ## error should be thrown when list attempts to use named and non-named list elements
    arglist[['arg_group1.arg2']] <- 'WRONG'
    expect_error(args_to_nested_list(arglist))

})

