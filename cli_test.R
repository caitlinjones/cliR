source("cli.R")

cli <- configure_cli()

add_arg('bin', doc = 'path to all BIC RNASeq R code', req = T)
add_arg('gsea_sh', doc = 'path to gsea wrapper script', req = T)
add_arg('gmt_dir', doc = 'directory containing GMT files', req = T)
add_arg('out_dir', short = 'o', doc = 'output directory', req = T)
add_arg('key_file', short = 'k', doc = 'sample-to-group key file', req = T)

add_arg('norm_counts_file', doc = 'XLSX file containing all DESeq-normalized gene counts')
add_arg('pipeline_rda', doc = "path to 'all_results.rda' file output by BIC RNASeq pipeline")

add_arg('comps', doc = "character string(s) in the form 'CondB_vs_CondA', representing the comparison(s) to be analyzed; REQUIRED when using --norm_counts_file; optional with use of --pipeline_rda and when not used, ALL comparisons found in RDA file will be analyzed.", nargs = '+')

add_arg('collections', doc = 'one or more of the MSigDB collections, indicated by their abbreviations: C1,C2,C3,C4,C5,C6,C7,H', nargs = '+', default = c('c1','c2','c3','c4','c5','c6','c7','h'), options = c('c1','c2','c3','c4','c5','c6','c7','h'))
add_arg('separate', type = 'logical', doc = 'set to TRUE to run separate analysis for each collection; a single analysis with all collections specified will be run by default', default = F)
add_arg('min_gs_size', type = 'integer', doc = 'minimum gene set size to be included in analysis', default = as.integer(15))
add_arg('max_gs_size', type = 'integer', doc = 'maximum gene set size to be included in analysis', default = as.integer(500))

add_required_choice('input_data', c('norm_counts_file', 'pipeline_rda'))
add_dependent_req("norm_counts_file", "comps")

args <- parse_cl(cli)

for(arg in names(args)){
    print(paste0(arg, " = ", paste0(args[[arg]], collapse = ", ")))
}

