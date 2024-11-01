#' Extraction methods
#'
#' Extract common information from objects.
#' Apart from the input data's branches,
#' you can extract diffusion components via \code{$DCx}.
#' From \code{\link{DPT}} objects, you can also extract the branch label via \code{$Branch},
#' or the diffusion pseudo time for a numbered cell via \code{$DPTx}.
#'
#' @param x       \code{\link{DiffusionMap}} or \code{\link{DPT}}  object
#' @param i,name  Name of a diffusion component \code{'DCx'}, \code{'DPTx'}, \code{'Branch'} or column from the data
#' @param j       N/A
#' @param ...     ignored
#'
#' @return The names or data row, see respective generics.
#'
#' @seealso \link[base]{Extract}, \code{\link[base]{names}} for the generics. \link{DiffusionMap accession methods}, \link{DiffusionMap methods}, \link{Coercion methods} for more
#'
#' @examples
#' data(guo)
#' dm <- DiffusionMap(guo)
#' dm$DC1        # A diffusion component
#' dm$Actb       # A gene expression vector
#' dm$num_cells  # Phenotype metadata
#'
#' dpt <- DPT(dm)
#' dm$Branch
#' dm$DPT1
#'
#' @name Extraction methods
#' @rdname extractions
#' @aliases
#'   names.DPT names.DiffusionMap
#'      [[.DPT    [[.DiffusionMap
#'       $.DPT     $.DiffusionMap
NULL


#' @importFrom methods is
#' @importFrom Biobase featureNames varLabels
#' @rdname extractions
#' @export
setMethod('names', 'DiffusionMap', function(x) {
	c(colnames(eigenvectors(x)), dataset_names(dataset(x)))
})
#' @rdname extractions
#' @export
setMethod('names', 'DPT', function(x) c(paste0('DPT', seq_len(nrow(x))), 'Branch', names(x@dm)))


#' @importFrom methods is
#' @importFrom Biobase exprs featureNames
#' @rdname extractions
#' @export
setMethod('[[', c('DiffusionMap', 'character', 'missing'), function(x, i, j, ...) {
	if (grepl('^DC\\d+$', i)) {
		eigenvectors(x)[, i]
	} else {
		dataset_get_feature(dataset(x), i)
	}
})
#' @rdname extractions
#' @export
setMethod('[[', c('DPT', 'character', 'missing'), function(x, i, j, ...) {
	if (identical(i, 'dpt')) return(dpt_for_branch(x, 1L))  #TODO

	num_i <- if (grepl('DPT\\d+', i)) as.integer(sub('DPT(\\d+)', '\\1', i))

	if (!is.null(num_i) && 1L <= num_i && num_i <= nrow(x)) {
		x[num_i, ]
	} else if (identical(i, 'Branch') || identical(i, 'branch')) {
		x@branch[, 1L]
	} else {
		x@dm[[i]]
	}
})


#' @rdname extractions
#' @export
setMethod('$', 'DiffusionMap', function(x, name) x[[name]])
#' @rdname extractions
#' @export
setMethod('$', 'DPT', function(x, name) x[[name]])
