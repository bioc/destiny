#' @include utils.r
NULL

#' Convert object to \link[Biobase:class.ExpressionSet]{ExpressionSet} or read it from a file
#'
#' These functions present quick way to create \link[Biobase:class.ExpressionSet]{ExpressionSet} objects.
#'
#' They work by using all continuous (double) columns as expression data, and all others as observation annotations.
#'
#' @param x  \link[base]{data.frame} to convert to an \link[Biobase:class.ExpressionSet]{ExpressionSet}.
#'
#' @return an \link[Biobase:class.ExpressionSet]{ExpressionSet} object
#'
#' @examples
#' library(Biobase)
#' df <- data.frame(Time  = seq_len(3), #integer column
#'                  Actb  = c(0.05, 0.3, 0.8),
#'                  Gapdh = c(0.2, 0.03, 0.1))
#' set <- as.ExpressionSet(df)
#' rownames(exprs(set)) == c('Actb', 'Gapdh')
#' phenoData(set)$Time == 1:3
#'
#' @seealso \link[utils]{read.table} on which \code{read.ExpressionSet} is based, and \link[Biobase:class.ExpressionSet]{ExpressionSet}.
#'
#' @name ExpressionSet helper methods
#' @rdname ExpressionSet-helpers
NULL

#' @importFrom methods setGeneric .valueClassTest
#' @rdname ExpressionSet-helpers
#' @export
setGeneric('as.ExpressionSet', function(x, ...) standardGeneric('as.ExpressionSet'), valueClass = 'ExpressionSet')

#' @param annotation_cols  The data.frame columns used as annotations. All others are used as expressions. (Logical, character or numerical index array)
#'
#' @importFrom Biobase ExpressionSet AnnotatedDataFrame phenoData
#' @rdname ExpressionSet-helpers
#' @export
setMethod('as.ExpressionSet', 'data.frame', function(x, annotation_cols = !sapply(x, is.double)) {
	if (!is.logical(annotation_cols))
		annotation_cols <- l_which(annotation_cols, names(x))

	assay_data <- t(as.matrix(x[!annotation_cols]))
	pheno_data <- AnnotatedDataFrame(x[annotation_cols])

	ExpressionSet(assay_data, pheno_data)
})

#' @param file    File path to read ASCII data from
#' @param header  Specifies if the file has a header row.
#' @param ...     Additional parameters to \link[utils]{read.table}
#'
#' @importFrom utils read.table
#' @rdname ExpressionSet-helpers
#' @export
read.ExpressionSet <- function(file, header = TRUE, ...) { # nolint: object_name_linter.
	as.ExpressionSet(read.table(file, header, ...))
}
