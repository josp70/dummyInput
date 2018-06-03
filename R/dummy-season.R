dummySeasonRow <- function(x, levels) {
    l <- length(levels)
    n <- l - 1;
    if (x == levels[1]) {
        return(rep(-1, n));
    }
    index <- levels[2:l] == x;
    return(index * 1);
}

#' A function to generate dummy inputs
#'
#' This function build a matrix of n inputs from a given array of values
#' @param values the vector of values
#' @return the matrix of the inputs
#' @export
#' @examples
#'    levels <- sapply(1:10, function(x) sprintf("%02d", x))
#'
#'    sample <- sample(levels, 100, replace=TRUE)
#'    dummySample <- dummySeason(sample);
#'    head(dummySample)
dummySeason <- function(values) {
    levels <- levels(as.factor(values));
    rows <- lapply(values, function(x) dummySeasonRow(x, levels))
    mat <- do.call(rbind, rows);
    n <- length(levels)
    colnames(mat) <- sapply(levels[2:n], function(x) x);
    class(mat) <- c('dummy', class(mat));
    levels(mat) <- levels;
    attributes(mat) <- c(attributes(mat), list(refLevel = levels[1]));
    return(mat);
}
