#' @title A illustration dataset
#' @name data
#' @description A dataset used to illustrate the performance of \code{vaccR} and \code{vaccC}.
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = gibbsR(100,10),
#'   rnC = gibbsC(100,10)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' 
#' tm2 <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm2)[,c(1,3,5,6)])
#' }
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib StatComp
NULL

#' @title A dataset used for illustration.
#' @name data
#' @description This dataset is used to compare the performance of C function \code{vaccR}) and C++ function \code{vaccC}.
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm)[,c(1,3,5,6)])
#' }
NULL

#' @title Use three inputs to predict response using R.
#' @description The prediction model is described in http://www.babelgraph.org/wp/?p=358.
#' @param age the first predictor (numeric)
#' @param female the second predictor (logical)
#' @param ily the third predictor (logical)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' res <- vaccR(age,female,ily)
#' }
#' @export
vaccR <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * ifelse(female, 1.25, 0.75)
  p <- pmax(0, p)
  p <- pmin(1, p)
  p
}

#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param N the number of samples
#' @param thin the number of between-sample random numbers
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rnR <- gibbsR(100,10)
#' par(mfrow=c(2,1));
#' plot(rnR[,1],type='l')
#' plot(rnR[,2],type='l')
#' }
#' @export
gibbsR <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}
