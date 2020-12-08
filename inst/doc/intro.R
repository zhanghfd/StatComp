## ----eval=FALSE---------------------------------------------------------------
#  function (age, female, ily)
#  {
#      p <- 0.25 + 0.3 * 1/(1 - exp(0.04 * age)) + 0.1 * ily
#      p <- p * ifelse(female, 1.25, 0.75)
#      p <- pmax(0, p)
#      p <- pmin(1, p)
#      p
#  }

## ----eval=FALSE---------------------------------------------------------------
#  double vacc3a(double age, bool female, bool ily){
#    double p = 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily;
#    p = p * (female ? 1.25 : 0.75);
#    p = std::max(p, 0.0);
#    p = std::min(p, 1.0);
#    return p;
#  }
#  NumericVector vaccC(NumericVector age, LogicalVector female,
#                      LogicalVector ily) {
#    int n = age.size();
#    NumericVector out(n);
#    for(int i = 0; i < n; ++i) {
#      out[i] = vacc3a(age[i], female[i], ily[i]);
#    }
#    return out;
#  }

## ----eval=TRUE----------------------------------------------------------------
library(StatComp)
library(microbenchmark)
data(data)
attach(data)
tm2 <- microbenchmark(
  vR = vaccR(age,female,ily),
  vC = vaccC(age,female,ily)
)
knitr::kable(summary(tm2)[,c(1,3,5,6)])

## ----eval=FALSE---------------------------------------------------------------
#  gibbsR <- function(N, thin) {
#    mat <- matrix(nrow = N, ncol = 2)
#    x <- y <- 0
#    for (i in 1:N) {
#      for (j in 1:thin) {
#        x <- rgamma(1, 3, y * y + 4)
#        y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
#      }
#      mat[i, ] <- c(x, y)
#    }
#    mat
#  }

## ----eval=FALSE---------------------------------------------------------------
#  NumericMatrix gibbsC(int N, int thin) {
#    NumericMatrix mat(N, 2);
#    double x = 0, y = 0;
#    for(int i = 0; i < N; i++) {
#      for(int j = 0; j < thin; j++) {
#        x = rgamma(1, 3, 1 / (y * y + 4))[0];
#        y = rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))[0];
#      }
#      mat(i, 0) = x;
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }

## ----eval=TRUE----------------------------------------------------------------
tm2 <- microbenchmark(
  vR = gibbsR(1e4, 10),
  vC = gibbsC(1e4, 10)
)
knitr::kable(summary(tm2)[,c(1,3,5,6)])

