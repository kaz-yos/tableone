\name{ShowRegTable}
\alias{ShowRegTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  %%  ~~function to do ... ~~
  Format regression results in medically decent format
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
  It shows the regression result in the HR [95\% CI] p-value format, which is
  usually the form used in medical research papers.
}
\usage{
ShowRegTable(model, exp = TRUE, digits = 2, pDigits = 3, quote = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
    %%     ~~Describe \code{model} here~~
    Regression model result objects that have the summary and confint methods.
}
  \item{exp}{
    %%     ~~Describe \code{exp} here~~
    TRUE by default. You need to specify exp = FALSE if your model is
    has the indentity link function (linear regression, etc).
}
  \item{digits}{
    %%     ~~Describe \code{digits} here~~
    Number of digits to print for the main part.
}
  \item{pDigits}{
    %%     ~~Describe \code{pDigits} here~~
    Number of digits to print for the p-values.
}
  \item{quote}{
    %%     ~~Describe \code{quote} here~~
    Whether to show everything in quotes. The default is FALSE. If TRUE,
    everything including the row and column names are quoted so that you
    can copy it to Excel easily.    
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix containing what you see is returned invisibly. You can capture
it by assignment to an object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  %%  ~~who you are~~
  Kazuki Yoshida
}
\note{
  %%  ~~further notes~~
  Developmental repository is on github. Your contributions are
  appreciated.
  
  https://github.com/kaz-yos/tableone
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  CreateContTable, print.ContTable, summary.ContTable, CreateCatTable,
  print.CatTable, summary.CatTable
}
\examples{
## Load Mayo Clinic Primary Biliary Cirrhosis Data
library(survival)
data(pbc)
## Check variables
head(pbc)

## Fit a Cox regression model
objCoxph <-
    coxph(formula = Surv(time, status == 2) ~ trt + age + albumin + ascites,
          data    = pbc,
          ties    = c("efron","breslow","exact")[1])

## Show the simple table
ShowRegTable(objCoxph)

## Show with quote to ease copy and paste
ShowRegTable(objCoxph, quote = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
