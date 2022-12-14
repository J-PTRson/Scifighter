% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt_menu.R
\name{opt_menu}
\alias{opt_menu}
\title{opt_menu}
\usage{
opt_menu(c.opt = NULL)
}
\arguments{
\item{c.opt}{options from which the user can choose (in capitals!)}
}
\value{
Returns a selected option
}
\description{
This function provides a number of options to the user and allows for menu interactions.
}
\examples{
\dontrun{
options <- toupper(c("yes","no"))
opt_menu(options)
}

}
