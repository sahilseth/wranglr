% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/need_pkgs.R
\name{need_pkgs}
\alias{need_pkgs}
\title{A Persistent way to load packages. If they are not found in the library, this downloads them.
This ensures that say a knitr document would work across systems.}
\usage{
need_pkgs(x, update_pkgs = FALSE)
}
\arguments{
\item{x}{a list of R packages}
}
\description{
A Persistent way to load packages. If they are not found in the library, this downloads them.
This ensures that say a knitr document would work across systems.
}
