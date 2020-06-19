\name{interpTools-package}
\alias{interpTools}
\title{interpTools: Evaluate the statistical performance of time series interpolators}

\description{
\code{interpTools} contains functions for generating artificial time series, simulating gaps, and interpolating the missing observations, with tools for subsequent analysis and visualization of the interpolators' statistical performance.
}

\details{
Built-in interpolation algorithms: 
\itemize{
\item Nearest Neighbor (NN)
\item Linear Interpolation (LI)
\item Natural Cubic Spline (NCS)
\item FMM Cubic Spline (FMM)
\item Hermite Cubic Spline (HCS)
\item Stineman Interpolation (SI)
\item Kalman - ARIMA (KAF)
\item Kalman - StructTS (KKSF)
\item Last Observation Carried Forward (LOCF)
\item Next Observation Carried Backward (NOCB)
\item Simple Moving Average (SMA)
\item Linear Weighted Moving Average (LWMA)
\item Exponential Weighted Moving Average (EWMA)
\item Replace with Mean (RMEA)
\item Replace with Median (RMED)
\item Replace with Mode (RMOD)
\item Replace with Random (RRND)
\item Hybrid Wiener Interpolator (HWI)
}
(or the user may pass in their own interpolating function, so long as its returned value is a single numeric vector.)

A list of built-in performance metrics can be found in the package files (\code{~/metric_definitions.pdf})


}

\author{
\itemize{
\item Sophie Castel (0000-0001-9086-0917)
\item Wesley Burr (0000-0002-2058-1899)
}
}

\references{
Castel, Sophie Terra Marguerite (2020). A Framework for Testing Time Series Interpolators (Master of Science). Trent University, Peterborough, Ont. https://digitalcollections.trentu.ca/objects/etd-814

Burr, W.S. (2012). Air Pollution and Health: Time Series Tools and Analysis (PhD Thesis). Queen's University, Kingston, Ont. https://qspace.library.queensu.ca/handle/1974/7617
}

\seealso{
\itemize{
\item tsinterp
}
}
