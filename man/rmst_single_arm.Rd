% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{rmst_single_arm}
\alias{rmst_single_arm}
\title{Calculate RMST for a single cut-off time point}
\usage{
rmst_single_arm(
  time_var,
  event_var,
  tau,
  group_label = "Single Group",
  alpha = 0.05
)
}
\arguments{
\item{time_var}{A numeric vector of follow up time.}

\item{event_var}{A numeric or integer vector of the status indicator;
0=alive 1=event.}

\item{tau}{A value of pre-defined cut-off time point.}

\item{group_label}{A character of customized treatment group name.}

\item{alpha}{A numeric value of the significant level for RMST
confidence interval. Default is 0.05.}
}
\value{
A data frame of
- Cutoff time: same as `tau`;
- Group label: same as `group_label`;
- Estimated RMST;
- Variance, standard error, and CIs of the estimated RMST;
- Number of events.
}
\description{
Copied from Merck/simtrial R package
Accessed source code from https://rdrr.io/github/Merck/simtrial/src/R/rmst.R on December 12, 2024.
}
\examples{
\dontrun{
data(ex1_delayed_effect, "simtrial")
data_single_arm <- ex1_delayed_effect[ex1_delayed_effect$trt == 1, ]
rmst_single_arm(
  time_var = data_single_arm$month,
  event_var = data_single_arm$evntd,
  tau = 10,
  group_label = "Treatment 1",
  alpha = 0.05
)
}
}
