\name{flag_false_detections}
\alias{flag_false_detections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Flag False Detections - False Detection Analysis (FDA)
%%  ~~function to do ... ~~
}
\description{ False Detection Analysis flagging detections that are deemed to be potential false positives. False positive detections are flagged in the detection dataframe based on the criteria defined by Pincock (2012) based on the presence of at least one short interval between successive detections and there are more short intervals compared to long intervals. Single detections of an ID are also flagged as potential false positives.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
flag_false_detections(det, transmitter_id, time_var, transmission_delay, short_multiplier, long_multiplier)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{det}{Dataframe containing detection records. If proceeded by parse_log_files, use det dataframe.}
  \item{transmitter_id}{variable describing the transmitter ID in det.}
  \item{time_var}{Variable of type POSIXct describing time in the det dataframe.}
  \item{transmission_daley}{numeric value of type int describing the nominal delay interval of the transmitter IDs in det.}
  \item{short_multiplier}{numeric value of type int describing the lower threshold of acceptable intervals between successive detections, default = 30 x transmission_delay.}
  \item{long_multiplier}{numeric value of type int describing the upper threshold of acceptable intervals between successive detections, default = 720 x transmission_delay.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Appends the false_positive_flag variable to the det dataframe to identify detections as true positive (1) or false positive (2).
}
\references{
For details on FDA see:

Pincock, D. G. (2012): False Detections: What they are and how to remove them from detection data. Document #: DOC-004691 Version 03, April 17, 2012

https://support.fishtracking.innovasea.com/s/downloads?tabset-59625=1

}
\author{
H. Pederson (hugh.pederson@innovasea.com)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
