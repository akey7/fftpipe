% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cos_sum.R
\name{cos_sum}
\alias{cos_sum}
\title{cos_sum}
\usage{
cos_sum(incoming, freqs, amplitudes = NULL, phases = NULL)
}
\arguments{
\item{incoming}{Required. A data.frame of the incoming waveform.}

\item{freqs}{Required. A double or vector of doubles specifying frequencies in Hz of component functions. If NULL, cos_sum() assumes amplitudes of 1.0 for all components. Defaults to NULL.}

\item{amplitudes}{Optional. A double or vector of doubles specifying amplitudes of the component functions. If specified, the length must match the length of the frequencies vector. If NULL, cos_sum() assumes amplitudes of 1.0 for all components. Defaults to NULL.}

\item{phases}{Optional. A double or vector of doubles specifying phase shifts of the components in radians. If specified, the length must match the length of the freqs vector. If NULL, cos_sum() assumes a phase shift of 0.0 for all components. Defaults to NULL.}
}
\value{
Returns a data.frame in the format of the waveform() function describing the sum of the incoming waveform with the sum of the components specified by this function.
}
\description{
Builds a waveform by summing cos() functions together. It mixes the results of the cos() sum with the incoming waveform by a summation. It obeys the sample rate and duration specified by the incoming waveform.
}
\examples{
waveform(duration_s = 1.0, sr = 100) \%>\%
  cos_sum(freqs = c(1.0, 3.0), amplitudes = NULL, phases = NULL)
}
