#' exp_decay()
#'
#' Multiplies incoming waveform by an exponential decay curve.
#'
#' @param incoming Required. A data.frame of the incoming waveform.
#' @param decay_tau Required. Rate of the decay.
#'
#' @return Returns a data.frame in the format of the waveform() function describing the product of the incoming waveform with the exponential decay specified by this function.
#' @export
#' @importFrom dplyr mutate
#' @importFrom utils tail
#'
#' @examples
#' waveform() %>%
#'   cos_sum(freqs = c(1, 3), amplitudes = NULL, phases = NULL) %>%
#'   exp_decay(decay_tau = 0.5)
exp_decay <- function(incoming, decay_tau) {
  stopifnot("incoming must be a dataframe with waveform data." = is.data.frame(incoming))

  sr <- length(incoming$.sec) / tail(incoming$.sec, n = 1)
  new_value <- incoming$.value * exp(-incoming$.sample / decay_tau / sr)

  dplyr::mutate(incoming, ".value" = new_value)
}
