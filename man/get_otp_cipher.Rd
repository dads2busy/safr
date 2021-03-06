% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/otp.R
\name{get_otp_cipher}
\alias{get_otp_cipher}
\title{Create a Cipher of a Message using a One-Time Pad.}
\usage{
get_otp_cipher(message_str, otp_str,
  alphanum_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
}
\arguments{
\item{message_str}{A string to be ciphered.}

\item{otp_str}{One-time pad (string)}

\item{alphanum_str}{Optional. The alphanumeric string to be used creating the message cipher.}
}
\description{
Create a Cipher of a Message using a One-Time Pad.
}
\examples{
get_otp_cipher("Talley", get_otp())
}
