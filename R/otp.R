#' Create a One-Time Pad.
#'
#' In cryptography, the one-time pad (OTP) is an encryption technique
#' that cannot be cracked, but requires the use of a one-time pre-shared
#' key the same size as, or longer than, the message being sent. In this
#' technique, a plaintext is paired with a random secret key (also referred
#' to as a one-time pad). Then, each bit or character of the plaintext is
#' encrypted by combining it with the corresponding bit or character from
#' the pad using modular addition. If the key is truly random, is at least
#' as long as the plaintext, is never reused in whole or in part, and is
#' kept completely secret, then the resulting ciphertext will be impossible
#' to decrypt or break ("One-Time Pad (OTP)". Cryptomuseum.com. Retrieved 2014-03-17.).
#'
#' @param alphanum_str Optional. The alphanumeric string to be used creating the one-time pad.
#' @export
#' @examples
#' get_otp()
get_otp <-
    function(alphanum_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") {
        key_seeds <- fy_shuffle(seq(from = 0, to = 1, by = .001))
        alphanum <- strsplit(alphanum_str, "")[[1]]
        alphanumkey <- fy_shuffle(alphanum, key_seeds)
        paste0(alphanumkey, collapse = "")
    }


#' Create a Cipher of a Message using a One-Time Pad.
#'
#' @param message_str A string to be ciphered.
#' @param otp_str One-time pad (string)
#' @param alphanum_str Optional. The alphanumeric string to be used creating the message cipher.
#' @import data.table
#' @export
#' @examples
#' get_cipher("Talley", get_otp())
get_cipher <- function(message_str, otp_str, alphanum_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") {
    message <- strsplit(toupper(message_str), "")[[1]]
    alphanum <- strsplit(alphanum_str, "")[[1]]
    alphanumkey <- strsplit(otp_str, "")[[1]]
    dt <- data.table::data.table(k = alphanumkey)
    dt <- dt[, kpos := grep(k, alphanum), k][1:length(message)]
    dt[, m := message]
    dt[, mpos := grep(m, alphanum), m]
    dt[, newpos := (mpos + kpos) %% 36 + 1]
    dt[, newm := alphanum[newpos]]
    dt[, paste0(newm, collapse="")][[1]]
}


#' Create a data.table of message ciphers using a One-Time Pad.
#'
#' @param message_vct A character vector of strings to be ciphered.
#' @param otp_str One-time pad (string)
#' @param alphanum_str Optional. The alphanumeric string to be used creating the message ciphers.
#'
#' @import data.table
#' @export
#' @examples
#' names <- c("Billy", "Bob", "Thornton")
#' get_ciphers(names, get_otp())
get_ciphers <- function(message_vct, otp_str, alphanum_str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") {
    if (is.character(message_vct)) {
        dt <- data.table::data.table(m = message_vct)
        dt[, newm := get_cipher(m, otp_str), m]
        attr(dt, "otp") <- otp_str
        dt
    } else {
        print("Not a character vector")
    }
}
