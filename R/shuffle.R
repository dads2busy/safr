#' Create a Fisher–Yates Shuffle.
#'
#' The Fisher–Yates shuffle is an algorithm for generating a random permutation of
#' a finite sequence—in plain terms, the algorithm shuffles the sequence. The algorithm
#' effectively puts all the elements into a hat; it continually determines the next
#' element by randomly drawing an element from the hat until no elements remain.
#' The algorithm produces an unbiased permutation: every permutation is equally likely.
#'
#' @param x A vector of values to be shuffled.
#' @param key_seeds Optional. A vector of numeric values between 0 and 1.
#'     Used to create a repeatable shuffle.
#' @export
#' @examples
#' key_seeds <- fy_shuffle(seq(from = 0, to = 1, by = .001))
#' alphanum <- strsplit(alphanum_str, "")[[1]]
#' alphanumkey <- fy_shuffle(alphanum, key_seeds)
fy_shuffle <-
    function (x, key_seeds = character())
    {
        n <- length(x)
        i <- n

        if (length(key_seeds) > 0) {
            while (i > 0) {
                j <- floor(key_seeds[i] * i + 1)
                if (i != j) {
                    temp <- x[i]
                    x[i] <- x[j]
                    x[j] <- temp
                }
                i <- i - 1
            }
        } else {
            while (i > 0) {
                j <- floor(runif(1) * i + 1)
                if (i != j) {
                    temp <- x[i]
                    x[i] <- x[j]
                    x[j] <- temp
                }
                i <- i - 1
            }
        }

        return(x)
    }
