
# quickly filter poll results to a specific series_no
pollresults = function(series_filter) {
  
  results = readxl::read_xlsx(here::here("whiteboardpollresults.xlsx")) %>%
    filter(series_no == series_filter) %>%
    mutate(
      across(everything(), ~replace_na(.x, 0)),
      votes = hq_votes+rtp_votes) %>%
    group_by(series_no,series,date,question) %>%
    mutate(
      N = sum(votes),
      share = votes / N
      ) %>%
    ungroup()
  
}

# make a scale_fill_manual of a specified number of random colors
scale_fill_random = function(number) {
  
  number <- as.integer(number)
  
  # Generate random hues for distinct colors; keep chroma/luminance moderate
  hues <- stats::runif(number, min = 0, max = 360)
  cols <- grDevices::hcl(h = hues, c = 70, l = 60, fixup = TRUE)
  
  ggplot2::scale_fill_manual(values = cols)
  
}

# bar plot for a specific question in a set of data
simple_bar_question = function(data, question_selection) {
  
  df = data %>% filter(question == question_selection)
  colors_needed = length(unique(df$option))
  
  ggplot(df) +
    geom_col(aes(x=option,y=votes,fill=option), stat="identity") +
    geom_text(aes(x=option,y=votes,label=votes), vjust=-0.5, color = "black", size = 3) +
    scale_fill_random(colors_needed) +
    facet_grid(~question) +
    labs(fill="",
         x="",
         y="Votes") +
    theme_minimal() +
    theme(legend.position = "none")
  
}

#' Return the scalar of a vector
#'
#' @examples
#' one(c(1))
#' one(numeric(0))
#' typeof(one(numeric(0)))
#' \dontrun {
#' one(c(1, 2))
#' }
#' @export
#'
one <- function(x) {
  if (length(x) == 1) x else
    # NA value typed the same as x
    if (length(x) == 0) x[NA] else
      stop("Input `x` to `one` function must not have length > 1")
}

#' Return a single value from a vector, for which a different condition holds
#'
#' Intended to help with EAV calculations that potentially have length 0, ruining data frame operations.
#' @param x a vector from which to return results
#' @param p a vector of booleans (or other `[` indices?) length of x
#' @export
`%forwhich%` <- function(x, p) {
  one(x[p])
}

