
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
  hues <- seq(15, 375, length.out = number + 1)[seq_len(number)]
  lums <- rep(c(55, 70), length.out = number)
  cols <- grDevices::hcl(h = sample(hues), c = 80, l = lums, fixup = TRUE)
  
  ggplot2::scale_fill_manual(values = cols)
  
}

# make a scale_color_manual of a specified number of random colors
scale_color_random = function(number) {
  
  number <- as.integer(number)
  
  # Generate random hues for distinct colors; keep chroma/luminance moderate
  hues <- seq(15, 375, length.out = number + 1)[seq_len(number)]
  lums <- rep(c(55, 70), length.out = number)
  cols <- grDevices::hcl(h = sample(hues), c = 80, l = lums, fixup = TRUE)
  
  ggplot2::scale_color_manual(values = cols)
  
}

# Faceted overview of every question in a poll: one panel per question, one
# color per option, vote counts + share printed above each bar, two panels
# per row (as many rows as needed to cover every question).
faceted_bar_plot = function(data, ncol = 2) {
  data = data %>%
    mutate(question = factor(question, levels = unique(question)))

  n_options = length(unique(data$option))
  palette <- palette.colors(n = n_options, palette = "Polychrome")

  ggplot(data, aes(x = fct_reorder(option, votes), y = votes, fill = option)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = paste0(votes, " (", percent(share, accuracy = 0.1), ")")),
              vjust = -0.5, size = 3.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_random(n_options) +
    # scale_color_manual(values = palette) +
    labs(x = NULL, y = "Votes") +
    theme_minimal(base_size = 13) +
    facet_wrap(~ question, scales = "free_x", ncol = ncol) +
    theme(strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 20, hjust = 1))
}

# Grouped bar chart for polls where every question shares the same category
# domain (e.g. day of week): one x position per shared category, one bar per
# question, dodged and colored by question. Faceting by question (the usual
# faceted_bar_plot) buries the category-to-category comparison that actually
# matters for this shape of poll, so this puts the shared category on the
# x-axis instead. Bars are dodged tightly (4+ questions per category), so
# labels are percent-only and rotated vertical to avoid colliding with the
# neighboring bar's label; exact vote counts belong in the summary table.
grouped_bar_by_question = function(data, category_order = NULL) {
  if (!is.null(category_order)) {
    data = data %>% mutate(option = factor(option, levels = category_order))
  }

  data = data %>%
    group_by(question) %>%
    mutate(is_top = share == max(share)) %>%
    ungroup()

  n_q = length(unique(data$question))

  data %>%
    ggplot(aes(x = option, y = share, fill = question, group = question)) +
    geom_col(position = position_dodge2(width = 0.8, padding = 0.1), width = 0.75) +
    geom_text(aes(label = percent(share, accuracy = 1), fontface = if_else(is_top, "bold", "plain")),
              position = position_dodge2(width = 0.8, padding = 0.1),
              angle = 90, hjust = -0.15, size = 3) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.22))) +
    scale_fill_random(n_q) +
    labs(x = NULL, y = "Share of that question's votes", fill = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))
}

# Summarize every two-option question in a poll: leader/trailer votes & shares,
# margin, and an exact binomial test against a 50/50 split.
summarize_two_option = function(data) {
  data %>%
    group_by(question) %>%
    filter(n() == 2) %>%
    group_modify(~ {
      N = unique(.x$N)
      ordered = .x %>% arrange(desc(votes))
      lead  = ordered %>% slice(1)
      trail = ordered %>% slice(2)
      bt = binom.test(lead$votes, N, p = 0.5)

      tibble(
        N             = N,
        leader        = lead$option,
        leader_votes  = lead$votes,
        leader_share  = lead$share,
        trailer       = trail$option,
        trailer_votes = trail$votes,
        trailer_share = trail$share,
        margin_votes  = lead$votes - trail$votes,
        margin_share  = lead$share - trail$share,
        p_value_50_50 = bt$p.value,
        ci_lower      = bt$conf.int[1],
        ci_upper      = bt$conf.int[2],
        flips_to_flip = floor((lead$votes - trail$votes) / 2) + 1L
      )
    }) %>%
    ungroup()
}

# Nicely formatted kable of a summarize_two_option() table
binom_summary_table = function(summary_df) {
  summary_df %>%
    transmute(
      question,
      N,
      `Leader (votes)`  = sprintf("%s (%d)", leader, leader_votes),
      `Trailer (votes)` = sprintf("%s (%d)", trailer, trailer_votes),
      `Leader share`    = percent(leader_share, 0.1),
      `Margin (votes)`  = margin_votes,
      `Margin (points)` = percent(margin_share, 0.1),
      `p vs 50/50`      = signif(p_value_50_50, 3),
      `Leader 95% CI`   = sprintf("[%.2f, %.2f]", ci_lower, ci_upper),
      `Votes to flip`   = flips_to_flip
    ) %>%
    kableExtra::kable()
}

# Leader share +/- 95% CI, one row per question (dashed line marks 50/50).
# Y-axis label includes the leader's answer, same "question: option" format
# as hq_rtp_plot().
leader_ci_plot = function(summary_df) {
  n_options = length(unique(summary_df$question))

  summary_df %>%
    mutate(label = paste0(question, ": ", leader)) %>%
    ggplot(aes(y = fct_rev(label), x = leader_share, color = label)) +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.15, linewidth = 0.8, width = 0.2) +
    geom_vline(xintercept = 0.5, linetype = 2, color = "gray60") +
    scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_color_random(n_options) +
    labs(x = "Leader's share of votes", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}

# Waffle chart (one square per vote) for a single question, any number of options
waffle_question = function(data, question_selection, cols = 9) {
  df = data %>% filter(question == question_selection) %>% arrange(desc(votes))
  N = unique(df$N)
  rows = ceiling(N / cols)

  tibble(
    id = 1:N,
    option = factor(rep(df$option, df$votes), levels = df$option)
  ) %>%
    mutate(row = (id - 1) %/% cols + 1,
           col = (id - 1) %% cols + 1) %>%
    ggplot(aes(x = col, y = rows - row + 1, fill = option)) +
    geom_tile(color = "white", linewidth = 0.5, width = 0.95, height = 0.95) +
    scale_fill_random(nrow(df)) +
    coord_equal() +
    labs(x = NULL, y = NULL, fill = NULL, title = question_selection) +
    theme_void() +
    theme(legend.position = "bottom")
}

# Chi-square goodness-of-fit summary for every multi-option (3+) question in a
# poll -- the generalization of summarize_two_option() for questions where a
# 50/50 binomial test doesn't apply (e.g. "pick your favorite day of the
# week"). Tests the observed split against "every option equally likely."
summarize_multi_option = function(data) {
  data %>%
    group_by(question) %>%
    filter(n() > 2) %>%
    group_modify(~ {
      Nq = unique(.x$N)
      k  = nrow(.x)
      max_votes = max(.x$votes)
      top_rows  = .x %>% filter(votes == max_votes) %>% arrange(option)
      ct  = suppressWarnings(chisq.test(.x$votes))
      top_vs_chance = suppressWarnings(binom.test(max_votes, Nq, p = 1 / k))

      tibble(
        N              = Nq,
        k              = k,
        top_option     = paste(top_rows$option, collapse = " & "),
        top_is_tie     = nrow(top_rows) > 1,
        top_votes      = max_votes,
        top_share      = unique(top_rows$share),
        zero_options   = sum(.x$votes == 0),
        chisq_stat     = unname(ct$statistic),
        df             = unname(ct$parameter),
        p_vs_uniform   = ct$p.value,
        p_top_vs_chance = top_vs_chance$p.value
      )
    }) %>%
    ungroup()
}

# Kable of summarize_multi_option()
multi_option_table = function(summary_df) {
  summary_df %>%
    transmute(
      question,
      N,
      `# options`        = k,
      `Top pick (votes)` = sprintf("%s (%d)", top_option, top_votes),
      `Top share`        = percent(top_share, 0.1),
      `Zero-vote options` = zero_options,
      `Chi-sq (df)`      = sprintf("%.1f (%d)", chisq_stat, df),
      `p vs uniform`     = signif(p_vs_uniform, 3),
      `p: top vs chance` = signif(p_top_vs_chance, 3)
    ) %>%
    kableExtra::kable()
}

# Per-option share +/- 95% CI for a single multi-option question, dashed line
# marks the "everyone equally likely" chance level (1/k) instead of 50/50.
multi_option_ci_plot = function(data, question_selection) {
  df = data %>% filter(question == question_selection) %>% arrange(desc(votes))
  k = nrow(df)
  chance = 1 / k

  df %>%
    rowwise() %>%
    mutate(
      ci = list(binom.test(votes, N, p = chance)$conf.int),
      ci_lower = ci[1],
      ci_upper = ci[2]
    ) %>%
    ungroup() %>%
    mutate(label = fct_reorder(option, share)) %>%
    ggplot(aes(y = label, x = share, color = label)) +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.15) +
    geom_vline(xintercept = chance, linetype = 2, color = "gray60") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_color_random(k) +
    labs(x = "Share of votes", y = NULL, title = question_selection) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "none")
}

# Feasible overlap bounds between the leading options of two *different* questions,
# since we never ask people to link their answers across questions on the whiteboard.
# Assumes the smaller question's voters are a subset of the larger question's voters,
# and also reports the overlap we'd expect if the two choices were unrelated.
overlap_bounds = function(a_total, N_a, b_total, N_b) {
  m = min(N_a, N_b)

  if (N_a >= N_b) {
    larger_total = a_total; larger_N = N_a; smaller_total = b_total
  } else {
    larger_total = b_total; larger_N = N_b; smaller_total = a_total
  }

  exclude = larger_N - m
  x_min = max(0, larger_total - exclude)
  x_max = min(larger_total, m)

  tibble(
    m                 = m,
    min_overlap       = max(0, x_min + smaller_total - m),
    expected_overlap  = m * (a_total / N_a) * (b_total / N_b),
    max_overlap       = min(x_max, smaller_total)
  )
}

# Compare HQ vs RTP on the leading option of every two-option question, via a
# two-proportion test -- are the two offices voting the same way?
hq_rtp_compare = function(data) {
  data %>%
    group_by(question) %>%
    filter(n() == 2, sum(rtp_votes) > 0) %>%
    group_modify(~ {
      df  = .x %>% arrange(desc(votes))
      top = df %>% slice(1)
      hq_N  = sum(df$hq_votes)
      rtp_N = sum(df$rtp_votes)
      pt = prop.test(x = c(top$hq_votes, top$rtp_votes), n = c(hq_N, rtp_N))

      tibble(
        option    = top$option,
        hq_votes  = top$hq_votes,  hq_N  = hq_N,  hq_share  = top$hq_votes / hq_N,
        rtp_votes = top$rtp_votes, rtp_N = rtp_N, rtp_share = top$rtp_votes / rtp_N,
        gap       = (top$hq_votes / hq_N) - (top$rtp_votes / rtp_N),
        p_value   = pt$p.value
      )
    }) %>%
    ungroup()
}

# Dumbbell-style plot comparing HQ share vs RTP share for each question's leader
hq_rtp_plot = function(compare_df) {
  compare_df %>%
    mutate(label = paste0(question, ": ", option)) %>%
    ggplot(aes(y = fct_rev(label))) +
    geom_segment(aes(x = hq_share, xend = rtp_share, yend = label), color = "gray70", linewidth = 1) +
    geom_point(aes(x = hq_share, color = "HQ"), size = 4) +
    geom_point(aes(x = rtp_share, color = "RTP"), size = 4) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_color_manual(values = c("HQ" = "#EDE939", "RTP" = "#104275"), name = NULL) +
    labs(x = "Share of office's votes for the overall leader", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# HQ vs RTP for a multi-option question: a chi-square test of independence on
# the office x option contingency table. Options with zero votes from BOTH
# offices are dropped first -- a fully empty column makes every expected
# count in it zero too, which is a division by zero in the chi-square
# statistic, and a column with no votes at all carries no information about
# whether the offices differ anyway.
hq_rtp_compare_multi = function(data) {
  data %>%
    group_by(question) %>%
    filter(n() > 2, sum(rtp_votes) > 0) %>%
    group_modify(~ {
      kept = .x %>% filter(hq_votes + rtp_votes > 0)
      tab  = rbind(kept$hq_votes, kept$rtp_votes)
      ct   = suppressWarnings(chisq.test(tab))

      tibble(
        options_tested  = nrow(kept),
        options_dropped = nrow(.x) - nrow(kept),
        chisq_stat      = unname(ct$statistic),
        df              = unname(ct$parameter),
        p_value         = ct$p.value
      )
    }) %>%
    ungroup()
}

# Long HQ/RTP share table (one row per question x option) for a multi-option
# poll, in the same shape hq_rtp_plot() already expects -- so the existing
# dumbbell plot works unmodified across as many rows per question as needed.
hq_rtp_shares_multi = function(data) {
  data %>%
    group_by(question) %>%
    filter(n() > 2, sum(rtp_votes) > 0) %>%
    mutate(
      hq_N      = sum(hq_votes),
      rtp_N     = sum(rtp_votes),
      hq_share  = hq_votes / hq_N,
      rtp_share = rtp_votes / rtp_N
    ) %>%
    ungroup() %>%
    select(question, option, hq_votes, hq_N, hq_share, rtp_votes, rtp_N, rtp_share)
}

# Cumulative HQ-RTP Alignment Index: correlation between HQ's and RTP's vote
# shares across every option, from the start of RTP participation (series 12,
# Earth Day) through the given series, inclusive. Used to track whether the
# two offices are drifting apart or staying in sync as more polls come in.
cumulative_alignment = function(through_series, since_series = 12) {
  df = readxl::read_xlsx(here::here("whiteboardpollresults.xlsx")) %>%
    filter(series_no >= since_series, series_no <= through_series, status == "complete") %>%
    mutate(across(c(hq_votes, rtp_votes), ~ replace_na(.x, 0))) %>%
    group_by(series_no, question) %>%
    filter(sum(rtp_votes) > 0) %>%
    mutate(hq_share = hq_votes / sum(hq_votes), rtp_share = rtp_votes / sum(rtp_votes)) %>%
    ungroup()

  cor(df$hq_share, df$rtp_share)
}

# Count of questions (since RTP joined) where HQ's and RTP's individual winners
# differed from each other, through the given series (inclusive) -- i.e. the
# combined "winner" only held because the offices' opposite leans canceled out.
flip_count_so_far = function(through_series, since_series = 12) {
  readxl::read_xlsx(here::here("whiteboardpollresults.xlsx")) %>%
    filter(series_no >= since_series, series_no <= through_series, status == "complete") %>%
    mutate(across(c(hq_votes, rtp_votes), ~ replace_na(.x, 0))) %>%
    group_by(series_no, question) %>%
    filter(sum(rtp_votes) > 0) %>%
    summarize(
      hq_winner  = option[which.max(hq_votes)],
      rtp_winner = option[which.max(rtp_votes)],
      .groups = "drop"
    ) %>%
    summarize(n = sum(hq_winner != rtp_winner)) %>%
    pull(n)
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

