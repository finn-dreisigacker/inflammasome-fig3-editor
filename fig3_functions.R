# =============================================================================
# fig3_functions.R - shared curve + plot functions for Fig. 3 (manuscript
# Figure 5), used by the Shiny editor (fig3_app/app.R) and by
# fig_inflammasome_theory_v5.R. All adjustable values live in fig3_params.csv.
#
# Schematic and speculative: no study data, y not to scale.
# Defaults = v4 (30.09.2026): complex (~7 min) -> GSDMD (~15 min) ->
# IL-1b/IL-18 (~1 h, broad); dashed "sustained" branch for complex and ILs
# (plateau with slight slope up to 24 h, then decline).
# =============================================================================

# single tidyverse packages instead of library(tidyverse): keeps the web
# build (Shinylive/webR) small
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(ggtext)
})

# -----------------------------------------------------------------------------
# Fixed geometry and palette
# -----------------------------------------------------------------------------

x_ex_start <- 0.6          # start of exercise on the axis
x_ex_end   <- 1.4          # end of exercise (= time 0)
x_max      <- 6.4
bin_edges  <- c(1.4, 2.4, 3.4, 4.4, 5.4)   # 0, 15 min, 1 h, 6 h, 24 h

# real time (min after end of exercise) <-> axis units; linear within each
# window, same bins as Figs. 1/2. Exercise is mapped with a nominal 60 min,
# negative minutes = during exercise.
time_breaks <- tibble(
  min = c(-105, -60, 0,   15,  60,  360, 1440, 2880),
  x   = c(0,    0.6, 1.4, 2.4, 3.4, 4.4, 5.4,  6.4)
)
time_to_x <- function(t) approx(time_breaks$min, time_breaks$x, xout = t, rule = 2)$y
x_to_time <- function(x) approx(time_breaks$x, time_breaks$min, xout = x, rule = 2)$y

fmt_time <- function(t) {
  vapply(t, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- round(v)
    if (v < -60) return("Ruhe")
    if (v < 0)   return(sprintf("-%d min", -v))
    if (v == 0)  return("0 (Ende)")
    if (v < 60)  return(sprintf("%d min", v))
    h <- v %/% 60; m <- v %% 60
    if (m == 0) sprintf("%d h", h) else sprintf("%d h %02d min", h, m)
  }, character(1))
}

# Validated palette (all-pairs, light): see v4 header
cols <- c(stress = "#8C8C8C", priming = "#EDA100", complex = "#4A3AA7",
          casp1  = "#4A3AA7", gsdmd   = "#E34948", il      = "#2A78D6",
          sustained = "#555555")
phase_fill <- c("#FBF0E4", "#EAF2F8", "#F4F4F4")
ink <- "#222222"

pt <- function(x) x / ggplot2::.pt
lw <- function(x) x / (ggplot2::.pt * 72.27 / 96)

small <- function(txt, size = 5, col = NULL) {
  paste0("<span style='font-size:", size, "pt",
         if (!is.null(col)) paste0(";color:", col), "'>", txt, "</span>")
}
swatch <- function(key) paste0("<span style='color:", cols[[key]], "'>■</span> ")

# -----------------------------------------------------------------------------
# Defaults (= v4)
# -----------------------------------------------------------------------------

fig3_defaults <- function() {
  list(
    curves = tribble(
      ~key,      ~peak_min, ~height, ~rise, ~fall, ~tail_share, ~tail_fall,
      "stress",   -4,       0.60,    0.35,  0.22,  0,           1,
      "priming",   5,       0.35,    0.45,  1.30,  0,           1,
      "complex",   7,       1.00,    0.40,  0.50,  0.50,        2.60,
      "gsdmd",    15,       0.30,    0.45,  0.50,  0,           1,
      "il",       60,       0.55,    0.70,  1.80,  0,           1
    ),
    sustained = tribble(
      ~key,      ~anchor_min, ~level, ~slope, ~fall_min, ~fall_width,
      "complex",  15,         0.76,   0.05,   1440,      0.45,
      "il",       60,         0.53,   0.03,   1440,      0.45
    ),
    labels_A = tribble(
      ~key,        ~kind,   ~curve,      ~text,                   ~subtitle,
      ~x,   ~y,   ~hjust, ~target_x, ~leader,
      "stress",    "curve", "stress",    "Cellular stress",
      "ATP release, ionic shifts, ROS<br>mitochondrial & mechanical stress",
      0.95, 0.93, 0.5,    1.30,      TRUE,
      "complex",   "curve", "complex",   "NLRP3, ASC, caspase-1", "",
      2.55, 1.10, 0,      2.10,      TRUE,
      "gsdmd",     "curve", "gsdmd",     "GSDMD",                 "",
      3.60, 0.10, 0,      2.75,      TRUE,
      "il",        "curve", "il",        "IL-1β, IL-18",     "",
      3.55, 0.86, 0,      3.30,      TRUE,
      "sustained", "note",  "complex_s",
      "possible sustained response<br>after strenuous or<br>muscle-damaging exercise", "",
      5.55, 0.97, 0.5,    5.00,      TRUE
    ),
    labels_C = tribble(
      ~key,        ~kind,   ~curve,      ~text,                   ~subtitle,
      ~x,   ~y,   ~hjust, ~target_x, ~leader,
      "casp1",     "curve", "casp1",     "caspase-1",             "",
      2.35, 1.62, 0,      2.20,      TRUE,
      "il",        "curve", "il",        "IL-1β, IL-18",     "",
      4.10, 1.66, 0.5,    3.60,      TRUE,
      "sustained", "note",  "complex_s", "possible sustained response", "",
      5.55, 2.86, 0.5,    5.00,      TRUE
    ),
    texts = list(
      phase1_title = "Phase 1", phase1_sub = "Cellular stress",
      phase2_title = "Phase 2", phase2_sub = "Inflammasome-related signalling",
      phase3_title = "Phase 3", phase3_sub = "Return towards baseline",
      bar0 = "Exercise", bar1 = "immediate", bar2 = "early post-exercise",
      bar3 = "early recovery", bar4 = "late recovery", bar5 = "delayed response",
      x_title = "Time after end of exercise", y_title = "schematic, not to scale",
      row1_title = "Cellular stress",
      row1_sub   = "ATP release, ionic shifts, ROS, mitochondrial & mechanical stress",
      row2_title = "Priming",
      row2_sub   = "NF-κB, NLRP3 expression, pro-IL-1β",
      row3_title = "Inflammasome assembly",
      row3_sub   = "NLRP3–ASC assembly, ASC oligomerisation",
      row4_title = "Caspase-1 activation and cytokine processing",
      row4_sub   = "caspase-1 activation, IL-1β/IL-18 maturation",
      row5_title = "Pyroptotic execution",
      row5_sub   = "GSDMD cleavage"
    ),
    settings = list(
      style = "curves", show_sustained = TRUE, show_stress = TRUE,
      phase_b1_min = 3, phase_b2_min = 210, font = "Arial",
      fig_width = 183, height_curves = 100, height_stages = 125
    )
  )
}

# -----------------------------------------------------------------------------
# Parameter file (long format: section, key, param, value)
# -----------------------------------------------------------------------------

char_cols <- c("key", "kind", "curve", "text", "subtitle")

fig3_write_params <- function(P, path) {
  tab_long <- function(df, section) {
    df %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(-key, names_to = "param", values_to = "value") %>%
      mutate(section = section, .before = 1)
  }
  kv_long <- function(l, section) {
    tibble(section = section, key = names(l), param = "value",
           value = vapply(l, as.character, character(1)))
  }
  out <- bind_rows(
    tab_long(P$curves,    "curve"),
    tab_long(P$sustained, "sustained"),
    tab_long(P$labels_A,  "label_curves"),
    tab_long(P$labels_C,  "label_stages"),
    kv_long(P$texts,      "text"),
    kv_long(P$settings,   "setting")
  )
  readr::write_csv(out, path, na = "")
  invisible(path)
}

fig3_read_params <- function(path) {
  D <- fig3_defaults()
  if (!file.exists(path)) return(D)
  L <- readr::read_csv(path, col_types = readr::cols(.default = "c"),
                       na = character(), progress = FALSE)

  tab <- function(sec, default) {
    d <- L %>% filter(section == sec) %>% select(-section)
    if (nrow(d) == 0) return(default)
    w <- d %>% pivot_wider(names_from = param, values_from = value)
    for (n in names(w)) {
      v <- w[[n]]
      if (n %in% char_cols) {
        v[is.na(v)] <- ""
        w[[n]] <- v
      } else {
        w[[n]] <- type.convert(v, as.is = TRUE)
      }
    }
    w
  }
  kv <- function(sec, default) {
    d <- L %>% filter(section == sec)
    out <- default
    for (i in seq_len(nrow(d))) {
      out[[d$key[i]]] <- if (sec == "setting") type.convert(d$value[i], as.is = TRUE) else d$value[i]
    }
    out
  }
  list(
    curves    = tab("curve",        D$curves),
    sustained = tab("sustained",    D$sustained),
    labels_A  = tab("label_curves", D$labels_A),
    labels_C  = tab("label_stages", D$labels_C),
    texts     = kv("text",          D$texts),
    settings  = kv("setting",       D$settings)
  )
}

# -----------------------------------------------------------------------------
# Curves
# -----------------------------------------------------------------------------

bump <- function(x, mu, sd_l, sd_r, amp) {
  s <- ifelse(x < mu, sd_l, sd_r)
  amp * exp(-(x - mu)^2 / (2 * s^2))
}
smoothstep <- function(x, a, b) {
  t <- pmin(1, pmax(0, (x - a) / (b - a)))
  t * t * (3 - 2 * t)
}
onset <- function(x) smoothstep(x, x_ex_start, x_ex_start + 0.5)

# plateau with a slight downward slope from x_b, decline after x_fall
plateau <- function(x, x_b, level, slope, x_fall, sd_fall) {
  y <- level - slope * (x - x_b)
  y * ifelse(x < x_fall, 1, exp(-(x - x_fall)^2 / (2 * sd_fall^2)))
}

# dashed branch: follows the solid curve up to its peak, then the smooth
# maximum of solid and plateau (never dips and re-rises)
sustain <- function(x, solid, x_peak, plat, k = 30) {
  soft_max <- log(exp(k * solid) + exp(k * plat)) / k
  solid + smoothstep(x, x_peak, x_peak + 0.4) * (soft_max - solid)
}

make_curve <- function(x, peak_min, height, rise, fall, tail_share, tail_fall) {
  mu <- time_to_x(peak_min)
  height * ((1 - tail_share) * bump(x, mu, rise, fall, 1) +
              tail_share    * bump(x, mu, rise, tail_fall, 1))
}

fig3_curves <- function(P) {
  x  <- seq(x_ex_start, x_max, length.out = 900)
  on <- onset(x)
  out <- tibble(x = x)
  cv <- P$curves
  for (i in seq_len(nrow(cv))) {
    r <- cv[i, ]
    out[[r$key]] <- make_curve(x, r$peak_min, r$height, r$rise, r$fall,
                               r$tail_share, r$tail_fall) * on
  }
  out$casp1 <- out$complex
  sv <- P$sustained
  for (i in seq_len(nrow(sv))) {
    s <- sv[i, ]
    peak_x <- time_to_x(cv$peak_min[cv$key == s$key])
    plat <- plateau(x, time_to_x(s$anchor_min), s$level, s$slope,
                    time_to_x(s$fall_min), s$fall_width)
    out[[paste0(s$key, "_s")]] <- sustain(x, out[[s$key]], peak_x, plat)
  }
  out
}

# dashed segments only where they depart from the solid curve
fig3_sustained_long <- function(cur, keys = c("complex", "il")) {
  map_dfr(keys, function(k) {
    d <- cur[[paste0(k, "_s")]] - cur[[k]]
    on <- d > 0.004
    tibble(x = cur$x, curve = k, y = cur[[paste0(k, "_s")]], on = on,
           seg = cumsum(c(TRUE, diff(on) != 0)))
  }) %>%
    filter(on) %>%
    mutate(group = paste(curve, seg))
}

curve_at <- function(cur, key, x_at) {
  if (!key %in% names(cur)) return(NA_real_)
  approx(cur$x, cur[[key]], xout = x_at, rule = 2)$y
}

# -----------------------------------------------------------------------------
# Shared plot pieces
# -----------------------------------------------------------------------------

fig3_label_html <- function(kind, curve, text, subtitle, bold = TRUE) {
  pmap_chr(list(kind, curve, text, subtitle), function(k, cu, t, s) {
    if (k == "note") return(small(t, 5, "#555555"))
    key <- sub("_s$", "", cu)
    out <- swatch(key)
    if (nzchar(t)) out <- paste0(out, if (bold) paste0("<b>", t, "</b>") else t)
    if (nzchar(s)) out <- paste0(out, "<br>", small(s))
    out
  })
}

fig3_background <- function(P) {
  b1 <- time_to_x(P$settings$phase_b1_min)
  b2 <- time_to_x(P$settings$phase_b2_min)
  blend <- tibble(from = pmax(c(b1 - 0.3, b2 - 0.2), x_ex_start),
                  to   = pmin(c(b1 + 0.3, b2 + 0.2), x_max),
                  c1 = phase_fill[1:2], c2 = phase_fill[2:3])
  solid <- tibble(x0 = c(x_ex_start, blend$to[1], blend$to[2]),
                  x1 = c(blend$from[1], blend$from[2], x_max),
                  fill = phase_fill)
  slices <- pmap_dfr(blend, function(from, to, c1, c2) {
    e <- seq(from, to, length.out = 31)
    tibble(x0 = head(e, -1), x1 = tail(e, -1) + 0.002,
           fill = colorRampPalette(c(c1, c2))(30))
  })
  bg <- bind_rows(solid, slices) %>% filter(x1 > x0)
  list(
    annotate("rect", xmin = bg$x0, xmax = bg$x1, ymin = -Inf, ymax = Inf,
             fill = bg$fill, colour = NA),
    geom_vline(xintercept = bin_edges, colour = "#CFCFCF",
               linewidth = lw(0.3), linetype = "13")
  )
}

# exercise + manuscript time windows as labelled bars along the baseline
fig3_period_bars <- function(P, y0, y1, gap = 0.02) {
  Tx <- P$texts
  bars <- tibble(
    x0    = c(x_ex_start, bin_edges) + gap,
    x1    = c(x_ex_end, bin_edges[-1], x_max) - gap,
    fill  = c("#E9CBA7", rep(c("#E6E6E6", "#D8D8D8"), length.out = 5)),
    col   = c("#5A4630", rep("#444444", 5)),
    label = c(Tx$bar0, Tx$bar1, Tx$bar2, Tx$bar3, Tx$bar4, Tx$bar5)
  )
  list(
    annotate("rect", xmin = bars$x0, xmax = bars$x1, ymin = y0, ymax = y1,
             fill = bars$fill, colour = NA),
    annotate("text", x = (bars$x0 + bars$x1) / 2, y = (y0 + y1) / 2,
             label = bars$label, size = pt(5), family = P$settings$font,
             colour = bars$col)
  )
}

fig3_theme <- function(P) {
  theme_classic(base_size = 6, base_family = P$settings$font) +
    theme(
      axis.line.x       = element_line(linewidth = lw(0.5)),
      axis.line.y       = element_blank(),
      axis.ticks.x      = element_line(linewidth = lw(0.5)),
      axis.ticks.y      = element_blank(),
      axis.ticks.length = unit(1.2, "pt"),
      axis.text.x       = element_text(size = 5.5, colour = "black"),
      axis.text.y       = element_blank(),
      axis.title        = element_text(size = 6),
      axis.title.x      = element_text(margin = margin(t = 3)),
      axis.title.y      = element_text(margin = margin(r = 3)),
      plot.margin       = margin(4, 6, 2, 2),
      legend.position   = "none"
    )
}

fig3_x_scale <- function() {
  scale_x_continuous(breaks = c(0.3, bin_edges),
                     labels = c("Rest", "0", "15 min", "1 h", "6 h", "24 h"),
                     expand = expansion(0))
}

fig3_leader_layers <- function(lab) {
  lead <- lab %>% filter(as.logical(leader), !is.na(yend))
  list(
    geom_segment(data = lead, aes(x = x, y = y, xend = target_x, yend = yend,
                                  colour = lcol), linewidth = lw(0.4)),
    geom_point(data = lead, aes(x = target_x, y = yend, colour = lcol), size = 0.6)
  )
}

# -----------------------------------------------------------------------------
# Style A: curves
# -----------------------------------------------------------------------------

fig3_plot_curves <- function(P, cur = fig3_curves(P)) {
  S <- P$settings; Tx <- P$texts
  show_s <- isTRUE(as.logical(S$show_sustained))

  long <- cur %>%
    select(x, complex, gsdmd, il) %>%
    pivot_longer(-x, names_to = "curve", values_to = "y") %>%
    mutate(curve = factor(curve, levels = c("gsdmd", "il", "complex")))

  lab <- P$labels_A
  if (!show_s) lab <- filter(lab, kind != "note")
  lab <- lab %>%
    mutate(html = fig3_label_html(kind, curve, text, subtitle),
           lcol = ifelse(kind == "note", "sustained", sub("_s$", "", curve)),
           yend = map2_dbl(curve, target_x, ~ curve_at(cur, .x, .y)))

  p <- ggplot() +
    fig3_background(P) +
    geom_hline(yintercept = 0, colour = "#8C8C8C", linewidth = lw(0.4),
               linetype = "22") +
    fig3_period_bars(P, -0.085, -0.025)

  if (isTRUE(as.logical(S$show_stress))) {
    p <- p +
      geom_area(data = cur, aes(x, stress), fill = cols[["stress"]], alpha = 0.28) +
      geom_line(data = cur, aes(x, stress), colour = cols[["stress"]],
                linewidth = lw(0.6))
  }
  if (show_s) {
    p <- p + geom_line(data = fig3_sustained_long(cur),
                       aes(x, y, colour = curve, group = group),
                       linewidth = lw(1.0), linetype = "32")
  }

  p +
    geom_line(data = long, aes(x, y, colour = curve, group = curve,
                               linewidth = curve)) +
    fig3_leader_layers(lab) +
    geom_richtext(data = lab, aes(x = x, y = y, label = html, hjust = hjust),
                  colour = ink, vjust = 0.5, size = pt(6), family = S$font,
                  lineheight = 1.1, fill = "#FFFFFFCC", label.colour = NA,
                  label.padding = unit(c(0.6, 1.2, 0.6, 1.2), "pt")) +
    scale_colour_manual(values = cols) +
    scale_linewidth_manual(values = c(complex = lw(1.4), gsdmd = lw(1.2),
                                      il = lw(1.4)), guide = "none") +
    fig3_x_scale() +
    scale_y_continuous(expand = expansion(0)) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(-0.10, 1.28),
                    expand = FALSE, clip = "off") +
    labs(x = Tx$x_title, y = Tx$y_title) +
    fig3_theme(P)
}

# -----------------------------------------------------------------------------
# Style C: stages (one row per manuscript stage)
# -----------------------------------------------------------------------------

fig3_plot_stages <- function(P, cur = fig3_curves(P)) {
  S <- P$settings; Tx <- P$texts
  show_s <- isTRUE(as.logical(S$show_sustained))
  H <- 1; sc <- 0.72; n <- 5
  base_of <- function(r) (n - r) * H

  rows <- tibble(row = 1:5,
                 title = unlist(Tx[paste0("row", 1:5, "_title")]),
                 subtitle = unlist(Tx[paste0("row", 1:5, "_sub")]),
                 col = cols[c("stress", "priming", "complex", "il", "gsdmd")]) %>%
    mutate(base = base_of(row))

  tracks <- tribble(
    ~key,      ~row, ~src,      ~rel, ~lwd, ~alpha,
    "stress",  1,    "stress",  1.00, 0.7,  0.30,
    "priming", 2,    "priming", 0.80, 1.3,  0.15,
    "complex", 3,    "complex", 1.00, 1.3,  0.15,
    "casp1",   4,    "complex", 0.80, 0.8,  0.22,
    "il",      4,    "il",      0.65, 1.3,  0.15,
    "gsdmd",   5,    "gsdmd",   0.60, 1.3,  0.15
  )
  if (!isTRUE(as.logical(S$show_stress))) tracks <- filter(tracks, key != "stress")

  track_df <- pmap_dfr(tracks, function(key, row, src, rel, lwd, alpha) {
    raw <- cur[[src]]
    f   <- sc * rel / max(raw, 1e-9)
    b   <- base_of(row)
    out <- tibble(key = key, part = "main", x = cur$x, ymin = b,
                  y = b + raw * f, lwd = lw(lwd), alpha = alpha)
    s_col <- paste0(key, "_s")
    if (show_s && s_col %in% names(cur)) {
      raw_s <- cur[[s_col]]
      on <- raw_s - raw > 0.004
      out <- bind_rows(out, tibble(key = key, part = "sustained", x = cur$x,
                                   ymin = b, y = b + raw_s * f, lwd = lw(1.0),
                                   alpha = 0, on = on,
                                   seg = cumsum(c(TRUE, diff(on) != 0))))
    }
    out
  })
  main_tr <- filter(track_df, part == "main")
  sus_tr  <- if ("on" %in% names(track_df)) {
    filter(track_df, part == "sustained") %>% mutate(group = paste(key, seg))
  } else track_df[0, ]

  track_at <- function(curve, x_at) {
    if (grepl("_s$", curve)) {
      d <- track_df %>% filter(part == "sustained", key == sub("_s$", "", curve))
    } else {
      d <- main_tr %>% filter(key == curve)
    }
    if (nrow(d) == 0) return(NA_real_)
    approx(d$x, d$y, xout = x_at, rule = 2)$y
  }

  titles <- rows %>%
    mutate(label = paste0("<span style='color:", col, "'>■</span> <b>",
                          title, "</b><br>", small(subtitle, 5, "#555555")),
           y = base + 0.93)

  lab <- P$labels_C
  if (!show_s) lab <- filter(lab, kind != "note")
  lab <- lab %>%
    mutate(html = fig3_label_html(kind, curve, text, subtitle, bold = FALSE),
           lcol = ifelse(kind == "note", "sustained", sub("_s$", "", curve)),
           yend = map2_dbl(curve, target_x, track_at))

  p <- ggplot() +
    fig3_background(P) +
    geom_hline(yintercept = rows$base, colour = "#9A9A9A", linewidth = lw(0.4)) +
    fig3_period_bars(P, -0.11, -0.03) +
    geom_ribbon(data = main_tr, aes(x = x, ymin = ymin, ymax = y, fill = key,
                                    alpha = alpha, group = key))
  if (show_s && nrow(sus_tr) > 0) {
    p <- p + geom_line(data = filter(sus_tr, on),
                       aes(x, y, colour = key, group = group),
                       linewidth = lw(1.0), linetype = "32")
  }

  p +
    geom_line(data = main_tr, aes(x, y, colour = key, group = key,
                                  linewidth = lwd)) +
    fig3_leader_layers(lab) +
    geom_richtext(data = titles, aes(x = 0.05, y = y, label = label),
                  colour = ink, hjust = 0, vjust = 1, size = pt(6),
                  family = S$font, lineheight = 1.15, fill = NA,
                  label.colour = NA, label.padding = unit(0, "pt")) +
    geom_richtext(data = lab, aes(x = x, y = y, label = html, hjust = hjust),
                  colour = ink, vjust = 0.5, size = pt(5), family = S$font,
                  fill = "#FFFFFFCC", label.colour = NA,
                  label.padding = unit(c(0.6, 1.2, 0.6, 1.2), "pt")) +
    scale_colour_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_alpha_identity() +
    scale_linewidth_identity() +
    fig3_x_scale() +
    scale_y_continuous(expand = expansion(0)) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(-0.12, n * H + 0.02),
                    expand = FALSE, clip = "off") +
    labs(x = Tx$x_title, y = Tx$y_title) +
    fig3_theme(P)
}

# -----------------------------------------------------------------------------
# Header, assembly, export
# -----------------------------------------------------------------------------

fig3_header <- function(P) {
  S <- P$settings; Tx <- P$texts
  b1 <- time_to_x(S$phase_b1_min); b2 <- time_to_x(S$phase_b2_min)
  ph <- tibble(
    x0 = c(x_ex_start, b1, b2), x1 = c(b1, b2, x_max), fill = phase_fill,
    label = c(sprintf("<b>%s</b><br>%s", Tx$phase1_title, Tx$phase1_sub),
              sprintf("<b>%s</b><br>%s", Tx$phase2_title, Tx$phase2_sub),
              sprintf("<b>%s</b><br>%s", Tx$phase3_title, Tx$phase3_sub))
  ) %>% mutate(xc = (x0 + x1) / 2)

  ggplot(ph) +
    geom_rect(aes(xmin = x0 + 0.03, xmax = x1 - 0.03, ymin = 0, ymax = 1),
              fill = ph$fill, colour = "#BDBDBD", linewidth = lw(0.4)) +
    geom_richtext(aes(x = xc, y = 0.5, label = label), size = pt(5.5),
                  family = S$font, colour = "#333333", lineheight = 1.05,
                  fill = NA, label.colour = NA) +
    scale_x_continuous(expand = expansion(0)) +
    scale_y_continuous(expand = expansion(0)) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1), expand = FALSE,
                    clip = "off") +
    labs(x = NULL, y = Tx$y_title) +
    theme_classic(base_size = 6, base_family = S$font) +
    theme(axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(),
          # invisible y title keeps the header aligned with the main panel
          axis.title.y = element_text(size = 6, colour = "transparent",
                                      margin = margin(r = 3)),
          plot.margin = margin(0, 6, 2, 2))
}

fig3_main <- function(P, cur = fig3_curves(P)) {
  if (identical(P$settings$style, "stages")) fig3_plot_stages(P, cur) else fig3_plot_curves(P, cur)
}

fig3_heights <- function(P) {
  S <- P$settings
  if (identical(S$style, "stages")) {
    list(total = S$height_stages, rel = c(0.07, 1))
  } else {
    list(total = S$height_curves, rel = c(0.10, 1))
  }
}

fig3_figure <- function(P, cur = fig3_curves(P)) {
  h <- fig3_heights(P)
  fig3_header(P) / fig3_main(P, cur) + plot_layout(heights = h$rel)
}

fig3_export <- function(P, dir, stem = "Fig_inflammasome_theory") {
  h <- fig3_heights(P)
  fig <- fig3_figure(P)
  base <- file.path(dir, paste0(stem, "_", P$settings$style))
  ggsave(paste0(base, ".png"), fig, width = P$settings$fig_width, height = h$total,
         units = "mm", dpi = 600, device = ragg::agg_png, bg = "white")
  ggsave(paste0(base, ".svg"), fig, width = P$settings$fig_width, height = h$total,
         units = "mm", device = svglite::svglite, bg = "white")
  invisible(paste0(base, c(".png", ".svg")))
}
