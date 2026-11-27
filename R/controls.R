# Custom inputs and small UI pieces. The radio/checkbox groups are plain
# HTML that Shiny's built-in bindings pick up (by class), styled in
# www/app.css.

# small flags for the market switch, drawn as inline SVG (flag emoji show as
# plain letters on Windows)
star_points <- function(cx, cy, r_out, r_in, n){
  a <- -pi / 2 + seq(0, by = pi / n, length.out = 2 * n)
  r <- rep(c(r_out, r_in), n)
  paste(sprintf("%.2f,%.2f", cx + r * cos(a), cy + r * sin(a)), collapse = " ")
}
flag_svg <- list(
  # Australia, 2:1: Union Jack, Commonwealth Star, Southern Cross
  au = paste0(
    '<svg class="cflag" viewBox="0 0 120 60" width="22" height="11" aria-hidden="true">',
    '<defs><clipPath id="cflag-au-c"><rect width="60" height="30"/></clipPath>',
    '<clipPath id="cflag-au-t"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath></defs>',
    '<rect width="120" height="60" fill="#012169"/>',
    '<g clip-path="url(#cflag-au-c)">',
    '<path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/>',
    '<path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#cflag-au-t)" stroke="#C8102E" stroke-width="4"/>',
    '<path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/>',
    '<path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g>',
    '<g fill="#fff">',
    '<polygon points="', star_points(30, 45, 9, 4.2, 7), '"/>',
    '<polygon points="', star_points(90, 50, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(90, 10, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(76, 24, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(103, 21, 4.6, 2.1, 7), '"/>',
    '<polygon points="', star_points(97, 32, 2.6, 1.2, 5), '"/></g></svg>'),
  # United States, 19:10: 13 stripes and a canton (stars as dots at this size)
  us = paste0(
    '<svg class="cflag" viewBox="0 0 190 100" width="21" height="11" aria-hidden="true">',
    '<rect width="190" height="100" fill="#fff"/>',
    paste0(sprintf('<rect y="%.3f" width="190" height="%.3f" fill="#B22234"/>',
                   (seq(0, 12, by = 2)) * 100 / 13, 100 / 13), collapse = ""),
    '<rect width="76" height="53.85" fill="#3C3B6E"/>',
    '<g fill="#fff">',
    paste0(apply(expand.grid(x = seq(7, 69, length.out = 6), y = seq(6, 48, length.out = 5)), 1,
                 function(p) sprintf('<circle cx="%.1f" cy="%.1f" r="2.8"/>', p[1], p[2])), collapse = ""),
    '</g></svg>'))

# a value restored from a bookmarked link (or the default), kept only if
# every part of it is one of the allowed choices: the link's text is the
# visitor's to edit
restored <- function(id, default, allowed){
  v <- restoreInput(id, default)
  if(!is.atomic(v) || !length(v) || !all(as.character(v) %in% allowed)) default else as.character(v)
}

# Australia / United States (a shiny radio input); the page title follows it
# (www/app.js)
country_switch <- function(id){
  selected <- restored(id, "au", names(countries))
  tags$div(id = id, class = "shiny-input-radiogroup country-switch", role = "radiogroup",
           `aria-label` = "Market",
           lapply(names(countries), function(k){
             bid <- paste0(id, "_", k)
             tagList(tags$input(type = "radio", class = "btn-check", name = id, id = bid, value = k,
                                autocomplete = "off", checked = if(k == selected) NA),
                     tags$label(`for` = bid, HTML(flag_svg[[k]]), countries[[k]]))
           }))
}

# small icon switch for overlay / stacked (a shiny radio input)
layout_switch <- function(id, selected){
  icon_overlay <- HTML('<svg width="18" height="14" viewBox="0 0 18 14" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" stroke-linejoin="round"><path d="M1 10 L6 5 L10 8 L17 2"/><path d="M1 12 L6 9 L11 11 L17 7"/></svg>')
  icon_stacked <- HTML('<svg width="18" height="14" viewBox="0 0 18 14" fill="none" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"><path d="M1 2.5 H17"/><path d="M1 7 H17"/><path d="M1 11.5 H17"/></svg>')
  opts <- list(list("overlay", "Overlay: all series on one chart", icon_overlay),
               list("stacked", "Stacked: one row per series", icon_stacked))
  selected <- restored(id, selected, c("overlay", "stacked"))
  tags$div(id = id, class = "shiny-input-radiogroup layout-switch", role = "radiogroup",
           `aria-label` = "Chart layout",
           lapply(opts, function(o){
             bid <- paste0(id, "_", o[[1]])
             tagList(tags$input(type = "radio", class = "btn-check", name = id, id = bid,
                                value = o[[1]], autocomplete = "off",
                                checked = if(o[[1]] == selected) NA),
                     tags$label(`for` = bid, title = o[[2]], `aria-label` = o[[2]], o[[3]]))
           }))
}

# a small labelled switch between a few text options (a shiny radio input),
# e.g. the Valuation tab's starting point
text_switch <- function(id, label, choices, selected){
  selected <- restored(id, selected, unname(choices))
  tags$div(class = "text-switch-wrap",
           tags$span(class = "period-label", id = paste0(id, "-label"), label),
           tags$div(id = id, class = "shiny-input-radiogroup text-switch", role = "radiogroup",
                    `aria-labelledby` = paste0(id, "-label"),
                    lapply(names(choices), function(lab){
                      val <- choices[[lab]]; bid <- paste0(id, "_", val)
                      tagList(tags$input(type = "radio", class = "btn-check", name = id, id = bid,
                                         value = val, autocomplete = "off", checked = if(val == selected) NA),
                              tags$label(`for` = bid, lab))
                    })))
}

# quick year ranges ending in the latest year (handled in the browser)
year_presets <- function(){
  div(class = "year-presets", role = "group", `aria-label` = "Quick year ranges",
      lapply(c(1, 3, 5, 10, 20, 50), function(n)
        tags$button(type = "button", `data-span` = n, `aria-pressed` = "false",
                    title = if(n == 1) "Last year" else paste("Last", n, "years"),
                    `aria-label` = if(n == 1) "Last year" else paste("Last", n, "years"),
                    paste0(n, "Y"))),
      tags$button(type = "button", `data-span` = 0, `aria-pressed` = "false", "All"))
}

# a plain year <select> (kept in step with the years slider in the browser)
year_select <- function(id, years, selected, label){
  tags$select(id = id, class = "form-select", `aria-label` = label,
              lapply(years, function(y) tags$option(value = y, selected = if(y == selected) NA, y)))
}

# segmented control that behaves as a shiny radio input (input$view)
segmented <- function(id, choices, selected, extras = list()){
  selected <- restored(id, selected, unname(choices))
  tags$div(
    id = id, class = "shiny-input-radiogroup seg", role = "radiogroup",
    lapply(names(choices), function(lab){
      val <- choices[[lab]]
      bid <- paste0(id, "_", val)
      tagList(
        tags$input(type = "radio", class = "btn-check", name = id, id = bid,
                   value = val, autocomplete = "off",
                   checked = if(val == selected) NA),
        tags$label(class = "btn", `for` = bid, extras[[val]]$before, lab, title = extras[[val]]$title)
      )
    })
  )
}

# series toggles: a shiny checkbox group (input$show) styled as legend cards
# (www/app.js never lets the last visible one be switched off)
series_toggles <- function(id){
  on <- restored(id, series$key, series$key)
  tags$div(
    id = id, class = "shiny-input-checkboxgroup series", role = "group",
    `aria-label` = "Series shown on the chart",
    lapply(seq_len(nrow(series)), function(i){
      bid <- paste0(id, "_", series$key[i])
      tagList(
        tags$input(type = "checkbox", class = "btn-check", name = id, id = bid,
                   value = series$key[i], autocomplete = "off",
                   checked = if(series$key[i] %in% on) NA),
        tags$label(class = "series-card", `for` = bid,
                   style = paste0("--cl:", series$colour[i], ";--cd:", series$colour_dark[i]),
                   title = "Click to show or hide",
                   div(class = "name", span(class = "dot"),
                       span(class = "lbl-full", series$label[i]),
                       span(class = "lbl-short", series$short[i])),
                   div(class = "figs",
                       span(class = "value", textOutput(paste0("val_", series$key[i]), inline = TRUE)),
                       span(class = "rate", textOutput(paste0("rate_", series$key[i]), inline = TRUE))),
                   uiOutput(paste0("ctx_", series$key[i]), class = "ctx")))
    })
  )
}

source_links <- function(x) tagList(lapply(seq_along(x), function(i){
  tagList(if(i > 1) " \u00b7 ",
          tags$a(href = x[[i]][[2]], target = "_blank", rel = "noopener", x[[i]][[1]]))
}))
