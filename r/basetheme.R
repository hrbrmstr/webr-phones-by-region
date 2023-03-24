basetheme <- function(...) {

  if(nargs() == 0) {
    return(current_theme())
  }

  pars    <- list(...)
  oldpars <- current_theme()

  if(is.null(names(pars))) names(pars) <- rep("", length(pars))

  if(names(pars)[1] == "") {
    if(is.null(pars[[1]])) {
      grDevices::palette("default")
      oldpars <- list()
      pars <- pars[-1]
    } else if(is.list(pars[[1]])) {
      pars <- c(pars[[1]], pars[-1])
    } else if(is.character(pars[[1]])) {
      pars <- c(get_theme(pars[[1]]), pars[-1])
    }
  }

  if(any(names(pars) == "")) {
    warning("unnamed arguments were discarded")
    pars <- pars[names(pars) != ""]
  }

  if(any(duplicated(names(pars)))) {
    pars <- pars[!duplicated(names(pars), fromLast=TRUE)]
  }

  template <- get_theme("default")
  if(length(pars) > 0 & any(!names(pars) %in% names(template))) {
    warning("unsupported arguments were discarded")
    pars <- pars[names(pars) %in% names(template)]
  }


  pars <- c(oldpars[!names(oldpars) %in% names(pars)], pars)
  pars <- Filter(Negate(is.null), pars)

  hook <- getHook("before.plot.new")
  hook$par     <- setParFun(pars)
  hook$palette <- setPalFun(pars)
  setHook("before.plot.new", hook, "replace")
  hook <- getHook("plot.new")
  hook$rect <- setRecFun(pars)
  setHook("plot.new", hook, "replace")

  invisible(pars)
}

num2col <- function(x, pal, ref=range(x, na.rm=TRUE), NAcol) {
  if(missing(pal)) {
    pal <- basetheme()$palette.numbers
  }
  if(is.null(pal)) {
    pal <- c("blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red")
  }

  if(length(pal) == 1) {
    pal <- colshade(pal, c(-0.8, -0.5, -0.3, 0, 0.3, 0.5, 0.8))
  }

  if(missing(NAcol)) {
    NAcol <- basetheme()$palette.missing
  }
  if(is.null(NAcol)) {
    NAcol <- "#B9BBB6"
  }

  ramp <- grDevices::colorRamp(pal)
  min <- min(ref, na.rm=TRUE)
  max <- max(ref, na.rm=TRUE)

  fun <- function(x) {
    if(is.factor(x)) x <- as.numeric(x)
    x[x < min | x > max] <- NA
    x <- x - min
    x <- x / (max-min)

    cols <- x
    cols[] <- NAcol
    cols[!is.na(x)] <- grDevices::rgb(ramp(x[!is.na(x)]), maxColorValue=255)
    cols
  }

  if(missing(x)) {
    fun
  } else {
    fun(x)
  }
}

lab2col <- function(x, pal, ref=x, NAcol) {
  if(missing(pal)) {
    pal <- basetheme()$palette.labels
  }
  if(is.null(pal)) {
    pal <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0", "#66E3D9", "#00FF7F")
  }

  if(missing(NAcol)) {
    NAcol <- basetheme()$palette.missing
  }
  if(is.null(NAcol)) {
    NAcol <- "#B9BBB6"
  }

  pal <- rep_len(pal, length(pal) * 3)
  pal <- colshade(pal, rep(c(0, 0.5, -0.5), each=length(pal)/3))

  vals <- unique(ref)
  if(length(vals) > length(pal))
    warning("number of unique values exceeds the number of colors in the palette by more than 3 times")
  pal  <- rep_len(pal, length(vals))

  fun <- function(x) {
    if(is.numeric(x)) x <- as.character(x)

    cols <- pal[match(x, vals)]
    cols[is.na(cols)] <- NAcol
    cols
  }

  if(missing(x)) {
    fun
  } else {
    fun(x)
  }
}

colshade <- function(cols, frac=0) {
  if(any(frac < -1 | frac > 1)) stop('"frac" must be between -1 and 1')
  if(length(frac) <= length(cols)) {
    frac <- rep_len(frac, length(cols))
  } else {
    cols <- rep_len(cols, length(frac))
  }

  addTint <- function(col, tint) {
    grDevices::rgb(t(grDevices::col2rgb(col)) + tint*t((255-grDevices::col2rgb(col))), maxColorValue=255)
  }
  addShade <- function(col, shade) {
    grDevices::rgb(t(grDevices::col2rgb(col)) * (1-shade), maxColorValue=255)
  }

  cols[frac <= 0] <- addTint(cols[frac <= 0], -frac[frac <= 0])
  cols[frac > 0]  <- addShade(cols[frac > 0], frac[frac > 0])
  cols
}

basetheme_default <- function() {
  palette <- c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")
  palette_numbers <- c()
  palette_labels  <- c()
  palette_missing <- c()

  list(fg              = "black",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "transparent",
       bty             = "o",
       cex             = 1,
       cex.axis        = 1,
       cex.lab         = 1,
       cex.main        = 1.2,
       cex.sub         = 1,
       col             = "black",
       col.axis        = "black",
       col.lab         = "black",
       col.main        = "black",
       col.sub         = "black",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 1,
       font.main       = 2,
       font.sub        = 1,
       lab             = c(5, 5, 7),
       las             = 0,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = "solid",
       lwd             = 1,
       mgp             = c(3, 1, 0),
       pch             = 1,
       tck             = NA,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_minimal <- function() {
  palette         <- c("#4D4D4D", "#F15854", "#60BD68", "#5DA5DA", "#66E3D9", "#B276B2", "#DECF3F", "#B4B4B4", "#FAA43A", "#8D4B08")
  palette_numbers <- c("#4F61A1", "#5DA5DA", "#66E3D9", "#7FFF7F", "#FFF25C", "#FAA43A", "#F15854")
  palette_labels  <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0", "#66E3D9", "#00FF7F")
  palette_missing <- "#B4B4B4"

  list(fg              = "black",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "white",
       bty             = "n",
       cex             = 1,
       cex.axis        = 0.8,
       cex.lab         = 1,
       cex.main        = 1.2,
       cex.sub         = 1,
       col             = "black",
       col.axis        = "black",
       col.lab         = "black",
       col.main        = "black",
       col.sub         = "black",
       family          = "serif",
       font            = 1,
       font.axis       = 1,
       font.lab        = 1,
       font.main       = 2,
       font.sub        = 2,
       lab             = c(4,4,7),
       las             = 1,
       lend            = 0,
       ljoin           = 0,
       lmitre          = 10,
       lty             = 1,
       lwd             = 1,
       mgp             = c(2.5,0.2,0),
       pch             = 20,
       tck             = 0,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
  )
}

basetheme_void <- function() {
  palette         <- c("#4D4D4D", "#F15854", "#60BD68", "#5DA5DA", "#66E3D9", "#B276B2", "#DECF3F", "#B4B4B4", "#FAA43A", "#8D4B08")
  palette_numbers <- c("#4F61A1", "#5DA5DA", "#66E3D9", "#7FFF7F", "#FFF25C", "#FAA43A", "#F15854")
  palette_labels  <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0", "#66E3D9", "#00FF7F")
  palette_missing <- "#B4B4B4"

  list(fg              = "black",
       adj             = 0.5,
       ann             = FALSE,
       bg              = "white",
       bty             = "n",
       cex             = 1,
       cex.axis        = 1.1,
       cex.lab         = 1.1,
       cex.main        = 1.5,
       cex.sub         = 1,
       col             = "black",
       col.axis        = "black",
       col.lab         = "black",
       col.main        = "black",
       col.sub         = "black",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 1,
       font.main       = 2,
       font.sub        = 2,
       lab             = c(5,5,7),
       las             = 1,
       lend            = 0,
       ljoin           = 0,
       lmitre          = 10,
       lty             = 1,
       lwd             = 2,
       mgp             = c(3,0.5,0),
       pch             = 19,
       tck             = 0,
       xaxs            = "r",
       xaxt            = "n",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "n",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_clean <- function() {
  palette         <- c("#4D4D4D", "#F15854", "#60BD68", "#5DA5DA", "#66E3D9", "#B276B2", "#DECF3F", "#B4B4B4", "#FAA43A", "#8D4B08")
  palette_numbers <- c("#4F61A1", "#5DA5DA", "#66E3D9", "#7FFF7F", "#FFF25C", "#FAA43A", "#F15854")
  palette_labels  <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F15854", "#B276B2", "#8D4B08", "#DECF3F", "#F17CB0", "#66E3D9", "#00FF7F")
  palette_missing <- "#B4B4B4"

  list(fg              = "black",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "white",
       bty             = "o",
       cex             = 1,
       cex.axis        = 0.7,
       cex.lab         = 0.8,
       cex.main        = 1,
       cex.sub         = 0.8,
       col             = "black",
       col.axis        = "black",
       col.lab         = "black",
       col.main        = "black",
       col.sub         = "black",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 1,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = "solid",
       lwd             = 1,
       mgp             = c(2, 0.5, 0),
       pch             = 20,
       tck             = -0.01,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_brutal <- function() {
  palette         <- c("#303643", "#BF616A", "#8FA87A", "#5E81AC", "#66CDAA", "#D070B9", "#E7D202", "#BCC3CE", "#F49538", "#7D5329")
  palette_numbers <- c("#5E81AC", "#0D98BA", "#66CDAA", "#98FB98", "#E7D202", "#F49538", "#BF616A")
  palette_labels  <- c("#5E81AC", "#8FA87A", "#BF616A", "#E7D202", "#7D5329", "#F49538", "#66CDAA", "#D070B9", "#98FB98", "#FCA3B7")
  palette_missing <- "#BCC3CE"

  list(fg              = "#3b4252",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "white",
       bty             = "o",
       cex             = 1,
       cex.axis        = 1.2,
       cex.lab         = 1.5,
       cex.main        = 2,
       cex.sub         = 1.5,
       col             = "#3b4252",
       col.axis        = "#3b4252",
       col.lab         = "#3b4252",
       col.main        = "#3b4252",
       col.sub         = "#3b4252",
       family          = "mono",
       font            = 2,
       font.axis       = 1,
       font.lab        = 2,
       font.main       = 2,
       font.sub        = 2,
       lab             = c(4,4,7),
       las             = 1,
       lend            = 0,
       ljoin           = 0,
       lmitre          = 10,
       lty             = 1,
       lwd             = 4,
       mgp             = c(2.5,0.25,0),
       pch             = 15,
       tck             = 0,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 4,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_ink <- function() {
  palette         <- c("#000000", "#FF0000", "#008B00", "#0000FF", "#00FFFF", "#EE1289", "#ECC700", "#778899", "#FF8C00", "#8B4513")
  palette_numbers <- c("#0000FF", "#20B2AA", "#00FFFF", "#00FF00", "#ECC700", "#FF8C00", "#FF0000")
  palette_labels  <- c("#FF0000", "#0000FF", "#008B00", "#8B4513", "#ECC700", "#A020F0", "#FF8C00", "#EE1289", "#00FFFF", "#00FF00")
  palette_missing <- "#778899"

  list(fg              = "blue",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "navajowhite",
       bty             = "o",
       cex             = 0.8,
       cex.axis        = 1,
       cex.lab         = 1,
       cex.main        = 1.5,
       cex.sub         = 1,
       col             = "blue",
       col.axis        = "blue",
       col.lab         = "blue",
       col.main        = "blue",
       col.sub         = "blue",
       family          = "",
       font            = 3,
       font.axis       = 3,
       font.lab        = 4,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = "dotted",
       lwd             = 2,
       mgp             = c(2, 0.5, 0),
       pch             = 4,
       tck             = -0.01,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = "gray80",
       rect.density    = 10,
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_dark <- function() {
  palette         <- c("#BAC1BF", "#ED2939", "#66C84D", "#3870C2", "#40E0D0", "#FC0FCD", "#F2D63B", "#777B7E", "#F2A83B", "#8A6842")
  palette_numbers <- c("#3870C2", "#00B7EB", "#40E0D0", "#66C84D", "#F2D63B", "#FC6600", "#ED2939")
  palette_labels  <- c("#3870C2", "#8AC1D4", "#F2A83B", "#66C84D", "#DA2E20", "#FFFD55", "#EB8677", "#A6EB99", "#DF89E8", "#8A6842")
  palette_missing <- c("#777B7E")

  list(fg              = "#7E848C",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "#2E3947",
       bty             = "n",
       cex             = 0.8,
       cex.axis        = 1,
       cex.lab         = 1,
       cex.main        = 1.5,
       cex.sub         = 1,
       col             = "#BEBEBE",
       col.axis        = "#7E848C",
       col.lab         = "#BEBEBE",
       col.main        = "#EFF0F2",
       col.sub         = "#737D89",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 2,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = 1,
       lwd             = 1,
       mgp             = c(3, 0.7, 0),
       pch             = 19,
       tck             = -0.01,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_deepblue <- function() {
  palette         <- c("#B9BBB6", "#C4512C", "#42BA90", "#3A82E4", "#00CCCC", "#EC5578", "#BEE948", "#777B7E", "#C38A13", "#795C32")
  palette_numbers <- c("#3A82E4", "#00B7EB", "#00CCCC", "#25CE7A", "#BEE948", "#C38A13", "#C4512C")
  palette_labels  <- c("#42BA90", "#BEE948", "#3A82E4", "#D25D38", "#E9A820", "#9041BA", "#795C32", "#EC5578", "#00B7EB", "#FEE12B")
  palette_missing <- "#777B7E"

  list(fg              = "#CCCCCC",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "#002E49",
       bty             = "l",
       cex             = 1,
       cex.axis        = 0.8,
       cex.lab         = 0.8,
       cex.main        = 1,
       cex.sub         = 0.8,
       col             = "#BEBEBE",
       col.axis        = "#BEBEBE",
       col.lab         = "#FFFFFF",
       col.main        = "#FFFFFF",
       col.sub         = "#326D87",
       family          = "",
       font            = 1,
       font.axis       = 1,
       font.lab        = 2,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = 1,
       lwd             = 2,
       mgp             = c(3, 0.7, 0),
       pch             = 21,
       tck             = -0.015,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

basetheme_royal <- function() {

  palette         <- c("#E5DDCE", "#B33941", "#2E8B57", "#1175BB", "#4AC6AE", "#E93FBC", "#DBA520", "#777B7E", "#EF7215", "#997950")
  palette_numbers <- c("#1175BB", "#5097A4", "#4AC6AE", "#88BE6D", "#E4CD05", "#EF7215", "#B33941")
  palette_labels  <- c("#7852A9", "#E93FBC", "#B33941", "#1175BB", "#2E8B57", "#DBA520", "#EF7215", "#4AC6AE", "#997950", "#E27069")
  palette_missing <- "#777B7E"

  palette <- c("#1175BB", "#B33941", "#EE6450", "#7D40AE", "#266D1F", "#FADA5E", "#5A4D41")

  list(fg              = "#DACEC4",
       adj             = 0.5,
       ann             = TRUE,
       bg              = "#2D2D3B",
       bty             = "o",
       cex             = 1,
       cex.axis        = 0.8,
       cex.lab         = 0.8,
       cex.main        = 1.2,
       cex.sub         = 0.8,
       col             = "#DACEC4",
       col.axis        = "#DACEC4",
       col.lab         = "#FFFFFF",
       col.main        = "#FFFFFF",
       col.sub         = "#DACEC4",
       family          = "serif",
       font            = 1,
       font.axis       = 1,
       font.lab        = 2,
       font.main       = 2,
       font.sub        = 3,
       lab             = c(5, 5, 7),
       las             = 1,
       lend            = "round",
       ljoin           = "round",
       lmitre          = 10,
       lty             = 1,
       lwd             = 1.2,
       mgp             = c(3, 0.7, 0),
       pch             = 21,
       tck             = -0.015,
       xaxs            = "r",
       xaxt            = "s",
       xpd             = FALSE,
       yaxs            = "r",
       yaxt            = "s",
       rect.col        = NA,
       rect.density    = numeric(),
       rect.angle      = 45,
       rect.lwd        = 1,
       rect.lty        = 1,
       rect.border     = NA,
       palette         = palette,
       palette.numbers = palette_numbers,
       palette.labels  = palette_labels,
       palette.missing = palette_missing
       )
}

list_themes <- function() {
  c(brutal   = basetheme_brutal,
    clean    = basetheme_clean,
    default  = basetheme_default,
    ink      = basetheme_ink,
    minimal  = basetheme_minimal,
    dark     = basetheme_dark,
    deepblue = basetheme_deepblue,
    royal    = basetheme_royal,
    void     = basetheme_void
    )
}

current_theme <- function() {
  hook1 <- getHook("before.plot.new")
  hook2 <- getHook("plot.new")

  theme <- list()
  if(is.function(hook1$par))
    theme <- c(theme, hook1$par(FALSE))
  if(is.function(hook1$palette))
    theme <- c(theme, hook1$palette(FALSE))
  if(is.function(hook2$rect))
    theme <- c(theme, hook2$rect(FALSE))

  theme
}

get_theme <- function(theme=NULL) {
  themes <- list_themes()
  if (theme %in% names(themes)) {
    themes[[theme]]()
  } else {
    stop("no such theme: ", theme, ".\nAvailable themes: ",
         paste(names(list_themes()), collapse=", ")
    )
  }
}

setParFun <- function(plist) {
  parList <- plist
  parList[grep("^palette\\.?", names(parList))] <- NULL
  parList[grep("^rect\\.", names(parList))] <- NULL
  function(set=TRUE) {
    if(set) {
      parList$new <- graphics::par('new')
      do.call(graphics::par, parList)
    } else {
      parList
    }
  }
}

setPalFun <- function(plist) {
  plist <- plist[grep("^palette\\.?", names(plist))]
  if(!is.null(plist$palette)) {
    function(set=TRUE) {
      if(set)
        grDevices::palette(value=plist$palette)
      else
        plist
    }
  } else {
    NULL
  }
}

setRecFun <- function(plist) {
  plist <- plist[grep("^rect\\.", names(plist))]
  if(length(plist) > 0) {
    function(set=TRUE) {
      if(set) {
        rlist <- list(xleft   = graphics::par("usr")[1], ybottom=graphics::par("usr")[3],
                      xright  = graphics::par("usr")[2], ytop=graphics::par("usr")[4],
                      col     = ifelse(is.null(plist$rect.col), NA, plist$rect.col),
                      density = ifelse(is.null(plist$rect.density), numeric(), plist$rect.density),
                      angle   = ifelse(is.null(plist$rect.angle), 35, plist$rect.angle),
                      lwd     = ifelse(is.null(plist$rect.lwd), 1, plist$rect.lwd),
                      lty     = ifelse(is.null(plist$rect.lty), 1, plist$rect.lty),
                      border  = ifelse(is.null(plist$rect.border), NA, plist$rect.border)
                      )
        do.call(graphics::rect, rlist)
      } else {
        plist
      }
    }
  } else {
    NULL
  }
}

TRUE