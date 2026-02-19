###############################################################################
# CARITAS-CDA: Shiny-App Functionality
# =============================================================================
# 
# Author: oliver.gardi@gmail.com
# Date:   August 2023

# CRU TS 0.5 grid cell series
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/ge/grid05/cells/N2.5E32.5/tmp/N4.25E31.75.tmp.txt
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/ge/grid05/cells/N2.5E32.5/pre/N4.25E31.75.pre.txt




# Default location ============================================================
# from downloaded location Lucerne.zip

# location <- readRDS("./data/Lucerne/location.rds")
# location$clim <- normalize.data(load.data("./data/Lucerne"))
# saveRDS(location, "default_location.rds")


# Test functions ==============================================================
# load libraries from app.R and source this file

# data <- normalize.data(load.data("./data/Lucerne"))

# data <- normalize.data(load.data("../../data/kmni/Terekeka"))
# plot.timeseries(data, "tas")
# plot.climograph(data, "tas")

# crop = getCrop("Maize"); crop@GMAX <- 210


# cs <- clim.suit(data, crop)
# plot.ecocrop(cs, period="Mar", index="combined", set.obs="cru4", set.mod="cmip6")
# plot.pdate(cs, period="rainfed", sce="ssp585", set.obs="cru4", set.mod="cmip6", suit=FALSE)
# plot.ccor(cs, getCrop("Maize"), period="rainfed", index="temp", sce="ssp370", set.obs="cru4", set.mod="cmip6")
  


# Libraries =========================================================

library(httr)        # for interacting with KNMI web interface
library(httr2)
library(rvest)

library(tidyr)
library(dplyr)
options(dplyr.summarise.inform = FALSE) # no dplyr info if summarize is performed on groups
library(data.table) # for moving window frollsum / frollapply
library(ggplot2)
library(lubridate)  # for days_in_month

# Variables =========================================================

CREDITS <- "1.2.3 / Januar 2026 / oliver.gardi@gmail.com"

DATA.DIR <- "./data"

NORM.PERIOD <- 1991:2020    # WMO Norm period, for normalization of climate projections

# KNMI fields
FIELDS <- c("cru4_tas"        = "cru4_tmp",
            "cru4_pre"        = "cru4_pre",
            "cmip6_tas_ssp126" = "cmip6_tas_mon_mod_ssp126",        # Mean monthly temperature
            "cmip6_tas_ssp245" = "cmip6_tas_mon_mod_ssp245",        
            "cmip6_tas_ssp370" = "cmip6_tas_mon_mod_ssp370",
            "cmip6_tas_ssp585" = "cmip6_tas_mon_mod_ssp585",
            "cmip6_pre_ssp126" = "cmip6_pr_mon_mod_ssp126",
            "cmip6_pre_ssp245" = "cmip6_pr_mon_mod_ssp245",
            "cmip6_pre_ssp370" = "cmip6_pr_mon_mod_ssp370",
            "cmip6_pre_ssp585" = "cmip6_pr_mon_mod_ssp585"
            )

# Variables
VARS   <- c("Mean temperature (ºC)" = "tas", 
            "Mean precipitation (mm/day)" = "pre"
            )

CCORS   <- c("Temperature" = "tas", 
             "Precipitation" = "pre"
             )

# Labels (for figures)
LAB.TEMP <- "Temperature (ºC)"
LAB.PREC <- "Precipitation (mm/day)"

# Sets / Scenarios
SCENS <- c("Observed"           = "cru4", 
           "+1.8ºC (ssp126)"    = "cmip6_ssp126", 
           "+2.7ºC (ssp245)"    = "cmip6_ssp245",
           "+3.6ºC (ssp370)"    = "cmip6_ssp370", 
           "+4.4ºC (ssp585)"    = "cmip6_ssp585")

## caritas cd colors
cach.blue <- rgb(102, 145, 165, max=255)
cach.blue20 <- rgb(102, 145, 165, alpha=.5*255, max=255)
cach.red <- rgb(219, 0, 27, alpha=255, max=255)
cach.red20 <- rgb(219, 0, 27, alpha=.5*255, max=255)
cach.brown20 <- rgb(110, 95, 60, alpha=.2*255, max=255)
cach.brown <- rgb(110, 95, 60, alpha=255, max=255)

# Helper functions ==================================================

notify <- function(msg, id = NULL) {
  showNotification(msg, id = id, type="message", duration = NULL, closeButton = FALSE)
}

month.seq <- function(start, end){
  start.id <- which(month.abb == start)
  end.id <- which(month.abb == end)
  if(end.id >= start.id) {
    period <- month.abb[start.id:end.id]
  } else {
    period <- month.abb[c(start.id:12, 1:end.id)]
  }
  return(period)
}

# Data functions ====================================================

# extract and download a set of fields from KNMI and store it in outdir
.download.knmi <- function(fields, lat, lon, outdir){
  
  max.download  <- 100
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)
  
  base.req <- request("https://climexp.knmi.nl/get_index.cgi") %>%
         req_body_form(email = "someone@somewhere",
                      maskmetadata = "",
                      lat1 = format(lat, nsmall = 1),
                      lat2 = format(lat, nsmall = 1),
                      lon1 = format(lon, nsmall = 1),
                      lon2 = format(lon, nsmall = 1),
                      intertype = "nearest",
                      gridpoints = "false",
                      masktype = "all",
                      standardunits = "standardunits")
  reqs <- lapply(fields, function(field) req_body_form(base.req, field=field) %>% req_retry(max_tries = 3, retry_on_failure = TRUE))
  cat(file=stderr(), "request fields ... \n")
  resps <- reqs
  fail = rep(TRUE, length(resps))
  i = 1
  while(sum(fail) > 0 & i <= 3) {
    Sys.sleep(2)
    cat(file=stderr(), paste0(i, ". KNMI request (", sum(fail), "/", length(resps), " fields) ... \n"))
    resps[fail] <- reqs[fail] %>% req_perform_parallel(on_error="continue", max_active = 8)
    fail <- vapply(resps, inherits, "error", FUN.VALUE = logical(1))
    i = i + 1
  }
  if(sum(fail) > 0) {
    cat(file=stderr(), paste0(i, ". KNMI request (", sum(fail), "/", length(resps), " fields) ... \n"))
    resps[fail] <- reqs[fail] %>% req_perform_sequential()
    fail <- vapply(resps, inherits, "error", FUN.VALUE = logical(1))
    if(sum(fail) > 0) stop("Persistent error in KNMI data retrieval")
  }
  names(resps) <- names(fields)
  cat(file=stderr(), "download ... \n")
  sourcefiles <- c()
  destfiles <- c()
  for(i in 1:length(resps)) {
    field <- names(resps[i])
    fielddir <- paste0(outdir, "/", field)
    if(!dir.exists(fielddir)) dir.create(fielddir)
    resp <- resps[[i]]
    urls <- html_nodes(resp_body_html(resp), "a") %>% html_attr("href")
    if(sum(grepl("[.]dat$", urls) == 1)) {               # one single file (cru4)
      file <- grep("[.]dat$", urls, value=TRUE)
      filename <- basename(file)
      sourcefiles <- c(sourcefiles, paste0("http://climexp.knmi.nl/data/", filename))
      destfiles   <- c(destfiles,   paste0(fielddir, "/", filename))
    } else {                                             # a bunch of files (cmip6)
      files <-  urls %>% grep("^rawdata[.]cgi", ., value=TRUE) %>%
        paste0("http://climexp.knmi.nl/", .) %>% GET() %>% content(encoding="UTF-8") %>% html_nodes("a") %>% html_attr("href") %>%
        grep("[.]dat$", ., value=TRUE)
        sourcefiles <- c(sourcefiles, paste0("http://climexp.knmi.nl/data/", basename(files)))
        destfiles   <- c(destfiles,  paste0(fielddir, "/", basename(files)))
    }
  }
  io <- as.data.frame(cbind(sourcefiles, destfiles))  # split into chunks of n files for download
  chunks <- split(io, ceiling(1:nrow(io)/max.download))
  lapply(chunks, function(chunk) download.file(chunk$sourcefiles, chunk$destfiles, method="libcurl", extra="--retry", quiet = TRUE))
}

# get new data for a specific location and return data directory (~240s / 20s for cached location)
download.data <- function(lat, lon, id=NULL) {
  outdir <- tempfile()
  cat(file=stderr(), "Get new loc from KNMI (", lat, "N/", lon, "E) ... \n", sep="")
  t <- system.time(.download.knmi(FIELDS, lat, lon, outdir))
  cat(file=stderr(), "done (", round(t["elapsed"]), "s)\n", sep="")
  return(outdir)
}

# parsing KNMI data files
.parse.data <- function(filename) {
  
  field <- sub("^i(.*)_-*[[:digit:]]*[.]*[[:digit:]]*E_.*$", "\\1", basename(filename))
  field.name <- names(FIELDS)[which(FIELDS==field)]
  set <- strsplit(field.name, "_")[[1]][1]
  var <- strsplit(field.name, "_")[[1]][2]
  sce <- strsplit(field.name, "_")[[1]][3]
  
  mod <- sub("^.*([[:digit:]]{3})[.]dat$", "\\1", basename(filename))
  if(mod == basename(filename)) {
    mod <- NA
  } else {
    mod <- paste0(set, "_", sce, "_", mod)
  }
  
  read.table(filename) %>%
    setNames(c("year", month.abb)) %>%
    filter(set=="cru4" & year > 1900 | year >= NORM.PERIOD[1]) %>%
    pivot_longer(-year, names_to="month", values_to="value") %>%
    mutate(month = factor(month, levels=month.abb),
           set = !!set, var = !!var, sce=!!sce, mod=!!mod)
}

# load data from a directory (5.771s)
load.data <- function(dir, id=NULL) {
  
  # read and merge all files per field ...
  data <- list()
  for(field in names(FIELDS)) {
    field.dir <- paste0(dir, "/", field)
    print(field.dir)
    data[[field]] <- do.call(rbind, lapply(list.files(field.dir, full.names = TRUE), .parse.data))
  }
  # ... and merge all together
  data <- do.call(rbind, data)
  
  return(data)
}

# normalize data (0.264s)
normalize.data <- function(dat, set.obs="cru4", set.mod="cmip6", norm.period=NORM.PERIOD) {
  
  # norm observed
  norm <- dat %>% 
    filter(set == !!set.obs & year %in% !!norm.period) %>%
    group_by(var, month) %>% summarize(norm.obs = mean(value))
  
  # add to norm modeled
  norm <- dat %>% 
    filter(set %in% !!set.mod & year %in% !!norm.period) %>%
    group_by(var, month, mod) %>% summarize(norm.mod = mean(value)) %>%
    left_join(norm, by=c("var", "month"))
  
  # Compare modeled norms with observed norm
  # ggplot(norm[norm$var=="tas",], aes(x=month)) + geom_boxplot(aes(y=norm.mod)) + geom_point(aes(y=norm.obs), colour="red")
  # ggplot(norm[norm$var=="pre",], aes(x=month)) + geom_boxplot(aes(y=norm.mod)) + geom_point(aes(y=norm.obs), colour="blue")
  
  # add to modeled data and calculate norm for every entry
  dat <- dat %>%
    left_join(norm, by=c("var", "month", "mod")) %>%
    mutate(norm = case_when(var=="pre" ~ value * (norm.obs)/(norm.mod),
                            TRUE       ~ value + (norm.obs - norm.mod))) 

 # Check whether normalization is done correctly
 # tmp <- dat %>%
 #    filter(set %in% !!set.mod & year %in% !!norm.period) %>%
 #    group_by(var, month, mod, norm.obs, norm.mod) %>% summarize(norm.corr = mean(norm))
 # 
 # tmp %>%
 #    ggplot(aes(x=month)) +
 #    geom_boxplot(aes(y=norm.corr)) +
 #    geom_point(aes(y=norm.obs), colour="red") +
 #    facet_wrap(vars(var), scales="free")
   
  dat <- dat %>% dplyr::select(-c(norm.mod:norm.obs))
  
  dat$norm[is.na(dat$norm)] <- dat$value[is.na(dat$norm)]
  
  return(dat)
}

# Plot functions ====================================================

# TAB 1: Time Series ------------------------------------------------
plot.timeseries <- function(dat, var="tas", set.obs="cru4", set.mod="cmip6", period=month.abb, norm=TRUE, title=paste(set.obs, set.mod, var, sep="_")) {
  
  lab <- ifelse(var=="pre", LAB.PREC, LAB.TEMP)
  
  if(norm) {
    dat$val <- dat$norm
  } else {
    dat$val <- dat$value
  }
  
  dat.obs <- dat %>% filter(set==!!set.obs & var==!!var & month %in% !!period)
  dat.mod <- dat %>% filter(set==!!set.mod & var==!!var & month %in% !!period & year > 1900)
  
  dat.mod$sce <- factor(dat.mod$sce, levels=sub("cmip6_", "", SCENS[-1]), labels=names(SCENS[-1]))
  
  dat.mod %>%
    group_by(year, sce, mod) %>%
    summarize(val = mean(val)) %>%
    ggplot(aes(x=year)) +
    geom_line(aes(y=val, group=mod, color=sce), alpha=0.03, show.legend = FALSE) +
    geom_line(data=dat.mod %>% group_by(year, sce, mod) %>% summarize(val = mean(val)) %>% group_by(year, sce) %>% summarize(ensemble = median(val)), aes(y=ensemble, group=sce, color=sce)) +
    geom_line(data=dat.obs %>% group_by(year) %>% summarize(val = mean(val)), aes(y=val), alpha=0.5, show.legend = FALSE) +
    labs(title=title, y=lab, x="Year", color="Scenario")
}

# TAB 2: Climatology ------------------------------------------------
plot.climograph <- function(dat, var="tas", set="cru4", sce=NULL, norm=TRUE, title=paste(set, var, sce, sep="_")) {
  
  if(var=="pre") {
    lab = LAB.PREC
    palette = "YlGnBu"
  } else {
    lab = LAB.TEMP
    palette = "YlOrBr"
  }
  
  dat <- dat[dat$var == var, ]
  
  if(norm) {
    dat$val <- dat$norm
  } else {
    dat$val <- dat$value
  }
  
  if(set == "cru4") {
    dat <- dat[dat$set == "cru4", ]
  } else {
    dat <- dat[dat$set == "cru4" | dat$sce == sce, ]
  }
  
  if(set == "cru4") {
    gg <- dat %>%
      group_by(year, month) %>%
      summarize(val = mean(val)) %>%
      ggplot(aes(x=month, y = val, group=year, color=year)) +
      geom_line(alpha=0.5)
  } else {
    gg <- dat %>%
      group_by(year, month, set) %>%
      summarize(ensemble = median(val)) %>%
      ggplot(aes(x=month, y = ensemble, group=year, color=year)) +
      geom_line(data = . %>% filter(set == "cru4"), alpha=0.5, color="grey60") +
      geom_line(data = . %>% filter(set == "cmip6"), alpha=0.5)
  }
  
  gg +
    scale_colour_distiller(palette = palette, direction = 1) +
    labs(title=title, y=lab, x="Month", color="Year")
}

# # TAB 3: Walter-Lieth -----------------------------------------------
# plot.walterlieth <- function(dat, set="cru4", sce=NULL, period=NORM.PERIOD, location, lon, lat) {
#  
#    month.days=c(31,28,31,30,31,30,31,31,30,31,30,31)
#    
#    tmp <- dat %>% 
#      group_by(set, sce, var, year, month) %>%
#      summarise(value=mean(norm))
#    
#    ylim <- range(tmp$value[tmp$var=="tas"], tmp$value[tmp$var=="pre"]*31/2)*0.6
#    
#    if(set == "cru4") {
#      dat <- dat[dat$year %in% period & dat$set=="cru4", ] %>%
#        group_by(var, month) %>%
#        summarise(value=mean(norm))
#        
#    } else {
#      dat <- dat[dat$year %in% 2051:2080 & dat$set==set & dat$sce==sce, ] %>%
#        group_by(var, month, year) %>%
#        summarise(ensemble=median(norm)) %>%
#        group_by(var, month) %>%
#        summarise(value=mean(ensemble))
#    }
#    
#   climateGraph(temp=as.numeric(dat$value[dat$var=="tas"]), rain=month.days * as.numeric(dat$value[dat$var=="pre"]), ylim=ylim, main=paste0(location, " (", lat, "N/", lon, "E)\n", set, " ", sce, "\n", period[1], "-", period[length(period)]), mar=c(2,3,5,3), textprop=0)
#    
# }

# TAB 4: Crop Specification -----------------------------------------
plot.crop <- function(name, tlim, topt, plim, popt) {
  df <- crossing(temp=seq(tlim[1], tlim[2], length.out=50), prec=seq(plim[1], plim[2], length.out=50))
  df$tsuit <- dismo:::.getY(c(tlim[1], topt[1], topt[2], tlim[2]), df$temp)
  df$psuit <- dismo:::.getY(c(plim[1], popt[1], popt[2], plim[2]), df$prec)
  df$suit <- pmin(df$tsuit, df$psuit)
  
  
  # # raster plot  
  # df %>%
  # ggplot(aes(x=temp, y=prec, fill=suit)) +
  #   geom_raster() +
  #   scale_fill_distiller(palette = "Spectral", direction = 1) +
  #   xlim(0, 50) + ylim(0, 5000)

  
  df %>%
  ggplot(aes(x=prec, y=temp, z=suit)) +
    geom_contour_filled(breaks=c(seq(0,1.2,0.2))) +
    scale_fill_brewer(palette = "Spectral", guide = guide_legend(reverse=TRUE),
                      labels=c("0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8", "0.8 - 1", "1"),) +
    labs(title=paste("Climate suitability of", name), x="Annual Precipation (mm)", y="Temperature") +
    ylim(0, 50) + xlim(0, 5000)
  
  
#   mat <- df[,c(1,2,5)] %>% pivot_wider(names_from = temp, values_from = suit)
#   rownames <- mat$prec
#   mat <- as.matrix(mat[-1])
#   rownames(mat) <- rownames
#   plotly::plot_ly(z = ~mat) %>% plotly::add_surface()
# 
# 
# 
#   par(mfrow=c(2, 1))
#   plot(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), xlab='temperature', ylab='response', main=title)
# 	lines(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), col="red")
# 	plot(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), xlab='precipitation', ylab='response')
# 	lines(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), col="blue")
}

# Functions for calculating climate suitability =================================

# # Helper function for calculation climate corridor values
# .ccor.stats <- function(x, length, fun){
#   start <- x[13]
#   if(is.na(start)){
#     return(NA)
#   } else {
#     x <- x[1:12]
#     fun(rep(x, 3)[(start+12):((start+12)+length-1)])
#   }
# }
# 
# # Main function for calculating climate suitability
# clim.suit.orig <- function(dat, crop) {
#   
#   duration <- round((crop@GMIN + crop@GMAX)/60)
#   
#   # prepare data
#   dat <- dat %>%
#     filter(var %in% c("tmn", "tas", "pre")) %>%
#     mutate(days = lubridate::days_in_month(as.Date(paste(year, month, "01", sep="-"), format="%Y-%b-%d"))) %>%
#     mutate(norm = ifelse(var=="pre", norm * days, norm)) %>%
#     dplyr::select(-c(value, days)) %>%
#     pivot_wider(names_from=c(month, var), names_sep = ".", values_from=norm)
#   
#   # calculate EcoCrop Indices
#   suit=apply(dat, 1, function(x) {
#     ec <- ecocrop(crop=crop,
#                 tavg=as.numeric(x[substr(names(x), 4, 8)  == ".tas"]),
#                 prec=as.numeric(x[substr(names(x), 4, 8)  == ".pre"]))
#     return(list(temp=ec@suit_temp, prec=ec@suit_prec))
#   })
#   
#   res <- dat %>%
#     expand_grid(month=month.abb)
#   
#   res$ecind.month <- rep(1:12, nrow(res)/12)
#   res$ecind.temp <- unlist(lapply(suit, "[[", "temp"))
#   res$ecind.prec <- unlist(lapply(suit, "[[", "prec"))
#   res$ecind.comb <- pmin(res$ecind.temp, res$ecind.prec)
#   
#   # add CCor variables (precsum, max and min temp in growing period)
#   res$ecind.month.prec <- res$ecind.month - 1
#   res$ccor.sumprec <- apply(res[,c(paste(month.abb, "pre", sep="."), "ecind.month.prec")], 1, .ccor.stats, length=duration+2, fun=sum)
#   res$ccor.maxtemp <- apply(res[,c(paste(month.abb, "tas", sep="."), "ecind.month")], 1, .ccor.stats, length=duration, fun=max)
#   res$ccor.avgtemp <- apply(res[,c(paste(month.abb, "tas", sep="."), "ecind.month")], 1, .ccor.stats, length=duration, fun=mean)
#   res$ccor.mintemp <- apply(res[,c(paste(month.abb, "tas", sep="."), "ecind.month")], 1, .ccor.stats, length=duration, fun=min)
#   
#   res <- res %>% dplyr::select(year:mod, month, ecind.temp:ecind.comb, ccor.sumprec:ccor.mintemp) 
#   res <- res %>%
#     left_join(
#       res %>%
#         group_by(year, set, sce, mod) %>%
#         summarize(max.irr = max(ecind.temp), max.rain = max(ecind.comb))
#       ) %>%
#     mutate(max.irr = ifelse(max.irr == ecind.temp, TRUE, FALSE)) %>%
#     mutate(max.rain = ifelse(max.rain == ecind.comb, TRUE, FALSE))
#     
#   return(res)
# 
# }

# Main function for calculating climate suitability
clim.suit <- function(dat, crop) {
  
  duration <- round((crop@GMIN + crop@GMAX)/60)
  temp.req <- c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX)
  prec.req <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)
  
  days <- dat %>% dplyr::select(year, month) %>% distinct() %>%
    mutate(days = days_in_month(as.Date(paste(year, month, "01", sep="-"), format="%Y-%b-%d")))
  
  # prepare data
  dat <- dat %>%
    filter(var %in% c("tmn", "tas", "pre")) %>%
    left_join(days) %>%
    mutate(norm = ifelse(var=="pre", norm * days, norm)) %>%
    dplyr::select(-c(value, days)) %>%
    pivot_wider(names_from=c(month, var), names_sep = ".", values_from=norm)
  
  res <- dat %>%
    expand_grid(month=month.abb)
  
  
  # --- Klimakorridor-Parameter ---
  # Kühlste Montattemperatur für jede der 12 potentiellen Anbauperioden
  period.temp.min <- frollapply(as.data.frame(t(dat[, paste(month.abb, "tas", sep=".")][,c(1:12, 1:duration-1)])), duration, min, align="left")
  res$ccor.mintemp <- unlist(lapply(period.temp.min, "[", 1:12))
  # Durchschnittstemperatur ...
  period.temp.avg <- frollmean(as.data.frame(t(dat[, paste(month.abb, "tas", sep=".")][,c(1:12, 1:duration-1)])), duration, align="left")
  res$ccor.avgtemp <- unlist(lapply(period.temp.avg, "[", 1:12))
  # Wärmste Monatstemperatur ...
  period.temp.max <- frollapply(as.data.frame(t(dat[, paste(month.abb, "tas", sep=".")][,c(1:12, 1:duration-1)])), duration, max, align="left")
  res$ccor.maxtemp <- unlist(lapply(period.temp.max, "[", 1:12))
  # Niederschlagssumme (inkl. Vormonat und Folgemonat)
  period.prec.sum <- frollsum(as.data.frame(t(dat[, paste(month.abb, "pre", sep=".")][,c(12, 1:12, 1:duration)])), duration+2, align="left")
  res$ccor.sumprec <- unlist(lapply(period.prec.sum, "[", 1:12))
  
  # --- Ecocrop Parameter ---
  # Temperatur-Eignung je Monat
  month.temp.suit <- .getY(temp.req, data.matrix(dat[, paste(month.abb, "tas", sep=".")]))
  # Niederschlags-Eignung je Monat (basiert auf der oben für CCor berechneten Niederschlagssumme, siehe dismo/ecocrop)
  month.prec.suit <- .getY(prec.req, matrix(res$ccor.sumprec, ncol=12, byrow = TRUE))
  # Kombinierter Index (Minimum Temperatur-Eignung / Niederschlagseignung) je Monat
  # month.min.suit  <- pmin(month.temp.suit, month.prec.suit)
  
  ## !!! ACHTUNG: im originalen EcoCrop Modell wird zwei mal über die Periode gefiltert
  ## 
  ## - Niederschlagssumme für jeden potentiellen Anbaumonat 
  ##   (Vegetionsperiode + Vormonat + Folgemonat, ccor.sumprec),
  ##   darauf basierend wird der Niederschlagsindex für jeden Anbaumonat berechnet
  ##   
  ## - und dann noch einmal: minimaler Index über die gesamte Vegetationsperiode
          
  # Minimale Niederschlagseignung für jede der 12 potentiellen Anbauperioden
  # period.prec.suit <- frollapply(as.data.frame(t(month.prec.suit[,c(1:12, 1:duration-1)])), duration, min, align="left")
  # res$ecind.prec <- unlist(lapply(period.prec.suit, "[", 1:12))
  
  ## Führt zu komischen Resultaten, wenn z.B. die Monate gegen Ende der Anbauperiode
  ## gänzlich ungeeignet sind um eine Kultur zu starten, wird deren Wert genommen.
  ## Darum: direkt den Niederschlagsindex des Anbaumonats verwenden
  ## Jetzt sind auch EcoCrop-Index und Klimakorridor 1:1 vergleichbar.
  
  res$ecind.prec <-  as.vector(t(month.prec.suit))
  
  # Minimale Temperatureignung für jede der 12 potentiellen Anbauperioden
  period.temp.suit <- frollapply(as.data.frame(t(month.temp.suit[,c(1:12, 1:duration-1)])), duration, min, align="left")
  res$ecind.temp <- unlist(lapply(period.temp.suit, "[", 1:12))
  # Minimale Klimaeignung für jede der 12 potentiellen Anbauperioden (kombinierter Index)
  # period.comb.suit <- frollapply(as.data.frame(t(month.min.suit[,c(1:12, 1:duration-1)])), duration, min, align="left") 
  # res$ecind.comb <- unlist(lapply(period.comb.suit, "[", 1:12))
  res$ecind.comb <- pmin(res$ecind.prec, res$ecind.temp)
  
  res <- res %>% dplyr::select(!ends_with(c(".tas", ".pre")))
  res <- res %>%
    left_join(
      res %>%
        group_by(year, set, sce, mod) %>%
        summarize(max.irr = max(ecind.temp), max.rain = max(ecind.comb))
      ) %>%
    mutate(max.irr = ifelse(max.irr == ecind.temp, TRUE, FALSE)) %>%
    mutate(max.rain = ifelse(max.rain == ecind.comb, TRUE, FALSE))
    
  return(res)

}

# TAB 5: Ecocrop Index ----------------------------------------------
plot.ecocrop <- function(cs, period="rainfed", index="combined", set.obs="cru4", set.mod="cmip6"){
  
  dat <- cs %>%
    filter(case_when(period=="rainfed" ~ max.rain,
                     period=="irrigated" ~ max.irr,
                     TRUE ~ month==period)) %>%
    mutate(ecind.comb = case_when(period=="irrigated" ~ ecind.temp, TRUE ~ ecind.comb)) %>%
    dplyr::select(year:mod, case_when(index=="temp" ~ "ecind.temp",
                               index=="prec" ~ "ecind.prec",
                               TRUE ~ "ecind.comb")) %>%
    rename(suit = 5) %>% drop_na(suit) %>%
    # get value if several max.irr or max.rain per year
    group_by(across(year:mod)) %>% summarize(suit = max(suit, na.rm=TRUE))
  
  dat.obs <- dat %>% filter(set==!!set.obs)
  dat.mod <- dat %>% filter(set==!!set.mod & year > 1900)
  
  dat.mod$sce <- factor(dat.mod$sce, levels=sub("cmip6_", "", SCENS[-1]), labels=names(SCENS[-1]))

  dat.mod %>%
    group_by(year, sce, mod) %>%
    summarize(ensemble = median(suit)) %>%
    ggplot(aes(x=year)) +
    geom_line(aes(y=ensemble, group=mod, color=sce), alpha=0.03) +
    geom_line(data=dat.mod %>% group_by(year, sce) %>% summarize(median = median(suit, na.rm=TRUE)), aes(y=median, group=sce, color=sce)) +
    geom_line(data=dat.obs, aes(y=suit), alpha=0.5) +
    ylim(0,1)

}

.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# TAB 6: Best planting date -------------------------------------------
plot.pdate <- function(cs, period="rainfed", sce="ssp585", set.obs="cru4", set.mod="cmip6", suit=FALSE) {
  
  dat <- cs %>%
    filter(case_when(period=="rainfed" ~ max.rain,
                     period=="irrigated" ~ max.irr,
                     TRUE ~ max.rain)) %>%
    filter(ecind.comb > 0) %>%
    filter(set == !!set.obs | (set == !!set.mod & sce == !!sce)) %>%
    mutate(sce = factor(sce, levels=c("cru4", "ssp126", "ssp245", "ssp370", "ssp585"))) %>%
    mutate(setsce = sce) %>%
    mutate(setsce = !!sce) %>%
    mutate(month = match(month, month.abb)) %>%
    mutate(period = cut(year, breaks = c(1900,1930,1960,1990,2020,2050,2080, 2100),
                        labels = c("1901-1930", "31-60", "61-90", "1991-2020", "21-50", "51-80", "2081-2100")))
  
  
  # TODO: circular-linear regression (month ~ year)
  
  if(suit) {
    sce.cols <- scales::hue_pal()(4)
    names(sce.cols) <- c("ssp126", "ssp245", "ssp370", "ssp585")
    gp <- dat %>%
      mutate(suit = case_when(!!period=="irrigated" ~ ecind.temp, TRUE ~ ecind.comb)) %>%
      ggplot(aes(x = period, y=month)) +
      geom_point(data = . %>% group_by(set, period, month) %>% summarize(n=length(suit), suit=median(suit, na.rm=TRUE)),
                 aes(size=n, fill=suit, col=set, alpha=set), shape=21) +
      scale_size(range=c(3, 12)) +
      geom_smooth(aes(x = (7-1) * (year-1901)/(2100-1901) + 1, weight=suit), formula= y ~ .$month.cos + .$month.sin, col=sce.cols[sce], se=FALSE) +
      scale_alpha_discrete(range = c(0.6, 1), breaks = c("cru4","cmip6"), labels = c("observed", sce)) +
      scale_y_continuous(breaks = 1:12, limits = c(1, 12), labels = month.abb) + 
      scale_x_discrete(drop = FALSE) +
      scale_fill_gradientn(limits=c(0.01,1), colours=RColorBrewer::brewer.pal(11,"RdYlGn")) +
      scale_colour_discrete(type=c("white", "grey30"), drop=FALSE) +
      guides(size = "none", colour = "none") +
      labs(x="", y="", alpha="Data", fill="Suitability")
  } else {
    gp <- dat %>%
      ggplot(aes(x = period, y=month, col=sce)) +
      geom_count(alpha=0.5) +
      scale_size(range=c(3, 12)) +
      geom_smooth(aes(x = (7-1) * (year-1901)/(2100-1901) + 1, col=setsce), se=FALSE, show.legend = FALSE) +
      scale_y_continuous(breaks = 1:12, limits = c(1, 12), labels = month.abb) +
      scale_x_discrete(drop = FALSE) +
      scale_colour_discrete(breaks = c(NA, sce), labels = c("observed", names(SCENS[SCENS==paste0("cmip6_", sce)])), drop=FALSE) +
      guides(size = "none") +
      labs(x="", y="", colour="Data")
  }
  gp
}

# TAB 7: Climate Corridor -------------------------------------------
plot.ccor <- function(cs, crop, period="rainfed", index="temp", sce="ssp370", set.obs="cru4", set.mod="cmip6") {
  
  dat <- cs %>%
    filter(case_when(period=="rainfed" ~ max.rain,
                     period=="irrigated" ~ max.irr,
                     TRUE ~ month==period)) %>%
    filter(set == !!set.obs | (set == !!set.mod & sce == !!sce)) %>%
    # if ecind.comb == 0 (max.irr or max.rain for the whole year), take the most common month
    filter(ecind.comb > 0 | month == .mode(month)) %>%
    # if ecind.comb == 1 for several month, take average indices
    group_by(across(year:mod)) %>%
    summarize_at(vars(ecind.temp:ccor.mintemp), mean, na.rm=TRUE) %>% ungroup() %>%
    select(year:mod, case_when(index=="prec" ~ "ccor.sumprec",
                               TRUE ~ c("ccor.maxtemp", "ccor.avgtemp", "ccor.mintemp")))
     

  # Join observed data ...
  dat <- dat %>%
      filter(set == !!set.obs) %>%
      dplyr::select(-c(sce, mod)) %>%
      pivot_longer(starts_with("ccor."), names_to="stat", values_to="observed") %>%
      drop_na() %>%
      dplyr::select(year, stat, observed) %>%
    # ... with statistics (p10, p50 and p90) of modelled data
    full_join(
      dat %>%
        filter(set == !!set.mod) %>%
        pivot_longer(starts_with("ccor."), names_to="stat", values_to="value") %>%
        drop_na() %>%
        group_by(year, stat) %>%
        summarize(Q10 = quantile(value, .10),
                  Q50 = quantile(value, .50),
                  Q90 = quantile(value, .90))
    )

  # Temperature Corridor
  if(index == "temp") {
    gg <- dat %>%
      mutate(stat = factor(stat, levels=c("ccor.maxtemp", "ccor.avgtemp", "ccor.mintemp"))) %>%
      ggplot(aes(x=year, y=observed)) +
        geom_ribbon(data= . %>% filter(year >= NORM.PERIOD[1]), aes(ymax=Q90, ymin=Q10, fill=stat))+
        geom_line(aes(colour=stat)) +
        scale_colour_manual(values = c(cach.red, cach.brown, cach.blue), name="Growing period",
                        labels=rev(c("Coldest month", "Average", "Hottest month"))) +
        scale_fill_manual(values = c(cach.red20, cach.brown20, cach.blue20), name="Growing period",
                        labels=rev(c("Coldest month", "Average", "Hottest month"))) +
        geom_hline(yintercept=c(crop@TOPMN, crop@TOPMX), lty=2) +
        geom_hline(yintercept=c(crop@TMIN, crop@TMAX)) +
        theme_light() + xlab("Year") + ylab("Mean Temperature (ºC)")
  }

  # Precipitation corridor
  if(index == "prec") {
    gg <- dat %>%
      ggplot(aes(x=year, y=observed)) +
        ylab("Precipitation (mm in growing season)") + xlab("Year") +
        geom_ribbon(data= . %>% filter(year >= NORM.PERIOD[1]), aes(ymax=Q90, ymin=Q10), colour=cach.brown20, fill=cach.brown20) +
        geom_smooth(data= . %>% filter(year >= NORM.PERIOD[1]), aes(y=Q50), colour=cach.red, se=FALSE) +
        geom_line(colour=cach.red) +
        geom_hline(yintercept=c(crop@ROPMN, crop@ROPMX), lty=2) +
        geom_hline(yintercept=c(crop@RMIN, crop@RMAX)) +
        theme_light()
  }

  gg
}

# Download Data button for Plotly-Figures =====================================

icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"

dl_button <- list(
    name = "Download data",
    icon = list(
        path = icon_svg_path,
        transform = "scale(0.84) translate(-1, -1)"
        ),
    click = htmlwidgets::JS("
          function(gd) {
            var text = '';
            for(var i = 0; i < gd.data.length; i++){
              text += gd.layout.xaxis.title.text + gd.data[i].name + ',' + gd.data[i].x + '\\n';
              text += gd.layout.yaxis.title.text + gd.data[i].name + ',' + gd.data[i].y + '\\n';
            };
            var blob = new Blob([text], {type: 'text/plain'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = 'data.csv';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   ")
)



