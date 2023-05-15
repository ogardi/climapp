###############################################################################
# CARITAS-CDA: Shiny-App Functionality
# =============================================================================
# 
# Author: oliver.gardi@bfh.ch
# Date:   February 2022

library(shiny)
library(dismo)
library(reactable)
library(httr)
library(rvest)
library(tidyr)
library(dplyr)
library(ggplot2)
library(berryFunctions)


data(ECOcrops)
ecocrops <- ECOcrops[order(ECOcrops$FAMNAME, ECOcrops$SCIENTNAME), c(1:3, 5:6, 8:11, 12:15)]

## caritas cd colors
cach.blue <- rgb(102, 145, 165, max=255)
cach.blue20 <- rgb(102, 145, 165, alpha=.5*255, max=255)
cach.red <- rgb(219, 0, 27, alpha=255, max=255)
cach.red20 <- rgb(219, 0, 27, alpha=.5*255, max=255)
cach.brown20 <- rgb(110, 95, 60, alpha=.2*255, max=255)
cach.brown <- rgb(110, 95, 60, alpha=255, max=255)


# Test ======================

# data <- normalize.data(load.data("Lucerne (47.0N_8.3E)"))
# data <- normalize.data(load.data("./data/Kita", lat=13.0, lon=-9.5))
# plot.climograph(data, "tas", "cru4", title="Temperature Annual Cycle")
# plot.climograph(data, "pre", "cmip5", sce="rcp85", norm=FALSE, title="Temperature Annual Cycle")
# plot.timeseries(data, "pre", "cru4", "cmip5")
# plot.ecocrop(data, getCrop("Maize"))
# plot.ccor(data, getCrop("Maize"))
plot.waltherlieth(data, "cru4", NULL, title="Test")

# Variables =========================================================

DATA.DIR <- "./data"

NORM.PERIOD <- 1991:2020

LAB.TEMP <- "Temperature (ºC)"
LAB.PREC <- "Precipitation (mm/day)"


# KNMI fields
FIELDS <- c("cru4_tas"        = "cru4_tmp",
            # "cru4_tmn"        = "cru4_tmn",
            # "cru4_tmx"        = "cru4_tmx",
            "cru4_pre"        = "cru4_pre",
            # "cmip5_tas_rcp26" = "cmip5_tas_Amon_mod_rcp26",
            # "cmip5_tas_rcp45" = "cmip5_tas_Amon_mod_rcp45",
            # "cmip5_tas_rcp60" = "cmip5_tas_Amon_mod_rcp60",
            # "cmip5_tas_rcp85" = "cmip5_tas_Amon_mod_rcp85",
            # "cmip5_tmn_rcp26" = "cmip5_tasmin_Amon_mod_rcp26",
            # "cmip5_tmn_rcp45" = "cmip5_tasmin_Amon_mod_rcp45",
            # "cmip5_tmn_rcp60" = "cmip5_tasmin_Amon_mod_rcp60",
            # "cmip5_tmn_rcp85" = "cmip5_tasmin_Amon_mod_rcp85",
            # "cmip5_tmx_rcp26" = "cmip5_tasmax_Amon_mod_rcp26",
            # "cmip5_tmx_rcp45" = "cmip5_tasmax_Amon_mod_rcp45",
            # "cmip5_tmx_rcp60" = "cmip5_tasmax_Amon_mod_rcp60",
            # "cmip5_tmx_rcp85" = "cmip5_tasmax_Amon_mod_rcp85",
            # "cmip5_pre_rcp26" = "cmip5_pr_Amon_mod_rcp26",
            # "cmip5_pre_rcp45" = "cmip5_pr_Amon_mod_rcp45",
            # "cmip5_pre_rcp60" = "cmip5_pr_Amon_mod_rcp60",
            # "cmip5_pre_rcp85" = "cmip5_pr_Amon_mod_rcp85",
            "cmip6_tas_ssp126" = "cmip6_tas_mon_mod_ssp126",        # Mean monthly temperature
            "cmip6_tas_ssp245" = "cmip6_tas_mon_mod_ssp245",        
            "cmip6_tas_ssp370" = "cmip6_tas_mon_mod_ssp370",
            "cmip6_tas_ssp585" = "cmip6_tas_mon_mod_ssp585",
            # "cmip6_tmn_ssp126" = "cmip6_tasmin_mon_mod_ssp126",     # Mean monthly daily minimum temperature
            # "cmip6_tmn_ssp245" = "cmip6_tasmin_mon_mod_ssp245",
            # "cmip6_tmn_ssp370" = "cmip6_tasmin_mon_mod_ssp370",
            # "cmip6_tmn_ssp585" = "cmip6_tasmin_mon_mod_ssp585",
            # "cmip6_tmx_ssp126" = "cmip6_tasmax_mon_mod_ssp126",     # Mean monthly daily maximum temperature
            # "cmip6_tmx_ssp245" = "cmip6_tasmax_mon_mod_ssp245",
            # "cmip6_tmx_ssp370" = "cmip6_tasmax_mon_mod_ssp370",
            # "cmip6_tmx_ssp585" = "cmip6_tasmax_mon_mod_ssp585",
            "cmip6_pre_ssp126" = "cmip6_pr_mon_mod_ssp126",
            "cmip6_pre_ssp245" = "cmip6_pr_mon_mod_ssp245",
            "cmip6_pre_ssp370" = "cmip6_pr_mon_mod_ssp370",
            "cmip6_pre_ssp585" = "cmip6_pr_mon_mod_ssp585"
            )

# Helper functions ==================================================

notify <- function(msg, id = NULL) {
  showNotification(msg, id = id, type="error", duration = NULL, closeButton = FALSE)
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
download.knmi <- function(field, lat, lon, outdir){
  
  if(!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)
  
  request <- POST(url = "https://climexp.knmi.nl/get_index.cgi", 
                  body = list(email = "someone@somewhere", 
                              field = field, 
                              maskmetadata = "", 
                              lat1 = format(lat, nsmall = 1), 
                              lat2 = format(lat, nsmall = 1), 
                              lon1 = format(lon, nsmall = 1), 
                              lon2 = format(lon, nsmall = 1), 
                              intertype = "nearest", 
                              gridpoints = "false",
                              masktype = "all", 
                              standardunits = "standardunits"), 
                  encode = "form")
  
  urls <- html_nodes(content(request), "a") %>% html_attr("href")
  if(sum(grepl("[.]dat$", urls) == 1)) {
    file <- grep("[.]dat$", urls, value=TRUE)
    filename <- basename(file)
    if(!file.exists(paste0(outdir, "/", filename))) {
      download.file(paste0("http://climexp.knmi.nl/data/", filename), paste0(outdir, "/", filename))
    }
  } else {
    files <-  urls %>% grep("^rawdata[.]cgi", ., value=TRUE) %>%
      paste0("http://climexp.knmi.nl/", .) %>% GET() %>% content() %>% html_nodes("a") %>% html_attr("href") %>%
      grep("[.]dat$", ., value=TRUE)
    
    # Problems retreiving data from https://climexp.knmi.nl/select.cgi?id=someone@somewhere&field=cmip6_tas_mon_mod_ssp585
    
    for(file in files) {
      filename <- basename(file)
      if(!file.exists(paste0(outdir, "/", filename))) {
        download.file(paste0("http://climexp.knmi.nl/data/", filename), paste0(outdir, "/", filename))
      }
    }
  }
  
}

parse.data <- function(filename) {
  
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
    filter(set=="cru4" & year > 1900 | year >= NORM.PERIOD[length(NORM.PERIOD)]) %>%
    pivot_longer(-year, names_to="month", values_to="value") %>%
    mutate(month = factor(month, levels=month.abb),
           set = !!set, var = !!var, sce=!!sce, mod=!!mod)
  
  # dat <- read.table(filename) 
  # names(dat) <- c("year", month.abb)
  # dat <- dat[dat$year > 1900]
  # dat <- pivot_longer(dat, -year, names_to="month", values_to="value")
  # dat$month <- factor(month, levels=month.abb)
  # dat$set <- set
  # dat$var <- var
  # dat$sce <- sce
  # dat$mode <- mod
  # return(dat)
   
}

load.data <- function(dir, lat, lon, id=NULL) {
  data <- list()
  print(dir)
  for(field in names(FIELDS)) {
    field.dir <- paste0(dir, "/", field)
    print(field.dir)
    # get the field if it does not exist yet
    if(!dir.exists(field.dir) || length(list.files(field.dir))==0) {
      if(!is.null(id)) notify(paste("downloading", field, "..."), id)
      download.knmi(FIELDS[field], lat, lon, field.dir)
    }
    # read all the files in the field
    data[[field]] <- do.call(rbind, lapply(list.files(field.dir, full.names = TRUE), parse.data))
  }
  # merge all together
  data <- do.call(rbind, data)
  
  return(data)
}

normalize.data <- function(dat, set.obs="cru4", set.mod=c("cmip5", "cmip6"), norm.period=NORM.PERIOD) {
  
  # norm observed
  norm <- dat %>% 
    filter(set == !!set.obs & year %in% !!norm.period) %>%
    group_by(var, month) %>% summarize(norm.obs = mean(value))
  
  # add to norm modeled
  norm <- dat %>% 
    filter(set %in% !!set.mod & year %in% !!norm.period) %>%
    group_by(var, month, mod) %>% summarize(norm.mod = mean(value)) %>%
    left_join(norm, by=c("var", "month"))
  
  # ggplot(norm[norm$var=="tas",], aes(x=month)) + geom_boxplot(aes(y=norm.mod)) + geom_point(aes(y=norm.obs), colour="red")
  # ggplot(norm[norm$var=="pre",], aes(x=month)) + geom_boxplot(aes(y=norm.mod)) + geom_point(aes(y=norm.obs), colour="blue")
  
  # add to modeled data and calculate norm for every entry
  dat <- dat %>%
    left_join(norm, by=c("var", "month", "mod")) %>%
    mutate(norm = case_when(#var=="pre" ~ value * (norm.obs+1)/(norm.mod+1),
                            TRUE       ~ value + (norm.obs - norm.mod))) 
  
 # tmp <- dat %>%
 #    filter(set %in% !!set.mod & year %in% !!norm.period) %>%
 #    group_by(var, month, mod, norm.obs, norm.mod) %>% summarize(norm.corr = mean(norm))
 # 
 # tmp %>%
 #    filter(var=="pre") %>%
 #    ggplot(aes(x=month)) + 
 #    geom_boxplot(aes(y=norm.corr)) + 
 #    geom_point(aes(y=norm.obs), colour="red")
   
  dat <- dat %>% dplyr::select(-c(norm.mod:norm.obs))
  
  dat$norm[is.na(dat$norm)] <- dat$value[is.na(dat$norm)]
  
  return(dat)
}

# Plot functions ====================================================
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
      summarize(val = mean(val)) %>%
      ggplot(aes(x=month, y = val, group=year, color=year)) +
      geom_line(data = . %>% filter(set == "cru4"), alpha=0.5, color="black") +
      geom_line(data = . %>% filter(set == "cmip6"), alpha=0.5)
  }
  
  gg +
    scale_colour_distiller(palette = palette, direction = 1) +
    labs(title=title, y=lab, x="Month")
  
}

plot.walterlieth <- function(dat, set="cru4", sce=NULL, title=paste(set, sce, sep="_")) {
 
   month.days=c(31,28,31,30,31,30,31,31,30,31,30,31)
   
   if(set == "cru4") {
     dat <- dat[dat$year %in% 1991:2020 & dat$set=="cru4", ] %>%
       group_by(var, month) %>%
       summarise(value=mean(norm))
     
     climateGraph(temp=as.numeric(dat$value[dat$var=="tas"]), rain=month.days * as.numeric(dat$value[dat$var=="pre"]), main="Kita (Mali)\nLat: 13.0 / Lon: -9.5\n1981 - 2020", mar=c(2,3,5,3), textprop=0, ylim=c(0,150))
       
   } else {
     dat <- dat[dat$year %in% 2051:2080 & dat$set==set & dat$sce==sce, ] %>%
       group_by(var, month) %>%
       summarise(value=mean(norm))
     
     climateGraph(temp=as.numeric(dat$value[dat$var=="tas"]), rain=month.days * as.numeric(dat$value[dat$var=="pre"]), main="Kita (Mali)\nLat: 13.0 / Lon: -9.5\n2051 - 2080 (ssp585)", mar=c(2,3,5,3), textprop=0, ylim=c(0,150))
   }
   
   
  
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
      summarize(val = mean(val)) %>%
      ggplot(aes(x=month, y = val, group=year, color=year)) +
      geom_line(data = . %>% filter(set == "cru4"), alpha=0.5, color="black") +
      geom_line(data = . %>% filter(set == "cmip6"), alpha=0.5)
  }
  
  gg +
    scale_colour_distiller(palette = palette, direction = 1) +
    labs(title=title, y=lab, x="Month")
  
   climateGraph(temp=as.numeric(cru.tmp), rain=as.numeric(cru.pre*month.days), main="Kita (Mali)\n[Lat: 13.0, Lon: -9.5]", mar=c(2,3,4,3), textprop=0)
}

plot.timeseries <- function(dat, var="tas", set.obs="cru4", set.mod="cmip6", period=month.abb, norm=TRUE, title=paste(set.obs, set.mod, var, sep="_")) {
  
  lab <- ifelse(var=="pre", LAB.PREC, LAB.TEMP)
  
  if(norm) {
    dat$val <- dat$norm
  } else {
    dat$val <- dat$value
  }
  
  dat.obs <- dat %>% filter(set==!!set.obs & var==!!var & month %in% !!period)
  dat.mod <- dat %>% filter(set==!!set.mod & var==!!var & month %in% !!period & year > 1900)
  
  dat.mod %>%
    group_by(year, sce, mod) %>%
    summarize(ensemble = mean(val)) %>%
    ggplot(aes(x=year)) +
    geom_line(aes(y=ensemble, group=mod, color=sce), alpha=0.05) +
    geom_line(data=dat.mod %>% group_by(year, sce) %>% summarize(mean = mean(val)), aes(y=mean, group=sce, color=sce)) +
    geom_line(data=dat.obs %>% group_by(year) %>% summarize(val = mean(val)), aes(y=val), alpha=0.5) +
    labs(title=title, y=lab, x="Year")
}

# plot.crop <- function(crop, title) {
#   par(mfrow=c(2, 1))
#   plot(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), xlab='temperature', ylab='response', main=title)
# 	lines(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), col="red")
# 	plot(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), xlab='precipitation', ylab='response')
# 	lines(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), col="blue")
# }

plot.crop <- function(crop, title) {
  df <- crossing(temp=seq(crop@TMIN, crop@TMAX, length.out=100), prec=seq(crop@RMIN, crop@RMAX, length.out=100))
  df$tsuit <- dismo:::.getY(c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), df$temp)
  df$psuit <- dismo:::.getY(c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), df$prec)
  df$suit <- pmin(df$tsuit, df$psuit)
  df %>%
  ggplot(aes(x=temp, y=prec, fill=suit)) +
    geom_raster() +
    scale_fill_distiller(palette = "Spectral", direction = 1)
  
  df %>%
  ggplot(aes(x=temp, y=prec, z=suit)) +
    geom_contour_filled() 
  
  
  
  par(mfrow=c(2, 1))
  plot(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), xlab='temperature', ylab='response', main=title)
	lines(c(0,1,1,0) ~ c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX), col="red")
	plot(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), xlab='precipitation', ylab='response')
	lines(c(0,1,1,0) ~ c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX), col="blue")
}

plot.ecocrop <- function(dat, crop, set.obs="cru4", set.mod="cmip6"){
  
  # prepare data
  dat <- dat %>%
    filter(var %in% c("tmn", "tas", "pre")) %>%
    dplyr::select(-value) %>%
    # group_by(year, month, set, var, sce) %>%
    # summarize(norm = mean(norm)) %>%
    pivot_wider(names_from=c(month, var), names_sep = ".", values_from=norm)
  
  dat$suit=apply(dat, 1, function(x) {
    max(ecocrop(crop,
                as.numeric(x[substr(names(x), 4, 8)  == ".tmn"]),
                as.numeric(x[substr(names(x), 4, 8)  == ".tas"]),
                30 * as.numeric(x[substr(names(x), 4, 8)  == ".pre"]))@suitability)
  })
  
  dat.obs <- dat %>% filter(set==!!set.obs)
  dat.mod <- dat %>% filter(set==!!set.mod & year > 1900)
  
  dat.mod %>%
    group_by(year, sce, mod) %>%
    summarize(ensemble = mean(suit)) %>%
    ggplot(aes(x=year)) +
    geom_line(aes(y=ensemble, group=mod, color=sce), alpha=0.05) +
    geom_line(data=dat.mod %>% group_by(year, sce) %>% summarize(median = median(suit, na.rm=TRUE)), aes(y=median, group=sce, color=sce)) +
    geom_line(data=dat.obs, aes(y=suit), alpha=0.5) +
    labs(title=paste("Suitability", crop@name), y="Suitability", x="Year")
  
  
}

plot.ccor <- function(dat, crop, var="tas", set.obs="cru4", set.mod="cmip6", sce="ssp370") {
  

  n.months <- ceiling(crop@GMIN / 30)
  
  dat.wide <- dat %>%
    filter(set == !!set.obs | (set == set.mod & sce == !!sce)) %>%
    pivot_wider(-value, names_from = month, values_from = norm)
  
  wettest.months <- dat.wide %>% 
    filter(var == "pre") %>%
    mutate(wet.id = apply(.[, month.abb], 1, function(x) which.max(stats::filter(x, filter=rep(1, n.months), method="conv", sides=1, circular=TRUE)))) %>%
    dplyr::select(-!!month.abb, -var)
  
  dat.wide <- dat.wide %>% filter(var == !!var) %>% left_join(wettest.months) %>% drop_na(wet.id)
  
  dat.wide$wet.min <- dat.wide$wet.avg <- dat.wide$wet.max <- 0.0
  for(m in 1:12){
    dat.wide$wet.min[dat.wide$wet.id == m]     <- apply(dat.wide[dat.wide$wet.id == m, rep(month.abb, 2)[(12-n.months+1):12+m]], 1, min)
    dat.wide$wet.avg[dat.wide$wet.id == m]     <- apply(dat.wide[dat.wide$wet.id == m, rep(month.abb, 2)[(12-n.months+1):12+m]], 1, mean)
    dat.wide$wet.max[dat.wide$wet.id == m]     <- apply(dat.wide[dat.wide$wet.id == m, rep(month.abb, 2)[(12-n.months+1):12+m]], 1, max)
  }
  
  dat.wide <- dat.wide %>% dplyr::select(-!!month.abb, -wet.id)
  
  # summarize models (25 and 75 quantile)
  dat.stat <- dat.wide %>%
    filter(set == set.mod) %>%
    pivot_longer(wet.max:wet.min, names_to="stat", values_to="value") %>%
    drop_na() %>%
    group_by(year, var, sce, stat) %>%
    summarize(Q10 = quantile(value, .10),
              Q25 = quantile(value, .25),
              Q50 = quantile(value, .50),
              Q75 = quantile(value, .75),
              Q90 = quantile(value, .90)) %>%
  full_join(
    dat.wide %>%
      filter(set == set.obs) %>% 
      dplyr::select(-c(sce, mod)) %>%
      pivot_longer(wet.max:wet.min, names_to="stat", values_to="observed") %>%
      drop_na() %>%
      dplyr::select(year, var, stat, observed)
  )

  # Temperature Corridor
  if(var == "tas") {
    gg <- dat.stat %>%
      filter(var == "tas") %>%
      filter(stat == "wet.min" | stat == "wet.max") %>%
      filter(sce == !!sce | is.na(sce)) %>%
      ggplot(aes(x=year, y=observed)) +
        geom_ribbon(data= . %>% filter(year > 1970), aes(ymax=Q75, ymin=Q25, fill=stat))+
        geom_line(aes(colour=stat)) +
        scale_colour_manual(values = c(cach.red, cach.blue), name="Growing period",
                        labels=rev(c("Coldest month", "Hottest month"))) +
        scale_fill_manual(values = c(cach.red20, cach.blue20), name="Growing period",
                        labels=rev(c("Coldest month", "Hottest month"))) +
        geom_hline(yintercept=c(crop@TOPMN, crop@TOPMX), lty=2) +
        geom_hline(yintercept=c(crop@TMIN, crop@TMAX)) +
        theme_light() + xlab("Year") + ylab("Mean Temperature (ºC)")
  }
  
  # Precipitation corridor
  if(var == "pre") {
    gg <- dat.stat %>%
      filter(var == "pre") %>%
      filter(stat == "wet.avg") %>%
      filter(sce == !!sce | is.na(sce)) %>%
      mutate(across(Q10:observed, function(x) x * crop@GMIN)) %>%
      ggplot(aes(x=year, y=observed)) +
        ylab("Precipitation (mm in growing season)") + xlab("Year") +
        geom_ribbon(data= . %>% filter(year > 1980), aes(ymax=Q90, ymin=Q10), colour=cach.brown20, fill=cach.brown20) +
        geom_smooth(data= . %>% filter(year > 1980), aes(y=Q50), colour=cach.red, se=FALSE) +
        geom_line(colour=cach.red) +
        geom_hline(yintercept=c(crop@ROPMN, crop@ROPMX), lty=2) +
        geom_hline(yintercept=c(crop@RMIN, crop@RMAX)) +
        theme_light()
  }
  
  gg
}