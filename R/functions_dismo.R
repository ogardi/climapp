###############################################################################
# EcoCrop Functions from the dismo package, adapted
# =============================================================================
# 
# Author: oliver.gardi@gmail.com
# Date:   September 2023

setClass('ECOCROPcrop',
	representation (
		name  = 'character',
		famname  = 'character',
		scientname  = 'character',
		code = 'integer',
		GMIN    = 'numeric', 
		GMAX   = 'numeric', 
		KTMP    = 'numeric',
		TMIN   = 'numeric', 
		TOPMN    = 'numeric',
		TOPMX  = 'numeric', 
		TMAX   = 'numeric', 
		RMIN   = 'numeric', 
		ROPMN    = 'numeric', 
		ROPMX   = 'numeric', 
		RMAX  = 'numeric', 
		LIG = 'numeric', 
		LIGR  = 'numeric', 
		PP   = 'numeric', 
		PPMIN     = 'numeric',
		PPMAX    = 'numeric',
		TEXT   = 'numeric',
		TEXTR   = 'numeric',
		DEP  = 'numeric',
		DEPR  = 'numeric',
		DRA  = 'numeric', 
		DRAR   = 'numeric', 
		PHMIN   = 'numeric', 
		PHOPMN   = 'numeric',
		PHOPMX  = 'numeric',
		SAL  = 'numeric',
		SALR  = 'numeric',
		FER  = 'numeric',
		FERR  = 'numeric',
		LIMITS  = 'numeric'
	)
)


setMethod ('show' , 'ECOCROPcrop', 
	function(object) {
		cat('name:',object@name, '\n')
		cat('latn:',object@scientname, '\n')
		cat('gdur:', object@GMIN, object@GMAX, '\n')
		cat('temp:', object@TMIN, object@TOPMN, object@TOPMX, object@TMAX, '\n')
		cat('prec:', object@RMIN, object@ROPMN, object@ROPMX, object@RMAX, '\n')
	}
)	


setMethod ('plot', signature(x='ECOCROPcrop', y='missing'),
	function(x, ..., col=c("red", "blue"), lwd=1) {
		graphics::par(mfrow=c(2, 1))
		col = rep(col, length.out=2)
		plot(c(0,1,1,0) ~ c(x@TMIN, x@TOPMN, x@TOPMX, x@TMAX), xlab='temperature', ylab='response', ...)
		lines(c(0,1,1,0) ~ c(x@TMIN, x@TOPMN, x@TOPMX, x@TMAX), col=col[1], lwd=lwd)
		plot(c(0,1,1,0) ~ c(x@RMIN, x@ROPMN, x@ROPMX, x@RMAX), xlab='precipitation', ylab='response', ...)
		lines(c(0,1,1,0) ~ c(x@RMIN, x@ROPMN, x@ROPMX, x@RMAX), xlab='precipitation', ylab='', col=col[2], lwd=lwd)
	}
)


.getECcrops <- function() {
	thisenvir = new.env()
	get(load("./ECOcrops.RData"), thisenvir)
}


getCrop <- function(name) {

	showECcrops <- function() {
		tab <- .getECcrops()
		tab[,c('NAME', 'SCIENTNAME')]
	}

	if (missing(name)) {
		return( showECcrops() )
	}
	
	tab <- .getECcrops() 
	tab1 <- toupper(as.vector(tab[,'NAME']))
	tab2 <- toupper(as.vector(tab[,'SCIENTNAME']))
	ind1 <- which(toupper(name) == tab1)
	ind2 <- which(toupper(name) == tab2)
	
	if (length(ind1) == 0 & length(ind2) == 0) {
		stop('Unknown crop. See "getCrop()" for a list')
	} 
	
	r <- max(ind1, ind2)
	r <- as.matrix(tab[r,])
	crop <- new('ECOCROPcrop')
	crop@name  <- r[,'NAME']
	crop@famname <- r[,'FAMNAME']
	crop@scientname <- r[,'SCIENTNAME']
	crop@code <- as.integer(r[,'CODE'])
	crop@GMIN  <- as.numeric(r[,'GMIN'])
	crop@GMAX   <- as.numeric(r[,'GMAX'])
	crop@KTMP   <- as.numeric(r[,'KTMP'])
	crop@TMIN   <- as.numeric(r[,'TMIN'])
	crop@TOPMN  <- as.numeric(r[,'TOPMN'])
	crop@TOPMX  <- as.numeric(r[,'TOPMX'])
	crop@TMAX   <- as.numeric(r[,'TMAX'])
	crop@RMIN   <- as.numeric(r[,'RMIN'])
	crop@ROPMN  <- as.numeric(r[,'ROPMN'])
	crop@ROPMX  <- as.numeric(r[,'ROPMX'])
	crop@RMAX   <- as.numeric(r[,'RMAX'])
	return(crop)	
}

setClass('ECOCROP',
	representation (
		crop = 'ECOCROPcrop',
		suit_std = 'vector',
		suit_temp = 'vector',
		suit_prec = 'vector',
		maxper = 'vector',
		max_std = 'numeric',
		max_temp = 'numeric',
		max_prec = 'numeric',
		max_irr = 'numeric'
	),
	prototype (	
		crop = new('ECOCROPcrop'),
		suit_std = rep(NA, 12),
		suit_temp = rep(NA, 12),
		suit_prec = rep(NA, 12),
		maxper = c(NA),
		max_std = as.numeric(NA),
		max_temp = as.numeric(NA),
		max_prec = as.numeric(NA),
		max_irr = as.numeric(NA)
	),	
	validity = function(object)
	{
		return(TRUE)
	}
)
	

setMethod ('show' , 'ECOCROP', 
	function(object) {
		cat('class      :', class(object), '\n')
		cat('Crop       :', object@crop@name, '\n')
		cat('Suit. std. :', object@suit_std, '\n')
		cat('Suit. temp.:', object@suit_temp, '\n')
		cat('Suit. prec.:', object@suit_prec, '\n')
		cat('Best period:', object@maxper, '\n')
	}
)



setMethod ('plot', signature(x='ECOCROP', y='missing'),
	function(x, ...) {
		plot(1:length(x@suit_std), x@suit_std, xlab='periods', ylab='suitability')
		lines(1:length(x@suit_std), x@suit_std, col='green', lwd=2)
	}
)

# from raster package
movingFun <- function (x, n, fun = mean, type = "around", circular = FALSE, na.rm = FALSE) {
    n <- round(abs(n))
    if (n == 0) {
        stop("n == 0")
    }
    x = as.vector(x)
    lng <- length(x)
    if (type == "around") {
        hn <- floor(n/2)
        if (circular) {
            x <- c(x[(lng - hn + 1):lng], x, x[1:hn])
        }
        else {
            x <- c(rep(NA, hn), x, rep(NA, hn))
        }
    }
    else if (type == "to") {
        if (circular) {
            x <- c(x[(lng - n + 2):lng], x)
        }
        else {
            x <- c(rep(NA, n - 1), x)
        }
    }
    else if (type == "from") {
        if (circular) {
            x <- c(x, x[1:n])
        }
        else {
            x <- c(x, rep(NA, n))
        }
    }
    else {
        stop("unknown type; should be \"around\", \"to\", or \"from\"")
    }
    m <- matrix(ncol = n, nrow = lng)
    for (i in 1:n) {
        m[, i] <- x[i:(lng + i - 1)]
    }
    apply(m, MARGIN = 1, FUN = fun, na.rm = na.rm)
}

.getY <- function (a, x) {
    inter <- function(x1, y1, x2, y2, x) {
        y1 + (y2 - y1) * (x - x1)/(x2 - x1)
    }
    y <- x
    if (is.null(a)) {
        y[] <- 1
    }
    else {
        y[] <- NA
        y[!is.na(x) & x <= a[1]] <- 0
        y[!is.na(x) & x <= a[2] & x > a[1]] <- inter(a[1], 0, a[2], 1, x[!is.na(x) & x <= 
            a[2] & x > a[1]])
        y[!is.na(x) & x <= a[3] & x > a[2]] <- 1
        y[!is.na(x) & x <= a[4] & x > a[3]] <- inter(a[3], 1, a[4], 0, x[ !is.na(x) & x <= 
            a[4] & x > a[3]])
        y[!is.na(x) & x >= a[4]] <- 0
    }
    return(y)
}

.doEcocrop <- function (crop, tavg, prec) {
    if (sum(is.na(c(tavg, prec))) > 0) {
        return(new("ECOCROP"))
    }
    duration <- round((crop@GMIN + crop@GMAX)/60)
    tmp <- c(crop@TMIN, crop@TOPMN, crop@TOPMX, crop@TMAX)
    temp <- .getY(tmp, tavg)
    pre <- c(crop@RMIN, crop@ROPMN, crop@ROPMX, crop@RMAX)
    shftprec <- c(prec[12], prec[-12])
    cumprec <- movingFun(prec, n = duration + 1, fun = sum, type = "from", circular = TRUE) + shftprec
    prec <- .getY(pre, cumprec)
    allv <- cbind(temp, prec)
    minv <- apply(allv, 1, min)
    obj <- new("ECOCROP")
    obj@crop <- crop
    obj@suit_std  <- movingFun(minv, n = duration, fun = min, type = "from", circular = TRUE)
    obj@suit_temp <- movingFun(temp, n = duration, fun = min, type = "from", circular = TRUE)
    obj@suit_prec <- movingFun(prec, n = duration, fun = min, type = "from", circular = TRUE)
    obj@max_std <- max(obj@suit_std)
    obj@max_irr <- max(obj@suit_temp)
    if (obj@max_std > 0) {
        obj@maxper <- which(obj@suit_std == max(obj@suit_std))
        obj@max_temp <- max(obj@suit_temp[obj@maxper])
        obj@max_prec <- max(obj@suit_prec[obj@maxper])
    }
    else {
        obj@maxper <- 0
        obj@max_temp <- 0
        obj@max_prec <- 0
    }
    return(obj)
}

ecocrop <- function (crop, tavg, prec){
    if (inherits(crop, "character")) {
      crop <- getCrop(crop)
    }
    .doEcocrop(crop = crop, tavg = tavg, prec = prec)
}