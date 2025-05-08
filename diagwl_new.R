#- diagwl.- Walter & Lieth climatic diagram.
# has been modified
diagwl_new <- function(dat, cols=1:6, format='%Y-%m-%d', yeari=NA, yearf=NA,
                   stname="", alt=NA, per="", mlab="", shem=NULL, p3line=FALSE, ...) {
  #dat: Data frame with the required climatic data (see details).
  #cols: Columns containing the needed data [1:6]. Set to NULL if a monthly
  #   climate summary is provided.
  #format: Format of the dates if data are provided in 4 columns ['%Y-%m-%d'].)
  #yeari, yearf: initial and final years of the period to use. (Defaults to the
  #  period contained in dat, but if it contains a climate summary, then the parameter per should be supplied (see below).
  #  must be supplied.)
  #stname: Name of the climatological station.
  #alt: Elevation (altitude) of the climatological station.
  #per: If data is a data frame with already calculated climate averages, the
  #  original period of the data. 
  #mlab: Vector of 12 monthly labels for the X axis (see the details).
  #shem: Southern hemisphere? (NULL by default, to be detected from warm season).
  #p3line: Draw a supplementary precipitation line referenced to three times the
  # temperature? (FALSE by default; this parameter was suggested by Bogdan Rosca)
  #...: Other optional graphic parameters.
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar=c(4,4,5,4), las=1, new=FALSE)
  pcol='#005ac8'; tcol='#e81800'; pfcol='#79e6e8'; sfcol='#09a0d1' #used colors
  #etiquetas de los meses
  if(length(mlab)!=12) {
    if(mlab=='es') mlab <- c('E','F','M','A','M','J','J','A','S','O','N','D')
    else if(mlab=='en'|mlab=='fr') mlab <- c('J','F','M','A','M','J','J','A',
                                             'S','O','N','D')
    else mlab <- c(1:12) #numeric labels
  }
  if(is.null(cols) | length(cols)>6) { #assume data frame contains a monthly climate summary:
    if(ncol(dat)<12) stop("Input data frame has more than 6 and less than 12 columns!")
    if(ncol(dat)==13) dat <- dat[,1:12]
    nr <- nrow(dat) #no. of raws of monthly data
    switch(nr,
           stop("At least two monthly rows (average precipitation and temperature)\n  must be supplied"),
           cat("Warning: When only monthly precipitation and mean temperature\n         are provided, no frost risk will be drawn.\n"),
           cat("Warning: When no absolute minimum temperatures are provided,\n         likely frost will not be drawn.\n")
    )
  } else {
    z <- apply(is.na(dat[,cols]),1,sum); nz <- sum(z>0) #check for missing data
    if(nz>0) { #remove missing data:
      cat("Warning: removing",nz,"lines containing missing data\n")
      dat <- dat[z==0,]
    }
    if(length(cols)==4) { #disaggregate dates into year, month, day:
      dates <- as.Date(dat[,cols[1]],format=format)
      dat <- data.frame(YY=as.integer(strftime(dates,'%Y')),
                        MM=as.integer(strftime(dates,'%m')),
                        DD=as.integer(strftime(dates,'%d')),dat[,cols[2:4]])
    } else dat <- dat[,cols]
    z <- range(dat[,1])
    if(is.na(yeari)) yeari <- z[1] else if(yeari < z[1]) yeari <- z[1]
    if(is.na(yearf)) yearf <- z[2] else if(yearf > z[2]) yearf <- z[2]
    if(yearf < yeari) { z <- yeari; yeari <- yearf; yearf <- z }
    dat <- dat[dat[,1] >= yeari & dat[,1] <= yearf,]
    per=sprintf('%s-%s',yeari,yearf) #period
    ny <- yearf-yeari+1 #no. of years
    datcli <- matrix(NA,4,12)
    datcli[1,] <- round(aggregate(dat[,4],list(dat[,2]),sum,na.rm=TRUE)$x / ny , 1)
    datcli[2,] <- round(aggregate(dat[,5],list(dat[,2]),mean,na.rm=TRUE)$x , 1)
    datcli[3,] <- round(aggregate(dat[,6],list(dat[,2]),mean,na.rm=TRUE)$x , 1)
    datcli[4,] <- round(aggregate(dat[,6],list(dat[,2]),min,na.rm=TRUE)$x , 1)
    dat <- datcli; nr <- nrow(dat)
  }
  dat <- as.matrix(dat)
  if(is.null(shem)) { #see if warm season is in Dec-Jan-Feb:
    if(nr==2) tmean <- dat[2,] else tmean <- apply(dat[2:3,],2,mean)
    if(mean(tmean[c(1,2,12)]) > 2+(mean(tmean[6:8]))) shem <- TRUE
    else shem <- FALSE
  }
  if(shem) { #Southern hemisphere: shift data six months forward
    m1 <- dat[,1:6]
    m2 <- dat[,7:12]
    dat <- cbind(m2,m1)
    mlab <- c(mlab[7:12],mlab[1:6])
  }
  p <- dat[1,] #monthly average precipitations
  if(nr==2) tm <- dat[2,]
  else tm <- apply(dat[2:3,],2,mean,na.rm=TRUE)  #monthly average temperatures
  pmax <- max(p,na.rm=TRUE) #maximum precipitation
  ymax <- 60  #default maximum Y-axis ordinate
  if(pmax > 300) ymax <- 50 + 10*floor((pmax+100)/200)
  ymin <- min(-1.5,min(tm)) #minimum Y-axis ordinate
  #ejes:
  if(ymin < -1.5) {
    ymin=floor(ymin/10)*10 #rounded minimum Y-axis ordinate
    labT <- paste(ymin)
    labP <- ""
    if(ymin < -10) {
      for(i in (ymin/10+1):-1) {
        labT <- c(labT,i*10)
        labP <- c(labP,"")
      }
    }
    labT <- c(labT,"0","10","20","30","40","50","")
    labP <- c(labP,"0","20","40","60","80","100","300")
  }
  else {
    labT <- c("0","10","20","30","40","50","")
    labP <- c("0","20","40","60","80","100","300")
  }
  if(ymax > 60) {
    for(i in 6:(ymax/10-1)) {
      labT <- c(labT,"")
      labP <- c(labP,100*(2*i-7))
    }
  }
  plot(0:13-0.5,c(tm[12],tm[1:12],tm[1]),xlim=c(0,12),ylim=c(ymin,ymax),type="n",xaxs="i",yaxs="i",xaxp=c(0,12,12),xlab="",ylab="",xaxt="n",yaxt="n",bty="n")
  lmin <- ymin #minimum Y-axis ordinate to label
  if(lmin==-1.5) lmin=0
  axis(2,((lmin/10):(ymax/10))*10,labels=labT,col.axis=tcol)
  axis(4,((lmin/10):(ymax/10))*10,labels=labP,col.axis=pcol)
  x_pos_left <- grconvertX(par("usr")[1], from = "user", to = "ndc") - 0.11
  x_user_left <- grconvertX(x_pos_left, from = "ndc", to = "user")
  text(x_user_left, mean(par("usr")[3:4]), 
       bquote("Temperature"~" ("*degree*C*~") "), 
       srt = 90, xpd = NA,cex=0.8)
  x_pos_right <- grconvertX(par("usr")[2], from = "user", to = "ndc") + 0.11
  x_user_right <- grconvertX(x_pos_right, from = "ndc", to = "user")
  text(x_user_right, mean(par("usr")[3:4]), 
       bquote("Precipitation"~" (mm)"), 
       srt = -90, xpd = NA, cex=0.8)
  #mtext(bquote("Temperature"~" ("~.* degree* C~")"),side=2,line=2,at=35)
  #mtext(bquote("Precipitation"~" (mm)"),side=4,line=2,adj=1,at=35)
  abline(0,0)
  #abline(50,0)
  #labels:
  if(is.na(alt)) mtext(stname,line=2,adj=0)
  else mtext(paste(stname," (","Elevation: ",alt,"m)",sep=""),line=2,adj=0)
  mtext(per,line=1,adj=0)
  mtext(bquote("MAT: "~.(round(mean(tm), 1)) * degree * C ),line=2,adj=1)
  mtext(bquote("MAP: "~.(format(round(sum(p), 0), big.mark = ",")) ~ "mm"),line=1,adj=1)
  x <- 0:13-0.5
  p2 <- c(p[12],p[1:12],p[1])
  if(p3line) { #additional precipitation line at 1:3 scale
    yl3 <- c(p[12],p[1:12],p[1])/3
    yl3[yl3>50] <- 50
  }
  if(pmax<=100) {
    xl <- x
    yl <- c(p[12],p[1:12],p[1])/2
    n2 <- 14
  }
  else { #scale change when precipitation > 100 mm
    xp <- numeric(30)
    yp <- numeric(30)
    xl <- numeric(25)
    yl <- numeric(25)
    n <- 0
    n2 <- 0
    gr <- FALSE
    if(p2[1]>100) { #first point
      n <- n+1
      xp[n] <- x[1]
      yp[n] <- 50
      n <- n+1
      xp[n] <- x[1]
      yp[n] <- 50 + (p2[1]-100)/20
      n2 <- n2+1
      xl[n2] <- x[1]
      yl[n2] <- 50
      gr <- TRUE
    }
    else {
      n2 <- n2+1
      xl[n2] <- x[1]
      yl[n2] <- p2[1]/2
    }
    for(i in 2:14) {  #remaining points
      if(gr) {  #if previous p > 100
        n <- n+1
        if(p2[i]>100) {
          xp[n] <- x[i]
          yp[n] <- 50 + (p2[i]-100)/20
        }
        else {
          xp[n] <- x[i-1] + (100-p2[i-1])/(p2[i]-p2[i-1])
          yp[n] <- 50
          n2 <- n2+1
          xl[n2] <- xp[n]
          yl[n2] <- 50
          n <- n+1
          xp[n] <- NA
          yp[n] <- NA
          n2 <- n2+1
          xl[n2] <- x[i]
          yl[n2] <- p2[i]/2
          gr <- FALSE
        }
      }
      else {  # if previos p <=100
        if(p2[i]>100) { #if p > 100
          n <- n+1
          xp[n] <- x[i-1] + (100-p2[i-1])/(p2[i]-p2[i-1])
          yp[n] <- 50
          if(xl[n2]!=x[i-1]) {  #avoid repeating points!
            n2 <- n2+1
            xl[n2] <- x[i-1]
            yl[n2] <- p2[i-1]/2
          }
          n2 <- n2+1
          xl[n2] <- xp[n]
          yl[n2] <- 50
          n <- n+1
          xp[n] <- x[i]
          yp[n] <- 50 + (p2[i]-100)/20
          gr <- TRUE
        }
        else { # p <=100
          n2 <- n2+1
          xl[n2] <- x[i]
          yl[n2] <- p2[i]/2
        }
      }
    }
    if(!is.na(yp[n])) {  #close last poligon
      n <- n+1
      xp[n] <- xp[n-1]
      yp[n] <- 50
      n2 <- n2+1
      xl[n2] <- 12.5
      yl[n2] <- 50
    }
    polygon(xp[1:n],yp[1:n],col=pcol,border=pcol)
  }
  #patterns:
  pi <- approx(xl[1:n2],yl[1:n2],n=66)$y
  ti <- approx(x,c(tm[12],tm[1:12],tm[1]),n=66)$y
  ti[ti<0] <- 0 #avoid patterns below zero
  d <- pi - ti
  xi <- (1:66)/5-0.7
  xw <- subset(xi,d>0) #humid period
  y1 <- subset(pi,d>0)
  y2 <- subset(ti,d>0)
  if(length(xw)>0) segments(xw,y1,xw,y2,col=pcol,lty=1,lwd=1)
  xw <- subset(xi,d<0) #dry period
  y1 <- subset(pi,d<0)
  y2 <- subset(ti,d<0)
  if(length(xw)>0) segments(xw,y1,xw,y2,col=tcol,lty=3,lwd=2)
  if(nr>2) {
    #sure frosts
    for(i in 1:12) if(dat[3,i]<=0) rect(i-1,-1.5,i,0,col=sfcol)
    if(nr>3) #likely frosts
      for(i in 1:12) { if(dat[4,i]<=0 && dat[3,i]>0) rect(i-1,-1.5,i,0,col=pfcol)}
    else mtext('(Likely frost months not provided)',1,line=1.5)
  } else mtext('(No monthly frost risk provided)',1,line=1.5)
  #curvas de P y T:
  lines(xl[1:n2],yl[1:n2],col=pcol,lwd=2)
  if(p3line) lines(x,yl3)
  lines(x,c(tm[12],tm[1:12],tm[1]),col=tcol,lwd=2)
  if(nr>2) {
    #mean maximum temperatures of the warmest month
    #mtext(formatC(max(as.matrix(dat[2,])),digits=1,format="f"),2,las=1,line=2,at=35)
    #mean minimum temperatures of the coldest month
    #mtext(formatC(min(as.matrix(dat[3,])),digits=1,format="f"),2,las=1,line=2,at=15)
  }
  #tick month limits:
  for(i in 0:13) segments(i,0,i,-1.5)
  #label months:
  mtext(mlab,1,las=1,line=0.5,adj=0.5,at=x[2:13])
  #reset old.par (reset former graphic parameters):
  invisible()
}

