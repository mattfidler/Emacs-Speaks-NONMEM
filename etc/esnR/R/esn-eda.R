### esn-eda.R --- EDA Functions
## 
## Filename: esn-eda.R
## Description: 
## Author: Matthew L. Fidler
## Maintainer: Matthew L. Fidler
## Created: Wed Dec  1 10:14:52 2010 (-0600)
## Version: 0.13
## Last-Updated: Thu Dec 16 14:09:08 2010 (-0600)
##           By: Matthew L. Fidler
##     Update #: 4
## URL: 
## Keywords: 
## Compatibility: 
## 
######################################################################
## 
### Commentary: 
## 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.
## 
######################################################################
## 
### Code:


library(lattice);
library(grid);

## #################################################################
## Event History
## #################################################################

event.history.plot <- function(data,time="TIME",id="ID",amt="AMT",date=NULL,date.type="DATE",max.ids=NULL,
                               expand.scale=0.03,png.name=NULL,
                               xmind=file.exists("esn-xmind.R"),
                               identify=FALSE,
                               ...){
  ret <-  NULL;
  if(any(names(data) == amt)){
    data[,time] <-  nm.get.time(data,time=time,id=id,date=date,date.type=date.type,...);
    d <-  data.frame(id=data[,id],time=data[,time],amt=data[,amt]);
    d$ev <-  1;
    d$ev[!is.na(d$amt)] <-  0;
    d$ev[which(d$amt == 0)] <-  0;
    events <-  "";
    events.log <-  "";
    if (is.null(max.ids)){
      if (!is.null(png.name) & !identify){
        png(paste(png.name,"event-history-overall-log.png",sep=""));
      }
      xplot <-  event.history.plot.xy(d,log=TRUE,...);
      print(xplot);
      if (identify){
        trellis.focus("panel",1,1);
        panel.identify(labels=d$id,col="red",fontface=2);
        trellis.unfocus();
      }
      if (!is.null(png.name) & !identify){
        dev.off();
        png(paste(png.name,"event-history-overall.png",sep=""));
      } else if (!is.null(png.name) & identify){
        dev.copy(device=png,filename=paste(png.name,"event-history-overall-log.png",sep=""));
        while(dev.cur() != 1){
          dev.off();
        }

      }
      xplot <-  event.history.plot.xy(d,log=FALSE,...);
      print(xplot);
      if (identify){
        trellis.focus("panel",1,1);
        panel.identify(labels=d$id,col="red",fontface=2);
        trellis.unfocus();
      }
      if (!is.null(png.name) & !identify){
        dev.off();
      } else if (!is.null(png.name) & identify){
        dev.copy(device=png,filename=paste(png.name,"event-history-overall.png",sep=""));
        while(dev.cur() != 1){
          dev.off();
        }

      }
    } else {

      r.1 <- range(d$time,na.rm=TRUE);
      tmp <-  expand.scale*(r.1[2]-r.1[1]);
      r.1[1] <-  r.1[1]-tmp;
      r.1[2] <-  r.1[2]+tmp;

      r.2 <- log10(range(d$time[d$time != 0],na.rm=TRUE));
      tmp <-  expand.scale*2*(r.2[2]-r.2[1]);
      r.2[1] <-  r.2[1]-tmp;
      r.2[2] <-  r.2[2]+tmp;
      ids <-  sort(unique(d$id));

      tmp <-  unique(c(seq(1,length(ids),by=max.ids),length(ids)));
      tmp <-  round(seq(1,length(ids)+1,length=length(tmp)));
      if (!is.null(png.name) & !identify){
        png(paste(png.name,"event-history-split-by-id-log-%03d.png",sep=""));
      }
      id.lst <-  c();
      for (i in 1:length(tmp)){
        if (i != length(tmp)){
          cur.id <-  ids[tmp[i]:(tmp[i+1]-1)];
          xplot <-  event.history.plot.xy(d[d$id %in% cur.id,],
                                          ylab=paste("ID (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                                          log=TRUE,xlim=r.2,...);
          print(xplot);
          if (identify){
            trellis.focus("panel",1,1);
            panel.identify(labels=d[d$id %in% cur.id,"id"],col="red",fontface=2);
            trellis.unfocus();
            dev.copy(device=png,filename=paste(png.name,"event-history-split-by-id-log-",cur.id[1],"-",cur.id[length(cur.id)],".png",sep=""));
            while(dev.cur() != 1){
              dev.off();
            }
          }
          id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));
        }
      }
      dxm <- data.frame(IDS=id.lst);
      row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
      if (xmind && !is.null(png.name) & !identify){
        xml <-  xmind.table(dxm,
                            "Semi-Log",
                            extra=paste("ehg",i,sep=""),
                            link.row=paste(png.name,"event-history-split-by-id-",tolower(i),sep=""),
                            link.ext=".png",
                            topic.link=paste(png.name,"event-history-overall-log.png",sep="")
                            );
        events.log <-  xml;
      }
      if (!is.null(png.name) & !identify){
        dev.off();
        png(paste(png.name,"event-history-split-by-id-%03d.png",sep=""));
      }
      id.lst <-  c();
      for (i in 1:length(tmp)){
        if (i != length(tmp)){
          cur.id <-  ids[tmp[i]:(tmp[i+1]-1)];
          xplot <-  event.history.plot.xy(d[d$id %in% cur.id,],
                                          ylab=paste("ID (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                                          log=FALSE,xlim=r.1,...);
          print(xplot);
          id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));
          if (identify){
            trellis.focus("panel",1,1);
            panel.identify(labels=d[d$id %in% cur.id,"id"],col="red",fontface=2);
            trellis.unfocus();
            dev.copy(device=png,filename=paste(png.name,"event-history-split-by-id-",cur.id[1],"-",cur.id[length(cur.id)],".png",sep=""));
            while(dev.cur() != 1){
              dev.off();
            }
          }
        }
      }
      dxm <- data.frame(IDS=id.lst);
      row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
      if (xmind && !is.null(png.name)){
        xml <-  xmind.table(dxm,
                            "Linear",
                            extra=paste("ehl",i,sep=""),
                            link.row=paste(png.name,"event-history-split-by-id-",tolower(i),sep=""),
                            link.ext=".png",
                            topic.link=paste(png.name,"event-history-overall.png",sep="")
                            );
        events <-  xml;
      }
      if (!is.null(png.name) & !identify){
        dev.off();
      }
      if (xmind && !is.null(png.name)){
        ret <-  paste("<topic id=\"xeh",xmind.tm(),"\" timestamp=\"",xmind.timestamp(),
                      "\"><title>Event History</title><children><topics type=\"attached\">",
                      events.log,
                      events,
                      "</topics></children></topic>");

      }
    }
  } else {
    cat("Could not find",amt,"in dataset\n");
  }
  return(ret);
}

event.history.plot.xy <- function(d,xlab="Time (hr)",ylab="ID",aspect=1/2,log=FALSE,...){
  xs <-  xscale.components.default;
  if (log){
    xs <-  eda.xscale.components.log10;
    d$time <- log10(d$time);
    key.val <- list(space="top",columns=2,
                    text=list(c("Dosing Event","Dosing Event (Time=0)","Non-dosing event","Non-dosing event (Time=0)")), #
                    points=list(pch=c("|","|","o","o"),col=c("black","blue","black","blue"))
                    );
  } else {
    key.val <- list(space="top",columns=2,
                    text=list(c("Dosing Event","Non-dosing event")), #
                    points=list(pch=c("|","o"),col=c("black","black"))
                    );
  }
  xplot <- xyplot(id~time,d,
                  groups=d$id,
                  ev = d$ev,
                  xlab=xlab,
                  ylab=ylab,
                  aspect=aspect,
                  scales=ifelse(log,list(x=list(log=10)),list()),
                  panel=function(x,y,ev,subscripts,...){
                    panel.grid(v=-1,h=FALSE,lty=1,lwd=0.5);
                    if (log){
                      df <-  data.frame(x=x,y=y,ev=ev);
                      df <-  df[is.infinite(df$x),];
                      df$x <- 0.005;
                      df1 <-  df[df$ev == 1,];
                      df0 <-  df[df$ev == 0,];
                      ## Add dosing and non-dosing events at zero.
                      if (length(df1[,1]) != 0){
                        grid.points(unit(df1$x,"npc"),unit(df1$y,"native"),size=unit(0.5,"char"),
                                    pch=1,gp=gpar(col="blue"));
                      }
                      if (length(df0[,1]) != 0){
                        grid.points(unit(df0$x,"npc"),unit(df0$y,"native"),size=unit(0.125,"char"),
                                    pch="|",gp=gpar(col="blue"))
                      }
                      ## Add lines
                      df2 <-  data.frame(x=x,y=y,ev=ev)
                      df2 <- df2[!is.infinite(df2$x),];
                      df2 <-  df2[order(df2$y,df2$x),];
                      df2 <-  df2[!duplicated(df2$y),];
                      df2 <-  df2[df2$y %in% df$y,];
                      for (cid in df2$y){

                        grid.lines(c(unit(0,"npc"),convertX(unit(df2[df2$y == cid,]$x,"native"),"npc")),
                                   unit(rep(cid,2),"native"),gp=gpar(col="blue",lty=3)
                                   );
                      }

                    }
                    panel.superpose(x,y,subscripts,lty=3,col="black",type="l",...);
                    ev <-  ev[subscripts];
                    panel.points(x[ev==1], y[ev==1], pch=1,col="black",...);
                    panel.points(x[ev==0], y[ev==0], pch="|",col="black",...);
                  },
                  key=key.val,
                  xscale.components=xs,
                  ...
                  );
  return(xplot);
}

## ###########################################################################
## Get NONMEM time functions
## ###########################################################################
nm.get.time <- function(data,time="TIME",id="ID",date=NULL,date.type="DATE",...){
  if (any(names(data) == id)){
    ## Need Time data
    if(any(names(data) == time)){
      if (!is.null(date)){
        if (!(date.type %in% c("DATE","DAT1","DAT2","DAT3"))){
          cat("date.type must be either DATE, DAT1, DAT2, or DAT3\n");
        } else {
          if (any(names(data) == date)){
            tm <-  paste(data[,date],data[,time]);
            ## Currently assume year is four digits.
            if (date.type=="DATE"){
              if (any(regexpr("/",tm) > -1,na.rm=TRUE)) {
                tm <- strptime(tm,format="%m/%d/%Y %H:%M");
              } else {
                tm <- strptime(tm,format="%m-%d-%Y %H:%M");
              }
            } else if (date.type == "DAT1"){
              if (any(regexpr("/",tm) > -1,na.rm=TRUE)) {
                tm <- strptime(tm,format="%d/%m/%Y %H:%M");
              } else {
                tm <- strptime(tm,format="%d-%m-%Y %H:%M");
              }
            } else if (date.type == "DAT2"){
              if (any(regexpr("/",tm) > -1,na.rm=TRUE)) {
                tm <- strptime(tm,format="%Y/%m/%d %H:%M");
              } else {
                tm <- strptime(tm,format="%Y-%m-%d %H:%M");
              }
            } else if (date.type == "DAT3"){
              if (any(regexpr("/",tm) > -1,na.rm=TRUE)) {
                tm <- strptime(tm,format="%Y/%d/%m %H:%M");
              } else {
                tm <- strptime(tm,format="%Y-%d-%m %H:%M");
              }
            }
            id.dat <-  data[,id];
            ids <-  unique(id.dat);
            tm2 <- rep(0,length(tm));
            for (i in ids){
              w.id <-  which(id.dat == i);
              min.time <-  min(tm[w.id]);
              tm2[w.id] <- difftime(tm[w.id],min.time,units="hours");
            }
            tm <-  tm2;
          } else {
            cat("Could not find",date,"in dataset\n");
          }
        }
      } else {
        tm <-  data[,time];
      }
      return(tm);
    } else {
      cat("Could not find",time,"in dataset\n");
    }
  } else {
    cat("Could not find",id,"in dataset\n");
  }
  return(data[,time]);
}


## #################################################################
## ID Checkout
## #################################################################

id.checkout <- function(data,id="ID",max.ids=NULL,expand.scale=0.03,png.name=NULL,one.file=FALSE,
                        xmind=file.exists("esn-xmind.R"),
                        data.file.info=NULL,
                        identify=FALSE,
                        ...){
  skip.it <- 1;
  if (any(names(data) == id)){
    n <-  names(data);
    n <-  n[n != id];
    id.checkout <- list();
    id.checkout.log <- list();
    id.checkout.mean <- list();
    id.checkout.mean.log <- list();
    ret <-  NULL;
    for (i in n){
      if (is.null(max.ids)){
        cat("ID vs ",i," (Overall)\n",file=stderr(),sep="");
      } else {
        cat("ID vs ",i," (Split into groups of at least ",max.ids,")\n",file=stderr(),sep="");
      }
      x <- id.convert.to.numeric(data[,i])$x;
      if (!all(is.na(x))){
        if (is.null(max.ids)){
          if (!is.null(data.file.info)){
            file.name.info <- file.info(paste(png.name,"id-checkout-",tolower(i),"-overall.png",sep=""))
            if (is.na(file.name.info[,"mtime"])){
              skip.it <-  1;
            } else {
              skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
            }
          }
          if (skip.it < 0){
          } else {
            if (!is.null(png.name)){
              png(paste(png.name,"id-checkout-",tolower(i),"-overall.png",sep=""));
            }
            p <- id.plot(i,data=data,
                         ...);
            print(p);
            if (identify){
              trellis.focus("panel",1,1);
              panel.identify(labels=d$id,col="red",fontface=2);
              trellis.unfocus();
            }
            if (!is.null(png.name)){
              dev.off();
            }
          }
          if (!is.null(data.file.info)){
            file.name.info <- file.info(paste(png.name,"id-checkout-",tolower(i),"-overall-log.png",sep=""))
            if (is.na(file.name.info[,"mtime"])){
              skip.it <-  1;
            } else {
              skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
            }
          }
          if (skip.it < 0) {
          } else {
            if (!is.null(png.name)) {
              png(paste(png.name,"id-checkout-",tolower(i),"-overall-log.png",sep=""));
            }
            p <- id.plot(i,data=data,log=TRUE,
                         ...);
            print(p);
            if (identify){
              trellis.focus("panel",1,1);
              panel.identify(labels=d$id,col="red",fontface=2);
              trellis.unfocus();
            }
            if (!is.null(png.name)){
              dev.off();
            }
          }
        } else {
          r.1 <- range(x,na.rm=TRUE);
          tmp <- expand.scale*(r.1[2]-r.1[1]);
          r.1[1] <-  r.1[1]-tmp;
          r.1[2] <-  r.1[2]+tmp;

          r.2 <- log10(range(x[x > 0],na.rm=TRUE));
          tmp <- expand.scale*2*(r.2[2]-r.2[1]);
          r.2[1] <-  r.2[1]-tmp;
          r.2[2] <-  r.2[2]+tmp;
          ## Now sort by mean value.
          dat <- data;
          dat$X <- id.convert.to.numeric(dat[,i])$x;
          tmp.f <- function(cid){
            ret <- c(ID=cid,m=mean(dat[dat[,id]==cid,"X"],na.rm=TRUE));
            return(ret);
          }
          dat2 <-data.frame(t(sapply(unique(dat[,id]),tmp.f)));
          dat2 <-  dat2[order(dat2$m),];
          dat2$RID <-  seq(1,length(dat2[,1]));
          dat2 <-  dat2[!is.na(dat2$m),];
          if (length(dat2[,1]) > 0){
            dat1 <-  data;
            dat1$ID <-  data[,id];
            dat2 <-  merge(dat2,dat1,by="ID");
            ids <-  sort(unique(dat2$RID));
            tmp <-  unique(c(seq(1,length(ids),by=max.ids/2),length(ids)));
            tmp <-  round(seq(1,length(ids)+1,length=length(tmp)));

            if (!is.null(data.file.info)){
              file.name.info <- file.info(paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-001.png",sep=""))
              if (is.na(file.name.info[,"mtime"])){
                skip.it <-  1;
              } else {
                skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
              }
            }
            if (skip.it < 0){
            } else {
              if (!is.null(png.name)){
                png(paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-%03d.png",sep=""));
              }
            }
            id.lst <- c();
            for (j in 1:length(tmp)){
              if (j != length(tmp)){
                cur.id <-  ids[tmp[j]:(tmp[j+1]-1)];
                d <- dat2[dat2$RID %in% cur.id,];
                if (skip.it < 0){
                } else {
                  p <- id.plot.mean(i,data=d,log=FALSE,
                                    ylab=paste("Ranked ID by ",i," (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                                    xlim=r.1,
                                    rug.data=x,
                                    ranked.id="RID",
                                    ...);
                  print(p);
                  if (identify){
                    trellis.focus("panel",1,1);
                    panel.identify(labels=d$id,col="red",fontface=2);
                    trellis.unfocus();
                  }
                }
                id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));
              }
            }
            dxm <- data.frame("Ranked IDs"=id.lst);
            names(dxm) <-  c("Ranked IDs");
            row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
            if (xmind && !is.null(png.name)){
              xml <-  xmind.table(dxm,
                                  paste("Ranked ID vs. ",i,sep=""),
                                  extra=paste("ricg",i,sep=""),
                                  link.row=paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-",sep=""),
                                  link.ext=".png",
                                  topic.link=paste(png.name,"ranked-id-checkout-",tolower(i),"-overall.png",sep="")
                                  );
              id.checkout.mean[[i]] <-  xml;
            }
            if (skip.it < 0){
            } else {
              if (!is.null(png.name)){
                dev.off();
              }
            }
            if (!is.null(data.file.info)){
              file.name.info <- file.info(paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-log-001.png",sep=""))
              if (is.na(file.name.info[,"mtime"])){
                skip.it <-  1;
              } else {
                skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
              }
            }
            if (skip.it < 0) {
            } else {
              if (!is.null(png.name)){
                png(paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-log-%03d.png",sep=""));
              }
            }
            id.lst <- c();
            for (j in 1:length(tmp)){
              if (j != length(tmp)){
                cur.id <-  ids[tmp[j]:(tmp[j+1]-1)];
                if (skip.it < 0) {
                } else {
                  d <- dat2[dat2$RID %in% cur.id,];
                  p <- id.plot.mean(i,data=d,log=TRUE,
                                    ylab=paste("Ranked ID by ",i," (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                                    xlim=r.1,
                                    rug.data=x,
                                    ranked.id="RID",
                                    ...);
                  print(p);
                  if (identify){
                    trellis.focus("panel",1,1);
                    panel.identify(labels=d$id,col="red",fontface=2);
                    trellis.unfocus();
                  }
                }
                id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));

              }
            }
            dxm <- data.frame("Ranked IDs"=id.lst);
            names(dxm) <-  c("Ranked IDs");
            row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
            if (xmind && !is.null(png.name)){
              xml <-  xmind.table(dxm,
                                  paste("Ranked ID vs. ",i,sep=""),
                                  extra=paste("ricl",i,sep=""),
                                  link.row=paste(png.name,"ranked-id-checkout-",tolower(i),"-split-by-id-",sep=""),
                                  link.ext=".png",
                                  topic.link=paste(png.name,"ranked-id-checkout-",tolower(i),"-overall.png",sep="")
                                  );
              id.checkout.mean.log[[i]] <-  xml;
            }
            if (skip.it < 0){
            } else {
              if (!is.null(png.name)){
                dev.off();
              }
            }
          }
          ids <-  sort(unique(data[,id]));
          tmp <-  unique(c(seq(1,length(ids),by=max.ids),length(ids)));
          tmp <-  round(seq(1,length(ids)+1,length=length(tmp)));
          if (!is.null(data.file.info)){
            file.name.info <- file.info(paste(png.name,"id-checkout-",tolower(i),"-split-by-id-001.png",sep=""))
            if (is.na(file.name.info[,"mtime"])){
              skip.it <-  1;
            } else {
              skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
            }
          }
          if (skip.it < 0 ){
          } else {
            if (!is.null(png.name)){
              png(paste(png.name,"id-checkout-",tolower(i),"-split-by-id-%03d.png",sep=""));

            }
          }
          id.lst <- c();
          for (j in 1:length(tmp)){
            if (j != length(tmp)){
              cur.id <-  ids[tmp[j]:(tmp[j+1]-1)];
              if (skip.it < 0){
              } else {
                d <- data[data[,id] %in% cur.id,];
                p <- id.plot(i,data=d,log=FALSE,
                             ylab=paste("ID (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                             xlim=r.1,
                             rug.data=x,
                             ...);
                print(p);
                if (identify){
                  trellis.focus("panel",1,1);
                  panel.identify(labels=d$id,col="red",fontface=2);
                  trellis.unfocus();
                }
              }
              id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));

            }
          }
          dxm <- data.frame(IDS=id.lst);
          row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
          if (xmind && !is.null(png.name)){
            xml <-  xmind.table(dxm,
                                paste("ID vs. ",i,sep=""),
                                extra=paste("icl",i,sep=""),
                                link.row=paste(png.name,"id-checkout-",tolower(i),"-split-by-id-",sep=""),
                                link.ext=".png",
                                topic.link=paste(png.name,"id-checkout-",tolower(i),"-overall.png",sep="")
                                );
            id.checkout[[i]] <-  xml;
          }
          if (skip.it < 0){
          } else {
            if (!is.null(png.name)){
              dev.off();
            }
          }
          if (!is.null(data.file.info)){
            file.name.info <- file.info(paste(png.name,"id-checkout-",tolower(i),"-split-by-id-log-001.png",sep=""))
            if (is.na(file.name.info[,"mtime"])){
              skip.it <-  1;
            } else {
              skip.it <- as.numeric(difftime(data.file.info[,"mtime"], file.name.info[,"mtime"],units="secs"));
            }
          }
          if (skip.it < 0) {
          } else {
            if (!is.null(png.name)){
              png(paste(png.name,"id-checkout-",tolower(i),"-split-by-id-log-%03d.png",sep=""));
            }
          }

          id.lst <- c();
          for (j in 1:length(tmp)){
            if (j != length(tmp)){
              cur.id <-  ids[tmp[j]:(tmp[j+1]-1)];
              if (skip.it < 0){
              } else {
                p <- id.plot(i,
                             data=d,
                             log=TRUE,
                             ylab=paste("ID (",cur.id[1],"-",cur.id[length(cur.id)],")",sep=""),
                             xlim=r.2,
                             rug.data=log10(x),
                             ...);
                print(p);
                if (identify){
                  trellis.focus("panel",1,1);
                  panel.identify(labels=d$id,col="red",fontface=2);
                  trellis.unfocus();
                }
              }
              id.lst <-  c(id.lst,paste("From",cur.id[1],"to",cur.id[length(cur.id)]));
            }
          }
          dxm <- data.frame(IDS=id.lst);
          row.names(dxm) <- sprintf("%03d",as.numeric(row.names(dxm)));
          if (xmind && !is.null(png.name)){
            xml <-  xmind.table(dxm,
                                paste("ID vs. ",i,sep=""),
                                extra=paste("icg",i,sep=""),
                                link.row=paste(png.name,"id-checkout-",tolower(i),"-split-by-id-log-",sep=""),
                                link.ext=".png",
                                topic.link=paste(png.name,"id-checkout-",tolower(i),"-overall-log.png",sep="")
                                );
            id.checkout.log[[i]] <-  xml;
          }
          if (skip.it < 0) {
          } else {
            if (!is.null(png.name)){
              dev.off();
            }
          }
          if (xmind && !is.null(png.name)){
            ret <- paste("<topic id=\"xic",xmind.tm(),
                         "\" timestamp=\"",xmind.timestamp(),
                         "\"><title>ID checkout</title><children><topics type=\"attached\">",
                         "<topic id=\"xicl",xmind.tm(),
                         "\" timestamp=\"",xmind.timestamp(),
                         "\"><title>Linear</title><children><topics type=\"attached\">",
                         paste(unlist(id.checkout),collapse=""),
                         "</topics></children></topic>",
                         "<topic id=\"xicg",xmind.tm(),
                         "\" timestamp=\"",xmind.timestamp(),
                         "\"><title>Semi-Log</title><children><topics type=\"attached\">",
                         paste(unlist(id.checkout.log),collapse=""),
                         "</topics></children></topic>",

                         "<topic id=\"xricl",xmind.tm(),
                         "\" timestamp=\"",xmind.timestamp(),
                         "\"><title>Linear, Ordered by Means</title><children><topics type=\"attached\">",
                         paste(unlist(id.checkout.mean),collapse=""),
                         "</topics></children></topic>",
                         "<topic id=\"xricg",xmind.tm(),
                         "\" timestamp=\"",xmind.timestamp(),
                         "\"><title>Semi-Log, Ordered by Means</title><children><topics type=\"attached\">",
                         paste(unlist(id.checkout.mean.log),collapse=""),
                         "</topics></children></topic>",

                         "</topics></children></topic>"
                         ,sep="")
          }
        }
      }
    }
  } else {
    cat("Could not find",id,"in dataset\n");
  }
  return(ret);
}

id.convert.to.numeric <-  function(x){
  time <-  FALSE;
  date <-  FALSE;
  if (any(regexpr("[0-9]?[0-9]:[0-9][0-9]",paste(x)) > -1)){
    tmp <- difftime(strptime(paste(x),format="%H:%M"),
                    strptime("00:00",format="%H:%M"),units="hours")
    x <-  as.numeric(tmp);
    time <- TRUE;
  }
  date <-  FALSE;
  if (any(regexpr("[0-9]+-[0-9]+-[0-9][0-9][0-9][0-9]",paste(x)) >-1)){
    x1 <- gsub("^([0-9]+)-.*$","\\1",paste(x));
    
    if (any(as.numeric(x1) > 12)){
      ## DAY-MONTH-YEAR
      tmp <-  strptime(paste(x),format="%d-%m-%Y");
    } else {
      ## MONTH-DAY-YEAR
      tmp <-  strptime(paste(x),format="%m-%d-%Y");
    }
    min.tmp <-  min(tmp);
    tmp <-  as.numeric(difftime(tmp,min.tmp,units="days"));
    x <-  tmp;
    date <-  TRUE;
  }
  if (any(regexpr("[0-9]+/[0-9]+/[0-9][0-9][0-9][0-9]",paste(x)) >-1)){
    x1 <- gsub("^([0-9]+)/.*$","\\1",paste(x));
    if (any(as.numeric(x1) > 12)){
      ## DAY-MONTH-YEAR
      tmp <-  strptime(paste(x),format="%d/%m/%Y");
    } else {
      ## MONTH-DAY-YEAR
      tmp <-  strptime(paste(x),format="%m/%d/%Y");
    }
    min.tmp <-  min(tmp);
    tmp <-  as.numeric(difftime(tmp,min.tmp,units="days"));
    x <-  tmp;
    date <-  TRUE;
  }
  x <- as.numeric(x);
  return(list(x=x,date=date,time=time));
}

id.plot.mean <-  function(x,data,show.na=TRUE,xlab=x,cex=0.5,aspect=2,jitter=TRUE,jitter.factor=1/10,id.by=10,
                          rug=TRUE,rug.data=NULL,log=FALSE,ylab=paste("ID rank ordered by mean ",xlab,sep=""),
                          ranked.id=NULL,
                          ...){
  dat <-  data;
  tmp <-  id.convert.to.numeric(dat[,x]);
  dat$X <-  tmp$x;
  time <- tmp$time;
  date <-  tmp$date;

  xs <-  xscale.components.default;
  if (log){
    xs <-  eda.xscale.components.log10;
  }
  if (log){
    if (any(dat$X <= 0,na.rm=TRUE)){
      w <- which(dat$X <= 0);
      dat$X[which(dat$X > 0)] <- log10(dat$X[which(dat$X > 0)]);
      dat$X[w] <-  NaN;
      key.val <- list(space="top",
                      columns=4,cex=0.9,
                      text=list(c("Data","Missing",expression(Data<=0),ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                      points=list(pch=c("o","o","o","|"),col=c("black","red","blue","red")))
      
    } else {
      dat$X <- log10(dat$X);
      key.val <- list(space="top",
                      columns=3,cex=0.9,
                      text=list(c("Data","Missing",ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                      points=list(pch=c("o","o","|"),col=c("black","red","red")))
    }
  } else {
    key.val <- list(space="top",columns=3,cex=0.9,
                    text=list(c("Data","Missing",ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                    points=list(pch=c("o","o","|"),col=c("black","red","red")));
  }
  ## Now order by mean ID value.
  if (is.null(ranked.id)){
    tmp.f <- function(id){
      return(c(ID=id,m=mean(dat[dat$ID==id,"X"],na.rm=TRUE)));
    }
    dat2 <-data.frame(t(sapply(unique(dat$ID),tmp.f)));
    dat2 <-  dat2[order(dat2$m),];
    dat2$RID <-  seq(1,length(dat2[,1]));
    dat <-  dat[,names(dat) %in% c("ID","X")];
    dat2 <-  merge(dat,dat2,by="ID");
    dat2 <-  dat2[!is.na(dat2$m),];
  } else {
    dat2 <-  dat;
    dat2$RID <-  dat[,ranked.id];
  }
  s <-  c(1,seq(id.by,round(max(dat2$RID)/id.by,0)*id.by,by=id.by));
  xyplot(RID~X,dat2,
         cex=cex,
         aspect=aspect,
         xlab=paste(xlab,ifelse(time," (hours since midnight)",ifelse(date," (days since first event)","")),sep=""),
         ylab=ylab,
         main=paste("Mean ",xlab," ordered ID vs. ",xlab,ifelse(log,", semilog",""),ifelse(jitter," (jittered)",""),sep=""),
         scales=ifelse(log,list(y=list(at=s),x=list(log=10)),list(y=list(at=s))),
         rug.data=rug.data,
         ID=dat2$ID,
         mean.level=dat2$m,
         panel=function(x,y,...,jitter,jitter.factor,vals,cex,rug.data,ID,mean.level) {
           if (all(is.na(x))){
             if (log){
               stop("Data missing (NA) or below zero")
             } else {
               stop("Data missing (NA)");
             }
           } else {
             panel.abline(h=vals,lty=3);
             if (jitter){
               if (show.na){
                 if (any(is.na(x))){
                   d <-  data.frame(x=x,y=y);
                   d <-  d[is.na(d$x),];
                   if (any(is.nan(d$x),na.rm=TRUE)){
                     tmp <- d[is.nan(d$x),];
                     d <-  d[!is.nan(d$x),];
                     tmp$x <-  0;
                     tmp$x <-  jitter(tmp$x);
                     tmp$y <-  jitter(tmp$y);
                     if (length(tmp$x) > 0) {
                       grid.points(unit(tmp$x,"npc"),unit(tmp$y,"native"),size=unit(0.5,"char"),gp=gpar(col="blue"),...);
                     }
                   }
                   if (any(is.na(d$x))){
                     d$x <-  1;
                     d$x <-  jitter(d$x);
                     d$y <-  jitter(d$y);
                     if (length(d$x) > 0){
                       grid.points(unit(d$x,"npc"),
                                   unit(d$y,"native"),
                                   gp=gpar(col="red"),
                                   size=unit(0.5,"char"),
                                   ...);
                     }
                   }
                 }
               }
               panel.xyplot(jitter(x,factor=jitter.factor),jitter(y,factor=jitter.factor),col="black",...)
               df <-  data.frame(y=as.numeric(convertY(unit(y,"native"),"npc")),id=ID,
                                 m=as.numeric(convertX(unit(mean.level,"native"),"npc")));
               df$x <- 0.03;
               df$x[df$m < 0.5] <- 0.97;
               df <-  df[,names(df) != "m"];
               df <- data.frame(t(df));
               tmp.f <- function(dc){
                 y <- dc[1];
                 x <- dc[3];
                 id <- dc[2];
                 if (x == 0.03){
                   grid.text(id,0.01,y,gp=gpar(cex=0.9),
                             just=c("left","center")
                             );
                 } else {
                   grid.text(id,0.99,y,gp=gpar(cex=0.9),
                             just=c("right","center")
                             );
                 }
               }
               lapply(df,tmp.f);

               if (rug){
                 if (is.null(rug.data)){
                   panel.rug(x,col="red",regular=FALSE);
                 } else {
                   panel.rug(rug.data,col="red",regular=FALSE);
                 }
               }
             } else {
               if (show.na){
                 if (any(is.na(x))){
                   d <-  data.frame(x=x,y=y);
                   d <-  d[is.na(d$x),];
                   if (any(is.nan(d$x),na.rm=TRUE)){
                     tmp <- d[is.nan(d$x),];
                     d <-  d[!is.nan(d$x),];
                     tmp$x <-  0;
                     if (length(tmp$x) > 0){
                       grid.points(unit(tmp$x,"npc"),unit(tmp$y,"native"),gp=gpar(col="blue"),size=unit(0.5,"char"),...);
                     }
                   }
                   if (any(is.na(d$x))){
                     d$x <-  1;
                     if (length(d$x) > 0) {
                       grid.points(unit(d$x,"npc"),
                                   unit(d$y,"native"),
                                   gp=gpar(col="red"),
                                   size=unit(0.5,"char"),
                                   ...);
                     }
                   }
                 }
               }
               panel.xyplot(x,y,col="black",...)
               df <-  data.frame(y=as.numeric(convertY(unit(y,"native"),"npc")),id=ID);
               df$x <- 0.03;
               df$x[df$y < 0.5] <- 0.97;
               df <- data.frame(t(df));
               tmp.f <- function(dc){
                 y <- dc[1];
                 x <- dc[3];
                 id <- dc[2];
                 grid.text(id,x,y,gp=gpar(cex=0.7));
               }
               lapply(df,tmp.f);
               if (rug){
                 if (is.null(rug.data)){
                   panel.rug(x,col="red",regular=FALSE);
                 } else {
                   panel.rug(rug.data,col="red",regular=FALSE);
                 }
               }
             }
           }
         },
         jitter=jitter,
         jitter.factor=jitter.factor,
         vals=s,
         xscale.components=xs,
         key=key.val,
         ...
         )

}

id.plot <- function(x,data,show.na=TRUE,xlab=x,cex=0.5,aspect=2,jitter=TRUE,jitter.factor=1/10,id.by=10,rug=TRUE,rug.data=NULL,log=FALSE,...){
  dat <-  data;
  tmp <-  id.convert.to.numeric(dat[,x]);
  dat$X <-  tmp$x;
  time <- tmp$time;
  date <-  tmp$date;

  xs <-  xscale.components.default;
  if (log){
    xs <-  eda.xscale.components.log10;
  }
  if (log){
    if (any(dat$X <= 0,na.rm=TRUE)){
      w <- which(dat$X <= 0);
      dat$X[which(dat$X > 0)] <- log10(dat$X[which(dat$X > 0)]);
      dat$X[w] <-  NaN;
      key.val <- list(space="top",
                      columns=4,cex=0.9,
                      text=list(c("Data","Missing",expression(Data<=0),ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                      points=list(pch=c("o","o","o","|"),col=c("black","red","blue","red")))

    } else {
      dat$X <- log10(dat$X);
      key.val <- list(space="top",
                      columns=3,cex=0.9,
                      text=list(c("Data","Missing",ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                      points=list(pch=c("o","o","|"),col=c("black","red","red")))
    }
  } else {
    key.val <- list(space="top",columns=3,cex=0.9,
                    text=list(c("Data","Missing",ifelse(is.null(rug.data),"Density (Rug)","Overall Density (Rug)"))),
                    points=list(pch=c("o","o","|"),col=c("black","red","red")));
  }
  s <-  c(1,seq(id.by,round(max(dat$ID)/id.by,0)*id.by,by=id.by));
  while (length(s) >= 10){
    id.by <- id.by + 10;
    s <-  c(1,seq(id.by,round(max(dat$ID)/id.by,0)*id.by,by=id.by));
  }
  xyplot(ID~X,dat,
         cex=cex,
         aspect=aspect,
         xlab=paste(xlab,ifelse(time," (hours since midnight)",ifelse(date," (days since first event)","")),sep=""),
         main=paste("ID vs. ",xlab,ifelse(log,", semilog",""),ifelse(jitter," (jittered)",""),sep=""),
         scales=ifelse(log,list(y=list(at=s),x=list(log=10)),list(y=list(at=s))),
         rug.data=rug.data,
         panel=function(x,y,...,jitter,jitter.factor,vals,cex,rug.data) {
           if (all(is.na(x))){
             if (log){
               stop("Data missing (NA) or below zero")
             } else {
               stop("Data missing (NA)");
             }
           } else {
             panel.abline(h=vals,lty=3);
             if (jitter){
               if (show.na){
                 if (any(is.na(x))){
                   d <-  data.frame(x=x,y=y);
                   d <-  d[is.na(d$x),];
                   if (any(is.nan(d$x),na.rm=TRUE)){
                     tmp <- d[is.nan(d$x),];
                     d <-  d[!is.nan(d$x),];
                     tmp$x <-  0;
                     tmp$x <-  jitter(tmp$x);
                     tmp$y <-  jitter(tmp$y);
                     if (length(tmp$x) > 0) {
                       grid.points(unit(tmp$x,"npc"),unit(tmp$y,"native"),size=unit(0.5,"char"),gp=gpar(col="blue"),...);
                     }
                   }
                   if (any(is.na(d$x))){
                     d$x <-  1;
                     d$x <-  jitter(d$x);
                     d$y <-  jitter(d$y);
                     if (length(d$x) > 0){
                       grid.points(unit(d$x,"npc"),
                                   unit(d$y,"native"),
                                   gp=gpar(col="red"),
                                   size=unit(0.5,"char"),
                                   ...);
                     }
                   }
                 }
               }
               panel.xyplot(jitter(x,factor=jitter.factor),jitter(y,factor=jitter.factor),col="black",...)
               if (rug){
                 if (is.null(rug.data)){
                   panel.rug(x,col="red",regular=FALSE);
                 } else {
                   panel.rug(rug.data,col="red",regular=FALSE);
                 }
               }
             } else {
               if (show.na){
                 if (any(is.na(x))){
                   d <-  data.frame(x=x,y=y);
                   d <-  d[is.na(d$x),];
                   if (any(is.nan(d$x),na.rm=TRUE)){
                     tmp <- d[is.nan(d$x),];
                     d <-  d[!is.nan(d$x),];
                     tmp$x <-  0;
                     if (length(tmp$x) > 0){
                       grid.points(unit(tmp$x,"npc"),unit(tmp$y,"native"),gp=gpar(col="blue"),size=unit(0.5,"char"),...);
                     }
                   }
                   if (any(is.na(d$x))){
                     d$x <-  1;
                     if (length(d$x) > 0) {
                       grid.points(unit(d$x,"npc"),
                                   unit(d$y,"native"),
                                   gp=gpar(col="red"),
                                   size=unit(0.5,"char"),
                                   ...);
                     }
                   }
                 }
               }
               panel.xyplot(x,y,col="black",...)
               if (rug){
                 if (is.null(rug.data)){
                   panel.rug(x,col="red",regular=FALSE);
                 } else {
                   panel.rug(rug.data,col="red",regular=FALSE);
                 }
               }
             }
           }
         },
         jitter=jitter,
         jitter.factor=jitter.factor,
         vals=s,
         xscale.components=xs,
         key=key.val,
         ...
         )
}

## #################################################################
## Log Axes functions.
## #################################################################

eda.logTicks <- function (lim, loc = c(1, 10), base=10) {
  ii <-floor(log(range(lim), base)) + c(-1, 2)
  main <- base^(ii[1]:ii[2])
  r <- as.numeric(outer(loc, main, "*"))
  r[lim[1] <= r & r <= lim[2]]
}
eda.xscale.components.log10 <-  function(lim,...){
  tryCatch(tmp <<- eda.xscale.components.log10a(lim,...),
           error = function(e) { print(e);tmp <<- xscale.components.default(lim,...);})
  return(tmp);
}
eda.xscale.components.log10a <- function(lim, ...) {
  ans <- xscale.components.default(lim = lim, ...)
  tick.at <- eda.logTicks(10^lim, loc = 1:9)
  tick.at.major <- eda.logTicks(10^lim, loc = 1)
  major <- tick.at %in% tick.at.major
  if (length(tick.at) == 1){
    ans <-  xscale.components.default(lim,...);
    ans$bottom$labels$labels <- parse(text=paste("10^",ans$bottom$ticks$at,sep=""));
    ans$bottom$labels$check.overlap <- TRUE;
    return(ans);
  } else if (length(tick.at) <= 9){
    ans$bottom$labels$labels <- gsub("e([+-])0?","0^",paste(tick.at));
    ans$bottom$ticks$at <- log(tick.at, 10)
    ans$bottom$ticks$tck <- rep(1,length(tick.at));
    ans$bottom$labels$at <- log(tick.at, 10)
    ans$bottom$labels$check.overlap <- TRUE;
    return(ans);
  } else {
    if (sum(major*1) >= 9) {
      ans$bottom$labels$labels <- paste("10^",log10(tick.at),sep="");
    } else {
      ans$bottom$labels$labels <- gsub("e([+-])0?","0^",paste(tick.at));
    }
    ans$bottom$ticks$at <- log(tick.at, 10)
    ans$bottom$ticks$tck <- ifelse(major, 1.5, 0.75)
    ans$bottom$labels$at <- log(tick.at, 10)
    ans$bottom$labels$labels[!major] <- ""
    while (sum(major*1) >= 15){
      w <-  which(ans$bottom$labels$labels != "");
      w <-  w[seq(2,length(w),by=2)];
      ans$bottom$labels$labels[w] = "";
      major[w] = FALSE;
    }
    ans$bottom$labels$labels[major] <- parse(text=ans$bottom$labels$labels[major]);
    ans$bottom$labels$check.overlap <- FALSE
    return(ans)
  }
}

eda.yscale.components.log10 <-  function(lim,...){
  tryCatch(tmp <<- eda.yscale.components.log10a(lim,...),
           error = function(e) { print(e);tmp <<- yscale.components.default(lim,...);})
  return(tmp);
}
eda.yscale.components.log10a <- function(lim, ...) {
  ans <- yscale.components.default(lim = lim, ...)
  tick.at <- eda.logTicks(10^lim, loc = 1:9)
  tick.at.major <- eda.logTicks(10^lim, loc = 1)
  major <- tick.at %in% tick.at.major
  if (length(tick.at) == 1){
    ans <-  yscale.components.default(lim,...);
    ans$left$labels$labels <- parse(text=paste("10^",ans$left$ticks$at,sep=""));
    ans$left$labels$check.overlap <- TRUE;
    return(ans);
  } else if (length(tick.at) <= 9){
    ans$left$labels$labels <- gsub("e([+-])0?","0^",paste(tick.at));
    ans$left$ticks$at <- log(tick.at, 10)
    ans$left$ticks$tck <- rep(1,length(tick.at));
    ans$left$labels$at <- log(tick.at, 10)
    ans$left$labels$check.overlap <- TRUE;
    return(ans);
  } else {
    if (sum(major*1) >= 9) {
      ans$left$labels$labels <- paste("10^",log10(tick.at),sep="");
    } else {
      ans$left$labels$labels <- gsub("e([+-])0?","0^",paste(tick.at));
    }
    ans$left$ticks$at <- log(tick.at, 10)
    ans$left$ticks$tck <- ifelse(major, 1.5, 0.75)
    ans$left$labels$at <- log(tick.at, 10)
    ans$left$labels$labels[!major] <- ""
    while (sum(major*1) >= 15){
      w <-  which(ans$left$labels$labels != "");
      w <-  w[seq(2,length(w),by=2)];
      ans$left$labels$labels[w] = "";
      major[w] = FALSE;
    }
    ans$left$labels$labels[major] <- parse(text=ans$left$labels$labels[major]);
    ans$left$labels$check.overlap <- FALSE
    return(ans)
  }
}

## ###########################################################################
## DV vs IDV by Treatment -- Individual, and Population
## ###########################################################################

dv.vs.idv.by.trt <- function(data,dv="DV",idv="TIME",date=NULL,date.type="DATE",trt="TRT",id="ID",amt="AMT",trt.codes=NULL,xlab.time="Time (hr)",xlab.tad="Time after dose (hr)",png.name=NULL,...){
  if (!is.null(date)){
    data[,idv] <- nm.get.time(data,time=idv,id=id,date=date,date.type=date.type,...);
  }
  d <- data.frame(idv=data[,idv],dv=data[,dv],trt=data[,trt],id=data[,id],amt=data[,amt]);
  if (!is.null(trt.codes) & length(unique(d$trt)) == length(trt.codes)){
    d$trt <-  factor(d$trt,levels=sort(unique(d$trt)));
    levels(d$trt) <-  trt.codes;
  } else {
    d$trt <- paste("Treatment",d$trt);
    d$trt <- factor(d$trt,levels=sort(unique(d$trt)));
  }
  d$id <-  as.factor(paste("ID = ",d$id,sep=""));
  d$tad <- NA
  last.time <-  NA;
  last.id <-  NA;
  for (i in 1:length(d[,1])){
    if (!is.na(last.id) & last.id != d$id[i]){
      last.time <-  NA;
    }
    last.id <- d$id[i];
    if (!is.na(d[i,"amt"]) & d[i,"amt"] > 0){
      d$tad[i] <-  0;
      last.time <-  d$idv[i];
    } else {
      d$tad[i] <- d$idv[i]-last.time;
    }
  }
  for (trt in unique(d$trt)){
    if(!is.null(png.name)){
      fn <- paste(png.name,"ind-plot-dv-vs-time-",
                  gsub("-$","",gsub("^-","",gsub("[^A-Za-z0-9]+","-",trt))),
                  "-%03d.png",
                  sep="");
      png(fn);
    }
    x <- xyplot(dv~idv | id,
                data=d[d$trt==trt,],
                groups=d$id,
                ylab=trt,
                xlab=xlab.time,
                layout=c(3,3),
                amt=d$amt,
                as.table=TRUE,
                panel=function(x,y,subscripts,...,amt){
                  ord <-  order(x);
                  panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                  amt=amt[subscripts[ord]];
                  rug.val <- x[ord];
                  rug.val <- x[!is.na(amt) & amt != 0];
                  panel.rug(rug.val,col="blue",end=0.1);
                },...
                );
    print(x);
    if (!is.null(png.name)){
      dev.off();
      fn <-  sub("-%","-log-%",fn);
      png(fn);
    }
    x <- xyplot(log10(dv)~idv | id,
                data=d[d$trt==trt,],
                groups=d$id,
                ylab=trt,
                xlab=xlab.time,
                layout=c(3,3),
                amt=d$amt,
                as.table=TRUE,
                yscale.components=eda.yscale.components.log10,
                panel=function(x,y,subscripts,...,amt){
                  ord <-  order(x);
                  panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                  amt=amt[subscripts[ord]];
                  rug.val <- x[ord];
                  rug.val <- x[!is.na(amt) & amt != 0];
                  panel.rug(rug.val,col="blue",end=0.1);
                },...
                );
    print(x);
    if (!is.null(png.name)){
      dev.off();
      fn <-  sub("-time-","-tad-",sub("-log-%","-%",fn));
      png(fn);
    }
    x <- xyplot(dv~tad | id,
                data=d[d$trt==trt,],
                groups=d$id,
                ylab=trt,
                xlab=xlab.tad,
                layout=c(3,3),
                as.table=TRUE,
                panel=function(x,y,subscripts,...){
                  ord <-  order(x);
                  panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                },
                ...
                );
    print(x);

    if (!is.null(png.name)){
      dev.off();
      fn <-  sub("-%","-log-%",fn);
      png(fn);
    }

    x <- xyplot(log10(dv)~tad | id,
                data=d[d$trt==trt,],
                groups=d$id,
                ylab=trt,
                xlab=xlab.tad,
                layout=c(3,3),
                as.table=TRUE,
                yscale.components=eda.yscale.components.log10,
                panel=function(x,y,subscripts,...){
                  ord <-  order(x);
                  panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                },
                ...
                );
    print(x);
    if (!is.null(png.name)){
      dev.off();
    }

  }
  for (trt in unique(d$trt)){
    if (!is.null(png.name)){
      fn <- paste(png.name,"spaghetti-plot-dv-vs-tad-",
                  gsub("-$","",gsub("^-","",gsub("[^A-Za-z0-9]+","-",trt))),
                  "-%03d.png",
                  sep="");
      png(fn);
    }
    x <-  xyplot(dv ~ tad,
                 data=d[d$trt==trt,],
                 groups=id,
                 ylab=trt,
                 xlab=xlab.tad,
                 layout=c(1,1),
                 as.table=TRUE,
                 panel=function(x,y,subscripts,...){
                   ord <-  order(x);
                   panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                                        #                         grid.points(unit(rug.val,"native"),unit(rep(0.03,length(rug.val)),"npc"),size=unit(0.5,"char"),pch=4,gp=gpar(col="red"));
                 },
                 ...
                 );
    print(x);
    if (!is.null(png.name)){
      fn <-  sub("-%","-log-%",fn);
      dev.off();
      png(fn);
    }
    x <-  xyplot(log10(dv) ~ tad,
                 data=d[d$trt==trt,],
                 groups=id,
                 ylab=trt,
                 xlab=xlab.tad,
                 layout=c(1,1),
                 as.table=TRUE,
                 scales=list(y=list(log=10)),
                 yscale.components=eda.yscale.components.log10,
                 panel=function(x,y,subscripts,...){
                   ord <-  order(x);
                   panel.superpose(x[ord],y[ord],subscripts[ord],...,type="b",pch=1,cex=0.7,lty=1,col="black");
                                        #                         grid.points(unit(rug.val,"native"),unit(rep(0.03,length(rug.val)),"npc"),size=unit(0.5,"char"),pch=4,gp=gpar(col="red"));
                 },
                 ...
                 );
    print(x);
    if (!is.null(png.name)){
      fn <-  sub("-%","-log-%",fn);
      dev.off();
      png(fn);
    }
  }
}


## Subset Dataset based on IGNORE ACCEPT, etc.

data.nm.ignore.expr <- function(ignore.expr){
                                        # Take away Parenthesis
  ignore.expr <- gsub("[()]","",ignore.expr);
  ## Take out white-space
  ignore.expr <-  gsub("[ \t\n]","",ignore.expr);
  ## Split by comma (if any)
  ignore.expr <- strsplit(ignore.expr,",")[[1]];
  ignore.expr <- gsub("^([^.]*)[.][Ee][Qq][.]['\"]?([^'\"]*)['\"]?$",
                      "(paste(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)]) == \"\\2\")",
                      ignore.expr);
  ## Label.NE.value
  ignore.expr <- gsub("^([^.]*)[.][Nn][Ee][.]['\"]?([^'\"]*)['\"]?$",
                      "(paste(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)]) != \"\\2\")",
                      ignore.expr);
  ## Label.GT.value
  ignore.expr <- gsub("^([^.]*)[.][Gg][Tt][.]['\"]?([^'\"]*)['\"]?$",
                      "(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)] > \\2)",
                      ignore.expr);

  ## Label.LT.value
  ignore.expr <- gsub("^([^.]*)[.][Ll][Tt][.]['\"]?([^'\"]*)['\"]?$",
                      "(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)] < \\2)",
                      ignore.expr);

  ## Label.GE.value
  ignore.expr <- gsub("^([^.]*)[.][Gg][Ee][.]['\"]?([^'\"]*)['\"]?$",
                      "(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)] >= \\2)",
                      ignore.expr);

  ## Label.LE.value
  ignore.expr <- gsub("^([^.]*)[.][Ll][Ee][.]['\"]?([^'\"]*)['\"]?$",
                      "(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)] <= \\2)",
                      ignore.expr);

  ## Label.value, Label=value and Label.EQ.value
  ignore.expr <-  gsub("^([^.=]*)[.=]['\"]?([^'\"]*)['\"]?$",
                       "(paste(return.data[,get.nm.alias(label=\"\\1\",data=return.data,aliases=aliases)]) == \"\\2\")",
                       ignore.expr);


  ignore.expr <- paste(ignore.expr,collapse=" | ");
  ignore.expr <- parse(text=ignore.expr);
  return(ignore.expr);
}
get.nm.alias <- function(label,data,aliases=NULL){
  if (is.null(aliases)){
    aliases <- names(data);
  }
  a <- aliases;
  if (sum(regexpr(paste("^",label,"[ \t]*=",sep=""),a) != -1) == 1){
    return(which(regexpr(paste("^",label,"[ \t]*=",sep=""),a) != -1));
  } else if (sum(regexpr(paste("=[ \t]*",label,"$",sep=""),a) != -1) == 1) {
    return(which(regexpr(paste("=[ \t]*",label,"$",sep=""),a) != -1));
  } else if (sum(regexpr(paste("^",label,"$",sep=""),a) != -1) == 1) {
    return(which(regexpr(paste("^",label,"$",sep=""),a) != -1));
  } else {
    warning("Could not find alias",label);
    return(NULL);
  }
}
nm.data.subset <-  function(data,
                            ignore.char = NULL,
                            ignore = NULL,
                            accept = NULL,
                            aliases = NULL){
  return.data <- data;
  if (!is.null(ignore.char)){
    ignore.char <-  gsub("['\"]","",ignore.char); ## Take out quotations.
    if (ignore.char == "@"){
                                        # Ignore @ or any Alphabetic character.
      return.data <-  return.data[regexpr("^[ \t]*[A-Za-z@]",paste(return.data[,1])) == -1,];
    } else if (ignore.char == "C" || ignore.char == "c") {
                                        # Ignore any character
      return.data <- return.data[regexpr("^[ \t]*[^ ][ \t]*$",paste(return.data[,1])) == -1,];
    } else {
      ## Ignore the character specified.
      return.data <-  return.data[regexpr(paste("^[ \t]*",ignore.char,sep=""),paste(return.data[,1])) == -1,];
    }
  }
  ## Now take from parenthetical IGNORE=() and ACCEPT=() comments.
  if (!is.null(ignore)){
    ignore.expr <- data.nm.ignore.expr(ignore);
    return.data <-  return.data[which(!eval(ignore.expr)),];
  } else if (!is.null(accept)){
    ignore.expr <- data.nm.ignore.expr(accept);
    return.data <-  return.data[which(eval(ignore.expr)),];
  }
  return(return.data);
}

## ###########################################################################
## Overall EDA function
## ###########################################################################

eda.nm.var <-  function(nm.label,data=d,input=input) {
  ## Get NM variable.
  return(names(data)[which(tolower(input) == tolower(nm.label))]);
}

data.checkout.esn <- function(data.file,date.type,max.ids,
                          ignore.char=NULL,ignore=NULL,accept=NULL,
                          input=NULL,
                          xmind=file.exists("esn-xmind.R")){
  dfi <-file.info(data.file);
  dat.file <- data.file;
  d <-  read.csv(dat.file,na.strings=c("."),stringsAsFactors=FALSE);
  tme <- eda.nm.var("TIME",d,input);
  id <-  eda.nm.var("ID",d,input);
  if (is.null(date.type)){
    dte <-  NULL;
  } else {
    dte <-  eda.nm.var(date.type,d,input);
  }
  dte.type <-  date.type;
  d <-  nm.data.subset(data=d,
                       ignore.char = ignore.char,
                       ignore = ignore,
                       accept = accept,
                       aliases = input);
  dat.name <-  paste("./eda/",
                     gsub(".*[/\\\\]([^/\\\\]*)[.][^.]*$","\\1",
                          dat.file),
                     sep="");
  xmind.name <- paste(dat.name,".xmind",sep="");
  if (!is.null(ignore.char)){
    dat.name <-  paste(dat.name,"_IGNORE=",ignore.char,sep="");
  }
  if (!is.null(ignore)){
    dat.name <-  paste(dat.name,"_IGNORE=",ignore,sep="");
  } else if (!is.null(accept)){
    dat.name <-  paste(dat.name,"_ACCEPT=",accept,sep="");
  }
  if (!file.exists("./eda")){
    dir.create("./eda");
  }
  if (!file.exists(dat.name)){
    dir.create(dat.name);
  }

  id.checkout(d,id=id,png.name=paste(dat.name,"/",sep=""),data.file.info=dfi);
  xmind.id <- id.checkout(d,id=id,png.name=paste(dat.name,"/",sep=""),max.ids=max.ids);

  event.history.plot(d,time=tme,id=id,date=dte,date.type=dte.type,png.name=paste(dat.name,"/",sep=""));
  xmind.event <- event.history.plot(d,time=tme,id=id,date=dte,date.type=dte.type,png.name=paste(dat.name,"/",sep=""),max.ids=max.ids);
  if (xmind) {
    if (is.null(ignore.char) && is.null(ignore) &&
        is.null(accept)
        ) {
      topic.title <-  "Data Checkout (All)";
    } else {
      topic.title <-  "Data Checkout";
      if (!is.null(ignore.char)) {
        topic.title <- paste(topic.title,", IGNORE=",ignore.char,sep="");
      }
      if (!is.null(ignore)){
        topic.title <- paste(topic.title,", IGNORE=",ignore,sep="");
      } else if (!is.null(accept)){
        topic.title <- paste(topic.title,", ACCEPT=",accept,sep="");
      }
    }
    xmind <- paste("<topic id=\"ovw",xmind.tm(),"\" timestamp=\"",
                   xmind.timestamp(),
                   "\"><title>",
                   topic.title,
                   "</title><children><topics type=\"attached\">",
                   xmind.id,
                   xmind.event,
                   "</topics></children></topic>",
                   sep="");
    xmind <-  gsub("[.]/eda/","./",xmind);
    if (!file.exists(xmind.name)){
      ## Create Xmind File.
      content <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>",
                       "<xmap-content xmlns=\"urn:xmind:xmap:xmlns:content:2.0\" xmlns:fo=\"http://www.w3.org/1999/XSL/Format\" xmlns:svg=\"http://www.w3.org/2000/svg\" xmlns:xhtml=\"http://www.w3.org/1999/xhtml\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"2.0\">",
                       "<sheet id=\"edas",xmind.tm(),"\">",
                       "<topic id=\"edab",xmind.tm(),"\"><title>",
                       "Data Exploration",
                       "</title><labels><label>",
                       "../",data.file,
                       ,"</label></labels><children>",
                       "<topics type=\"attached\">",
                       xmind,
                       "</topics>",
                       "</children></topic><title>EsN Exploratory Data Analysis </title></sheet></xmap-content>",sep="");
      create.xmind(content,xmind.name);
    } else {
      ## Add Content to current xmind file.
      xmind.decompress <<-  TRUE;
      tryCatch(unzip.xmind(xmind.name),
               error=function(e){xmind.decompress<<-FALSE;});
      if (!xmind.decompress){
        cat("Xmind cannot be decompressed.  Try installing 7zip\n",file=stderr());
      } else {
        cat("Parsing\n");
        xml <-  parse.content();
        cat("Updating\n");
        id <- xmind.get.id(xml,topic.title);
        ## Remove anything with "Topic Title"
        if (!is.null(id)){
          parent <- xmind.parent(xml,id);
          xml <- xmind.rm(xml,id);
        } else {
          parent <-  xmind.get.id(xml,paste("../",data.file,sep=""));
          if (is.null(parent)){
            stop("Can't find Xmind parent topic, so can't update.");
          }
        }
        cat("Saving Xmind File\n");
        xml <- str.xmind(xml);
        write(xml,paste(xmind.dir,"content.xml",sep=""));
        zip.xmind(xmind.dir,xmind.name);
      }
    }

  }
  ##    dv.vs.idv.by.trt(d,idv=tme,id=id,date=dte,date.type=dte.type,trt=trt,trt.codes=trt.codes,png.name=paste(dat.name,"/",sep=""));
}


######################################################################
### esn-eda.R ends here
