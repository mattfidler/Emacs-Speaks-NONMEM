### esn-xpose-summary.R --- Esn Xpose Summary
##
## Filename: esn-xpose-summary.R
## Description:
## Author: Matthew L. Fidler
## Maintainer:
## Created: Tue Feb  9 19:34:16 2010 (-0600)
## Version:
## Last-Updated:
##           By:
##     Update #: 54
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
### Change log:
## 09-Feb-2010    Matthew L. Fidler
##    Took out requirement for Ghostscript.
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



##
if (!exists("logTicks")){
  ##
  ## functions for nice log-axis
  ##
  ## From https://stat.ethz.ch/pipermail/r-help/2008-July/168323.html
  ##
  ## May be used in Xpose?
  ##
  logTicks <<- function (lim, loc = c(1, 5), base=10) {
    ii <-floor(log(range(lim), base)) + c(-1, 2)
    main <- base^(ii[1]:ii[2])
    r <- as.numeric(outer(loc, main, "*"))
    r[lim[1] <= r & r <= lim[2]]
  }
}
setup.esn.variables <- function(){
  if (!exists("smy")){
    smy <<- list();
  }

}

## Variables

## Xpose plot functions for single model

esn.redefs <- function() {
  if (!exists("xpose.plot.qq.orig")){
    xpose.plot.qq.orig <<- xpose.plot.qq;
    xpose.plot.qq <<- function(x,object,...,ylb=get.pretty.plot.text(pre.txt="Quantiles of ",x=x,object=object)){
      xpose.plot.qq.orig(x,object,...,ylb=ylb);
    }
  }
  if (!exists("xpose.plot.splom.orig")){
    xpose.plot.splom.orig <<- xpose.plot.splom;
    xpose.plot.splom <<- function(plist, object,...,varnames=NULL){
      if (is.null(varnames)) {
        varnames <- c()
        for (i in plist) {
          varnames <- c(varnames, get.plot.txt(xlabel(i, object,only.chars=TRUE)))
        }
      } else {
        varnames <- unlist(lapply(varnames,get.plot.txt,only.chars=TRUE));
      }
      xpose.plot.splom.orig(plist, object,...,varnames=varnames);
    }
  }
}


x.figs <- list(
               runsum=list(caption="Run Summary (Xpose)"),
                                        # Basic
               basic.gof=list(caption="Basic Goodness-of-fit plots (Linear)"),
               basic.gof.log=list(caption="Basic Goodness-of-fit plots (Log)",
                 use.log=TRUE),
                                        # DV vs. PRED.
               dv.vs.pred=list(caption="DV vs. PRED (Linear)",fix.it=TRUE),
               dv.vs.pred.log=list(caption="DV vs. PRED (Log)",
                 logy=TRUE,logx=TRUE,fix.it=TRUE),
               dv.vs.pred.by.cov=list(caption="DV vs. PRED by covariates (Linear)",
                 max.plots.per.page=1),
               dv.vs.pred.by.cov.log=list(caption="DV vs. PRED by covariates (Log)",
                 max.plots.per.page=1,logx=TRUE,logy=TRUE),
               dv.vs.pred.by.idv=list(caption="DV vs. PRED by IDV (Linear)",fix.it=TRUE),
               dv.vs.pred.by.idv.log=list(caption="DV vs. PRED by IDV (Log)",
                 logx=TRUE,logy=TRUE,fix.it=TRUE),
               ## DV vs. IPRED.
               dv.vs.ipred=list(caption="DV vs. IPRED (Linear)",fix.it=TRUE),
               dv.vs.ipred.log=list(caption="DV vs. IPRED (Log)",
                 logy=TRUE,logx=TRUE,fix.it=TRUE),
               dv.vs.ipred.by.cov=list(caption="DV vs. IPRED by covariates (Linear)",
                 max.plots.per.page=1),
               dv.vs.ipred.by.cov.log=list(caption="DV vs. IPRED by covariates (Log)",
                 max.plots.per.page=1,logx=TRUE,logy=TRUE),
               dv.vs.ipred.by.idv=list(caption="DV vs. IPRED by IDV (Linear)",fix.it=TRUE),
               dv.vs.ipred.by.idv.log=list(caption="DV vs. IPRED by IDV (Log)",
                 logx=TRUE,logy=TRUE,fix.it=TRUE),
               ## DV vs. PRED&IPRED.
               dv.vs.pred.ipred=list(caption="DV vs. PRED & IPRED (Linear)",fix.it=TRUE),
               dv.vs.pred.ipred.log=list(caption="DV vs. PRED & IPRED (Log)",
                 logx=TRUE,logy=TRUE,fix.it=TRUE),
               ## Individaul Plots
               ind.plots=list(caption="Individual Plots (Linear)",
                 layout=c(8,4),pch=c(16,32,32),cex=c(0.5,0,0),
                 lty=c(0,1,2),
                 lwd=c(0,1.5,1.5),
                 col=c("black","grey25","grey25")),
               ind.plots.log=list(caption="Individual Plots (Semi-Log)",
                 logy=TRUE,
                 layout=c(8,4),pch=c(16,32,32),cex=c(0.5,0,0),
                 lty=c(0,1,2),
                 lwd=c(0,1.5,1.5),
                 col=c("black","grey25","grey25")),
               ind.plots.wres.hist=list(caption="Histogram of IWRES",
                 layout=c(6,4)),
               ind.plots.wres.qq=list(caption="QQ plot of IWRES",
                 layout=c(6,4)),
               ind.plots.cwres.hist=list(caption="Histogram of CWRES",
                 layout=c(6,4)),
               ind.plots.cwres.qq=list(caption="QQ plot of CWRES",
                 layout=c(6,4)),
                                        # WRES
               wres.dist.hist=list(caption="Histogram of WRES",fix.it=TRUE),
               wres.dist.qq=list(caption="QQ plot of WRES",fix.it=TRUE),
               wres.vs.cov=list(caption="WRES vs COV",max.plots.per.page=1),
               wres.vs.idv=list(caption="WRES vs IDV",fix.it=TRUE),
               wres.vs.idv.bw=list(caption="Boxplot of WRES vs IDV",fix.it=TRUE),
               wres.vs.pred=list(caption="WRES vs PRED",fix.it=TRUE),
               wres.vs.pred.bw=list(caption="Boxplot of WRES vs PRED",fix.it=TRUE),
                                        # IWRES
               iwres.dist.hist=list(caption="Histogram of IWRES",fix.it=TRUE),
               iwres.dist.qq=list(caption="QQ Plot of IWRES",fix.it=TRUE),
               iwres.vs.idv=list(caption="IWRES vs IDV",fix.it=TRUE),

               ## Autocorrelation functions.
               autocorr.wres=list(caption="Autocorrelation of WRES",fix.it=TRUE),
               autocorr.cwres=list(caption="Autocorrelation of CWRES",fix.it=TRUE),
               
                                        # ABS functions
               abs.cwres.vs.cov.bw=list(caption="Boxplot of |CWRES| vs COV",
                 max.plots.per.page=1),
               abs.wres.vs.cov.bw=list(caption="Boxplot of |WRES| vs COV",
                 max.plots.per.page=1),
               abs.wres.vs.pred=list(caption="Boxplot of |WRES| vs PRED",fix.it=TRUE),
               abs.cwres.vs.pred=list(caption="Boxplot of |CWRES| vs PRED"),
               abs.wres.vs.pred.by.cov=list(caption="|WRES| vs PRED by COV",
                 max.plots.per.page=1),
               abs.cwres.vs.pred.by.cov=list(caption="|CWRES| vs PRED by COV",
                 max.plots.per.page=1),
               abs.iwres.vs.ipred=list(caption="|IWRES| vs IPRED",fix.it=TRUE),
               abs.iwres.vs.ipred.by.cov=list(caption="|IWRES| vs IPRED by COV",
                 max.plots.per.page=1),
               abs.iwres.wres.vs.ipred.pred=list(caption="|IWRES| vs  IPRED and PRED"),
               ## Parameter plots
               parm.qq=list(caption="QQ plot of parameters",max.plots.per.page=1),
               parm.hist=list(caption="Histogram of parameters",max.plots.per.page=1),
               parm.splom=list(caption="Scatterplot matrix of parameters"),
               param.vs.param=list(caption="Parameter vs Parameter plots",max.plots.per.page=6),
               param.vs.cov=list(caption="Parameter vs. Covariate", max.plots.per.page=6),
               ranpar.qq=list(caption="QQ plot of random parameters",max.plots.per.page=1),
               ranpar.hist=list(caption="Histogram of random parameters",max.plots.per.page=1),
               ranpar.splom=list(caption="Scatterplot of random parameters",max.plots.per.page=1,fix.it=TRUE),
               ranpar.vs.cov=list(caption="Boxplot of random parameters vs covariates",max.plots.per.page=1),
               ## Custom Parameter Plots
               delta.vs.cov=list(caption="Delta vs Covariates"),
               pop.vs.cov=list(caption="Parameter vs Covariates")

               );
## Comparison figures
x.cmp.figs <- list(
                   absval.dpred.vs.cov.model.comp=list(caption="Change in Predictions vs Covariates",max.plots.per.page=2),
                   absval.dipred.vs.cov.model.comp=list(caption="Change in Individual Predicitons vs Covariates",max.plots.per.page=2),
                   absval.dcwres.vs.cov.model.comp=list(caption="Change in Conditional Weighted Residuals vs Covariates",max.plots.per.page=2),
                   absval.dwres.vs.cov.model.comp=list(caption="Change in Weighted Residuals vs Covariates",max.plots.per.page=2),
                   absval.diwres.vs.cov.model.comp=list(caption="Change in Individual Weighted Residuals vs Covariates",max.plots.per.page=2),

                   abs.dpred.vs.cov.model.comp=list(caption="Change in Predictions vs Covariates",max.plots.per.page=2),
                   abs.dipred.vs.cov.model.comp=list(caption="Change in Individual Predicitons vs Covariates",max.plots.per.page=2),
                   abs.dcwres.vs.cov.model.comp=list(caption="Change in Conditional Weighted Residuals vs Covariates",max.plots.per.page=2),
                   abs.dwres.vs.cov.model.comp=list(caption="Change in Weighted Residuals vs Covariates",max.plots.per.page=2),
                   abs.diwres.vs.cov.model.comp=list(caption="Change in Individual Weighted Residuals vs Covariates",max.plots.per.page=2),

                   basic.model.comp=list(caption="Basic Model comparison Plots",max.plots.per.page=2),
                   basic.model.comp.cwres=list(caption="Basic Model comparison Plots (CWRES)",max.plots.per.page=2),
                   add.model.comp=list(caption="Additional Model comparison Plots (CWRES)",max.plots.per.page=2)
                   );

## Misc functions



## Now get control stream labels
xpose.labs <- function(xpdb=NULL,pr=NULL,...){
  if (!any(regexpr("smy",ls(envir=globalenv())) >= 0)) {
    smy <<- list();
  }
  if (is.null(pr) & !is.null(xpdb)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
    com <- gsub(";[;Cc|]*","",pr$mod[which(regexpr(";.*",pr$mod) > -1)]);
                                        #        (re-search-forward "^;[|C]? *\\([A-Z0-9a-z]\\{1,4\\}\\):[ \t]*\\(.+\\)" nil t)
    tmp.f <- function(x){
      ret <-  paste("xpdb@Prefs@Labels[[\"",x[1],"\"]] <- ",sep="");
      ovn <-  x[1];
      x <-  x[-1];
      x <-  gsub("\\","\\\\",x,fixed=TRUE);
      if (length(x) == 1){
        x <-  gsub("^[ \t]*","",x);
        ret <-  paste(ret,"\"",x,"\";",sep="");
      } else {
        if (regexpr("\\(",x[1]) > -1){
          var <-  strsplit(x[1],"\\(")[[1]];
          x[1] <- paste(var[-1],collapse="(");
          var <-  gsub("^[ \t]*","",gsub("[ \t]*$","",var[1]));
          ret <-  paste(ret,"\"",var,"\";",sep="");
          tmp.g <-  function(x){
            tmp1 <-  gsub("(.*)([0-9]+)[ \t]*$","\\1",x);
            tmp2 <-  gsub("(.*)([0-9]+)[ \t]*$","\\2",x);
            return(c(tmp1,tmp2));
          }
          tmp <- sapply(x,tmp.g);
          x <-  tmp[2,];
          x <-  x[-length(x)];
          y <-  tmp[1,];
          y <-  y[-1];
          tmp.g <-  function(z){
            z[-1] <-  gsub("[ ,.)\t]*$","",z[-1]);
            return(paste("\n\tlevels(xpdb@Data[,\"",ovn,"\"]) <- gsub(\"",paste(z,collapse="\",\""),"\",levels(xpdb@Data[,\"",ovn,"\"]))",sep=""));
          }
          ret <- paste(ret,
                       "\nif (any(names(xpdb@Data) == \"",ovn,"\")) {",
                       "\n\tif (!is.factor(xpdb@Data[,\"",ovn,"\"])) {",
                       "\n\t\txpdb@Data[,\"",ovn,"\"] <- factor(xpdb@Data[,\"",ovn,"\"])",
                       "\n\t}",
                       paste(unlist(lapply(data.frame(t(data.frame(x,y,stringsAsFactors=FALSE)),stringsAsFactors=FALSE),tmp.g)),collapse=""),
                       "\n}\n",
                       sep="");

        } else {
          x <- gsub("^[ \t]*","",paste(x,collapse="="));
          ret <-  paste(ret,"\"",x,"\";",sep="");
        }
      }
      return(ret);
    }
    expr <- parse(text=paste(unlist(lapply(strsplit(gsub(" +$","",gsub("^ +","",com[which(regexpr("^ *[A-Z0-9a-z]{1,4}:[ \t]*.+$",com) > -1)],":[ \t]*")),"[:=]"),tmp.f)),collapse="\n"));
    eval(expr);
    ## Now get units if present.
    time.units <-  NULL;
    if (!is.null(xpdb@Prefs@Labels[["TIME"]])){
      if (regexpr(".*\\((ho?u?rs?|seco?n?d?s?|minu?t?e?s?|da?ys)\\)",xpdb@Prefs@Labels[["TIME"]]) > -1){
        time.units <- gsub(".*\\((ho?u?rs?|seco?n?d?s?|minu?t?e?s?|da?ys)\\)","\\1",xpdb@Prefs@Labels[["TIME"]]);
        xpdb@Prefs@Labels[["TI"]] <-  paste("Duration of Infusion (",time.units,")",sep="");
        xpdb@Prefs@Labels[["DUR"]] <-  paste("Duration of Infusion (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TAD"]] <-  paste("Time After Dose (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TSFD"]] <-  paste("Time Since First Dose (",time.units,")",sep="");

        xpdb@Prefs@Labels[["MRT"]] <-  paste("Mean Residence Time (",time.units,")",sep="");
        xpdb@Prefs@Labels[["ALPH"]] <-  paste("Alpha (1/",time.units,")",sep="");
        xpdb@Prefs@Labels[["BETA"]] <-  paste("Beta (1/",time.units,")",sep="");
        xpdb@Prefs@Labels[["HLK"]] <-  paste("T1/2,K (",time.units,")",sep="");
        xpdb@Prefs@Labels[["HLKA"]] <-  paste("T1/2,KA (",time.units,")",sep="");
        xpdb@Prefs@Labels[["HLA"]] <-  paste("T1/2,ALPHA (",time.units,")",sep="");
        xpdb@Prefs@Labels[["HLB"]] <-  paste("T1/2,BETA (",time.units,")",sep="");

        xpdb@Prefs@Labels[["TVMRT"]] <-  paste("Population Mean Residence Time (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVALPH"]] <-  paste("Population Alpha (1/",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVBETA"]] <-  paste("Population Beta (1/",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVHLK"]] <-  paste("Population T1/2,K (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVHLKA"]] <-  paste("Population T1/2,KA (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVHLA"]] <-  paste("Population T1/2,ALPHA (",time.units,")",sep="");
        xpdb@Prefs@Labels[["TVHLB"]] <-  paste("Population T1/2,BETA (",time.units,")",sep="");
        for (nme in names(xpdb@Data)){
          if (regexpr("^(TV)?K(A|EL|[0-9]*)$",toupper(nme)) > -1){
            tmp <-  gsub("^(TV)?","",toupper(nme));
            xpdb@Prefs@Labels[[tmp]] <-  gsub("[ \t]$","",gsub("^[ \t]*","",paste(tmp," (1/",time.units,")",sep="")));
            xpdb@Prefs@Labels[[paste("TV",tmp,sep="")]] <-   gsub("[ \t]$","",gsub("^[ \t]*","",paste("Population ",tmp," (1/",time.units,")",sep="")));
          }
        }
      }
    }
    for (i in seq(1,length(pr$theta.names))){
      vn <- gsub("^[  \t]*","",gsub("[ \t]*\\$[A-Z]+","",gsub("[ \t]*=.*$","",pr$mod[which(regexpr(paste("[ \t]*[^ \t]+[ \t]*=.*\\bTHETA\\(",i,"\\)",sep=""),pr$mod) > -1)])));
      if (regexpr("^TV",vn) > -1 ){
        xpdb@Prefs@Labels[[vn]] <- gsub("[ \t]$","",gsub("^[ \t]*","",paste("Population",pr$theta.names[i],pr$theta.units[i])));
        xpdb@Prefs@Labels[[gsub("^TV","",vn)]] <- gsub("[ \t]$","",gsub("^[ \t]*","",paste(pr$theta.names[i],pr$theta.units[i])));
      } else {
        xpdb@Prefs@Labels[[vn]] <- gsub("[ \t]$","",gsub("^[ \t]*","",paste(pr$theta.names[i],pr$theta.units[i])));
      }
    }
    eval(expr);
    if (any(names(xpdb@Prefs@Labels) == "")){
      tmp <- xpdb@Prefs@Labels;
      xpdb@Prefs@Labels <- tmp[which(names(tmp) != "")];
    }
    return(xpdb);
  } else {
    cat("Requires pr or xpdb to be non-nil\n",file=stderr());
  }
}

xpose.merge <- function(runno,tab.suffix="",
                        table.names=c("sdtab", "mutab", "patab",
                          "catab", "cotab", "mytab", "extra", "xptab", "cwtab"),
                        cwres.name=c("cwtab")) {
  tab.files <- sapply(paste(table.names, runno, tab.suffix,"~*",sep = ""),Sys.glob);
  for (i in 1:length(tab.files)){
    merged.file <- NULL;
    out.file.times <- c();
    if (length(tab.files[[i]]) >= 1){
      for (file in tab.files[[i]]){
        out.file.times <- c(out.file.times,file.info(file)$mtime);
        con <- file(file,"r");
        cont <- readLines(con);
        close(con);
        if (is.null(merged.file)){
          merged.file <- cont;
        } else {
          cont[1] <- "";
          merged.file <- paste(merged.file,cont);
        }
      }
      write.file <- TRUE;
      file <- gsub("~[0-9]+$","",tab.files[[i]][1]);
      if (file.exists(file)){
        file.time <- file.info(file)$mtime;
        if (all(file.time > out.file.times)){
          cat("Not writing ",file," since it is newer than the  files ",file,"~*.\n",sep="");
          write.file <- FALSE;
        }
      }
      if (write.file){
        merged.file <- paste(merged.file,collapse="\n");
        write(merged.file,file);
      }
    }
  }
}

xvarval <- function(x,object,data=object@Data){
                                        # Returns the value of an Xpose object
  vals <- names(data)%in% xvardef2(x,object);
  if (any(vals)){
    ret <- data[,vals]
  } else {
    return(NA);
  }
  return(ret);
}

## Calculation functions.

tex.cmp.notes <-  function(xpdb,xpdb.ref,pr=NULL,pr.ref=NULL,...){
  if (is.null(pr)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
  }
  if (is.null(pr.ref)){
    if (!any(names(smy) == xpdb.ref@Runno)){
      pr.ref <-  getSum(xpdb.ref,...);
      smy[[xpdb.ref@Runno]] <<- pr.ref;
    } else {
      pr.ref <- smy[[xpdb.ref@Runno]];
    }
  }
  grad <- pr$grad;

  n <-  length(grad[[length(grad)]]);

  grad <- pr.ref$grad;
  n.ref <- length(grad[[length(grad)]]);
  items <-  c();
  p.value <-  function(x){
    if (x < 0.001) {
      return("< 0.001");
    } else {
      return(round(x,3));
    }
  }
  if (n > n.ref){
    if ((n - n.ref) == 1){
      items <-  c(items,paste("Added",(n-n.ref),"parameter in",
                              gsub("_","\\\\textunderscore ",xpdb@Runno),
                              "when compared to",gsub("_","\\\\textunderscore ",xpdb.ref@Runno)
                              ));
    } else {
      items <-  c(items,paste("Added",(n-n.ref),"parameters in",
                              gsub("_","\\\\textunderscore ",xpdb@Runno),"when compared to",gsub("_","\\\\textunderscore ",xpdb.ref@Runno)
                              ));
    }
    if (pr$mvof > pr.ref$mvof){
      items <-  c(items,"Increase in MVOF; Adding these parameter(s) makes model worse.");
    } else if (pr$mvof < pr.ref$mvof) {
      items <-  c(items,"Decrease in MVOF; Adding these parameter(s) makes model better.");
      items <-  c(items,paste("Likelihood ratio test p-value",
                              p.value(1-pchisq(pr.ref$mvof-pr$mvof,n-n.ref))));
    } else {
      items <-  c(items,"Identical MVOF; Adding these parameter(s) doesn't change the model.");
    }
  } else if (n.ref > n) {
    if ((n.ref-n) == 1){
      items <-  c(items,paste("Removed",(n.ref-n),"parameter in",
                              gsub("_","\\\\textunderscore ",xpdb@Runno),"when compared to",gsub("_","\\\\textunderscore ",xpdb.ref@Runno)
                              ));
    } else {
      items <-  c(items,paste("Removed",(n-n.ref),"parameters in",
                              gsub("_","\\\\textunderscore ",xpdb@Runno),"when compared to",gsub("_","\\\\textunderscore ",xpdb.ref@Runno)
                              ));
    }
    if (pr$mvof > pr.ref$mvof){
      items <-  c(items,"Increase in MVOF; Removing these parameter(s) makes model worse.");
      items <-  c(items,paste("Liklihood ratio test p-value",
                              p.value(1-pchisq(pr$mvof-pr.ref$mvof,n.ref-n))));

    } else if (pr$mvof < pr.ref$mvof) {
      items <-  c(items,"Decrease in MVOF; Removing these parameter(s) makes model better.");
    } else {
      items <-  c(items,"Identical MVOF; Removing these parameter(s) doesn't change the model.");
    }
  } else {
    items <- c(items,paste("The number of parameters is the same in",gsub("_","\\\\textunderscore ",xpdb@Runno),"and",gsub("_","\\\\textunderscore ",
                                                                                                                           gsub("_","\\\\textunderscore ",xpdb.ref@Runno))));
  }
  items <- paste("\\\\item",items);
  items <-  c(items,paste("\\\\item ",gsub("_","\\\\textunderscore ",xpdb@Runno)," Errors/Notes:"),"\\\\begin{itemize}",paste("\\\\item",pr$errors),"\\\\end{itemize}");
  items <-  c(items,paste("\\\\item ",gsub("_","\\\\textunderscore ",xpdb.ref@Runno)," Errors/Notes:"),"\\\\begin{itemize}",paste("\\\\item",pr.ref$errors),"\\\\end{itemize}");
  items <-  c("\\\\begin{itemize}",items,"\\\\end{itemize}");
  items <- paste("\\\\textbf{",gsub("_","\\\\textunderscore ",xpdb@Runno),"compared to",gsub("_","\\\\textunderscore ",xpdb.ref@Runno),"}\n\n",paste(items,collapse="\n"));
  return(items);
}

tex.theta.table.cmp <-  function(xpdb,xpdb.ref,pr=NULL,pr.ref=NULL,dig=NULL,sigdig=NULL,...){
  if (is.null(pr)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
  }
  if (is.null(pr.ref)){
    if (!any(names(smy) == xpdb.ref@Runno)){
      pr.ref <-  getSum(xpdb.ref,...);
      smy[[xpdb.ref@Runno]] <<- pr.ref;
    } else {
      pr.ref <- smy[[xpdb.ref@Runno]];
    }
  }
  tab1 <-  pr$theta.tab;
  tab2 <-  pr.ref$theta.tab;
  tab1$num <-  seq(1,length(tab1[,1]));
  tab <-  merge(tab1,tab2,by=c("theta","units"),suffixes=c(".new",".cmp"),all=TRUE);
  tab$dest=tab$est.new-tab$est.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$drse=tab$se.new-tab$se.cmp;
  tab <-  tab[,regexpr("\\.(new|cmp)",names(tab))==-1];

  tab1 <- pr$theta.tab;
  tab1$cil = nm.round(tab1$cil,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ciu = nm.round(tab1$ciu,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ci = paste("(",tab1$cil,",",tab1$ciu,")",sep="");

  tab1 <-  tab1[,!(names(tab1) %in% c("cil","ciu"))];

  tab <-  merge(tab1,tab,by=c("theta","units"));
  tab <-  tab[order(tab$num),];
  tab <- tab[,names(tab) != "num"];

  tab$rse = nm.round(tab$rse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$se = nm.round(tab$se,dig=dig,sigdig=sigdig,single=TRUE)
  tab$est = nm.round(tab$est,dig=dig,sigdig=sigdig,single=TRUE)

  tab$drse = nm.round(tab$drse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dse = nm.round(tab$dse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dest = nm.round(tab$dest,dig=dig,sigdig=sigdig,single=TRUE)


  names(tab) <-
    gsub("^units$","Units",
         gsub("^ci$",paste(pr$ci*100,"\\\\% Confidence Interval",sep=""),
              gsub("^rse$","Relative SE\\\\%",
                   gsub("^se$","SE",
                        gsub("^est$","Estimate",
                             gsub("^dci$",paste("$\\\\Delta$ ",pr$ci*100,"\\\\% Confidence Interval",sep=""),
                                  gsub("^drse$","$\\\\Delta$ Relative SE\\\\%",
                                       gsub("^dse$","$\\\\Delta$ SE",
                                            gsub("^dest$","$\\\\Delta$ Estimate",
                                                 gsub("^theta$","$\\\\theta$",
                                                      names(tab)))))))))));
  tmp <- latex.table(tab,longtable=TRUE,rowname=NULL,
                     caption=paste("$\\theta$ estimates \\& change from reference file",gsub("_","\\\\textunderscore ",xpdb.ref@Runno),".",sep=""),
                     label=paste("theta.cmp.estimates",
                       gsub("[ _\\\\\\/\\-]",".",xpdb.ref@Runno)
                       ,sep="")
                     );
  return(tmp);
}


tex.eta.table.cmp <-  function(xpdb,xpdb.ref,pr=NULL,pr.ref=NULL,dig=NULL,sigdig=NULL,...){
  if (is.null(pr)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
  }
  if (is.null(pr.ref)){
    if (!any(names(smy) == xpdb.ref@Runno)){
      pr.ref <-  getSum(xpdb.ref,...);
      smy[[xpdb.ref@Runno]] <<- pr.ref;
    } else {
      pr.ref <- smy[[xpdb.ref@Runno]];
    }
  }
  tab1 <-  pr$eta.tab;
  tab2 <-  pr.ref$eta.tab;
  tab1$num <-  seq(1,length(tab1[,1]));
  tab <-  merge(tab1,tab2,by=c("eta"),suffixes=c(".new",".cmp"),all=TRUE);
  tab$dest=tab$est.new-tab$est.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$drse=tab$se.new-tab$se.cmp;

  tab$dcv=tab$cv.new-tab$cv.cmp;
  tab$dshrink=tab$shrink.new-tab$shrink.cmp;

  tab <-  tab[,regexpr("\\.(new|cmp)",names(tab))==-1];

  tab1$cil = nm.round(tab1$cil,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ciu = nm.round(tab1$ciu,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ci = paste("(",tab1$cil,",",tab1$ciu,")",sep="");
  tab1 <-  tab1[,!(names(tab1) %in% c("cil","ciu","num"))];
  tab <-  merge(tab1,tab,by="eta",all=TRUE);
  tab <-  tab[order(tab$num),names(tab)!= "num"];

  tab$rse = nm.round(tab$rse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$se = nm.round(tab$se,dig=dig,sigdig=sigdig,single=TRUE)
  tab$est = nm.round(tab$est,dig=dig,sigdig=sigdig,single=TRUE)
  tab$cil = nm.round(tab$cil,dig=dig,sigdig=sigdig,single=TRUE)
  tab$ciu = nm.round(tab$ciu,dig=dig,sigdig=sigdig,single=TRUE)
  tab$ci = paste("(",tab$cil,",",tab$ciu,")",sep="");
  tab$shrink = nm.round(tab$shrink,dig=dig,sigdig=sigdig,single=TRUE);

  tab$cv = nm.round(tab$cv,dig=dig,sigdig=sigdig,single=TRUE);
  tab$etabar = nm.round(tab$etabar,dig=dig,sigdig=sigdig,single=TRUE);
  tab$etabar.se = nm.round(tab$etabar.se,dig=dig,sigdig=sigdig,single=TRUE);
  tab$etabar.p = nm.round(tab$etabar.p,dig=dig,sigdig=sigdig,single=TRUE);

  tab$drse = nm.round(tab$drse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dse = nm.round(tab$dse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dest = nm.round(tab$dest,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dshrink = nm.round(tab$dshrink,dig=dig,sigdig=sigdig,single=TRUE);
  tab$dcv = nm.round(tab$dcv,dig=dig,sigdig=sigdig,single=TRUE);
  tab <-  tab[,!(names(tab) %in% c("cil","ciu"))];
  names(tab) <-
    gsub("^drse$","$\\\\Delta$ Relative SE\\\\%",
         gsub("^dse$","$\\\\Delta$ SE",
              gsub("^dest$","$\\\\Delta$ Estimate",
                   gsub("^dshrink$","$\\\\Delta$ Shrinkage \\\\%",
                        gsub("^dcv$","$\\\\Delta$ CV\\\\%",
                             gsub("^etabar.p$","p-value $\\\\overline{eta}$",
                                  gsub("^etabar.se$","SE $\\\\overline{eta}$",
                                       gsub("^etabar$","$\\\\overline{eta}$",
                                            gsub("^shrink$","Shrinkage \\\\%",
                                                 gsub("^cv$","CV\\\\%",
                                                      gsub("^ci$",paste(pr$ci*100,"\\\\% Confidence Interval",sep=""),
                                                           gsub("^rse$","Relative SE\\\\%",
                                                                gsub("^se$","SE",
                                                                     gsub("^est$","Estimate",
                                                                          gsub("^eta$","$\\\\eta$",
                                                                               names(tab))))))))))))))));

  row.names(tab) <-  gsub("([0-9]+)","$\\\\eta_{\\1}$",row.names(tab));

  tmp <- latex.table(tab,longtable=TRUE,
                     caption=paste("$\\eta$ estimates \\& change from reference file",gsub("_","\\\\textunderscore ",xpdb.ref@Runno),".",sep=""),
                     file=".totabtemp",
                     label=paste("eta.cmp.estimates",gsub("[ _\\\\\\/\\-]",".",xpdb.ref@Runno)
                       ,sep=""));
  tmp <-  gsub("{tab}","{~}",paste(tmp,collapse="\n"),perl=TRUE);
  return(tmp);
}

tex.eps.table.cmp <- function(xpdb,xpdb.ref,pr=NULL,pr.ref=NULL,dig=NULL,sigdig=NULL,...){
  if (is.null(pr)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
  }
  if (is.null(pr.ref)){
    if (!any(names(smy) == xpdb.ref@Runno)){
      pr.ref <-  getSum(xpdb.ref,...);
      smy[[xpdb.ref@Runno]] <<- pr.ref;
    } else {
      pr.ref <- smy[[xpdb.ref@Runno]];
    }
  }
  tab1 <-  pr$eps.tab;
  tab2 <-  pr.ref$eps.tab;
  tab1$num <-  seq(1,length(tab1[,1]));
  tab <-  merge(tab1,tab2,by=c("eps"),suffixes=c(".new",".cmp"),all=TRUE);
  tab$dest=tab$est.new-tab$est.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$dse=tab$se.new-tab$se.cmp;
  tab$drse=tab$se.new-tab$se.cmp;

  tab$dcv=tab$cv.new-tab$cv.cmp;
  tab$dshrink=tab$shrink.new-tab$shrink.cmp;

  tab <-  tab[,regexpr("\\.(new|cmp)",names(tab))==-1];

  tab1$cil = nm.round(tab1$cil,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ciu = nm.round(tab1$ciu,dig=dig,sigdig=sigdig,single=TRUE)
  tab1$ci = paste("(",tab1$cil,",",tab1$ciu,")",sep="");
  tab1 <-  tab1[,!(names(tab1) %in% c("cil","ciu","num"))];
  tab <-  merge(tab1,tab,by="eps",all=TRUE);
  tab <-  tab[order(tab$num),names(tab)!= "num"];

  tab$rse = nm.round(tab$rse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$se = nm.round(tab$se,dig=dig,sigdig=sigdig,single=TRUE)
  tab$est = nm.round(tab$est,dig=dig,sigdig=sigdig,single=TRUE)
  tab$cil = nm.round(tab$cil,dig=dig,sigdig=sigdig,single=TRUE)
  tab$ciu = nm.round(tab$ciu,dig=dig,sigdig=sigdig,single=TRUE)
  tab$ci = paste("(",tab$cil,",",tab$ciu,")",sep="");
  tab$shrink = nm.round(tab$shrink,dig=dig,sigdig=sigdig,single=TRUE);

  tab$cv = nm.round(tab$cv,dig=dig,sigdig=sigdig,single=TRUE);

  tab$drse = nm.round(tab$drse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dse = nm.round(tab$dse,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dest = nm.round(tab$dest,dig=dig,sigdig=sigdig,single=TRUE)
  tab$dshrink = nm.round(tab$dshrink,dig=dig,sigdig=sigdig,single=TRUE);
  tab$dcv = nm.round(tab$dcv,dig=dig,sigdig=sigdig,single=TRUE);
  tab <-  tab[,!(names(tab) %in% c("cil","ciu"))];
  names(tab) <-
    gsub("^drse$","$\\\\Delta$ Relative SE\\\\%",
         gsub("^dse$","$\\\\Delta$ SE",
              gsub("^dest$","$\\\\Delta$ Estimate",
                   gsub("^dshrink$","$\\\\Delta$ Shrinkage \\\\%",
                        gsub("^dcv$","$\\\\Delta$ CV\\\\%",
                             gsub("^shrink$","Shrinkage \\\\%",
                                  gsub("^cv$","CV\\\\%",
                                       gsub("^ci$",paste(pr$ci*100,"\\\\% Confidence Interval",sep=""),
                                            gsub("^rse$","Relative SE\\\\%",
                                                 gsub("^se$","SE",
                                                      gsub("^est$","Estimate",
                                                           gsub("^eta$","$\\\\eta$",
                                                                names(tab)))))))))))));

  row.names(tab) <-  gsub("([0-9]+)","$\\\\varepsilon_{\\1}$",row.names(tab));

  tmp <- latex.table(tab,longtable=TRUE,
                     caption=paste("$\\varepsilon$ estimates \\& change from reference file",gsub("_","\\\\textunderscore ",xpdb.ref@Runno),".",sep="")
                     ,file=".totabtemp",label=paste("eps.cmp.estimates",gsub("[ _\\\\\\/\\-]",".",xpdb.ref@Runno),sep=""));
  tmp <-  gsub("{tab}","{~}",paste(tmp,collapse="\n"),perl=TRUE);
  return(tmp);
}



fix.caption <-  function(caption,xpdb,fix.log=FALSE) {
  if (fix.log){
    if (regexpr("\\.log$",caption) != -1){
      caption <- gsub("\\.log$"," (Log)",caption);
    }
  }
  if (regexpr("\\.vs\\.",caption) != -1){
    var1 <- toupper(gsub("^([^.]*).*$","\\1",caption));
    if (regexpr("\\.bw$",caption) != -1){
      var2 <- toupper(gsub("^.*\\.([^.]*)\\.bw$","\\1",caption));
      caption <- paste(var1,"vs.",var2,"(Box and Whiskers)");
    } else if  (regexpr("\\.vs\\.[^.]*\\.by\\.[^.]*$",caption) !=-1) {
      var2 <- toupper(gsub("^.*\\.vs\\.([^.]*).*$","\\1",caption));
      var3 <- toupper(gsub("^.*\\.by\\.([^.]*)$","\\1",caption));
      caption <- paste(var1,"vs.",var2,"by",var3);
    } else if (regexpr("\\.vs\\.[^.]*$",caption) != -1) {
      var2 <- toupper(gsub("^.*\\.([^.]*)$","\\1",caption));
      caption <- paste(var1,"vs.",var2);
    } else if (regexpr("\\.vs\\.[^.]*\\.[^.]*$",caption) != -1) {
      var2 <- toupper(gsub("^.*\\.([^.]*)\\..*$","\\1",caption));
      var3 <- toupper(gsub("^.*\\.[^.]*\\.(.*)$","\\1",caption));
      caption <- paste(var1," vs. ",var2,", ",var3,sep="");
    }
  }
  for (var in names(xpdb@Prefs@Xvardef)){
    if (length(xpdb@Prefs@Xvardef[[var]]) == 1){
      tmp <- gsub("\\([^)]*\\)","",xpdb@Prefs@Xvardef[[var]]);
      caption <- gsub(paste("\\b\\Q",toupper(var),"\\E\\b",sep=""),tmp,caption,perl=TRUE);
    }
  }
  for (var in names(xpdb@Prefs@Labels)){
    tmp <- gsub("\\([^)]*\\)","",xpdb@Prefs@Labels[[var]]);
    caption <- gsub(paste("\\b\\Q",var,"\\E\\b",sep=""),tmp,caption,perl=TRUE);
  }
  caption <- gsub("\\b(COV|cov)\\b","Covariates",caption,perl=TRUE);
  return(caption);

}

get.plot.txt <- function(lab,only.chars = FALSE){
  expr <- FALSE;
  if (length(lab) > 1) {
    return(sapply(lab,get.plot.txt,only.chars=only.chars));
  } else {
    if (!only.chars){
      if (regexpr("\\bETA[0-9]+\\b",lab,perl=TRUE) != -1){
        lab <- gsub("ETA([0-9]+)","\",eta[\\1],\"",lab);
        expr <- TRUE;
      }
      if (regexpr("\\b([VQK])([A-Z0-9a-z]{1,2})\\b",lab,perl=TRUE) != -1) {
        lab <- gsub("\\b([VQK])([A-Z0-9a-z]+)\\b","\",\\1[\"\\2\"],\"",lab);
        expr <- TRUE;
      }
    }
    try(lab <- gsub("(\\^|\\*\\*)2\\b","\262",lab,perl=TRUE),silent=TRUE);
    try(lab <- gsub("\\bug\\b","\265g",lab,perl=TRUE),silent=TRUE);
    try(lab <- gsub("\\bmug\\b","\265g",lab,perl=TRUE),silent=TRUE);
    if (expr){
      lab <- parse(text=paste("paste(\"",lab,"\",sep=\"\")",sep=""));
    }
    return(lab);
  }
}

get.pretty.plot.text <- function(pre.txt="",x=NULL,object=NULL,post.txt="",lab=NULL) {
  if (is.null(lab)){
    lab <- paste(pre.txt,xlabel(x,object),post.txt,sep="");
  }
  lab <- get.plot.txt(lab);
  return(lab);
}

plot.xpose <- function(xpdb,plot,file=NULL,fix.it=FALSE,main="",caption="",pdf=FALSE,ps=FALSE,run.name="run",cmp.file=NULL,
                       pop.check=TRUE,
                       ...){
  if (plot == "ind.plots"){
    if (all(is.infinite(log10(xpdb@Data[[xvardef("ipred", xpdb)]]))) |
        all(is.infinite(log10(xpdb@Data[[xvardef("pred", xpdb)]]))) ) {
      cat("All predictions are zero.\n");
      return("");
    }
  }
  if (regexpr("[Ll]og",caption) > -1){
    if (regexpr("\\.vs\\.",plot)){
      var1 <- gsub("^([^.]*)\\.vs.*","\\1",plot);
      var2 <- gsub("^.*\\.vs\\.([^.]*).*","\\1",plot);
      d <- xpdb@Data
      if (any(names(d) == xvardef2(var1,xpdb))){
        d <- d[d[,xvardef2(var1,xpdb)] != 0,];
      }
      if (any(names(d) == xvardef2(var2,xpdb))){
        d <- d[d[,xvardef2(var2,xpdb)] != 0,];
      }
      if (regexpr("pred\\.ipred",plot) & any(names(d) == xvardef2("ipred",xpdb))){
        d <- d[d[,xvardef2("ipred",xpdb)] != 0,];
      }
      xpdb@Data <- d;
    }
  }
  strp <- function(...,bg){ return(strip.default(bg="grey95",...));}
  if (pop.check){
    if (regexpr("\\.?para?m?\\.?",paste(plot)) != -1){
      ret <- "";
      if (exists("patab.tv")){
        if (is.list(patab.tv)){
          if (any(names(patab.tv) == xpdb@Runno)){
            pop <- xpdb;
            pop@Prefs@Xvardef[["parms"]] <- patab.tv[[pop@Runno]];
            any.different <- FALSE;
            for (var in xvardef2("parms",pop) ){
              if (!is.factor(pop@Data[[var]])) {
                any.different <- TRUE;
              }
            }
            if (any.different){
              ret <- plot.xpose(xpdb=pop,plot=plot,file="pop",fix.it=fix.it,main=main,caption=gsub("parameters","population parameters",caption),pdf=pdf,ps=ps,
                                run.name=run.name,cmp.file=cmp.file,pop.check=FALSE,...);
            }
          }
        }
      }
      if (exists("patab.ind")){
        if (is.list(patab.ind)){
          if (any(names(patab.ind) == xpdb@Runno)){
            ind <- xpdb;
            ind@Prefs@Xvardef[["parms"]] <- patab.ind[[ind@Runno]];
            any.different <- FALSE;
            for (var in xvardef2("parms",ind) ){
              if (!is.factor(ind@Data[[var]])) {
                any.different <- TRUE;
              }
            }
            if (any.different){
              ret <- paste(ret,"\n\n",
                           plot.xpose(xpdb=ind,plot=plot,file="ind",fix.it=fix.it,main=main,caption=gsub("parameters","individual parameters",caption),
                                      pdf=pdf,ps=ps,
                                      run.name=run.name,cmp.file=cmp.file,pop.check=FALSE,...),sep="");
            }
          }
        }
      }
      if (!(ret == "" & exists(plot))){
        return(ret);
      }
    }
  }
  labs <- xpdb@Prefs@Labels
  for (lab in names(labs)){
    if (regexpr("(qq|splom|delta)",plot) != -1){
    } else {
      labs[[lab]] <- get.pretty.plot.text(lab=labs[[lab]],object=xpdb);
    }
  }
  xpdb@Prefs@Labels <- labs;
  ## Fix captions to have labels.
  cat("Run",plot,file=stderr());
  caption <- fix.caption(caption,xpdb);
  if ((regexpr("cwres",plot)!=-1) &
      !(any(names(xpdb@Data) == xvardef2("cwres",xpdb)))){
    cat("\tSkipped; No CWRES.\n",file=stderr());
    return("");
  } else if ((regexpr("iwres",plot)!=-1) &
             !(any(names(xpdb@Data) == xvardef2("iwres",xpdb)))){
    cat("\tSkipped; No IWRES.\n",file=stderr());
    return("");
  } else {
    go <-  TRUE;
    if (regexpr("^[^.\n]*?\\.vs\\.[^.\n]*?(?:\\.bw)?$",plot,perl=TRUE) != -1){
      x <-  gsub("^([^.\n]*?)\\.vs\\.([^.\n]*?)(?:\\.bw|\\.sparse)?$","\\1",plot,perl=TRUE);
      y <-  gsub("^([^.\n]*?)\\.vs\\.([^.\n]*?)(?:\\.bw|\\.sparse)?$","\\2",plot,perl=TRUE);
      if (y == "cov"){
        y = "covariates";
      }
      if (x == "pop"){
        x=y;
      }
      if (x == "eta"){
        x=y;
      }
      if (x == "delta"){
        x=y;
      }
      if(is.null(check.vars2(c(x,y),xpdb))){
        go <-  FALSE;
      }
    } else if  (regexpr("^[^.\n]*?\\.vs\\.[^.\n]*?\\.by\\.[^.\n]*?$",plot,perl=TRUE) != -1){
      x <-  gsub("^([^.\n]*?)\\.vs\\.([^.\n]*?)\\.by\\.[^.\n]*?$","\\1",plot,perl=TRUE);
      y <-  gsub("^([^.\n]*?)\\.vs\\.([^.\n]*?)\\.by\\.[^.\n]*?$","\\2",plot,perl=TRUE);
      z <-  gsub("^([^.\n]*?)\\.vs\\.([^.\n]*?)\\.by\\.([^.\n]*?)$","\\3",plot,perl=TRUE);
      if (y == "cov"){
        y = "covariates";
      }
      if (z == "cov"){
        z = "covariates";
      }
      if(is.null(check.vars2(c(x,y,z),xpdb))){
        go <-  FALSE;
      }
    }
    if (!go){
      cat("\tSkipped;  Not all variables present.\n",file=stderr());
    } else {
      if (is.null(cmp.file)){
        file <- paste(c(plot,file),collapse="-");
      } else {
        file <- paste(c(plot,cmp.file,file),collapse="-");
      }
      file <- gsub("\\.","-",file);
      if (run.name == "run"){
        run <- paste("run",xpdb@Runno,sep="");
      } else {
        run <-  run.name;
      }
      if (!file.exists(paste("./",run,".xfig",sep=""))){
        dir.create(paste("./",run,".xfig",sep=""));
      }
      file<-paste("./",run,".xfig/",file,sep="");
      unlink(paste(file,"-???.png",sep=""))
      plot <- parse(text=paste(plot,"(xpdb,strip=strp,...)",sep=""));
      png(paste(file,"-%03d.png",sep=""),bg="transparent");
      plot <- eval(plot);
      print(plot);
      dev.off();
      if (!go){
        cat("\tError encountered running Xpose command. Aborting.\n",file=stderr());
      } else {
        ##    png(file=paste(file,"-%03d.png",sep=""),
        ##        width=9,height=5.75,units="in",bg="transparent",
        ##        res=400);
        ##    CairoPNG(file=paste(file,"-%03d.png",sep=""),
        ##             width=9,height=5.75,units="in",bg="transparent",
        ##             dpi=400);
        ##    eval(cmd);
        ##    dev.off();
        ## Then put them into LaTeX
        file.name <- paste(file,"-","%03d",sep="");
        fix.caption.latex <- function(x) {
          ret <- x;
          ret <- gsub("([^\\\\])([&])","\\1\\\\\\\\\\2",ret);
          ret <- gsub("|","$|$",ret,fixed=TRUE);
          return(ret);
        }
        latex <- paste("\\\\includegraphics{",file.name,"}\\\\\\\\\n",sep="");
        latex <- paste("\\\\begin{longtable}{c}\n\\\\caption{",fix.caption.latex(caption),
                       "\\\\label{fig:",gsub("[.\\\\\\/\\-]",".",file),"}}.ENDHEAD.\\\\\\\\\n",
                       latex,
                       "\\\\end{longtable}",
                       sep="");
        cat(" Generated",file,"\n",file=stderr());
        return(latex);
      }
    }
  }
  return("");
}
## Overivew table.

## NONMEM summary object.

nm.latex <- function(xpdb,pr=NULL,order=x.order,pdf=FALSE,ps=FALSE,
                     base.file=paste("run",xpdb@Runno,sep=""),compare=NULL,cmp.order=NULL,...){
  if (is.null(pr)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
  }
                                        # Histogram inforamtion.
  xpdb@Prefs@Graph.prefs$hicol="grey75";
  xpdb@Prefs@Graph.prefs$hilwd=1.5;
  xpdb@Prefs@Graph.prefs$hidcol="black";
  xpdb@Prefs@Graph.prefs$hidlwd=1.5;
  xpdb@Prefs@Graph.prefs$hidlty=1;
                                        # Overall information.
  xpdb@Prefs@Graph.prefs$pch=16;
  xpdb@Prefs@Graph.prefs$col="black";
  xpdb@Prefs@Graph.prefs$cex=0.5;
                                        # IDs printed
  xpdb@Prefs@Graph.prefs$idsext=0.001;
  xpdb@Prefs@Graph.prefs$idscex=0.6;
                                        # Box and Whiskers Black and white.
  xpdb@Prefs@Graph.prefs$bwoutcol="black";
  xpdb@Prefs@Graph.prefs$bwumbcol="black";
  xpdb@Prefs@Graph.prefs$bwreccol="black";
  xpdb@Prefs@Graph.prefs$bwoutlwd=1.5;
  xpdb@Prefs@Graph.prefs$bwumblwd=1.5;
  xpdb@Prefs@Graph.prefs$bwreclwd=1.5;

  xpdb@Prefs@Labels$TSLD = "Time Since Last Dose";

                                        #  xpdb@Prefs@Labels
                                        #  xpdb@Prefs@Graph.prefs

  figs <- x.figs;
  ##
  ## Generate extra functions that are not in the current Xpose mileu (but
  ## defined in the Xpose options).
  ##
  new.fns <-  unlist(order)[!(unlist(order) %in% c("overview","errors","par.summary","cov.summary",names(figs)))];
  new.fns.type <- eval(parse(text=paste("c(",paste(paste("tryCatch(is.function(",new.fns,")*1,error=function(e) { return(2) })",sep=""),collapse=","),")",sep="")));
  names(new.fns.type) <- new.fns;
  ## Get Functions that are currently defined.
  tmp.f <- function(x){
    return(paste("figs[[\"",x,"\"]]=list(caption=\"",x,"\");",sep=""));
  }
  eval(parse(text=sapply(names(new.fns.type[new.fns.type==1]),tmp.f)));
  ## Define new x.vs.y functions.
  tmp.f <- function(x){
    all <- x;
    y <- gsub("^(.*?)\\.vs\\.(.*?)$","\\2",x);
    x <- gsub("^(.*?)\\.vs\\.(.*?)$","\\1",x);
    return(paste("eval(make.vs(\"",x,"\",\"",y,"\"),envir=.GlobalEnv);figs[[\"",all,"\"]]=list(caption=paste(xlabel(xvardef2(\"",x,"\", xpdb),xpdb), \" vs \", xlabel(xvardef2(\"",y,"\", xpdb), xpdb),sep=\"\"));",sep=""));
  }
  eval(parse(text=sapply(names(new.fns.type[new.fns.type==2&regexpr("^[A-Za-z0-9]+\\.vs\\.[A-Za-z0-9]+$",names(new.fns.type)) != -1]),tmp.f)));

  ## Define new x.vs.y.bw functions.
  tmp.f <- function(x){
    all <- x;
    y <- gsub("^(.*?)\\.vs\\.(.*?)\\.bw$","\\2",x);
    x <- gsub("^(.*?)\\.vs\\.(.*?)\\.bw$","\\1",x);
    ret <- paste("eval(make.vs.bw(\"",x,"\",\"",y,"\"),envir=.GlobalEnv);figs[[\"",all,"\"]]=list(caption=paste(\"Boxplot of \",xlabel(xvardef2(\"",x,"\", xpdb), xpdb), \" vs \", xlabel(xvardef2(\"",y,"\", xpdb), xpdb),\"\",sep=\"\"));",sep="");
    return(ret);
  }
  eval(parse(text=sapply(names(new.fns.type[new.fns.type==2&regexpr("^[A-Za-z0-9]+\\.vs\\.[A-Za-z0-9]+\\.bw$",names(new.fns.type)) != -1]),tmp.f)));

  ## Define new x.vs.y.by.z functions
  tmp.f <- function(x){
    all <- x;
    y <- gsub("^(.*?)\\.vs\\.(.*?)\\.by\\.(.*?)$","\\2",x);
    z <- gsub("^(.*?)\\.vs\\.(.*?)\\.by\\.(.*?)$","\\3",x);
    x <- gsub("^(.*?)\\.vs\\.(.*?)\\.by\\.(.*?)$","\\1",x);
    return(paste("eval(make.vs.by(\"",x,"\",\"",y,"\",\"",z,"\"),envir=.GlobalEnv);figs[[\"",all,"\"]]=list(caption=paste(xlabel(xvardef2(\"",x,"\", xpdb), xpdb), \" vs \", xlabel(xvardef2(\"",y,"\", xpdb), xpdb),\" by \",xlabel(xvardef2(\"",z,"\",xpdb),xpdb),sep=\"\"));",sep=""));
  }
  eval(parse(text=sapply(names(new.fns.type[new.fns.type==2&regexpr("^([A-Za-z0-9]+|[Dd][Vv]\\.[Pp][Rr][Ee][Dd]|[Pp][Rr][Ee][Dd]\\.[Dd][Vv])\\.vs\\.[A-Za-z0-9]+\\.by\\.[A-Za-z0-9]+$",names(new.fns.type)) != -1]),tmp.f)));
  figs <- c(figs,x.cmp.figs);

  if (is.null(cmp.order) | is.null(compare)){
    figs <- figs[names(figs) %in% unlist(order)];
  } else {
    figs <- figs[names(figs) %in% c(unlist(order),unlist(cmp.order))];
  }
  tmp.f <- function(x) {

    if (regexpr("((th)?(eta|eps)\\.table(\\.cmp)?)",x) == -1) {
      u.log <- regexpr("\\.log$",x)!=-1;
      lst <- figs[[x]];
      fn <- x;

      if (u.log) {
        if (!exists(fn)){
          fn <- gsub("\\.log$","",x);
          lst$file="log";
        }
      }

      if (!exists(fn) & regexpr("abs\\.",fn) != -1){
        fn <- gsub("abs","absval",fn); # Xpose 4.0.4 uses absval instead of abs.
      }

      if (!exists(fn) & regexpr("absval\\.",fn) != -1) {
        fn <- gsub("absval","abs",fn); # Xpose 4.0.1 uses abs instead of absval.
      }

      if (!exists(fn) & regexpr("abs$",fn) != -1){
        fn <- gsub("abs","absval",fn); # Xpose 4.0.4 uses absval instead of abs.
      }
      
      if (!exists(fn) & regexpr("absval$",fn) != -1) {
        fn <- gsub("absval","abs",fn); # Xpose 4.0.1 uses abs instead of absval.
      }

      if (!exists(fn) & regexpr("param\\.",fn) != -1){
        fn <-  gsub("param","parm",fn); # Xpose 4.0.4 uses parm
      }

      if (!exists(fn) & regexpr("param$",fn) != -1){
        fn <-  gsub("param","parm",fn); # Xpose 4.0.4 uses parm
      }

      if (!exists(fn) & regexpr("par\\.",fn) != -1){
        fn <-  gsub("par","parm",fn); # Xpose 4.0.4 uses parm
      }

      if (!exists(fn) & regexpr("par$",fn) != -1){
        fn <-  gsub("par","parm",fn); # Xpose 4.0.4 uses parm
      }

      if (!exists(fn) & regexpr("parm\\.",fn) != -1){
        fn <-  gsub("parm","param",fn); # Older Xpose uses par or param.
        if (!exists(fn)) {
          fn <-  gsub("param","par",fn);
        }
      }

      if (!exists(fn) & regexpr("parm\\.",fn) != -1){
        fn <-  gsub("parm","param",fn); # Older Xpose uses par or param.
        if (!exists(fn)) {
          fn <-  gsub("param","par",fn);
        }
      }
      ## Ok tried everytyhing I know of, if fn doesn't exist return "".
      if (exists(fn)){
        if(any(fn %in% names(x.cmp.figs))){
          ## A comparison figure.
          lst$xpdb=xpdb;
          lst$plot=fn;
          lst$pdf = pdf;
          lst$ps = ps;
          if (regexpr("^run",base.file) == -1){
            lst$run.name = base.file;
            lst$base.file = base.file;
          }
          ret <-  c();
          i <-  0;
          for(xpcompare in compare){
            if (typeof(xpcompare) == "character"){
              xpcompare <- xpcmp[[xpcompare]];
            }
            nlst <-  lst;
            tmp <-  xpcompare;
            if (!is.null(tmp)){
              nlst$object.ref = tmp;
              nlst$cmp.file= i;
              nlst$main=nlst$caption;
              if (fn == "basic.model.comp" |
                  fn == "basic.model.comp.cwres" |
                  fn == "add.model.comp")
                {
                  nlst$caption <- paste(nlst$caption,
                                        ".  RunC =",gsub("_","\\\\textunderscore ",nlst$xpdb@Runno),
                                        "; RunR =",gsub("_","\\\\textunderscore ",tmp@Runno),".",sep="");
                  nlst$xpdb@Runno <-  "C";
                  tmp@Runno <-  "R";
                  nlst$object.ref <-  tmp;
                } else {
                  nlst$caption <- paste(nlst$caption,
                                        " (",
                                        gsub("^ ","",gsub("_","\\\\textunderscore ",nlst$xpdb@Runno)),
                                        " compared to ",
                                        gsub("^ ","",gsub("_","\\\\textunderscore ",tmp@Runno)),")"
                                        ,sep="");
                }
              tryCatch(ret <-c(ret,do.call(plot.xpose,nlst)),
                       error=function(e){cat("Error with previous call... Continuing\n")});
              i <- i+1;
            }
          }
          return(ret);
        } else {
          lst$xpdb=xpdb;
          lst$plot=fn;
          lst$pdf = pdf;
          lst$ps = ps;

          if (regexpr("^run",base.file) == -1){
            lst$run.name = base.file;
            lst$base.file = base.file;
          }
          ## Take off Try.
          return(tryCatch(do.call(plot.xpose,lst)));
        }
      } else {
        cat("Could not find ",fn,".\n");
        return("");
      }
    } else {
      return("");
    }
  }
                                        #    print(names(figs));
  ret <- lapply(names(figs),tmp.f);
  ret <- as.list(ret);
  names(ret) <- names(figs);
  if ((is.null(cmp.order) | is.null(compare))){
    cat("Overview Table\n");
    try(ret[["overview"]] <- overview.table(xpdb,pr,run.summary=NULL,...),silent=FALSE);

    cat("Eps Table\n");
    try(ret[["eps.table"]] <- tex.eps.table(xpdb,pr,...),silent=TRUE);

    cat("Eta Table\n");
    try(ret[["eta.table"]] <- tex.eta.table(xpdb,pr,...),silent=TRUE);


    cat("Theta Table\n");
    try(ret[["theta.table"]] <-  tex.theta.table(xpdb,pr,...),silent=TRUE);

    cat("Error list\n");
    try(ret[["errors"]] <- errors.latex(xpdb,pr),silent=TRUE);
  } else {
    ret <-  ret[!names(ret) %in% c("eps.table","theta.table","eta.table")];
  }
  cat("Parameter Summary\n");
  try(ret[["par.summary"]] <- par.summary.latex(xpdb),silent=TRUE);
  cat("Covariate Summary\n");
  try(ret[["cov.summary"]] <- cov.summary.latex(xpdb),silent=TRUE);
  if (!(is.null(cmp.order) | is.null(compare))){
    try(ret[["overview"]] <- overview.table(xpdb,pr,run.summary=compare,...),silent=FALSE);
    cat("Comparison Notes.\n");
    tmp <-  c()
    for (xpcompare in compare){
      if (typeof(xpcompare) == "character"){
        xpcompare <- xpcmp[[xpcompare]];
      }
      if (!is.null(xpcompare)){
        try(tmp <-  c(tmp,tex.cmp.notes(xpdb,xpcompare,...)));
      }
    }
    ret$cmp.notes <-  tmp;

    cat("Comparison Theta Table\n");
    tmp <-  c();
    for (xpcompare in compare){
      if (typeof(xpcompare) == "character"){
        xpcompare <- xpcmp[[xpcompare]];
      }
      if (!is.null(xpcompare)){
        try(tmp <-  c(tmp,tex.theta.table.cmp(xpdb,xpcompare,...)));
      }
    }
    ret$theta.table.cmp <-  tmp;
    cat("Comparison Eta Table\n");
    tmp <-  c();
    for (xpcompare in compare){
      if (typeof(xpcompare) == "character"){
        xpcompare <- xpcmp[[xpcompare]];
      }
      if (!is.null(xpcompare)){
        try(tmp <-  c(tmp,tex.eta.table.cmp(xpdb,xpcompare,...)));
      }
    }
    ret$eta.table.cmp <-  tmp;

    cat("Comparison Eps Table\n");
    tmp <-  c();
    for (xpcompare in compare){
      if (typeof(xpcompare) == "character"){
        xpcompare <- xpcmp[[xpcompare]];
      }
      if (!is.null(xpcompare)){
        try(tmp <-  paste(tmp,tex.eps.table.cmp(xpdb,xpcompare,...)));
      }
    }
    ret$eps.table.cmp <-  tmp;
  }
  lst <- ret;
  tmp.f2 <- function(x,i){
    if (length(x) >= i){
      return(x[i]);
    } else {
      return(x[1]);
    }
  }
  tmp.f <- function(x,dat=order,i=NULL){
    ## Order the data correctly.
    tmp <- data.frame(what=dat[[x]],stringsAsFactors=FALSE);
    tmp$num <-  seq(1,length(tmp[,1]));
    if (sum(names(lst) %in% dat[[x]]) == 1){
      tmp.name <- names(lst)[names(lst) %in% dat[[x]]];
      if (is.null(i) ) {
        tmp2 <- data.frame(what=tmp.name,val=lst[[tmp.name]],stringAsFactors=FALSE);
      } else {
        tmp2 <- data.frame(what=tmp.name,val=lst[[tmp.name]][i],stringAsFactors=FALSE);
      }
      tmp <-  merge(tmp,tmp2);
    } else if (sum(names(lst) %in% dat[[x]]) != 0){
      tmp2 <- lst[names(lst) %in% dat[[x]]]
      if (!is.null(i)){
        tmp2 <- lapply(tmp2,tmp.f2,i=i);
      }
      for (var in names(tmp2)){
        if (is.null(tmp2[[var]])){
          tmp2[[var]] <- "";
        }
      }
      tmp2 <-  data.frame(what=names(tmp2),val=unlist(tmp2),stringsAsFactors=FALSE);
      tmp <-  merge(tmp,tmp2);
    }
    tmp <-  tmp[order(tmp$num),];
    tmp <-  paste(tmp$val,collapse="\n\n");
    if (is.null(i)){
      return(paste("\\\\section{",x,"}\n\n",tmp,sep=""));
    } else {
      return(paste("\\\\subsection{",x,"}\n\n",tmp,sep=""));
    }
  }
  if (!is.null(cmp.order)){
    ret <- "";
    for (i in 1:length(compare)){
      ret <- paste(ret,"\\\\section{",gsub("^ +","",gsub("_","\\\\textunderscore ",compare[i])),"}\n\n");
      ret <- paste(ret,paste(unlist(lapply(names(cmp.order),tmp.f,dat=cmp.order,i=i)),collapse="\n\n"),sep="\n\n");
    }
    ret <- paste(paste(unlist(lapply(names(order),tmp.f)),collapse="\n\n"),ret,sep="\n\n");
  } else {
    ret <- paste(unlist(lapply(names(order),tmp.f)),collapse="\n\n");
  }
  return(ret);
}


## Extra plotting functions
delta.vs.cov <- function(xpdb,onlyfirst=TRUE,abline=FALSE,smooth=TRUE,
                         type="p",max.plots.per.page=2,main="Default",
                         sigdig=3,eq.on.var.for.no.cov = TRUE,
                         eq = TRUE,
                         ...){
  for (i in xvardef2("parms", xpdb)) {
    if (is.null(i)) {
      cat("Parameters are not properly set in the database!\n")
      return(NULL)
    }
  }
  if (is.null(check.vars2(c("tvparms"),xpdb))){
    cat("To have delta plots you must specify typical value variables (TVCL for population value of CL).\n");
    return(NULL);
  }
  if (is.null(check.vars2(c("dv", "pred", "covariates"), xpdb))) {
    cat("Covariates are undefined in the database!\n");
    return(NULL)
  }
  tvparms <-  xvardef2("tvparms",xpdb);
  tvparms <-  tvparms[gsub("^TV","",tvparms) %in% xvardef2("parms",xpdb)];
  dat <- Data(xpdb);
  for (par in tvparms){
    tv <-  par;
    tr <-  gsub("^TV","",tv);
    tv <- as.numeric(paste(dat[,names(dat) == tv]));
    tr <- as.numeric(paste(dat[,names(dat) == tr]));
    delta <-  tr-tv;
    dat$delta <- delta;
    names(dat) <- gsub("delta",gsub("^TV","DV",par),names(dat));
                                        #    xpdb@Prefs@Labels[[gsub("^TV","DV",par)]] = paste(gsub("^TV","",par),"-",par);
                                        #    xpdb@Prefs@Labels[[gsub("^TV","DV",par)]] = paste(xlabel(gsub("^TV","",par),xpdb),"-",xlabel(par,xpdb),sep="");
    xpdb@Prefs@Labels[[gsub("^TV","DV",par)]] = get.plot.txt(paste(xlabel(gsub("^TV","",par),xpdb)," [Individual - Population]",sep=""));
    covs <- dat[,names(dat) %in% xvardef2("covariates",xpdb)]
    cov <- names(covs);
    tmp <- sapply(xpdb@Data,is.factor);
    tmp <- names(tmp[tmp==TRUE]);
    covs <-  cov[cov %in% tmp];
    Data(xpdb) <- dat;
    if(length(covs) > 0){
      eval(parse(text=paste("xpdb@Data$",covs," <- factor(xpdb@Data$",covs,");",sep="")));
    }
  }
  dvparms <-  gsub("^TV","DV",tvparms);
  number.of.plots <- 0;
  for (i in dvparms) {
    for (j in cov){
      var.val <-  dat[,names(dat) == i];
      if (!all(var.val[1] == var.val)){
        number.of.plots <- number.of.plots + 1
      }
    }
  }
  plotList <- vector("list", number.of.plots)
  plot.num <- 0
  for (i in dvparms) {
    for (j in cov) {
      var.val <-  dat[,names(dat) == i];
      if (!all(var.val[1] == var.val)){
        xplot <- xpose.plot.default(j,i, xpdb, main = NULL,
                                    smooth = smooth, type = type, onlyfirst = onlyfirst,
                                    pass.plot.list = TRUE, layout = layout,
                                    xp.j=j,xp.i=i,sigdig=sigdig,
                                    xp.eq=eq,
                                    eq.on.var.for.no.cov = eq.on.var.for.no.cov,
                                    panel = function(x,y,object,...,xp.i,xp.j,sigdig,onlyfirst,inclZeroWRES,samp,eq.on.var.for.no.cov,xp.eq) {
                                      if (xp.eq){
                                        i = xp.i;
                                        j = xp.j;
                                        xpdb = object;
                                        txt <- "";
                                        ## Figure out if there is not any covariate relationships on the parameter yet by looking at TV values.

                                        tvval <-  gsub("^.*(TV[A-Z0-9]+).*$","\\1",xlabel(i,xpdb));
                                        no.cov.yet <- length(unique(paste(xpdb@Data[,names(xpdb@Data)==tvval]))) == 1;
                                        if (is.factor(x)){
                                        # Factor Shift.
                                          f <- x;
                                          if (eq.on.var.for.no.cov & no.cov.yet){
                                            var <-  gsub("TV","",tvval);
                                            if (!is.null(samp)) {
                                              dat <- SData(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                           subset = NULL, samp = samp)
                                            }
                                            else {
                                              dat <- Data(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                          subset = NULL)
                                            }
                                            var.val <-  dat[,names(dat) == var];
                                            x0 <-  var.val;
                                            lm1 <- lm(x0~f);
                                            tmp <-  coef(lm1);
                                            txt <- paste(var,"=",gsub("*(Intercept)","",gsub("+-","-",paste(paste(sapply(tmp,signif,3),names(tmp),sep="*"),collapse="+"),fixed=TRUE),fixed=TRUE),sep="");
                                            tmp <-  unlist(strsplit(gsub("\\+(.*)=","\\1=",paste("+",strsplit(txt,"\\+")[[1]],sep="")),"\\-"));
                                            tmp <-  gsub("^-(.*)=","\\1=",gsub("^([^+])","-\\1",tmp));
                                            txt2 <-  "";
                                            beg <-  gsub("."," ",gsub("(.*=).*","\\1",tmp[1]));
                                            while (length(tmp) > 0){
                                              txt1 <-  "";
                                              while (nchar(txt1) < 60 & length(tmp) > 0){
                                                txt1=paste(txt1,tmp[1],sep="");
                                                tmp = tmp[-1];
                                              }
                                              if (txt2 == ""){
                                                txt2 = txt1;
                                              } else {
                                                txt2 <-  paste(txt2,txt1,sep="\n");
                                              }
                                              if (length(tmp) > 0){
                                                tmp[1] = paste(beg,tmp[1],sep="");
                                              }
                                            }
                                            txt <-  txt2;
                                          }

                                        } else {
                                          ctr <-  median(x);
                                          y0 <-  x - ctr;
                                          if (eq.on.var.for.no.cov & no.cov.yet){
                                            var <-  gsub("TV","",tvval);
                                            if (!is.null(samp)) {
                                              dat <- SData(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                           subset = NULL, samp = samp)
                                            }
                                            else {
                                              dat <- Data(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                          subset = NULL)
                                            }
                                            var.val <-  dat[,names(dat) == var];
                                            x0 <-  var.val;
                                            lm1 <- lm(x0 ~ y0);
                                            txt <- paste("y = ",signif(coef(lm1)[1],sigdig)," + ",signif(coef(lm1)[2],sigdig),"*(x - ",ctr,")",sep="");
                                            txt <-  gsub("+ -","- ",txt,fixed=TRUE);

                                            ## Exp.
                                            x0 <-  log(var.val);
                                            lm1 <-  lm(x0~y0);
                                            txt <-  paste(txt,"; y = ",signif(exp(coef(lm1)[1]),sigdig),
                                                          "*exp(",signif(coef(lm1)[2],sigdig),"*(x - ",ctr,"))",sep="");
                                            ## Pow.
                                            y0 <-  log(x/ctr);
                                            lm1 <-  lm(x0~y0);
                                            txt <-  paste(txt,"\ny = ",signif(exp(coef(lm1)[1]),sigdig),
                                                          "*(x/",ctr,")^",signif(coef(lm1)[2],sigdig),sep="");
                                          } else {
                                            x0 <-  y;
                                            lm1 <- lm(x0 ~ y0 + 0);
                                            txt <- paste("y = ",signif(coef(lm1)[1],sigdig),"*(x - ",ctr,")",sep="");

                                            ## Now Exponential
                                            x0 <- log(x0);
                                            lm1 <-  lm(x0 ~y0);
                                            txt <-  paste(txt,"; y = ",signif(exp(coef(lm1)[1]),sigdig),
                                                          "*exp(",signif(coef(lm1)[2],sigdig),"*(x - ",ctr,"))",sep="");
                                            ## Now Power
                                            y0 <-  log(x/ctr);
                                            lm1 <-  lm(x0 ~y0);
                                            txt <-  paste(txt,"\ny = ",signif(exp(coef(lm1)[1]),sigdig),
                                                          "*(x/",ctr,")^",signif(coef(lm1)[2],sigdig),sep="");
                                          }
                                        }
                                        ovp <- upViewport();
                                        grid::grid.text(txt,just=c("center","top"),x=grid::unit(0.5,"npc"),y=grid::unit(0.97,"npc"),gp=gpar(cex=0.6,fontface="bold")
                                                        );
                                        downViewport(ovp);
                                      }
                                      xpose.panel.default(x,y,object,...,onlyfirst=onlyfirst,inclZeroWRES=inclZeroWRES,samp=samp);
                                    },
                                    ...)

        plot.num <-  plot.num + 1;
        plotList[[plot.num]] <- xplot
      }
    }
  }
  default.plot.title <- "Delta Plots vs. covariates "
  plotTitle <- xpose.multiple.plot.title(object = xpdb, plot.text = default.plot.title,
                                         main = main, ...)
  xpose.multiple.plot.default(plotList, plotTitle = plotTitle,
                              max.plots.per.page = max.plots.per.page,...)
  invisible()
}


## COV.summary for LaTeX
cov.summary.latex <- function (object, subset = xsubset(object),
                               inclZeroWRES = FALSE,...)
{
  ## Need to add categorical data
  if (any(is.null(xvardef2("covariates", object)))) {
    return(cat("No covariates found in the current database!\n"))
  }
  data <- Data(object, onlyfirst = FALSE, subset = subset,
               inclZeroWRES = inclZeroWRES)
  covnams <- object@Prefs@Xvardef$covariates
  cats <- NULL
  conts <- NULL
  for (cov in covnams) {
    if (is.factor(data[[cov]])) {
      cats <- c(cats, cov)
    }
    else {
      conts <- c(conts, cov)
    }
  }
  ret <- "";
  tmp.f <- function(x){
    ret <- xlabel(x,object);
    if (is.null(ret)){
      ret <- x;
    }
                                        # Fix units with m^2 to have the proper values.
    ret <- gsub("(\\^|\\*\\*)([0-9]+)","$^{\\2}$",ret);
    return(ret);
  }
  if (!is.null(cats)) {
    cat.mat <- categorical.table(object, cats, onlyfirst = TRUE,
                                 subset = subset, inclZeroWRES = inclZeroWRES);
    cat.matA <- categorical.table(object, cats, onlyfirst = FALSE,
                                  subset = subset, inclZeroWRES = inclZeroWRES);
    class(cat.mat) <- "matrix";
    class(cat.matA) <- "matrix";
    cat.mat[,1] <- unlist(lapply(cat.mat[,1],tmp.f));
    cat.matA[,1] <- unlist(lapply(cat.matA[,1],tmp.f));
    cat.matA <- cat.matA[,3:dim(cat.matA)[2]];
    d1 <- dim(cat.mat)[2]-2;
    d2 <- dim(cat.matA)[2];
    tmp <- cbind(cat.mat,cat.matA);
    tmp <- gsub("%","\\\\\\\\%",tmp);
    tmp[1,3:dim(tmp)[2]] <-paste("\\\\textbf{",tmp[1,3:dim(tmp)[2]],"}",sep="");
    tmp[1,2] <- "";
    tmp[2:dim(tmp)[1],1] <- unlist(lapply(tmp[2:dim(tmp)[1],1],getVarName));
    buildRows <- function(x){
      row<-strsplit(tmp[x,],"\n");
      len <- length(row[[2]])
      row[[1]]=c(paste(ifelse(len<=1,"",paste("{}rowspan",len," ",sep="")),"\\\\textbf{",
           row[[1]][1],
           "}",sep=""),
           rep("",len-1));
      row <- unlist(row);
      row <- t(matrix(row,ncol=len,byrow=TRUE));
      nrow <- dim(row)[2];
      eval(parse(text=paste("row <- paste(",paste(paste("row[,",1:nrow,"]",sep=""),collapse=","),",sep=\"&\");",sep="")));
      row <- paste(row,collapse="\\\\\\\\\\\\hline\n");
      return(row);
    }
    body <- paste("\\\\begin{longtable}{||r||c||",paste(rep("c|",times=d1),collapse=""),"|",
                  paste(rep("c|",times=d2),collapse=""),"|}\n",
                  "\\\\caption{Categorical covariate summary\\\\label{tab:cat",gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(tmp[1,]),"\n",
                  "\\\\hline\\\\hline\n",
                  "{}rowspan2 %.GREYBG.\n&{}rowspan2 \\\\textbf{Category}&\\\\multicolumn{",d1,
                  "}{||c||}{\\\\textbf{Subject(s)}}&\\\\multicolumn{",d2,"}{||c||}{\\\\textbf{Observation(s)}}\\\\\\\\\\\\hline\n",
                  paste(tmp[1,],collapse="&"),".ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",

                  paste(sapply(2:dim(tmp)[1],buildRows),collapse="\\\\\\\\\\\\hline\\\\hline\n"),
                  "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                  sep="");
    ret <- body;
  }
  if (!is.null(conts)) {
    con.mat <- continuous.table(object, conts, onlyfirst = TRUE,
                                subset = subset, inclZeroWRES = inclZeroWRES)
    con.matA <- continuous.table(object, conts, onlyfirst = FALSE,
                                 subset = subset, inclZeroWRES = inclZeroWRES)
    class(con.mat) <- "matrix";
    class(con.matA) <- "matrix";
    con.mat[,1] <- unlist(lapply(con.mat[,1],tmp.f));
    con.matA[,1] <- unlist(lapply(con.matA[,1],tmp.f));
    con.mat[,7] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.mat[,7],perl=TRUE));
    con.matA[,7] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.matA[,7],perl=TRUE));

    con.mat[2:dim(con.mat)[1],1] <- unlist(lapply(con.mat[2:dim(con.mat)[1],1],getVarName));

    tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.mat[2:dim(con.mat)[1],",1:dim(con.mat)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
    tmp1 <-  con.mat[1,];
    tmp1 <- tmp1[-1];
    tmp <- paste("\\\\begin{longtable}{||r||",paste(rep("c|",times=dim(con.mat)[2]-1),collapse=""),"|}\n",
                 "\\\\caption{Baseline continuous covariates\\\\label{tab:cov.base.",gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.mat[1,]),"\n",
                 "\\\\hline\\\\hline\n",
                 "%.GREYBG.\n&",
                 "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                 "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                 tmp,
                 "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                 sep=""
                 );
    ret <- c(ret,tmp);
    tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.matA[2:dim(con.matA)[1],",1:dim(con.matA)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
    tmp1 <-  con.matA[1,];
    tmp1 <- tmp1[-1];
    tmp <- paste("\\\\begin{longtable}{||r||",paste(rep("c|",times=dim(con.matA)[2]-1),collapse=""),"|}\n",
                 "\\\\caption{All observations of continuous covariates\\\\label{tab:cov.all.",gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.matA[1,]),"\n",
                 "\\\\hline\\\\hline\n",
                 "%.GREYBG.\n&",
                 "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                 "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                 tmp,
                 "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                 sep=""
                 );
    ret <- c(ret,tmp);
    ret <- paste(ret,collapse="\n\n");
    ## Now by each category.
    if (!is.null(cats) & is.null(subset)){
      for (cat in cats){
        con.mat <- NULL;
        con.matA <- NULL;
        for (lvl in levels(object@Data[,cat])){
          subst <- paste(cat,"==\"",lvl,"\"",sep="");
          tmp.mat<- continuous.table(object, conts, onlyfirst = TRUE,
                                     subset = subst, inclZeroWRES = inclZeroWRES);
          class(tmp.mat) <- "matrix";
          tmp <- c(xlabel(cat,object),rep(paste(lvl),times=length(tmp.mat[,1])-1));
          tmp.mat <- cbind(tmp.mat[,1],tmp,tmp.mat[,-1]);
          if (is.null(con.mat)){
            con.mat <- tmp.mat;
          } else {
            con.mat <- rbind(con.mat,tmp.mat[-1,]);
          }
          tmp.mat<- continuous.table(object, conts, onlyfirst = FALSE,
                                     subset = subst, inclZeroWRES = inclZeroWRES);
          class(tmp.mat) <- "matrix";
          tmp <- c(xlabel(cat,object),rep(paste(lvl),times=length(tmp.mat[,1])-1));
          tmp.mat <- cbind(tmp.mat[,1],tmp,tmp.mat[,-1]);
          if (is.null(con.matA)){
            con.matA <- tmp.mat;
          } else {
            con.matA <- rbind(con.matA,tmp.mat[-1,]);
          }
        }
        con.mat[,1] <- unlist(lapply(con.mat[,1],tmp.f));
        con.mat <- con.mat[c(1,(order(con.mat[-1,1],con.mat[-1,2])+1)),];
        con.mat[duplicated(con.mat[,1]),1] <- "";
        con.mat[,8] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.mat[,8],perl=TRUE));
        tmp <- which(con.mat[,1] != "");
        x <- tmp;
        x <- (x[-1]-x[-length(x)]);
        if (!all(x[1]==x)){
          print(con.mat);
          stop("Somethings wrong with the table.");
        }
        x <- x[1];
        con.mat[tmp,1] <- paste(ifelse(x <= 1,"",paste("{}rowspan",x," ",sep="")),"\\\\textbf{",con.mat[tmp,1],"}",sep="");
        tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.mat[2:dim(con.mat)[1],",1:dim(con.mat)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
        tmp1 <-  con.mat[1,];
        tmp1 <-  tmp1[-1];
        tmp <- paste("\\\\begin{longtable}{||r|r||",paste(rep("c|",times=dim(con.mat)[2]-2),collapse=""),"|}\n",
                     "\\\\caption{Baseline continuous covariates by ",xlabel(cat,object),"\\\\label{tab:cov.base.by.",cat,".",
                     gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.mat[1,]),"\n",
                     "\\\\hline\\\\hline\n",
                     "%.GREYBG.\n&",
                     "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                     "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                     tmp,
                     "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                     sep=""
                     );
        ret <- c(ret,tmp);
        ret <- paste(ret,collapse="\n\n");

        con.matA[,1] <- unlist(lapply(con.matA[,1],tmp.f));
        con.matA <- con.matA[c(1,(order(con.matA[-1,1],con.matA[-1,2])+1)),];
        con.matA[duplicated(con.matA[,1]),1] <- "";
        con.matA[,8] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.matA[,8],perl=TRUE));
        tmp <- which(con.matA[,1] != "");
        x <- tmp;
        x <- (x[-1]-x[-length(x)]);
        if (!all(x[1]==x)){
          print(con.matA);
          stop("Somethings wrong with the table.");
        }
        x <- x[1];
        con.matA[tmp,1] <- paste(ifelse(x <= 1,"",paste("{}rowspan",x," ",sep="")),"\\\\textbf{",con.matA[tmp,1],"}",sep="");
        tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.matA[2:dim(con.matA)[1],",1:dim(con.matA)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
        tmp1 <-  con.matA[1,];
        tmp1 <- tmp1[-1];
        tmp <- paste("\\\\begin{longtable}{||r|r||",paste(rep("c|",times=dim(con.matA)[2]-2),collapse=""),"|}\n",
                     "\\\\caption{All observations of continuous covariates by ",xlabel(cat,object),"\\\\label{tab:cov.base.by.",cat,".",
                     gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.matA[1,]),"\n",
                     "\\\\hline\\\\hline\n",
                     "%.GREYBG.\n&",
                     "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                     "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                     tmp,
                     "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                     sep=""
                     );
        ret <- c(ret,tmp);
        ret <- paste(ret,collapse="\n\n");
      }
    }

  }
  ret <- gsub("\\\\cdot\\b","\\\\\\\\cdot",ret,perl=TRUE);
  ret <- gsub("e(-?[0-9]+)","$\\\\\\\\cdot$$10^{\\1}$",ret);
  ret <- gsub("\\{(-?)0+","{\\1",ret);
  ret <- gsub("([^{])-","\\1$-$",ret);
  return(ret);
}
par.summary.latex <-
  function (object, subset = xsubset(object),
            inclZeroWRES = FALSE)
{
  par.rename.f <- function(x){
    ret <- xlabel(x,object);
    if (is.null(ret)){
      ret <- "";
    }
    ret <- gsub("ETA([0-9]+)","$\\\\eta_{\\1}$",ret);
    ret <- gsub("(\\(0-[^\\)]*\\))","$_{\\1}$",ret);
    ret <- gsub("(\\^|\\*\\*)([0-9]+)","$^{\\2}$",ret);
    ret <- gsub("\\b([KkVvQq])([0-9CcPpSs]+)\\b","\\1$_{\\2}$",ret,perl=TRUE);
    ret <- gsub("\\b[Tt](1/2,[^ ]*)\\b","t$_{\\1}$",ret,perl=TRUE);
    ret <- gsub("\\b([CcTt])max\\b","\\1$_{max}$",ret,perl=TRUE);
    ret <- gsub("[Aa]lpha([^}])","$\\\\alpha$\\1",ret);
    ret <- gsub("[Aa]lpha}","\\\\alpha}",ret);
    ret <- gsub("[Bb]eta([^}])","$\\\\beta$\\1",ret);
    ret <- gsub("[Bb]eta}","\\\\beta}",ret);
    ret <- gsub("\\b[Aa][Op][Bb]\\b","A/B",ret,perl=TRUE);
    ret <- gsub("\\*","$\\\\cdot$",ret);
    return(ret);
  }
  if (is.null(object@Prefs@Xvardef$parms)) {
    cat("The current database has no parameters defined!\n")
    invisible()
    return("");
  }
  data <- Data(object, onlyfirst = FALSE, subset = subset,
               inclZeroWRES = inclZeroWRES)
  if (any(is.null(data))) {
    return("The subset expression is invalid.")
  }
  parnams <- object@Prefs@Xvardef$parms
  cats <- NULL
  conts <- NULL
  for (parm in parnams) {
    if (is.factor(data[[parm]])) {
      cats <- c(cats, parm)
    }
    else {
      conts <- c(conts, parm)
    }
  }
  if (!is.null(cats)) {
    buildRows <- function(x){
      row<-strsplit(tmp[x,],"\n");
      len <- length(row[[2]])
      row[[1]]=c(paste(ifelse(len <= 1,"",paste("{}rowspan",len," ",sep="")),row[[1]][1],sep=""),
           rep("",len-1));
      row <- unlist(row);
      row <- t(matrix(row,ncol=len,byrow=TRUE));
      nrow <- dim(row)[2];
      eval(parse(text=paste("row <- paste(",paste(paste("row[,",1:nrow,"]",sep=""),collapse=","),",sep=\"&\");",sep="")));
      row <- paste(row,collapse="\\\\\\\\\\\\hline\n");
      return(row);
    }
    cat.mat <- categorical.table(object, cats, onlyfirst = TRUE,
                                 subset = subset, inclZeroWRES = inclZeroWRES);
    cat.matA <- categorical.table(object, cats, onlyfirst = FALSE,
                                  subset = subset, inclZeroWRES = inclZeroWRES);

    class(cat.mat) <- "matrix";
    class(cat.matA) <- "matrix";

    cat.mat[,1] <- unlist(lapply(cat.mat[,1],par.rename.f));
    cat.mat <- cat.mat[c(1,order(cat.mat[-1,1])+1),];

    cat.matA[,1] <- unlist(lapply(cat.matA[,1],par.rename.f));
    cat.matA <- cat.matA[c(1,order(cat.matA[-1,1])+1),];


    cat.matA <- cat.matA[,3:dim(cat.matA)[2]];
    d1 <- dim(cat.mat)[2]-2;
    d2 <- dim(cat.matA)[2];
    tmp <- cbind(cat.mat,cat.matA);
    tmp <- gsub("%","\\\\\\\\%",tmp);
    tmp[1,3:dim(tmp)[2]] <-paste("\\\\textbf{",tmp[1,3:dim(tmp)[2]],"}",sep="");
    tmp[1,2] <- "";
    body <- paste("\\\\begin{longtable}{||r||c||",paste(rep("c|",times=d1),collapse=""),"|",
                  paste(rep("c|",times=d2),collapse=""),"|}\n",
                  "\\\\caption{Summary of parameters with distinct values\\\\label{tab:par.cat",gsub("[ ._$\\-]","",object@Runno),"}}\\\\\\\\%spancol=",length(tmp[1,]),"\n",
                  "\\\\hline\\\\hline\n",
                  "{}rowspan2 \\\\textbf{Parameter}\n&{}rowspan2 \\\\textbf{Value}&\\\\multicolumn{",d1,
                  "}{||c||}{\\\\textbf{Baseline}}&\\\\multicolumn{",d2,"}{||c||}{\\\\textbf{All Observation(s)}}\\\\\\\\\\\\hline\n",
                  paste(tmp[1,],collapse="&"),".ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                  paste(sapply(2:dim(tmp)[1],buildRows),collapse="\\\\\\\\\\\\hline\\\\hline\n"),
                  "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                  sep="");
    ret <- body;
  }
  if (!is.null(conts)) {
    con.mat <- continuous.table(object, conts, onlyfirst = TRUE,
                                subset = subset, inclZeroWRES = inclZeroWRES)
    con.matA <- continuous.table(object, conts, onlyfirst = FALSE,
                                 subset = subset, inclZeroWRES = inclZeroWRES)
    class(con.mat) <- "matrix";
    class(con.matA) <- "matrix";

    con.mat[,1] <- unlist(lapply(con.mat[,1],par.rename.f));
    con.mat <- con.mat[c(1,order(con.mat[-1,1])+1),];
    con.mat[,7] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.mat[,7],perl=TRUE));

    con.matA[,1] <- unlist(lapply(con.matA[,1],par.rename.f));
    con.matA[,7] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.matA[,7],perl=TRUE));

    tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.mat[2:dim(con.mat)[1],",1:dim(con.mat)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
    tmp <- paste("\\\\begin{longtable}{||r||",paste(rep("c|",times=dim(con.mat)[2]-1),collapse=""),"|}\n",
                 "\\\\caption{Parameters with a range of values for baseline observations\\\\label{tab:par.base.",gsub("[ ._$\\-]","",object@Runno),"}}\\\\\\\\%spancol=",length(con.mat[1,]),"\n",
                 "\\\\hline\\\\hline\n",
                 "\\\\textbf{",paste(con.mat[1,],collapse="}&\\\\textbf{"),
                 "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                 tmp,
                 "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                 sep=""
                 );
    ret <- c(ret,tmp);
    tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.matA[2:dim(con.matA)[1],",1:dim(con.matA)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
    tmp <- paste("\\\\begin{longtable}{||r||",paste(rep("c|",times=dim(con.matA)[2]-1),collapse=""),"|}\n",
                 "\\\\caption{Parameters with a range of values for all observations\\\\label{tab:par.all.",gsub("[ ._$\\-]","",object@Runno),"}}\\\\\\\\%spancol=",length(con.matA[1,]),"\n",
                 "\\\\hline\\\\hline\n",
                 "\\\\textbf{",paste(con.matA[1,],collapse="}&\\\\textbf{"),
                 "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                 tmp,
                 "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                 sep=""
                 );
    ret <- c(ret,tmp);
    ret <- paste(ret,collapse="\n\n");
    ## Now by each category.
    cats <- NULL
    covnams <- object@Prefs@Xvardef$covariates
    for (cov in covnams) {
      if (is.factor(data[[cov]])) {
        cats <- c(cats, cov)
      }
    }
    if (!is.null(cats) & is.null(subset)){
      for (cat in cats){
        con.mat <- NULL;
        con.matA <- NULL;
        for (lvl in levels(object@Data[,cat])){
          subst <- paste(cat,"==\"",lvl,"\"",sep="");
          tmp.mat<- continuous.table(object, conts, onlyfirst = TRUE,
                                     subset = subst, inclZeroWRES = inclZeroWRES);
          class(tmp.mat) <- "matrix";
          tmp <- c(xlabel(cat,object),rep(paste(lvl),times=length(tmp.mat[,1])-1));
          tmp.mat <- cbind(tmp.mat[,1],tmp,tmp.mat[,-1]);
          if (is.null(con.mat)){
            con.mat <- tmp.mat;
          } else {
            con.mat <- rbind(con.mat,tmp.mat[-1,]);
          }
          tmp.mat<- continuous.table(object, conts, onlyfirst = FALSE,
                                     subset = subst, inclZeroWRES = inclZeroWRES);
          class(tmp.mat) <- "matrix";
          tmp <- c(xlabel(cat,object),rep(paste(lvl),times=length(tmp.mat[,1])-1));
          tmp.mat <- cbind(tmp.mat[,1],tmp,tmp.mat[,-1]);
          if (is.null(con.matA)){
            con.matA <- tmp.mat;
          } else {
            con.matA <- rbind(con.matA,tmp.mat[-1,]);
          }
        }
        con.mat[,1] <- unlist(lapply(con.mat[,1],par.rename.f));
        con.mat <- con.mat[c(1,(order(con.mat[-1,1],con.mat[-1,2])+1)),];
        con.mat[duplicated(con.mat[,1]),1] <- "";
        con.mat[,8] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.matA[,8],perl=TRUE));
        tmp <- which(con.mat[,1] != "");
        x <- tmp;
        x <- (x[-1]-x[-length(x)]);
        if (!all(x[1]==x)){
          print(con.mat);
          stop("Somethings wrong with the table.");
        }
        x <- x[1];
        con.mat[tmp,1] <- paste(ifelse(x <= 1,"","{}rowspan",x," ",sep=""),"\\\\textbf{",con.mat[tmp,1],"}",sep="");
        tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.mat[2:dim(con.mat)[1],",1:dim(con.mat)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
        tmp1 <-  con.mat[1,];
        tmp1 <- tmp1[-1];
        tmp <- paste("\\\\begin{longtable}{||r|r||",paste(rep("c|",times=dim(con.mat)[2]-2),collapse=""),"|}\n",
                     "\\\\caption{Parameters stratified by ",
                     par.rename.f(cat)," with a range of values for baseline observations\\\\label{tab:cov.base.by.",cat,".",
                     gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.mat[1,]),"\n",
                     "\\\\hline\\\\hline\n",
                     "%.GREYBG.\n&",
                     "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                     "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                     tmp,
                     "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                     sep=""
                     );
        ret <- c(ret,tmp);
        ret <- paste(ret,collapse="\n\n");

        con.matA[,1] <- unlist(lapply(con.matA[,1],par.rename.f));
        con.matA <- con.matA[c(1,(order(con.matA[-1,1],con.matA[-1,2])+1)),];
        con.matA[duplicated(con.matA[,1]),1] <- "";
        con.matA[,8] <- gsub("e,([0-9]+)-","e-\\1,",gsub("^(-?[^-]*)-(.*)$","(\\1,\\2)",con.matA[,8],perl=TRUE));
        tmp <- which(con.matA[,1] != "");
        x <- tmp;
        x <- (x[-1]-x[-length(x)]);
        if (!all(x[1]==x)){
          print(con.matA);
          stop("Somethings wrong with the table.");
        }
        x <- x[1];
        con.matA[tmp,1] <- paste(ifelse(x <= 1,"",paste("{}rowspan",x," ",sep="")),"\\\\textbf{",con.matA[tmp,1],"}",sep="");
        tmp <- eval(parse(text=paste("paste(paste(",paste(paste("con.matA[2:dim(con.matA)[1],",1:dim(con.matA)[2],"]",sep=""),collapse=","),",sep=\"&\"),collapse=\"\\\\\\\\\\\\\\\\\\\\\\\\hline\n\")",sep="")));
        tmp1 <- con.matA[1,];
        tmp1 <- tmp1[-1];
        tmp <- paste("\\\\begin{longtable}{||r|r||",paste(rep("c|",times=dim(con.matA)[2]-2),collapse=""),"|}\n",
                     "\\\\caption{Parameters stratified by ",
                     par.rename.f(cat)," with a range of values for all observations\\\\label{tab:cov.base.by.",cat,".",
                     gsub("[ _\\\\\\/\\-]",".",object@Runno),"}}\\\\\\\\%spancol=",length(con.matA[1,]),"\n",
                     "\\\\hline\\\\hline\n",
                     "%.GREYBG.\n&",
                     "\\\\textbf{",paste(tmp1,collapse="}&\\\\textbf{"),
                     "}.ENDHEAD.\\\\\\\\\\\\hline\\\\hline\n",
                     tmp,
                     "\\\\\\\\\\\\hline\\\\hline\n\\\\end{longtable}\n",
                     sep=""
                     );
        ret <- c(ret,tmp);
        ret <- paste(ret,collapse="\n\n");
      }
    }
  }
  ret <- gsub("\\\\cdot\\b","\\\\\\\\cdot",ret,perl=TRUE);
  ret <- gsub("e(-?[0-9]+)","$\\\\\\\\cdot$$10^{\\1}$",ret);
  ret <- gsub("\\{(-?)0+","{\\1",ret);
  ret <- gsub("([^{])-","\\1$-$",ret);
  return(ret)
}
save.sum <- function(xpdb,base.file=NULL,...){
  if (is.null(base.file)){
    if (regexpr(" [0-9]+$",xpdb@Runno) != -1){
      base.file <- gsub("^ ","run",xpdb@Runno);
    } else {
      base.file <- gsub("^ ","",xpdb@Runno);
    }
  }
  pr <-  getSum(xpdb,base.file=base.file,...);
  if (!exists("smy")){
    smy <<- list();
  }
  smy[[xpdb@Runno]] <<- pr;
}

create.latex <- function(xpdb,title=NULL,author=NULL,date=NULL,file=paste("sum",xpdb@Runno,sep=""),toc=TRUE,...){
  tex <- nm.latex(xpdb,base.file=file,...)
  tex <- paste("\\\\begin{landscape}",tex,"\\\\end{landscape}",sep="\n");
  tex <- gsub("\\\\","\\",tex,fixed=TRUE);
  contents <- "\n\\tableofcontents\n";
  ret <- paste("\\documentclass[12pt]{article}
\\title{",title,"}
\\usepackage{amsmath}
\\usepackage{hyperref}
\\author{",author,"}
\\begin{document}
  \\maketitle",contents,tex,"
\\end{document}
",sep="");
  rm(tex);
  write(ret,paste(file,".ptex",sep=""));
                                        #  gen.rtf(tex,title=title,author=author,date=date,file=file);
}

## Begin TORTF
gen.rtf <- function(file="tab",use.macro=FALSE,...){
  con <- file(paste(file,".ptex",sep=""), "r");
  ret <- readLines(con) # empty
  close(con);
  ret <- munge.tex(ret);
  latex2rtf <- "latex2rtf";
  if (Sys.getenv("ProgramFiles") != ""){
    ## Search for the path in the following locations.
    ## First check to see if LaTeX2rtf is installed in Program Files.
    if (file.exists("../USERSCRIPTS/lib/latex2rtf/latex2rt.exe")){
      latex2rtf <-  shortPathName("../USERSCRIPTS/lib/latex2rtf/latex2rt.exe");
    } else {
      Sys.setenv(rtfpath=shortPathName(paste(Sys.getenv("ProgramFiles"),"\\latex2rtf\\cfg")));
      latex2rtf <- shortPathName(paste(Sys.getenv("ProgramFiles"),"\\latex2rtf\\latex2rt.exe",sep=""));
      if (!file.exists(latex2rtf)){
        stop("This requires Latex2rtf (GUI) to be installed in the default location.");
      }
    }
  }


  tex <- paste(file,".tex",sep="");
  write(ret,tex);
  rtf.name <- paste(file,".rtf",sep="");
  cat("Waiting 5 seconds for networks drives to catch up\n");
  Sys.sleep(5);
  system(paste(latex2rtf,tex));
                                        #  unlink(tex);
  con <- file(rtf.name, "r");
  rtf <- readLines(con) # empty
  close(con);
  rtf <- munge.rtf(rtf,use.macro=use.macro,...);
  write(rtf,rtf.name);

  if (Sys.getenv("ProgramFiles") != ""){
    if (file.exists("savepassword.vbs")){
      if (file.exists(gsub("rtf","doc",rtf.name))){
        unlink(gsub("rtf","doc",rtf.name));
      }
      if (file.exists(gsub(".rtf","-ro.doc",rtf.name))){
        unlink(gsub(".rtf","-ro.doc",rtf.name));
      }
      system(paste("wscript savepassword.vbs",round(runif(1)*10000),rtf.name))
    }
  }
  return(cat(""));
}


read.nm.tables.esn <-
  function (table.files = NULL, runno = NULL, tab.suffix = "",
            table.names = c("sdtab", "mutab", "patab", "catab", "cotab",
              "mytab", "extra", "xptab"), cwres.name = c("cwtab"),
            cwres.suffix = "", quiet = FALSE, ...)
{
  table.names <- names(table.files);
  if (is.null(table.files)) {
    if (is.null(runno)) {
      cat(paste("runno must be specified if no table files provided\n"))
      return(NULL)
    }
    match.pos <- match(cwres.name, table.names)
    if (!is.na(match.pos))
      table.names <- table.names[-match.pos]
    tab.files <- sapply(table.names, paste, runno, tab.suffix,
                        sep = "")
    cwres.files <- sapply(cwres.name, paste, runno, cwres.suffix,
                          tab.suffix, sep = "")
    tab.files <- c(tab.files, cwres.files)
  }
  else {
    tab.files <- table.files
  }
  totab <- NULL
  totnam <- NULL
  seen.files <- NULL
  filedim <- NULL
  for (i in 1:length(tab.files)) {
    filename <- tab.files[i]
    if (!is.readable.file(filename)) {
      next;
    }
    else {
      cat(paste("    Reading", filename, "\n"))
      fields.per.line <- count.fields(filename)
      fields.in.first.line <- fields.per.line[1]
      fields.in.rest <- fields.per.line[-1]
      if ((length(unique(fields.in.rest)) != 1) || (all(fields.in.first.line ==
                   fields.in.rest))) {
        if (!quiet) {
          cat(paste("Found different number of fields in ",
                    filename, ".\n", sep = ""))
          cat("This may be due to multiple TABLE and header rows \n")
          cat("caused by running multiple simulations in NONMEM (NSIM > 1).\n")
          cat("Will try to remove these rows. It may take a while...\n")
        }
        tmp <- readLines(filename, n = -1)
        inds <- grep("TABLE", tmp)
        if (length(inds) != 1) {
          inds <- inds[c(2:length(inds))]
          inds2 <- inds + 1
          tempfile <- paste(filename, ".xptmp", sep = "")
          write.table(tmp[-c(inds, inds2)], file = tempfile,
                      row.names = FALSE, quote = FALSE)
          assign(paste("n.", filename, sep = ""), read.table(tempfile,
                                                             skip = 2, h = T))
          unlink(tempfile)
        }
        else {
          assign(paste("n.", filename, sep = ""), read.table(filename,
                                                             skip = 1, h = T))
        }
      }
      else {
        assign(paste("n.", filename, sep = ""), read.table(filename,
                                                           skip = 1, h = T))
      }
      seen.files <- c(seen.files, paste("n.", filename,
                                        sep = ""))
    }
  }
  if (any(is.null(seen.files))) {
    cat("Couldn't find any table files that match run number",
        runno, "!\n")
    return(NULL)
  }
  for (nfile in seen.files) {
    if (is.null(filedim)) {
      filedim <- nrow(get(nfile))
    }
    else {
      filedim <- c(filedim, nrow(get(nfile)))
    }
  }
  file.df <- data.frame(seen.files = seen.files, filedim = filedim)
  lngths <- sort(unique(file.df$filedim))
  if (length(lngths) != 1) {
    cat("\nThe table files associated with this run number (",
        runno, ") appear\n")
    cat("to have different lengths.\n")
    cat("You will have to sort this out and try again!\n")
    return(NULL)
  }
  maxlngth <- file.df$filedim;
  maxlngth <- max(maxlngth);
  for (ii in 1:nrow(file.df)) {
    filnam <- as.character(file.df[ii, "seen.files"])
    new.df <- get(filnam)
    sz <- file.df[ii, "filedim"]
    rl <- maxlngth/sz
    if (any(is.null(totab))) {
      totab <- new.df
    }
    else {
      totab <- cbind(totab, new.df)
    }
    totnam <- c(totnam, names(new.df))
    if (!is.na(pmatch(paste("n.",table.names[3],sep=""), filnam))) {
      write(names(new.df), file = ".patab.names.tmp")
    }
    else {
      if (!is.na(pmatch(paste("n.",table.names[4],sep=""), filnam))) {
        write(names(new.df), file = ".catab.names.tmp")
      }
      else {
        if (!is.na(pmatch(paste("n.",table.names[5],sep=""), filnam))) {
          write(names(new.df), file = ".cotab.names.tmp")
        }
        else {
          if (!is.na(pmatch(paste("n.",table.names[1],sep=""), filnam))) {
            write(names(new.df), file = ".sdtab.names.tmp")
          }
        }
      }
    }
  }
  totab <- totab[, !duplicated(totnam)]
  return(totab)
}


## Make vs functions.


check.vars2 <- function(vars, object, silent = FALSE)
{
  for (v in vars) {
    if (is.null(xvardef2(v, object)) &
        !any(names(object@Data) == substring(toupper(v),0,min(4,nchar(v))))
        ) {
      if (!silent) {
        cat(paste("\n", "-----------Variable(s) not defined!-------------\n",
                  v, "is/are not defined in the current database\n",
                  "and must be defined for this command to work!\n",
                  "------------------------------------------------\n"))
      }
      return(NULL)
    }
  }
  return(TRUE)
}

xvardef2 <-  function(x,object){
  if(length(object@Prefs@Xvardef[[x]]) != 0){
    return(object@Prefs@Xvardef[[x]])
  } else if (any(names(object@Data) == toupper(x))) {
    return(toupper(x));
  } else if (any(names(object@Data) == substring(toupper(x),0,min(4,nchar(x))))){
    return(substring(toupper(x),0,min(4,nchar(x))));
  } else {
    return(NULL);
  }
}


create.function <-  function(fn,create.dummy=FALSE){
  ## X vs Y
  if (regexpr("\\.sparse$",fn) > -1) {
    tmp.fn <- gsub("\\.sparse$","",fn);
    if (eval(parse(text=paste("tryCatch(is.function(",tmp.fn,")*1,error=function(e) { return(2) })",sep=""))) == 2){
      create.function(tmp.fn);
    }
    eval(parse(text=paste(fn,"<- function(...,sparse=TRUE,type=\"p\"){tryCatch(",
                 tmp.fn,"(...,sparse=sparse,type=type),error=function(e) { print(e); return(invisible()) }) }",sep="")),envir=.GlobalEnv);
  } else {
    if (regexpr("\\.vs\\.",fn) > -1){
      tmp <-  strsplit(fn,"\\.vs\\.")[[1]];
      x <- tmp[1];
      ret <-  tmp[2];
      if (regexpr("\\.by\\.",fn) > -1) {
        tmp <-  strsplit(ret,"\\.by\\.")[[1]];
        y <-  tmp[1];
        by <-  tmp[2];
        if (regexpr("^([A-Za-z0-9]+\\.norm(alized?)?\\.)?(dv\\.pred|pred\\.dv|[^.]*)$",x) > -1) {
          if (regexpr("^([A-Za-z0-9]+\\.norm(alized?)?\\.)?([^.]*)$",y) > -1) {
            if (regexpr("^[^.]*$",by) > -1){
              eval(make.vs.by(x,y,by),envir=.GlobalEnv);
            } else {
              if (regexpr("dv\\.pred",x) > -1){
                is.log <-  FALSE;
                is.sparse <-  FALSE;
                oby <-  by;
                if (regexpr("\\.log",by) > -1){
                  is.log <-  TRUE;
                  by <-  gsub("\\.log","",by);
                }
                if (regexpr("\\.sparse",by) > -1){
                  is.sparse <-  TRUE;
                  by <-  gsub("\\.sparse","",by);
                }
                if (regexpr("^[^.]*$",by) == -1){
                  cat("Invalid by variable:",oby,"\nCan't create:",fn,"\n",file=stderr());
                } else {
                  eval(make.vs.by(x,y,by),envir=.GlobalEnv);
                  ## Now make specific function.
                  eval(parse(text=
                             paste(fn," <- function(object,...",ifelse(is.log,",logy=TRUE",",logy=NULL"),
                                   ifelse(is.sparse,",sparse=TRUE",",sparse=NULL"),"){",
                                   "\n\t",x,".vs.",y,".by.",by,"(object=object,...,logy=logy,sparse=sparse);}\n",
                                   sep="")
                             ),
                       envir=.GlobalEnv
                       )
                }
              } else {
                cat("Invalid by variable:",by,"\nCan't create:",fn,"\n",file=stderr());
                eval(parse(text=
                           paste(fn," <- function(...) { cat(\"Dummy function, invalid by variable: ",by,"\n\"); return(invisible());}",sep="")
                           ),
                     envir=.GlobalEnv
                     )

              }
            }
          } else {
            cat("Invalid y variable:",y,"\nCan't create:",fn,"\n",file=stderr());
            eval(parse(text=
                       paste(fn," <- function(...) { cat(\"Dummy function, invalid y variable: ",y,"\n\"); return(invisible());}",sep="")
                       ),
                 envir=.GlobalEnv
                 )
          }
        } else {
          cat("Invalid x variable:",x,"\nCan't create:",fn,"\n",file=stderr());
          eval(parse(text=
                     paste(fn," <- function(...) { cat(\"Dummy function, invalid x variable ",x,"\n\"); return(invisible());}",sep="")
                     ),
               envir=.GlobalEnv
               )
        }
      } else {
        if (regexpr("\\.bw$",ret) > -1){
                                        # X vs Y (box & whiskers)
          y <-  gsub("\\.bw$","",ret);
          if (regexpr("^([A-Za-z0-9]+\\.norm(alized?)?\\.)?([^.]*)$",x) > -1) {
            if (regexpr("^([A-Za-z0-9]+\\.norm(alized?)?\\.)?([^.]*)$",y) > -1) {
              eval(make.vs.bw(x,y),envir=.GlobalEnv);
            } else {
              cat("Invalid y variable:",y,"\nCan't create:",fn,"\n",file=stderr());
              eval(parse(text=
                         paste(fn," <- function(...) { cat(\"Dummy function, invalid y variable: ",y,"\n\"); return(invisible());}",sep="")
                         ),
                   envir=.GlobalEnv
                   )
            }
          } else {
            cat("Invalid x variable:",x,"\nCan't create:",fn,"\n",file=stderr());
            eval(parse(text=
                       paste(fn," <- function(...) { cat(\"Dummy function, invalid x variable: ",x,"\n\"); return(invisible());}",sep="")
                       ),
                 envir=.GlobalEnv
                 )
          }
        } else {
          ## X vs Y
          y <-  ret;
          if (regexpr("^(log\\.)?([A-Za-z0-9]+\\.norm(alized?)?\\.)?(dv\\.pred|pred\\.dv|[^.]*)$",x) > -1) {
            if (regexpr("^(log\\.)?([A-Za-z0-9]+\\.norm(alized?)?\\.)?([^.]*)$",y) > -1) {
              eval(make.vs(x,y),envir=.GlobalEnv);
            } else {
              cat("Invalid y variable:",y,"\nCan't create:",fn,"\n",file=stderr());
              eval(parse(text=
                         paste(fn," <- function(...) { cat(\"Dummy function, invalid y variable: ",y,"\n\"); return(invisible());}",sep="")
                         ),
                   envir=.GlobalEnv
                   )
            }
          } else {
            cat("Invalid x variable:",x,"\nCan't create:",fn,"\n",file=stderr());
            eval(parse(text=
                       paste(fn," <- function(...) { cat(\"Dummy function, invalid x variable ",x,"\n\"); return(invisible());}",sep="")
                       ),
                 envir=.GlobalEnv
                 )
          }
        }
      }
    }
  }
  if (create.dummy){
    if (eval(parse(text=paste("tryCatch(is.function(",fn,")*1,error=function(e) { return(2) })",sep=""))) == 2){
      cat("Creating dummy ",fn,"\n",file=stderr());
      eval(parse(text=paste(fn," <- function(...){ return(NULL);}",sep="")),envir=.GlobalEnv);
    }
  }
}
get.functions <-  function(xpdb=NULL,pr=NULL,pr.ref=NULL,orig=list(),...){
  if (is.null(pr) & !is.null(xpdb)){
    if (!any(names(smy) == xpdb@Runno)){
      pr <-  getSum(xpdb,...);
      smy[[xpdb@Runno]] <<- pr;
    } else {
      pr <- smy[[xpdb@Runno]]
    }
    com <- gsub(";[;Cc|]*","",pr$mod[which(regexpr(";.*",pr$mod) > -1)]);
    graph.ids <-  which(regexpr("[A-Za-z0-9.]*\\.vs\\.[A-Za-z0-9.]*",com) > -1)
    label.ids <-  which(regexpr(":",com)> -1)
    graph.lst <-  orig;
    for (i in graph.ids){
      while (regexpr("[A-Za-z0-9.]*\\.vs\\.[A-Za-z0-9.]*",com[i]) > -1){
        tmp <-  gsub("^(.*?[^.A-Za-z0-9]+)([A-Za-z0-9.]*\\.vs\\.[A-Za-z0-9.]*)(.*)$","\\2",com[i],perl=TRUE);
        com[i] <- gsub("^(.*?[^.A-Za-z0-9]+)([A-Za-z0-9.]*\\.vs\\.[A-Za-z0-9.]*)(.*)$","\\1\\3",com[i],perl=TRUE);
        tmp0 <- label.ids[label.ids <= i];
        tmp1 <-  i-tmp0;
        label.id <- tmp0[which(min(tmp1) == tmp1)];
        label <-  gsub("^[ \t]*(.*?)[ \t]*:.*$","\\1",com[label.id]);
        if (eval(parse(text=paste("tryCatch(is.function(",tmp,")*1,error=function(e) { return(2) })",sep=""))) == 2){
          cat("Generating function: ",tmp,"\n",file=stderr());
          create.function(tmp);
        }
        if (is.null(graph.lst[[label]])) {
          graph.lst[[label]] <-  c(tmp);
        } else {
          graph.lst[[label]] <-  c(tmp,graph.lst[[label]]);
        }
      }
    }
    ## Create Functions
    return(graph.lst);
  } else {
    cat("Requires xpdb or pr to be non-null\n",file=stderr())

  }
}

make.vs <- function(x,y) {
  ox <-  x;
  oy <-  y;
  logx <- FALSE;
  logy <- FALSE;
  if (regexpr("^log\\.",x) > 0){
    logy <-  TRUE;
    x <-  gsub("^log\\.","",x);
  }
  if (regexpr("^log\\.",y) > 0){
    logx <-  TRUE;
    y <-  gsub("^log\\.","",y);
  }
  norm.x <- "";
  dv.pred.lab <-  "Observations & Population Predictions";
  if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",x) > -1){
    norm.x <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",x);
    x <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",x);
    dv.pred.lab <- "Norm. Observations & Pop. Predictions";
    if (x == "dv.pred" || x == "pred.dv"){
      norm.x <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.x,"\",object)]));
object@Data[,xvardef2(\"dv\",object)] <-  object@Data[,xvardef2(\"dv\",object)] / nrm;
object@Data[,xvardef2(\"pred\",object)] <-  object@Data[,xvardef2(\"pred\",object)] / nrm;
object@Prefs@Labels$DV <-  paste(object@Prefs@Labels$DV,\"/\",xlabel(xvardef2(\"",norm.x,"\",object),object));
",sep="");
    } else {
      norm.x <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.x,"\",object)]));
object@Data[,xvardef2(\"",x,"\",object)] <-  object@Data[,xvardef2(\"",x,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",x,"\",object)]] <-  paste(xlabel(xvardef2(\"",x,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.x,"\",object),object));
",sep="");
    }
  }
  oy <-  y;
  norm.y <-  "";
  if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",y) > -1){
    norm.y <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",y);
    y <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",y);
    norm.y <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.y,"\",object)]));
object@Data[,xvardef2(\"",y,"\",object)] <-  object@Data[,xvardef2(\"",y,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",y,"\",object)]] <-  paste(xlabel(xvardef2(\"",y,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.y,"\",object),object));
",sep="");
  }
  if (y != "cov"){
    if (x != "dv.pred" && x != "pred.dv") {
      return(parse(text=paste(ox,".vs.",oy," <-
function (object, abline = c(0, 0), smooth = TRUE, ...,logy=",logy,",logx=",logx,")
{
  if (is.null(check.vars2(c(\"",x,"\", \"",y,"\"), object, silent = FALSE))) {
    return(NULL);
  }
  ",norm.x,norm.y,"
  xplot <- xpose.plot.default(xvardef2(\"",y,"\", object), xvardef2(\"",x,"\",
                                                                    object), smooth = smooth, abline = abline, object, logx=logx, logy=logy,...)
  return(xplot)
}",sep="")));
       } else {
          return(parse(text=paste(ox,".vs.",oy,"<- function(object, smooth = NULL, sparse=NULL,...,logy=",logy,",logx=",logx," ){
            if (is.null(check.vars2(c(\"",y,"\",\"dv\",\"id\",\"pred\"), object))) {
              return(NULL)
            }
            ",norm.x,norm.y,"
            xplot <- xpose.plot.default(xvardef2(\"",y,"\", object), xvardef2(\"dv\", object),
                                        object,
                                        smooth=smooth,
                                        panel=function(x,y,object,subscripts,...,onlyfirst,inclZeroWRES,samp,logy,logx,sparse){
                                          xpose.panel.default(x,y,object,subscripts=subscripts,...);
                                          if (!is.null(samp)) {
                                            dat <- SData(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                         subset = xsubset(object), samp = samp)
                                            dat <- dat[subscripts,];
                                          }
                                          else {
                                            dat <- Data(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                        subset = xsubset(object))
                                            dat <- dat[subscripts,];
                                          }
                                          if (is.null(sparse)){
                                            for (id in unique(dat[,xvardef2(\"id\",object)])){
                                              xx <- dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"",y,"\",object)];
                                              yy <- dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"pred\",object)];
                                              if (logy){
                                                yy <- log10(yy);
                                              }
                                              if (logx){
                                                xx <-  log10(xx);
                                              }
                                              ord <- order(xx);
                                              panel.lines(xx[ord],yy[ord],col=\"red\");
                                            }
                                          } else {
                                            xx <- dat[,xvardef2(\"",y,"\",object)];
                                            yy <-  dat[,xvardef2(\"pred\",object)];
                                            if (logy){
                                              yy <- log10(yy)
                                            }
                                            if (logx){
                                              xx <-  log10(xx);
                                            }
                                            ord <- order(xx);
                                            panel.lines(xx[ord],yy[ord],col=\"red\");
                                          }
                                        },
                                        sparse=sparse, logx=logx,logy=logy,
                                        ...)
            return(xplot)
          }",sep="")));
       }
  } else {
return(parse(text=paste(ox,".vs.cov <-
  function (object, ylb = \"",x,"\", smooth = TRUE, type = \"p\", main = \"Default\",
            ...,,logy=",logy,",logx=",logx,")
{
  if (is.null(check.vars2(c(\"covariates\", \"",tolower(x),"\"), object, silent = FALSE))) {
    return()
  }
  ",norm.x,norm.y,"
  number.of.plots <- 0
  for (i in xvardef2(\"covariates\", object)) {
    number.of.plots <- number.of.plots + 1
  }
  plotList <- vector(\"list\", number.of.plots)
  plot.num <- 0
  for (j in xvardef2(\"covariates\", object)) {
    xplot <- xpose.plot.default(j, xvardef2(\"",x,"\", object),
                                object, main = NULL, ylb = ylb, smooth = smooth,
                                type = type, pass.plot.list = TRUE, ...,logx=logx,logy=logy)
    plot.num <- plot.num + 1
    plotList[[plot.num]] <- xplot
  }
  default.plot.title <- paste(xlabel(xvardef2(\"",x,"\", object),
                                     object), \" vs \", \"Covariates\", sep = \"\")
  plotTitle <- xpose.multiple.plot.title(object = object, plot.text = default.plot.title,
                                         main = main, ...)
  xpose.multiple.plot.default(plotList, plotTitle = plotTitle,
                              ...)
  invisible()
}",sep="")));
}
}
make.vs.bw <- function(x,y) {
    ox <-  x;
    norm.x <- "";
    if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",x) > -1){
        norm.x <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",x);
        x <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",x);
        norm.x <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.x,"\",object)]));
object@Data[,xvardef2(\"",x,"\",object)] <-  object@Data[,xvardef2(\"",x,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",x,"\",object)]] <-  paste(xlabel(xvardef2(\"",x,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.x,"\",object),object));
",sep="");
    }
    oy <-  y;
    norm.y <-  "";
    if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",y) > -1){
        norm.y <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",y);
        y <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",y);
        norm.y <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.y,"\",object)]));
object@Data[,xvardef2(\"",y,"\",object)] <-  object@Data[,xvardef2(\"",y,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",y,"\",object)]] <-  paste(xlabel(xvardef2(\"",y,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.y,"\",object),object));
",sep="");
    }
  return(parse(text=paste(ox,".vs.",oy,".bw <-
  function (object, abline = c(0, 0), smooth = TRUE, ...)
{
  if (is.null(check.vars2(c(\"",x,"\", \"",y,"\"), object, silent = FALSE))) {
    return(NULL)
  }
  ",norm.x,norm.y,"
  xplot <- xpose.plot.bw(xvardef2(\"",x,"\", object), xvardef2(\"",y,"\",
                                                               object), object, binvar= xvardef2(\"",y,"\",object), ...)
  return(xplot)
}",sep="")));
}
make.vs.by  <- function(x,y,z) {
ox <-  x;
norm.x <- "";
dv.pred.lab <-  "Observations & Population Predictions";
if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",x) > -1){
norm.x <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",x);
x <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",x);
dv.pred.lab <- "Norm. Observations & Pop. Predictions";
if (x == "dv.pred" || x == "pred.dv"){
norm.x <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.x,"\",object)]));
object@Data[,xvardef2(\"dv\",object)] <-  object@Data[,xvardef2(\"dv\",object)] / nrm;
object@Data[,xvardef2(\"pred\",object)] <-  object@Data[,xvardef2(\"pred\",object)] / nrm;
object@Prefs@Labels$DV <-  paste(object@Prefs@Labels$DV,\"/\",xlabel(xvardef2(\"",norm.x,"\",object),object));
",sep="");
} else {
norm.x <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.x,"\",object)]));
object@Data[,xvardef2(\"",x,"\",object)] <-  object@Data[,xvardef2(\"",x,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",x,"\",object)]] <-  paste(xlabel(xvardef2(\"",x,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.x,"\",object),object));
",sep="");
}
}
oy <-  y;
norm.y <-  "";
if (regexpr("^[A-Za-z0-9]+.norm(alized?)?.",y) > -1){
norm.y <- gsub("^([A-Za-z0-9]+).norm(alized?)?.*","\\1",y);
y <-  gsub("^([A-Za-z0-9]+).norm(alized?)?.(.*)","\\3",y);
norm.y <- paste("
nrm <- as.numeric(paste(object@Data[,xvardef2(\"",norm.y,"\",object)]));
object@Data[,xvardef2(\"",y,"\",object)] <-  object@Data[,xvardef2(\"",y,"\",object)] / nrm;
object@Prefs@Labels[[xvardef2(\"",y,"\",object)]] <-  paste(xlabel(xvardef2(\"",y,"\",object),object),\"/\",xlabel(xvardef2(\"",norm.y,"\",object),object));
",sep="");
}
if (z != "cov"){
if (x == "dv.pred" || x == "pred.dv") {
 return(parse(text=paste(ox,".vs.",oy,".by.",z," <- function(object, smooth = NULL,logy=FALSE,sparse=NULL,...){
   if (is.null(check.vars2(c(\"",y,"\", \"dv\",\"id\",\"pred\",\"",z,"\"), object))) {
     return(NULL)
   }",norm.x,norm.y,"
   xplot <- xpose.plot.default(xvardef2(\"",y,"\", object), xvardef2(\"dv\", object),
                               object,
                               smooth=smooth,
                               panel=function(x,y,object,subscripts,...,onlyfirst,inclZeroWRES,samp,logy,logx,sparse){
                                 xpose.panel.default(x,y,object,subscripts=subscripts,...);
                                 if (!is.null(samp)) {
                                   dat <- SData(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                subset = NULL, samp = samp)
                                   dat <- dat[subscripts,];
                                 }
                                 else {
                                   dat <- Data(object, inclZeroWRES, onlyfirst = onlyfirst,
                                               subset = NULL)
                                   dat <- dat[subscripts,];
                                 }
                                 if (!is.null(sparse)){
                                   xx <-  dat[,xvardef2(\"",y,"\",object)];
                                   yy <-  dat[,xvardef2(\"pred\",object)];
                                   if (logy) {
                                     yy <-  log10(yy);
                                   }
                                   if (logx) {
                                     xx <-  log10(xx);
                                   }
                                   ord <- order(xx);
                                   panel.lines(xx[ord],yy[ord],col=\"red\");
                                 } else {
                                   for (id in unique(dat[,xvardef2(\"id\",object)])){
                                     xx <- dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"",y,"\",object)];
                                     yy <-  dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"pred\",object)];
                                     if (logy){
                                       yy <- log10(yy);
                                     }
                                     if (logx) {
                                       xx <-  log10(xx);
                                     }
                                     ord <- order(xx);
                                     panel.lines(xx[ord],yy[ord],col=\"red\");
                                   }
                                 }
                               },
                               by=xvardef2(\"",z,"\",object),
                               logy=logy,
                               sparse=sparse,
                               ...)
   return(xplot)
 }",sep="")));
} else {
  return(parse(text=paste(ox,".vs.",oy,".by.",z," <-
  function (object, abline = c(0, 0), smooth = TRUE, ...)
{
  if (is.null(check.vars2(c(\"",x,"\", \"",y,"\",\"",z,"\"), object, silent = FALSE))) {
    return(NULL)
  }
  ",norm.x,norm.y,"
  xplot <- xpose.plot.default(xvardef2(\"",y,"\", object), xvardef2(\"",x,"\",
                                                                    object), object, smooth = smooth, abline = abline, by=xvardef2(\"",z,"\",object), ...)
  return(xplot)
}",sep="")));
}
} else {
if (x == "dv.pred" || x == "pred.dv") {
return(parse(text=paste(
ox,".vs.",oy,".by.cov <- function (object, abline = NULL, smooth = NULL, main = \"Default\",logy=FALSE,
                                   max.plots.per.page = 1, sparse = NULL,
                                   ...)
{
  if (is.null(check.vars2(c(\"",y,"\", \"dv\",\"id\",\"pred\",\"covariates\"), object))) {
    return(NULL)
  }
  ",norm.x,norm.y,"
  number.of.plots <- 0
  for (i in xvardef2(\"covariates\", object)) {
    number.of.plots <- number.of.plots + 1
  }
  plotList <- vector(\"list\", number.of.plots)
  plot.num <- 0
  for (i in xvardef2(\"covariates\", object)) {
    xplot <- xpose.plot.default(xvardef2(\"",y,"\", object), xvardef2(\"dv\", object),
                                object,
                                smooth=smooth,
                                panel=function(x,y,object,subscripts,...,onlyfirst,inclZeroWRES,samp,logy,logx,sparse){
                                  xpose.panel.default(x,y,object,subscripts=subscripts,...,onlyfirst=onlyfirst,inclZeroWRES=inclZeroWRES,samp=samp,logy=logy);
                                  if (!is.null(samp)) {
                                    dat <- SData(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                 subset = xsubset(object), samp = samp)
                                    dat <- dat[subscripts,];
                                  }
                                  else {
                                    dat <- Data(object, inclZeroWRES, onlyfirst = onlyfirst,
                                                subset = xsubset(object))
                                    dat <- dat[subscripts,];
                                  }
                                  if (is.null(sparse)){
                                    for (id in unique(dat[,xvardef2(\"id\",object)])){
                                      xx <- dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"",y,"\",object)];
                                      yy <- dat[dat[,xvardef2(\"id\",object)] == id,xvardef2(\"pred\",object)];
                                      if (logy){
                                        yy <- log10(yy);
                                      }
                                      if (logx){
                                        xx <-  log10(xx);
                                      }
                                      ord <- order(xx);
                                      panel.lines(xx[ord],yy[ord],col=\"red\");
                                    }
                                  } else {
                                    xx <- dat[,xvardef2(\"",y,"\",object)];
                                    yy <- dat[,xvardef2(\"pred\",object)];
                                    if (logy){
                                      yy <- log10(yy);
                                    }
                                    if (logx){
                                      xx <-  log10(xx);
                                    }
                                    ord <- order(xx);
                                    panel.lines(xx[ord],yy[ord],col=\"red\");
                                  }
                                },
                                by=xvardef2(i,object),
                                logy=logy,
                                pass.plot.list=TRUE,
                                abline = abline,
                                main = NULL,
                                sparse = sparse,
                                ...)

    plot.num <- plot.num + 1
    plotList[[plot.num]] <- xplot
  }
  default.plot.title <- paste(\"",dv.pred.lab," vs \", xlabel(xvardef2(\"",y,"\", object), object),
                              sep = \"\")
  plotTitle <- xpose.multiple.plot.title(object = object, plot.text = default.plot.title,
                                         main = main, ...)
  xpose.multiple.plot.default(plotList, plotTitle = plotTitle,max.plots.per.page = max.plots.per.page,
                              ...)
  invisible()
}
",sep="")));
} else {
return(parse(text=paste(
ox,".vs.",oy,".by.cov <- function (object, abline = NULL, smooth = NULL, main = \"Default\",
                                   max.plots.per.page = 1,
                                   ...)
{

  if (is.null(check.vars2(c(\"",x,"\", \"",y,"\", \"covariates\"), object))) {
    return(NULL)
  }
  ",norm.x,norm.y,"
  number.of.plots <- 0
  for (i in xvardef2(\"covariates\", object)) {
    number.of.plots <- number.of.plots + 1
  }
  plotList <- vector(\"list\", number.of.plots)
  plot.num <- 0
  for (i in xvardef2(\"covariates\", object)) {
    xplot <- xpose.plot.default(xvardef2(\"",y,"\", object),
                                xvardef2(\"",x,"\", object), abline = abline, object, main = NULL,
                                by = i, smooth = smooth, pass.plot.list = TRUE,
                                ...)
    plot.num <- plot.num + 1
    plotList[[plot.num]] <- xplot
  }
  default.plot.title <- paste(xlabel(xvardef2(\"",x,"\", object),
                                     object), \" vs \", xlabel(xvardef2(\"",y,"\", object), object),
                              sep = \"\")
  plotTitle <- xpose.multiple.plot.title(object = object, plot.text = default.plot.title,
                                         main = main, ...)
  xpose.multiple.plot.default(plotList, plotTitle = plotTitle,
                              max.plots.per.page = max.plots.per.page,
                              ...)
  invisible()
}
",sep="")));
}}}

######################################################################
### esn-xpose-summary.R ends here
