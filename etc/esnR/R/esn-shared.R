### esn-shared.R --- Shared Esn routines
##
## Filename: esn-shared.R
## Description:
## Author: Matthew L. Fidler
## Maintainer: Matthew L. Fidler
## Created: Wed Feb  3 11:23:28 2010 (-0600)
## Version: 0.1
##     Update #: 273
## URL:
## Keywords:
## Compatibility: R 2.10.0
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
## 28-Jun-2010    Matthew L. Fidler
##    If correlation table not present, don't add correlation table to overivew document.
## 28-Jun-2010    Matthew L. Fidler
##    Added code to take out fortran's line in matrix output.
## 15-Apr-2010    Matthew L. Fidler
##    Fixed THETA parsing when values are fixed.
## 03-Feb-2010    Matthew L. Fidler
##    Split up into columns to preserve memory.
## 03-Feb-2010    Matthew L. Fidler
##    Started taking out perl=TRUE because it eats memory...
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


latex.table <- function(tab,caption=NULL,label=NULL,double=TRUE){ #
  nme <-  names(tab);
  tmp.f <-  function(x){
    return(paste(x,collapse="&"));
  }
  ret <- paste(paste(sapply(data.frame(t(tab)),tmp.f),collapse="\\\\\\\\\n"),"\\\\\\\\\n\\\\hline\n\\\\end{longtable}\n\n",sep="");
  ## Bold headings not already in bold.
  tmp.f <-  function(x){
    if (regexpr("textbf",x) > -1){
      return(x);
    } else {
      return(paste("\\\\textbf{",x,"}",sep=""));
    }
  }
  hd <-  unlist(lapply(nme,tmp.f));

  hd <-  c(paste(hd[-length(hd)],"&",sep=""),hd[length(hd)])
  multi <- regexpr("multicolumn.[0-9]+.",hd) > -1;
  if (any(multi)){
    nums <-  as.numeric(gsub("^.*multicolumn.([0-9]+).*$","\\1",hd[which(multi)]))-1;
    multi <-  which(multi);
    nums <- data.frame(x=multi+1,y=multi+nums);
    for (i in 1:length(nums[,1])){
      hd[seq(nums[i,1],nums[i,2])] <- gsub("[ ~]*&$","",hd[seq(nums[i,1],nums[i,2])]);
      ## If extends to the end of the row, delete the & on the multicolumn
      ## argument.
      if (nums[i,2] == length(hd)){
        hd[nums[i,1]-1] <-  gsub("[ ~]*&$","",hd[nums[i,1]-1]);
      }
    }

  }
  hd <- paste(hd,collapse="");
  if (regexpr(".ENDHEAD.",ret) == -1){
    hd <- paste(hd,".ENDHEAD.",sep="");
  }
  hd <-  paste(hd,"\\\\\\\\\n\\\\hline\\\\hline\n",sep="");
  ret <-  paste(hd,ret,sep="");
  if (!is.null(caption)){
    hd <-  paste("\\\\multicolumn{",length(nme),"}{l}{.CAPTION.",sep="");
    if (!is.null(label)){
      if (regexpr("fig:",label) > -1){
        hd <-  paste(hd,"Figure ",gsub("[^A-Z0-9_]","_",label),": ",sep="");
      } else {
        hd <-  paste(hd,"Table tab:",label,": ",sep="");
      }
    }
    hd <- paste(hd,caption,"}\\\\\\\\%spancol=",length(nme),"\n\\\\hline\\\\hline\n",sep="");
  }
  ret <- paste(hd,ret,sep="");
  ret <-  paste("\\\\begin{longtable}{",paste(rep("c",length=length(nme)),collapse=""),"}\n",ret,sep="");
  return(ret);
}

## Only works on models with one problem statement.
getSum <- function(xpdb=NULL,ci=0.95,base.file=ifelse(is.null(xpdb),"",paste("run",xpdb@Runno,sep="")),lst.ext=".lst",lst.file=NULL,ctl.file=NULL,...){
  if (!any(regexpr("smy",ls(envir=globalenv())) >= 0)) {
    smy <<- list();
  }
  if (!is.null(lst.file)) {
    ret <- getLst(xpdb,lst.name=lst.file,lst.ext=".txt",...);
  } else {
    if (regexpr("run ",base.file) != -1){
      base.file <- gsub("run ","",base.file);
    }
    lst.files <- Sys.glob(paste(base.file,"*.lst",sep=""));
    lst.files <- lst.files[regexpr("~[0-9]+\\.",lst.files) == -1];
    if (length(lst.files) >= 1){
      ret <- getLst(xpdb,lst.name=lst.files[1],lst.ext=".lst",...);
    } else {
      lst.files <- Sys.glob(paste(base.file,"*.rpt",sep=""));
      lst.files <- lst.files[regexpr("~[0-9]+\\.",lst.files) == -1];
      if (length(lst.files) >= 1){
        ret <- getLst(xpdb,lst.name=lst.files[1],lst.ext=".rpt",...);
      } else {
        lst.files <-  Sys.glob(paste(base.file,"*",lst.ext,sep=""));
        lst.files <- lst.files[regexpr("~[0-9]+\\.",lst.files) == -1];
        if (length (lst.files) >= 1) {
          ret <-  getLst(xpdb,lst.name=lst.files[1],lst.ext=lst.ext,...);
        } else {
          ret <- getLst(xpdb,...);
        }
      }
    }
  }
  if (!is.null(ctl.file)) {
    ret <- c(getMod(xpdb,mod.name=ctl.file,mod.ext="txt",...),ret);
  } else if (!is.null(xpdb) & file.exists(paste("../TEXTFILES/CONTROL/Control.", xpdb@Runno, ".txt", sep=""))){
    ctl.file <- paste("../TEXTFILES/CONTROL/Control.", xpdb@Runno, ".txt", sep="");
    ret <- c(getMod(xpdb,mod.name=ctl.file,mod.ext="txt",...),ret);
  } else {
    mod.files <- Sys.glob(paste(base.file,"*.mod",sep=""));
    mod.files <- mod.files[regexpr("~[0-9]+\\.mod",mod.files) == -1];
    if (length(mod.files) >= 1){
      ret <- c(getMod(xpdb,mod.name=mod.files[1],mod.ext=".mod",...),ret);
    } else {
      mod.files <- Sys.glob(paste(base.file,"*.ctl",sep=""));
      mod.files <- mod.files[regexpr("~[0-9]+\\.ctl",mod.files) == -1];
      if (length(mod.files) >= 1){
        ret <- c(getMod(xpdb,mod.name=mod.files[1],mod.ext=".ctl",...),ret);
      } else {
        mod.files <-  Sys.glob(paste(base.file,"*",sep=""));
        mod.files <- mod.files[regexpr(paste("~[0-9]+",mod.ext,sep=""),mod.files) == -1];
        if (length (mod.files) >= 1) {
          ret <-  c(getMod(xpdb,mod.name=mod.files[1],mod.ext=mod.ext,...),ret);
        } else {
          ret <- c(getMod(xpdb,...),ret);
        }
      }
    }
  }
  ret$ci=ci;
  if (length(ret$theta.names) != length(ret$f.t)){
    ret$theta.names <- paste("THETA(",seq(1,length(ret$f.t)),")",sep="");
    ret$theta.units <- rep("",length(ret$f.t));
  }
  if (length(ret$eta.names) != length(diag(ret$f.o))){
    ret$eta.names <- paste("ETA(",seq(1,length(diag(ret$f.o))),")",sep="");
  }
  if (length(ret$eps.names) != length(diag(ret$f.s))){
    ret$eps.names <- paste("EPS(",seq(1,length(diag(ret$f.s))),")",sep="");
  }
  if (!is.null(ret$eps.names)){
    ret$shrinkage <- shrinkage(xpdb,ret);
  } else {
    ret$shrinkage <- rep(NA,length(ret$eta.names));
  }
  ret$theta.tab <- theta.table(xpdb,pr=ret);
  ret$eta.tab <- eta.table(xpdb,pr=ret);
  if (!is.null(ret$eps.names)){
    ret$eps.tab <- eps.table(xpdb,pr=ret);
  }
  ret$errors <- getErrors(ret);
  ret$problem <- gsub("[ \t]*$","", gsub("^[ \t]*","",gsub("\\$[Pp][Rr][Oo][A-Za-z]*","",ret$mod[which(regexpr("\\$[Pp][Rr][Oo]",ret$mod)>0)])));
  ret$sim.or.est <-  ifelse(any(regexpr("\\b$SIM",ret$mod) > -1),"SIM","EST");
  return(ret);
}


## Get Output
getLst <- function(xpdb=NULL,run="run",mod.ext=".mod",lst.ext=".lst",lst.name=ifelse(is.null(xpdb),"",paste(run,xpdb@Runno,lst.ext,sep="")),...){
                                        # Get propreties of the list file in R compatable objects.
  if (!is.null(xpdb)){
    tmp <- paste("../TEXTFILES/RAWOUTPUT/Output.",xpdb@Runno,".txt",sep="");
    if (file.exists(tmp)){
      lst.name <- tmp;
    }
  }
  cat("Output File:",lst.name,"\n");
  con <- file(lst.name, "r");
  lst <- readLines(con) # empty
  stars <- which(regexpr("^ \\*+$",lst)!=-1);
  close(con)
  fe <- getFE("^ \\*+ +FINAL PARAMETER ESTIMATE +\\*+$",lst,stars);
  se <- getFE("^ \\*+ +STANDARD ERROR OF ESTIMATE +\\*+$",lst,stars);
  cov <- getMat("^ \\*+ +COVARIANCE MATRIX OF ESTIMATE +\\*+$",lst,stars);
  cor <- getMat("^ \\*+ +CORRELATION MATRIX OF ESTIMATE +\\*+$",lst,stars);
  icov <- getMat("^ \\*+ +INVERSE COVARIANCE MATRIX OF ESTIMATE +\\*+$",lst,stars);
  eg <- getEigen(lst,stars);
  etabar <- getEtabar(lst,stars);
  if (!etabar$found){
    etabar$etabar <- rep(NA,length(diag(fe$o)));
    etabar$etabar.se <- rep(NA,length(diag(fe$o)));
    etabar$etabar.p <- rep(NA,length(diag(fe$o)));
  }
  if (all(regexpr("^ *TOT\\. NO\\. OF INDIVIDUALS:",lst) == -1)){
    n <-  NA;
  } else {
    n <- strsplit(lst[which(regexpr("^ *TOT\\. NO\\. OF INDIVIDUALS:",lst) != -1)],"[^0-9]+")[[1]];
    n <- n[n!=""];
    n <- as.integer(n);
    if (length(n) == 0) {
      n <- NA;
    }
  }
  gradStart <- which(regexpr(" GRADIENT:",lst)!=-1);
  zeros <- which(regexpr("^0",lst) != -1);
  if (length(gradStart) == 0){
    grad <-  NULL;
    parm <-  NULL;
    zeros <-  NULL;
  } else {
    zeros <- zeros[zeros > gradStart[1]];
    zeros <- zeros[1:length(gradStart)];
    tmp.f <- function(x){
      return(seq(gradStart[x],zeros[x]-1));
    }
    grad <- sapply(strsplit(strsplit(paste(lst[c(unlist(sapply(1:length(gradStart),tmp.f)))],collapse=" ")," *GRADIENT: *")[[1]]," +"),as.double);
    parm <- sapply(strsplit(strsplit(paste(lst[c(unlist(sapply(1:length(gradStart),tmp.f)))],collapse=" ")," *PARAMETER: *")[[1]]," +"),as.double);
  }
  mvof <- getMVOF(lst,stars);

  n.obs <- as.numeric(gsub("TOT\\. NO\\. OF OBS RECS: *([0-9]+) *","\\1",lst[which(regexpr("TOT\\. NO\\. OF OBS RECS:",lst) != -1)[1]]));
  n.ind <- as.numeric(gsub("TOT\\. NO\\. OF INDIVIDUALS: *([0-9]+) *","\\1",lst[which(regexpr("TOT\\. NO\\. OF INDIVIDUALS:",lst) != -1)[1]]));

  if (is.null(zeros)){
    msg <-  NULL;
  } else {
    msg.begin <- zeros[length(zeros)];
    msg.end <- which(regexpr("^(1$| *NO.)",lst) != -1);
    msg.end <- msg.end[msg.end > msg.begin];
    msg.end <- msg.end[1]-1;
    msg <- lst[msg.begin:msg.end];
    msg <- msg[regexpr("^ (ETABAR|SE|P VAL)",msg) == -1];
    msg <- msg[regexpr("^ AND THE P-VALUE IS GIVEN",msg) == -1];
    while (msg[length(msg)] == ""){
      msg <- msg[-length(msg)];
    }
  }
  if (is.null(mvof) | is.null(grad)){
    aic <-  NULL;
    sbc <-  NULL;
  } else if (is.null(n.obs)){
    aic <- round(mvof+2*length(grad[[length(grad)]]),3);
    sbc <-  NULL;
  } else {
    aic <- round(mvof+2*length(grad[[length(grad)]]),3);
    sbc <- round(mvof+sqrt(n.obs)*length(grad[[length(grad)]]),3);
  }
  return(
         list(
              lst=lst,
              n=n,
              lst.name=lst.name,
              f = fe$found,
              f.t =  fe$t,
              f.o = fe$o,
              f.s = fe$s,
              s = se$found,
              s.t = se$t,
              s.o = se$o,
              s.s = se$s,
              cov = cov$found,
              cov.val = cov$mat,
              cor = cor$found,
              cor.val = cor$mat,
              icov=icov$found,
              icov.val=icov$mat,
              eigen = eg$found,
              eigen.val = eg$v,
              eigen.cn = eg$cn,
              etabar = etabar$found,
              etabar.val = etabar$etabar,
              etabar.se = etabar$etabar.se,
              etabar.p = etabar$etabar.p,
              mvof = mvof,
              aic = aic,
              grad = grad,
              parm = parm,
              n.obs = n.obs,
              n.ind = n.ind,
              sbc = sbc,
              msg = msg
              ));
}

getFE <- function(expr,lst,stars){
  ## Get the final estimates from the inputted list file
  fpe1 <- which(regexpr(expr,lst)!=-1);
  found <- FALSE;
  fpe.t <- c();
  fpe.o <- matrix();
  fpe.s <- matrix();
  if (length(fpe1) != 0){
    found=TRUE;
    fpe2 <- stars[stars > fpe1];
    if (length(fpe2) < 2){
      fpe2 <- length(lst);
    } else {
      fpe2 <- fpe2[2];
    }
    fpe <-lst[((fpe1-1):fpe2-1)];
    sep <- which(regexpr("(MATRIX FOR|VECTOR OF)",fpe)!=-1);
    if (length(sep) == 3) {
      fpe.t <- parseTheta(fpe[sep[1]:(sep[2]-1)]);
      mat <- fpe[sep[2]:(sep[3]-1)];
      fpe.o <- parseMat(fpe[sep[2]:(sep[3]-1)]);
      fpe.s <- parseMat(fpe[sep[3]:length(fpe)]);
    } else if (length(sep) == 2) {
      fpe.t <- parseTheta(fpe[sep[1]:(sep[2]-1)]);
      fpe.o <- parseMat(fpe[sep[2]:length(fpe)]);
      fpe.s <- NULL;
    }
  }
  return(list(
              found=found,
              t=fpe.t,
              o=fpe.o,
              s=fpe.s));
}

getMat <-function(expr,lst,stars){
  mat1 <- which(regexpr(expr,lst) != -1);
  found <- FALSE;
  mat <- matrix();
  if (length(mat1) != 0){
    found <- TRUE;
    mat2 <- stars[stars > mat1];
    if (length(mat2) < 2){
      mat2 <- length(lst);
    } else {
      mat2 <- mat2[2];
    }
    tmp <-lst[(mat1-1):mat2-1];
    ## Take out repeats of headers.
    w.tmp <- which(tmp == "1");
    w.plus <- which(regexpr("^\\+",tmp) != -1);
    w.tmp2 <- c();
    for (var in w.tmp){
      if (any(w.plus > var)){
        w.tmp2 <- c(w.tmp2, (w.plus[w.plus > var])[1]);
      }
    }
    w.tmp <- w.tmp[-length(w.tmp)];
    if (length(w.tmp) > 0) {
      for (i in seq(length(w.tmp),1,by=-1)){
        tmp <- tmp[-seq(w.tmp[i],w.tmp2[i]-2)];
      }
    }
    mat <- parseMat(tmp);
  }
  return(list(
              found=found,
              mat=mat));
}

getEigen <- function(lst,stars){
  mat1 <- which(regexpr("^ \\*+ +EIGENVALUES OF.*ESTIMATE +\\*+$",lst)!=-1);
  found <- FALSE;
  eigen <- c();
  if (length(mat1) != 0) {
    found <- TRUE;
    mat2 <- stars[stars > mat1];
    if (length(mat2) < 2){
      mat2 <- length(lst);
    } else {
      mat2 <- mat2[2];
    }
    mat <- lst[(mat1-1):mat2-1];
    mat <- strsplit(paste(mat[which(regexpr("[0-9]+\\.[0-9]+E[+-][0-9]+",mat) != -1)],collapse=" ")," +")[[1]];
    mat <- as.double(mat[mat!=""]);
    mat <- sort(mat);
    cn <- mat[length(mat)]/mat[1];
  } else {
    mat=NULL;
    cn=NULL;
  }
  return(list(found=found,v=mat,cn=cn));
}

getEtabar_internal <- function(expr,lst){
  etabar.i <- which(regexpr(expr,lst)!=-1);
  etabar <- NULL;
  if (length(etabar.i) != 0){
    etabar <- strsplit(lst[etabar.i+1],"( +|[A-Z][^ -]+|P)")[[1]];
    etabar <- etabar[etabar!=""];
    etabar <- etabar[etabar!="-"];
    etabar <- as.double(etabar);
    names(etabar) <- paste("ETA",1:length(etabar),sep="");
  }
  return(etabar);
}
getEtabar <- function(lst,stars){
  mat1 <- which(regexpr("^ ETABAR IS THE ARITHMETIC MEAN.*$",lst) != -1);
  found <- FALSE;
  etabar <- NULL;
  etabar.se <- NULL;
  etabar.p <- NULL;
  if (length(mat1) != 0){
    found <- TRUE;
    mat2 <- stars[stars > mat1];
    if (length(mat2) < 1){
      mat2 <- length(lst);
    } else {
      mat2 <- mat2[1];
    }
    lst <- lst[(mat1+2):(mat2-1)];
    lst <- lst[lst!= ""];
    lst <- lst[lst!= " "];
    lst <- lst[lst != "1"];
    mat1 <- which(regexpr("^0",lst)!=-1);
    if (length(mat1) != 0){
      mat1 <- mat1[1];
      lst <- lst[seq(1,mat1-1)];
    }
    lst <- strsplit(paste(lst,collapse=""),":")[[1]];
    etabar <- getEtabar_internal("ETABAR",lst);
    etabar.se <- getEtabar_internal("SE",lst);
    etabar.p <- getEtabar_internal("P VAL\\.",lst);
  }
  return(list(
              found=found,
              etabar=etabar,
              etabar.se=etabar.se,
              etabar.p=etabar.p));
}


getMVOF <- function(lst,stars){
  mvof <- NA;
  if (any(regexpr("#OBJV:",lst) != -1)){
    fpe1 <-  which(regexpr("#OBJV:",lst) != -1);
    fpe1 <-  fpe1[length(fpe1)];
    mvof <-  gsub("[^-0-9.E]*([-0-9.E]*).*","\\1",lst[fpe1]);
    mvof <- as.numeric(mvof);
    return(mvof);
  } else {
    fpe1 <- which(regexpr("MINIMUM VALUE OF OBJECTIVE FUNCTION",lst)!=-1);
    if (length(fpe1) > 0){
      mvof <- lst[(fpe1+2):(stars[stars>fpe1])[2]];
      mvof  <- mvof[which(regexpr("^ *\\*+ *-?[0-9.]+ *\\*+ *$",mvof)!=-1)];
      mvof <- gsub("\\**","",mvof);
      mvof <- gsub(" *","",mvof);
      mvof <- as.double(mvof);
    }
  }
  return(mvof);
}

parseTheta <- function(theta,expr="TH *[0-9]+"){
                                        # This parses the THETA block, and returns the numbers associated with the THETA block.
  thnums <- which(regexpr(expr,theta)!=-1);
  thvals <- which(regexpr("[0-9]+\\.[0-9]+E[+-][0-9]+",theta)!=-1);
  theta <- gsub("[.][.][.]+","NA",theta);
  tmp.f <- function(i){
    tmp <- theta[thnums[i]];
    tmp <- gsub("TH *","",tmp);
    th1 <- strsplit(tmp," +");
    th1 <- th1[[1]][th1[[1]]!=""];
    th2 <- strsplit(gsub("[*]+","NA",theta[thvals[i]])," +");
    th2 <- th2[[1]][th2[[1]]!=""];
    tmp.g <- function(j){
      return(paste("ret[",th1[j],"] = ",th2[j],";",sep=""));
    }
    return(paste(sapply(1:length(th1),tmp.g),collapse=""));
  }
  ret <- c();
  eval(parse(text=paste(sapply(1:length(thnums),tmp.f),collapse="")));
  return(ret);
}
parseMat <- function(mat) {
                                        # Change lower triangular matrix to R matrix.
  ones <- which(regexpr("^1", mat) != -1);
  for (one in rev(ones)){
    tmp <- one+1;
    while (length(mat) > tmp && regexpr("^ E",mat[tmp]) == -1) {
      tmp <- tmp + 1;
    }
    if (length(mat) != tmp){
      tmp <- tmp -1;
    }
    mat <- mat[-seq(one,tmp)]
  }
  mat.name <- which(regexpr("(TH ?|OM|SG|ETA|EPS)[0-9]+",mat)!=-1);
                                        # Get the first items that are close together like
                                        # TH1
                                        # TH2
                                        # would grab both lines, where
                                        # TH1
                                        #
                                        # TH2
                                        # would grab just the first line.

  mat.name.diff <- which(mat.name[-1]-mat.name[-length(mat.name)]>=2)[1];
  mat.name <- mat.name[1];
  mat.name <- strsplit(gsub("TH ","TH",paste(mat[mat.name:(mat.name+mat.name.diff)],collapse=" "))," +")[[1]];
  mat.name <- mat.name[mat.name!=""];
  mat <- fixMat(mat);
  mat <- mat[which(regexpr("^\\+",mat) != -1)];
  mat <- gsub("^\\+ *","",mat);
  tmp <- strsplit(mat," +");
  tmp.f <- function(i){
    ret <- tmp[[i]];
    tmp.g <- function(j){
      return(tmp[[j]][i]);
    }
    if (i+1 <= length(tmp)) {
      ret2 <- unlist(sapply((i+1):length(tmp),tmp.g));
    } else {
      ret2 <- c()
    }
    ret <- c(ret,ret2);
    ret <- gsub("NA",NA,ret);
    ret <- as.double(ret);
    return(ret);
  }
  if (length(tmp) != 0){
    tmp2 <- unlist(lapply(1:length(tmp),tmp.f))
    if (length(mat.name) != length(tmp)){
      mat.name <- unique(gsub("[0-9]+","",mat.name));
      if (length(mat.name) == 1){
        warning("Interpolating names; the number of",mat.name,"does not match the extent of the matrix...");
        mat.name <- paste(mat.names,seq(along=tmp),sep="");
      } else if (length(mat.name) == 2) {
        small.name <- unique(sapply(mat.name,substring,first=0,last=min(sapply(mat.name,nchar))));
        if (length(small.name) == 1){
          large.name <- mat.name[which(max(sapply(mat.name,nchar)) == sapply(mat.name,nchar))];
          if (length(tmp) >= 10){
            mat.name <- paste(large.name,1:9,sep="");
            mat.name <- c(mat.name,paste(small.name,seq(along=tmp)[-(1:9)],sep=""));
            warning("Interpolating names; the name of",large.name," does not match the extent of the matrix...");
          } else {
            stop("Could not interpolate Names");
          }
        }
      } else {
        print(mat.name);
        stop("Could not interpolate names");
      }
    }
    tmp <- matrix(tmp2,byrow=TRUE,ncol=length(tmp), dimnames = list(mat.name,mat.name));
  } else {
    tmp <- NULL;
  }
  return(tmp);
}
fixMat <- function(mat){
                                        # This pastes the overflow of matricies on a single line, and then replaces
                                        # the line with NA.
                                        #
                                        #It also changes ..... to NA
  ## In addition it removes any breaks in the file
  mat[length(mat)+1] <- " ";
  plus <- which(regexpr("^\\+",mat) != -1);
  blank <- which(regexpr("^ +$",mat) != -1);
  plus <- plus[!((plus+1) %in% blank)]; # These have extension lines.
  if (length(plus) > 0){
    ## Assume that the next blank line is a separator
    blank <- blank[blank > plus[1]];
    blank <- blank[1:length(plus)];
    n <- blank-plus-1;
    tmp.f <- function(i){
                                        # Paste together.
      return(paste(" mat[plus[",i,"]] <- paste(mat[plus[",i,"]:(plus[",i,"]+n[",i,"])],collapse=\"\",sep=\"\");",
                   "mat[(plus[",i,"]+1):(plus[",i,"]+n[",i,"])] <- NA;",
                   sep=""));
    }
    eval(parse(text=paste(sapply(1:length(n),tmp.f),collapse="")));
    mat <- mat[!is.na(mat)];
  }
  mat <- gsub("\\.\\.+","NA",mat);
  cat("\n\n");
  return(mat);
}
getMod <- function(xpdb=NULL,run="run",mod.ext=".mod",lst.ext=".lst",mod.name=ifelse(is.null(xpdb),"",paste(run,xpdb@Runno,mod.ext,sep="")),ntheta=NA,...){
  cat("Model File:",mod.name,"\n");
  ## Get the Model file.
  con <- file(mod.name, "r");
  mod <- readLines(con) # empty
  close(con)
  mod <-  paste(mod,collapse="\n");
  mod <-  gsub("(\\$)([Pp][Kk]|[Ee][Rr][Rr][A-Za-z]*|[Dd][Ee][Ss])([ \t])","\n\\1\\2\n\\3",mod);
  mod <-  strsplit(mod,"\n")[[1]];
  recs <- which(regexpr("^\\$",mod)!=-1);
  on <- getNames.esn("OME",mod,recs);
  sn <- getNames.esn("SIG",mod,recs);
  tn <- getNames.esn("THE",mod,recs);
  assoc <- getAssocs(mod,recs);
  return(list(
              mod=mod,
              assoc=assoc,
              theta.names=tn$names,
              theta.units=tn$units,
              eta.names=on$names,
              eta.covs=on$covs,
              eps.names=sn$names,
              eps.covs=sn$covs
              ));
}
getNames.esn <- function(rec,mod,recs){
                                        # Get the variable labels for THETA, ETA and EPS.
  theta <- getRecs(rec,mod,recs);
  if (is.null(theta)){
    return(NULL);
  } else {
    est  <- "(?:\\([^();\n]*?(?:[();]|$)|[0-9.]+)";
    com  <- "\\= *;[;C]*.*?([A-Za-z][A-Za-z0-9_.]*.*)";
    comsame  <- ";[;C]*.*?([A-Za-z][A-Za-z0-9_.]*.*)";
    ## Replace FIXED.
    theta <- gsub("\\bFIX(?:ED?)?\\b","",theta,perl=TRUE);
    blk <- which(regexpr("\\bBLOCK *(?:\\( *[0-9]+ *\\))?\\b",theta,perl=TRUE) != -1);
    recs <- unique(c(which(regexpr("^\\$",theta,perl=TRUE) != -1),length(theta)+1));
    tmp.df <- data.frame(t(data.frame(d1=recs[-length(recs)],d2=recs[-1]-1)));
    tmp.f <- function(x){
      ret <- theta[seq(x[1],x[2])];
      return(ret);

    }
    recs <- lapply(tmp.df,tmp.f);
    tmp.f <- function(x){
      x <- strsplit(x,";[;C]*")[[1]];
      if (length(x) == 1){
        return("");
      } else {
        x <- paste(x[-1],collapse=";");
        x <-  gsub("THETA[0-9]*","",x);
        x <-  gsub("ETA","",x);
        x <-  gsub("EPS","",x);
        x <-  gsub("ERR","",x);
        x <- gsub("^[^A-Za-z\n]*([A-Za-z])","\\1",x);
      }
      return(x);
    }
    tmp.f2 <- function(x){
      return(strsplit(x,";")[[1]][1]);
    }
    tmp.f3 <- function(x,val="O"){
      x <- strsplit(x,"\n")[[1]];
      val <- strsplit(sub(est,val,paste(sapply(x,tmp.f2),collapse="\n"),perl=TRUE),"\n")[[1]];
      comment <- sapply(x,tmp.f);
      ret <- paste(paste(val,comment,sep=";"),collapse="\n");
      return(ret);
    }
    nms <- c();
    units <- c();
    last.block <- NULL;
    covs <- list();
    for (i in 1:length(recs)){
      cur.rec <- recs[[i]];
      est.labs <- c();
      if (any(regexpr("\\bBLOCK *(?:\\( *[0-9]+ *\\))?",cur.rec,perl=TRUE) !=-1)){
        if (any(regexpr("\\bSAME\\b",cur.rec,perl=TRUE) != -1)){
          if (is.null(last.block)){
            warning("Last estimates were not a BLOCK statement, assuming NO labels. ");
            est.labs <- c();
          } else {
            est.labs <- est.labs[which(regexpr("^[ \t]*;",cur.rec) != -1)];
            if (length(est.labs) > last.block){
              warning("There are more labels for BLOCK SAME then the last block. Removing extra labels at end.");
              est.labs <- est.labs[1:last.block];
            } else if (length(est.labs) < last.block){
              warning("There are less labels for BLOCK SAME than the last block.  Interpolating labels at end.");
              if (rec == "THE"){
                est.labs <- c(est.labs,paste("THETA(",seq(last.block-length(est.labs),last.block)+length(names)+length(est.labs),")",sep=""));
              } else if (rec == "OME"){
                est.labs <- c(est.labs,paste("ETA(",seq(last.block-length(est.labs),last.block)+length(names)+length(est.labs),")",sep=""));
              } else if (rec == "SIG"){
                est.labs <- c(est.labs,paste("EPS(",seq(last.block-length(est.labs),last.block)+length(names)+length(est.labs),")",sep=""));
              }
            }
            est.labs <- sapply(est.labs,tmp.f);
          }
        } else {
          cur.rec <- gsub("\\bBLOCK *(?:\\( *[0-9]+ *\\))?","",cur.rec,perl=TRUE);
          est.labs <- paste(cur.rec[which(regexpr(est,sapply(cur.rec,tmp.f2),perl=TRUE) != -1)],collapse="\n");
          i <- 1;
          j <- 1;
          while (regexpr(est,paste(sapply(strsplit(est.labs,"\n")[[1]],tmp.f2),collapse="\n"),perl=TRUE) != -1) {
            if (i == j){
              est.labs <- tmp.f3(est.labs,val="X");
            } else {
              est.labs <- tmp.f3(est.labs);
            }
            i <- i + 1;
            if (i > j){
              j <- j+1;
              i <- 1;
            }
          }
        }
        if (all(est.labs == "")){
          warning("Could not figure out some variable labels.");
          return(NULL);
        } else {
          est.labs <- strsplit(est.labs,"\n")[[1]];
          est.labs <- sapply(est.labs[regexpr("X[ \t]*$",sapply(est.labs,tmp.f2)) != -1],tmp.f);
          last.block <- length(est.labs);
          tmp <- length(names)+seq(1,length(est.labs));
          names(tmp) <- est.labs;
          covs[[length(covs)+1]] <- tmp;
        }
      } else {
        ## Get the records with an estimate.
        est.labs <- sapply(cur.rec[which(regexpr(est,sapply(cur.rec,tmp.f2),perl=TRUE) != -1)],tmp.f);
        last.block <- NULL;
      }
      tmp <- gsub(" *\\([^)]*\\) *$","",est.labs);
      nms <- c(nms,tmp);
      tmp <- gsub(".*(\\([^)]*\\)) *$","\\1",est.labs);
      tmp[which(regexpr("\\(",tmp)== -1)] <- "";
      units <- c(units,tmp);
    }
    names(nms)   <- NULL;
    names(units) <- NULL;
    ## Now replace based on rules.
    nms <- gsub(" (is|applies|appies|to) \\b","",nms,perl=TRUE);
    nms <- gsub("(THETA|ETA|OMEGA|ERR|SIGMA)[0-9]+","",nms);
    nms <- gsub("(THETA|ETA|OMEGA|ERR|SIGMA)[(][0-9]+[)]","",nms);
    nms <- gsub(";.*","",nms);
    nms <- gsub("  +"," ",nms);
    nms <- gsub("^ ","",nms);
    nms <- gsub(" $","",nms);


    units <- gsub(" (is|applies|appies|to) \\b","",units,perl=TRUE);
    units <- gsub("(THETA|ETA|OMEGA|ERR|SIGMA)[0-9]+","",units);
    units <- gsub("(THETA|ETA|OMEGA|ERR|SIGMA)[(][0-9]+[)]","",units);
    units <- gsub(";.*","",units);
    units <- gsub("  +"," ",units);
    units <- gsub("^ ","",units);
    units <- gsub(" $","",units);
    return(list(names=nms,
                units=units,
                covs=covs));
  }
}

getRecs <-function(rec,mod,recs){
                                        # Get the records rec and return them.
  theta.begin <- which(regexpr(paste("^\\$",rec,sep=""),mod)!=-1);
  theta.end <- recs[which(recs %in% theta.begin)+1];
  if (length(theta.begin) == 0){
    return(NULL);
  } else {
    theta.end[is.na(theta.end)] = length(mod)+1;
    if (length(theta.end) < length(theta.begin)){
      theta.end <- c(theta.end,length(mod));
    }
    tmp.f <- function(i){
      return(seq(theta.begin[i],theta.end[i]-1));
    }
    return(mod[(unlist(lapply(1:length(theta.begin),tmp.f)))]);
  }
}

getAssocs <- function(modO,recs){
  ## Get the thetas, ETAS and EPS values that are associated with one another.
  ## Also get the population and typical values associated with each variable.
                                        # Attempt to get thetas that are associated with one another.
  mod <- gsub(";.*","",modO); # Take out comments.
  mod <- gsub("^\\$PRO.*","",mod); # Take out comments.
  mod <- mod[regexpr("^ *$",mod)==-1]; # Take out blank lines
  mod <- mod[regexpr("^ *[^=\n ]* *=[^=\n]*$",mod)!=-1]; # only include lines with an equals sign and one item on the lhs.
  modAll <- mod;
  mod <- mod[regexpr("\\b(THETA|ETA|EPS)\\([0-9]+\\)",mod,perl=TRUE)!=-1]; # Only include lines with THETA, ETA or EPS values.
  mod <-  gsub("\\b\\$[A-Za-z0-9]*","",mod);
                                        # First find THETAs that correspond to some covariate effect.
                                        # THETA(x)*W or W*THETA(x) or THETA(X)*( or )*THETA(X)  or **THETA(x)
                                        # Find all theta effects.
                                        #lin <- mod[regexpr("(\\bTHETA\\([0-9]+\\) *\\* *[A-Z][A-Z0-9]*|[A-Z][A-Z0-9]* *\\* *\\bTHETA\\([0-9]+\\)|\\bTHETA\\([0-9]+\\) *\\* *\\(|\\) *\\* *\\bTHETA\\([0-9]+\\)|\\*\\* *THETA\\([0-9]+\\))",mod,perl=TRUE) !=-1];
  lin <- mod[regexpr("\\bTHETA\\([0-9]+\\)",mod,perl=TRUE) !=-1];
  thetas <- strsplit(lin,"\\bTHETA\\(",perl=TRUE);
  tmp.thetas <- function(x){
    return(as.double(gsub("([0-9]+).*","\\1",x[2])));
  }
  thetas <- sort(unique(unlist(lapply(thetas,tmp.thetas))));
  tmp.t2 <- function(x){
    tmp <- which(regexpr(paste("\\bTHETA\\(",x,"\\)",sep=""),mod)!=-1);
    if (length(tmp) > 0){
      tmp <- max(tmp);
      ## Look for any line except for the comparison line.
      theta.cmp <- mod[tmp];
      eq <- mod[-tmp];
      eq <- strsplit(mod,"=");
      tmp.eq <- function(x){
        return(regexpr(paste("\\b",gsub(" *","",x[1]),"\\b",sep=""),theta.cmp,perl=TRUE)!=-1);
      }
      l.eq <- c();
      eq <- mod[sapply(eq,tmp.eq)];
      while (length(eq) != length(l.eq)) {
        teq <- strsplit(eq,"=");
        tmp.eq2 <- function(x){
          return(which(regexpr(paste("\\b",gsub(" *","",x[1]),"\\b",sep=""),mod,perl=TRUE)!=-1));
        }
        l.eq <- eq;
        eq <- mod[unique(unlist(lapply(teq,tmp.eq2)))];
        eq <- eq[!is.na(eq)];
      }
      ## All lines associated with the theta varaible before it has been called
      ## are in eq.
      eq <- strsplit(paste(eq,collapse=" "),"\\bTHETA\\(",perl=TRUE)[[1]];
      ret <- sort(as.integer(gsub("([0-9]+).*","\\1",eq[2:length(eq)])));
      ## Now get all possible associations.
      eq <- modAll[which(regexpr(paste("\\bTHETA\\((",paste(ret,collapse="|"),")\\)",sep=""),modAll,perl=TRUE)!=-1)];
      eq <- strsplit(eq,"=");
      l.eq <- c();
      tmp.eq3 <- function(x){
        return(which(regexpr(paste("\\b",gsub(" *","",x[1]),"\\b",sep=""),modAll,perl=TRUE)!=-1));
      }
      eq <- unique(modAll[sort(unique(unlist(sapply(eq,tmp.eq3))))]);
      while (length(eq) != length(l.eq)) {
        teq <- strsplit(eq,"=");
        l.eq <- eq;
        eq <- unique(modAll[sort(unique(unlist(sapply(teq,tmp.eq3))))]);
        eq <- eq[!is.na(eq)];
      }
      eq <- strsplit(eq,"=");
      tmp.f <- function(x){
        return(gsub(" *","",x[1]));
      }
      eq <- sapply(eq,tmp.f);
      eq <- eq[regexpr("\\(",eq)==-1];
      lst <- eq;

      ## Now get all ETAs associated with the group.
      etas <- modAll[regexpr(paste("^ *(",paste(lst,collapse="|"),") *=",sep=""),modAll)!=-1];
      etas <- etas[regexpr("\\bETA",etas)!=-1];
      etas.all <- etas;
      if (length(etas) != 0){
        etas <- gsub("([0-9]+).*","\\1",strsplit(paste(etas,collapse=" "),"\\bETA\\(")[[1]][-1]);
      }
      etas <- as.integer(etas);
      ## Now get the type of etas.
      etas.all <- standardize.lines(etas.all);
      if (length(etas) > 1){
        etas.type <- "IOV";
      } else if (any(regexpr(paste("\\(1\\+ETA\\(",etas,"\\)\\)",sep=""),etas.all) > -1)){
        etas.type <- "CCV";
      } else if (any(regexpr(paste("DEXP\\(ETA\\(",etas,"\\)\\)",sep=""),etas.all) > -1)){
        etas.type <- "EXP";
      } else {
        etas.type <- "ADD";
      }
      ## Now get all EPS associated with the group.
      epss <- modAll[regexpr(paste(" *(",paste(lst,collapse="|"),") *=",sep=""),modAll)!=-1]; #
      epss <- epss[regexpr("\\bEPS",epss)!=-1];
      if (length(epss) != 0){
        epss <- gsub("([0-9]+).*","\\1",strsplit(paste(epss,collapse=" "),"\\bEPS\\(")[[1]][-1]);
      }
      epss <- as.integer(epss);
      ret <- list(ret,eq,etas,epss,etas.type);
    } else {
      stop("Something is wrong.");
    }
    return(ret);
  }
  theta.theta <- sapply(thetas,tmp.t2);
  ## Take out duplicate name possibilities.
  tmp.f <- function(x){
    return(theta.theta[2,x]);
  }
  dups <- unlist(sapply(1:dim(theta.theta)[2],tmp.f));
  dups <- dups[duplicated(dups)];
  tmp.f <- function(x){
    return(paste("theta.theta[2,",x,"][[1]] <- theta.theta[2,",x,"][[1]][!(theta.theta[2,",x,"][[1]] %in% dups)];",sep=""))
  }
  eval(parse(text=paste(sapply(1:dim(theta.theta)[2],tmp.f),collapse="")));
  ## Now look for all variables that are outputted in the tables to
  ## see associated output variables.
  tabVars <- strsplit(paste(getRecs("TAB",modO,recs),collapse=" "),"[ \n\t]+")[[1]];
  tmp.f <- function(x){
    return(tabVars[regexpr(paste("^(",paste(theta.theta[2,x][[1]],collapse="|"),")$",sep=""),tabVars)!=-1]);
  }
  theta.theta <- rbind(theta.theta,sapply(1:dim(theta.theta)[2],tmp.f));
                                        #theta.theta <- theta.theta[,which((sapply(theta.theta[1,],length)+sapply(theta.theta[3,],length))!=1)];
  return(theta.theta);
}

standardize.lines <- function(x){
  ## Known relationships:
  x <- paste(toupper(x),"\n",sep="");
  x <- gsub(";.*?\n","\n",x,perl=TRUE);
  x <- gsub("EXP","DEXP",x,perl=TRUE);
  ## Take out ALL spaces
  x <- gsub(" ","",x);
  ## Shift all (ETA(1)+1) to (1+ETA(1))
  x <- gsub("\\b\\(ETA\\(([0-9]+)\\)\\+1\\)","(1+ETA(\\1)",x,perl=TRUE);
  ## Shift ALL COV*THETA(x) -> THETA(x)*COV
  x <- gsub("\\b([A-Z][A-Z0-9]*)\\*THETA\\(([0-9]+)\\)","THETA(\\2)*\\1",x,perl=TRUE);

  ## Shift ALL (COV-#)*THETA(x) -> THETA(x)*(COV-#)
  x <- gsub("\\(([A-Z][A-Z0-9]*-[0-9.]+)\\)\\*THETA\\(([0-9]+)\\)","THETA(\\2)*(\\1)",x,perl=TRUE);

  ## Shift all (THETA(x)*COV+1) -> (1+THETA(x)*COV)
  x <- gsub("\\(THETA\\(([0-9]+)\\)\\*([A-Z][A-Z0-9]*)+1\\)",
            "(1+THETA(\\1)*\\2)",x);

  ## Shift all (COV)**THETA(y)*THETA(x) to THETA(x)*(COV)**THETA(y)
  x <- gsub("([(]?[A-Z][A-Z0-9]*(?:/[0-9.]+)?[)]\\*\\*[(]?THETA[(][0-9]+[)]{1,2})\\*(THETA[(][0-9]+[)])",
            "\\2*\\1",x,perl=TRUE);
  ## Shift (THETA(x)*COV+1) -> (1+THETA(x)*COV)
  x <- gsub("\\((THETA\\([0-9]+\\)\\*[A-Z][A-Z0-9]*)\\+1\\)","(1+\\1)",x);

  ## Shift (1+THETA(x)*COV)*THETA(y) -> THETA(y)*(1+THETA(x)*COV)
  x <- gsub("(\\(1\\+THETA\\([0-9]+\\)\\*[A-Z][A-Z0-9]*\\))\\*(THETA\\([0-9]+\\))","\\2*\\1",x);

  ## Change proportional shifts
  ## THETA(x)*(1+THETA(y)*COV) -> THETA(x)*(1~THETA(y)*COV)
  x <- gsub("(THETA\\([0-9]\\)\\*\\(1)\\+(THETA\\([0-9]+\\)\\*[A-Z][A-Z0-9]*\\))","\\1~\\2",x);
  x <- gsub("\n\n+","\n",x);
  return(x);

}

shrinkage <- function(xpdb,pr=NULL){
  return(c(shrinkage.eta(xpdb,pr),shrinkage.eps(xpdb)));
}
shrinkage.eta <- function(xpdb,pr=NULL){
                                        # Calculate the extent of eta shrinkage of the data.
  eta <- c();
  ## SE etabar times sqrt(n) to estimate the standard deviation of eta.
  if (pr$etabar){
    eta <- pr$etabar.se*sqrt(pr$n);
  }
  if (length(eta) == 0) {
    if (is.null(xpdb) &&any(regexpr("ETAS",ls(envir=globalenv())) >= 0)) {
      if (any(regexpr("^ETA[0-9]+$",names(ETAS)) >= 0)) {
        d <- ETAS[,regexpr("^ETA[0-9]+$",names(ETAS)) >= 0];
      } else {
        shrink <-  rep(NA,length(diag(pr$f.s)));
        return(shrink);
      }
    } else if (is.null(xpdb) &&any(regexpr("ALL",ls(envir=globalenv())) >= 0)) {
      if (any(regexpr("^ETA[0-9]+$",names(ALL)) >= 0)) {
        d <- ALL[,regexpr("^ETA[0-9]+$",names(ALL)) >= 0];
      } else {
        shrink <-  rep(NA,length(diag(pr$f.s)));
        return(shrink);
      }
    } else {                                        ## Try directly from the data.
      d <- xpdb@Data;
      d <- d[!duplicated(xvarval("id",xpdb)),];
      d <- xvarval("ranpar",xpdb,data=d);
    }
    ## If we grabbed the right parameters, use the parameters directly to calculate SD.
    if (! (pr$etabar) || length(d) == length(eta)){
      eta <- sapply(d,sd,na.rm=TRUE);
    }                               #;s
  }
  shrink <-  c();
  if (length (eta) != 0 & ! all(is.na(eta))) {
    eta <- eta[order(names(eta))];
    e2 <- sqrt(diag(pr$f.o));
    if (length(e2) == 0){
      n <-max(length(diag(pr$f.s)),length(diag(pr$f.o)),na.rm=TRUE)
      shrink <- rep(NA,n);
    } else {
      e2 <- e2[order(names(e2))];
      shrink <- 1-eta/e2;
    }
  } else {
    n <-max(length(diag(pr$f.s)),length(diag(pr$f.o)),na.rm=TRUE)
    shrink <-  rep(NA,n);
    names(shrink) <-  paste("ETA",seq(1:n),sep="");
  }
  return(shrink*100);
}

shrinkage.eps <- function(xpdb) {
  if (is.null(xpdb) &&any(regexpr("ALL",ls(envir=globalenv())) >= 0)) {
    if(any(regexpr("^IWRE",names("ALL")) >= 0)){
      ret <- 1-sd(ALL[,which(regexpr("^IWRE",names("ALL")) >= 0)[1]]);
      names(ret) <- "EPS"
      return(ret*100);
    } else {
      return(NULL);
    }
  } else {
    if (is.null(check.vars("iwres",xpdb))){
      return(NULL);
    } else {
      ret <- 1-sd(xvarval("iwres",xpdb));
      names(ret) <- "EPS";
      return(ret*100);
    }
  }
}

theta.table <- function(xpdb,pr=NULL){
  if(is.null(pr)){
    if (exists("smy")){
      pr <- smy[[xpdb@Runno]]
    }
  }
  ci <- pr$ci;
  est <- pr$f.t;
  se <- pr$s.t;
  if (is.null(se)){
    se <- rep(NA,length(est));
    rse <- rep(NA,length(est));
    cil <- rep(NA,length(est));
    ciu <- rep(NA,length(est));
  } else {
    rse=pr$s.t/pr$f.t*100;
    cil=(pr$f.t+pr$s.t*qnorm((1-ci)/2));
    ciu=(pr$f.t-pr$s.t*qnorm((1-ci)/2));
  }
  return(
         data.frame(
                    theta=pr$theta.names,
                    units=pr$theta.units,
                    est=est,
                    se=se,
                    rse=rse,
                    cil=cil,
                    ciu=ciu
                    )
         );
}

eta.table <- function(xpdb,pr=NULL){
  if(is.null(pr)){
    if (exists("smy")){
      pr <- smy[[xpdb@Runno]]
    }
  }
  ci <- pr$ci;
  shrink <- pr$shrinkage;

  return(
         data.frame(
                    eta=pr$eta.names,
                    est=diag(pr$f.o),
                    se=diag(pr$s.o),
                    rse=diag(pr$s.o)/diag(pr$f.o)*100,
                    ## This assumes lognormal distribution of parameters.
                    cv=sqrt(exp(diag(pr$f.o))-1)*100,
                    cil=(diag(pr$f.o)+diag(pr$s.o)*qnorm((1-ci)/2)),
                    ciu=(diag(pr$f.o)-diag(pr$s.o)*qnorm((1-ci)/2)),
                    shrink=shrink,
                    etabar =pr$etabar.val,
                    etabar.se = pr$etabar.se,
                    etabar.p = pr$etabar.p
                    )
         );
}
eps.table <- function(xpdb,pr=NULL){
  if(is.null(pr)){
    if (exists("smy")){
      pr <- smy[[xpdb@Runno]]
    }
  }
  ci <- pr$ci;
  shrink <- pr$shrinkage;
  shrink <- shrink[regexpr("ETA",names(shrink))==-1];
  shrink2 <- shrinkage.eps(xpdb);
  if (!is.null(shrink2)){
    return(
           data.frame(
                      eps=diag(pr$f.s),
                      se=diag(pr$s.s),
                      rse=diag(pr$s.s)/diag(pr$f.s)*100,
                      cv=sqrt(diag(pr$f.s))*100,
                      cil=(diag(pr$f.s)+diag(pr$s.s)*qnorm(1-ci/2)),
                      ciu=(diag(pr$f.s)-diag(pr$s.s)*qnorm(1-ci/2)),
                      shrink=shrink2
                      )
           );
  } else {
    return(
           data.frame(
                      eps=diag(pr$f.s),
                      se=diag(pr$s.s),
                      rse=diag(pr$s.s)/diag(pr$f.s)*100,
                      cv=sqrt(diag(pr$f.s))*100,
                      cil=(diag(pr$f.s)+diag(pr$s.s)*qnorm(1-ci/2)),
                      ciu=(diag(pr$f.s)-diag(pr$s.s)*qnorm(1-ci/2)),
                      shrink=NA
                      )
           );
  }
}


## Control Stream/Model Functions
nm.advan.trans <- list(
                                        #ADVAN1
                       c("(([SFRD]|ALAG)[012C]|XSCALE)",
                         "K", #TRANS1
                         "(CL|V)" #TRANS2
                         ),
                                        #ADVAN2
                       c("(([SFRD]|ALAG)[0123C]|XSCALE)",
                         "KA?", #TRANS1
                         "(KA|CL|V)"), #TRANS2
                                        #ADVAN3
                       c("(([SFRD]|ALAG)[0123C]|XSCALE)",
                         "K(12|21)?", #TRANS1
                         NA, #TRANS2
                         "(CL|V|Q|VSS)", #TRANS3
                         "(CL|V1|Q|V2)", #TRANS4
                         "(AOB|ALPHA|BETA)", #TRANS5
                         "(ALPHA|BETA|K21)"), #TRANS6
                                        #ADVAN4
                       c("(([SFRD]|ALAG)[01234C]|XSCALE)",
                         "K(|23|32|A)", #TRANS1
                         NA, # TRANS2
                         "(CL|V|Q|VSS|KA)", #TRANS3
                         "(CL|V[23]|Q|KA)", #TRANS4
                         "(AOB|ALPHA|BETA|KA)", #TRANS5
                         "(ALPHA|BETA|K32|KA)"), #TRANS6
                                        #ADVAN5
                       c("(([SFRDK]|ALAG)[0123C]|XSCALE)"),
                                        #ADVAN6
                       c("(([SFRD]|ALAG)[0123C]|XSCALE)"),
                       ##ADVAN7
                       c("(([SFRDK]|ALAG)[0123C]|XSCALE)"),
                       ## ADVAN8
                       c("(([SFRD]|ALAG)[0123C]|XSCALE)"),
                       ## ADVAN9
                       c("(([SFRD]|ALAG)[0123C]|XSCALE)"),
                       ## ADVAN10
                       c("([VK]M|([RFSD]|ALAG)[12]|XSCALE"),
                       ## ADVAN11
                       c("(([SFRD]|ALAG)[01234C]|XSCALE)",
                         "K(12|21|13|31)", # TRANS1
                         NA, #TRANS2
                         NA, #TRANS3
                         "(CL|V[123]|Q[23])", #TRANS4
                         NA, #TRANS5
                         "(ALPHA|BETA|GAMMA|K(21|31)"), #TRANS6
                                        #AVDAN12
                       c("(([SFRD]|ALAG)[012345C]|XSCALE)",
                         "(K(|23|32|24|42|A)", #TRANS1
                         NA, #TRANS2
                         NA, #TRANS3
                         "(CL|V[234]|Q[34]|KA)", #TRANS4
                         NA, #TRANS5
                         "(ALPHA|BETA|GAMMA|K(32|42))") #TRANS6
                       );

getErrors <- function(pr){
  ## No Covaraince.  SEMs are not present
  errors <- c();
  if (!pr$s) {
    errors <- c("No covariance step",errors);
  }
  if (any(regexpr("0MINIMIZATION SUCCESSFUL",pr$lst) != -1)){
    ## Minimization  sucessful.
  } else {
    errors <- c("Unsuccessful minimization",errors);
  }
  ## S Matrix Algorithmically singular
  ## "ALGORITHMICALLY SINGULAR" and "S MATRIX"
  if (any(regexpr("S MATRIX.*ALGORITHMICALLY SINGULAR",pr$lst)!=-1)) {
    errors <- c("S matrix algirothmically singular",errors);
  }
  ## R Matrix Algorithmically singular
  ## "ALGORITHMICALLY SINGULAR" and "R MATRIX"
  if (any(regexpr("R MATRIX.*ALGORITHMICALLY SINGULAR",pr$lst)!=-1)) {
    errors <- c("R matrix algorithmically singular",errors);
  }
  ## "NON-POSITIVE-SEMIDEFINITE" and "S MATRIX"

  if (any(regexpr("S MATRIX.*NON-POSITIVE-SEMIDEFINITE",pr$lst)!=-1)) {
    errors <- c("S matrix non-positive-semidefinite",errors);
  }

  ## "NON-POSITIVE-SEMIDEFINITE" and "R MATRIX"
  if (any(regexpr("R MATRIX.*NON-POSITIVE-SEMIDEFINITE",pr$lst)!=-1)) {
    errors <- c("R matrix non-positive-semidefinite",errors);
  }
  ## Rounding Errors. "ROUNDING ERRORS"
  ## Number of SIG. DIGITS IN FINAL EST.: +([0-9.])+
  ## NO. OF SIG. DIGITS UNREPORTABLE
  if (any(regexpr("(ROUNDING ERRORS|NO\\. OF SIG\\. DIGITS UNREPORTABLE)",pr$lst)!=-1)){
    if (any(regexpr("SIG\\. DIGITS IN FINAL EST\\.: +([0-9.]+)",pr$lst)!=-1)){
      tmp <- pr$lst[which(regexpr("SIG\\. DIGITS IN FINAL EST\\.: +([0-9.]+)",pr$lst) != -1)[1]];
      tmp<- gsub("^.*SIG\\. DIGITS IN FINAL EST\\.: +([0-9.]+).*$","\\1",tmp);
      errors <- c(paste("Rounding Errors (",tmp,")",sep=""),errors);
    } else if (any(regexpr("NO\\. OF SIG\\. DIGITS UNREPORTABLE",pr$lst))){
      errors <- c("Unknown number of Significant Digits",errors);
    } else {
      errors <- c("Rounding Errors",errors);
    }
  }
  ## number of "RESET HESSIAN" found.
  tmp <- sum((regexpr("\\bRESET HESSIAN\\b",pr$lst,perl=TRUE)!=-1)*1);
  if (tmp > 0){
    errors <- c(paste(tmp,"hessian resets"),errors);
  }
  ## Zero Graident Found.
  grad <- pr$grad;
  if (any(unlist(grad)==0,na.rm=TRUE)) {
    errors <- c("Zero gradients",errors);
  }
  ## Final Zero Gradient.
  if (any(grad[[length(grad)]]==0,na.rm=TRUE)){
    errors <- c("Final zero gradients",errors);
  }
  parm <- pr$parm;
  if (any(parm[[length(parm)]] == 0.1,na.rm=TRUE)){
    error <-  c("Some parameters have not moved",errors);
  }
  ## Large Final Gradient. (>100)
  if (any(grad[[length(grad)]] > 100,na.rm=TRUE)){
    errors <- c("Large final gradients (>100)",errors);
  }
  ## Small Final Graident. (<0.01)
  if (any(grad[[length(grad)]] < 0.01,na.rm=TRUE)){
    errors <- c("Small final gradients (<0.01)",errors);
  }
  ## High condition number > 1000
  if (pr$eigen){
    if (length(pr$eigen.cn) == 1) {
      if (pr$eigen.cn > 1000){
        errors <- c(paste("High condition number (",pr$eigen.cn,")",sep=""),errors);
      }
    }
  }
  if (pr$s) {
    ## High Theta, Omega, and Sigma %RSE (>30%)
    tmp <- c();
    if (any(pr$theta.tab$rse > 30,na.rm=TRUE)) {
      tmp <- c(paste(pr$theta.tab[pr$theta.tab$rse > 30,]$theta),tmp);
    }
    if (any(pr$eta.tab$rse > 30,na.rm=TRUE)) {
      tmp <- c(paste(pr$eta.tab[pr$eta.tab$rse > 30,]$eta),tmp);
    }
    if (any(pr$eps.tab$rse > 30,na.rm=TRUE)){
      tmp <- c(paste(pr$eps.tab[pr$eps.tab$rse > 30,]$eps),tmp);
    }
    if (length(tmp) > 0){
      errors <- c(paste("Large SEs (RSE > 30\\\\%; ",paste(sapply(tmp,getVarName),collapse=", "),")",sep=""),errors);
    }
    ## Theta, Omega, and Sigma, CI contains 0.
    tmp <- c();
    if (any(pr$theta.tab$cil <= 0 & pr$theta.tab$ciu >= 0,na.rm=TRUE)){
      tmp <- c(paste(pr$theta.tab[pr$theta.tab$cil <= 0 & pr$theta.tab$ciu >= 0,]$theta),tmp);
    }
    if (any(pr$eta.tab$cil <= 0 & pr$eta.tab$ciu >= 0,na.rm=TRUE)){
      tmp <- c(paste(pr$eta.tab[pr$eta.tab$cil <= 0 & pr$eta.tab$ciu >= 0,]$eta),tmp);
    }
    if (any(pr$eps.tab$cil <= 0 & pr$eps.tab$ciu >= 0,na.rm=TRUE)){
      tmp <- c(paste(pr$eps.tab[pr$eps.tab$cil <= 0 & pr$eps.tab$ciu >= 0,]$eps),tmp);
    }
    ## TODO:  Add CI contain one for powers.
    if (length(tmp) > 0) {
      errors <- c(paste("CIs contain zero (",paste(sapply(tmp,getVarName),collapse=", "),")",sep=""),errors);
    }
  }
  ## Shrinkage (>20%)
  tmp <- c();
  if (any(pr$eta.tab$shrink>20,na.rm=TRUE)){
    tmp <- c(paste(pr$eta.tab[pr$eta.tab$shrink>20,]$eta),tmp);
  }
  if (!is.null(pr$shrinkage["ETA"])){
    if (!is.na(pr$shrinkage["ETA"])){
      if (pr$shrinkage["ETA"] > 20){
        tmp <- c("ETA",tmp);
      }
    }
  }
  if (length(tmp) > 0){
    errors <- c(paste("Large shrinkage (>20\\\\%; ",paste(sapply(tmp,getVarName),collapse=", "),")",sep=""),errors);
  }
  ## Significant Correlations (0.9)
  if (pr$cor){
    cor <- pr$cor.val;
    if(is.null(cor)){
    } else {
      diag(cor) <- NA;
      tmp <- data.frame(cor);
      tmp.f <- function(x){
        return(any(abs(x) > 0.9,na.rm=TRUE));
      }
      if (any(sapply(tmp,tmp.f))){
        errors <- c("Correlations > 0.9",errors);
      }
    }
  }
  ## ETAs may be nonzero.
  if (pr$etabar){
    tmp <- c();
    if (any(pr$eta.tab$etabar.p < 0.05,na.rm=TRUE)){
      tmp <- c(paste(pr$eta.tab[pr$eta.tab$etabar.p < 0.05,]$eta),tmp);
    }
    if (length(tmp) > 0){
      errors <- c(paste("Nonzero etas (p < 0.05; ",paste(tmp,collapse=", "),")",sep=""),errors);
    }
  }

  ## Errors in PRDERR
  ## THERE ARE ERROR MESSAGES IN FILE PRDERR
  if (any(regexpr("THERE ARE ERROR MESSAGES IN FILE PRDERR",pr$lst)!=-1)){
    error <- c("Errors in PRDERR",errors);
  }
  if (any(regexpr("PARAMETER ESTIMATE IS NEAR ITS BOUNDARY",pr$lst)!=-1)){
    error <- c("Parameter near boundary",errors);
  }
  if (any(regexpr("[0-9]+, EXCEEDED",pr$lst)!=-1)){
    error <- c("Number of iterations exceeded",errors);
  }
  if (any(regexpr("ERROR=136",pr$lst)!=-1)){
    error <- c("Near infinite objective function",errors);
  }
  if (any(regexpr("SUM OF \"SQUARED\" WEIGHTED INDIVIDUAL RESIDUALS IS INFINITE",pr$lst)!=-1)){
    error <- c("Inifite WRES",errors);
  }
  if (any(regexpr("HESSIAN OF POSTERIOR DENSITY IS NON-POSITIVE-DEFINITE DURING SEARCH",pr$lst)!=-1)){
    error <- c("Hessian non-positive-definite",errors);
  }
  if (any(regexpr("TO DIFFERENTIAL EQUATIONS, *([0-9]+) *, MAY BE TOO LARGE",pr$lst)!=-1)){
    error <- c("Tolerance too large?",errors);
  }
  if (any(regexpr("HOWEVER, PROBLEMS OCCURRED WITH THE MINIMIZATION",pr$lst)!=-1)){
    error <- c("Problems in minimization",errors);
  }
  if (any(regexpr("PK PARAMETER FOR ([A-Z0-9]+) IS NON-POSITIVE/",pr$lst)!=-1)){
    error <- c("Non-positive PK parameter",errors);
  }
  errors <-  c(paste("MVOF",pr$mvof),errors);
  errors <-  c(paste("AIC",round(pr$mvof+2*length(grad[[length(grad)]]),3)),errors);

  errors <-  c(paste("SBC",round(pr$mvof+sqrt(pr$n.obs)*length(grad[[length(grad)]]),3)),errors);
  return(errors);

}


getVarName <- function(x,cov=0){
  ## Get smallest variable name of X.  Must be at least 1 character.
  found <- FALSE;
  if (file.exists("./labels.csv")){
    f <- read.csv("./labels.csv");
    f$var <- tolower(f$var);
    n <- paste(f[f$var%in%tolower(x)&f$covariate==cov,]$label);
    if (length(n) == 1){
      found <- TRUE;
      ret <- gsub("\\","\\\\",n,fixed=TRUE);
    }
  }
  if (!found){
    len <- sapply(x,nchar);
    ret <- x[len>0];
    len <- sapply(ret,nchar);
    if (length(ret) == 0){
      ret <-  x;
      return(ret);
    } else {
      ret <- ret[which(len==min(len))];
      ret <- ret[1];
            ret <- gsub("_","~",ret);
        }
    }
                                        # Now replace based on rules.
    ret <- gsub("( |^)(is|applies|appies|to)( |$)\\b","",ret,perl=TRUE);
    ret <- gsub("( |^)(THETA|ETA|OMEGA|ERR|SIGMA)\\(?[0-9]+\\)?( |$)","",ret,perl=TRUE);
    ret <- gsub(";.*","",ret);
    ret <- gsub("  +"," ",ret,perl=TRUE);
    ret <- gsub("^ ","",ret);
    ret <- gsub(" $","",ret);
    return(ret);
}


overview.table <- function(xpdb,pr=NULL,
                           ci.col=TRUE,
                           shrinkage=TRUE,
                           p.value=0.05,
                           r2=0.9,
                           run.summary=cmp,
                           cmp.table=FALSE,
                           ...
                           ){
                                        #  eps.tab <- eps.table(xpdb,pr);
    if (is.null(pr)){
        if (!any(names(smy) == xpdb@Runno)){
            pr <-  getSum(xpdb,...);
            smy[[xpdb@Runno]] <<- pr;
        } else {
            pr <- smy[[xpdb@Runno]]
        }
    }

    theta.tab <- NULL;
    eta.tab <- NULL;

    get.theta.eta <- function(xpdb,pr=NULL){
        if (is.null(pr)){
            if (!any(names(smy) == xpdb@Runno)){
                pr <-  getSum(xpdb,...);
                smy[[xpdb@Runno]] <<- pr;
            } else {
                pr <- smy[[xpdb@Runno]]
            }
        }
        theta.tab <- theta.table(xpdb,pr);
        theta.tab$t <- 1:length(theta.tab[,1]);
        if (!is.null(theta.tab.ref) & !is.null(pr.ref)){
            best.assoc <- function(i){
                vars <- pr$assoc[2,i][[1]];
                best.assoc2 <- function(j){
                    return(sum((vars %in% pr.ref$assoc[2,j][[1]])*1));
                }
                x <- sapply(1:length(pr.ref$assoc[2,]),best.assoc2);
                if (length(x) == 0){
                    x <- c();
                } else {
                    x <- which(max(x) == x);
                }
                if (length(x) > 1) {
                    ## Now get the best association in terms of Levenshtein distance since there is a tie.
                    var.lines <-
                        paste(pr$mod[which(regexpr(paste("^ *(",paste(vars,collapse="|"),") *=",sep=""),pr$mod)  != -1)],collapse="\n");
                    var.ref.f <- function(j){
                        return(paste(pr.ref$mod[which(regexpr(paste("^ *(",paste(pr.ref$assoc[2,j][[1]],collapse="|"),") *=",sep=""),pr.ref$mod)  != -1)],collapse="\n"));
                    }
                    var.ref <- unlist(lapply(1:length(pr.ref$assoc[2,]),var.ref.f));
                    return(compare.lines(var.lines,var.ref,theta=i));
                } else {
                    return(x);
                }
            }
            t1 <- seq(1,length(pr$assoc[2,]));
            df <- data.frame(t=t1,t2=sapply(t1,best.assoc));
            tmp <- merge(theta.tab,df,all=TRUE);
            tmp2 <- theta.tab.ref;
            tmp2$t2 <- 1:length(tmp2[,1]);
            tmp2 <- tmp2[,names(tmp2) %in% c("est","t2")];
            names(tmp2) <- gsub("est","est.ref",names(tmp2));
            tmp <- merge(tmp,tmp2,all.x=TRUE);
            tmp$delta <- tmp$est-tmp$est.ref;
            theta.tab <- tmp;
        }
        ## Create theta-eta table.
        theta.eta <- NULL;
        for (i in 1:length(pr$assoc[3,])){
            if (length(pr$assoc[3,i][[1]]) > 0){
                tmp <- data.frame(t=i,e=pr$assoc[3,i][[1]]);
            } else {
                tmp <- data.frame(t=i,e=NA);
            }
            if (is.null(theta.eta)){
                theta.eta <- tmp;
            } else {
                theta.eta <- rbind(theta.eta,tmp);
            }
        }
        theta.tab <- merge(theta.tab,theta.eta,all=TRUE);
        theta.tab <<- theta.tab;
        eta.tab <- eta.table(xpdb,pr=pr);
        if (!is.null(eta.tab.ref) & !is.null(pr.ref)){
            ## merge Etas.
            tmp.f <- function(x,prv=pr){
                ret <- prv$assoc[3,x][[1]];
                if (length(ret) == 1){
                    return(ret);
                } else if (length(ret) > 1) {
                    return(paste(ret,collapse=","));
                } else {
                    return(NA);
                }
            }
            df$e <- sapply(df$t,tmp.f);
            df$e2 <- sapply(df$t2,tmp.f,prv=pr.ref);
            df <- df[!is.na(df$e),c("e","e2")];
            tmp <- eta.tab;
            tmp$e <- seq(1,length(tmp[,1]));
            tmp <- merge(tmp,df);
            tmp2 <- eta.tab.ref;
            tmp2$e2 <- seq(1,length(tmp2[,1]));
            tmp2 <- tmp2[,c("e2","cv")];
            names(tmp2) <- gsub("cv","cv.ref",names(tmp2));
            tmp <- merge(tmp,tmp2);
            tmp$delta <- tmp$cv-tmp$cv.ref;
            eta.tab <- tmp[,!(names(tmp) %in% c("e2","cv.ref"))];
        }
        names(eta.tab) <- gsub("eta.eta","eta.",paste("eta.",names(eta.tab),sep=""));
        eta.tab$e <- 1:length(eta.tab[,1]);
        eta.tab <<- eta.tab;
        theta.eta <- merge(theta.tab,eta.tab,all=TRUE);
        w.eta.p <- which(theta.eta$eta.bar.p <= p.value);
        w.eta.ci <- which(theta.eta$eta.cil <= 0);
        ## Get which are in both.
        w.eta.p.ci <- w.eta.p[w.eta.p %in% w.eta.ci];
        w.eta.ci <- w.eta.ci[!(w.eta.ci %in% w.eta.p.ci)];
        w.eta.p <- w.eta.p[!(w.eta.p %in% w.eta.p.ci)];
        w.theta.ci <- which(theta.eta$cil <= 0);
        keep <- c("e","theta","units","est","rse");
        if (all(unlist(pr$assoc[5,which(sapply(pr$assoc[3,],length) == 1)]) == "EXP")){
            keep <- c(keep,"eta.cv");
        } else {
            keep <- c(keep,"eta.se");
        }
        keep <- c(keep,"eta.rse");
        if (!is.null(eta.tab.ref) & !is.null(theta.tab.ref)){
            keep <- c(keep,"delta","eta.delta");
        } else {
            if (ci.col){
                keep <- c(keep,"cil","ciu");
            }
            if (shrinkage){
                keep <- c(keep,"eta.shrink");
            }
        }
        theta.eta <- theta.eta[,names(theta.eta) %in% keep];
        theta.eta[,!(names(theta.eta) %in% c("theta","units","e"))] <-
            sapply(theta.eta[,!(names(theta.eta) %in% c("theta","units","e"))],nm.round,
                   ...);
        theta.eta$e <- paste(theta.eta$e);
        ## Now footnote any items that have a confidence interval that contains zero.
        vars <- c("eta.cv","eta.se","eta.rse","eta.shrink");
        vars <- vars[vars %in% names(theta.eta)];
        for (var in vars){
            theta.eta[w.eta.ci,var] <- paste(theta.eta[w.eta.ci,var],"$^{.ifn1.}$",sep="");
            theta.eta[w.eta.p,var] <- paste(theta.eta[w.eta.p,var],"$^{.ifn2.}$",sep="");
            theta.eta[w.eta.p.ci,var] <- paste(theta.eta[w.eta.p.ci,var],"$^{.ifn1.,.ifn2.}$",sep="");
        }
        for (var in c("est","rse")){
            theta.eta[w.theta.ci,var] <- paste(theta.eta[w.theta.ci,var],"$^{.ifn1.}$",sep="");
        }
        if (is.null(eta.tab.ref) | is.null(theta.tab.ref)){
            if (ci.col){
                theta.eta$ci <- paste("(",theta.eta$cil,",",theta.eta$ciu,")",sep="");
                theta.eta$ci <- gsub("\\(NA,NA\\)","NA",theta.eta$ci);
                theta.eta <- theta.eta[,!(names(theta.eta) %in% c("cil","ciu"))];
            }
        }
        theta.eta$theta <- paste(theta.eta$theta,theta.eta$units);
        theta.eta <- theta.eta[,names(theta.eta) != "units"];
        ord <- c("e","theta","est","rse");
        if (!is.null(eta.tab.ref) & !is.null(theta.tab.ref)){
            ord <- c(ord,"delta");
        } else if (ci.col){
            ord <- c(ord,"ci");
        }
        if (all(unlist(pr$assoc[5,which(sapply(pr$assoc[3,],length) == 1)]) == "EXP")) {
            ord <- c(ord,"eta.cv");
        } else {
            ord <- c(ord,"eta.se");
        }
        ord <- c(ord,"eta.rse");
        if (!is.null(eta.tab.ref) & !is.null(theta.tab.ref)){
            ord <- c(ord,"eta.delta");
        } else if (shrinkage){
            ord <- c(ord,"eta.shrink");
        }
        theta.eta <- theta.eta[order(theta.eta$e),ord];
        if (is.null(run.summary)){
            theta.eta[1,"theta"] <- paste(".ENDHEAD.\\hline\\hline\n",theta.eta[1,"theta"],sep="");
        }
        theta.eta[theta.eta$e == "NA",names(theta.eta) %in% c("eta.cv","eta.se","eta.rse","eta.shrink","e","eta.delta")] <- ".GREYBG.";
        theta.eta[duplicated(theta.eta$e),names(theta.eta) %in% c("eta.cv","eta.se","eta.rse","eta.shrink","e","eta.delta")] <- "";

        if (!is.null(run.summary)){
            err <- pr$errors;
            if (!is.null(theta.tab.ref) & !is.null(eta.tab.ref) & !is.null(pr.ref)){
                tmp <- data.frame(cur=as.numeric(gsub("(MVOF|AIC|SBC) ","",pr$errors[1:3])),
                                  ref=as.numeric(gsub("(MVOF|AIC|SBC) ","",pr.ref$errors[1:3])));
                tmp$d <- tmp$cur - tmp$ref;
                err[1:3] <- paste(err[1:3],", $\\Delta$=",round(tmp$d[1:3],3),"",sep="");
                grad <- pr$grad;
                n <-  length(grad[[length(grad)]]);
                grad <- pr.ref$grad;
                n.ref <- length(grad[[length(grad)]]);
                p.value <-  function(x){
                    if (x < 0.001) {
                        return("<0.001");
                    } else {
                        return(paste("=",round(x,3)));
                    }
                }
                if (n > n.ref) {
                    ## Adding parameter
                    if (pr$mvof < pr.ref$mvof) {
                        err <- c(err[1:3],
                                 paste("Adding made better model, p",p.value(1-pchisq(pr.ref$mvof-pr$mvof, n-n.ref)),sep=""),
                                 err[-(1:3)]);
                    } else if (pr$mvof == pr.ref$mvof) {
                        err <- c(err[1:3],"Adding had no change in MVOF.",err[-(1:3)]);
                    } else {
                        err <- c(err[1:3],"Adding made model worse.",err[-(1:3)]);
                    }
                } else if (n < n.ref) {
                                        # Removing
                    if (pr$mvof > pr.ref$mvof) {
                        err <- c(err[1:3],
                                 paste("Removing made worse model, p",
                                       p.value(1-pchisq(pr$mvof-pr.ref$mvof,n.ref-n)),sep="")
                                 ,err[-(1:3)]);
                    } else if (pr$mvof == pr.ref$mvof ) {
                        err <- c(err[1:3],"Removing had no change in MVOF.",err[-(1:3)]);
                    } else {
                        err <- c(err[1:3],"Removing made model better.",err[-(1:3)]);
                    }
                }
            }
            theta.eta$run <- "";
            theta.eta$run[1] <- paste(ifelse(length(theta.eta[,1]) == 0,"",
                                             paste("{}rowspan",length(theta.eta[,1])+1," ",sep="")),
                                      "\\textbf{\\href{",gsub("_","...-...",gsub("^ +","",xpdb@Runno)),".RTFEXT}{...CF2...",gsub("^ +","",gsub("_","\\\\textunderscore ",xpdb@Runno)),"}}",
                                      "\\begin{itemize}\n\\item ",
                                      paste(gsub("\\\\+","\\\\",gsub("\\([^)]*\\)","",err)),collapse="\n\\item "),
                                      "\\end{itemize}",sep="");
            theta.eta <- theta.eta[,c("run",ord)];
            theta.eta[1,"run"] <- paste(".ENDHEAD.\\hline\\hline\n",theta.eta[1,"run"],sep="");
            tmp.n <- length(theta.eta[1,])-2;
            if (all(pr$assoc[5,] == "EXP")){
                tmp.theta.n <- which(names(theta.eta) == "eta.cv") - which(names(theta.eta) == "theta");
            } else {
                tmp.theta.n <- which(names(theta.eta) == "eta.se") - which(names(theta.eta) == "theta");
            }
            tmp <- theta.eta[1,];
            tmp[1,] <- "";
            tmp$e <- -1;
            tmp[,"theta"] <- paste("\\multicolumn{",tmp.theta.n,"}{||l||}{\\begin{verbatim}",
                                   paste(pr$msg,collapse="\n")
                                   ,"\\end{verbatim}}",sep="");
            tmp[,"run"] <- "\\hline";
            tmp.w <- which(names(tmp) == "theta");
            tmp[,seq(tmp.w+1,tmp.w+tmp.n-1)] <- "-AND-RM-";
            tmp[,ifelse(all(pr$assoc[5,] == "EXP"),"eta.cv","eta.se")] <-
                paste("\\multicolumn{",(tmp.n - tmp.theta.n),"}{||l||}{\\textbf{Data:}\n\\begin{itemize}",
                      "\n\\item Observations: ",pr$n.obs,
                      "\n\\item Individuals: ",pr$n.ind,
                      "\\end{itemize}",
                      "}",sep="");
            theta.eta <- rbind(theta.eta,tmp);
        }
        if (any(theta.eta$e == "")){
            x <- unique(c(which(theta.eta$e != ""),length(theta.eta$e)+1));
            x <- x[-1]-x[-length(x)];
            vars <- c(ifelse(all(pr$assoc[5,] == "EXP"),"eta.cv","eta.se"),"eta.rse","eta.shrink","eta.delta");
            vars <- vars[vars %in% names(theta.eta)];
            if (any(x > 1)){
                for (var in vars){
                    theta.eta[theta.eta$e != "",var] <- paste(ifelse(x <= 1,"",
                                                                     paste("{}rowspan",x," ",sep="")),
                                                              theta.eta[theta.eta$e != "",var],sep="");
                }
            }
        }
        return(theta.eta);
    }
    plt <- any(regexpr("ALL",ls(envir=globalenv())) >= 0);
    if (is.null(run.summary)){
        theta.tab.ref <- NULL;
        eta.tab.ref <- NULL;
        pr.ref <- NULL;
        theta.eta <- get.theta.eta(xpdb,pr);
        if (plt){
            cur.caption <- paste("Parameter estimate summary table (",FILEID,")",sep="");
        } else {
            cur.caption <- paste("Parameter estimate summary table ",gsub("_","\\\\textunderscore ",xpdb@Runno),".",sep="");
        }
    } else {
        if (cmp.table){
            if (plt) {
                cur.caption <- paste("Parameter estimate summary table (",FILEID,")",sep="");
            } else {
                cur.caption <- paste("Comparison of runs to ",gsub("_","\\\\textunderscore ",xpdb@Runno),".",sep="");
            }
            theta.tab.ref <- theta.table(xpdb,pr);
            eta.tab.ref <- eta.table(xpdb,pr);
            pr.ref <- pr;
        } else {
            cur.caption <- "Run Summary";
            theta.tab.ref <- NULL;
            eta.tab.ref <- NULL;
            pr.ref <- NULL;
        }
        theta.eta <- NULL;
        for (var in run.summary){
            xp <- xpcmp[[var]];
            if (!is.null(xp)){
                tmp <- get.theta.eta(xp);
                if (is.null(theta.eta)){
                    theta.eta <- tmp;
                } else {
                    theta.eta <- rbind(theta.eta,tmp);
                }
            }

        }

        ci.col <- TRUE;
        shrinkage <- TRUE;
    }
    tmp <- theta.eta[1,];
    tmp <- gsub("theta","",
                gsub("run","",
                     gsub("est","\\\\textbf{Population Mean}",
                          gsub("^delta$","\\\\textbf{$\\\\Delta$ Mean}",
                               gsub("^eta\\.delta$","\\\\textbf{$\\\\Delta$ \\\\%CV}",
                                    gsub("rse","\\\\textbf{\\\\%RSE}",
                                         gsub("ci",paste("\\\\textbf{",round(pr$ci*100,2),"\\\\% CI}",sep=""),
                                              gsub("eta.cv","\\\\textbf{\\\\%CV}",
                                                   gsub("eta.se","\\\\textbf{SE}",
                                                        gsub("eta.rse","\\\\textbf{\\\\%RSE}",
                                                             gsub("eta.shrink","\\\\textbf{\\\\%Shrink}",
                                                                  names(theta.eta))))))))))));
    theta.eta <- rbind(tmp,theta.eta);
    if (ci.col){
        fpe <- "\\\\multicolumn{3}{||c||}{\\\\textbf{Final Parameter Estimate}}";
        if (!is.null(run.summary)){
            cs <- "||c||c||ccc||";
        } else {
            cs <- "||c||ccc||"
        }
    } else {
        fpe <- "\\\\multicolumn{2}{||c||}{\\\\textbf{Final Parameter Estimate}}";
        if (!is.null(run.summary)){
            cs <- "||c||c||cc||";
        } else {
            cs <- "||c||cc||";
        }
    }
    if (shrinkage){
        bsv <- "\\\\multicolumn{3}{||c||}{\\\\textbf{Between Subject Variability}}";
        cs <- paste(cs,"ccc||",sep="");
    } else {
        bsv <- "\\\\multicolumn{2}{||c||}{\\\\textbf{Between Subject Variability}}";
        cs <- paste(cs,"cc||",sep="");
    }
    names(theta.eta) <- gsub("run",  "{}rowspan2 \\\\textbf{Run}",
                             gsub("theta","{}rowspan2 \\\\textbf{Parameter}",
                                  gsub("est",fpe,
                                       gsub("rse"," ",
                                            gsub("ci","~",
                                                 gsub("^delta$","~",
                                                      gsub(ifelse(all(pr$assoc[5,] == "EXP"),"eta.cv","eta.se"),bsv,
                                                           gsub("eta.rse","  ",
                                                                gsub("eta.shrink","~~",
                                                                     gsub("^eta.delta$","~~",
                                                                          names(theta.eta)))))))))));

    theta.eta <- theta.eta[,names(theta.eta) != "e"];
    n <- length(theta.eta);
    tmp <- latex.table(theta.eta,caption=cur.caption,
                       label=paste("overview.table",ifelse(plt,gsub("-",".",FILEID),gsub("[ _\\\\\\/\\-]",".",xpdb@Runno) ) ,sep="") );
    tmp <- sub("\\\\+begin\\{longtable\\}\\{c+\\}",paste("\\\\begin{longtable}{",cs,"}",sep=""),tmp);
    tmp <- sub("\\\\+end\\{longtable\\}","",tmp);
    if (!is.null(run.summary)){
        ret <- gsub("&-AND-RM-","",paste(tmp,"\\hline\n\\end{longtable}\n%begin.table.footnote.",n,"\n",
                                         sep=""));
        if (regexpr(".ifn1.",ret) != -1){
            ret <- paste(ret,
                         "$^{.ifn1.}$ ",round(pr$ci*100,2),"\\% Confidence Interval Contains zero\n",sep="");
        }
        if (regexpr(".ifn2.",ret) != -1){
            ret <- paste(ret,
                         "$^{.ifn2.}$ Mean of $\\eta$ nonzero (p-value < ",p.value,")\n",sep="");
        }
        ret <- paste(ret,"%end.table.footnote.",n,"\n",sep="");
        ret <- number.footnotes(ret);
    } else {
        tmp <- paste(tmp,"\\hline\n\\end{longtable}\n%begin.table.footnote.",n,"\n",
                     "Minimum value of the objective function: ",pr$mvof,"\n",
                     ifelse(is.null(pr$shrinkage["EPS"]) | is.na(pr$shrinkage["EPS"]),"",paste("Residual Shrinkage: ",nm.round(pr$shrinkage["EPS"],dig=2),"\\%\n",sep="")),
                     ifelse(is.null(pr$eigen.cn),"",ifelse(length(pr$eigen.cn) != 1,"",paste("Condition Number: ", nm.round(pr$eigen.cn,dig=3),"\n",sep=""))),
                     sep=""
                     );
        if (regexpr(".ifn1.",tmp) != -1){
            tmp <- paste(tmp,
                         "$^{.ifn1.}$ ",round(pr$ci*100,2),"\\% Confidence Interval Contains zero\n",sep="");
        }
        if (regexpr(".ifn2.",tmp) != -1){
            tmp <- paste(tmp,
                         "$^{.ifn2.}$ Mean of $\\eta$ nonzero (p-value < ",p.value,")\n",sep="");
        }
        tmp <- paste(tmp,"%end.table.footnote.",n,"\n",sep="");
        ret <- number.footnotes(tmp);
        r <- pr$cor.val;
        if (!is.null(r)){
            r <- round(r*r,3);
            r.dat <- NULL;
            ## Get correlated values.
            for (i in row.names(r)[-dim(r)[1]]){
                w.i <- seq(which(row.names(r) == i)+1,dim(r)[1]);
                if (any(r[i,w.i] >= r2,na.rm=TRUE)){
                    tmp <- data.frame(v1=i,v2=row.names(r)[which(r[i,w.i] >= r2)+w.i[1]-1]);
                    if (is.null(r.dat)){
                        r.dat <- tmp;
                    } else {
                        r.dat <- rbind(tmp,r.dat);
                    }
                }
            }
            if (!is.null(r.dat)){
                tmp.f1 <- function(x){
                    return(pr$cor.val[paste(x[1]),paste(x[2])]);
                }
                r.dat$r <- sapply(data.frame(t(r.dat)),tmp.f1);
                tmp.f2 <- function(x){
                    return(r[paste(x[1]),paste(x[2])]);
                }
                r.dat$r2 <- sapply(data.frame(t(r.dat)),tmp.f2);
                r.dat <- r.dat[order(r.dat$r2,decreasing=TRUE),];
                r.dat$v1 <- paste(r.dat$v1);
                r.dat$v2 <- paste(r.dat$v2);
                tmp <- theta.tab[,names(theta.tab) %in% c("theta","t")];
                tmp$v1 <- paste("TH",theta.tab$t,sep="");
                r.dat <- merge(r.dat,tmp,all.x=TRUE);
                r.dat$v1[!is.na(r.dat$theta)] <- paste(r.dat$theta[!is.na(r.dat$theta)]);
                r.dat <- r.dat[,names(r.dat) %in% c("v1","v2","r","r2")];
                tmp <- theta.tab[,names(theta.tab) %in% c("theta","t")];
                tmp$v2 <- paste("TH",theta.tab$t,sep="");
                r.dat <- merge(r.dat,tmp,all.x=TRUE);
                r.dat$v2[!is.na(r.dat$theta)] <- paste(r.dat$theta[!is.na(r.dat$theta)]);
                r.dat <- r.dat[,names(r.dat) %in% c("v1","v2","r","r2")];

                ## Put Omega labels on.
                tmp <- data.frame(v1=rep(seq(1,length(eta.tab[,1])),each=length(eta.tab[,1])),
                                  v2=rep(seq(1,length(eta.tab[,1])),times=length(eta.tab[,1])));
                tmp <- tmp[tmp$v1 <= tmp$v2,];
                tmp1 <- data.frame(v1=seq(1,length(eta.tab[,1])),t1=eta.tab$eta.);
                tmp2 <- data.frame(v2=seq(1,length(eta.tab[,1])),t2=eta.tab$eta.);
                tmp <- merge(tmp,tmp1,all.x=TRUE);
                tmp <- merge(tmp,tmp2,all.x=TRUE);
                tmp$lab <- paste("Cov(",tmp$t1,",",tmp$t2,")",sep="");
                tmp$lab[tmp$v1 == tmp$v2] <- paste(tmp$t1[tmp$v1 == tmp$v2]);
                tmp$var <- paste("OM",tmp$v1,tmp$v2,sep="");
                tmp1 <- tmp[,names(tmp) %in% c("var","lab")];
                tmp <- tmp1;
                names(tmp) <- gsub("var","v1",names(tmp));
                r.dat <- merge(r.dat,tmp,all.x=TRUE);
                r.dat$v1[!is.na(r.dat$lab)] <- r.dat$lab[!is.na(r.dat$lab)];
                r.dat <- r.dat[,names(r.dat) %in% c("v1","v2","r","r2")];

                tmp <- tmp1;
                names(tmp) <- gsub("var","v2",names(tmp));
                r.dat <- merge(r.dat,tmp,all.x=TRUE);
                r.dat$v2[!is.na(r.dat$lab)] <- r.dat$lab[!is.na(r.dat$lab)];
                r.dat <- r.dat[,names(r.dat) %in% c("v1","v2","r","r2")];
                r.dat <- r.dat[,c("v1","v2","r","r2")];
                ## To Do:  Put Sigma labels on.
                names(r.dat) <- gsub("v1","\\\\textbf{Parameter 1}",
                                     gsub("v2","\\\\textbf{Parameter 2}",
                                          gsub("r","\\\\textbf{r}",
                                               gsub("r2","\\\\textbf{r$^{2}$}",
                                                    names(r.dat)))));
                tmp <- latex.table(r.dat,
                                   caption=paste("Highly Correlated Parameters (r$^2$ > ",r2,") for ",
                                   ifelse(plt,paste("Run",FILEID),gsub("_","\\\\textunderscore ",xpdb@Runno))
                                   ,".",sep=""),
                                   label=paste("corr.table",
                                   ifelse(plt,gsub("-",".",FILEID),gsub("[ _\\\\\\/\\-]",".",xpdb@Runno))
                                   ,sep="")
                                   );
                tmp <- sub("cccc","cc||cc",tmp);
                ret <- paste(ret,"\n\n",tmp,sep="");
            }
        }
    }
    return(ret);
    ## Get THETAs unassociated with ETAs or EPS.
                                        #only.theta <- as.matrix(assoc[,sapply(assoc[3,],length)==0]);
                                        #only.theta <- as.matrix(only.theta[,sapply(only.theta[4,],length)==0]);
                                        #only.theta <- as.matrix(only.theta[,sapply(only.theta[1,],length) == 1]);
}

nm.round <- function(x,sigdig=NULL,dig=NULL,na.footnote="",single=FALSE){
    ## Rounding function.
    if (length(x) > 1 ) {
        return(sapply(x,nm.round,sigdig=sigdig,dig=dig,na.footnote=na.footnote));
    } else {
        if(is.null(x)){
            r1 <- "NA";
            r1 <- gsub(" *NA *",paste("%.GREYBG.\nNA$^{",na.footnote,"}$",sep=""),r1);
            return(r1);
        }
        if (is.null(sigdig) && is.null(dig)){
            stop("Either you must specify sigdig or dig.")
        } else if (is.null(dig)){
                                        # Significant figures.
            if (is.na(x)){
                r1 <- "NA";
            } else {
                r1 <- formatC(x,digits=sigdig,format="g",flag="#");
                r1 <- gsub("e\\+0([0-9])","$\\\\\\\\cdot$$10^{\\1}$",r1);
                r1 <- gsub("e\\+([1-9][0-9])","$\\\\\\\\cdot$$10^{\\1}$",r1);
                r1 <- gsub("e\\-0([0-9])","$\\\\\\\\cdot$$10^{-\\1}$",r1);
                r1 <- gsub("e\\-([1-9][0-9])","$\\\\\\\\cdot$$10^{-\\1}$",r1);
            }
            if (na.footnote != ""){
                r1 <- gsub(" *NA *",paste("%.GREYBG.\nNA$^{",na.footnote,"}$",sep=""),r1);
            }
            r1 <- gsub("^-","$-$",r1);
            if (single){
                r1 <-  gsub("\\\\\\\\","\\\\",r1);
            }
            return(r1);
        } else {
            if (is.na(x)){
                r1 <- "NA";
                if (na.footnote != ""){
                    r1 <- gsub(" *NA *",paste("%.GREYBG.\nNA$^{",na.footnote,"}$",sep=""),r1);
                }
            } else {
                r1 <- sprintf(x,fmt=paste("%.",dig,"f",sep=""));
                r1 <- gsub("^-","$-$",r1);
            }
            if (single){
                r1 <-  gsub("\\\\\\\\","\\\\",r1);
            }
            return(r1);
        }}
}

latexFix <-  function(tex){

    tex <- tex[regexpr("^%",tex,perl=TRUE) == -1];
    lab <-  tex[regexpr("label",tex,perl=TRUE) != -1];
    lab <-  gsub("{","{tab:",lab,perl=TRUE);
    tex <-  tex[regexpr("(setlongtables|endfoot|label)",tex,perl=TRUE) == -1];
    caption <-  which(regexpr("(\\\\caption{[^}]*)}",tex,perl=TRUE) != -1)[1];
    align <- gsub("^.*{longtable}{([^}]*)}.*$","\\1",tex[caption],perl=TRUE);
    endhead <-  which(regexpr("endhead",tex) != -1)[1];
    tex[1:endhead] <- gsub("multicolumn\\{1\\}\\{c\\}\\{(.*?)\\}","textbf{\\1}",tex[1:endhead],perl=TRUE);
    new.align <-  gsub("[rl]","c",align);
    tex[caption] <-  gsub(paste("({longtable}{)",align,"}",sep=""),paste("\\1",new.align,"}",sep=""),tex[caption],perl=TRUE);
    n <- nchar(gsub("\\|","",align,perl=TRUE));
    spancol <-  paste("%spancol=",n,sep="");
    ##Length of spancol.
    tex[caption] <-  paste(gsub("(\\\\caption{[^}]*)}",paste("\n\\1",gsub("\\\\","\\\\\\\\",lab,perl=TRUE),"}",sep=""),tex[caption],perl=TRUE),spancol,sep="");
    tex <- tex[-seq(which(regexpr("endfirsthead",tex) != -1)[1],which(regexpr("endhead",tex) != -1)[1]-1)];
    tex <-  strsplit(paste(tex,collapse="\n"),"\n")[[1]];
    tex <- gsub("([^\\$])\\^([0-9]+)","\\1$^{\\2}$",tex);
    tex <-  gsub("_([^{])"," \\1",tex);
    tex <- gsub("\\\\","\\\\\\\\",tex);
    tex <- gsub("\\\\+cdot","\\\\\\\\cdot",tex);
    return(tex);
}

number.footnotes <- function(x){
    ret <- x;
    fn.nums <- strsplit("abcdefghijklmnopqrstuvwxyz",split="")[[1]];
                                        # Footnotes are a b c d .. x y z aa bb cc ... xx yy zz aaa ...
    cur.num <- 0;
    tmp.fn <- function(x,xtrafn="i"){
        return(paste("ret <- strsplit(ret,split=paste(\"[.]",xtrafn,"fn",x,"[.]\"),perl=TRUE)[[1]];",
                     "if (length(ret) != 1) {sep=rep(fn.nums[cur.num%%length(fn.nums)+1],times=ceiling((cur.num+0.5)/length(fn.nums)));",
                     "cur.num <- cur.num+1;",
                     "ret <- paste(ret,collapse=fn.nums[cur.num]);}",sep=""));
    }
    tmp <- gsub("^([0-9]+)[.].*$","\\1",strsplit(ret,"[.]ifn",perl=TRUE)[[1]]);
    if (length(tmp) > 1) {
        tmp <- as.double(tmp[grep("^[0-9]+$",tmp)]);
        tmp <- tmp[!is.na(tmp)];
        if (length(tmp) == 0){
        } else {
            tmp <- max(tmp,na.rm=TRUE);
            eval(parse(text=sapply(seq(1,tmp),tmp.fn)))
        }
    }
    tmp <- gsub("^([0-9]+)[.].*$","\\1",strsplit(ret,"[.]fn",perl=TRUE)[[1]]);
    if (length(tmp) > 1) {
        tmp <- as.double(tmp[grep("^[0-9]+$",tmp)]);
        tmp <- tmp[!is.na(tmp)];
        if (length(tmp) == 0){
        } else {
            tmp <- max(tmp,na.rm=TRUE);
            eval(parse(text=sapply(seq(1,tmp),tmp.fn,xtrafn="")))
        }
    }
    return(ret);
}


levenshtein <- function(string1, string2, case=FALSE, damerau=FALSE, map=NULL) {
########
    ##
    ## levenshtein algorithm in R
    ##
    ## Author  : Hans-Joerg Bibiko
    ## Date    : 09/07/2006
    ##
    ## Contact : bibiko@eva.mpg.de
    ##
########
    ##
    ## string1, string2 := strings to compare
    ##
    ## case = TRUE := case sensitivity; case = FALSE := case insensitivity
    ##
    ## damerau = TRUE := Damerau enhancement: "al" to "la" costs only 1 instead of 2
    ##
    ## map := character vector of c(regexp1, replacement1, regexp2, replacement2, ...)
    ##
    ##   example:
    ##      map <- c("[aeiou]","V","[^aeiou]","C") := replaces all vowels with V and all others with C
    ##
    ##      levenshtein("Bank","Bond", map=map)   =>  0
    ##
########


    if(!is.null(map)) {
        m <- matrix(map, ncol=2, byrow=TRUE)
        s <- c(ifelse(case, string1, tolower(string1)), ifelse(case, string2, tolower(string2)))
        for(i in 1:dim(m)[1]) s <- gsub(m[i,1], m[i,2], s)
        string1 <- s[1]
        string2 <- s[2]
    }

    if(ifelse(case, string1, tolower(string1)) == ifelse(case, string2, tolower(string2))) return(0)

    s1 <- strsplit(paste(" ", ifelse(case, string1, tolower(string1)), sep=""), NULL)[[1]]
    s2 <- strsplit(paste(" ", ifelse(case, string2, tolower(string2)), sep=""), NULL)[[1]]

    l1 <- length(s1)
    l2 <- length(s2)

    d <- matrix(nrow = l1, ncol = l2)

    for(i in 1:l1) d[i,1] <- i-1
    for(i in 1:l2) d[1,i] <- i-1
    for(i in 2:l1) for(j in 2:l2) {
        d[i,j] <- min((d[i-1,j]+1) , (d[i,j-1]+1) , (d[i-1,j-1]+ifelse(s1[i] == s2[j], 0, 1)))
        if(damerau && i>1 && j>1 && s1[i]==s2[j-1] && s1[i-1]==s2[j])
            d[i,j] <- min(d[i,j] , d[i-2,j-2]+ifelse(s1[i] == s2[j], 0, 1))
    }

    d[l1,l2]
}


compare.lines <- function(x1,x2,theta=NULL,eta=NULL){
    ## Get the "closest" match
    x.cmp <- standardize.lines(x1);
    x.ref <- unlist(lapply(x2,standardize.lines))
    vars.defined <- strsplit(x.cmp,"[=\n]")[[1]];
    vars.defined <- paste("^(",paste(vars.defined[1:length(vars.defined) %%2 == 1],collapse="|"),")$",sep="");
    var.cmp <- strsplit(x.cmp,"[+=\n]")[[1]];
    if (!is.null(theta)){
        var.cmp <- var.cmp[regexpr(paste("\\bTHETA\\(",theta,"\\)",sep=""),var.cmp) != -1];
    }
    if (!is.null(eta)){
        var.cmp <- var.cmp[regexpr(paste("\\bETA\\(",eta,"\\)",sep=""),var.cmp) != -1];

    }
    if (length(var.cmp) == 0 ){
        return(NA);
    }
    tmp.f <- function(x){
        return(levenshtein(var.cmp,x));
    }
    last.d <- Inf;
    var.to <- NA;
    for (i in 1:length(x.ref)){
        var <- strsplit(x.ref[i],"[+=\n]")[[1]];
        if (any(regexpr(vars.defined,var) != -1)){
            ## Only compare variables that are equivalent.
            if (!is.null(theta)){
                var <- var[regexpr("\\bTHETA\\([0-9]+\\)",var) != -1];
            }
            if (!is.null(eta)){
                var <- var[regexpr("\\bETA\\([0-9]+\\)",var) != -1];
            }
            d <- sapply(var,tmp.f);
            ## Get the variable with the smallest Levenshtein distance.
            if (length(d) != 0) {
                if (min(d) < last.d){
                    var.to <- which(d == min(d));
                    if (!is.null(theta)){
                        var.to <- as.numeric(gsub(".*THETA\\(([0-9]+)\\).*","\\1",var[var.to]))
                    }
                    if (!is.null(eta)){
                        var.to <- as.numeric(gsub(".*\\bETA\\(([0-9]+)\\).*","\\1",var[var.to]));
                    }
                }
            }
        }
    }
    return(var.to);
}


errors.latex <- function(xpdb,pr=NULL){
    return(paste("\\\\begin{itemize}\n\\\\item",paste(pr$errors,collapse="\n\\\\item "),"\n\\\\end{itemize}"));
}

munge.tex <- function(ret){
    ## Remove NORTF sections.
    ret=gsub("\n *% *BEGIN *NORTF *\n(?:.|\n)*?\n *% *END *NORTF *\n","\n",ret,perl=TRUE);
    ## Change \tabularnewline to  \\
    ret=gsub("\\\\tabularnewline","\\\\\\\\",ret);
    ret=gsub("\\\\endfoot\n+","",ret,perl=TRUE);
                                        #Fix the subscripts.
    ret=gsub("\\$([^\\\\][^\n_\\$]*?)_{([^}]+?)}\\$","\\1.SUB.(\\2).SUB.",ret,perl=TRUE)
    ret=gsub("\\$([^\\\\][^\\n)_\\$]*?)_([^\n_\\$])\\$","\\1.SUB.(\\2).SUB.",ret,perl=TRUE);
    ret=gsub("\\$(\\\\[^\n_\\$]*?)_{([^}\\$\n]+?)}\\$","\\$\\1\\$.SUB.(\\2).SUB.",ret,perl=TRUE);
    ret=gsub("\\$\\_{([^}]+?)}\\$",".SUB.(\\1).SUB.",ret,perl=TRUE);
    ret=gsub("\\$\\_([^\n_\\$])\\$",".SUB.(\\1).SUB.",ret,perl=TRUE);
                                        #Fix the superscripts
    ret=gsub("\\$([^\\\\][^\n^\\$]*?)\\^{([^}]+?)}\\$","\\1.SUP.(\\2).SUP.",ret,perl=TRUE);
    ret=gsub("\\$([^\\\\][^\n)%\\$]*?)\\^([^\n^\\$])\\$","\\1.SUP.(\\2).SUP.",ret,perl=TRUE);
    ret=gsub("\\$(\\\\[^\n^\\$]*?)\\^{([^}\\$\n]+?)}\\$","\\$\\1\\$.SUP.(\\2).SUP.",ret,perl=TRUE);
    ret=gsub("\\$\\^{([^}]+?)}\\$",".SUP.(\\1).SUP.",ret,perl=TRUE);
    ret=gsub("\\$\\^([^\n^\\$])\\$",".SUP.(\\1).SUP.",ret,perl=TRUE);

    ## Fix strikeout
    ret=gsub("\\\\sout\\{","{.STRIKE.",ret,perl=TRUE);

                                        # Fix Begin and end LANSCAPE
    ret=gsub("\\\\begin{landscape}",".BEGIN.LANDSCAPE.",ret,perl=TRUE);
    ret=gsub("\\\\end{landscape}",".END.LANDSCAPE.",ret,perl=TRUE);

                                        #Assume each long-table has a caption, and label inside the table.
    ret=gsub("\\\\ref{tab:([^\\n}]*?)}","{}-tab:\\1-{}",ret,perl=TRUE);
    ret=gsub("\\\\ref{fig:([^\\n}]*?)}","{}-fig:\\1-{}",ret,perl=TRUE);
    ret=gsub("\\\\clearpage",".CLEARPAGE.",ret,perl=TRUE);
    ##  ret=strsplit(ret,"\\\\begin{longtable}",perl=TRUE)[[1]];
    ret <-  strsplit(paste(ret,collapse="\n"),"\\\\begin{longtable}",perl=TRUE);
    ret <-  ret[[1]];
    fin <- "";
    tmp.f <- function(row){
        rowcnt=row;
                                        # Fix caption to be multicolumn.
        if (regexpr("\\\\caption",rowcnt)!=-1){
            num <- gsub("\\{([clrp|]*)\\}(.|\n)*","\\1",rowcnt);
            num <- nchar(gsub("[^clrp]","",num,perl=TRUE));
            rowcnt <- gsub("\\\\caption{(.*?)\\\\label{(tab|fig):(.*?)}(.*?)}",paste("\\\\multicolumn{",num,"}{l}{.CAPTION.Table \\2:\\3: \\1\\4}",sep=""),rowcnt,perl=TRUE);
        }
        rowcnt <- strsplit(rowcnt,"%begin.table.footnote.",perl=TRUE)[[1]];
        if (length(rowcnt) > 1){
            num <- gsub("^([0-9]+).*","\\1",rowcnt[2]);
            rowcnt[2] <- gsub("^([0-9]+)","",rowcnt[2]);
            tmp <- strsplit(rowcnt[2],paste("%end.table.footnote.",num,sep=""))[[1]];
            extra <- tmp[2];
            rowcnt[2] <- tmp[1];
            lines <- strsplit(rowcnt[2],"\n[\n ]*")[[1]];
            rowcnt[2] <- paste(paste(paste("\n\\multicolumn{",num,"}{l}{",sep=""),lines,".KEEPN..FOOTNOTE.}\\\\\n",sep=""),collapse="");
            tmp <- rowcnt[2]
            rowcnt <- rowcnt;
            rowcnt <- strsplit(rowcnt,"\\\\end{longtable}",perl=TRUE)[[1]];
            rowcnt[1] <- paste(rowcnt[1],tmp);
            rowcnt <- gsub("%.?end.table.footnote.[0-9]+","",rowcnt,perl=TRUE);
            rowcnt <- gsub("\\\\multicolumn{[0-9]+}{l}{( *| *(\\.KEEPN\\.)?\\.FOOTNOTE\\. *)}\\\\\\\\\n+","",rowcnt,perl=TRUE);
            rowcnt <- paste(rowcnt,collapse="\\end{longtable}");
            rowcnt <- paste(rowcnt,extra,sep="");
        }
        return(rowcnt);
    }
    ret <- paste(sapply(ret,tmp.f),collapse="\\begin{tabular}");
    ret <-  gsub("end\\{longtable\\}","end\\{tabular\\}",ret,);
    ret=gsub("\n\n+\\.CLEARPAGE\\.\n\n+","\n.CLEARPAGE.\n",ret,perl=TRUE);
    ret <- gsub("Table fig","Figure fig",ret,perl=TRUE);
                                        #    ret <- gsub("\\\\\\\\(.*?)\n(.*?)\\\\endhead",".ENDHEAD.\\\\\\\\\\1\n\\2",ret,perl=TRUE);
    ret <- gsub("(\\\\(?:sub)*section{)","\\1.SECTION.",ret,perl=TRUE);
    ret <- strsplit(ret,"\n")[[1]];
    graph.which <- which(regexpr("includegraphics\\{[^%]*%03d\\}",ret) != -1);
    ## Put in figures.
    for (i in graph.which){
        tmp <- ret[i];
        file <- gsub(".*includegraphics\\{([^%]*%[^d]*d)\\}.*","\\1.png",tmp);
        files.matching <- Sys.glob(gsub("%[^d]*d","*",file));
        files.matching <- files.matching[regexpr(gsub("%[^d]*d","[0-9]+",file),files.matching) != -1];
        j <-  0;
        while (i <= 60*3 & length(files.matching) == 0) {
            Sys.sleep(1);
            files.matching <- Sys.glob(gsub("%[^d]*d","*",file));
            files.matching <- files.matching[regexpr(gsub("%[^d]*d","[0-9]+",file),files.matching) != -1];
            if (i == 0){
                cat("Waiting for figures (",file,") to shop up (at most 3 minutes)");
            } else {
                cat(".");
            }
            j <-  j+1;
        }
        if (j > 0){
            cat("\n");
        }
        tmp <- paste("\\includegraphics{",paste(files.matching,collapse="}\\\\\n\\includegraphics{"),"}\\\\\n",sep="");
        ret[i] <- tmp;
    }
    ret <- paste(ret,collapse="\n");
                                        #Fix hidden RTF type commands.
    ret <- gsub("%(\\..+?\\.)\n","\\1\n",ret,perl=TRUE);
    ret <- gsub("$$","",ret,fixed=TRUE);
    return(ret);
}

munge.rtf <- function(rtf,style.ref=1,use.macro=FALSE,...){
    if (use.macro == FALSE){
        style.ref = 0;
    }
    ret <- rtf;
    ##Add light grey for shading to color table
    ret=gsub("^\\\\red192\\\\green192\\\\blue192;","\\\\red192\\\\green192\\\\blue192;\n\\\\red217\\\\green217\\\\blue217;",ret);
    ## Add some word based fields for characters.
    ##108 is lambda
    ret=gsub("[{]\\\\f6\\\\u-3988\\\\'6[cC][}]","{\\\\field{\\\\*\\\\fldinst SYMBOL 108 \\\\\\\\f \"Symbol\"}}",ret);
    ## 45 is dash.
    ret=gsub("[{]\\\\f6\\\\u-4051\\\\'2[dD][}]","-",ret);
    ##165 is infitiy.  16 size since a subscript.q
    ret=gsub("[{]\\\\f6\\\\u-3931\\\\'[Aa]5[}]","{\\\\field{\\\\*\\\\fldinst SYMBOL 165 \\\\\\\\f \"Symbol\"}}",ret);
    ##183 is large cdot. 180 is times. 165 normal cdot?
    ret=gsub("[{]\\\\f6\\\\u-3881\\\\'D7[}]","{\\\\field{\\\\*\\\\fldinst SYMBOL 180 \\\\\\\\f \"Symbol\"}}",ret);
    ##177 is plus-minus
    ret=gsub("[{]\\\\f6\\\\'B1[}]","{\\\\field{\\\\*\\\\fldinst SYMBOL 177 \\\\\\\\f \"Symbol\"}}",ret);
    ## Change the properites to add a header row. by adding trhdr to the headings.
    ## Change the first row defintion to be font size 9.
    ret=gsub("\\\\trowd(.*?\\n.*?)(Table|Figure)","\\\\fs18\\\\trowd\\1\\2",ret);
    ## Switch orientation to landscape.
    ret=gsub("\\.BEGIN\\.LANDSCAPE\\.","\\\\sect\\\\sectd\\\\lndscpsxn\\\\pgwsxn15840\\\\pghsxn12240\\\\marglsxn1080\\\\margtsxn2160\\\\margbsxn1440\\\\margtsxn1440\\\\sectdefaultcl",ret);
    ret=gsub("\\.END\\.LANDSCAPE\\.","\\\\sect\\\\sectd\\\\pghsxn15840\\\\pgwsxn12240\\\\margbsxn1080\\\\marglsxn2160\\\\margrsxn1440\\\\marglsxn1440\\\\sectdefaultcl",ret);
    ## Change Table tab:ref to table that is numbered by word.
    if (style.ref == 0){
        ret=gsub("Table tab:([^\n:]*):","Table {\\\\*\\\\bkmkstart _Ref_tab_\\1}{\\\\field\\\\flddirty{\\\\*\\\\fldinst SEQ Table \\\\\\\\* ARABIC}}{\\\\*\\\\bkmkend _Ref_tab_\\1}:",ret);
    } else {
        ret=gsub("Table tab:([^\n:]*):","Table {\\\\*\\\\bkmkstart _Ref_tab_\\1}{\\\\field\\\\flddirty{\\\\*\\\\fldinst STYLEREF -STYLEREF- \\\\\\\\s }}.{\\\\field\\\\flddirty{\\\\*\\\\fldinst SEQ Table \\\\\\\\* ARABIC \\\\\\\\s -STYLEREF-}}{\\\\*\\\\bkmkend _Ref_tab_\\1}:",ret);
    }
    ret=gsub("-tab:([^\n-]*)-","{\\\\field\\\\flddirty{\\\\*\\\\fldinst { REF _Ref_tab_\\1 \\\\\\\\h }{ \\\\\\\\* MERGEFORMAT }}{\\\\fldrslt \\\\fs20 tab:\\1}}",ret);
    ## Change Figure fig:ref to figure that is numbered by word.
    if (style.ref == 0){
        ret=gsub("Figure fig:([^:\n]*):","Figure {\\\\*\\\\bkmkstart _Ref_fig_\\1}{\\\\field\\\\flddirty{\\\\*\\\\fldinst SEQ Figure \\\\\\\\* ARABIC}}{\\\\*\\\\bkmkend _Ref_fig_\\1}:",ret);
    } else {
        ret=gsub("Figure fig:([^:\n]*):","Figure {\\\\*\\\\bkmkstart _Ref_fig_\\1}{\\\\field\\\\flddirty{\\\\*\\\\fldinst STYLEREF -STYLEREF- \\\\\\\\s }}.{\\\\field\\\\flddirty{\\\\*\\\\fldinst SEQ Figure \\\\\\\\* ARABIC \\\\\\\\s -STYLEREF-}}{\\\\*\\\\bkmkend _Ref_fig_\\1}:",ret);
    }
    ret <- gsub("-STYLEREF-",style.ref,ret);
    ret=gsub("-fig:([^\n-]*)-","{\\\\field\\\\flddirty{\\\\*\\\\fldinst { REF _Ref_fig_\\1 \\\\\\\\h }{ \\\\\\\\* MERGEFORMAT }}{\\\\fldrslt \\\\fs20 fig:\\1}}",ret)
    ret=gsub("_Ref_(tab|fig)([^ \n.}]+)","_Ref_\\1\\2_",ret);
    ## Add table of contents, figures, and tables.
    if (use.macro){
        info <- which(regexpr("[{]\\\\info",ret)!= -1);
        ret[info] <- paste(ret[info],"\n{\\template esn-xpose-summary.dot}",sep="");
    }
    toc <- which(regexpr("fldinst TOC",ret)!=-1);
    ## TOC shouldn't be numbered (if word is going to number it)
                                        #  ret[1:(toc-1)] <- gsub("\\\\+s[0-9]\\b","",ret[1:(toc-1)],perl=TRUE);
    ret[toc]=gsub("[{]\\\\field[{]\\\\\\*\\\\fldinst TOC \\\\\\\\o \"1-3\" [}][{]\\\\fldrslt [}][}]","{\\\\field{\\\\*\\\\fldinst TOC \\\\\\\\h \\\\\\\\o \"1-3\" }{\\\\fldrslt }}\\\\pard\\\\qj\\\\sl240\\\\slmult1 \\\\fi0 {\\\\ql\\\\sb240\\\\sa60\\\\keepn\\\\f14\\\\b\\\\fs40\\\\li0 Figures\\\\par}{\\\\field{\\\\*\\\\fldinst TOC \\\\\\\\h \\\\\\\\c \"Figure\" }{\\\\fldrslt }}\\\\pard\\\\qj\\\\sl240\\\\slmult1 \\\\fi0 {\\\\ql\\\\sb240\\\\sa60\\\\keepn\\\\f14\\\\fs40\\\\li0\\\\b Tables\\\\par}{\\\\field{\\\\*\\\\fldinst TOC \\\\\\\\h \\\\\\\\c \"Table\" }{\\\\fldrslt }}",ret[toc]);
    ret[toc+1]=gsub("\\\\page","",ret[toc+1]);

    ret=gsub("[{]\\\\footer[^\n}]*[}]","{}",ret);       # Take out header and footer.

                                        # Change margins
    ret=gsub("\\\\margl[0-9]+","\\\\margl2160",ret);
    ret=gsub("\\\\margr[0-9]+","\\\\margr1440",ret);
    ret=gsub("\\\\margt[0-9]+","\\\\margt1440",ret);
    ret=gsub("\\\\margb[0-9]+","\\\\margb1080",ret);
    ret=gsub("\\.CLEARPAGE\\.","\\\\page",ret);


                                        # Change Times to Helv.
    ## Exclude the first \f13.
    tmp <- which(regexpr("\\\\f13",ret)!=-1)[1];

    ret[-tmp]=gsub("\\\\f13","\\\\f14",ret[-tmp]);

    ret=gsub("\\\\fi0","\\\\li720\\\\sa240\\\\fs20\\\\fi0",ret);# Paragraph 0.5" indent, 12 point after.
    ret=gsub("\\\\pard\\\\qj\\\\sl240\\\\slmult1 *\\\\fi360","\\\\pard\\\\qj\\\\sl240\\\\slmult1 \\\\fi0\\\\li720\\\\sa240\\\\fs20",ret);

    ret=gsub("NA\\\\~[}]","NA}",ret);

    ## File specific changes.
    ## Make non-breaking dashes.
    ret=gsub("\\(([^-\n]*)-([^-\n].*)\\)","(\\1\\\\_\\2)",ret,perl=TRUE);
    ## Put all cells on the same column
    cells  = rev(which(regexpr("\\\\cell\\b",ret,perl=TRUE) !=-1));
    for (val in cells){
        while (regexpr("(cell|cellx)",ret[val-1]) == -1){
            ret[val-1] <- paste(ret[val-1],"\n",ret[val],sep="");
            ret <- ret[-val];
            val <- val-1;
        }
    }
    trowd <- which(regexpr("\\\\trowd",ret)!=-1);
    ## Get row definitions.
    ret[trowd] <- gsub("\\\\trowd","\\\\trowd\\\\trftsWidth1\\\\trautofit1\\\\trpaddl130\\\\trpaddr130\\\\trpaddfl3\\\\trpaddfr3\\\\trkeep",ret[trowd]);

    ret = strsplit(ret,"\\\\cellx");
    lens <- sapply(ret,length)[trowd]-1;
    tmp.mrg <- function(k){
        ret <-
            paste("ret[[",k,"]][i] <- gsub(\"\\\\\\\\clvmrg\",\"\",ret[[",k,"]][i],perl=TRUE);
        ret[[",k,"]][i] <- paste(ret[[",k,"]][i],\"\\\\clvmrg\",sep=\"\");",sep="")
        return(ret)
    }
    iskeeprow=0;
    head="\\\\trhdr";
    for (rn in 1:length(trowd)){
        rowd <- ret[[trowd[rn]]];
        currow = c();
        for (j in seq(trowd[rn]+1,trowd[rn]+lens[rn])) {
            cell <- ret[[j]];
            ret[[j]] <- "";
            i <- j-trowd[rn];
            ## Now do cell specific stuff.
            if (regexpr("([{] *NA *[}]|[{] *[{][}] *NA *[}])",cell)!=-1){
                rowd[i] <- paste(rowd[i],"\\clcbpat16",sep="");
            }
            while (regexpr("\\.RM\\.BORDER\\.([^\n.]*)\\.",cell)!=-1) {
                tmp = gsub("(?:.|\n)*\\.RM\\.BORDER\\.(.*?)\\.(?:.|\n)*","\\1",cell,perl=TRUE);
                rowd[i]=gsub(paste("\\\\clbrdr",tmp,"(\\\\brdr\\w+)+",sep=""),"",rowd[i]);
                cell=gsub(paste("\\.RM\\.BORDER\\.",tmp,"\\.",sep=""),"",cell);
            }
            while (regexpr("\\.GREY\\.BORDER\\.([^.\n]*)\\.",cell)!=-1) {
                tmp = gsub("(?:.|\n)*\\.GREY\\.BORDER\\.(.*?)\\.(?:.|\n)*","\\1",cell);
                rowd[i]=gsub(paste("(\\\\clbrdr",tmp,"\\\\brdrs)",sep=""),"\\1\\brdrcf16",rowd[i]);
                cell=gsub(paste("\\.GREY\\.BORDER\\.",tmp,"\\.","",sep=""),cell);
            }
            ## Grey out things with greybg in it.
            if (regexpr("\\.GREYBG\\. *",cell)!=-1){
                rowd[i]=paste(rowd[i],"\\clcbpat16",sep="");
                cell=gsub("\\.GREYBG\\. *","",cell);
            }
            ## Do vertical merges.
            if (regexpr("rowspan([0-9]+) ",cell)!=-1){
                tmp=as.double(gsub("(?:.|\n)*rowspan([0-9]+) (?:.|\n)*","\\1",cell,perl=TRUE));
                rowd[i] <- gsub("\\\\clvmgf","",rowd[i]);
                rowd[i] <- paste(rowd[i],"\\clvmgf",sep="");
                k <- trowd[rn+1:tmp-1];
                k <-  k[!is.na(k)];
                eval(parse(text=paste(sapply(k,tmp.mrg),collapse="")));
                if ((tmp-1) > iskeeprow){
                    iskeeprow=tmp-1;
                }
                cell=gsub("rowspan([0-9]+) ","",cell);
            }
            currow=c(currow,cell);
        }
        row = paste(currow,collapse="\n");
        ## Fix lists.
        row <- gsub("\\\\pard\\\\qj\\\\sl240\\\\slmult1 \\\\sb50 \\\\li600\\\\fi-300 \\\\bullet\\\\tab",
                    "\\\\pard\\\\intbl\\\\tql\\\\tx150\\\\ql\\\\sl240\\\\slmult1 \\\\sb25 \\\\li150\\\\fi-150 \\\\bullet\\\\tab",row);
        ## Fix verbatim.
        row <- gsub("\\\\pard\\\\ql\\\\b0\\\\i0\\\\scaps0","\\\\pard\\\\intbl\\\\ql\\\\b0\\\\i0\\\\scaps0\\\\fs16",row);

        if (rn != 1){
            if (regexpr("\\.FOOTNOTE\\.",row)!=-1){
                                        # Footnote is kept with table:
                ret[[trowd[rn-1]+1]] <- gsub("\\\\pard(\\\\keepn)?","\\\\pard\\\\keepn",ret[[trowd[rn-1]+1]],perl=TRUE);
            }
        }
        row=gsub("\\\\trkeep","",row);
        lhead <- head;
        ## Clear Caption borders on left, right and top.
        if (regexpr("\\.CAPTION\\.",row)!=-1) {
            row=gsub("\\.CAPTION\\.","",row);
            rowd=gsub("\\\\clbrdr[lrt](\\\\brdr\\w+)+","",rowd);
            ## Add header property to current row.
            rowd[1]=paste(rowd[1],"\\trhdr",sep="");
            rowd[1]=sub("\\\\trkeep\\\\trhdr","\\\\trkeep\\\\trhdr",rowd[1]);
            head="\\\\trhdr";
        }
        if (regexpr("\\.ENDHEAD\\.",row)!=-1) {
            head="";
            row=gsub("\\.ENDHEAD\\.","",row);
        }
        if (regexpr("\\.FOOTNOTE\\.",row)!=-1) {
            row=gsub("\\.FOOTNOTE\\.","",row);
            rowd=gsub("\\\\clbrdr[lrb](\\\\brdr\\w+)+","",rowd);
        }
        ## Keep appropriate text together.
        if (regexpr("\\.KEEPN\\.",row)!=-1){
            row <- gsub("\\\\pard","\\\\pard\\\\keepn",row);
            row <- gsub("\\.KEEPN\\.","",row);
        }
        if (iskeeprow > 0){
            row <- gsub("\\\\pard","\\\\pard\\\\keepn",row);
            row <- gsub("\\.KEEPN\\.","",row);
            iskeeprow = iskeeprow-1;
        }
        if (lhead != ""){
            rowd[1] <- gsub("\\\\trkeep",paste("\\\\trkeep",lhead,sep=""),rowd[1]);
        }
                                        # Set the current row
        ret[[trowd[rn]+1]]=row;
        ret[[trowd[rn]]]=rowd;
        cat(".",file=stderr());
    }
    ret <- strsplit(unlist(paste(unlist(lapply(ret,paste,collapse="\\clvertalc\\trpaddr115\\trpaddl115\\cellx")),collapse="\n")),"\n")[[1]];
    ## Change all double borders to thicker lines, instead of double line.

    ret=gsub("\\\\brdrdb","\\\\brdrs\\\\brdrw30",ret);
    ## Super and subscripts.

    ret= gsub("\\.SUB\\.\\((.*?)\\)\\.SUB\\.","{\\\\sub \\1}",ret,perl=TRUE);
    ret= gsub("\\.SUP\\.\\((.*?)\\)\\.SUP\\.","{\\\\super \\1}",ret,perl=TRUE);
    ## Take out latex2rtf's numbering
    ret= gsub("[0-9.]+ +\\.SECTION\\.","",ret);
    ret= gsub("\\{\\\\\\*\\\\bkmkstart +(.*?)\\}[0-9.]+\\{\\\\\\*\\\\bkmkend +\\1\\} +\\.SECTION\\.","",ret);
    ret=gsub("\\{\\}","",ret);
                                        #    ret <- gsub("[.][{]3[}]N[.][{]3[}]","\n",ret);
    ret <- gsub("[.][{]3[}]-[.][{]3[}]","_",ret);
    ret <- gsub("[.][{]3[}]CF([0-9])[.][{]3[}]","\\\\cf\\1{}",ret);
    ret <- gsub("Zapf Chancery","Arial",ret)
    ret <- gsub("Times New Roman","Arial",ret)


    ## Fix Bibliographies to be overcited.
    ##   ret=gsub("\\[({\\\\field[^]]*BIB[^]]*)\\]( *)([.,:;]?)","\\3\\{\\\\super \\1\\}\\2",ret,perl=TRUE);
    newcont=paste(ret,collapse="\n");
    newcont <- gsub("\\\\s1\\b","",newcont,perl=TRUE);
    newcont=gsub("\n\n+","\n\n",newcont);
    if (use.macro){
        newcont <- gsub("RTFEXT","doc",newcont);
    } else {
        newcont <- gsub("RTFEXT","rtf",newcont);
    }
    for (i in 3:7){
        newcont <- gsub(paste("\\\\s",i,"\\b",sep=""),paste("\\\\s",(i-1),sep=""),newcont,perl=TRUE);
    }
    cat("\n",file=stderr());
    return(newcont);
}

tex.theta.table <-  function(xpdb,pr=NULL,dig=NULL,sigdig=NULL,...){
    if (is.null(pr)){
        if (!any(names(smy) == xpdb@Runno)){
            pr <-  getSum(xpdb,...);
            smy[[xpdb@Runno]] <<- pr;
        } else {
            pr <- smy[[xpdb@Runno]]
        }
    }
    tab <- pr$theta.tab;
    tab$rse = nm.round(tab$rse,dig=dig,sigdig=sigdig,single=TRUE)
    tab$se = nm.round(tab$se,dig=dig,sigdig=sigdig,single=TRUE)
    tab$est = nm.round(tab$est,dig=dig,sigdig=sigdig,single=TRUE)
    tab$cil = nm.round(tab$cil,dig=dig,sigdig=sigdig,single=TRUE)
    tab$ciu = nm.round(tab$ciu,dig=dig,sigdig=sigdig,single=TRUE)
    tab$ci = paste("(",tab$cil,",",tab$ciu,")",sep="");
    tab <-  tab[,!(names(tab) %in% c("cil","ciu"))];
    names(tab) <-
        gsub("^units$","Units",
             gsub("^ci$",paste(pr$ci*100,"\\\\% Confidence Interval",sep=""),
                  gsub("^rse$","Relative SE\\\\%",
                       gsub("^se$","SE",
                            gsub("^est$","Estimate",
                                 gsub("^theta$","$\\\\theta$",
                                      names(tab)))))))
    tmp <- latex.table(tab,
                       caption="$\\theta$ estimates",
                       label="theta.estimates");
    return(tmp);
}

tex.eps.table <-  function(xpdb,pr=NULL,dig=NULL,sigdig=NULL,...){
    if (is.null(pr)){
        if (!any(names(smy) == xpdb.ref@Runno)){
            pr <-  getSum(xpdb.ref,...);
            smy[[xpdb.ref@Runno]] <<- pr;
        } else {
            pr <- smy[[xpdb.ref@Runno]];
        }
    }
    tab <- pr$eps.tab;
    if (!is.null(tab)){
        tab$rse = nm.round(tab$rse,dig=dig,sigdig=sigdig,single=TRUE)
        tab$se = nm.round(tab$se,dig=dig,sigdig=sigdig,single=TRUE)
        tab$est = nm.round(tab$est,dig=dig,sigdig=sigdig,single=TRUE)
        tab$cil = nm.round(tab$cil,dig=dig,sigdig=sigdig,single=TRUE)
        tab$ciu = nm.round(tab$ciu,dig=dig,sigdig=sigdig,single=TRUE)
        tab$ci = paste("(",tab$cil,",",tab$ciu,")",sep="");
        tab$shrink = nm.round(tab$shrink,dig=dig,sigdig=sigdig,single=TRUE);
        tab$cv = nm.round(tab$cv,dig=dig,sigdig=sigdig,single=TRUE);
        tab <-  tab[,!(names(tab) %in% c("cil","ciu"))];
        names(tab) <-
            gsub("^shrink$","Shrinkage \\\\%",
                 gsub("^cv$","CV\\\\%",
                      gsub("^ci$",paste(pr$ci*100,"\\\\% Confidence Interval",sep=""),
                           gsub("^rse$","Relative SE\\\\%",
                                gsub("^se$","SE",
                                     gsub("^est$","Estimate",
                                          gsub("^eta$","$\\\\eta$",
                                               names(tab))))))));
        row.names(tab) <-  gsub("EPS([0-9]+)","$\\\\varepsilon_{\\1}$",row.names(tab));
        tmp <- latex.table(tab,
                           caption="$\\varepsilon$ estimates",
                           label="eps.estimates");
        tmp <-  gsub("[{]tab[}]","{~}",paste(tmp,collapse="\n"));
        return(tmp);
    } else {
        return("No EPS defined;  Probably single subject data -- EPS defined in OMEGA block.");
    }
}


tex.eta.table <-  function(xpdb,pr=NULL,dig=NULL,sigdig=NULL,...){
    if (is.null(pr)){
        if (!any(names(smy) == xpdb@Runno)){
            pr <-  getSum(xpdb,...);
            smy[[xpdb@Runno]] <<- pr;
        } else {
            pr <- smy[[xpdb@Runno]]
        }
    }
    tab <- pr$eta.tab;
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
    tab <-  tab[,!(names(tab) %in% c("cil","ciu"))];
    names(tab) <-
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
                                                          names(tab)))))))))))
    row.names(tab) <-  gsub("ETA([0-9]+)","$\\\\eta_{\\1}$",row.names(tab));
    tmp <- latex.table(tab,
                       caption="$\\eta$ estimates",
                       label="eta.estimates");
    tmp <-  gsub("[{]tab[}]","{~}",paste(tmp,collapse="\n"));
    return(tmp);
}

read.nm<-function(file,na.strings=c("NA","."),...)
{
  plt.file <- gsub("(/[^/]*).txt$","-PROCESSED\\1.csv",file);
  if (file.exists(plt.file)){
    d <- read.csv(plt.file)
  } else {
    nmf<-readLines(file,n=-1)[-1]
    nmf<-gsub(" +",",",nmf)
    nmf<-gsub("^,+","",nmf)
    ## Change 1.00+100 or 1.00-100 to 1.00E+100 or 1.00E-100
    nmf<-gsub("([0-9])([-+])([0-9])","\\1E\\2\\3",nmf)
    ## Now drop repeating tables and headers
    tabn <- which(regexpr("^[ \t]*TABLE",nmf)!=-1)
    tabn <-  c(tabn,tabn+1)
    nmf <- nmf[-tabn];
    tf<-tempfile()
    cat(nmf,file=tf,sep="\n")
    d<-read.csv(tf,na.strings=na.strings,...)
    unlink(tf)
  }
  return(d)
}

esn.input <- function(buffer.name){
  ## Get input data and replace column names with names in $INPUT. Return as dataset.
  nme <- sapply(1:length(.esn.aliases[[buffer.name]]),function(x) .esn.aliases[[buffer.name]][[x]][1])
  nme2 <- sapply(1:length(.esn.aliases[[buffer.name]]),function(x) .esn.aliases[[buffer.name]][[x]][2])
  o.name <-  names(.esn.inputs[[buffer.name]])
  trans <- data.frame(o=o.name,n1=nme,n2=nme2,stringsAsFactors = FALSE);
  trans$same <- NA;
  w <- which(is.na(trans$n2))
  trans$same[w] <-  trans$o[w] == trans$n1[w];
  d <- .esn.inputs[[buffer.name]];
  w <- which(!trans$same[w])
  ## rename data to NONMEM names.
  names(d)[w] <- trans$n1[w];
  ## Add aliases.
  w <- which(!is.na(trans$n2));
  for (i in w){
   d[[trans$n2[i]]] <-  d[[trans$n1[i]]];
  }
  return(d)
}

esn.lisp <-  function(obj,force.string=FALSE){
  ## turns a R list into a Emacs Lisp representation
  if (is.null(obj)){
    cat("nil\n")
  }
  else if (length(obj) == 0) {
    cat("nil\n")
  }else if (length(obj) == 1){
    if (!force.string && is.numeric(obj))
      cat(obj,"\n",sep="")
    else
      cat("\"", obj, "\"",sep="")
  } else {
    if (!force.string && all(is.numeric(obj))){
      cat(paste("(list ", paste(obj,collapse=" "),")\n",sep=""))
    } else {
      cat(paste("(list \"",paste(obj,collapse="\" \""),"\")\n",sep=""))
    }
  }
}
merge.esn <- function(buffer.name) {
  ## Merge esn merges hidden .esn.output objects.
  to.merge <- which(buffer.name == sapply(names(.esn.outputs),substring,0,nchar(buffer.name)));
  d <- .esn.outputs[[to.merge[1]]]
  for (i in 2:length(to.merge)){
    d <- merge(d,.esn.outputs[[i]]);
  }
  return(d)
}

##xpose.esn <- function(buffer.name,cotab.reg,catab.reg,sdtab.reg)

######################################################################
### esn-shared.R ends here
