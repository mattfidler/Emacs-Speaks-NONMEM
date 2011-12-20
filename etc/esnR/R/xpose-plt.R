## Redefine Runsum.
setup.plt.xpose <-  function(){
    if (!exists("runsum.xpose")){
        runsum.xpose <<- runsum;
        runsum <<- function(object,
                            modfile=paste("../TEXTFILES/CONTROL/Control.", object@Runno, ".txt", sep=""),
                            listfile=paste("../TEXTFILES/RAWOUTPUT/Output.", object@Runno, ".txt", sep=""),
                            ...){
            if (check.vars2("iwres",object)){
                object@Data$IWRE <-  object@Data$WRES;
            }
            return(runsum.xpose(object=object,modfile=modfile,listfile=listfile,...));
        }
    }
}
remove.plt.xpose <- function(){
    if (exists("runsum.xpose")){
        runsum <<- runsum.xpose;
        rm("runsum.xpose",envir=.GlobalEnv);
    }
}

## Create Xpose type object from PLT tables.

gen.fake.nm.table <- function(d,file=stderr(),table.no=1){
    tmp.f <- function(x){
        return(gsub("  -"," -",paste(" ",paste(formatC(x,digits=4,format="E"),collapse="  "))))
    }
    dat <- paste(sapply(data.frame(t(d)),tmp.f),collapse="\n");
    tmp.f <- function(x){
        n <- nchar(x);
        if (n < 11){
            ## Fill with spaces
            return(paste(x,paste(rep(" ",10-n),collapse="")));
        } else if (n > 11) {
            ## Substring.
            return(substr(x,1,11));
        }
    }
    dat <- paste(gsub(" +$","",paste(" ",paste(sapply(names(d),tmp.f),collapse=" "),sep="")),"\n",dat,"\n",sep="");
    dat <- paste("TABLE NO.  ",table.no,"\n",dat,sep="");
    cat(dat,file=file);
    return(invisible());
}

xpose.plt <-  function(runno=FILEID,...){
    ## Need to generate fake NONMEM tables to input into xpose from PLT object.
    FILEID <-  runno;
    ALLFILE <- paste("../TABLES/ALLRECORDS-PROCESSED/AllRecords.", FILEID, ".csv", sep="")
    ALL	<- read.table(ALLFILE, header=T, as.is=T,sep=",")

    ETAFILE <- paste("../TABLES/FIRSTRECORDS-PROCESSED/FirstRecords.", FILEID, ".csv", sep="")
    if (file.exists(ETAFILE)){
        ETAS <- read.table(ETAFILE, header=T, as.is=T,sep=",");
    } else {
        ETAS <- NULL;
    }

    DEMOFILE <- paste("../TABLES/COVARIATES/CovariateFile.", FILEID, ".csv", sep="")
    if (file.exists(DEMOFILE)){
        DEMO <- read.table(DEMOFILE, header=T, as.is=T,sep=",");
    }
    ## Now merge the data together.
    if (is.null(ETAS)){
        mrg <-  ALL;
        cat("Length All: ",length(ALL[,1]),"\n",file=stderr());
    } else {
        cat("Length All: ",length(ALL[,1]),"\n",file=stderr());
        cat("Length Etas: ",length(ETAS[,1]),"\n",file=stderr());
        mrg <- merge(ALL,ETAS);
        cat("Length Merged: ",length(mrg[,1]),"\n",file=stderr());
    }
    if (is.null(DEMO)){
        tmp <- DEMO[,!(names(DEMO) %in% c("PRED","IPRED","WRES","RES","IRES","IWRES","DV","CMT","SS","EVID","RATE","MDV","TIME","AMT"))];
        cat("Length Demographics: ",length(tmp[,1]),"\n",file=stderr());
        mrg <- merge(mrg,tmp);
    }
    cat("Length Merged: ",length(mrg[,1]),"\n",file=stderr());
    extra <- c();
    if (length(mrg[,1]) != length(ALL[,1])){
        cat("Change to Xpose merged since All, Etas, and Demos didn't merge correctly.\n")
        cat("All:\n");
        print(summary(ALL));
        cat("Parsed Demo:\n");
        print(summary(tmp));
        cat("Etas:\n");
        print(summary(ETAS));
        cat("Can't Continue (because of merge)\n",file=stderr());
        return(NULL);
    } else {
        if (any(names(mrg) == "AMT")){
            cat("Dropping dosing records.\n");
            mrg <- mrg[is.na(mrg$AMT) | mrg$AMT == 0,];
            cat("Length Merged: ",length(mrg[,1]),"\n",file=stderr());
        }
        ## Check to see if lognormal by file-name.
        if (file.exists(paste("../TEXTFILES/CONTROL/Control.", FILEID, ".txt", sep=""))){
            con <- file(paste("../TEXTFILES/CONTROL/Control.", FILEID, ".txt", sep=""),"r");
            ctl <- readLines(con);
            close(con);
            if (any(regexpr("[Cc]ontrol [sS]tream [nN]ame:.*[-_]l(ogn?|n?)[-_.]",ctl) > -1)){
                print(ctl[which(regexpr("[Cc]ontrol [sS]tream [nN]ame:.*[-_]l(ogn?|n?)[-_.]",ctl) > -1)]);
                cat("Assume lognormal since -log- -ln- or -logn- is in file name.\n");
                cat("Transforming DV, PRED, IPRED\n");
                for (var in c("DV","PRED","IPRED","dv","pred","ipred")){
                    if(any(names(mrg) == var)){
                        mrg[,var] <- exp(mrg[,var]);
                    }
                }
            }
        }
        ## Generate Xpose tables.
        id.reg <-"(ID|NUM|OBS|id|num|obs)";

        ## Standard Table items.
        sdtab.reg <- "(DV|EVID|I(DV?|PRED|W?RES)|MDV|PRED|RES|T(AD|IME|SLD)|C?WRES|dv|evid|i(dv?|pred|w?res)|mdv|pred|res|t(ad|ime|sld)|c?wres)";

        sdtab <- mrg[,regexpr(sdtab.reg,names(mrg)) > -1];

        ## Get Covariate information
        if (file.exists(paste("../TEXTFILES/GRAPHICSSCRIPTS/GraphicsScript.",FILEID,".txt",sep=""))){
            source(paste("../TEXTFILES/GRAPHICSSCRIPTS/GraphicsScript.",FILEID,".txt",sep=""));
            catab.names <- c();
            cotab.names <- c();
            for (i in seq(along=COVARType)){
                if (any(names(mrg) == COVARColNames[i])){
                    # In dataset.
                    if (any(COVARType[i] == c("Race","Gender","DoseGroup","Categorical"))){
                        catab.names <- c(catab.names,COVARColNames[i]);
                    } else {
                        cotab.names <- c(cotab.names,COVARColNames[i]);
                    }
                }
            }
            if (length(cotab.names) > 0){
                cotab <- mrg[,c(names(mrg)[regexpr(id.reg,names(mrg)) > -1],
                                names(mrg)[names(mrg) %in% cotab.names])];
            } else {
                cotab <- NULL;
            }
            if (length(catab.names) > 0){
                catab <- mrg[,c(names(mrg)[regexpr(id.reg,names(mrg)) > -1],
                                names(mrg)[names(mrg) %in% catab.names])];
            } else {
                catab <- NULL;
            }
        } else {
            # Not in dataset.
            catab <- NULL;
            cotab <- NULL;
        }
        cat("SDTAB:\n");
        print(summary(sdtab));
        cat("COTAB:\n");
        print(summary(cotab));
        cat("CATAB:\n");
        print(summary(catab));
        ## PATAB is everything not in the previous tables.
        patab.names <- names(mrg);
        patab.names <- patab.names[!patab.names %in% c(names(sdtab),names(cotab),names(catab))];
        ## Add back ID variables
        patab.names <- c(patab.names,names(mrg)[regexpr(id.reg,names(mrg)) > -1]);
        patab <- mrg[,patab.names];
        i <- 1;
        if (!is.null(sdtab)){
            gen.fake.nm.table(sdtab,file="sdtab9999",table.no=i);
            i <- i+1;
        }
        if (!is.null(patab)){
            gen.fake.nm.table(patab,file="patab9999",table.no=i);
            i <- i+1;
        }
        if (!is.null(cotab)){
            gen.fake.nm.table(cotab,file="cotab9999",table.no=i);
            i <- i+1;
        }
        if (!is.null(catab)){
            gen.fake.nm.table(catab,file="catab9999",table.no=i);
            i <- i+1;
        }
        xpdb <- xpose.data("9999");
        unlink("sdtab9999");
        unlink("patab9999");
        unlink("cotab9999");
        unlink("catab9999");
        xpdb@Runno <- FILEID;
        xpdb <- xpose.labs(xpdb);
        xpdb@Data$ID <- as.numeric(paste(xpdb@Data$ID))
        return(xpdb);
    }
}
