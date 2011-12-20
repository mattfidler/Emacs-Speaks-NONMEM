## If Xpose tables are produced, archive them AND try to produce xpose plots
## Take out objects to free memory...
smy <- list();
if (file.exists("../USERSCRIPTS/xpose-plt.R") && file.exists("../USERSCRIPTS/esn-xpose-summary.R")){
    if (!any(.packages(all.available=TRUE) == "xpose4")){
        cat("Xpose features require Xpose package to be loaded\n",file=stderr());
    } else {
        smy <<- list();
        smy[[FILEID]] <- s;
        library(xpose4);
        source("../USERSCRIPTS/xpose-plt.R");
        setup.plt.xpose();
        source("../USERSCRIPTS/esn-xpose-summary.R");
        esn.redefs();
        setup.esn.variables();
xpsm.ord <- list("Model Overview"=c("theta.table","eta.table","eps.table","cor.table","errors","cov.summary","par.summary"),
	"Individual Plots"=c("ind.plots.log","ind.plots")
	);

	"Individual Plots"=c("ind.plots.log","ind.plots")
	);

	"Individual Plots"=c("ind.plots.log","ind.plots")
	);



        xpdb1 <<- xpose.plt(FILEID);
        xpdb1 <<- xpose.labs(xpdb1);
        ## Add file defined functions.
        xpsm.ord <- get.functions(xpdb1,orig=xpsm.ord);

        if (!is.null(xpdb1)){
            xpdb1@Prefs@Xvardef$ipre <- xpdb1@Prefs@Xvardef$ipred;
            xpdb1@Prefs@Xvardef$iwre <- xpdb1@Prefs@Xvardef$iwres;
            ## Add CWRES
            try(xpose.calculate.cwres(xpdb1,cres.table.prefix="",
                                      est.tab.suffix=paste("../MISCELLANEOUS/CWRESIDUALS-ESTIMATES/CWRES.est.",FILEID,".txt",sep=""),
                                      deriv.tab.suffix=paste("../TABLES/CWDERIV/CWRES.deriv.",FILEID,".txt",sep="")
                                      ));
            create.latex(xpdb1,title=paste("Summary of",FILEID),author="Userscript",date=date(),order=xpsm.ord,sigdig=3,file=FILEID,toc=FALSE);
            gen.rtf(file=FILEID,use.macro=TRUE);
            if (!file.exists("../USERSCRIPTS/xpose/")){
                dir.create("../USERSCRIPTS/xpose/");
            }
            if (file.exists(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".tex",sep=""))){
                unlink(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".tex",sep=""));
            }
            if (file.exists(paste(FILEID,".tex",sep=""))){
                file.rename(paste(FILEID,".tex",sep=""),
                            paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".tex",sep=""))
            }
            if (file.exists(paste(FILEID,".ptex",sep=""))){
                unlink(paste(FILEID,".ptex",sep=""));
            }
            if (file.exists(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".rtf",sep=""))){
                unlink(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".rtf",sep=""));
            }
            if (file.exists(paste(FILEID,".rtf",sep=""))){
                file.rename(paste(FILEID,".rtf",sep=""),
                            paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".rtf",sep=""))
            }
            if (file.exists(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".doc",sep=""))){
                unlink(paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".doc",sep=""));
            }
            if (file.exists(paste(FILEID,".doc",sep=""))){
                file.rename(paste(FILEID,".doc",sep=""),
                            paste("../USERSCRIPTS/xpose/xpose-summary-",FILEID,".rtf",sep=""))
            }
            if (file.exists(paste("../USERSCRIPTS/xpose/xpose-summary-figures-",FILEID,sep=""))){
                unlink(paste("../USERSCRIPTS/xpose/xpose-summary-figures-",FILEID,sep=""),recursive = TRUE);
            }
            if (file.exists(paste(FILEID,".xfig",sep=""))){
                file.rename(paste(FILEID,".xfig",sep=""),
                            paste("../USERSCRIPTS/xpose/xpose-summary-figures-",FILEID,sep=""))
            }
            ## Now generate html list.
            tmp.f1 <- function(x){
                tmp.f2 <- function(y){
                    files <- list.files(paste("../USERSCRIPTS/xpose/xpose-summary-figures-",FILEID,sep=""),pattern=paste(y,"-[0-9]+\\.png",sep=""));
                    if (length(files) >= 1){
                        tmp <- files;
                        tmp <- tmp[!is.null(tmp)];
                        tmp <-  tmp[tmp != ""];
                        return(paste(paste("\"",fix.caption(y,xpdb1,fix.log=TRUE)," (",paste(seq(1,length(files)),"of",length(files)),")\"=\"../USERSCRIPTS/xpose/xpose-summary-figures-",FILEID,"/",tmp,"\"",sep=""),collapse=","));
                    } else {
                        return(NULL);
                    }
                }
                tmp <- unlist(lapply(xpsm.ord[[x]],tmp.f2));
                tmp <- tmp[!is.null(tmp)];
                return(eval(parse(text=paste("list(",paste(tmp,collapse=","),")",sep=""))));
            }
            tmp <- lapply(names(xpsm.ord),tmp.f1);
            names(tmp) <- names(xpsm.ord);
            if (any(ls() == "par.links")){
                par.links <- c(par.links,tmp);
            }
        }
    }
}
