## Take out objects to free memory...
tmp.f <- function(x) {
    if (regexpr("(ALL|FILEID|ETAFILE|ETAS|DEMOFILE|DEMO|Globalheader|ids)",x) < 0) {
        return(paste("rm(",x,")",sep=""));
    }
}
eval(parse(text=paste(unlist(lapply(ls(),tmp.f)),collapse=";")));

## Figure out what environment is running.
is.r <- FALSE;
try(eval(expression(is.r <- R.version.string != "")))

esn.path <- "c:/DOCUME~1/US041375/LOCALS~1/Temp/EmacsPortable/Other/esn/";
cat(paste("Working Directory:",getwd() ,"\n",sep=""),file=stderr())

## Is this from PLT tools?
FROMPLTTOOLS <- any(regexpr("ALL",ls()) >= 0); ## Assumes the presence
## of ALL to imply that
## this came from PLT
## tools directly.
if (!FROMPLTTOOLS) {
    ## Get the information that will be available in PLT tools.  Get the Last
    ## Working FileID.  The way its done in the example user script doesn't work
    ## for me...
    FILEID <- sort(list.files("../TABLES/ALLRECORDS-PROCESSED/",pattern="AllRecords\\..*\\.csv",all.files=TRUE));
    FILEID <- FILEID[length(FILEID)];
    FILEID <- gsub("AllRecords.([0-9]+-[0-9]+).csv","\\1",FILEID);

    ALLFILE <- paste("../TABLES/ALLRECORDS-PROCESSED/AllRecords.", FILEID, ".csv", sep="")
    ALL	<- read.table(ALLFILE, header=T, as.is=T,sep=",")

    ETAFILE <- paste("../TABLES/FIRSTRECORDS-PROCESSED/FirstRecords.", FILEID, ".csv", sep="")
    if (file.exists(ETAFILE)){
        ETAS <- read.table(ETAFILE, header=T, as.is=T,sep=",");
    } else {
        ETAS <-  NULL;
    }

    DEMOFILE <- paste("../TABLES/COVARIATES/CovariateFile.", FILEID, ".csv", sep="")
    if (file.exists(DEMOFILE)){
        DEMO <- read.table(DEMOFILE, header=T, as.is=T,sep=",");
    } else {
        DEMO <-  NULL;
    }
    GlobalHeader <- "Testing outside of PLT tools";

    ## Aliases -- Somehow need to get Aliases.
    ## Get Graphic Scripts Parameters
    ## ALL$ID <- ALL$PTID
    ## ALL$DV <- ALL$CP
}

## Take care of any ID aliases.
IDALIAS <- c("");

if (file.exists(paste("../TEXTFILES/GRAPHICSSCRIPTS/GraphicsScript.",FILEID,".txt",sep=""))){
    try(source(paste("../TEXTFILES/GRAPHICSSCRIPTS/GraphicsScript.",FILEID,".txt",sep="")));
} else {
    ## Try to get graphics script from control stream name and working directory.
    con <- file(paste("../TEXTFILES/CONTROL/Control.",FILEID,".txt",sep=""),"r");
    mod <- readLines(con)
    close(con);
    if (any(regexpr("; *[Cc]ontrol [sS]tream [nN]ame:",mod) > 0)) {
        ctl.model <- gsub("\\..*$","",gsub(";+ [Cc]ontrol *[Ss]tream *[nN]ame:? *","",gsub(".*[\\\\/]([^\\\\/]*)$","\\1",mod[which(regexpr("; *[Cc]ontrol [Ss]tream [nN]ame:",mod) > 0)])));
        if (is.na(ctl.model)){
            ## Look for it in a file context
            start.file <- which(regexpr(";[Cc]?[ \t]*[Ff]ile:? *",mod)>=0)[1];
            stop.file <- start.file;
            while (regexpr(";[Cc]?[ \t]*.*?[.][^.]*[ \t]*$",mod[stop.file]) == -1){
                stop.file <- stop.file + 1;
            }
            fn <- mod[start.file:stop.file];
            fn <- paste(gsub("^[ \t]*;+[Cc]?[ \t]*([Ff]ile:?)?[ \t]*","",fn),collapse="");
            fn <- gsub(".*[\\\\/]([^\\\\/]*)$","\\1",fn)
            ctl.model <- fn;
        }
        lst <- list.files("../SCRIPTS-GRAPHICS/",pattern=paste(ctl.model));
        if (length(lst) == 1){
            source(paste("../SCRIPTS-GRAPHICS/",lst[1],sep=""));
        } else {
            cat("There is more than one graphics script that could be associated with this file...\n Won't load graphics script and therefore won't create any Xpose plots...\n");
        }
    }
}




source("../USERSCRIPTS/esn-shared.R");
ctl.file <- paste("../TEXTFILES/CONTROL/Control.", FILEID, ".txt", sep="")

lst.file <- paste("../TEXTFILES/RAWOUTPUT/Output.", FILEID, ".txt", sep="")
s <- getSum(lst.file=lst.file,ctl.file=ctl.file);


## List of default links.
par.links <- list(
                  "NONMEM Input/Messages/Output/Control Stream"=list(
                  "Raw Data (FDATA)"=paste("../TABLES/RAWDATA/RawData.",FILEID,".txt",sep=""),
                  "Data (FDATA)"=paste("../TABLES/INPUT/Data.",FILEID,".txt",sep=""),
                  "Data (CSV)"=paste("../TABLES/INPUT-PROCESSED/Data.",FILEID,".csv",sep=""),
                  "Control Stream"=paste("../TEXTFILES/CONTROL/Control.",FILEID,".txt",sep=""),
                  "Control Stream Output"=paste("../TEXTFILES/RAWOUTPUT/Output.",FILEID,".txt",sep=""),
                  "Messages (NMTran)"=paste("../MISCELLANEOUS/MESSAGES-NONMEM/NMTRAN-NONMEMMessages.",FILEID,".txt",sep=""),
                  "Prediction Errors"=paste("../MISCELLANEOUS/PRDERR/PRDERR.",FILEID,".txt",sep=""),
                  "Parameters (CSV)"=paste("../TABLES/PARAMETERS/Parameters.",FILEID,".csv",sep="")
                  ),
                  "Statistics"=list(
                  "GLM"= paste("../TEXTFILES/GLM-AIC/AIC.",FILEID,".txt",sep=""),
                  "Statistics"= paste("../TEXTFILES/STATISTICS/Statistics.",FILEID,".txt",sep="")
                  ),
                  "Graphics"=list(
                  "Summary"= paste("../PDF/SUMMARY/Summary.",FILEID,".pdf",sep=""),
                  "Results"=paste("../PDF/RESULTS/Results.",FILEID,".pdf",sep=""),
                  "Brief Summary"=paste("../PDF/BRIEFSUMMARY/BriefSummary.",FILEID,".pdf",sep=""),
                  "Parameter Tracking"=paste("../PDF/TRACKINGGRAPHICS/ParameterTrackingGraphics.",FILEID,".pdf",sep=""),
                  "Graphics"=paste("../GRAPHICS/Graphics.",FILEID,".pdf",sep=""),
                  "Graphics Script"=paste("../TEXTFILES/GRAPHICSSCRIPTS/GraphicsScript.",FILEID,".txt",sep="")
                  ),
                  "Output Tables"=list(
                  "CWRES est output"=paste("../MISCELLANEOUS/CWRESFILEIDUALS-ESTIMATES/CWRES.est.",FILEID,".txt",sep=""),
                  "CWRES deriv output (NONMEM)"=paste("../TABLES/CWDERIV/CWRES.deriv.",FILEID,".txt",sep=""),
                  "CWRES deriv output (CSV)"=paste("../TABLES/CWDERIV-PROCESSED/CWRES.deriv.",FILEID,".csv",sep=""),
                  "CWRES Residuals (compute.cwres.R)"=paste("../TABLES/CWRESIDUALS/CWResiduals.",FILEID,".txt",sep=""),
                  "CWRES Residuals (CSV)"=paste("../TABLES/CWRESIDUALS-PROCESSED/CWResiduals.",FILEID,".csv",sep=""),
                  "All Records (NONMEM)"=paste("../TABLES/ALLRECORDS/AllRecords.",FILEID,".txt",sep=""),
                  "All Records (CSV)"=paste("../TABLES/ALLRECORDS-PROCESSED/AllRecords.",FILEID,".csv",sep=""),
                  "First Records (NONMEM)"=paste("../TABLES/FIRSTRECORDS/FirstRecords.",FILEID,".txt",sep=""),
                  "First Records (CSV)"=paste("../TABLES/FIRSTRECORDS-PROCESSED/FirstRecords.",FILEID,".csv",sep=""),
                  "Extra Records (NONMEM)"=paste("../TABLES/EXTRARECORDS/ExtraRecords.",FILEID,".txt",sep=""),
                  "Extra Records (CSV)"=paste("../TABLES/EXTRARECORDS-PROCESSED/ExtraRecords.",FILEID,".csv",sep=""),
                  "Half Lives (CSV)"=paste("../TABLES/HALFLIVES/HalfLives.",FILEID,".csv",sep=""),
                  "Covariates (CSV)"=paste("../TABLES/COVARIATES/CovariateFile.",FILEID,".csv",sep="")
                  ),
                  "Miscellaneous"=list(
                  "MSFO"=paste("../MISCELLANEOUS/MSFO/msfo.",FILEID,sep=""),
                  "Previous MSFO"=paste("../MISCELLANEOUS/MSFO-PREVIOUS/msfo.",FILEID,sep=""),
                  "Parameter Count"=paste("../MISCELLANEOUS/PARAMETER-COUNT/Parameters.",FILEID,".txt",sep="")
                  )
                  );
## Source everything in lib.
## Note PLT tools "source" function doesn't have the same scoping.
tmp.f <- function(x){
    con <- file(x,"r");
    ret <- paste(readLines(con),collapse="\n");
    close(con);
    ret <- paste("cat(\"--------------------------------------------------------------------------------\\n\");\ncat(\"Starting ",x,"\\n\");\ncat(\"--------------------------------------------------------------------------------\\n\");\n",ret,sep="");
    return(ret);
}
html.us <- "";
if (file.exists("../USERSCRIPTS/html.R")){
    con <- file("../USERSCRIPTS/html.R","r");
    html.us <- paste(readLines(con),collapse="\n");
    close(con);
}
if (file.exists("../USERSCRIPTS/esn-xmind.R")){
    con <- file("../USERSCRIPTS/esn-xmind.R","r");
    html.us <- paste(html.us,"\n",paste(readLines(con),collapse="\n"),"\nxmind.plt.update()\n",sep="");
    close(con);
}
eval(parse(text=paste(paste(sapply(paste("../USERSCRIPTS/lib/",list.files("../USERSCRIPTS/lib/",".*\\.R$"),sep=""),tmp.f),collapse="\n"),html.us,sep="\n")));

tmp.f <- function(x) {
    if (regexpr("(ALL|FILEID|ETAFILE|ETAS|DEMOFILE|DEMO|Globalheader|ids)",x) < 0) {
        return(paste("rm(",x,")",sep=""));
    }
}
eval(parse(text=paste(unlist(lapply(ls(),tmp.f)),collapse=";")));


## Now regenerate anything that hasn't been run
tmp.fn <-  function(id) {
    cat("--------------------------------------------------------------------------------\n",file=stderr());
    cat("Could not find run information for",id,"running UserDefinedScript for",id,"\n",file=stderr());
    cat("--------------------------------------------------------------------------------\n",file=stderr());
    FILEID <<- id;
    ALLFILE <<- paste("../TABLES/ALLRECORDS-PROCESSED/AllRecords.", FILEID, ".csv", sep="")
    ALL	<<- read.table(ALLFILE, header=T, as.is=T,sep=",")

    ETAFILE <<- paste("../TABLES/FIRSTRECORDS-PROCESSED/FirstRecords.", FILEID, ".csv", sep="")
    if (file.exists(ETAFILE)){
        ETAS <<- read.table(ETAFILE, header=T, as.is=T,sep=",");
    }

    DEMOFILE <<- paste("../TABLES/COVARIATES/CovariateFile.", FILEID, ".csv", sep="")
    if (file.exists(DEMOFILE)){
        DEMO <<- read.table(DEMOFILE, header=T, as.is=T,sep=",");
    } else {
        DEMO <<-  NULL;
    }
    GlobalHeader <<- "Testing outside of PLT tools";
    ## Recursive call.
    source("../USERSCRIPTS/UserScript.R");
    return(NULL);
}
regen <- any(regexpr("ids",ls()) >= 0); ## Assumes the presence
if (!regen) {
    ids <- gsub("AllRecords.([0-9]+-[0-9]+).csv","\\1",sort(list.files("../TABLES/ALLRECORDS-PROCESSED/",pattern="AllRecords\\..*\\.csv",all.files=TRUE)));
    r.last <-  paste("../USERSCRIPTS/r-last-run/r-last-run-",ids,".R",sep="");
    ids <-  ids[!unlist(lapply(r.last,file.exists))];

    lapply(ids,tmp.fn);
    if (Sys.getenv("ProgramFiles") != ""){
        if (file.exists("../USERSCRIPTS/esn-plt-sum-link.exe")) {
            system("../USERSCRIPTS/esn-plt-sum-link.exe");
        }
    }
    tmp.f <- function(x) {
        return(paste("rm(",x,")",sep=""));
    }
    eval(parse(text=paste(unlist(lapply(ls(),tmp.f)),collapse=";")));
}
