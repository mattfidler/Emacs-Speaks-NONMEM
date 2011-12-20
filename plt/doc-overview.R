tex <- overview.table(pr=s,run.summary=NULL,sigdig=3);
tex <- paste(tex,errors.latex(pr=s),sep="\n\n");
tex <- gsub("\\\\","\\",tex,fixed=TRUE);
tmp <-  gsub("\\\\","\\",tex.theta.table(pr=s,run.summary=NULL,sigdig=3),fixed=TRUE);
tex2 <- tmp;
tmp <-  gsub("\\\\","\\",tex.eta.table(pr=s,run.summary=NULL,sigdig=3),fixed=TRUE);
tex2 <- paste(tex2,tmp,sep="\n\n");
tmp <-  gsub("\\\\","\\",tex.eps.table(pr=s,run.summary=NULL,sigdig=3),fixed=TRUE);
tex2 <- paste(tex2,tmp,sep="\n\n");
tex <- paste("\\documentclass[12pt]{article}
\\title{Run ",FILEID,"}
\\usepackage{amsmath}
\\usepackage{multirow}
\\usepackage{lscape}
\\usepackage{pdflscape}
\\usepackage{longtable}
\\usepackage{hyperref}
\\usepackage[nospace]{overcite}
\\author{EsN generated}
\\begin{document}
  \\maketitle
\\section{Overview/Errors}
",tex,"
\\section{Theta/Eta/Eps tables}
",tex2,"
\\end{document}
",sep="");
tex <- munge.tex(tex);
write(tex,"tex.tex");
is.r <- FALSE;
try(eval(expression(is.r <- R.version.string != "")))
have.latex2rtf <- TRUE;
if (Sys.getenv("ProgramFiles") != ""){
    if (Sys.getenv("ProgramFiles") != ""){
        ## Search for the path in the following locations.
        ## First check to see if LaTeX2rtf is installed in Program Files.
        if (file.exists("../USERSCRIPTS/lib/latex2rtf/latex2rt.exe")){
            latex2rtf <-  shortPathName("../USERSCRIPTS/lib/latex2rtf/latex2rt.exe");
        } else {
            Sys.setenv(rtfpath=shortPathName(paste(Sys.getenv("ProgramFiles"),"\\latex2rtf\\cfg")));
            latex2rtf <- shortPathName(paste(Sys.getenv("ProgramFiles"),"\\latex2rtf\\latex2rt.exe",sep=""));
            if (!file.exists(latex2rtf)){
                have.latex2rtf <- FALSE;
                warning("This requires Latex2rtf (GUI) to be installed in the default location.");
            }
        }
    }
}
if (have.latex2rtf){
    system(paste(latex2rtf,"tex.tex"));
    unlink("tex.tex");
    con <- file("tex.rtf","r")
    rtf <- readLines(con);
    close(con);
    unlink("tex.rtf");
    rtf <- munge.rtf(rtf,use.macro=(Sys.getenv("ProgramFiles") != ""));
    if (!file.exists("../USERSCRIPTS/overview-tables-doc/")){
        dir.create("../USERSCRIPTS/overview-tables-doc/");
    }
    write(rtf,paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".rtf",sep=""));
    if (Sys.getenv("ProgramFiles") != ""){
        if (file.exists("../USERSCRIPTS/savepassword.vbs")){
            if (file.exists(paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".doc",sep=""))){
                unlink(paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".doc",sep=""));
            }
            if (file.exists(paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,"-ro.doc",sep=""))){
                unlink(paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,"-ro.doc",sep=""));
            }
            system(paste("wscript ../USERSCRIPTS/savepassword.vbs",round(runif(1)*10000),paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".rtf",sep="")))
        }
    }
    unlink("tex.tex");
    unlink("tex.rtf");

    if (any(ls()=="par.links")){
        lst <- list(
                    "Overview (doc)"=paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".doc",sep=""),
                    "Overview (rtf)"=paste("../USERSCRIPTS/overview-tables-doc/overview-table-doc.",FILEID,".rtf",sep="")
                    );
        if (!any(names(par.links)=="User Generated")) {
            par.links[["User Generated"]] = lst;
        } else {
            par.links[["User Generated"]] = c(par.links[["User Generated"]],lst);
        }
    }
}
