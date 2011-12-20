### esn-xmind.R --- Routines for dealing with Xmind files.
##
## Filename: esn-xmind.R
## Description: R Routines for dealing with Xmind files.
## Author: Matthew L. Fidler
## Maintainer: Matthew L. Fidler
## Created: Tue Feb  2 09:37:36 2010 (-0600)
##     Update #: 1117
## URL: http://esnm.sourceforge.net
## Keywords: Emacs Speaks NONMEM
## Compatibility: R 2.10.0
##
######################################################################
##
### Commentary:
##
## Making and changing xmind files.
##
######################################################################
##
### Change log:
## 01-Jul-2010    Matthew L. Fidler
##    If control stream isn't in mind map, place it inside the mind-map.
## 01-Jul-2010    Matthew L. Fidler
##    Added handling of Maps that have floating and attached topics.
## 01-Jul-2010    Matthew L. Fidler
##    Added error handling when label doesn't include working directory
## 12-Feb-2010    Matthew L. Fidler
##    Added run-terminated marker.
## 09-Feb-2010    Matthew L. Fidler
##
##    Took out project update on last run.  Sometimes it doesn't update
##    everything it needs to otherwise.
##
## 08-Feb-2010    Matthew L. Fidler
##    Made project file update only on last run.
## 08-Feb-2010    Matthew L. Fidler
##    xlink bug fix.
## 08-Feb-2010    Matthew L. Fidler
##    Added Marker updates and updates on Project file for PLT tools.
## 08-Feb-2010    Matthew L. Fidler
##    Added bug fixes, plus add/delete table information.
## 05-Feb-2010    Matthew L. Fidler
##    Added table creation.
## 04-Feb-2010    Matthew L. Fidler
##    Added Parsing, and Table Creation.
## 02-Feb-2010    Matthew L. Fidler
##    Added functions to open and close Xmind files.
## 02-Feb-2010    Matthew L. Fidler
##    Initial Version.
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


## ###########################################################################
## Xmind PLT tools functions:
## ###########################################################################

xmind.plt.update <-  function(){
    ## Get Working directory and
    if (file.exists(paste("../TEXTFILES/CONTROL/Control.",FILEID, ".txt", sep=""))){
        con <-  file (paste("../TEXTFILES/CONTROL/Control.",FILEID, ".txt", sep=""),"r");
        ctl <-  readLines(con);
        close(con);
        ctl.name <- gsub(";+ [Cc]ontrol *[Ss]tream *[nN]ame:? *","",gsub(".*[\\\\/]([^\\\\/]*)$","\\1",ctl[which(regexpr("; Control stream name: .*",ctl)>=0)[1]]));
        if (is.na(ctl.name)){
            ## Look for it in a file context
            start.file <- which(regexpr(";[Cc]?[ \t]*[Ff]ile:? *",ctl)>=0)[1];
            stop.file <- start.file;
            while (regexpr(";[Cc]?[ \t]*.*?[.][^.]*[ \t]*$",ctl[stop.file]) == -1){
                stop.file <- stop.file + 1;
            }
            fn <- ctl[start.file:stop.file];
            fn <- paste(gsub("^[ \t]*;+[Cc]?[ \t]*([Ff]ile:?)?[ \t]*","",fn),collapse="");
            fn <- gsub(".*[\\\\/]([^\\\\/]*)$","\\1",fn)
            ctl.name <- fn;
        }
        work.dir <- gsub("^.*\\b[^/\\]*([wW][oO][rR][kK][^/\\]*)\\b.*$","\\1",ctl[which(regexpr("\\b[^/\\]*[wW][oO][rR][kK][^/\\]*\\b",ctl)> -1)[1]]);
        xmind.name <- gsub("\\.[^.]*",".xmind",ctl.name);
        xmind.file <- paste("../",work.dir,"/",xmind.name,sep="");
        ctl.file <- paste("../TEXTFILES/CONTROL/Control.", FILEID, ".txt", sep="");

        lst.file <- paste("../TEXTFILES/RAWOUTPUT/Output.", FILEID, ".txt", sep="");
        s <- getSum(lst.file=lst.file,ctl.file=ctl.file);
        r.saves <- list();
        ## Build list of runs and last runs to (eventually) build the Xmind file.
        tmp.f <- function(x){
            con <- file(x,"r");
            ret <- paste(readLines(con),collapse="\n");
            close(con);
            return(ret);
        }
        eval(parse(text=paste(sapply(paste("../USERSCRIPTS/r-last-run/",list.files("../USERSCRIPTS/r-last-run/",".*\\.R$"),sep=""),tmp.f),collapse="\n")));

        is.last <<-  FALSE;
        tmp.f <-  function(x){
            if (sort(r.saves[[x]],decreasing=TRUE)[1] == FILEID){
                is.last <<- TRUE;
            }
        }
        sapply(names(r.saves),tmp.f);
        if (is.last && file.exists(xmind.file)){
            cat("Updating xmind file: ",xmind.file,"\n");
            cat("Decompressing\n");
            xmind.decompress <<-  TRUE;
            tryCatch(unzip.xmind(xmind.file),
                     error=function(e){xmind.decompress<<-FALSE;});
            if (!xmind.decompress){
                cat("Xmind cannot be decompressed.  Try installing 7zip\n",file=stderr());
            } else {
                cat("Parsing\n");
                xml <-  parse.content();
                val <- paste(work.dir,"/",ctl.name,sep="");
                id <- xmind.get.id(xml,val);
                if (is.null(id)){
                    val <- ctl.name;
                    id <- xmind.get.id(xml,val);
                }
                cat("Changing Marker... (id:",id,")\n");
                if (length(s$errors) == 3){
                    xml <- xmind.marker(xml,id,"other-yes");
                } else if (any(regexpr("Unsuccessful minimization",s$errors) > -1)){
                    xml <- xmind.marker(xml,id,"other-no");
                } else {
                    xml <- xmind.marker(xml,id,"other-exclam");
                }
                cat("Adding Theta Estimates\n")
                theta.est <- xmind.get.id(xml,"^Theta Estimates");
                if (!is.null(theta.est)){
                    xml <- xmind.rm(xml,theta.est);
                }
                xml.tag <<- xmind.theta.table(s,sigdig=3)
                theta.est <- parse.tag();
                xml <- xmind.add(xml,theta.est,id);

                cat("Adding Eta Estimates\n")
                eta.est <- xmind.get.id(xml,"^Eta Estimates");
                if (!is.null(eta.est)){
                    xml <- xmind.rm(xml,eta.est);
                }
                xml.tag <<- xmind.eta.table(s,sigdig=3);
                eta.est <- parse.tag();
                xml <- xmind.add(xml,eta.est,id);

                cat("Adding Eps Estimates\n")
                eps.est <- xmind.get.id(xml,"^Eps Estimates");
                if (!is.null(eps.est)){
                    xml <- xmind.rm(xml,eps.est);
                }
                xml.tag <<- xmind.eps.table(s,sigdig=3);
                eps.est <- parse.tag();
                xml <- xmind.add(xml,eps.est,id);

                cat("Adding Links\n")
                links <- xmind.get.id(xml,"^Links$");
                if (!is.null(links)){
                    xml <- xmind.rm(xml,links);
                }
                xml.tag <<- xmind.get.links()
                links <- parse.tag();
                xml <- xmind.add(xml,links,id);

                cat("Adding Errors & Notes\n")
                notes <- xmind.get.id(xml,"^Errors/Notes$");
                if (!is.null(notes)){
                    xml <- xmind.rm(xml,notes);
                }
                xml.tag <<- xmind.errors.notes(s);

                notes <- parse.tag();
                xml <- xmind.add(xml,notes,id);

                cat("Saving Xmind File\n");

                xml <- str.xmind(xml);
                write(xml,paste(xmind.dir,"content.xml",sep=""));
                zip.xmind(xmind.dir,xmind.file);
                ## Now create/update the ./xmind/xmind-RUN-ID.xmind file
                ind.file <- paste("../USERSCRIPTS/xmind/xmind-",FILEID,".xmind",sep="");
                decompress <-  FALSE;
                if (!file.exists(ind.file)){
                    if (file.exists(xmind.file)){
                        try(dir.create("../USERSCRIPTS/xmind"));
                        file.copy(xmind.file,ind.file);
                        cat("Updating xmind file: ",xmind.file,"\n");
                        cat("Decompressing\n");
                        unzip.xmind(ind.file);
                        decompress <-  TRUE;
                        content <-  get.content();
                        ## Fix links
                        content <-  gsub("file:","file:../",content);
                        write(content,paste(xmind.dir,"content.xml",sep=""));

                    }
                }
                if (file.exists(paste("../USERSCRIPTS/xmind/xmind-",FILEID,".xmind",sep=""))){
                    if (!decompress){
                        cat("Updating xmind file: ",xmind.file,"\n");
                        cat("Decompressing\n");
                        unzip.xmind(ind.file);
                    }
                    ## Remove PLT runs
                    cat("Parsing\n");
                    xml <-  parse.content();
                    val <- paste(work.dir,"/",ctl.name,sep="");
                    id <- xmind.get.id(xml,val);
                    if (is.null(id)){
                        val <- ctl.name;
                        id <- xmind.get.id(xml,val);
                    }

                    plt <- xmind.get.id(xml,"^PLT Runs");
                    if (!is.null(plt)){
                        xml <-  xmind.rm(xml,plt);
                    }

                    cat("Changing Marker...\n");
                    if (length(s$errors) == 3){
                        xml <- xmind.marker(xml,id,"other-yes");
                    } else if (any(regexpr("Unsuccessful minimization",s$errors) > -1)){
                        xml <- xmind.marker(xml,id,"other-no");
                    } else {
                        xml <- xmind.marker(xml,id,"other-exclam");
                    }
                    cat("Adding Theta Estimates\n")
                    theta.est <- xmind.get.id(xml,"^Theta Estimates");
                    if (!is.null(theta.est)){
                        xml <- xmind.rm(xml,theta.est);
                    }
                    xml.tag <<- xmind.theta.table(s,sigdig=3)
                    theta.est <- parse.tag();
                    xml <- xmind.add(xml,theta.est,id);

                    cat("Adding Eta Estimates\n")
                    eta.est <- xmind.get.id(xml,"^Eta Estimates");
                    if (!is.null(eta.est)){
                        xml <- xmind.rm(xml,eta.est);
                    }
                    xml.tag <<- xmind.eta.table(s,sigdig=3);
                    eta.est <- parse.tag();
                    xml <- xmind.add(xml,eta.est,id);

                    cat("Adding Eps Estimates\n")
                    eps.est <- xmind.get.id(xml,"^Eps Estimates");
                    if (!is.null(eps.est)){
                        xml <- xmind.rm(xml,eps.est);
                    }
                    xml.tag <<- xmind.eps.table(s,sigdig=3);
                    eps.est <- parse.tag();
                    xml <- xmind.add(xml,eps.est,id);

                    cat("Adding Links\n")
                    links <- xmind.get.id(xml,"^Links$");
                    if (!is.null(links)){
                        xml <- xmind.rm(xml,links);
                    }
                    xml.tag <<- xmind.get.links(link.prefix="../")
                    links <- parse.tag();
                    xml <- xmind.add(xml,links,id);

                    cat("Adding Errors & Notes\n")
                    notes <- xmind.get.id(xml,"^Errors/Notes$");
                    if (!is.null(notes)){
                        xml <- xmind.rm(xml,notes);
                    }
                    xml.tag <<- xmind.errors.notes(s);

                    notes <- parse.tag();
                    xml <- xmind.add(xml,notes,id);

                    cat("Saving Xmind File\n");

                    xml <- str.xmind(xml);
                    write(xml,paste(xmind.dir,"content.xml",sep=""));
                    zip.xmind(xmind.dir,ind.file);
                }

                ## Now update "_project.xmind" (if last run)
                xmind.file <- "../USERSCRIPTS/_project.xmind";
                if (is.last && file.exists(xmind.file)){
                    cat("Updating xmind file: ",xmind.file,"\n");
                    cat("Decompressing\n");
                    unzip.xmind(xmind.file);
                    cat("Parsing\n");
                    xml <-  parse.content();
                    val <- paste(work.dir,"/",ctl.name,sep="");
                    id <- xmind.get.id(xml,val);
                    if (is.null(id)){
                        val <- ctl.name;
                        id <- xmind.get.id(xml,val);
                        if (is.null(id)){
                            cat("Could not find information in project file, adding....\n");
                            id <- xmind.get.id(xml,"^R updates");
                            if (is.null(id)){
                                id <- paste("esn_r_update_ctl_",xmind.timestamp(),sep="");
                                rupdates <- paste("<topic id=\"esn_r_updates_",
                                                  xmind.timestamp(),
                                                  "\" timestamp=\"",xmind.timestamp(),
                                                  "\"><title>R Updates (could not find file in project.xmind)</title><children><topics type=\"attached\"><topic id=\"",
                                                  id,"\" timestamp=\"",xmind.timestamp(),"\"><title>",
                                                  ctl.name,
                                                  "</title><labels><label>",
                                                  work.dir,"/",ctl.name,
                                                  "</label></labels></topic></topics></children></topic>",sep="");
                                xml <- xmind.add(xml,rupdates);
                                xml <- parse.content(str.xmind(xml));
                            } else {
                                rid <- id;
                                id <- paste("esn_r_update_ctl_",xmind.timestamp(),sep="");
                                rupdates <- paste("<topic id=\"",
                                                  id,"\" timestamp=\"",xmind.timestamp(),"\"><title>",
                                                  ctl.name,
                                                  "</title><labels><label>",
                                                  work.dir,"/",ctl.name,
                                                  "</label></labels></topic>",sep="");
                                xml <- xmind.add(xml,rupdates,rid);
                                xml <- parse.content(str.xmind(xml));
                            }
                        }
                    }
                    cat("Changing Marker...\n");
                    if (length(s$errors) == 3){
                        xml <- xmind.marker(xml,id,"other-yes");
                    } else {
                        xml <- xmind.marker(xml,id,"other-exclam");
                    }
                    cat("Saving Xmind File\n");

                    xml <- str.xmind(xml);
                    write(xml,paste(xmind.dir,"content.xml",sep=""));
                    zip.xmind(xmind.dir,xmind.file);
                }
                ## PLT Run Tables
                if (is.last){
                    val <- paste(work.dir,"/",ctl.name,sep="");
                    xmind.plt.runlist(val);
                }
            }
        }
    } else {
        return(NULL);
    }
}

xmind.plt.runlist <- function(val){
    ## Creates a Run Listing for PLT tools runs.
    r.saves <- list();

    ## Build list of runs and last runs to (eventually) build the HTML file.
    tmp.f <- function(x){
        con <- file(x,"r");
        ret <- paste(readLines(con),collapse="\n");
        close(con);
        return(ret);
    }
    eval(parse(text=paste(sapply(paste("../USERSCRIPTS/r-last-run/",list.files("../USERSCRIPTS/r-last-run/",".*\\.R$"),sep=""),tmp.f),collapse="\n")));
    run <<- 0;
    tmp.f <-  function(x){
        file <- paste("../",gsub("[.][^.]*$",".xmind",x),sep="");
        if (file.exists(file)){
            run <<- run + 1;
            cat("Updating xmind file: ",file,"\n");
            cat("Decompressing\n");
            unzip.xmind(file);
            cat("Parsing\n");
            xml <-  parse.content();
            id <- xmind.get.id(xml,x);
            cat("Adding PLT runs\n");
            plt <- xmind.get.id(xml,"^PLT Runs");
            if (!is.null(plt)){
                xml <-  xmind.rm(xml,plt);
            }
            df <- data.frame(Time=strftime(strptime(sort(r.saves[[x]],decreasing=TRUE),format="%y%m%d-%H%M%S"),format="%d %b %y, %I:%M:%S %p"));
            row.names(df) <- r.saves[[x]]
            xml.tag <<- xmind.table(df,"PLT Runs",extra=paste("p",run,sep=""),link.row="../USERSCRIPTS/xmind/xmind-");
            plt <- parse.tag();
            xml <- xmind.add(xml,plt,id);

            cat("Saving Xmind File\n");
            xml <- str.xmind(xml);
            write(xml,paste(xmind.dir,"content.xml",sep=""));
            zip.xmind(xmind.dir,file);
        }
    }
    sapply(val,tmp.f);
    return(NULL);
}

## ###########################################################################
## Xmind Manipulation routines
## ###########################################################################

xmind.add <-  function(content,new,parent=NULL){
    if (any(names(content) == "content")){
        return(list(xml.type=content$xml.type,
                    content=xmind.add(content$content,new,parent)));
    } else {
        if (!is.null(content$attr) && any(names(content$attr) == "id") &&
            ( (!is.null(parent) && content$attr[["id"]] == parent) ||
             (is.null(parent) && content$node == "topic")
             )
            ){
            if (is.null(parent)){
                parent <- content$attr[["id"]];
            }
                                        # Add new content
            tmp.f <- function(node){
                if (is.list(node)){
                    return(node$node);
                } else {
                    return("");
                }
            }
            if (any(sapply(content$children,tmp.f) == "children")){
                i.c <- which(sapply(content$children,tmp.f) == "children")[1];
                i.t <- which(sapply(content$children[[i.c]]$children,tmp.f) == "topics");
                children <- content$children;
                if (length(i.t) > 1){
                    for (i in i.t) {
                        if (children[[i.c]]$children[[i]]$attr[["type"]] == "attached") {
                            i.t <- i;
                            break;
                        }
                    }
                }
                tmp <- children[[i.c]]$children[[i.t]]$children;
                tmp[[length(tmp)+1]] <- new;
                children[[i.c]]$children[[i.t]]$children <- tmp;
                return(list(node=content$node,attr=content$attr,children=children));
            } else {
                                        # Add <children><topics type="attached">"
                topics <- list(node="topics",attr=list(type="attached"),children=list(new));
                child <- list(node="children",attr=NULL,children=list(topics));
                tmp <- content$children;
                tmp[[length(tmp)+1]] <-child;
                children <- tmp;
                return(list(node=content$node,attr=content$attr,children=children));
            }
        } else {
            if (is.null(content$children)){
                children <-  NULL;
            } else {
                children <- list();
                for (i in 1:length(content$children)){
                    if (is.list(content$children[[i]])){
                        children[[length(children)+1]] <- xmind.add(content$children[[i]],new,parent=parent);
                    } else {
                        children[[length(children)+1]] <- content$children[[i]];
                    }
                }
            }
            return(list(node=content$node,
                        attr=content$attr,
                        children=children));
        }
    }
}

xmind.rm <- function(content,id){
    ## Removes topic id from the content and returns new content
    if (any(names(content) == "content")){
        return(list(xml.type=content$xml.type,content=xmind.rm(content$content,id)));
    } else {
        if (!is.null(content$attr) && any(names(content$attr) == "id") &&
            content$attr[["id"]] == id) {
            return(NULL);
        } else {
            children <-  list();
            if (is.null(content$children)){
                children <- NULL;
            } else {
                for (i in 1:length(content$children)){
                    if (is.list(content$children[[i]])){
                        ret <- xmind.rm(content$children[[i]],id);
                        if (!is.null(ret)){
                            children[[length(children)+1]] <- ret;
                        } else {
                                        # Remove.
                        }
                    } else {
                        children[[length(children)+1]] <- content$children[[i]]
                    }
                }
                if (length(children) == 0){
                    children <- NULL;
                    ## Take out topics and children if empty.
                    if (content$node == "topics"){
                        return(NULL);
                    }
                    if (content$node == "children"){
                        return(NULL);
                    }
                }
            }
            return(list(node=content$node,attr=content$attr,children=children));
        }
    }
}

xmind.parent <-  function(content,id,lastid=NULL){
    ## Gets the ID of the parent topic
    if (any(names(content) == "content")){
        return(xmind.parent(content$content,id=id));
    } else {
        if (!is.null(content$attr) && any(names(content$attr) == "id")){
            if (content$attr[["id"]] == id) {
                return(lastid);
            } else {
                lastid <- content$attr[["id"]];
            }
        }
        for (i in 1:length(content$children)){
            if (is.list(content$children[[i]])){
                ret <- xmind.parent(content$children[[i]],id=id,lastid=lastid);
                if (!is.null(ret)){
                    return(ret);
                }
            }
        }
    }
    return(NULL);
}

xmind.get.id <- function(content,txt,lastid=NULL){
                                        # Gets id in label or text.
    if(any(names(content) == "content")){
        return(xmind.get.id(content$content,txt=txt,lastid=lastid));
    } else {
        if (!is.null(content$attr) && any(names(content$attr) == "id")){
            lastid <-  content$attr[["id"]];
        }
        if ((content$node == "label" || content$node == "title") &&
            !is.null(content$children) &&
            length(content$children) == 1
            ){
            if (regexpr(txt,content$children[[1]],ignore.case=TRUE) != -1) {
                return(lastid);
            } else {
                return(NULL);
            }
        } else {
            for (i in 1:length(content$children)){
                if (is.list(content$children[[i]])){
                    ret <- xmind.get.id(content$children[[i]],txt=txt,lastid=lastid)
                    if (!is.null(ret)){
                        return(ret);
                    }
                }
            }
        }
    }
    return(NULL);
}

parse.tag <-  function(indent.level=""){
    tag <-  sub("^([^>]*>).*","\\1",xml.tag);
    xml.tag <<-  substring(xml.tag,nchar(tag)+1);
    if (regexpr("\\/[ \t]*>$",tag) > -1){
        self.close = TRUE;
    } else {
        self.close = FALSE;
    }
    tag <-  sub("/?[ \t]*>$","",tag)
    node.name <- sub("<[ \t]*([^ \t]*).*","\\1",tag);
    a <- paste("list(\"",gsub("=","\"=",gsub("\"[ \t]","\",\"",gsub("\\","\\\\",gsub("\"xlink:href","\" xlink:href",substring(tag,nchar(node.name)+3)),fixed=TRUE))),")",sep="");
    if (a == "list(\")"){
        a <-  NULL;
    } else {
        tryCatch(a <- eval(parse(text=a)),error=function(e){cat("Error in Xmind File\n");cat(tag,"\n");cat(a,"\n");stop(e)});
    }
    if (self.close){
        children = NULL
    } else {
        children=list();
        while (regexpr(paste("^<[ \t]*/[ \t]*",node.name,"[ \t]*>",sep=""),xml.tag) == -1) {
            if (substring(xml.tag,1,1) == "<"){
                if (regexpr("^<[ \t]*/",xml.tag) > -1){
                    stop("Could not find XML node end ",node.name);
                } else {
                    children[[length(children)+1]] <- parse.tag(indent.level=paste(indent.level," ",sep=""));
                }
            } else {
                if (xml.tag == ""){
                    stop("Malformed XML node ",node.name);
                } else {
                    children[[length(children)+1]] <- sub("^([^<]*).*","\\1",xml.tag);

                    xml.tag <<- substring(xml.tag,nchar(children[[length(children)]])+1);
                }
            }
        }
        if (length(children) == 0){
            children <- NULL;
        }
        xml.tag <<- sub(paste("^<[ \t]*/[ \t]*",node.name,"[ \t]*>",sep=""),"",xml.tag);
    }
    if (node.name == "title" && is.null(children)){
        children <- list();
        children[[1]] <- "";
        ## Make sure title is not <title/>
    }
    return(list(node=node.name,attr=a,children=children));
}
get.content <- function(dir = xmind.dir) {
    ## Gets the contents of the unziped xmind file.
    con <- file(paste(dir,"content.xml",sep=""),"r");
    ret <- paste(readLines(con),collapse="\n");
    close(con);
    return(ret);
}
cat.tag <- function(content,indent.level=""){
    cat(paste(indent.level,"<",content$node,sep=""));
    if (!is.null(content$attr)){
        lvl <- paste("\n",indent.level,paste(rep(" ",nchar(content$node)),collapse=""));
        cat(paste(paste(c(" ",rep(lvl,length(content$attr)-1)),names(content$attr),"=\"",content$attr,"\"",sep=""),collapse=""));
    }
    if (is.null(content$children)){
        cat("/>\n");
    } else {
        cat(">\n");
        indent <-  paste(indent.level," ");
        for (i in 1:length(content$children)){
            if (is.list(content$children[[i]])){
                tryCatch(cat.tag(content$children[[i]],indent.level=indent),error=function(e){});
            } else {
                cat(indent,content$children[[i]],"\n");
            }
        }
        cat(paste(indent.level,"</",content$node,">\n",sep=""));
    }
}
cat.xmind <- function(content){
    cat(content$xml.type,"\n");
    cat.tag(content$content);
}
str.tag <- function(content){
    ret <- paste("<",content$node,sep="");
    if (!is.null(content$attr)){
        ret <- paste(ret," ",paste(paste(names(content$attr),"=\"",content$attr,"\"",sep=""),collapse=" "),sep="");
    }
    if (is.null(content$children)){
        ret <- paste(ret,"/>",sep="");
    } else {
        ret <- paste(ret,">",sep="");
        for (i in 1:length(content$children)){
            if (is.list(content$children[[i]])){
                ret <- paste(ret,str.tag(content$children[[i]]),sep="");
            } else {
                ret <- paste(ret,content$children[[i]],sep="");
            }
        }
        ret <- paste(ret,"</",content$node,">",sep="");
    }
    return(ret);
}
str.xmind <- function(content){
    return(paste(content$xml.type,str.tag(content$content),sep=""));
}

parse.content <- function(content=get.content()){
    xml.type <- gsub("(<[?]xml[^>]*>).*","\\1",content);
    content <-  substring(content,nchar(xml.type)+1);
    xml.tag <<- content;
    content <- list(xml.type=xml.type,content=parse.tag());
    return(content);
}

## ###########################################################################
## Change marker type for ID
## ###########################################################################

xmind.marker <-  function(content,id,type="other-question"){
    ## other-exclam -- warning on most recent run
    ## other-yes -- Successful Minimization
    ## other-no -- Run Terminated
    ## other-question -- Run Status Unknown
    if (any(names(content) == "content")){
        return(list(xml.type=content$xml.type,
                    content=xmind.marker(content$content,id,type)));
    } else {
        if (!is.null(content$attr) && any(names(content$attr) == "id") &&
            content$attr[["id"]] == id){
                                        # Add new content
            tmp.f <- function(node){
                if (is.list(node)){
                    return(node$node);
                } else {
                    return("");
                }
            }
            if (any(sapply(content$children,tmp.f) == "marker-refs")){
                i.m <- which(sapply(content$children,tmp.f) == "marker-refs");
                children <- content$children;
                children[[i.m]]$children <- list(list(node="marker-ref",attr=list("marker-id"=type),children=NULL));
                return(list(node=content$node,attr=content$attr,children=children));
            } else {
                ## Add marker.
                tmp <- content$children;
                tmp[[length(tmp)+1]] <- list(node="marker-refs",attr=NULL,
                                             children=list(list(node="marker-ref",attr=list("marker-id"=type),children=NULL)));
                return(list(node=content$node,attr=content$attr,children=tmp));
            }
        } else {
            if (is.null(content$children)){
                children <-  NULL;
            } else {
                children <- list();
                for (i in 1:length(content$children)){
                    if (is.list(content$children[[i]])){
                        children[[length(children)+1]] <- xmind.marker(content$children[[i]],id,type);
                    } else {
                        children[[length(children)+1]] <- content$children[[i]];
                    }
                }
            }
            return(list(node=content$node,
                        attr=content$attr,
                        children=children));
        }
    }
}

## #################################################################
## Get Xmind Tables for certain types of properties.
## #################################################################

xmind.errors.notes <-  function(summary){
    table <- data.frame("Errors and Notes"=gsub("\\\\+","",summary$errors));
    names(table) <-  "Errors and Notes"
    row.names(table) <-  paste(row.names(table),".",sep="");
    return(xmind.table(table,"Errors/Notes",extra="e"));
}

xmind.theta.table <-  function(summary,dig=NULL,sigdig=NULL,...){
    d <- summary$theta.tab;
    d$rse = nm.round(d$rse,dig=dig,sigdig=sigdig,single=TRUE)
    d$se = nm.round(d$se,dig=dig,sigdig=sigdig,single=TRUE)
    d$est = nm.round(d$est,dig=dig,sigdig=sigdig,single=TRUE)
    d$cil = nm.round(d$cil,dig=dig,sigdig=sigdig,single=TRUE)
    d$ciu = nm.round(d$ciu,dig=dig,sigdig=sigdig,single=TRUE)
    d$ci = paste("(",d$cil,",",d$ciu,")",sep="");
    d <-  d[,!(names(d) %in% c("cil","ciu"))];
    row.names(d) <-  paste("THETA(",row.names(d),")",sep="");
    names(d) <-
        gsub("^units$","Units",
             gsub("^ci$",paste(summary$ci*100,"% Confidence Interval",sep=""),
                  gsub("^rse$","Relative SE%",
                       gsub("^se$","SE",
                            gsub("^est$","Estimate",
                                 gsub("^theta$","Theta Label",
                                      names(d)))))))
    return(xmind.table(d,"Theta Estimates",extra="t"));

}
xmind.eta.table <-  function(pr,dig=NULL,sigdig=NULL,...){
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
        gsub("^etabar.p$","p-value etabar",
             gsub("^etabar.se$","SE etabar",
                  gsub("^etabar$","etabar",
                       gsub("^shrink$","Shrinkage%",
                            gsub("^cv$","CV%",
                                 gsub("^ci$",paste(pr$ci*100,"% Confidence Interval",sep=""),
                                      gsub("^rse$","Relative SE%",
                                           gsub("^se$","SE",
                                                gsub("^est$","Estimate",
                                                     gsub("^eta$","eta",
                                                          names(tab)))))))))))
    return(xmind.table(tab,"Eta Estimates",extra="o"));
}

xmind.eps.table <-  function(pr=NULL,dig=NULL,sigdig=NULL,...){
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
            gsub("^shrink$","Shrinkage%",
                 gsub("^cv$","CV%",
                      gsub("^ci$",paste(pr$ci*100,"% Confidence Interval",sep=""),
                           gsub("^rse$","Relative SE%",
                                gsub("^se$","SE",
                                     gsub("^est$","Estimate",
                                          gsub("^eta$","eta",
                                               names(tab))))))));
        return(xmind.table(tab,"Eps Estimates",extra="s"));
    } else {
        return("");
    }
}
xmind.get.links <- function(folded=TRUE,link.prefix=""){
                                        # Data Link
                                        # Graphics
    ## Look through repository?
    j <<- 0;
    tmp.f <- function(x) {
        j <<- j + 1;
        ret <- paste("<topic id=\"xl",j,
                     xmind.tm(),
                     "\" structure-class=\"org.xmind.ui.spreadsheet\" timestamp=\"",
                     xmind.timestamp(),
                     "\"",
                     ifelse(folded," branch=\"folded\"",""),
                     ">",
                     "<extensions>",
                     "<extension provider=\"org.xmind.ui.spreadsheet\">",
                     "<content>",
                     "<columns>",
                     "<column>Link</column>",
                     "</columns>",
                     "</content>",
                     "</extension>",
                     "</extensions>",
                     "<title>",
                     x,
                     "</title><children>",
                     "<topics type=\"attached\">",
                     sep=""
                     );
        lst <- par.links[[x]];
        i <<-  0;
        tmp.g <- function(y,x){
            if(file.exists(par.links[[x]][[y]])){
                i <<-  i + 1;
                return(paste("<topic id=\"lr",i,j,xmind.tm(),"\"><title>",
                             i,".</title>",
                             "<children><topics type=\"attached\">",
                             "<topic id=\"ld",i,j,
                             xmind.tm(),
                             "\" xlink:href=\"file:",link.prefix,
                             par.links[[x]][[y]],"\"><title>",
                             y,
                             "</title><labels><label>Link</label></labels></topic>",
                             "</topics></children></topic>",
                             sep=""));
            } else {
                return("");
            }
        }
        tmp <- paste(unlist(lapply(names(lst),tmp.g,x=x)),collapse="");
        if (tmp == ""){
            return("");
        } else {
            return(paste(ret,tmp,"</topics></children></topic>",sep=""));
        }
    }
    return(paste("<topic id=\"lk",xmind.tm(),"\"><title>Links</title><children><topics type=\"attached\">",
                 paste(unlist(lapply(names(par.links),tmp.f)),collapse=""),
                 "</topics></children></topic>",sep="")
           );
}

## #################################################################
## Blank Manifest & Meta information for Xmind; No thumbnails.
## #################################################################

xmind.manifest <- function(){
    return("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><manifest xmlns=\"urn:xmind:xmap:xmlns:manifest:1.0\"><file-entry full-path=\"content.xml\" media-type=\"text/xml\"/><file-entry full-path=\"META-INF/\" media-type=\"\"/><file-entry full-path=\"META-INF/manifest.xml\" media-type=\"text/xml\"/><file-entry full-path=\"meta.xml\" media-type=\"text/xml\"/></manifest>");
}

xmind.meta <-  function(){
    return("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><meta xmlns=\"urn:xmind:xmap:xmlns:meta:2.0\" version=\"2.0\"/>");
}

## #################################################################
## Timestamp function for Xmind.
## #################################################################

xmind.timestamp <-  function(){
    return(paste(round(as.numeric(difftime(Sys.time(),ISOdatetime(1970,1,1,0,0,0,tz="GMT"),units="secs"))*1000)));
}
xmind.tm <-  function(){
    return(format(Sys.time(),"%Y%m%d%H%M%S"));
}
## #################################################################
## Create a Xmind spread-sheet topic from a table.
## #################################################################

xmind.table <-  function(tab,title="Spreadsheet",folded=TRUE,extra="",link.row=NULL,
                         link.ext=".xmind",
                         topic.link=NULL){
    cols <- names(tab);
    ret <- paste("<topic id=\"xt",extra,
                 xmind.tm(),
                 "\" structure-class=\"org.xmind.ui.spreadsheet\" timestamp=\"",
                 xmind.timestamp(),
                 "\"",
                 ifelse(folded," branch=\"folded\"",""),
                 ifelse(is.null(topic.link),"",paste(" xlink:href=\"file:",topic.link,"\"",sep="")),
                 ">",
                 "<extensions>",
                 "<extension provider=\"org.xmind.ui.spreadsheet\">",
                 "<content>",
                 "<columns>",
                 paste(paste("<column>",cols,"</column>",sep=""),collapse=""),
                 "</columns>",
                 "</content>",
                 "</extension>",
                 "</extensions>",
                 "<title>",
                 title,
                 "</title><children>",
                 "<topics type=\"attached\">",
                 sep=""
                 );
    rows <-  row.names(tab);
    tmp.f <-  function(x){
        return(gsub("<","&lt;",gsub(">","&gt;",gsub("$^{.*?}$","",gsub("%.GREYBG.\n","",gsub("[$]-[$]","-",gsub("[$].*cdot.*10.[{](-?[0-9]+)[}][$]","e\\1",paste(x))),fixed=TRUE),perl=TRUE))));
    }
    if (length(tab[,1]) > 1){
        d <- sapply(tab,tmp.f);
    } else {
        d <-  data.frame(t(sapply(tab,tmp.f)));
    }
    row.names(d) <-  rows;
    names(d) <- cols;
    len <-  length(d[,1]);
    tab.f <-  function(i){
        paste("<topic id=\"xr",extra,i,xmind.tm(),"\"><title>",
              rows[i],
              "</title>",
              "<children><topics type=\"attached\">",
              paste(paste("<topic ",
                          ifelse(is.null(link.row),"",paste("xlink:href=\"file:",link.row,rows[i],link.ext,"\" ",sep="")),
                          "id=\"xd",extra,i,
                          seq(1,length(d[i,])),
                          xmind.tm(),
                          "\"><title>",
                          paste(d[i,]),
                          "</title><labels><label>",
                          cols,"</label></labels></topic>",
                          sep=""),
                    collapse=""),
              "</topics></children></topic>",sep="");
    }
    ret <-  paste(ret,paste(sapply(1:len,tab.f),collapse=""),sep="");
    ret <-  paste(ret,
                  "</topics>",
                  "</children></topic>",
                  sep="");
    return(ret);
}



## #################################################################
## Functions for creating a decompressed xmind.
## #################################################################

unlink.xmind <- function(){
    cat("Cleaning up old temporary information.");
    ex.dir <- paste(tempdir(),"\\","xmind\\",sep="");
    unlink(paste(ex.dir,"content.xml",sep=""),recursive = TRUE)
    unlink(paste(ex.dir,"styles.xml",sep=""),recursive = TRUE)
    unlink(paste(ex.dir,"META-INF/manifest.xml",sep=""),recursive = TRUE)
    unlink(paste(ex.dir,"META-INF",sep=""),recursive = TRUE)
    unlink(paste(ex.dir,"Thumbnails/thumbnail.jpg",sep=""),recursive = TRUE)
    unlink(paste(ex.dir,"Thumbnails",sep=""),recursive = TRUE)
    tmp.f <-  function(x) {
        unlink(paste(ex.dir,"attachments/",x,sep=""),recursive = TRUE)
    }
    sapply(list.files(paste(ex.dir,"attachments/",sep="")),tmp.f);
    unlink(paste(ex.dir,"attachments",sep=""),recursive = TRUE)
    tmp.f <-  function(x) {
        unlink(paste(ex.dir,"markers/",x,sep=""),recursive = TRUE)
    }
    sapply(list.files(paste(ex.dir,"markers/",sep="")),tmp.f);
    unlink(paste(ex.dir,"meta.xml",sep=""),recursive=TRUE);
    cat("\n");
}
create.xmind <- function(content.xml,xmind.file){
    ## Creates Xmind file.
    ex.dir <-  paste(tempdir(),"\\","xmind\\",sep="");
    try(unlink.xmind())
    try(dir.create(ex.dir));
    print(ex.dir)
    dir.create(paste(ex.dir,"\\META-INF\\",sep=""));
    ## Create basic manifest.
    manifest.xml <-  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><manifest xmlns=\"urn:xmind:xmap:xmlns:manifest:1.0\"><file-entry full-path=\"content.xml\" media-type=\"text/xml\"/><file-entry full-path=\"META-INF/\" media-type=\"\"/><file-entry full-path=\"META-INF/manifest.xml\" media-type=\"text/xml\"/></manifest>";
    write(manifest.xml,paste(ex.dir,"\\META-INF\\manifest.xml",sep=""));
    ## Write content.
    write(content.xml,paste(ex.dir,"\\content.xml",sep=""));
    ## Zip Xmind to output.
    zip.xmind(ex.dir,xmind.file);
    try(unlink.xmind());
}

unzip.xmind <-  function(xmind.file){
    ## Returns the directory of the extraction.
    zip.file <- gsub("xmind$","zip",xmind.file);
    ex.dir <- paste(tempdir(),"\\","xmind\\",sep="");
    try(unlink.xmind())
    try(dir.create(ex.dir))
    wd <-  getwd();
    ## For some reason throws an error if you specify directory from unzip.
    file.rename(xmind.file,zip.file);
    setwd(ex.dir)
    xmind.fail <<- FALSE;
    tryCatch(unzip(xmind.file),error = function(e) { xmind.fail <<- TRUE; },
             warning = function(e) {xmind.fail <<- TRUE; });
    setwd(wd);
    if (xmind.fail){
        if (!unzip7z(zip.file,ex.dir)){
            if (!unix.zip(zip.file,ex.dir)){
                file.rename(zip.file,xmind.file);
                stop("Failed to unzip xmind");
            }
        }
    }
    if (file.exists(zip.file)){
        file.rename(zip.file,xmind.file);
    }
    xmind.dir <<- ex.dir;
    return(ex.dir);
}
unzip7z <- function(zip,dir){
    cmd <- paste(get7z()," x ",zip," -o",dir,sep="");
    if (Sys.getenv("ProgramFiles") != ""){
        cmd <- paste("\"",Sys.getenv("COMSPEC"),"\" /C \"",cmd,"\"",sep="");
    }

    system(cmd);
    if (file.exists(paste(dir,"/content.xml",sep=""))){
        return(TRUE);
    } else {
        return(FALSE);
    }
}

## #################################################################
## Functions for creating a compressed xmind
## #################################################################

zip.xmind <- function(dir,xmind.out){
    if(!zip7z(dir,xmind.out)){
        if(!unix.zip(dir,xmind.out)){
            if(!winzip(dir,xmind.out)){
                cat("Could not compress and create an Xmind.  Requires 7-Zip, unzip, OR WinZip\n");
                return(FALSE);
            }
        }
    }
    return(TRUE);
}

unix.zip <- function(dir,xmind.out){
    fix.dir <- paste(paste(strsplit(dir,"[\\/]")[[1]],collapse="/"),sep="");
    xmind.file <- paste(strsplit(xmind.out,"[\\/]")[[1]],collapse="\\");
    zip.file <-  gsub("xmind$","zip",xmind.file);
    if (Sys.getenv("ProgramFiles") != ""){
        cmd <- paste("\"",Sys.getenv("COMSPEC"),"\" /C \"zip -r -1 ../tmp.zip *\"",sep="");
    } else {
        cmd <- paste("zip -r -1 ../tmp.zip *",sep="");
    }
    old <- getwd();
    fix.dir <- gsub("[^/]*$","",fix.dir);
    setwd(fix.dir);
    is.true <- FALSE;
    if (file.exists("../tmp.zip")) {
        is.true <- TRUE;
        file.remove("../tmp.zip");
    }
    try(system(cmd),silent=TRUE);
    setwd(old);
    file.rename(paste(fix.dir,"/../tmp.zip",sep=""),xmind.file);
    if (is.true) {
        return(TRUE);
    } else {
        return(FALSE);
    }
}
get7z <-  function(){
    z7 <-  c();
                                        # Check for portable version.
    if (file.exists("../USERSCRIPTS/lib/7z.exe") &&
        file.exists("../USERSCRIPTS/lib/7z.dll"))
    {
        return("..\\USERSCRIPTS\\lib\\7z.exe");
    }
    if (Sys.getenv("ProgramFiles") != ""){
        z7 <-  paste(Sys.getenv("ProgramFiles"),"\\7-Zip\\7z.exe",
                     sep="");
        if (!file.exists(z7)) {
            z7 <-  c();
        }
        if (length(z7) == 0) {
            drives <- c("c:\\","d:\\","e:\\","f:\\","g:\\","h:\\","i:\\","j:\\","k:\\","l:\\","m:\\","n:\\","o:\\","p:\\","q:\\","r:\\","s:\\","t:\\","u:\\","v:\\","w:\\","x:\\","y:\\","z:\\");
            w <- sapply(paste(drives,"PortableApps\\7-ZipPortable\\App\\7-Zip\\7z.exe",sep=""),file.exists);
            if (any(w)){
                z7 <- paste(drives[which(w)[1]],"PortableApps\\7-ZipPortable\\App\\7-Zip\\7z.exe",sep="");
            }
        }
        return(z7);
    } else {
        return("7z");
    }
}
zip7z <- function(dir,xmind.out){
    fix.dir <- paste(paste(strsplit(dir,"[\\/]")[[1]],collapse="\\"),"\\*",sep="");
    xmind.file <- paste(strsplit(xmind.out,"[\\/]")[[1]],collapse="\\");
    zip.file <-  gsub("xmind$","zip",xmind.file);
    cmd <- paste(get7z()," a ",zip.file," ",fix.dir," -r -mx=1",sep="");
    if (Sys.getenv("ProgramFiles") != ""){
        cmd <- paste("\"",Sys.getenv("COMSPEC"),"\" /C \"",cmd,"\"",sep="");
    }
    system(cmd);
    if (file.exists(zip.file)){
        file.rename(zip.file,xmind.file)
        return(TRUE);
    } else {
        return(FALSE);
    }
}
winzip <-  function(dir,xmind.out) {
    if (Sys.getenv("ProgramFiles") != ""){
        winzip <-  Sys.glob(paste(Sys.getenv("ProgramFiles"),"\\WinZip\\winzip*.exe",
                                  sep=""));
        if (length(winzip) != 1){
            return(FALSE);
        }

    }
    xmind.zip <-  strsplit(gsub("xmind$","zip",xmind.out),"[\\/]")[[1]];
    xmind.output.dir <- paste(xmind.zip[-length(xmind.zip)],collapse="\\");
    full.zip <- paste(xmind.zip,collapse="/");
    full.xmind <- gsub("zip$","xmind",full.zip);
    xmind.zip <- xmind.zip[length(xmind.zip)];
    xmind.dir <- paste(strsplit(dir,"[\\/]")[[1]],collapse="\\");
    files <-  Sys.glob(paste(xmind.dir,"\\*",sep=""));
    files <- paste(seq(0,length(files)-1),"=FI-",files,sep="");
    files <- gsub("[-](.*)(META-INF|Thumbnails)(.*)","+\\1\\2\\3\\\\",files);
    ret <-  c("[version]",
              "WJF-version=1",
              "[files]",
              files,
              "[output]",
              paste("base=",xmind.zip),
              "baseappend=0",
              paste("folder=",xmind.output.dir),
              "folderappend=0",
              "log=none",
              "logfolder=",
              "logoverwrite=1",
              "logtojobfolder=1",
              "[options]",
              "jobflags=0001",
              "compression=2",
              "span=1",
              "split=0",
              "splitunit=bytes",
              "pwmode=0",
              "cryptmode=0",
              "pathmode=1",
              "CdWriteSpeed=-1",
              "CdFinalize=1",
              "[schedule]",
              "type=99",
              "[wizard]",
              "UseVars=0");
    file.cnt <- paste(paste(ret,collapse="\n"),"\n");
    winzip.job <- paste(tempdir(),"\\","winzip.wjf",sep="");
    if (file.exists(winzip.job)){
        unlink(winzip.job);
    }
    write(file.cnt,winzip.job);
    cmd <- paste("\"",Sys.getenv("COMSPEC"),"\" /C \"\"",winzip,"\" \"",winzip.job,"\"\"",sep="");
    system(cmd);
    unlink(winzip.job);
    if (file.exists(full.zip)){
        file.rename(full.zip,full.xmind)
        return(TRUE);
    } else {
        return(FALSE);
    }
}


######################################################################
### esn-xmind.R ends here
