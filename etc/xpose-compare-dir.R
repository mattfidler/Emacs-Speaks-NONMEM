        ## Start: tmp.R
        if (xpcmp.cur != " tmp" & (all.dir || any(xp.cmp == " tmp"))){
        xpose.merge(0,tab.suffix="tmp");

        xpctmp<- xpose.data(0,tab.suffix="tmp");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="tmp.est",deriv.tab.suffix="tmp.deriv"));
        xpctmp@Runno <- " tmp";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="tmp.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="tmp");
        }

        }
        ## Stop: tmp.R
        ## Start: esn-pdx.R
        if (xpcmp.cur != " esn-pdx" & (all.dir || any(xp.cmp == " esn-pdx"))){
        xpose.merge(0,tab.suffix="esn-pdx.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-pdx.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-pdx.el.est",deriv.tab.suffix="esn-pdx.el.deriv"));
        xpctmp@Runno <- " esn-pdx";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-pdx.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-pdx");
        }

        }
        ## Stop: esn-pdx.R
        ## Start: esn-options-header-pdx-small.R
        if (xpcmp.cur != " esn-options-header-pdx-small" & (all.dir || any(xp.cmp == " esn-options-header-pdx-small"))){
        xpose.merge(0,tab.suffix="esn-options-header-pdx-small.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-options-header-pdx-small.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-options-header-pdx-small.el.est",deriv.tab.suffix="esn-options-header-pdx-small.el.deriv"));
        xpctmp@Runno <- " esn-options-header-pdx-small";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-options-header-pdx-small.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("ETA1");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-options-header-pdx-small");
        }

        }
        ## Stop: esn-options-header-pdx-small.R
        ## Start: esn-update.R
        if (xpcmp.cur != " esn-update" & (all.dir || any(xp.cmp == " esn-update"))){
        xpose.merge(0,tab.suffix="esn-update.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-update.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-update.el.est",deriv.tab.suffix="esn-update.el.deriv"));
        xpctmp@Runno <- " esn-update";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-update.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-update");
        }

        }
        ## Stop: esn-update.R
        ## Start: esn-options-header-small.R
        if (xpcmp.cur != " esn-options-header-small" & (all.dir || any(xp.cmp == " esn-options-header-small"))){
        xpose.merge(0,tab.suffix="esn-options-header-small.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-options-header-small.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-options-header-small.el.est",deriv.tab.suffix="esn-options-header-small.el.deriv"));
        xpctmp@Runno <- " esn-options-header-small";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-options-header-small.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("ETA1");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-options-header-small");
        }

        }
        ## Stop: esn-options-header-small.R
        ## Start: esn-xpose.R
        if (xpcmp.cur != " esn-xpose" & (all.dir || any(xp.cmp == " esn-xpose"))){
        xpose.merge(0,tab.suffix="esn-xpose.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-xpose.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-xpose.el.est",deriv.tab.suffix="esn-xpose.el.deriv"));
        xpctmp@Runno <- " esn-xpose";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-xpose.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-xpose");
        }

        }
        ## Stop: esn-xpose.R
        ## Start: esn-plt.R
        if (xpcmp.cur != " esn-plt" & (all.dir || any(xp.cmp == " esn-plt"))){
        xpose.merge(0,tab.suffix="esn-plt.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-plt.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-plt.el.est",deriv.tab.suffix="esn-plt.el.deriv"));
        xpctmp@Runno <- " esn-plt";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-plt.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-plt");
        }

        }
        ## Stop: esn-plt.R
        ## Start: esn-code.R
        if (xpcmp.cur != " esn-code" & (all.dir || any(xp.cmp == " esn-code"))){
        xpose.merge(0,tab.suffix="esn-code.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-code.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-code.el.est",deriv.tab.suffix="esn-code.el.deriv"));
        xpctmp@Runno <- " esn-code";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-code.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-code");
        }

        }
        ## Stop: esn-code.R
        ## Start: esn-help.R
        if (xpcmp.cur != " esn-help" & (all.dir || any(xp.cmp == " esn-help"))){
        xpose.merge(0,tab.suffix="esn-help.el");

        xpctmp<- xpose.data(0,tab.suffix="esn-help.el");
        if (!is.null(xpctmp)){

        xpctmp@Prefs@Xvardef$ipre <- xpctmp@Prefs@Xvardef$ipred;
        xpctmp@Prefs@Xvardef$iwre <- xpctmp@Prefs@Xvardef$iwres;
        try(xpose.calculate.cwres(xpctmp,est.tab.suffix="esn-help.el.est",deriv.tab.suffix="esn-help.el.deriv"));
        xpctmp@Runno <- " esn-help";


        xpctmp@Prefs@Labels$TI <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$DUR <- "Duration of Infusion (nil)";
        xpctmp@Prefs@Labels$TAD <- "Time After Dose (nil)";
        xpctmp@Prefs@Labels$TSLD <- "Time Since Last Dose (nil)";
        xpctmp@Prefs@Labels$TSFD <- "Time Since First Dose (nil)";
        xpctmp@Prefs@Labels$TIME <- "Time (nil)";
        write.csv(xpctmp@Data,file="esn-help.csv",row.names=FALSE,quote=FALSE,na=".");
        patab.tv[[xpctmp@Runno]] <- c("");
        patab.ind[[xpctmp@Runno]] <- c("");

        xpcmp[[xpctmp@Runno]] <- xpctmp;

        save.sum(xpdb=xpctmp,base.file="esn-help");
        }

        }
        ## Stop: esn-help.R
