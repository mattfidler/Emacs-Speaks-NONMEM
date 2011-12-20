
## Change log:
## 08-Feb-2010    Matthew L. Fidler
##    Made A call to UserScript externally.  Otherwise PLT tools stops prematurely.
library(esnR)
if (!file.exists("../USERSCRIPTS/NO-SCRIPT.txt")) {
    if (Sys.getenv("ProgramFiles") != ""){
        ## system(paste(R.home(),"\\bin\\R.exe -f ..\\USERSCRIPTS\\UserScript.R",sep=""))
        source("../USERSCRIPTS/UserScript.R",verbose=TRUE);
    } else {
        ## system("R -f ..\\USERSCRIPTS\\UserScript.R");
        source("../USERSCRIPTS/UserScript.R",verbose=TRUE);
    }
}
