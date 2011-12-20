### html.R --- HTML PLT tools summary document.
##
## Filename: html.R
## Description:
## Author: Matthew L. Fidler
## Maintainer:
## Created: Tue Feb  2 14:56:11 2010 (-0600)
## Version: 0.
##     Update #: 30
## URL: esnm.sourceforge.net
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
## 02-Feb-2010    Matthew L. Fidler
##
##    Added HTML file as a template in this document instead of a separate file.
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

html.template <- "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Summary</title>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\" />

<script language=\"javascript\" type=\"text/javascript\">
function toggleInfo(timestamp) {
/*cls*/
    var abs = document.getElementById(\"id_\"+timestamp);
    if(abs.className.indexOf('noshow') == -1) {
       abs.className = 'noshow';
       document.getElementById(\"t1foot\").className = '';
       document.getElementById(\"t2foot\").className = '';
       document.getElementById(\"t3foot\").className = '';
/*unfilter*/
    } else {
       abs.className = '';
       document.getElementById(\"t1foot\").className = 'noshow';
       document.getElementById(\"t2foot\").className = 'noshow';
       document.getElementById(\"t3foot\").className = 'noshow';
/*filter*/
    }
}
</script>
<script language=\"javascript\" type=\"text/javascript\">
/**
 * Copyright (c)2005-2009 Matt Kruse (javascripttoolbox.com)
 *
 * Dual licensed under the MIT and GPL licenses.
 * This basically means you can use this code however you want for
 * free, but don't claim to have written it yourself!
 * Donations always accepted: http://www.JavascriptToolbox.com/donate/
 *
 * Please do not link to the .js files on javascripttoolbox.com from
 * your site. Copy the files locally to your server instead.
 *
 */
eval(function(p,a,c,k,e,d){e=function(c){return(c<a?'':e(parseInt(c/a)))+((c=c%a)>35?String.fromCharCode(c+29):c.toString(36))};if(!''.replace(/^/,String)){while(c--){d[e(c)]=k[c]||e(c)}k=[function(e){return d[e]}];e=function(){return'\\\\w+'};c=1};while(c--){if(k[c]){p=p.replace(new RegExp('\\\\b'+e(c)+'\\\\b','g'),k[c])}}return p}('6 1C=(9(){6 q={};q.23=9(a,b){h(a==b)?0:(a<b)?-1:1};q[\\'2M\\']=q.23;q.2S=9(33){h 9(w){5(X(w)==\"3B\"){w=4n(w.1R(/^[^\\\\d\\\\.]*([\\\\d., ]+).*/g,\"$1\").1R(1e 17(\"[^\\\\\\\\\\\\d\"+33+\"]\",\"g\"),\\'\\').1R(/,/,\\'.\\'))||0}h w||0}};q.1n=9(a,b){h q.1n.O(a)-q.1n.O(b)};q.1n.O=q.2S(\".\");q.1I=9(a,b){h q.1I.O(a)-q.1I.O(b)};q.1I.O=q.2S(\",\");q.1T=9(a,b){h q.23(q.1T.O(a),q.1T.O(b))};q.1T.O=9(w){5(w==C){h\"\"}h(\"\"+w).4k()};q.4f=q.1n;q.4e=q.1I;q.16=9(a,b){h q.1n(q.16.O(a),q.16.O(b))};q.16.2P=9(19){19=+19;5(19<4G){19+=4u}M 5(19<4w){19+=4x}h 19};q.16.2Y=[{1S:/(\\\\d{2,4})-(\\\\d{1,2})-(\\\\d{1,2})/,f:9(x){h(1e 2I(q.16.2P(x[1]),+x[2],+x[3])).2K()}},{1S:/(\\\\d{1,2})[\\\\/-](\\\\d{1,2})[\\\\/-](\\\\d{2,4})/,f:9(x){h(1e 2I(q.16.2P(x[3]),+x[1],+x[2])).2K()}},{1S:/(.*\\\\d{4}.*\\\\d+:\\\\d+\\\\d+.*)/,f:9(x){6 d=1e 2I(x[1]);5(d){h d.2K()}}}];q.16.O=9(w){6 m,v,f=q.16.2Y;F(6 i=0,L=f.y;i<L;i++){5(m=w.1Q(f[i].1S)){v=f[i].f(m);5(X(v)!=\"2H\"){h v}}}h 43};h q})();6 1K=(9(){9 G(o){h(X o!=\"2H\")};9 14(o,1a){h 1e 17(\"(^|\\\\\\\\s)\"+1a+\"(\\\\\\\\s|$)\").41(o.10)};9 1l(o,1a){6 c=o.10||\"\";5(G(c)&&!14(o,1a)){o.10+=(c?\" \":\"\")+1a}};9 1k(o,1a){6 c=o.10||\"\";o.10=c.1R(1e 17(\"(^|\\\\\\\\s)\"+1a+\"(\\\\\\\\s|$)\"),\"$1\")};9 Y(o,34){6 c=o.10;5(c.1Q(1e 17(\"(^|\\\\\\\\s)\"+34+\"([^ ]+)\"))){h 17.$2}h C};9 1A(o){5(1c.3g){6 3f=1c.3g;h(1A=9(o){h\\'24\\'==3f(o,C).3Y(\\'1B\\')})(o)}M 5(1c.3i){h(1A=9(o){h\\'24\\'==o.3i[\\'1B\\']})(o)}h(1A=9(o){h\\'24\\'==o.2N[\\'1B\\']})(o)};9 1M(o,a,b){5(o!=C&&o.W){5(o.W==a||(b&&o.W==b)){h o}1q(o=o.1Z){5(o.W&&(o.W==a||(b&&o.W==b))){h o}}}h C};9 3a(2J,3k){F(6 i=2;i<3j.y;i++){6 a=3j[i];5(G(2J[a])){3k[a]=2J[a]}}}6 7={3x:\"7-45\",3y:\"7-12:\",3J:\"7-29\",3z:\"7-29:\",3A:\"3X 4a q\",2v:\"7-37-4b\",2x:\"7-37-1d\",2e:\"7-36\",3E:\"7-36:\",2X:\"7-46\",2D:\"7-2y\",3q:\"7-38\",2w:\"7-47\",3l:\"7-38-3c:\",3d:\"7-3c:\",2W:\"3R: 3P\",3v:\"7-2L:\",3u:\"7-B:\",3H:\"7-B-3Q:\",3t:\"7-B-2i:\"};7.S={};7.3b=1;7.T=9(o,p){5(o!=C&&o.W&&o.W!=\"2d\"){o=1M(o,\"2d\")}5(o==C){h C}5(!o.I){6 I=C;1y{6 I=\"3S\"+(7.3b++)}1q(1j.1J(I)!=C);o.I=I}n.S[o.I]=n.S[o.I]||{};5(p){3a(p,n.S[o.I],\"12\",\"3I\",\"1U\",\"R\",\"D\",\"1d\",\"B\",\"P\")}h o};7.1s=9(t,E,1o,1H){t=n.T(t);5(t==C){h}5(E!=\"3p\"){n.2C(t.3V,1o,1H)}5(E!=\"1g\"){n.2C(t.3U,1o,1H)}};7.2C=9(1v,1o,1H){5(1v!=C){5(1v.J&&1v.J.y&&1v.J.y>0){6 J=1v.J;F(6 j=0,1r=J.y;j<1r;j++){6 V=J[j];5(V.A&&V.A.y&&V.A.y>0){6 A=V.A;F(6 k=0,39=A.y;k<39;k++){6 35=A[k];1o.4i(n,35,1H)}}}}}};7.1h=9(Z){6 2j=Z.1Z;6 A=2j.A;5(A&&A.y){5(A.y>1&&A[A.y-1].1D>0){(n.1h=9(Z){h Z.1D})(Z)}F(6 i=0,L=A.y;i<L;i++){5(2j.A[i]==Z){h i}}}h 0};7.1E={\\'3K\\':9(H){5(G(H.1w)&&H.E&&((H.E!=\"4v\"&&H.E!=\"4y\")||H.4z)){h H.1w}h\"\"},\\'3O\\':9(H){5(H.22>=0&&H.2t){h H.2t[H.22].3L}h\"\"},\\'4d\\':9(H){h H.1a||\"\"}};7.1z=9(Z,3e){5(3e&&G(Z.1W)){h Z.1W}5(!Z.1G){h\"\"}6 1G=Z.1G;6 1u=\"\";F(6 i=0,L=1G.y;i<L;i++){6 H=1G[i];6 E=H.4A;5(E==1){6 2o=H.W;5(n.1E[2o]){1u+=n.1E[2o](H)}M{1u+=n.1z(H)}}M 5(E==3){5(G(H.1W)){1u+=H.1W}M 5(G(H.1E)){1u+=H.1E}}}h 1u};7.1m={};7.2g=9(1t){5(!G(1t.1D)){h C}6 1p=1M(1t,\"2d\");6 2E=1t.1Z.15+\"-\"+n.1h(1t);5(G(n.1m[1p.I])){h n.1m[1p.I][2E]}6 1i=[];n.1m[1p.I]={};6 3h=1M(1t,\"1g\");6 2G=3h.31(\\'4r\\');F(6 i=0;i<2G.y;i++){6 A=2G[i].A;F(6 j=0;j<A.y;j++){6 c=A[j];6 15=c.1Z.15;6 3m=15+\"-\"+n.1h(c);6 2A=c.2A||1;6 2F=c.2F||1;6 1L;5(!G(1i[15])){1i[15]=[]}6 m=1i[15];F(6 k=0;k<m.y+1;k++){5(!G(m[k])){1L=k;4h}}n.1m[1p.I][3m]=1L;F(6 k=15;k<15+2A;k++){5(!G(1i[k])){1i[k]=[]}6 2Z=1i[k];F(6 l=1L;l<1L+2F;l++){2Z[l]=\"x\"}}}}h n.1m[1p.I][2E]};7.q=9(o,p){6 t,8,21=C;5(X(p)==\"9\"){p={R:p}}p=p||{};5(!G(p.D)){p.D=n.2g(o)||0}p.R=p.R||1C[\\'2M\\'];t=n.T(o,p);8=n.S[t.I];5(G(8.2z)&&8.2z==8.D&&G(8.2s)){8.1d=!8.2s}M{8.1d=!!p.1d}8.2z=8.D;8.2s=!!8.1d;6 R=8.R;5(X(R.O)==\"9\"){21=8.R.O;R=1C.23}n.1s(t,\"1g\",9(N){5(14(N,n.2e)){1k(N,n.2v);1k(N,n.2x);5(8.D==7.2g(N)&&(Y(N,7.2e))){1l(N,8.1d?n.2v:n.2x)}}});6 K=t.1O;5(K==C||K.y==0){h}6 3C=(8.1d)?9(a,b){h R(b[0],a[0])}:9(a,b){h R(a[0],b[0])};6 1U=!!8.1U;6 D=8.D;F(6 i=0,L=K.y;i<L;i++){6 18=K[i],1f=18.J,J=[];5(!14(18,7.2X)){6 Q,U=0;5(Q=1f[U]){1y{5(2l=Q.A){6 1Y=(D<2l.y)?n.1z(2l[D],1U):C;5(21)1Y=21(1Y);J[U]=[1Y,1f[U]]}}1q(Q=1f[++U])}J.q(3C);U=0;6 20=0;6 f=[1k,1l];5(Q=J[U]){1y{18.4o(Q[1])}1q(Q=J[++U])}}}5(8.P){n.B(t)}M{5(8.12){n.2a(t,8.12,!!8.3I)}}};7.u=9(o,z,p){6 N;p=p||{};6 t=n.T(o,p);6 8=n.S[t.I];5(!z){8.z=C}M{5(z.W==\"3O\"&&z.E==\"2h-4p\"&&z.22>-1){z={\\'u\\':z.2t[z.22].1w}}5(z.W==\"3K\"&&z.E==\"3L\"){z={\\'u\\':\"/^\"+z.1w+\"/\"}}5(X(z)==\"4l\"&&!z.y){z=[z]}F(6 i=0,L=z.y;i<L;i++){6 u=z[i];5(X(u.u)==\"3B\"){5(u.u.1Q(/^\\\\/(.*)\\\\/$/)){u.u=1e 17(17.$1);u.u.3s=1N}M 5(u.u.1Q(/^9\\\\s*\\\\(([^\\\\)]*)\\\\)\\\\s*\\\\{(.*)}\\\\s*$/)){u.u=2k(17.$1,17.$2)}}5(u&&!G(u.D)&&(N=1M(o,\"4g\",\"4s\"))){u.D=n.1h(N)}5((!u||!u.u)&&8.z){4t 8.z[u.D]}M{8.z=8.z||{};8.z[u.D]=u.u}}F(6 j 2T 8.z){6 3w=1N}5(!3w){8.z=C}}h 7.2q(o)};7.B=9(t,B,p){p=p||{};5(G(B)){p.B=B}h 7.2q(t,p)};7.2f=9(t,2i,p){t=n.T(t,p);h n.B(t,(7.S[t.I].B||0)+2i,p)};7.4C=9(t,p){h n.2f(t,1,p)};7.4E=9(t,p){h n.2f(t,-1,p)};7.2q=9(o,p){6 D,N,4F,4D=2O,u;6 B,P,1b,2m;6 1X=[],11=0,1P=0;6 t,8,V,13;p=p||{};t=n.T(o,p);8=n.S[t.I];6 B=8.B;5(G(B)){5(B<0){8.B=B=0}P=8.P||25;1b=B*P+1;2m=1b+P-1}6 K=t.1O;5(K==C||K.y==0){h}F(6 i=0,L=K.y;i<L;i++){6 18=K[i];F(6 j=0,1r=18.J.y;j<1r;j++){V=18.J[j];13=2O;5(8.z&&V.A){6 A=V.A;6 3r=A.y;F(D 2T 8.z){5(!13){u=8.z[D];5(u&&D<3r){6 w=n.1z(A[D]);5(u.3s&&w.3M){13=(w.3M(u)<0)}M 5(X(u)==\"9\"){13=!u(w,A[D])}M{13=(w!=u)}}}}}1P++;5(!13){11++;5(G(B)){1X.3F(V);5(11<1b||11>2m){13=1N}}}V.2N.1B=13?\"24\":\"\"}}5(G(B)){5(1b>=11){1b=11-(11%P);8.B=B=1b/P;F(6 i=1b,L=1X.y;i<L;i++){1X[i].2N.1B=\"\"}}}n.1s(t,\"1g\",9(c){((8.z&&G(8.z[7.1h(c)])&&14(c,7.2w))?1l:1k)(c,7.3q)});5(8.12){n.2a(t)}6 1V=4c.48(11/P)+1;5(G(B)){5(8.2B){8.2B.1F=B+1}5(8.2r){8.2r.1F=1V}}5(8.2p){8.2p.1F=11}5(8.2n){8.2n.1F=1P}h{\\'49\\':8,\\'44\\':11,\\'3Z\\':1P,\\'1V\\':1V,\\'B\\':B,\\'P\\':P}};7.2a=9(t,10,p){p=p||{};p.12=10;t=n.T(t,p);6 8=n.S[t.I];6 K=t.1O;5(K==C||K.y==0){h}10=8.12;6 f=[1k,1l];F(6 i=0,L=K.y;i<L;i++){6 18=K[i],1f=18.J,U=0,Q,20=0;5(Q=1f[U]){5(8.40){1y{f[20++%2](Q,10)}1q(Q=1f[++U])}M{1y{5(!1A(Q)){f[20++%2](Q,10)}}1q(Q=1f[++U])}}}};7.3G=9(t,D){6 2R={},K=n.T(t).1O;F(6 i=0,L=K.y;i<L;i++){6 2U=K[i];F(6 r=0,1r=2U.J.y;r<1r;r++){2R[n.1z(2U.J[r].A[D])]=1N}}6 2Q=[];F(6 w 2T 2R){2Q.3F(w)}h 2Q.q()};7.27=9(p){6 A=[],26=1j.31(\"2d\");6 w,8;5(26!=C){F(6 i=0,L=26.y;i<L;i++){6 t=7.T(26[i]);8=7.S[t.I];5(w=Y(t,7.3y)){8.12=w}5(14(t,7.2D)){7.2y(t)}5(w=Y(t,7.3v)){7.2L(t,{\\'P\\':+w})}5((w=Y(t,7.3z))||(14(t,7.3J))){7.29(t,{\\'D\\':(w==C)?C:+w})}5(8.12&&14(t,7.3x)){7.2a(t)}}}};7.29=9(t,p){t=n.T(t,p);6 8=n.S[t.I];n.1s(t,\"1g\",9(c){6 E=Y(c,7.3E);5(E!=C){E=E||\"2M\";c.3D=c.3D||7.3A;1l(c,7.2e);c.2u=2k(\"\",\"1K.q(n,{\\'R\\':1C[\\'\"+E+\"\\']})\");5(p.D!=C){5(p.D==7.2g(c)){8.R=1C[\\'\"+E+\"\\']}}}});5(p.D!=C){7.q(t,p)}};7.2L=9(t,p){t=n.T(t,p);6 8=n.S[t.I];5(8.P){n.1s(t,\"1g,3p\",9(c){6 E=Y(c,7.3u);5(E==\"3W\"){E=1}M 5(E==\"3T\"){E=-1}5(E!=C){c.2u=2k(\"\",\"1K.2f(n,\"+E+\")\")}});5(w=Y(t,7.3H)){8.2B=1j.1J(w)}5(w=Y(t,7.3t)){8.2r=1j.1J(w)}h 7.B(t,0,p)}};7.2c=9(e){e=e||1c.3n;5(X(e.3o)==\"9\"){e.3o()}5(G(e.2c)){e.2c=1N}};7.2y=9(t,p){p=p||{};t=n.T(t,p);6 8=n.S[t.I],w;7.1s(t,\"1g\",9(N){5(14(N,7.2w)){6 1D=7.1h(N);6 1x=7.3G(t,1D);5(1x.y>0){5(X(p.3N)==\"9\"){1o.3N(N,1x)}M{6 2b=\\'<2h 4m=\"1K.u(n,n)\" 2u=\"1K.2c(3n)\" 4q=\"\\'+7.2D+\\'\"><28 1w=\"\">\\'+7.2W+\\'</28>\\';F(6 i=0;i<1x.y;i++){2b+=\\'<28 1w=\"\\'+1x[i]+\\'\">\\'+1x[i]+\\'</28>\\'}2b+=\\'</2h>\\';N.1F+=\"<4B>\"+2b}}}});5(w=Y(t,7.3l)){8.2p=1j.1J(w)}5(w=Y(t,7.3d)){8.2n=1j.1J(w)}};5(X(30)!=\"2H\"){30(7.27)}M 5(1c.2V){1c.2V(\"42\",7.27,2O)}M 5(1c.32){1c.32(\"4j\",7.27)}h 7})();',62,291,'|||||if|var|table|tdata|function||||||||return||||||this||args|sort||||filter||val||length|filters|cells|page|null|col|type|for|def|node|id|rows|bodies||else|cell|convert|pagesize|cRow|sorttype|tabledata|resolve|cRowIndex|row|nodeName|typeof|classValue|td|className|unfilteredrowcount|stripeclass|hideRow|hasClass|rowIndex|date|RegExp|tb|yr|name|pagestart|window|desc|new|tbrows|THEAD|getCellIndex|matrix|document|removeClass|addClass|tableHeaderIndexes|numeric|func|tableObj|while|L2|processTableCells|tableCellObj|ret|section|value|colValues|do|getCellValue|isHidden|display|Sort|cellIndex|nodeValue|innerHTML|childNodes|arg|numeric_comma|getElementById|Table|firstAvailCol|getParent|true|tBodies|totalrows|match|replace|re|ignorecase|useinnertext|pagecount|innerText|unfilteredrows|cellValue|parentNode|displayedCount|sortconvert|selectedIndex|alphanumeric|none||tables|auto|option|autosort|stripe|sel|cancelBubble|TABLE|SortableClassName|pageJump|getActualCellIndex|select|count|tr|Function|rowCells|pageend|container_all_count|nname|container_filtered_count|scrape|container_count|lastdesc|options|onclick|SortedAscendingClassName|FilterableClassName|SortedDescendingClassName|autofilter|lastcol|rowSpan|container_number|processCells|AutoFilterClassName|cellCoordinates|colSpan|trs|undefined|Date|o1|getTime|autopage|default|style|false|fixYear|valArray|values|numeric_converter|in|tbody|addEventListener|FilterAllLabel|NoSortClassName|formats|matrixrow|jQuery|getElementsByTagName|attachEvent|separator|prefix|cellsK|sortable|sorted|filtered|L3|copy|uniqueId|rowcount|RowcountPrefix|useInnerText|cs|getComputedStyle|thead|currentStyle|arguments|o2|FilteredRowcountPrefix|cellId|event|stopPropagation|TFOOT|FilteredClassName|cellsLength|regex|PageCountPrefix|AutoPageJumpPrefix|AutoPageSizePrefix|keep|AutoStripeClassName|StripeClassNamePrefix|AutoSortColumnPrefix|AutoSortTitle|string|newSortFunc|title|SortableColumnPrefix|push|getUniqueColValues|PageNumberPrefix|ignorehiddenrows|AutoSortClassName|INPUT|text|search|insert|SELECT|All|number|Filter|TABLE_|previous|tFoot|tHead|next|Click|getPropertyValue|total|ignoreHiddenRows|test|load|9999999999999|unfilteredcount|autostripe|nosort|filterable|floor|data|to|asc|Math|IMG|currency_comma|currency|TD|break|call|onload|toLowerCase|object|onchange|parseFloat|appendChild|one|class|TR|TH|delete|2000|checkbox|100|1900|radio|checked|nodeType|br|pageNext|filterReset|pagePrevious|filterList|50'.split('|'),0,{}))
</script>
<style type=\"text/css\" media=\"screen\">
tr.noshow { display: none;}
body{

	font-family:Arial, Helvetica, sans-serif; font-size:88%;
}
table.example {
	border:1px solid black;
	border-collapse:collapse;
        font-size:11px;
}
table.example th, table.example td {
	border:1px solid #aaaaaa;
	padding: 2px 15px 2px 15px;
}
table.example thead th {
	background-color:#ccccff;
}
table.example tfoot td {
	background-color:#eeeeee;
}

table.example tr.tbody_header {
	font-weight:bold;
	text-align:center;
	background-color:#dddddd;
}

table.example a.pagelink {
	padding-left:5px;
	padding-right:5px;
	border:1px solid #666666;
	margin:0px 5px 0px 5px;
}
table.example a.currentpage {
	background-color:yellow;
}
/* Striping */
tr.alternate {
	background-color:#ffffcc;
}

/* Sorting */
th.table-sortable {
	cursor:pointer;
	background-image:url(\"sortable.gif\");
	background-position:center left;
	background-repeat:no-repeat;
	padding-left:12px;
}
th.table-sorted-asc {
	background-image:url(\"sorted_up.gif\");
	background-position:center left;
	background-repeat:no-repeat;
}
th.table-sorted-desc {
	background-image:url(\"sorted_down.gif\");
	background-position:center left;
	background-repeat:no-repeat;
}
th.table-filtered {
	background-image:url(\"filter.gif\");
	background-position:center left;
	background-repeat:no-repeat;
}
select.table-autofilter {
	font-size:smaller;
}

/* Examples which stray from the default */
table.altstripe tr.alternate2 {
	background-color:#ccffff;
}

/* Sort Icon Styles */
table.sort01 th.table-sortable { background-image:url(\"icons/01_unsorted.gif\"); }
table.sort01 th.table-sorted-asc { background-image:url(\"icons/01_ascending.gif\"); }
table.sort01 th.table-sorted-desc { background-image:url(\"icons/01_descending.gif\"); }

table.sort02 th.table-sortable { background-image:none; padding-left:16px; }
table.sort02 th.table-sorted-asc { background-image:url(\"icons/02_ascending.gif\"); }
table.sort02 th.table-sorted-desc { background-image:url(\"icons/02_descending.gif\"); }

table.sort03 th.table-sortable { background-image:none; }
table.sort03 th.table-sorted-asc { background-image:url(\"icons/03_ascending.gif\"); }
table.sort03 th.table-sorted-desc { background-image:url(\"icons/03_descending.gif\"); }

table.sort04 th.table-sortable { background-image:none; }
table.sort04 th.table-sorted-asc { background-image:url(\"icons/04_ascending.gif\"); }
table.sort04 th.table-sorted-desc { background-image:url(\"icons/04_descending.gif\"); }

table.sort05 th.table-sortable { background-image:url(\"icons/05_unsorted.gif\"); padding-left:16px;}
table.sort05 th.table-sorted-asc { background-image:url(\"icons/05_ascending.gif\"); }
table.sort05 th.table-sorted-desc { background-image:url(\"icons/05_descending.gif\"); }

table.sort06 th.table-sortable { background-image:none; padding-left:16px;}
table.sort06 th.table-sorted-asc { background-image:url(\"icons/06_ascending.gif\"); }
table.sort06 th.table-sorted-desc { background-image:url(\"icons/06_descending.gif\"); }

table.sort07 th.table-sortable { background-image:none; }
table.sort07 th.table-sorted-asc { background-image:url(\"icons/07_ascending.gif\"); }
table.sort07 th.table-sorted-desc { background-image:url(\"icons/07_descending.gif\"); }

table.sort08 th.table-sortable { background-image:none; }
table.sort08 th.table-sorted-asc { background-image:url(\"icons/08_ascending.gif\"); }
table.sort08 th.table-sorted-desc { background-image:url(\"icons/08_descending.gif\"); }

table.sort09 th.table-sortable { background-image:none; padding-left:30px;}
table.sort09 th.table-sorted-asc { background-image:url(\"icons/09_ascending.gif\"); }
table.sort09 th.table-sorted-desc { background-image:url(\"icons/09_descending.gif\"); }

table.sort10 th.table-sortable { background-image:url(\"icons/10_unsorted.gif\"); }
table.sort10 th.table-sorted-asc { background-image:url(\"icons/10_ascending.gif\"); }
table.sort10 th.table-sorted-desc { background-image:url(\"icons/10_descending.gif\"); }

table.sort11 th.table-sortable { background-image:url(\"icons/11_unsorted.gif\");padding-left:24px; }
table.sort11 th.table-sorted-asc { background-image:url(\"icons/11_ascending.gif\"); }
table.sort11 th.table-sorted-desc { background-image:url(\"icons/11_descending.gif\"); }

table.sort12 th.table-sortable { background-image:none; }
table.sort12 th.table-sorted-asc { background-image:url(\"icons/12_ascending.gif\"); }
table.sort12 th.table-sorted-desc { background-image:url(\"icons/12_descending.gif\"); }

table.sort13 th.table-sortable { background-image:none; }
table.sort13 th.table-sorted-asc { background-image:url(\"icons/13_ascending.gif\"); }
table.sort13 th.table-sorted-desc { background-image:url(\"icons/13_descending.gif\"); }

table.sort14 th.table-sortable { background-image:none; }
table.sort14 th.table-sorted-asc { background-image:url(\"icons/14_ascending.gif\"); }
table.sort14 th.table-sorted-desc { background-image:url(\"icons/14_descending.gif\"); }

table.sort15 th.table-sortable { background-image:none; }
table.sort15 th.table-sorted-asc { background-image:url(\"icons/15_ascending.gif\"); }
table.sort15 th.table-sorted-desc { background-image:url(\"icons/15_descending.gif\"); }

table.sort16 th.table-sortable { background-image:none; }
table.sort16 th.table-sorted-asc { background-image:url(\"icons/16_ascending.gif\"); }
table.sort16 th.table-sorted-desc { background-image:url(\"icons/16_descending.gif\"); }

table.sort17 th.table-sortable { background-image:none; }
table.sort17 th.table-sorted-asc { background-image:url(\"icons/17_ascending.gif\"); }
table.sort17 th.table-sorted-desc { background-image:url(\"icons/17_descending.gif\"); }

table.sort18 th.table-sortable { background-image:url(\"icons/18_unsorted.gif\"); }
table.sort18 th.table-sorted-asc { background-image:url(\"icons/18_ascending.gif\"); }
table.sort18 th.table-sorted-desc { background-image:url(\"icons/18_descending.gif\"); }

table.sort19 th.table-sortable { background-image:url(\"icons/19_unsorted.gif\");padding-left:24px; }
table.sort19 th.table-sorted-asc { background-image:url(\"icons/19_ascending.gif\"); }
table.sort19 th.table-sorted-desc { background-image:url(\"icons/19_descending.gif\"); }

/* Icons box */
.iconset {
	margin:5px;
	border:1px solid #cccccc;
	border-color:#cccccc #666666 #666666 #cccccc;
	text-align:center;
	cursor:pointer;
	width:100px;
}
.iconset img {
	margin:3px;
}
/* Documentation */
tr.doc_section {
	font-weight:bold;
	text-align:center;
	background-color:#dddddd;
}
</style>
</head>
<body>";
get.links <- function(){
    # Data Link
    # Graphics
    ## Look through repository?
    tmp.f <- function(x) {
        ret <- paste("<strong>",x,"</strong>",sep="");
        lst <- par.links[[x]];
        tmp.g <- function(y,x){
            if(file.exists(par.links[[x]][[y]])){
                return(paste("<li><a href=\"",par.links[[x]][[y]],"\" target=\"_blank\">",
                             y,"</a></li>",sep=""));
            } else {
                return("");
            }
        }
        tmp <- paste(unlist(lapply(names(lst),tmp.g,x=x)),collapse="");
        if (tmp == ""){
            return("");
        } else {
            return(paste(ret,"<ul>",tmp,"</ul>",sep=""));
        }
    }
    return(paste(unlist(lapply(names(par.links),tmp.f)),collapse="\n"));
}

get.html.head <- function(id="table1",class="example table-autosort table-autofilter table-autopage:5 table-stripeclass:alternate table-page-number:t1page table-page-count:t1pages table-filtered-rowcount:t1filtercount table-rowcount:t1allcount"){
    return(paste("<table id=\"",id,"\" cellspacing=\"0\" class=\"",class,"\">\n",
                 "\t<thead>\n",
                 "\t\t<tr>\n",
                 "\t\t\t<th class=\"table-sortable:default\"><strong>Time Stamp</strong></th>\n", #1
                 "\t\t\t<th class=\"table-sortable:default\"><strong>Last</strong><select onchange=\"Table.filter(this,this)\" id=\"lasthead\"> <option value=\"\"> </option> <option value=\"F\">F</option> <option value=\"T\" selected>T</option> </select></th>\n", #2
                 "\t\t\t<th class=\"table-sortable:default table-filterable\"><strong>CTL</strong></th>\n", #4
                 "\t\t\t<th class=\"table-sortable\"><strong>$PROBLEM</strong></th>\n", #5
                 "\t\t\t<th class=\"table-sortable:numeric\"><strong>OBJ</strong></th>\n", #6
                 "\t\t\t<th class=\"table-sortable:numeric\"><strong>AIC</strong></th>\n", #7
                 "\t\t\t<th class=\"table-sortable:numeric\"><strong>CN</strong></th>\n", #8
                 "\t\t\t<th class=\"table-sortable:numeric\"><strong>MS</strong></th>\n", #9
                 "\t\t\t<th class=\"table-sortable:default table-filterable\"><strong>SIM/EST</strong></th>\n", #10
                 "\t\t\t<th class=\"table-sortable:default\"><strong>COVARIANCE</strong></th>\n", #11
                 "\t\t\t<th class=\"table-sortable:numeric table-filterable\"><strong>&theta;</strong></th>\n", #12
                 "\t\t\t<th class=\"table-sortable:numeric table-filterable\"><strong>&omega;</strong></th>\n", #13
                 "\t\t\t<th class=\"table-sortable:numeric table-filterable\"><strong>&sigma;</strong></th>\n", #14
                 "\t\t</tr>\n",
                 "\t</thead>\n",
                 "\t<tbody>\n",
                 sep=""));
}

get.html.mid <- function(){
    return(paste("\t</tbody>\n",
                 "\t<tfoot>\n",
                 sep=""));
}
get.html.tab <- function(t) {
    tab <- t;
    ## Convert All to character representations.
    tmp.g <- function(x){
        return(paste("if(all(sapply(tab[,",x,"],is.numeric))){tab[,",x,"] <- signif(tab[,",x,"],3);} else { tab[,",x,"] <- paste(tab[,",x,"]); }",sep=""))
    }
    eval(parse(text=paste(sapply(seq(1,length(tab[1,])),tmp.g),collapse="\n")))
    ## Header.
    ret <-  paste("<table cellspacing=\"0\" border=\"1\"><thead><tr>\n\t<th><strong>",paste(names(tab),collapse="</strong></th>\n\t<th><strong>"),"</strong></th>\n</tr></thead>\n",sep="")

    tmp.f <- function(x,tab){
        return(
               paste("<tr>\n\t<td>",paste(paste(tab[x,]),collapse="</td>\n\t<td>"),"</td>\n</tr>\n",sep="")
               );
    }
    ret <- paste(ret,"<tbody>",
                 paste(sapply(seq(1,length(tab[,1])),tmp.f,tab=tab),collapse=""),
                 "</tbody></table>",sep="\n");
    return(ret);
}

get.html.end <- function(){
    return(
           paste("\t\t<tr id=\"t1foot\">\n",
                 "\t\t\t<td colspan=\"2\" class=\"table-page:previous\" style=\"cursor:pointer;\">&lt; &lt; Previous</td>\n",
                 "\t\t\t<td colspan=\"9\" style=\"text-align:center;\">Page <span id=\"t1page\"></span>&nbsp;of <span id=\"t1pages\"></span></td>\n",
                 "\t\t\t<td colspan=\"2\" class=\"table-page:next\" style=\"cursor:pointer;\">Next &gt; &gt;</td>\n",
                 "\t\t</tr>\n",
                 "\t\t<tr id=\"t2foot\">\n",
                 "\t\t\t<td colspan=\"14\"><span id=\"t1filtercount\"></span>&nbsp;of <span id=\"t1allcount\"></span>&nbsp;rows match filter(s)</td>\n",
                 "\t\t</tr>\n",
                 "\t\t<tr id=\"t3foot\"><td colspan=\"14\">\n",
                 "<strong>CTL</strong> Control Stream<br>",
                 "<strong>OBJ</strong> Minimum Objective Function<br>",
                 "<strong>AIC</strong> Akaike Information Criterion<br>",
                 "<strong>CN</strong> Condition Number<br>",
                 "<strong>MS</strong> Maximum Shrinkage",
                 "\t\t</td></tr>\n",
                 "\t</tfoot>\n",
                 "</table>\n",
                 sep="")
           );
}
get.html.sum <- function(s,id=FILEID) {
    id2 <- gsub("-","_",id);
    return(
           paste("<tr class=\"noshow\" id=\"id_",id2,"\">",
                 "<td colspan=\"13\">",
                 "<strong>Notes/Errors</strong>",
                 paste("<ul>\n",paste(paste("<li>",gsub("\\\\+%","%",s$errors),"</li>",sep=""),collapse="\n"),"\n</ul>\n",sep=""),
                 get.links(),
                 get.html.tab(s$theta.tab),
                 get.html.tab(s$eta.tab),
                 ifelse(is.null(s$eps.tab),"No EPS table, probably single subject data.",get.html.tab(s$eps.tab)),
                 "</td>",
                 "</tr>\n",
                 sep="")
           );
}

get.html.row <- function(s,id = FILEID){
    control.stream.name <- gsub(";+ [Cc]ontrol *[Ss]tream *[nN]ame:? *","",gsub(".*[\\\\/]([^\\\\/]*)$","\\1",s$mod[which(regexpr("; Control stream name: .*",s$mod)>=0)[1]]));
    if (is.na(control.stream.name)){
        ## Look for it in a file context
        start.file <- which(regexpr(";[Cc]?[ \t]*[Ff]ile:? *",s$mod)>=0)[1];
        stop.file <- start.file;
        while (regexpr(";[Cc]?[ \t]*.*?[.][^.]*[ \t]*$",s$mod[stop.file]) == -1){
            stop.file <- stop.file + 1;
        }
        fn <- s$mod[start.file:stop.file];
        fn <- paste(gsub("^[ \t]*;+[Cc]?[ \t]*([Ff]ile:?)?[ \t]*","",fn),collapse="");
        fn <- gsub(".*[\\\\/]([^\\\\/]*)$","\\1",fn)
        control.stream.name <- fn;
    }
    working.directory <-  gsub(".*[\\\\/]([^\\\\/]+)[\\\\/]+[^\\\\/]*$","\\1",s$mod[which(regexpr("; Control stream name: .*",s$mod)>=0)[1]]);
    id2 <- gsub("-","_",id);
    return(paste("\t\t<tr onClick=\"toggleInfo('",
                 id2,
                 "');\" style=\"cursor:pointer;\" id=\"tr_",
                 id2
                 ,"\">\n\t\t\t<td>",
                 id,
                 "</td>\n\t\t\t<td>L",
                 id,
#                 "</td>\n\t\t\t<td>",
#                 working.directory,
                 "</td>\n\t\t\t<td>",
                 control.stream.name,
                 "</td>\n\t\t\t<td>",
                 s$problem,
                 "</td>\n\t\t\t<td>",
                 s$mvof,
                 "</td>\n\t\t\t<td>",
                 s$aic,
                 "</td>\n\t\t\t<td>",
                 s$eigen.cn,
                 "</td>\n\t\t\t<td>",
                 round(max(s$shrinkage,na.rm=TRUE),2),
                 "</td>\n\t\t\t<td>",
                 s$sim.or.est,
                 "</td>\n\t\t\t<td>",
                 ifelse(s$cov,"Covariance Step Completed",
                    ifelse(any(regexpr("^\\$COV",s$mod)>=0),
                           "Covariance Step Aborted",
                           "No covariance requested")),
                 "</td>\n\t\t\t<td>",
                 length(s$f.t),
                 "</td>\n\t\t\t<td>",
                 length(diag(s$f.o)),
                 "</td>\n\t\t\t<td>",
                 length(diag(s$f.s)),
                 "</td>\n\t\t</tr>\n",sep="")
           );
}

get.r.save <- function(s,id=FILEID){
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
    nme <- paste(work.dir,ctl.name,sep="/");
    return(
           paste("nme <- \"",nme,"\";\n",
                 "c.id <- \"",id,"\";\n",
                 "if (any(names(r.saves) == nme)){\n",
                 "    r.saves[[nme]] <- c(r.saves[[nme]],c.id);\n",
                 "} else {\n",
                 "    r.saves[[nme]] <- c(c.id);\n",
                 "}\n",sep=""
                 )
           );
}

## Now get all runs.
get.rows.sums <- function(name,sum=FALSE){
    vals <- sort(r.saves[[name]]);
    cur.run <- rep("F",length=length(vals));
    cur.run[length(cur.run)] <- "T";
    ret.rows <- "";
    ret.sums <- "";
    for (i in 1:length(vals)){
        cur.id <- vals[i];
        if (!sum){
            cat(paste("Read File: ../USERSCRIPTS/html-rows/html-rows-",cur.id,".html\n",sep=""),file=stderr());
            con <- file(paste("../USERSCRIPTS/html-rows/html-rows-",cur.id,".html",sep=""), "r");
            ret <- sub(paste("L",cur.id,sep=""),cur.run[i],paste(readLines(con),collapse="\n"));
            close(con);
            ret.rows <- paste(ret.rows,ret,sep="\n");
        } else {
            cat(paste("Read File: ../USERSCRIPTS/html-sums/html-sums-",cur.id,".html\n",sep=""),file=stderr());
            con <- file(paste("../USERSCRIPTS/html-sums/html-sums-",cur.id,".html",sep=""),"r");
            ret <- paste(readLines(con),collapse="\n");
            close(con);
            ret.sums <- paste(ret.sums,ret,sep="\n");
        }
    }
    if (sum){
        return(ret.sums);
    } else {
        return(ret.rows);
    }
}

if (!file.exists("../USERSCRIPTS/html-rows/")){
    dir.create("../USERSCRIPTS/html-rows/");
}
write(get.html.row(s=s),paste("../USERSCRIPTS/html-rows/html-rows-",FILEID,".html",sep=""));

if (!file.exists("../USERSCRIPTS/html-sums/")){
    dir.create("../USERSCRIPTS/html-sums/");
}
write(get.html.sum(s=s),paste("../USERSCRIPTS/html-sums/html-sums-",FILEID,".html",sep=""));

if (!file.exists("../USERSCRIPTS/r-last-run/")){
    dir.create("../USERSCRIPTS/r-last-run/");
}
write(get.r.save(s=s),paste("../USERSCRIPTS/r-last-run/r-last-run-",FILEID,".R",sep=""));

r.saves <- list();

## Build list of runs and last runs to (eventually) build the HTML file.
tmp.f <- function(x){
    con <- file(x,"r");
    ret <- paste(readLines(con),collapse="\n");
    close(con);
    return(ret);
}
eval(parse(text=paste(sapply(paste("../USERSCRIPTS/r-last-run/",list.files("../USERSCRIPTS/r-last-run/",".*\\.R$"),sep=""),tmp.f),collapse="\n")));

ret <- get.html.head();

ret <- paste(ret,
             paste(sapply(names(r.saves),get.rows.sums),collapse="\n"));

ret <- paste(ret,get.html.mid(),sep="\n");

ret <- paste(ret,paste(sapply(names(r.saves),get.rows.sums,sum=TRUE),
                       collapse="\n"));

ret <- paste(ret,get.html.end(),sep="\n");


ret <- paste(html.template,ret,
             "<script language=\"Javascript\" type=\"text/javascript\">\nTable.filter(document.getElementById(\"lasthead\"),document.getElementById(\"lasthead\"))\n</script>\n</body>",sep="\n");
ret <- gsub("/*cls*/",paste("\n",paste(paste("if (timestamp != '",paste(gsub("-","_",unlist(r.saves)),sep=""),"'){document.getElementById('",paste("id_",gsub("-","_",unlist(r.saves)),sep=""),"').className = 'noshow';}",sep=""),collapse="\n"),"\n",sep=""),ret,fixed=TRUE);

ret <- gsub("/*unfilter*/",paste("\n",paste(paste("if (timestamp != '",paste(gsub("-","_",unlist(r.saves)),sep=""),"'){document.getElementById('",paste("tr_",gsub("-","_",unlist(r.saves)),sep=""),"').className = '';}",sep=""),collapse="\n"),"\n",sep=""),ret,fixed=TRUE);

ret <- gsub("/*filter*/",paste("\n",paste(paste("if (timestamp != '",paste(gsub("-","_",unlist(r.saves)),sep=""),"'){document.getElementById('",paste("tr_",gsub("-","_",unlist(r.saves)),sep=""),"').className = 'noshow';}",sep=""),collapse="\n"),"\n",sep=""),ret,fixed=TRUE);
write(ret,file="../USERSCRIPTS/summary.html");


######################################################################
### html.R ends here
