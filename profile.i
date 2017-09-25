/*==================================================================================================*/

func profile(c,winpic,widthavg=){
/* DOCUMENT profile(c,winpic,widthavg=)
   SEE ALSO: zprofile, lnprofile, zmprofile
 */

if(is_void(winpic))winpic=window();

winnow=window();

keybd_focus,1;

write,"Left: X profile;  Right Y: profile; Middle; try again";

window,winpic;
do{
  do{q = mouse(1,0,"");}while(is_void(q));
  if(q(10)==1){
    if(is_void(widthavg)){
      cp = c(,nint(q(2)));
    }else{
      cp = c(,nint(q(2))-widthavg/2:nint(q(2))-widthavg/2+widthavg-1)(,avg);
    }
  }else if(q(10)==3){
    if(is_void(widthavg)){
      cp = c(nint(q(1)),);
    }else{
      cp = c(nint(q(1))-widthavg/2:nint(q(1))-widthavg/2+widthavg-1,)(avg,);
    }

  }
  if(q(10)<2)break;
}while(q(10)==2)

window,winnow;

return cp;
}

/*==================================================================================================*/

func lnprofile(c,winpic,widthavg=,bad=){
/* DOCUMENT lnprofile(c,winpic,widthavg=)
   SEE ALSO: profile, zprofile, zmprofile
 */

dc = dimsof(c);

if(is_void(winpic))winpic=window();

if(is_void(widthavg))widthavg = 1;

winnow=window();

keybd_focus,1;

write,"Left draws profile (Hold down and drag);  Right or Middle quit";

window,winpic;
do{q = mouse(1,2,"");}while(is_void(q)||q(1)==q(3));
pldj,q(1),q(2),q(3),q(4),color="red";
nrm = sqrt((q(4)-q(2))^2 + (q(3)-q(1))^2)
if(q(3)!=q(1)){
  slp = (q(4)-q(2))/(q(3)-q(1));
  cs = sign(q(3)-q(1))*sqrt(1/(1+slp^2));
  sn = slp * cs;
}

cp = 0.0
for(i=1;i<=widthavg;i++){
  sp = span(0.,nrm,nint(nrm+1));
  y =  i - widthavg/2.0 - 0.5;
  xp = q(1) - y*sn + sp*cs;
  yp = q(2) + y*cs + sp*sn;

  cp += interp2reg(c,1.0,dc(2),1.0,dc(3),xp,yp,bad=bad)/widthavg
}

window,winnow;

return cp;
}

/*==================================================================================================*/

func zprofile(c,winpic,widthavg=,bad=){
/* DOCUMENT zprofile(c,winpic,widthavg=)
   works with pli
   SEE ALSO: profile, lnprofile, zmprofile
 */

dc = dimsof(c);

if(is_void(winpic))winpic=window();

if(is_void(widthavg))widthavg = 1;

winnow=window();

keybd_focus,1;

write,"Left draws profile (N X click);  Right or Middle quit";

window,winpic;

cp = 0.0;
do{qo = mouse(1,0,"");}while(is_void(qo));
do{
  do{qn = mouse(1,0,"");}while(is_void(qn));
  pldj,qo(1),qo(2),qn(1),qn(2),color="red";
  nrm = sqrt((qn(2)-qo(2))^2 + (qn(1)-qo(1))^2);
  if(qn(1)!=qo(1)){
    slp = (qn(2)-qo(2))/(qn(1)-qo(1));
    cs = sign(qn(1)-qo(1))*sqrt(1/(1+slp^2));
    sn = slp * cs;
  }

  ccp = 0.0
  for(i=1;i<=widthavg;i++){
    sp = span(0.,nrm,nint(nrm+1));
    y =  i - widthavg/2.0 - 0.5;
    xp = qo(1) - y*sn + sp*cs;
    yp = qo(2) + y*cs + sp*sn;

    xp(::5);yp(::5);
    ccp += interp2reg(c,1.0,dc(2),1.0,dc(3),xp,yp,bad=bad)/widthavg;
  }
  qo = qn;
  cp = _(cp,ccp);

}while(qn(10)<2)

window,winnow;

return cp(2:);
}

/*==================================================================================================*/

func zmprofile(z,y,x,dnds,widthavg=,outside=){
/* DOCUMENT zmprofile(z,y,x,widthavg=,outside=)
   With meshes! plf,z,y.x
   SEE ALSO: zprofile, lnprofile, profile
 */

require, "digit2.i"

if(is_void(widthavg))widthavg = 0;

nw = long(widthavg*dnds);

write,"Left draws profile (N X click);  Right or Middle quit";

cp = 0.0;
do{qo = mouse(1,0,"");}while(is_void(qo));
do{
  do{qn = mouse(1,0,"");}while(is_void(qn));
  pldj,qo(1),qo(2),qn(1),qn(2),color="red";          // pldj, x0, y0, x1, y1
  nrm = sqrt((qn(2)-qo(2))^2 + (qn(1)-qo(1))^2);
  n = long(nrm*dnds);

  if(qn(1)!=qo(1)){
    slp = (qn(2)-qo(2))/(qn(1)-qo(1));
    cs = sign(qn(1)-qo(1))*sqrt(1/(1+slp^2));
    sn = slp * cs;
  }

  ccp = 0.0
  for(i=0;i<=nw;i++){
    yy =  i/dnds - widthavg/2.0;
    sp = span(0,nrm,n);
    xp = span(qo(1),qn(1),n) - yy*sn;
    yp = span(qo(2),qn(2),n) + yy*cs;

    ccp += interp2(yp,xp,z,y,x,outside=)/max([nw,1]);
  }
  qo = qn;
  cp = _(cp,ccp);

}while(qn(10)<2)

return cp(2:);
}

/*==================================================================================================*/

// func mousepic(winpic,close=,wait=){
// /* DOCUMENT mousepic(winpic,close=)
//    works with pli,pliwin....
//    SEE ALSO: profile, lnprofile, zmprofile, mousetrack, mousepoly, grabtrack, grabpoly
//  */
//
// if(is_void(winpic))winpic=window();
//
// winnow=window();
//
// keybd_focus,1;
//
// window,winpic;
//
// if(wait){write,"pan & zoom_in | q(uit) | <>:move on";if (strtok(rdline(prompt=""))(1)=="q") return;}
//
// write,"Left draws profile (N X click);  Right or Middle quit";
//
// q = array(double,[2,2,10000]);
//
// n  = 1;
// do{qo = mouse(1,0,"");}while(is_void(qo));
// q(..,n) = qo(1:2);
// do{
//   do{qn = mouse(1,0,"");}while(is_void(qn));
//   pldj,qo(1),qo(2),qn(1),qn(2),color="red";
//   qo = qn;
//   n += 1;
//   q(..,n)= qo(1:2);
//   cp = _(cp,ccp);
//
// }while(qn(10)<2)
//
// window,winnow;
//
// if(close==1){
//   pldj,q(1,1),q(2,1),qn(1),qn(2),color="red";
// }
// return q(..,1:n);
//
// }
/*==================================================================================================*/

func mousepic(winpic, close=, wait=){
    /* DOCUMENT mousepic (winpic, close=)
       works with pli,pliwin....
       SEE ALSO: profile, lnprofile, zmprofile, mousetrack, mousepoly, grabtrack, grabpoly
    */

    if(is_void(winpic))
        winpic= window();

    winnow= window();

    keybd_focus,1;

    window, winpic;

    write,"Modified leftORmiddle to draw profile (vertices);  Modified Right to quit";

    q= array(double,[2,2,10000]);

    n= 1;
    qo= mousez(1,0,"");
    q(..,n)= qo(1:2);
    do {
        qn= mousez(1,0,"");
        pldj,qo(1),qo(2),qn(1),qn(2),color="red";
        qo= qn;
        n+= 1;
        q(..,n)= qo(1:2);
    } while(qn(10)<3)

    window,winnow;

    if (close==1) {
        pldj,q(1,1),q(2,1),qn(1),qn(2),color="red";
    }
    return q(..,1:n);
}

/*==================================================================================================*/

func grabpoly(c,q,bad=){
/* DOCUMENT grabpoly(c,q)
   SEE ALSO: mousepic, mousetrack, mousepoly, grabtrack
 */

dq = dimsof(q);
dc = dimsof(c);
n = dq(3);
qb = array(double,[2,2,n+1]);

i1min = min([max([long(floor(q(1,min))+1),1]),dc(2)]);          // prepare to work with a SUBIMAGE (speed & MEM)
i1max = max([min([long(ceil(q(1,max))+1),dc(2)]),1]);
i2min = min([max([long(floor(q(2,min))+1),1]),dc(3)]);
i2max = max([min([long(ceil(q(2,max))+1),dc(3)]),1]);

if(i1min==i1max||i2min==i2max)return;

f1 = indgen(0:i1max-i1min);
f2 = indgen(0:i2max-i2min);

cb = c(i1min:i1max,i2min:i2max);
dc = dimsof(cb);
ff = array(double,[3,2,dc(2),dc(3)]);
ff(1,..) = f1(,-,-);
ff(2,..) = f2(-,,-);

qb(..,1:n) = q -[i1min-1,i2min-1](,-);
qb(..,n+1) = qb(..,1);   //window,4;fma;plf,cb,ff(2,..),ff(1,..);plfp,-5,qb(2,),qb(1,),n+1;

a = array(double,dimsof(cb));
for(i=1;i<=n;i++){
  a += angle2vec(qb(,i)(,-,-)-ff,qb(,i+1)(,-,-)-ff);
}                        //window,3;fma;pli,a;qb+=0.5;for(i=1;i<=n;i++){j=i%n+1;pldj,qb(1,i),qb(2,i),qb(1,j),qb(2,j),color="red";}

dat = cb(where(abs(a)>(2*pi-0.001)));
if(bad){w = dat!=bad;if(anyof(w)){dat = dat(where(w));}else{dat=[];}}

return dat;
}

func angle2vec(a,b)
{
  nrm = abs(a(1,..),a(2,..))*abs(b(1,..),b(2,..));
  sn = (a(1,..)*b(2,..)-a(2,..)*b(1,..))/nrm;
  cn = (a*b)(sum,..)/nrm;
  return atan(sn,cn);
}

/*==================================================================================================*/

func mousepoly(c,winpic,&pol){
/* DOCUMENT mousepoly(c,winpic,&pol)
   SEE ALSO: mousepic, mousetrack, grabpoly, grabtrack
 */
  pol = mousepic(winpic,close=1);
  return grabpoly(c,pol);
}

/*==================================================================================================*/

func grabtrack(c,q,metric,&c0,&s0,widthavg=,bad=){
/* DOCUMENT grabtrack(c,q,widthavg=)
   c is a 2-D real array (i,j) i=1:nc1 & j=1:nc2
   q is a track whose corner coordinates are x1=0:nc1-1 & x2=0:nc2-1
          the track values @ c are bi-linear-interpolates
   SEE ALSO: mousepic, mousetrack, grabpoly, mousepoly
 */

if(is_void(widthavg))widthavg = 1;

if(is_void(metric))metric=[1.,1.,1.];
if(is_void(s0))s0=0.0;
if(is_void(c0))c0=0.0;

d1 = metric(1);d12=d1^2;
d2 = metric(2);d22=d2^2;
ds = metric(3);

dq = dimsof(q);
n = dq(3);
qb = array(double,[2,2,n]);
dc = dimsof(c);

i1min = min([max([long(floor(q(1,min))+1),1]),dc(2)]);          // prepare to work with a SUBIMAGE (speed & MEM)
i1max = max([min([long(ceil(q(1,max))+1),dc(2)]),1]);
i2min = min([max([long(floor(q(2,min))+1),1]),dc(3)]);
i2max = max([min([long(ceil(q(2,max))+1),dc(3)]),1]);

qb = q -[i1min-1,i2min-1](,-);

c00 = c0 + (i1min-1)*d1;
s00 = c0 + (i2min-1)*d2;

m=0
for(i=1;i<n;i++){
  j=i%n+1;
  nrm = sqrt(d22*(qb(2,j)-qb(2,i))^2 + d12*(qb(1,j)-qb(1,i))^2); //unaffected by q->qb shift

  if(nint(nrm/ds)>=1){
    m += nint(nrm/ds);
  }else{
    m += 2;
  }
}
cp = s0 = c0 = array(double,[1,m]);

if(i1min==i1max||i2min==i2max)return cp;

cb = c(i1min:i1max,i2min:i2max);
dc = dimsof(cb);

m1=m2=1;
for(i=1;i<n;i++){
  j=i%n+1;
  nrm = sqrt(d22*(qb(2,j)-qb(2,i))^2 + d12*(qb(1,j)-qb(1,i))^2);
  if(qb(1,j)!=qb(1,i)){
    slp = (d2/d1)*(qb(2,j)-qb(2,i))/(qb(1,j)-qb(1,i));
    cs = sign(qb(1,j)-qb(1,i))*sqrt(1/(1+slp^2));
    sn = slp * cs;
  }

  if(nint(nrm/ds)>=1){
    sp = indgen(nint(nrm/ds))*ds;        //span(0.,nrm,nint(nrm+1));
  }else{
    sp = span(0.,nrm,2);
  }

  m2 = m1+numberof(sp)-1;

  gcnt = array(long,numberof(sp));
  cpp = array(double,numberof(sp));
  for(k=1;k<=widthavg;k++){
    y =  ds*(k - widthavg/2.0 - 0.5);
    xp = qb(1,i) - y*sn/d1 + sp*cs/d1;
    yp = qb(2,i) + y*cs/d2 + sp*sn/d2;

    yp = interp2reg(cb,1.0,dc(2),1.0,dc(3),xp,yp,bad=bad,outside=0.0);
    cpp += yp;
    gcnt += yp!=0.0;
  }

  w=gcnt==0;if(anyof(w))gcnt(where(w))=1;
  cp(m1:m2) = cpp/gcnt;

  s0(m1:m2) = s00 + qb(2,i)*d1 + sp*sn;
  c0(m1:m2) = c00 + qb(1,i)*d1 + sp*cs;

  m1 = m2+1;
}

return cp;
}

/*==================================================================================================*/

func mousetrack(c,winpic,&track,widthavg=,bad=){
/* DOCUMENT mousetrack(c,winpic,&track,widthavg=)
   SEE ALSO: mousepic, grabtrack, grabpoly, mousepoly
 */

  track = mousepic(winpic);
  return grabtrack(c,track,widthavg=widthavg,bad=bad);
}

/*==================================================================================================*/
func mousez(system, style, prompt, zoom_factor=, all=)
/* DOCUMENT func mousez(system, style, prompt, zoom_factor=, all=)
	Returns a mouse event as described in the "mouse" command while
	preserving the "normal" operation of the mouse (zoom in, pan, zoom out).
	i.e. an event is returned only when a "modifier key" (such as SHIFT, ALT,
	CTRL) are depressed when clicking a mouse button.
   SEE ALSO: mouse

     ARGUMENT:  system, style, prompt: As described in mouse help pages.

     KEYWORD:	zoom_factor: Zoom factor for mouse-click zoom in and zoom out.
		all: When set all mouse events are returned to the calling
		     routines.  The zoom or pan operation are carried out first
		     and then the event is returned.
*/
/*
    MODIFICATION HISTORY
	J.F. Pelletier, Jan. 99

	Fixed NUMLOCK bug.  When NUMLOCK is on, it changes the value returned by
	the mouse command for the modifier key from 0 to 16...
						JFP, Jul, 08 1999

 Thierry:  On SGI shift (modifier(11)==1) gives key(10)==1 for Left AND Middle keys....
*/
{
if (is_void(zoom_factor)) zf = 1.5			//Default zoom factor
else zf = zoom_factor

//Continuously monitor mouse events until a modifier key is use.
cont = 1
while (cont) {
   event = mouse(system, style, prompt)		//Get mouse event
   if (event == []) {				//Keyboard input
	button = -1
	mask = -1; }
   else {
	button = event(10)
	mask = event(11)
   }

/*Clicking a mouse button alone causes: mask=0 if NUM LOCK is off and
					mask=16 if NUM LOCK is on!!!*/
//If a modifier key was used, return the event to the caller
   if (mask != 0 && mask != 16 && is_void(all)) return(event)

// Process simple button click to reproduce "standard" behavior of the mouse.
   if (button == 1) {					//Left button   ZOOM IN
/*	Change the limits without moving the point where the mouse was clicked
	1) Reduce the distance between this point and the edges by a factor zf
	2) If nlim(1) = lim(1) --> mouse was clicked left of Y-axis.  In this case
		we only zoom in the Y direction.
*/
	mx = avg([event(1),event(3)])		//Mouse position in X
	my = avg([event(2),event(4)])		//Mouse position in Y
	lim = limits()				//Current limits
	nlim = lim				//New limits

	nlim(1) = mx-(mx-lim(1))/zf
	if (nlim(1) != lim(1)) nlim(2) = mx+(lim(2)-mx)/zf
	nlim(3) = my-(my-lim(3))/zf
	if (nlim(3) != lim(3)) nlim(4) = my+(lim(4)-my)/zf
	limits, nlim(1), nlim(2), nlim(3), nlim(4)
	}
   else if (button == 2) {				//Middle button PAN
	disp_x = event(1) - event(3)
	disp_y = event(2) - event(4)
	lim = limits()
	limits, lim(1)+disp_x, lim(2)+disp_x, lim(3)+disp_y, lim(4)+disp_y
	}
   else if (button == 3) {				//Right button  ZOOM OUT
	mx = avg([event(1),event(3)])		//Mouse position in X
	my = avg([event(2),event(4)])		//Mouse position in Y
	lim = limits()				//Current limits
	nlim = lim				//New limits

	nlim(1) = mx-(mx-lim(1))*zf
	if (nlim(1) != lim(1)) nlim(2) = mx+(lim(2)-mx)*zf
	nlim(3) = my-(my-lim(3))*zf
	if (nlim(3) != lim(3)) nlim(4) = my+(lim(4)-my)*zf
	limits, nlim(1), nlim(2), nlim(3), nlim(4)
   }
   if (!is_void(all)) return(event)			//Return every event.

}
}
