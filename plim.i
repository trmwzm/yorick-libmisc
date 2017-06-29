require, "plot.i";
require, "histop.i";
require, "rwflat.i";
require, "button.i";
require, "color.i";

func plwin_log(x) {
  return log(max(x,1e-30));
}
func plwin_abslog(x) {
  return log(max(abs(x),1e-30));
}

func flwin (args)
/* DOCUMENT flwin,file_arr1,file_arr1,win=,width=,mstyp=,\
      height=,dpi=,mnmx=,fdisp=,landscape=,wrat=,datfrac=
 
 *     MSTYP       RESULT
 *     ---------   ----------------
 *     0 "point"   [X,Y]
 *     1 "box"     [XMIN, XMAX, YMIN, YMAX] DEFAULT
 *     2 "line"    [X0, Y0, X1, Y1]
 *     3 "length"  LENGHT

   
 */
{
  s= ["win","width","height","dpi","mnmx","fdisp",\
      "landscape","wrat","datfrac=","mstyp"];
  win=width=height=dpi=mnmx=fdisp=landscape=wrat=datfrac= [];  // key init
  mstyp= 1;
  sk= args(-);        // key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,s(1)))) win=    args(s(1));
  if (nk && anyof(strmatch(sk,s(2)))) width=  args(s(2));
  if (nk && anyof(strmatch(sk,s(3)))) height= args(s(3));
  if (nk && anyof(strmatch(sk,s(4)))) dpi=    args(s(4));
  if (nk && anyof(strmatch(sk,s(5)))) mnmx=   args(s(5));
  if (nk && anyof(strmatch(sk,s(6)))) fdisp=  args(s(6));
  if (nk && anyof(strmatch(sk,s(7)))) landscape=args(s(7));
  if (nk && anyof(strmatch(sk,s(8)))) wrat=   args(s(8));
  if (nk && anyof(strmatch(sk,s(9)))) datfrac= args(s(9));
  if (nk && anyof(strmatch(sk,s(10)))) mstyp= args(s(10));
  i=0; while (i<nk&&strmatch(s,sk(i+1))(sum)) i++; 
  if (i<nk) error,"unkown keyword: "+sk(i+1);
  // positional arg processing
  if (args(0)!=1) error,\
     "flwin,flarr,fdisp=,win=,width=,height=,dpi=,mnmx=,"+\
     "lanscape=,wrat=,datfrac=";
  dx= dimsof(args(1,:));
  x1= args(1,:)(1);
  typ= structof(x1);
  if (is_void(fdisp)) fdisp= (typ==complex? plwin_abslog: plwin_log);
  if (is_void(win)) win= window();
  if (is_void(dpi)) dpi= 100;
  if (is_void(width)) width= (dpi==100? 800: 600);
  if (is_void(height)) height= (dpi==100? 600: 450);
  if (is_void(wrat)) wrat= 0.2;  // width ratio of preview
  if (is_void(datfrac)) datfrac= 0.97;  // width ratio of preview
  if (is_void(mnmx)) {
    mnmx= histchop(fdisp(args(1,:)(::5,::10)),datfrac);
    mnmx+= mnmx(dif)/5;
    write,format="Data [min, Max]: [%.3f, %.3f]\n",mnmx(1),mnmx(2);
  }
  colslp= 1.5;  // color slope bright/dim
  b= 5;
  c= 40;  // button height 
  w= width;
  h= height;
  wp= (w-2*b)*wrat;
  vp= [[b,w-b-wp,b,h-b],[w-b-wp+1,w-b,b+c,h-b]]; // xm,xM,ym,yM

  xw= xwindow(win,width=width,height=height,dpi=dpi,\
        landscape=landscape,viewport=vp,yopt=[0,0],\
        xopt=[0,0],units=2);
  gw= window_geometry();
  ndcb= gw(3:4)
  pxn= (dpi==100? 1064.38: 798.288);  //=1/xw(2) =1/gw(2)  
  // Pix [X,Y] to NDC [x,y]:  [x,y]= ndcb+[1,-1]*[X,Y]/pxn 
  // Pic origin 0 is upper-left, NDC 0 is lower-left
  
  wbp= [wp,c]/[6.0,4.0]; // px half [width,height] of four buttons in [wp,c] 
  wbn= wbp/pxn;
  bp= [w-b-wp/2,h-c/2-2]+[[-2,-1],[0,-1],[2,-1],\
                          [-2,1], [0,1], [2,1]]*wbp;
  bn= ndcb+[1,-1]*bp/pxn;
  bpan=  Button(text="pan", x=bn(1,1),y=bn(2,1),dx=wbn(1),dy=wbn(2));
  bgrab= Button(text="grab",x=bn(1,2),y=bn(2,2),dx=wbn(1),dy=wbn(2));
  bright= Button(text="brit",x=bn(1,3),y=bn(2,3),dx=wbn(1),dy=wbn(2));
  bredo= Button(text="redo",x=bn(1,4),y=bn(2,4),dx=wbn(1),dy=wbn(2));
  bdone= Button(text="done",x=bn(1,5),y=bn(2,5),dx=wbn(1),dy=wbn(2));
  dim= Button(text="dim",x=bn(1,6),y=bn(2,6),dx=wbn(1),dy=wbn(2));

  nf= long(dx(2:3)/[wp,h-2*b-c]+0.5);
  pim= fdisp(args(1,:)(::nf(1),::nf(2)));
  
  plsys,2;
  pli,pim,cmin=mnmx(1),cmax=mnmx(2);
  limits;
  
  bbs= [bpan,bgrab,bredo,bdone,dim,bright];
  button_plot,bbs;

  qout= [];
  cnt= 0;
  opan= save();
  iwn= 1;
  nx=ny= [];
  qpn= [];

  write,"PAN#1 button";
  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(bpan,x,y)) {
      qpn= xmouse(1,win=win,system=2,forever=1,\
          prompt="in preview WIN: clic-drag PAN box, then grab");
      write,"GRAB button";
      save,opan,string(0),qpn;
      nx= min(max(1,long(qpn(1:2)*nf(1)+.5)),dx(2));
      ny= min(max(1,long(qpn(3:4)*nf(2)+.5)),dx(3));
      pam= fdisp(args(1,:)(nx(1):nx(2),ny(1):ny(2)));
      fma;
      plsys,2;
      pli,pim,cmin=mnmx(1),cmax=mnmx(2);
      pl_box,qpn,color=PL_RED;
      for (i=1;i<opan(*);i++) pl_box,opan(noop(i)),color=PL_CYAN;
      plsys,1;
      pli,pam,cmin=mnmx(1),cmax=mnmx(2);
      button_plot,bbs;
    } else if (button_test(bright,x,y)) {
      brighten,colslp;
    } else if (button_test(dim,x,y)) {
      brighten,1/colslp;
    } else if (button_test(bgrab,x,y)) {
      if (is_void(qpn)) continue;
      if (is_void(nx)) continue;
      pl= xmouse(mstyp,win=win,system=1,forever=1,\
          prompt="in data WIN: clic-drag GRAB box");
      write,"REDO or DONE button";
      plsys,1;
      if (mstyp==1) { //box
        pl_box,pl,color=PL_RED;
        qdata= pl+[nx(1),nx(1),ny(1),ny(1)];
      } else if (mstyp==0) {  //pt
        plp,pl(2),pl(1),symbol=PL_STAR,color=PL_RED,width=3,fill=0;
        qdata= pl+[nx(1),ny(1)];
      } else if (mstyp==2) {  //line
        pldj,pl(1),pl(2),pl(3),pl(4),color=PL_RED;
        qdata= pl+[nx(1),ny(1),nx(1),ny(1)];
      }
    } else if (button_test(bredo,x,y)) {
      if (is_void(nx) || is_void(qpn)) continue;
      write,"PAN button";
      nx=ny= []; // going to re-pan
      qpn= [];
      if (opan(*)) opan=opan(:opan(*)-1)
    } else if (button_test(bdone,x,y)) {
      nx=ny= []; // going to re-pan
      qpn= [];              
      if (!is_void(qdata)) {
        qout= _(qout,qdata);
        write,"data ",pr1(qdata)(1);
        q= rdline(prompt="q to quit, RET for pair# "+pr1(cnt+1)+": ");
        if (q=="q")
          break;
        else
          write,"PAN button";
      } else 
        break;
    }
  } 
  return qout;
}
wrap_args, flwin;


func flwin2 (args)
/* DOCUMENT flwin,file_arr1,file_arr1,win=,mstyp=,width=,\
      height=,dpi,mnmx1=,mnmx2=,fdisp=,landscape=,wrat=
 
 *     MSTYP       RESULT
 *     ---------   ----------------
 *     0 "point"   [X,Y]
 *     1 "box"     [XMIN, XMAX, YMIN, YMAX] DEFAULT
 *     2 "line"    [X0, Y0, X1, Y1]
 *     3 "length"  LENGHT

   #include "plim.i"  
   dra= "tonzi_air/";
   fla= "ts675_p_"+["hhhh","hhvvc8","hvhv","vvvv"]+"_crp.dat";
   rcla= 1300; 

   dru= "";
   flu= "uav_2x15_"+["hhchv","hhcvh","hhcvv","shh"]+".dat";
   rclu= 1311;

   a= readFlat(dra+fla(1),float,[2,rcla],name="x");
   b= readFlat(dru+flu(0),float,[2,rclu],name="x");
  
   flwin2,a.x,b.x,win=1
   
 */
{
  s= ["win","width","height","dpi","mnmx1","mnmx2","fdisp",\
      "landscape","wrat","datfrac=","mstyp"];
  win=width=height=dpi=mnmx1=mnmx2=fdisp= [];
  landscape=wrat=datfrac= [];
  mstyp= 1;
  sk= args(-);        // key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,s(1)))) win=    args(s(1));
  if (nk && anyof(strmatch(sk,s(2)))) width=  args(s(2));
  if (nk && anyof(strmatch(sk,s(3)))) height= args(s(3));
  if (nk && anyof(strmatch(sk,s(4)))) dpi=    args(s(4));
  if (nk && anyof(strmatch(sk,s(5)))) mnmx1=  args(s(5));
  if (nk && anyof(strmatch(sk,s(6)))) mnmx2=  args(s(6));
  if (nk && anyof(strmatch(sk,s(7)))) fdisp=  args(s(7));
  if (nk && anyof(strmatch(sk,s(8)))) landscape=args(s(8));
  if (nk && anyof(strmatch(sk,s(9)))) wrat=   args(s(9));
  if (nk && anyof(strmatch(sk,s(10)))) datfrac= args(s(10));
  if (nk && anyof(strmatch(sk,s(11)))) mstyp= args(s(11));
  i=0; while (i<nk&&strmatch(s,sk(i+1))(sum)) i++; 
  if (i<nk) error,"unkown keyword: "+sk(i+1);
  // positional arg processing
  if (args(0)!=2) error,\
     "flwin2,flarr,flarr2,fdisp=,win=,width=,height=,dpi=,mnmx1=,,mnmx2=,"+\
     "lanscape=,wrat=,datfrac=";
  dx1= dimsof(args(1,:));
  dx2= dimsof(args(2,:));
  x1= args(1,:)(1,1);
  typ= structof(x1);
  if (is_void(fdisp)) fdisp= (typ==complex? plwin_abslog: plwin_log);
  if (is_void(win)) win= window();
  if (is_void(dpi)) dpi= 100;
  if (is_void(width)) width= (dpi==100? 510: 400);
  if (is_void(height)) height= (dpi==100? 600: 450);
  if (is_void(wrat)) wrat= 0.2;  // width ratio of preview
  if (is_void(datfrac)) datfrac= 0.8;  // width ratio of preview
  if (is_void(mnmx1)) {
    mnmx1= histchop(fdisp(args(1,:)(::5,::10)),datfrac);
    mnmx1+= mnmx1(dif)/4;
    write,format="Data#1 [min, Max]: [%.3f, %.3f]\n",mnmx1(1),mnmx1(2);
  }
  if (is_void(mnmx2)) {
    mnmx2= histchop(fdisp(args(2,:)(::5,::10)),datfrac);
    mnmx2+= mnmx2(dif)/4;
    write,format="Data#2 [min, Max]: [%.3f, %.3f]\n",mnmx2(1),mnmx2(2);
  } 

  b= 5;
  c= 40;  // button height 
  w= width;
  h= height;
  wp= (w-2*b)*wrat;
  vp= [[b,w-b-wp,b,h-b],[w-b-wp+1,w-b,b+c,h-b]]; // xm,xM,ym,yM
  vp= _(vp,vp+[w,w,0,0]);

  xw= xwindow(win,width=2*width,height=height,dpi=dpi,\
        landscape=landscape,viewport=vp,yopt=[0,0,0,0],\
        xopt=[0,0,0,0],units=2);
  gw= window_geometry();
  ndcb= gw(3:4)
  pxn= (dpi==100? 1064.38: 798.288);  //=1/xw(2) =1/gw(2)  
  // Pix [X,Y] to NDC [x,y]:  [x,y]= ndcb+[1,-1]*[X,Y]/pxn 
  // Pic origin 0 is upper-left, NDC 0 is lower-left
  
  wbp= [wp,c]/4.0; // px half [width,height] of four buttons in [wp,c] 
  wbn= wbp/pxn;
  bp= [w-b-wp/2,h-c/2-2]+[[-1,-1],[1,-1],[-1,1],[1,1]]*wbp;
  bn= ndcb+[1,-1]*bp/pxn;
  bpan1=  Button(text="pan", x=bn(1,1),y=bn(2,1),dx=wbn(1),dy=wbn(2));
  bgrab1= Button(text="grab",x=bn(1,2),y=bn(2,2),dx=wbn(1),dy=wbn(2));
  bredo1= Button(text="redo",x=bn(1,3),y=bn(2,3),dx=wbn(1),dy=wbn(2));
  bdone1= Button(text="done",x=bn(1,4),y=bn(2,4),dx=wbn(1),dy=wbn(2));
  bp= [w,0]+[w-2-wp/2,h-c/2-2]+[[-1,-1],[1,-1],[-1,1],[1,1]]*wbp;
  bn= ndcb+[1,-1]*bp/pxn;
  bpan2=  Button(text="pan", x=bn(1,1),y=bn(2,1),dx=wbn(1),dy=wbn(2));
  bgrab2= Button(text="grab",x=bn(1,2),y=bn(2,2),dx=wbn(1),dy=wbn(2));
  bredo2= Button(text="redo",x=bn(1,3),y=bn(2,3),dx=wbn(1),dy=wbn(2));
  bdone2= Button(text="done",x=bn(1,4),y=bn(2,4),dx=wbn(1),dy=wbn(2));

  nf1= long(dx1(2:3)/[wp,h-2*b-c]+0.5);
  pim1= fdisp(args(1,:)(::nf1(1),::nf1(2)));

  nf2= long(dx2(2:3)/[wp,h-2*b-c]+0.5);
  pim2= fdisp(args(2,:)(::nf2(1),::nf2(2)));

  plsys,2;
  pli,pim1,cmin=mnmx1(1),cmax=mnmx1(2);
  limits;
  
  plsys,4;
  pli,pim2,cmin=mnmx2(1),cmax=mnmx2(2);
  limits;
 
  bbs= [bpan1,bgrab1,bredo1,bdone1,bpan2,bgrab2,bredo2,bdone2];
  button_plot,bbs;

  qout= [];
  cnt= 0;
  opan1= save();
  opan2= save();
  iwn= 1;
  nx1=nx2=ny1=ny2= [];
  qpn1= qpn2 = [];

  write,"PAN#1 button";
  for (;;) {
    m= mouse(1, 2, "");
    if (is_void(m)) { rdline,prompt=""; continue; }
    x= m(5);
    y= m(6);
    if (button_test(bpan1,x,y)) {
      if (iwn==2) continue;
      qpn1= xmouse(1,win=win,system=2,forever=1,\
          prompt="in preview WIN# 1: clic-drag PAN box, then grab");
      write,"GRAB#1 button";
      save,opan1,string(0),qpn1;
      nx1= min(max(1,long(qpn1(1:2)*nf1(1)+.5)),dx1(2));
      ny1= min(max(1,long(qpn1(3:4)*nf1(2)+.5)),dx1(3));
      pam1= fdisp(args(1,:)(nx1(1):nx1(2),ny1(1):ny1(2)));
      fma;
      plsys,2;
      pli,pim1,cmin=mnmx1(1),cmax=mnmx1(2);
      pl_box,qpn1,color=PL_RED;
      for (i=1;i<opan1(*);i++) pl_box,opan1(noop(i)),color=PL_CYAN;
      plsys,4;
      pli,pim2,cmin=mnmx2(1),cmax=mnmx2(2);
      for (i=1;i<=opan2(*);i++) pl_box,opan2(noop(i)),color=PL_CYAN;
      plsys,1;
      pli,pam1,cmin=mnmx1(1),cmax=mnmx1(2);
      if (!is_void(pam2)) {
        plsys,3;
        pli,pam2,cmin=mnmx2(1),cmax=mnmx2(2);
      }
      button_plot,bbs;
    } else if (button_test(bgrab1,x,y)) {
      if (iwn==2) continue;
      if (is_void(qpn1)) continue;
      if (is_void(nx1)) continue;
      pl1= xmouse(mstyp,win=win,system=1,forever=1,\
          prompt="in data WIN# 1: clic-drag GRAB box");
      write,"REDO or DONE #1 button";
      plsys,1;
      if (mstyp==1) { //box
        pl_box,pl1,color=PL_RED;
        qdata1= pl1+[nx1(1),nx1(1),ny1(1),ny1(1)];
      } else if (mstyp==0) {  //pt
        plp,pl1(2),pl1(1),symbol=PL_STAR,color=PL_RED,width=3,fill=0;
        qdata1= pl1+[nx1(1),ny1(1)];
      } else if (mstyp==2) {  //line
        pldj,pl1(1),pl1(2),pl1(3),pl1(4),color=PL_RED;
        qdata1= pl1+[nx1(1),ny1(1),nx1(1),ny1(1)];
      }
    } else if (button_test(bredo1,x,y)) {
      if (iwn==2) continue;
      if (is_void(nx1) || is_void(qpn1)) continue;
      write,"PAN#1 button";
      nx1=ny1= []; // going to re-pan
      qpn1= [];
      if (opan1(*)) opan1=opan1(:opan1(*)-1)
    } else if (button_test(bdone1,x,y)) {
      if (is_void(qpn1)||is_void(qdata1)) continue;
      write,"data#1 ",pr1(qdata1)(1);
      if (iwn==2) continue;
      iwn= 2;
      nx1=ny1= []; // going to re-pan
      qpn1= [];              
      write,"PAN#2 button";
      /* ============================  WIN # 2 ======================*/
    } else if (button_test(bpan2,x,y))  {
      if (iwn==1) continue;
      qpn2= xmouse(1,win=win,system=4,forever=1,\
          prompt="in preview WIN# 2: clic-drag PAN box, then grab");
      write,"GRAB#2 button";
      save,opan2,string(0),qpn2;
      nx2= min(max(1,long(qpn2(1:2)*nf2(1)+.5)),dx2(2));
      ny2= min(max(1,long(qpn2(3:4)*nf2(2)+.5)),dx2(3));
      pam2= fdisp(args(2,:)(nx2(1):nx2(2),ny2(1):ny2(2)));
      fma;
      plsys,2;
      pli,pim1,cmin=mnmx1(1),cmax=mnmx1(2);
      for (i=1;i<=opan1(*);i++) pl_box,opan1(noop(i)),color=PL_CYAN;
      plsys,4;
      pli,pim2,cmin=mnmx2(1),cmax=mnmx2(2);
      pl_box,qpn2,color=PL_RED;
      for (i=1;i<opan2(*);i++) pl_box,opan2(noop(i)),color=PL_CYAN;
      plsys,1;
      pli,pam1,cmin=mnmx1(1),cmax=mnmx1(2);
      if (mstyp==1) { //box
        pl_box,pl1,color=PL_RED;
      } else if (mstyp==0) {  //pt
        plp,pl1(2),pl1(1),symbol=PL_STAR,color=PL_RED,width=3,fill=0;
      } else if (mstyp==2) {  //line
        pldj,pl1(1),pl1(2),pl1(3),pl1(4),color=PL_RED;
      }
      if (!is_void(pam1)) {
        plsys,3;
        pli,pam2,cmin=mnmx2(1),cmax=mnmx2(2);
      }
      button_plot,bbs;
    } else if (button_test(bgrab2,x,y)) {
      if (iwn==1) continue;
      if (is_void(nx2)) continue
      if (is_void(qpn2)) continue;
      pl2= xmouse(mstyp,win=win,system=3,forever=1,\
          prompt="in data WIN# 3: clic-drag GRAB box");
      write,"REDO or DONE #2 button";
      plsys,3;
      if (mstyp==1) { //box
        pl_box,pl2,color=PL_RED;
        qdata2= pl2+[nx2(1),nx2(1),ny2(1),ny2(1)];
      } else if (mstyp==0) {  //pt
        plp,pl2(2),pl2(1),symbol=PL_STAR,color=PL_RED,width=3,fill=0;
        qdata2= pl2+[nx2(1),ny2(1)];
      } else if (mstyp==2) {  //line
        pldj,pl2(1),pl2(2),pl2(3),pl1(4),color=PL_RED;
        qdata2= pl2+[nx2(1),ny2(1),nx2(1),ny2(1)];
      }
    } else if (button_test(bredo2,x,y)) {
      if (iwn==1) continue;
      if (is_void(nx2) || is_void(qpn2)) continue;
      nx2=ny2= []; // going to re-pan
      qpn2= [];
      if (opan2(*)) opan2=opan2(:opan2(*)-1)
    } else if (button_test(bdone2,x,y)) {
      if (iwn==1) continue;
      if (is_void(qpn2)||is_void(qdata2)) continue;
      iwn= 1;
      write,"data#2 ",pr1(qdata2)(1);
      cnt++;
      qout= _(qout,[[qdata1,qdata2]]);
      qdata1=qdata2=[]; //safety
      qpn2= [];              
      nx2=ny2= []; // going to re-pan
      q= rdline(prompt="q to quit, RET for pair# "+pr1(cnt+1)+": ");
      if (q=="q") break;
    }
  } 
  return qout;
}
wrap_args, flwin2;


