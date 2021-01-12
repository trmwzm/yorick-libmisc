
func binavrg (z, x, fac, &x10, badval=, badvalout=)
/* DOCUMENT: g= binavrg (z, x, fac, &x10, badval=, badvalout=)
   G is bin average of input randomly sampled (X) data (Z)
   Number of bins from fraction FRAC (if real) of input, or fixed (if FRAC integer).
     - trailing dimension of Z expected equal to that of X ("ni" below.)
     - bin ~boundaries~ returned in X10
   IFF provided *and* non-nil the bin boundaries extrema are taken from X10
   otherwise there are computed to have bin centers at MIN/MAX(X)

   no= numberof(g);
   output grid boundaries: go= span(x10(1),x10(2),no+1); ZCEN for bin center

   --- usage ---
   n= 20000;
   x= (random(n)(cum)-n/4)/(n/4);
   x= x(digitize(random(n),span(0,1,n+1))-1);
   // using a fraction (FAC is real) of 0.2
   fac= 0.2;
   g= binavrg(exp(-3*x^2),x,fac,x10); xx=span(x10(1),x10(2),numberof(g)); fma;plg,g,xx;
   // using a fixed numberof output points (FAC is integer)
   nfac= long(n*fac);
   g= binavrg(exp(-3*x^2),x,ifac,x10); xx=span(x10(1),x10(2),numberof(g)); fma;plg,g,xx;
   --- usage, again ---
   n= 20000;
   y= gaussm(random_n(n),3);
   x= span(0,1.,n);
   bfrac= 0.1;
   ib= long(random(long(n*bfrac+0.5))*n+1);
   bval= -10000.0;
   y(ib)= bval;
   yy= binavrg(y,x,.01,x01,badval=bval);
   fma;plg,max(y,-2),x;plg,yy,span(x01(1),x01(2),numberof(yy)),color="red";
*/
{
  dz= dimsof(z);
  ni= dz(0);
  nl= [];    // #leading dims, if any
  if (dz(1)>1)
    z= reform(z,[2,(nl=numberof(z)/ni),ni]);
  else
    z= z(-,..);

  no= is_integer(fac)? fac: (is_real(fac)? min(max(long(fac*ni+0.5),1),ni): []);
  badvalout= is_void(badvalout)? 0: badvalout;
  zo= array(double(badvalout),nl,no);      // ok with nl==[]
  if (dz(1)==1)
    zo= zo(-,..);

  if (is_void(x10)) {
    x1= x(min);
    x0= x(max);
    dx= (x0-x1)/no;
    x10= [x1-dx/2,x0+dx/2];
  }

  go= span(x10(1),x10(2),no+1);

  for (il=1; il<=dimsof(z)(2); il++) {
    if (!is_void(badval)) {
      wgd= !is_void(badval)? where(z(il,..)!=badval): indgen(ni);
      li= digitize(x(wgd),go)-1;
    } else
      li= digitize(x,go)-1;
    hz= histogram(li,top=no);
    wh= where(hz!=0);
    zo(il,wh)= histogram(li,z(il,wgd))(wh)/hz(wh);
  }

  dz(0)= no;
  return reform(zo,dz);
}

/*------------------------------------------------------------------*/

func imbinavrg (z, fac1, fac2, &go1, &go2, badval=, badvalout=)
/* DOCUMENT: imbinavrg (z, fac1, fac2, badval=, badvalout=)
   ni1/2= dimsof(z)(2/3)
   output gris: go1= span(0.5,ni1+0.5,no1+1)(zcen);
   go2= span(0.5,ni2+0.5,no2+1)(zcen);
*/
{
  dz= dimsof(z);
  if (dz(1)!=2)
    error;
  ni1= dz(2);
  ni2= dz(3);
  if (is_void(badval))
    badval=0;
  if (is_void(badvalout))
    badvalout=badval;

  if (is_void(fac1))
    error;
  if (is_void(fac2))
    fac2=fac1;

  if (is_integer(fac1))
    no1= fac1;
  else
    no1= min(max(long(fac1*ni1+0.5),1),ni1);

  if (is_integer(fac2))
    no2= fac2;
  else
    no2= min(max(long(fac2*ni2+0.5),1),ni2);

  go1= span(0.5,ni1+0.5,no1+1);
  go2= span(0.5,ni2+0.5,no2+1);

  wgd= where(z!=badval);
  igd= indim(wgd,dz);

  li1= digitize(igd(1,),go1)-1;
  li2= digitize(igd(2,),go2)-1;

  zo= array(structof(z)(badvalout), no1, no2);

  zndx= (li1+(li2-1)*no1);          // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

  hz= histogram(zndx, top=no1*no2);
  wh= where(hz!=0);
  zo(wh)= histogram(zndx, z(wgd))(wh)/hz(wh);

  go1= go1(zcen);
  go2= go2(zcen);
  return zo;
}

/*------------------------------------------------------------------*/

func xyzbinavrg (x,y,z, n1, n2, badval=, badvalout=)
/* DOCUMENT xyzbinavrg
   where x refers to 1st index in output grid
   y refers to 2nd index in output grid
   z is the value in output grid
   output gris: go1= span(0.5,ni1+0.5,no1+1)(zcen);
   go2= span(0.5,ni2+0.5,no2+1)(zcen);
   n=24
   getModGrid_init, n, rstr=1;
   din= [3,GRIDAR(n).nxgrid,GRIDAR(n).ngridface,GRIDAR(n).nygrid];
   dpt= readFlat(TRMROOT+GRIDAR(n).griddir+"Depth.data",float,din);
   fma;pli,xyzbinavrg (MLON,MLAT,addedges(dpt), 360, 180, badval=MODATNAN, badvalout=0)
*/
{
  dz= dimsof(z);
  dy= dimsof(y);
  dx= dimsof(x);
  if (anyof(dz!=dx)||anyof(dy!=dx))
    error;

  ni= numberof(z);

  if (is_void(badval))
    badval= 0;
  if (is_void(badvalout))
    badvalout= badval;

  if (is_void(n1))
    error;
  if (is_void(n2))
    n2= n1;

  if(!is_integer(n1))
    error;
  if (!is_integer(n2))
    error;

  xmnmx= [x(*)(min),x(*)(max)];
  ymnmx= [y(*)(min),y(*)(max)];

  lx= xmnmx(dif)(1);
  ly= ymnmx(dif)(1);

  go1= span(xmnmx(1)-2.*lx/n1,xmnmx(2)+2.*lx/n1,n1+1);
  go2= span(ymnmx(1)-2.*ly/n2,ymnmx(2)+2.*ly/n2,n2+1);

  wgd= where(z!=badval);

  li1= digitize(x(wgd),go1)-1;
  li2= digitize(y(wgd),go2)-1;

  zo= array(structof(z)(badvalout), n1, n2);

  zndx= (li1+(li2-1)*n1);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

  hz= histogram(zndx, top=n1*n2);
  wh= where(hz!=0);
  zo(wh)= histogram(zndx, z(wgd))(wh)/hz(wh);

  go1= go1(zcen);
  go2= go2(zcen);
  return zo;
}
/*------------------------------------------------------------------*/

func gridbinavrg (gi1, gi2, z, go1, go2, &zndx, badval=, badvalout=, reuse=, proptresh=)
/* DOCUMENT gridbinavrg (gi1, gi2, z, go1, go2, &zndx, badval=, badvalout=,reuse=,proptresh=)
   gridbinavrg z on grid gi1/gi2 to go1/go2
   in and out grids are assumed to be SEPARABLE (xgrid * ygrid) "ll" type grid
   gi/o/1/2 also assumed monotonically increasing
   if there is a third dimension to z (nimberof(gi1), numberof(gi2), NZ)\
   the binaverage is carried out NZ times
*/
{
  di1= dimsof(gi1);
  di2= dimsof(gi2);
  dz= dimsof(z);

  if (dz(2)!=di1(2) || dz(3)!=di2(2))
    error;

  if (dz(1)==3)
    n3= dz(4);
  else
    n3= 1;

  ni= numberof(z);

  if (is_void(badval))
    badval= 0;
  if (is_void(badvalout))
    badvalout= badval;

  if (is_void(go1) || is_void(go2))
    error;
  n1= numberof(go1);
  n2= numberof(go2);

  zo= array(structof(z)(badvalout), n1, n2, n3);
  zo1= array(structof(z)(badvalout), n1, n2);

  gbo1= go1(pcen);gbo1(1)-=gbo1(2)-gbo1(1);gbo1(0)+=gbo1(0)-gbo1(-1);
  gbo2= go2(pcen);gbo2(1)-=gbo2(2)-gbo2(1);gbo2(0)+=gbo2(0)-gbo2(-1);

  if (reuse!=1 || is_void(zndx)) {                // kludgy stuff
    li1= digitize(gi1(,-:1:di2(2)),gbo1)-1;
    mt= li1==n1+1;
    if (anyof(mt))
      li1(where(mt))= 1; // U case
    li2= digitize(gi2(-:1:di1(2),),gbo2)-1;
    mt= li2==n2+1;
    if (anyof(mt))
      li2(where(mt))= n2; // V case
    zndx= (li1+(li2-1)*n1);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)
  }

  for (iz=1;iz<=n3;iz++) {
    z1= z(,,iz);
    zo1(*)= badvalout;
    wgd= where(z1!=badval);

    zndxz= zndx(wgd);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

    hz= histogram(zndxz, top=n1*n2);
    if (!is_void(proptresh))
      countmin= proptresh*hz(max);
    else
      countmin= 0;

    wh= where(hz>countmin);
    zo1(wh)= histogram(zndxz, z1(wgd))(wh)/hz(wh);

    if(nz==1)
      return zo1;

    zo(,,iz)= zo1;
  }

  return zo;
}

/*------------------------------------------------------------------*/

func imbadfill (h, fac1=, fac2=, badval=)
{
  if (is_void(fac1))
    fac1= 0.05;
  if (is_void(fac2))
    fac2=fac1;
  if (is_integer(fac1) || is_integer(fac2))
    error,"Expecting FAC1/2 as real n_out/n_in fraction.";

  badval= is_void(badval)? h(*)(min): badval;

  local go1, go2;
  hh= imbinavrg(h,fac1,fac2,go1,go2,badval=badval,badvalout=badval);

  wh= where(h==badval);
  wh2= indim(wh,dimsof(h));

  y0= go2(long(wh2(2,..)*fac1+0.5));
  x0= go1(long(wh2(1,..)*fac2+0.5));
  reg= reform(indgen(numberof(hh)),dimsof(hh));
  m= hh==badval;
  if (anyof(m)) {
    w= where(m);
    reg(w)= 0;
    hh(w)= avg(hh(where(!m)));
  }
  xx= go1(,-:1:numberof(go2));
  yy= go2(-:1:numberof(go1),);
  hb= interp2(y0,x0,hh,yy,xx,reg);

  ho= h;
  ho(wh)= hb;
  return ho;
}
