require, "imbinavrg.i";
require, "poly_fit.i";

scratch= save(scratch, tmp);
tmp= save(eval, _eval1, stats, scale, elacs);
func digfun (base, y, x, type=, degree=, dydx0=, dydx1=, tension=, nxfit=, equid=, quiet=)
/* DOCUMENT f= digfun (y, x, type=, degree=, dydx0=, dydx1=, tension=, nxfit=, equid=, quiet=)
   F is a closure evaluating Y(XP) by interpolation of the
   data provided at construction. The interplation method used
   set through an instance TYPE.
   for polynomial interpolation POLY, DEGREE is that of the polynomial
   for spline interpolation     SPLINE, DYDX0/1=:start/end slopes  TENSION=: tension
   for spline interpolation     SPLINET, DYDX0/1=:start/end slopes NXFIT=: number of cubic knots
 */
{
  clsn= "digfun";
  op= save(digfun_lin, digfun_splinelsq, digfun_spline, \
           digfun_poly);    //  ,digfun_cheby

  typs= strpart(op(*,),strlen(clsn)+2:);
  if (is_void(y))
    return typs;
  if (is_void(type) || noneof(type==typs))
    error,"Must supply digfun TYPE, which is one of: "+(typs+" ")(sum);

  ob= base(:);

  ob, op=op;

  dy= dimsof(y);
  if (dy(0)!=dimsof(x)(0))
    error,"x & y not conformable";

  y= y(*,);             // *internal*
  ny= dimsof(y)(2);
  nxy= dimsof(y)(0);

  ob, dy=dy, nxy=nxy, ny=ny, type=type;

  ob, stats, y, x;
  ob, scale, y, x, dydx0, dydx1;

  ob, splin= save();
  if (strgrepm("spline",type)) {
    save, ob(splin), nxfit=nxfit;
    save, ob(splin), tension=tension;
    save, ob(splin), dydx0=dydx0;
    save, ob(splin), dydx1=dydx1;
    save, ob(splin), equid=equid;
    if (!quiet && type=="spline" && (!is_void(nxfit) || !is_void(equid)))
      write,"Warning: provided NXFIT=, or EQUID=, keys not used, see splinelsq.";
    if (!quiet && !is_void(degree))
      write,"WARNING: provided DEGREE*= key[s] ignored, sets polynomial degree.";
  }

  ob, pol=save();
  if (type=="poly") {
    save, ob(pol), degree;
    if (!quiet && (!is_void(nxfit) || !is_void(tension) || !is_void(dydx0) || !is_void(dydx1)))
      write,"WARNING: provided NXFIT/TENSION/DYDX*= key[s] ignored, see spline.";
  }

  return op(clsn+"_"+type,ob,y,x);
}
func eval (x, deriv=, deriv2=, integ=, outside=)
{
  use, dy, ny, xmin, xmax;

  use_method, scale, 1, x;

  nx= numberof(x);
  y= array(double, [2,ny,nx]);
  for (i=1; i<=ny; i++)
    y(i,..)= use_method(_eval1,i,x,deriv=deriv,deriv2=deriv2,integ=integ);

  use_method, elacs, y, 1, deriv=deriv,deriv2=deriv2,integ=integ;

  if (!is_void(outside)) {
    m= x<xmin | x>xmax;
    if (anyof(m)) {
      w= where(m);
      y(,w)= outside;
    }
  }

  dyx= dy;
  dyx(0)= nx;
  return reform(y,dyx);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  error,"virtual func";
}
func stats (&y, &x)
{
  xmin= min(x);
  xmax= max(x);
  ymin= y(,min);
  ymax= y(,max);
  save, use, xmin, xmax, ymin, ymax;
}
func scale (&y, &x, &dydx0, &dydx1)
{
  use, xmin, xmax, ymin, ymax;
  x= (x-xmin)/(xmax-xmin)-0.5;
  m= ymax==ymin;
  y= (y-ymin)/(ymax-ymin+m)-0.5;
  if (!is_void(dydx0))
    dydx0*= (xmax-xmin)/(ymax-ymin+m);
  if (!is_void(dydx1))
    dydx1*= (xmax-xmin)/(ymax-ymin+m);

}
func elacs (&y, &x,deriv=,deriv2=,integ=)
{
  use, xmin, xmax, ymin, ymax;

  x= xmin+(x+0.5)*(xmax-xmin);
  if (deriv)
    y= y*(ymax-ymin)/(xmax-xmin);
  else if (deriv2)
    y= y*(ymax-ymin)/(xmax-xmin)^2;
  else if (integ)
    y= y*(ymax-ymin)*(xmax-xmin);
  else
    y= ymin+(y+0.5)*(ymax-ymin);
}
digfun= closure(digfun,restore(tmp)); restore, scratch;

/*-------------------------- SPLINELSQ ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_splinelsq (base, ob, y, x)
/* DOCUMENT m= digfun(y,x,type="splinelsq")
   discontinuous second derivatives
 */
{
  save, ob, [], base(:); //  clobber eval, scale, elacs

  if (is_void(ob(splin,nxfit)))
    error,"must provide NXFIT.";

  if (is_void(ob(splin,equid))) {
    xf= span(-0.5,0.5,ob(splin,nxfit));  // better ... implement XY equispaced
  } else {
    xf= equidx(y,x,ob(splin,nxfit));
  }

  a= array(0.0,[3,ob(ny),3,ob(splin,nxfit)]);
  for (i=1; i<=ob(ny); i++)
    a(i,1:3,..)= splinelsq(y(i,..),x,xf,dydx0=(is_void(ob(splin,dydx0))? []: ob(splin,dydx0,i)), \
                      dydx1=(is_void(ob(splin,dydx1))? []: ob(splin,dydx1,i)));

  save, ob, a;

  return closure(ob, eval);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  use, a;
  if (deriv==1)
    return splined(a(i,..),x);
  else if (deriv2==1){  // continuous approximation kludge
    return splinei(a(i,..),x);
  } else if (integ==1)
    error,"not implemented";
  else
    return splinef(a(i,..),x);
}
digfun_splinelsq= closure(digfun_splinelsq,restore(tmp));restore, scratch;

/*-------------------------- SPLINE ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_spline (base, ob, y, x)
/* DOCUMENT */
{
  save, ob, [], base(:);

  a= array(0.0,[3,ob(ny),5,ob(nxy)]);
  for (i=1; i<=ob(ny); i++) {
    a(i,1,..)= spline(y(i,..),x,dydx0=(is_void(ob(splin,dydx0))? []: ob(splin,dydx0,i)), \
                      dydx1=(is_void(ob(splin,dydx1))? []: ob(splin,dydx1,i)));
    a(i,2,..)= spline(a(i,1,..),x);  // 2: d2y/dx2
    a(i,3,..)= spline(a(i,2,..),x);  // 3: d3y/dx3
    a(i,4,..)= y(i,..);
    a(i,5,..)= x;
  }

  save, ob, a;

  return closure(ob, eval);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  use, a, splin;

  if (is_void(splin(tension))) {
    if (deriv==1)
      return sprime(a(i,1,..),a(i,4,..),a(i,5,..),x);
    else if (deriv2==1){  // continuous approximation kludge
      return spline(a(i,3,..),a(i,2,..),a(i,5,..),x);
    } else if (integ==1)
      error,"not implemented";
    else
      return spline(a(i,1,..),a(i,4,..),a(i,5,..),x);
  } else {
    if (deriv==1)
      return tspline(splin(tension),a(i,2,..),a(i,1,..),a(i,5,..),x);
    else if (deriv2==1){  // continuous approximation kludge
      return tspline(splin(tension),a(i,3,..),a(i,2,..),a(i,5,..),x);
    } else if (integ==1)
      error,"not implemented";
    else
      return tspline(splin(tension),a(i,1,..),a(i,4,..),a(i,5,..),x);
  }
}
digfun_spline= closure(digfun_spline,restore(tmp));restore, scratch;

/*-------------------------- LIN ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_lin (base, ob, y, x)
/* DOCUMENT m= digfun(y, x, type="lin")
   discontinuous second derivatives
 */
{
  save, ob, [], base(:); //  clobber _eval1

  a= array(0.0,[3,ob(ny),4,ob(nxy)]);
  for (i=1; i<=ob(ny); i++) {
    a(i,1,..)= x;
    a(i,2,..)= y(i,..);
    a(i,3,..)= ((a(i,2,:-1)-a(i,2,2:))/(a(i,1,:-1)-a(i,1,2:)))(pcen);
    a(i,4,..)= ((a(i,3,:-1)-a(i,3,2:))/(a(i,1,:-1)-a(i,1,2:)))(pcen);
  }

  save, ob, a;

  return closure(ob, eval);
}
func _eval1 (i, x, deriv=, deriv2=, integ=)
{
  use, a;

  out= x;
  if (deriv==1)
    return interp(a(i,3,..),a(i,1,..),x);
  else if (deriv2==1)
    return interp(a(i,4,..),a(i,1,..),x);
  else if (integ==1)
    return integ(a(i,2,..),a(i,1,..),x);
  else
    return interp(a(i,2,..),a(i,1,..),x);
}
digfun_lin= closure(digfun_lin,restore(tmp));restore, scratch;

/*-------------------------- POLY ------------------------*/
scratch= save (scratch, tmp); tmp= save(_eval1);
func digfun_poly (base, ob, y, x)
/* DOCUMENT m= digfun(y, x, type="poly")
 */
{
  save, ob, [], base(:); //  clobber _eval1

  a= array(0.,[2,ob(ny),ob(pol,degree)+1]);
  for (i=1; i<=ob(ny); i++)
    a(i,..)= poly1_fit(y(i,..),x,ob(pol,degree));

  save, ob, degree=ob(pol,degree);

  save, ob, a;

  return closure(ob, eval);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  use, a, degree;

  if (deriv==1)
    y= poly1_deriv(x, a(i,..));
  else if (deriv2==1)
    y= poly1_deriv(x, a(i,2:)*indgen(degree));
  else if (integ==1)
    y= poly1(x, _(0,a(i,..)/indgen(degree+1)));
  else
    y= poly1(x, a(i,..));
  return y;
}
digfun_poly= closure(digfun_poly,restore(tmp));restore, scratch;

/*-------------------------- TEST ------------------------*/
#if 0
func test (n=, figs=, type=, degree=, tension=, nxfit=)
{
  if (is_void(n))
    n= 80;

  xmin= 2;
  xmax= 2.4;

  yoff= 3;
  yamp= 1.5;

  decim=2;

  x= span(xmin,xmax,n);
  y= yoff+yamp*transpose([sin(2*pi*x),cos(2*pi*x)]);

  f= digfun(y,x,type=type,degree=degree,tension=tension,nxfit=nxfit);

  lx= xmax-xmin;
  xx= span(xmin,xmax-lx/10,n/decim)+random(n/decim)*lx/10;
  yy= yoff+yamp*transpose([sin(2*pi*xx),cos(2*pi*xx)]);
  dyy= yamp*transpose([2*pi*cos(2*pi*xx),-2*pi*sin(2*pi*xx)]);
  yf= f(xx);
  dyf= f(xx,deriv=1);

  yydiff= (yy-yf)(,2:-1);
  yddiff= (dyy-dyf)(,2:-1);
  write,type,format="%s\n";
  write,[1,2],yydiff(,avg),yydiff(,rms),format="func #%i: avgdiff=%g, rmsdiff=%g\n";
  write,[1,2],yddiff(,avg),yddiff(,rms),format="func deriv #%i: avgdiff=%g, rmsdiff=%g\n";

  if (figs) {
    window,0;
    fma;
    plg,yy(1,..),xx;plg,yf(1,..),xx,type=4;
    plg,yy(2,..),xx,color="red";plg,yf(2,..),xx,color="red",type=4;
    pltitle,"F residual: type= "+type+", degree= "+pr1(degree)+", n="+pr1(n);
    limits;

    window,1;
    fma;
    plg,dyy(1,..),xx;plg,dyf(1,..),xx,type=4;
    plg,dyy(2,..),xx,color="red";plg,dyf(2,..),xx,color="red",type=4;
    pltitle,"dF/dX residual:type= "+type+", degree= "+pr1(degree)+", n="+pr1(n);
    limits;

    window,2;
    fma;
    xx= xx(2:-1);
    plg,yydiff(1,..),xx;plg,yydiff(2,..),xx,color="red";
    plg,yddiff(1,..),xx,type=4;plg,yddiff(2,..),xx,color="red",type=4;
    pltitle,"F & dF/dX(dash) residual:type= "+type+", degree= "+pr1(degree)+", n="+pr1(n);
    limits;
  }
}

test,n=50,type="lin",figs=1;
q= rdline(prompt=" q quits");

test,n=50,type="poly",figs=1,degree=17;
q= rdline(prompt=" q quits");

test,n=50,type="spline",figs=1;
q= rdline(prompt=" q quits");

test,n=50,type="spline",figs=1, tension=4.0;
q= rdline(prompt=" q quits");

test,n=50,type="splinelsq",figs=1, nxfit=10;
q= rdline(prompt=" q quits");

#endif

#if 0

func foo (x, &df) {
  y= sqrt(1-x^2);
  if (is_scalar(x))
    df= y==0? -1e30: -x/y;
  else
    df= merge2(-1e30,-x/(y+(y==0)),y==0);
  return y;
}
n= 40;
x= span(0,1,n);
dy= [];
y= foo(x,dy);

m= 2000;
xx= span(0,1,m);
yy= foo(xx);

f= digfun(y,x,type="spline");
y_splin= f(xx);

f= digfun(yy,xx,type="splinelsq",nxfit=n);
y_splinlsq= f(xx);

f= digfun(yy,xx,type="splinelsq",nxfit=2*n,equid=1,dydx0=dy(1),dydx1=dy(0));
y_splinlsq10= f(xx);

fma;plg,yy-y_splin,xx;plg,yy-y_splinlsq,xx,color="red";plg,yy-y_splinlsq10,xx,color="blue";

#endif
