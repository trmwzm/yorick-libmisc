require, "imbinavrg.i";
require, "poly_fit.i";

scratch= save(scratch, tmp);
tmp= save(eval, _eval1, stats, scale, elacs, digfun_type);
func digfun (base, y, x, type=, degree=)
/* DOCUMENT f= digfun (y, x, type=, degree=)
   F is a closure evaluating Y(XP) by interpolation of the
   data provided at construction. The interplation method used
   set through an instance TYPE.
   for polynomial interpolation POLY, DEGREE is that of the polynomial
   for SPLINELSQ, DEGREE is the numberof knots.
 */
{
  ob= base(:);

  op= save(digfun_lin, digfun_splinelsq, digfun_spline, \
           digfun_poly);    //  ,digfun_cheby
  ob, op=op;

  clsn= "digfun";
  typs= strpart(op(*,),strlen(clsn)+2:);
  if (is_void(y))
    return typs;
  if (is_void(type) || noneof(type==typs))
    error,"Must supply digfun TYPE, which is one of: "+(typs+" ")(sum);

  dy= dimsof(y);
  if (dy(0)!=dimsof(x)(0))
    error,"x & y not conformable";
  n= dy(0);

  y= y(*,);             // *internal*
  ny= dimsof(y)(2);

  ob, dy=dy, n=n, ny=ny;

  ob, stats, y, x;
  ob, scale, y, x;

  return ob(digfun_type,clsn+"_"+type,ob,y,x,degree=degree);
}
func eval (x, deriv=, deriv2=, integ=)
{
  use, dy, n, ny;

  use_method, scale, 1, x;

  nx= numberof(x);
  y= array(double, [2,ny,nx]);
  for (i=1; i<=ny; i++)
    y(i,..)= use_method(_eval1,i,x,deriv=deriv,deriv2=deriv2,integ=integ);

  use_method, elacs, y, 1, deriv=deriv,deriv2=deriv2,integ=integ;

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
  save, use, xmin, xmax, ymin, ymax;
  xmin= min(x);
  xmax= max(x);
  ymin= y(,min);
  ymax= y(,max);
}
func scale (&y, &x)
{
  use, xmin, xmax, ymin, ymax;
  x= (x-xmin)/(xmax-xmin)-0.5;
  y= (y-ymin)/(ymax-ymin)-0.5;
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
func digfun_type (type, o, y, x, degree=)
{
  use, op;
  return op(noop(type),o,y,x,degree=degree);
}
digfun= closure(digfun,restore(tmp)); restore, scratch;

/*-------------------------- SPLINELSQ ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_splinelsq (base, ob, y, x, degree=)
/* DOCUMENT m= digfun(y,x,type="splinelsq")
   discontinuous second derivatives
 */
{
  save, ob, [], base(:); //  clobber eval, scale, elacs

  if (is_void(degree))
    error,"Provide DEGREE= value, which is the number of knots.";

  xf= span(-0.5,0.5,degree);
  dydx1= (y(,2)-y(,1))/(x(2)-x(1));
  dydx0= (y(,2)-y(,1))/(x(2)-x(1));

  a= array(0.0,[3,ob(ny),9,degree]);
  xx= x(zcen);
  xd= x(dif);
  for (i=1; i<=ob(ny); i++) {
    a(i,1:3,..)= splinelsq(y(i,..),x,xf,dydx0=dydx0(i,),dydx1=dydx1(i,));    // 1: dydx
    yy= y(i,dif)/xd;
    a(i,4:6,..)= splinelsq(yy,xx,xf,dydx0=0.0,dydx1=0.0);  // 2: d2y/dx2
    a(i,7:9,..)= splinelsq(yy(dif),xx(zcen),xf,dydx0=0.0,dydx1=0.0);  // 3: d3y/dx3
  }

  save, ob, a, degree;
  save, ob, type="digfun_spline";

  return closure(ob, eval);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  use, a;
  if (deriv==1)
    return splinef(a(i,4:6,..),x);
  else if (deriv2==1){  // continuous approximation kludge
    return splinef(a(i,7:9,..),x);
  } else if (integ==1)
    error,"not implemented";
  else
    return splinef(a(i,1:3,..),x);
}
digfun_splinelsq= closure(digfun_splinelsq,restore(tmp));restore, scratch;

/*-------------------------- SPLINE ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_spline (base, ob, y, x, degree=)
/* DOCUMENT */
{
  save, ob, [], base(:);

  dydx1= (y(,2)-y(,1))/(x(2)-x(1));
  dydx0= (y(,2)-y(,1))/(x(2)-x(1));

  a= array(0.0,[3,ob(ny),5,ob(n)]);
  for (i=1; i<=ob(ny); i++) {
    a(i,1,..)= spline(y(i,..),x,dydx0=dydx0(i,),dydx1=dydx1(i,));    // 1: dydx
    a(i,2,..)= spline(a(i,1,..),x,dydx0=0.0,dydx1=0.0);  // 2: d2y/dx2
    a(i,3,..)= spline(a(i,2,..),x,dydx0=0.0,dydx1=0.0);  // 3: d3y/dx3
    a(i,4,..)= y(i,..);
    a(i,5,..)= x;
  }

  save, ob, a;
  save, ob, type="digfun_spline";

  return closure(ob, eval);
}
func _eval1 (i,x,deriv=,deriv2=,integ=)
{
  use, a;
  if (deriv==1)
    return spline(a(i,2,..),a(i,1,..),a(i,5,..),x);
  else if (deriv2==1){  // continuous approximation kludge
    return spline(a(i,3,..),a(i,2,..),a(i,5,..),x);
  } else if (integ==1)
    error,"not implemented";
  else
    return spline(a(i,1,..),a(i,4,..),a(i,5,..),x);
}
digfun_spline= closure(digfun_spline,restore(tmp));restore, scratch;

/*-------------------------- INTERP ------------------------*/
scratch= save(scratch, tmp); tmp= save(_eval1);
func digfun_lin (base, ob, y, x, degree=)
/* DOCUMENT m= digfun(y, x, type="lin")
   discontinuous second derivatives
 */
{
  save, ob, [], base(:); //  clobber _eval1

  a= array(0.0,[3,ob(ny),4,ob(n)]);
  for (i=1; i<=ob(ny); i++) {
    a(i,1,..)= x;
    a(i,2,..)= y(i,..);
    a(i,3,..)= ((a(i,2,:-1)-a(i,2,2:))/(a(i,1,:-1)-a(i,1,2:)))(pcen);
    a(i,4,..)= ((a(i,3,:-1)-a(i,3,2:))/(a(i,1,:-1)-a(i,1,2:)))(pcen);
  }

  save, ob, a;
  save, ob, type="digfun_lin";

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
func digfun_poly (base, ob, y, x, degree=)
/* DOCUMENT m= digfun(y, x, type="poly")
 */
{
  save, ob, [], base(:); //  clobber _eval1

  degree= is_void(degree)? 5: degree;

  a= array(0.,[2,ob(ny),degree+1]);
  for (i=1; i<=ob(ny); i++)
    a(i,..)= poly1_fit(y(i,..),x,degree);
  save, ob, a, degree;
  save, ob, type="digfun_poly";

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
#if 1
func test (n=,type=,degree=)
{
  if (is_void(n))
    n= 80;

  xmin= 2;
  xmax= 2.2;

  yoff= 3;
  yamp= 1.5;

  x= span(xmin,xmax,n);
  y= yoff+yamp*transpose([sin(2*pi*x),cos(2*pi*x)]);

  f= digfun(y,x,type=type,degree=degree);

  xx= span(xmin,xmax,n/2);
  yy= yoff+yamp*transpose([sin(2*pi*xx),cos(2*pi*xx)]);
  dyy= yamp*transpose([2*pi*cos(2*pi*xx),-2*pi*sin(2*pi*xx)]);
  yf= f(xx);
  dyf= f(xx,deriv=1);

  window,0;
  fma;
  plg,yy(1,..),xx;plg,yf(1,..),xx,type=4;
  plg,yy(2,..),xx,color="red";plg,yf(2,..),xx,color="red",type=4;
  pltitle,"F residual: type= "+type+", decim= "+pr1(decim)+", n="+pr1(n);
  limits;

  window,1;
  fma;
  plg,dyy(1,..),xx;plg,dyf(1,..),xx,type=4;
  plg,dyy(2,..),xx,color="red";plg,dyf(2,..),xx,color="red",type=4;
  pltitle,"dF/dX residual:type= "+type+", decim= "+pr1(decim)+", n="+pr1(n);
  limits;
}
#endif
