scratch= save(scratch, tmp); tmp= save(_eval,digfun_type);
func digfun (base,y,x,decim=,type=)
/* DOCUMENT
 */
{
  if (!is_void(type))
    return base(digfun_type,type,y,x,decim=decim);

  ob= base(:);
  save, ob, type="digfun";
  save, ob, a= 0.0;            // fit parameter
  save, ob, xmin= 0.0;
  save, ob, xmax= 0.0;
  save, ob, dx= 0.0;
  save, ob, yav= 0.0;
  return closure(ob,_eval);
}
func _eval (x,deriv=,integ=,deriv2=)
{
  error,"virtual func";
}
func digfun_type (type,y,x,decim=)
{
  return save(digfun_lin,digfun_splinelsq,digfun_spline,                \
              digfun_fft,digfun_cheby)(noop(type),y,x,decim=decim);
}
digfun= closure(digfun,restore(tmp)); restore, scratch;

scratch= save(scratch, tmp); tmp= save(_eval);
func digfun_splinelsq (base,y,x,decim=)
/* DOCUMENT m= digfun_splinelsq (base,y,x,decim=)
            m= digfun (base,y,x,decim=,type="digfun_splinelsq")
   discontinuous second derivatives
 */
{
  ob= digfun().function;

  save, ob, [], base(:); //  clobber eval

  d= dimsof(y,x);
  if (is_void(d))
    error,"x & y not conformable";
  n= d(2);

  if (is_void(decim))
    decim= 10;

  nf= n/decim;

  yav= y(avg);

  save, ob, xmin= min(x);
  save, ob, xmax= max(x);
  save, ob, dx= (ob(xmax)-ob(xmin))/(nf-1);
  save, ob, yav= yav;
  save, ob, decim;

  xf= span(ob(xmin),ob(xmax),nf);
  // -- do a LSQ qpline then fake continuous 2nd derivative
  sf= splinelsq(y-yav,x,xf,dydx0=0.0,dydx1=0.0);
  sf2= spline(sf(3,..),xf,dydx0=0.0,dydx1=0.0);
  sf3= spline(sf2,xf,dydx0=0.0,dydx1=0.0);

  a= xf(-:1:5,..);
  a(1:3,..)= sf;
  a(4,..)= sf2;
  a(5,..)= sf3;

  save, ob, a;
  save, ob, type="digfun_splinelsq";

  return closure(ob,_eval);
}
func _eval (x,deriv=,deriv2=,integ=)
{
  use, a,yav;
  out= x;
  if (deriv==1)
    return splined(a(1:3,..),x);
  else if (deriv2==1)  // continuous approximation kludge
    return spline(a(5,..),a(4,..),a(1,..),x);
  else if (integ==1)
    return yav*x+splinei(a(1:3,..),x);
  else
    return yav+splinef(a(1:3,..),x);
}
digfun_splinelsq= closure(digfun_splinelsq,restore(tmp));restore, scratch;


scratch= save(scratch, tmp); tmp= save(_eval);
func digfun_spline (base,y,x,decim=)
/* DOCUMENT */
{
  ob= digfun().function;

  save, ob, [], base(:);

 d= dimsof(y,x);
  if (is_void(d))
    error,"x & y not conformable";
  n= d(2);

  if (is_void(decim))
    decim= 1;

  nf= n/decim;

  x10= [x(1),x(0)];
  y10= [y(1),y(0)];
  
  x= reform(x(1:decim*nf),[2,decim,nf])(avg,..);
  y= reform(y(1:decim*nf),[2,decim,nf])(avg,..);

  if (decim>1) {
    x= _(x10(1),x,x10(2));
    y= _(y10(1),y,y10(2));
    nf+= 2;
  }
  
  yav= y(avg);
  y-= yav;

  save, ob, xmin= min(x);
  save, ob, xmax= max(x);
  save, ob, dx= (ob(xmax)-ob(xmin))/(nf-1);
  save, ob, yav= yav;
  save, ob, decim;

  if (x(ptp)!=0.) {
    sf= spline(y,x,dydx0=0.0,dydx1=0.0);
    sf2= spline(sf,x,dydx0=0.0,dydx1=0.0);
    sf3= spline(sf2,x,dydx0=0.0,dydx1=0.0);
  } else
    sf= sf2= sf3= y;

  // first index is XYZ/SCH  2nd is dat,deriv,
  a= x(-:1:5,..);

  a(1,..)= y;
  a(2,..)= sf;
  a(3,..)= sf2;
  a(4,..)= sf3;
  a(5,..)= x;

  save, ob, a;
  save, ob, type="digfun_spline";

  return closure(ob,_eval);
}
func _eval (x,deriv=,deriv2=,integ=)
{
  use, a, yav;
  out= x;
  if (deriv==1)
    return spline(a(3,..),a(2,..),a(5,..),x);
  else if (deriv2==1){  // continuous approximation kludge
    return spline(a(4,..),a(3,..),a(5,..),x);
  } else if (integ==1)
    error,"not implemented";
  else
    return yav+spline(a(2,..),a(1,..),a(5,..),x);
}
digfun_spline= closure(digfun_spline,restore(tmp));restore, scratch;

scratch= save(scratch, tmp); tmp= save(_eval);
func digfun_lin (base,y,x,decim=)
/* DOCUMENT m= digfun_lin (base,y,x,decim=)
            m= digfun (base,y,x,decim=,type="digfun_lin")
   discontinuous second derivatives
 */
{
  ob= digfun().function;

  save, ob, [], base(:); //  clobber eval

  d= dimsof(y,x);
  if (is_void(d))
    error,"x & y not conformable";
  n= d(2);

  if (is_void(decim))
    decim= 1;

  nf= n/decim;

  x10= [x(1),x(0)];
  y10= [y(1),y(0)];
  
  x= reform(x(1:decim*nf),[2,decim,nf])(avg,..);
  y= reform(y(1:decim*nf),[2,decim,nf])(avg,..);

  if (decim>1) {
    x= _(x10(1),x,x10(2));
    y= _(y10(1),y,y10(2));
    nf+= 2;
  }

  yav= y(avg);

  save, ob, xmin= min(x);
  save, ob, xmax= max(x);
  save, ob, dx= (ob(xmax)-ob(xmin))/(nf-1);
  save, ob, yav= yav;
  save, ob, decim;

  a= x(-:1:4,..);
  a(2,..)= y-yav;
  a(3,..)= ((a(2,:-1)-a(2,2:))/(a(1,:-1)-a(1,2:)))(pcen);
  a(4,..)= ((a(3,:-1)-a(3,2:))/(a(1,:-1)-a(1,2:)))(pcen);

  save, ob, a;
  save, ob, type="digfun_lin";

  return closure(ob,_eval);
}
func _eval (x,deriv=,deriv2=,integ=)
{
  use, a,yav;
  out= x;
  if (deriv==1)
    return interp(a(3,..),a(1,..),x);
  else if (deriv2==1)
    return interp(a(4,..),a(1,..),x);
  else if (integ==1)
    return yav*x+integ(a(2,..),a(1,..),x);
  else
    return yav+interp(a(2,..),a(1,..),x);
}
digfun_lin= closure(digfun_lin,restore(tmp));restore, scratch;


scratch= save(scratch, tmp); tmp= save(_eval);
func digfun_fft (base,y,x,decim=)
/* DOCUMENT m= digfun_fft (base,y,x,decim=)
            m= digfun (base,y,x,decim=,type="digfun_fft")
   discontinuous second derivatives
 */
{
  ob= digfun().function;

  error,"nothing yet";
  
  save, ob, [], base(:); //  clobber eval

  d= dimsof(y,x);
  if (is_void(d))
    error,"x & y not conformable";
  n= d(2);

  if (is_void(decim))
    decim= 10;

  nf= n/decim;

  // binning
  x= reform(x(1:decim*nf),[2,decim,nf])(avg,..);
  y= reform(y(1:decim*nf),[2,decim,nf])(avg,);

  yav= y(avg);

  save, ob, xmin= min(x);
  save, ob, xmax= max(x);
  save, ob, dx= (ob(xmax)-ob(xmin))/(nf-1);
  save, ob, yav= yav;
  save, ob, decim;

  xf= span(ob(xmin),ob(xmax),nf);
  yf= spline(y,x,xf,dydx0=0.0,dydx1=0.0);
  
  a= xf(-:1:4,..);
  a(2,..)= yf;
  a(3,..)= ((a(2,:-1)-a(2,2:))/(a(1,:-1)-a(1,2:)))(pcen);
  a(4,..)= ((a(3,:-1)-a(3,2:))/(a(1,:-1)-a(1,2:)))(pcen);

  save, ob, a;
  save, ob, type="digfun_fft";

  return closure(ob,_eval);
}
func _eval (x,deriv=,deriv2=,integ=)
{
  use, a,yav;
  out= x;
  if (deriv==1)
    return interp(a(3,..),a(1,..),x);
  else if (deriv2==1)
    return interp(a(4,..),a(1,..),x);
  else if (integ==1)
    return yav*x+integ(a(2,..),a(1,..),x);
  else
    return yav+interp(a(2,..),a(1,..),x);
}
digfun_fft= closure(digfun_fft,restore(tmp));restore, scratch;
#if 0
func test (decim=,type=)
{
  if (is_void(n))
    n= 1000;
  x= span(0,5.,n);
  y= sin(2*pi*x);

  f= digfun(y,x,type=type,decim=decim);

  xx= span(1.,4.,n)+random_n(n)/(3*n);
  window,0;
  fma;
  plg,sin(2*pi*xx)-f(xx),xx;
  window,1;
  fma;
  plg,2*pi*cos(2*pi*xx)-f(xx,deriv=1),xx;
}
#endif
