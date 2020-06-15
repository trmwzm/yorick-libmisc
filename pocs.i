require, "dsp.i";
require,"nfft.i";

func filmsk (x,msk,bandfrac,beta=,niter=)
/* DOCUMENT filmsk (x,msk,bandfrac,beta=,niter=)
   first cut POCS (projection on convex set) extrapolation of
   missing data (assumed regularly sampled)
   USAGE:
   X            original array with missing/bad data,
   MSK          mask == 1 if *valid* data, conformable with X
   BANDFRAC     fractional band to preserve
   BETA         of kaiser window over preserved band
   NITER        sic.
   SEE ALSO:
 */
{
  nx= numberof(x);
  if (nx!=numberof(msk))
    error,"mask and x not conformable.";
  if (is_void(beta))
    beta= 10.0;
  if (is_void(bandfrac))
    bandfrac= 0.9;
  win= windex(kaiser(long(nx*bandfrac),beta),nx);
  w= where(msk);
  xx= (w-1.0)/nx-1/2.;
  op= nfft_new(nx,xx);
  if (is_void(niter))
    niter= 5;
  y= array(0.,nx);
  for (it=1;it<=niter;it++) {
    xf= op(x(w)-y(w),1);
    xf= roll(xf)*win;
    y+= roll(fft(xf).re)/nx;
  }
  return y;
}

func filmsk2 (x,msk,bandfrac,beta=,niter=)
/* DOCUMENT filmsk2 (x,msk,bandfrac,beta=,niter=)
   2-dimensional of nlayers X nx X ny
   first cut POCS (projection on convex set) extrapolation of
   missing data (assumed regularly sampled)
   X            original array with missing/bad data,
   MSK          mask == 1 if *valid* data, conformable with X
   BANDFRAC     fractional band to preserve
   BETA         of kaiser window over preserved band
   NITER        sic.

   SEE ALSO:
 */
{
  dx= dimsof(x);
  if (anyof(dx(-1:)!=dimsof(msk)(-1:)))
      error,"mask and x OR x(-,..) not conformable.";
  nx1= dx(-1);
  nx2= dx(0);
  if (is_void(beta))
    beta= 10.0;
  if (numberof(beta)==1)
    beta= [beta(1),beta(1)];
  if (is_void(bandfrac))
    bandfrac= 0.9;
  if (numberof(bandfrac)==1)
    bandfrac= [bandfrac(1),bandfrac(1)];
  win= windex(kaiser(long(nx1*bandfrac(1)),beta(1)),nx1) * \
       windex(kaiser(long(nx2*bandfrac(2)),beta(2)),nx2)(-:1:nx1,..);
  w= where(msk);
  xx1= ((w-1)%nx1)*1./nx1-1./2;
  xx2= ((w-1)/nx1)*1./nx2-1./2;
  op = nfft_new(nx1, xx1, nx2, xx2);
  if (is_void(niter))
    niter= 5;
  y= array(0.,dx);
  for (it=1;it<=niter;it++) {
    if (dx(1)==2) {
      xf= op(x(w)-y(w),1);
      xf= roll(xf)*win;
      y+= roll(fft(xf).re)/(nx1*nx2);
    } else if (dx(1)==3) {
      for (j=1;j<=dx(2);j++) {
        xf= op(x(j,w)-y(j,w),1);
        xf= roll(xf)*win;
        y(j,..)+= roll(fft(xf).re)/(nx1*nx2);
      }
    } else
      if (it==1)error,"dimsof(x)(1)== 2 or 3 [nlayer,nx,ny])";
  }
  return y;
}

func equispaced (y,x,n,bandfrac,&x10,&it,beta=,niter=,pad=,splin=,tol=)
/* DOCUMENT equispaced (y,x,n,bandfrac,beta=,niter=,pad=,splin=,tol=)

   m= 100;
   n= 2*m;
   p=8;
   scale= [.1,.1];
   offset= 10.;
   x= random(m)*scale(1);
   y= offset+scale(2)+sin(2*pi*p*x/scale(1))
   local x10;
   yy= equispaced(y,x,n,0.9,x10,pad=.001);
   fma;plg,yy,span(x10(1),x10(2),n);plg,y,x,type=0,marker='\3',color="red";
   SEE ALSO:
 */
{
  ny= numberof(y);
  if (numberof(x)!=ny)
    error,"NX!=NY.";
  if (is_void(beta))
    beta= 10.0;
  if (is_void(bandfrac))
    bandfrac= 0.9;
  if (is_void(tol))
    tol= 1e-4;

  yav= avg(y);
  y-= yav;

  xpp= abs(x(ptp));
  x/= xpp;
  dx= 1./ny;

  win= windex(kaiser(long(n*bandfrac),beta),n);

  pad= is_void(pad)? 0: pad;
  x1= min(x)-pad;
  x0= max(x)+pad+dx;
  x10= [x1,x0];

  xx= (x-x1)/(x0-x1)-0.5;
  op= nfft_new(n,xx);

  yy= array(0.,n);
  xx= span(x1,x0,n);
  ya= y(rms);
  if (is_void(niter))
    niter= 100;

  it= 0;
  dy= y;
  toli= dt= 1;
  while (it<niter && dt>tol) {
    if (it>0) {
      dy= splin==1? y-spline(yy,xx,x): y-interp(yy,xx,x);
      dya= dy(rms);
      dt= toli-dya/ya;
      toli= dya/ya;
    }
    xf= op(dy,1);
    xf= roll(xf)*win;
    yy+= roll(fft(xf).re)/n;
    it+= 1;
  }
  if (it==niter)
    write,"WARNING pocs/equispaced max iterations reached: "+pr1([toli,dt]);
  x10*= xpp;
  yy+= yav;

 return yy;
}

func equispaced2 (z,y,x,n,m,bandfrac,&y10,&x10,&err,&it, \
                  beta=,niter=,pad=,tol=,errequi=)
/* DOCUMENT equispaced2 (z,y,x,nx,xy,bandfrac,&y10,&x10,&err,&it, \
                         beta=,niter=,pad=,tol=,errequi=)

   N is First dimension length and that dim corresponds to X
   M is Second dimension length and that dim corresponds to Y

   k= 10000;
   n= 500;
   m= 600;
   p= [1,4];
   x= random(k);
   y= random(k);
   if (0) {
     z= sin(2*pi*p(1)*x)*sin(2*pi*p(2)*y);
     local y10, x10;
     err= 1; // *** must be non-void to trigger computation
     zz= equispaced2(z,y,x,n,m,0.2,y10,x10,res);
   } else {
     a= x+1i*y-0.5*(1+1i);
     z= log(a);
     err= 1; // *** must be non-void to trigger computation
     y= 5*y+123.;
     x= 8*x+89.;
     zz= equispaced2(z,y,x,n,m,0.2,y10,x10,res,pad=1e-2);
     zz= atan(zz.im,zz.re);
   }
   fma;pli,zz;

   SEE ALSO:
 */
{
  d= dimsof(x,y,z);
  un= array(structof(x)(0),d);
  x+= un;
  y+= un;
  z+= un; un= [];
  nn= numberof(y);

  z= z(*);
  y= y(*);
  x= x(*);

  if (is_void(beta))
    beta= 10.0;
  if (numberof(beta)==1)
    beta= [beta(1),beta(1)];
  if (is_void(bandfrac))
    bandfrac= 0.9;
  if (numberof(bandfrac)==1)
    bandfrac= [bandfrac(1),bandfrac(1)];
  if (is_void(tol))
    tol= 5e-4;
  if (is_void(niter))
    niter= 400;

  xav= avg(x);
  yav= avg(y);
  zav= avg(z);
  x-= xav;
  y-= yav;
  z-= zav;

  xpp= abs(x(ptp));
  x/= xpp;
  ypp= abs(y(ptp));
  y/= ypp;

  dx= 1./sqrt(nn);
  dy= 1./sqrt(nn);

  win= windex(kaiser(max(1,long(n*bandfrac(1))),beta(1)),n) *   \
       windex(kaiser(max(1,long(m*bandfrac(2))),beta(2)),m)(-:1:n,..);

  pad= is_void(pad)? [0,0]: (numberof(pad)==1? [pad(1),pad(1)]: pad);

  x1= min(x)-pad(1)/xpp;
  x0= max(x)+pad(1)/xpp+dx;
  x10= [x1,x0];
  y1= min(y)-pad(1)/ypp;
  y0= max(y)+pad(1)/ypp+dy;
  y10= [y1,y0];

  op= nfft_new(n,(x-x1)/(x0-x1)-0.5,m,(y-y1)/(y0-y1)-0.5);

  zz= array(structof(z)(0),n,m);
  xx= span(x1,x0,n)(,-:1:m);
  yy= span(y1,y0,m)(-:1:n,);
  za= is_complex(z)? double(sqrt((z*conj(z))(avg))): z(rms);

  it= 0;
  err= z;
  toli= dt= 1;
  while (it<niter && dt>tol) {
    if (it>0) {
      err= is_complex(z)? z-bicub(x,y,zz,x1,x0,y1,y0): z-interp2(y,x,zz,yy,xx);
      dza= is_complex(z)? abs(err)(avg)/za: err(rms)/za;
      dt= toli-dza;
      if (dt<0)
        error,"increasing RMS.";
      toli= dza;
    }
    xf= op(err,1);
    zz+= is_complex(z)? roll(fft(roll(xf)*win))/(n*m): roll(fft(roll(xf)*win).re)/(n*m);
    it+= 1;
  }
  if (it==niter)
    write,"WARNING pocs/equispaced2 max iterations reached: "+pr1([toli,dt]);

  err= is_complex(z)? z-bicub(x,y,zz,x1,x0,y1,y0): z-interp2(y,x,zz,yy,xx);
  if (errequi) {
    xf= op(err,1);
    err= is_complex(z)? roll(fft(roll(xf)*win))/(n*m): roll(fft(roll(xf)).re)/(n*m);
  } else {
    err= reform(err,d);
  }

  x10*= xpp;
  y10*= ypp;
  x10+= xav;
  y10+= yav;
  zz+= zav;

  return zz;
}
