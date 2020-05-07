require, "yut.i";
require, "lmfit.i";
require, "roots.i";
require, "bessel.i";
require, "convcorrel.i";
require, "fpanels.i";
require, "convol.i";
require, "nfft.i";

/*----------------------------------------------------------------------*/

func windex (w,n)
/* DOCUMENT w= windex (w,n); // numberof(w)==n
   crop (rarely) OR pad (commonly) windowing arrays, n is OUTPUT length.
   SEE ALSO:
 */
{
  if (n==0)
    return w;
  nn= numberof(w);
  n2= n<=nn? (n-1)/2: (nn-1)/2;
  y= array(structof(w),n);
  y(:n2+1)= w(:n2+1);
  y(-n2+1:)= w(-n2+1:);
  if (n<nn && n%2==0)
    y(n/2+1)= w(n2+2);
  return y;
}

/*----------------------------------------------------------------------*/

func hamming(m, alpha){
/* DOCUMENT  hamming(m, alpha)
 if (is_void(alpha))alpha=0.54
 returns alpha + (1-alpha) * cos (((2 * pi) * indgen(0:m-1)) / m )
 0.5 < ALPHA < 1
 Returns the filter coefficients of a Hamming window of length m.
 For a definition of the Hamming window, see e.g. A. V. Oppenheim &
 R. W. Schafer, "Discrete-Time Signal Processing".
 TEST plg,10*log10(roll(abs(fft(grow(roll(hamming(m)),array(double,5*m))))))
      plg,20*log10(roll(abs(interpol8(fft(hamming(100)),4000))))
 */

  if (!(is_scalar(m) && (structof(m)==int || structof(m)==long ) && m > 0)){
    error,"hamming:  m has to be an integer > 0";
  }

  if (is_void(alpha))
    alpha=0.54;

  if (m == 1){
    return 1;
  }else{
    return alpha + (1-alpha) * cos (((2 * pi) * indgen(0:m-1)) / m );
  }

}

/*----------------------------------------------------------------------*/

func hanning(m){
/* DOCUMENT  hamming(m)
 Returns the filter coefficients of a hanning window of length m.
 returns 0.5 + 0.5 * cos (((2 * pi) * indgen(0:m-1)) / m )
 equivalent: hamming(m,0.5)
 For a definition of the hanning window, see e.g. A. V. Oppenheim &
 R. W. Schafer, "Discrete-Time Signal Processing".
 */

  if (!(is_scalar(m) && (structof(m)==int || structof(m)==long ) && m > 0)){
    error,"hanning:  m has to be an integer > 0";
  }

  if (m == 1){
    return 1;
  }else{
    return 0.5 + 0.5 * cos (((2 * pi) * indgen(0:m-1)) / m );
  }

}

/*----------------------------------------------------------------------*/

func bartlett(m){
/* DOCUMENT  bartlett(m)
 Returns the filter coefficients of a Bartlett (triangular) win of length m.
 returns 0.5 + 0.5 * cos (((2 * pi) * indgen(0:m-1)) / m )
 For a definition of the hanning window, see e.g. A. V. Oppenheim &
 R. W. Schafer, "Discrete-Time Signal Processing".
 */

  if (!(is_scalar(m) && (structof(m)==int || structof(m)==long ) && m > 0)){
    error,"bartlett:  m has to be an integer > 0";
  }

  if (m == 1){
    return 1;
  }else{
    n=(m-1)/2
    return grow(2-indgen(n+1 : m)*(2./m),indgen(m-2*n-1:m-n-2)*(2./m));
  }

}

/*----------------------------------------------------------------------*/

func blackman(m){
/* DOCUMENT blackman(m)

 Returns the filter coefficients of a Blackman window of length m.

 For a definition of the  Blackman window, see e.g. A. V. Oppenheim &
 R. W. Schafer, "Discrete-Time Signal Processing".
 */

  if (!(is_scalar(m) && (structof(m)==int || structof(m)==long ) && m > 0)){
    error,"blackman:  m has to be an integer > 0";
  }

  if (m == 1){
    return 1;
  }else{
    b= 0.4266 + 0.4966  * cos (((2 * pi) * indgen(0:m-1)) / m ) +
        0.07685 * cos (((2 * pi) * indgen(0:m-1)) * 2 / m );
    return b - b(m/2+1);
  }
}

/*----------------------------------------------------------------------*/

func kaiser (m, beta)
/* DOCUMENT  kaiser(m, beta)

 Returns the filter coefficients of the n-point Kaiser-Bessel window with
 parameter beta.

 For the definition of the Kaiser-Bessel  window, see A. V. Oppenheim &
 R. W. Schafer, "Discrete-Time Signal Processing".

 */
{
  if (!(is_scalar(m) && (structof(m)==int || structof(m)==long ) && m > 0)){
    error,"kaiser:  m has to be an integer > 0";
  }

  if (m == 1){
    return 1;
  } else {
    n= (m - 1)/2;
    w= bessi(0,pi * beta * sqrt(1-((2.*indgen(0:m-n-1))/m)^2)) / bessi(0,beta * pi);
    return grow(w, w(n+1:2:-1));
  }

}

/*-------------------------------------------------------------------------------------*/

func taylor (noel,slldb,nbar,&z0,half=)
/* DOCUMENT: taylor(noel,slldb,nbar)
 computes taylor distribution for given sidelobe level and nbar
 helf= 1   will move the sampling points by +0.5
 amp=array of amplitudes computed by subroutine
 noel=number of array elements
 sll=sidelobe level in db
 nbar=nbar in taylor distribution: number of near-in sidelobes or,
 Number of zeroes used in approximating the linear Taylor weighting function
 fma;x=roll(abs(interpol8(fft(taylor(32,40,6,z0)),10000)));xl=20*log10(x/x(max)+1e-40);plg,xl(5001:),span(0,16,5000)
 xx=roll(abs(interpol8(fft(array(1.0,32)),10000)));xxl=20*log10(xx/xx(max)+1e-40);plg,xxl(5001:),span(0,16,5000),color="red"
 // compare to Mailloux pp.131

*/
{
  nbar= long(nbar);
  noel= long(noel);
  if (nbar<=1)error,"nbar should be >1";
  if (nbar>noel)nbar=noel;

  sll= 10^(abs(slldb)/20.0);
  as= (log(sll+sqrt(sll^2-1.0))/pi)^2;

  s= nbar^2/(as+nbar^2-nbar+.25);
  z0= sqrt(s*(as+.25));

  nl1= nbar-1;
  f= array(0.5,nl1);

  for (ii=1;ii<=nl1;ii++){
   f(ii)= f(ii)*(-1)^(ii+1);
   a1= ii^2/s;
   jj= indgen(1:nl1);

   x= 1-a1/(as+jj^2-jj+.25);
   f(ii)= f(ii)*product(x);

   if (ii<nl1){
    jj=double(indgen(ii+1:nl1))^2;
    x= 1-ii^2/jj;
    f(ii)= f(ii)/product(x);
   }
   if (ii>1){
    jj=double(indgen(1:ii-1))^2;
    x= 1-ii^2/jj;
    f(ii)= f(ii)/product(x);
   }
  }

  w= array(double,noel);
  w (1)= 1.0;
  w(2:nbar)= f;
  w(-(nbar-2):0)= f(::-1);

  if (half){
    n2= (noel-1)/2;
    w*=exp(-pi*1i/noel*roll(span(-n2, noel-n2-1, noel),-n2)*half);
  }

  w= fft(w).re;

  return w;
}

/*-------------------------------------------------------------------------------------*/

func bayliss(noel,slldb,nbar){
/* DOCUMENT: bayliss(noel,slldb,nbar)
 computes bayliss distribution for difference beam
 of given sidelobe level and nbar

 amp=array of amplitudes computed by subroutine
 noel=number of array elements
 sll=sidelobe level in db
 nbar=nbar in taylor distribution: number of near-in sidelobes or,
 Number of zeroes used in approximating the linear Bayliss weighting function
 x=roll(abs(interpol8(fft(bayliss(18,40,8)),1000)));
 x/=x(max);x=20*log10(x+1e-40);plg,x(501:),span(0,1,500)) // compare to Mailloux R.
 p=poly(span(0,-60,200),baycoff(1,)(-,),baycoff(2,)(-,),baycoff(3,)(-,),baycoff(4,)(-,),baycoff(5,)(-,));
 interp(p,span(0,60,200),15,1) // compare to Mailloux R. pp.139
*/
 nbar= long(nbar);
 noel= long(noel);
 if (nbar<=1)error,"nbar should be >1";

 /*  5 X 6  5==#of polynomial coef,  6= A & Omega_1_to_4 & p0 */
 baycoff = [[ 0.30387530, -0.05042922, -0.00027989, -0.00000343, -0.00000002 ],\
            [ 0.98583020, -0.03338850,  0.00014064,  0.00000190,  0.00000001 ],\
            [ 2.00337487, -0.01141548,  0.00041590,  0.00000373,  0.00000001 ],\
            [ 3.00636321, -0.00683394,  0.00029281,  0.00000161,  0.00000000 ],\
            [ 4.00518423, -0.00501795,  0.00021735,  0.00000088,  0.00000000 ],\
            [ 0.47972120, -0.01456692, -0.00018739, -0.00000218, -0.00000001 ]]; // Mailloux (a little bit better at high slldb)
 coff= poly(-abs(slldb),baycoff(1,),baycoff(2,),baycoff(3,),baycoff(4,),baycoff(5,));

//  baycoff= [[0.3198967,   0.4729149e-01,-0.4340961e-04,-0.5196632e-05, 0.1332618e-06, -0.1066079e-08], \
//             [0.9875386,   0.3312969e-01, 0.1546012e-03,-0.2224568e-05, 0.1252845e-07,  0.5972615e-11], \
//             [2.009977,    0.9967974e-02, 0.5392084e-03,-0.8825718e-05, 0.1119661e-06, -0.7902019e-09], \
//             [3.02426,     0.3082081e-02, 0.5954844e-03,-0.1337191e-04, 0.2207372e-06, -0.1605496e-08], \
//             [3.996253,    0.7023814e-02, 0.4476344e-04, 0.6227692e-05,-0.1405058e-06,  0.1070271e-08], \
//             [0.47972120,  0.01456692,   -0.00018739,    0.00000218,   -0.00000001,     0.0]]; // AFRL-CAESOFT
//  coff= poly(abs(slldb),baycoff(1,),baycoff(2,),baycoff(3,),baycoff(4,),baycoff(5,),baycoff(6,)); // NOTE SIGN of arg!!

 a= coff(1);
 p0= coff(6); // location of difference peak
 sig= (nbar+0.5)/sqrt(a^2+nbar^2);
 z= array(double,max([nbar,4]));
 z0= 0;
 z(1:4)= (sig*coff(2:5))^2
 for (ns=5;ns<=nbar;ns++)z(ns)=sig^2*(a^2+ns^2);
 m2= (indgen(nbar-1)+0.5)^2;

 f= array(1.0,nbar);  //f(i) <-> B(i-1)  ie. m= ii - 1 or ii= m+1

 for (ii=1;ii<=nbar;ii++){
   mph2= (ii - 0.5)^2;
   f(ii)= f(ii)*(-1)^(ii-1)*mph2;
   x= 1 - mph2/z(1:nbar-1);
   f(ii)= f(ii)*product(x);

   x= 1 - mph2/(indgen(0:nbar-1)+0.5)^2;
   w= where(x!=0.0);
   if (numberof(w))x= x(w);
   f(ii)= f(ii)/product(x);
 }

 w= array(double,noel);

 for (ii=1;ii<=noel;ii++){
   r= (ii-(noel+1.0)/2)/noel;
   as= (sin(span(0.5,nbar-0.5,nbar)*2*pi*r)*f)(sum);
   w(ii)= as;
 }

return roll(w,-noel/2);}

/*-------------------------------------------------------------------------------------*/

func chebpoly(n){
/*DOCUMENT p= chebpoly(n)
 chebpoly coefficients of Chebyshev polynomial. [p(1)+p(2)*x+...+p(i)*x^(i-1)]
 chebpoly(n) returns the N+1 polynomial coefficients (ordered in
 ascending powers) of the Nth Chebyshev polynomial.

 if nargin==0, help chebpoly, return, end
 n= abs(n);
 */

 t0= 1;
 t1= [0, 1];
 if (n<=0){
   p= t0;
 }else if (n<=1){
   p= t1;
 }else{
   for (k=2;k<=n;k++){
     p= fftconvol([0,2], t1) - grow(t0,[0,0]);  //sure should not need fft for this....
     t0= t1;
     t1= p;
   }
 }
 return p;
}

/*----------------------------------------------------------------------*/

func chebpts(d1, d2, n){
/* DOCUMENT  y= chebpts(d1, d2, n)
 chebpts chebyshev-spaced nodes for polynomial interpolation.
 chebpts(x1, x2, n) returns a vector of n nodes between x1 and x2.
 check:  poly2(chebpts(-1,1,n),chebpoly(n))
 */

 if (is_void(d1)||is_void(d2)||is_void(n))error,"chebpts(d1, d2, n), no args void"

 if (n==0 && (d1<d2? (0>d1 && 0<d2) : (0<d1 && 0>d2)))return 0.0;

 theta= (2*indgen((n-1):0:-1)+1)*pi/(2*n);
 y= 0.5*(d1 + d2 + (d2-d1)*cos(theta));
 return y;
}

// /*----------------------------------------------------------------------*/
//
// func chebwin(n, at){
//   /*The window is described in frequency domain by the expression:
//
//           Cheb(n-1, beta * cos(pi * k/n))
//    W(k)= -------------------------------,   beta= cosh(1/(n-1) * acosh(10^(at/20))
//                  Cheb(n-1, beta)
//
//   and Cheb(m,x) denoting the m-th order Chebyshev polynomial calculated at the point x
//   */
//
//  if (is_void(n)|is_void(at)|!(n%2))error,"chebwin (n, at), n odd";
//  if (n == 1)return [1.0];
//
//  gamma= 10.0^(-at/20.0);     // beta calculation
//  beta= cosh(1.0/(n-1) * acosh(1/gamma));
//
//  k= span(0.0,n-1.0,n);
//  x= beta*cos(pi*k/n);
//
//  //Returns the value of the n-1th-order Chebyshev polynomial calculated at
//  //the point x. The Chebyshev polynomials are defined by the equations:
//  //            / cos(n acos(x)),    |x| <= 1
//  //   Tn(x)= |
//  //           \ cosh(n acosh(x)),  |x| > 1
//
//  p= array(double,dimsof(x));  // Chebyshev window (freq. domain)
//  w= abs(x)<=1;
//  we= where(w);
//  if (numberof(we))p(we)= cos((n-1)*acos(x(we)));
//  we= where(!w);
//  if (numberof(we))p(we)= cosh((n-1)*acosh(x(we)));
//
//  w= fft(p).re;  // inverse Fourier transform
//  m= (n+1)/2;
//  w= w(1:m)/w(1);
//  w= _(w,w(m:2:-1));
//  return p;
// }
//
/*----------------------------------------------------------------------*/

func chebyshev(n, &cwp, slr=, fwhm=)
/* DOCUMENT  "dolph-chebychev" filter
 symmetric Chebyshev window w= chebyshev(n, &cwp, slr=, fwhm=)
 fwhm == main lobe width (0<slr<.5) & fwhm UNSPECIFIED
 slr == side lobe height (slr>0<.001) & slr UNSPECIFIED
 cwp == the unspecified Chebyshev window parameter on output
 lifted (and fixed) from scilab... pretty ugly, but works
 */
{
  cwp=[];

  no2= (n-1)/2;
  xt= span(-no2,no2,2*no2+1);
  un= array(1,n);

  if (is_void(slr)&&!is_void(fwhm)){
     uk= "slr";
  }else if (is_void(fwhm)&&!is_void(slr)){
     uk= "fwhm";
  }else{
    error,"Parameter under- or over- specified";
  }

  if (uk=="slr"){
     fwhmn= 1./(fwhm*double(n));
     arg2= 1/cos(pi*fwhmn);
     coshin2= log(arg2+sqrt(arg2*arg2-1));
     slr= 2/(exp((n-1)*coshin2)+exp(-(n-1)*coshin2));
     cwp= slr;
  }else if (uk=="fwhm"){
     arg1= (1+slr)/slr;
     coshin1= log(arg1+sqrt(arg1*arg1-1));
     fwhmn= .5*(exp(coshin1/(n-1))+exp(-coshin1/(n-1)));
     fwhmn= 1/fwhmn;
     fwhmn= ((log(fwhmn+1i*sqrt(1-fwhmn*fwhmn)))/pi).im;
     cwp= 1./(double(n)*fwhmn);
  }

  np1= (n+1)/2;
  ieo= 2*np1-n;
  xn= double(n-1);
  fnf= double(n);
  x0= (3-cos(2*pi*fwhmn))/(1+cos(2*pi*fwhmn));
  alpha= (x0+1)/2;
  beta= (x0-1)/2;
  c2= xn/2;

  //Obtain the frequency values of the Chebyshev window

  f= indgen(0:n-1)/fnf;
  xarg= alpha*cos(2*pi*f)+beta*un;
  zarg= complex(un-xarg*xarg);
  pm1= slr*cos(c2*(log(xarg+1i*sqrt(zarg))).im);
  arg= c2*log(xarg+sqrt(-zarg));
  pp1= slr*0.5*(exp(arg)+exp(-arg)).re;
  dx= array(long,n);
  ww=where(xarg<=1.0);
  if (numberof(ww)>=1){
    dx(ww)= 1;
  }

  pre= dx*pm1+(un-dx)*pp1;
  pim= array(double,n);

  if (ieo!=1){ //even
     pre= pre*cos(pi*f);
     pim= -pre*sin(pi*f);
     antisym= grow(array(1,(n/2)+1),array(-1,n-(n/2)-1));
     pre= pre*antisym;
     pim= pim*antisym;
  }

  twn= 2*pi/fnf;
  xj= indgen(0:n-1);
  w= array(double,np1+1);
  win= array(double,n);
  for (i=1;i<=np1+1;i++){
     w(i)= sum(pre*cos(twn*(i-1)*xj)+pim*sin(twn*(i-1)*xj));
  }

  c1= w(1);
  w= w/c1;
  if (ieo == 1){  // odd
     win(np1+1:0)= w(n-np1+1:2:-1);
     win(1:np1)= w(1:np1);
  }else{
     win(np1+2:0)= w(n-np1:2:-1);
     win(1:np1+1)= w(1:np1+1);
     //win(1:np1)= w(1:np1);  //highest freq canceled ?? dunno !!
  }

   return win;
}

/*-------------------------------------------------------------------------------------*/

func sinc (x)
/* DOCUMENT sinc(x)
  !! sinc(1.895494) == 0.5
  !! sinc(2.852342) == 0.1
  !! sinc(1.39156)^2 == 0.5
  !! sinc(pi*0.886/2)^2 == 0.5
 */
{
  epsil= 1e-10;
  if (is_scalar(x)) {
    if (abs(x)<epsil ) {
      return 1.0-x^2/6.;
    } else
      return sin(x)/x;
  } else {
    q= abs(x)<epsil;
    if (noneof(q)) {
      return sin(x)/x;
    } else {
      y= x;
      w= where(q);
      y(w)=1.0-(y(w)^2.)/6.;
      w= where(!q);
      y(w)= sin(y(w))/y(w);
      return y;
    }
  }
}

/*-------------------------------------------------------------------------------------*/

func dsincdx (x)
/* DOCUMENT dincdx(x)
   cox(x)/x-sin(x)/x^2;
 */
{
  epsil= 1e-10;
  if (is_scalar(x)) {
    if (abs(x)<epsil ) {
      return -2/6.0*x;
    } else
      return cos(x)/x-sin(x)/x^2;
  } else {
    q= abs(x)<epsil;
    if (noneof(q)) {
      return cos(x)/x-sin(x)/x^2;
    } else {
      y= x;
      w= where(q);
      y(w)= -2/6.0*y(w);
      w= where(!q);
      y(w)= cos(y(w))/y(w)-sin(y(w))/y(w)^2;
      return y;
    }
  }
}

/*-------------------------------------------------------------------------------------*/

func dirichlet_kernel(x,n,rad=){
/* DOCUMENT dirichlet_kernel(x,n,rad=)
   if (rad) : SUM_n=-N_to_N exp{i*n*x}
   else    : SUM_n=-N_to_N exp{i*2*pi*n*x}

   TEST :
   n=301;
   m=20
   fma;plg,dirichlet_kernel(span(-((n-1)/2)*(1./n),(n-1-(n-1)/2)*(1./n),n),m);
   x=array(double,n);x(1:2*m+1)=1;x=roll(x,-m);plg,roll(fft(x).re,-(n+2)/2),color="red"
 */
 epsil= 1e-10;
 if (!rad)x*=2*pi;
 x= cycle(x,-pi,pi);
 if (is_scalar(x)){
   if (abs(x) < epsil ){return 2*n+1-n*(n-1.0)*x^2;
   }else return sin((n+0.5)*x)/sin(0.5*x);
 }else{
   q= abs(x)<epsil;
   if (noneof(q)){
     return sin((n+0.5)*x)/sin(0.5*x);
   }else{
     y= x;
     w= where(q);
     y(w)=2*n+1-n*(n-1.0)*(y(w)^2.);
     w=where(!q);
     y(w)=sin((n+0.5)*y(w))/sin(0.5*y(w));
     return y;
   }
 }
}

/*-------------------------------------------------------------------------------------*/

func morlet (f, t, width)
/* DOCUMENT y = morlet_m(f,t,width)

   SEE ALSO:
 */
{
  sf= f/width;
  st= 1/(2*pi*sf);
  // a= 1/sqrt(2*pi*st^2);
  a= 1/sqrt(st*sqrt(pi));
  y= a*exp(-t^2/(2*st^2))*exp(1i*2*pi*f*t);
  return y
}

/*-------------------------------------------------------------------------------------*/

func fft_ctor (z, len)
/* DOCUMENT a= fft_ctor(z,len)
   map complex valued of F-transformed real array Z=fft(r) into a two double
   arrays of non duplicate real and imaginary parts.
   Z: complex input array FFT(real array) of dimensions (dimsof) [ND,N(1),...,N(ND)]
   [LEN]: optional long array [L(1),...,L(ND)] <= [ND,N(1),...,N(ND)].
          If the Ith dimension is the length of that dimension when unpacked
          back to complex.  L(I) is effectively *odd*, but if L(I) is given as even
          then 2*((L(I)-1)/2)+1 is used.
          If L(I)==0, then that dimension is not processed.
          Assuming L(I) odd, then the real and imaginary part arrays are of sun-dimension
          M(I)= (L(I)+1)/2.

   A: double array of dimensions DA= _(dimsof(z)(1)+1,merge2((LEN+1)/2,dimsof(z)(2:),len>0),2);
      A(..,1) has real parts
      A(..,2) has imaginary parts

   n= 600;
   p= 50.0;
   m= 100;
   f= gaussm(random_n(n),n/p);
   F= fft(f);

   // pack
   nfft= n/15;
   m= 2*((nfft-1)/2)+1;
   n1= (m+1)/2;
   cre= indgen(1:n1);
   cim= cre(0)+indgen(1:n1-1);
   Fr= array(double,m);
   Fr(cre)= F(:n1).re;
   Fr(cim)= F(2:n1).im;

   // unpack
   Fz= Fr(cre)+1i*_(0.0,Fr(cim));
   Fz= _(Fz,array(complex,n-n1-n1+1),conj(Fz(n1:2:-1)));

   ff= double(fft(Fz,-1)/n);
   fma;plg,f;plg,ff,color="red",type=2;
   SEE ALSO:
 */
{
  dz= dimsof(z);
  nd= dz(1);
  dz= dz(2:);

  len= is_void(len)? dz: len+array(0,nd);
  if (anyof(len>dz|len<0))
    error,"requested numberof larger than available";

  m= len>0;
  iz= where(m);
  ndf= numberof(iz);
  len= merge2(2*((len-1)/2)+1,len,m);

  lm= __fft_rtoc_ndx(dz,len);

  n1= merge2((len+1)/2,dz,m);
  ddz= _(nd,dz);

  if (ndf==0)
    error,"no dimension transformed?";
  if (ndf==1) {
    dda= _(nd,n1);
    ar= ai= array(0.0,dda);

    // null freq.
    ar(indexarr(dda,lm(,1,1),lm(,2,1)))= z(indexarr(ddz,lm(,1,1),lm(,2,1))).re;

    // + freqs.
    zz= z(indexarr(ddz,lm(,1,2),lm(,2,2)));
    ar(indexarr(dda,lm(,1,2),lm(,2,2)))= zz.re;
    ai(indexarr(dda,lm(,1,2),lm(,2,2)))= zz.im;
  } else if (ndf==2) {
    n2= n1;
    n2(iz(2))+= n1(iz(2))-1;
    dda= _(nd,n2);
    ar= ai= array(0.0,dda);

    // null frequency
    zz= z(indexarr(ddz,lm(,1,1),lm(,2,1)));
    ar(indexarr(dda,lm(,1,1),lm(,2,1)))= zz.re;

    // axis #1
    zz= z(indexarr(ddz,lm(,1,2),lm(,2,2)));
    ar(indexarr(dda,lm(,1,2),lm(,2,2)))= zz.re;
    ai(indexarr(dda,lm(,1,2),lm(,2,2)))= zz.im;

    // axis #2
    zz= z(indexarr(ddz,lm(,1,4),lm(,2,4)));
    ar(indexarr(dda,lm(,1,4),lm(,2,4)))= zz.re;
    ai(indexarr(dda,lm(,1,4),lm(,2,4)))= zz.im;

    // (+,+) frequencies
    zz= z(indexarr(ddz,lm(,1,5),lm(,2,5)));
    ar(indexarr(dda,lm(,1,5),lm(,2,5)))= zz.re;
    ai(indexarr(dda,lm(,1,5),lm(,2,5)))= zz.im;

    // (+,-) frequencies
    zz= z(indexarr(ddz,lm(,1,8),lm(,2,8)));
    ar(indexarr(dda,lm(,1,8),lm(,2,8)))= zz.re;
    ai(indexarr(dda,lm(,1,8),lm(,2,8)))= zz.im;
  } else {
    error,"missing code.";
  }
  return [ar,ai];
}

func fft_rtoc (a, dz, len)
/* DOCUMENT z= fft_rtoc(a,dz,len)
   inverse map of FFT_CTOR
   map stored double valued from FFT of real array Z=fft(r) into a complex array
   ready for inverse FFT.

   Z: complex FFT input array of dimensions (dimsof) [ND,N(1),...,N(ND)]
   [LEN]: optional long array [L(1),...,L(ND)] <= [ND,N(1),...,N(ND)]
          if Ith dimension L(I)==0 then that dimension is not packed

   SEE ALSO:
 */
{
  da= dimsof(a);
  dda= da;
  nd= da(1)-1;
  ddz= _(nd,dz);
  if (numberof(dz)!=nd)
    error,"DZ not comformable.";
  da= da(2:);
  m= len>0;
  iz=  where(m);
  ndf= numberof(iz);
  len= merge2(2*((len-1)/2)+1,len,m);

  lm= __fft_rtoc_ndx(dz,len);

  n1= merge2((len+1)/2,dz,m);
  dda= dimsof(a(..,1));
  ddz= _(nd,dz);

  local ar,ai;
  eq_nocopy,ar,a(..,1);
  eq_nocopy,ai,a(..,2);

  if (ndf==0)
    error,"no dimension transformed?";
  z= array(complex,ddz);
  if (ndf==1) {
    // null freq.
    zz= ar(indexarr(dda,lm(,1,1),lm(,2,1)));
    z(indexarr(ddz,lm(,1,1),lm(,2,1)))= zz;

    // axis #1
    zz= ar(indexarr(dda,lm(,1,2),lm(,2,2)))+1i*ai(indexarr(dda,lm(,1,2),lm(,2,2)));
    z(indexarr(ddz,lm(,1,2),lm(,2,2)))= zz;
    z(indexarr(ddz,merge2(lm(,2,3),lm(,1,3),m),merge2(lm(,1,3),lm(,2,3),m),merge2(-1,1,m)))= conj(zz);
  } else if (ndf==2) {
    // null freq.
    zz= ar(indexarr(dda,lm(,1,1),lm(,2,1)));
    z(indexarr(ddz,lm(,1,1),lm(,2,1)))= zz;

    // axis #1
    zz= ar(indexarr(dda,lm(,1,2),lm(,2,2)))+1i*ai(indexarr(dda,lm(,1,2),lm(,2,2)));
    z(indexarr(ddz,lm(,1,2),lm(,2,2)))= zz;
    z(indexarr(ddz,merge2(lm(,2,3),lm(,1,3),m),merge2(lm(,1,3),lm(,2,3),m),merge2(-1,1,m)))= conj(zz);

    // axis #2
    zz= ar(indexarr(dda,lm(,1,4),lm(,2,4)))+1i*ai(indexarr(dda,lm(,1,4),lm(,2,4)));
    z(indexarr(ddz,lm(,1,4),lm(,2,4)))= zz;
    z(indexarr(ddz,merge2(lm(,2,7),lm(,1,7),m),merge2(lm(,1,7),lm(,2,7),m),merge2(-1,1,m)))= conj(zz);

    // (+,+) frequencies
    zz= ar(indexarr(dda,lm(,1,5),lm(,2,5)))+1i*ai(indexarr(dda,lm(,1,5),lm(,2,5)));
    z(indexarr(ddz,lm(,1,5),lm(,2,5)))= zz;
    z(indexarr(ddz,merge2(lm(,2,9),lm(,1,9),m),merge2(lm(,1,9),lm(,2,9),m),merge2(-1,1,m)))= conj(zz);

    // (+,-) frequencies
    zz= ar(indexarr(dda,lm(,1,8),lm(,2,8)))+1i*ai(indexarr(dda,lm(,1,8),lm(,2,8)));
    z(indexarr(ddz,lm(,1,8),lm(,2,8)))= zz;
    z(indexarr(ddz,merge2(lm(,2,6),lm(,1,6),m),merge2(lm(,1,6),lm(,2,6),m),merge2(-1,1,m)))= conj(zz);
  } else {
    error,"missing code.";
  }
  return z;
}

func __fft_rtoc_ndx (dz,len)
{
  nd= numberof(dz);
  len= is_void(len)? dz: len+array(0,nd);
  if (anyof(len>dz|len<0))
    error,"requested numberof larger than available";

  m= len>0;
  iz= where(m);
  len= merge2(2*((len-1)/2)+1,len,m);
  ndf= numberof(iz);
  n1= merge2((len+1)/2,dz,m);

  lm= array(0,_(3,nd,2,3^ndf));
  if (ndf==0)
    error,"no dimension transformed?";
  if (ndf==1) {
    lm(..,1)= [array(1,nd),merge2(1,0,m)];
    lm(..,2)= [merge2(2,1,m),merge2(n1,0,m)];
    lm(..,3)= [merge2(-n1+2,1,m),0];
  } else if (ndf==2) {
    m1= array(0,nd);m1(iz(1))= 1;
    n11= dz;
    n11(iz(1))= (len(iz(1))+1)/2;
    m2= array(0,nd);m2(iz(2))= 1;
    n12= dz;
    n12(iz(2))= (len(iz(2))+1)/2;
    //                 region numbers and index 1/2 start/stop
    //    |7|8|    |9|
    //    |
    //    |4|5|    |6|
    //    |1|2|____|3|_
    //
    lm(..,1)= [array(1,nd),merge2(1,0,m)];
    lm(..,2)= [merge2(2,1,m1),merge2(merge2(n11,1,m1),0,m)];
    lm(..,3)= [merge2(-n11+2,1,m1),merge2(merge2(0,1,m1),0,m)];
    lm(..,4)= [merge2(2,1,m2),merge2(merge2(n12,1,m2),0,m)];
    lm(..,5)= [merge2(2,1,m),merge2(n1,0,m)];
    lm(..,6)= [merge2(merge2(-n11+2,2,m1),1,m),merge2(n12,0,m2)];
    lm(..,7)= [merge2(-n12+2,1,m2),merge2(merge2(0,1,m2),0,m)];
    lm(..,8)= [merge2(merge2(-n12+2,2,m2),1,m),merge2(n11,0,m1)];
    lm(..,9)= [merge2(-n1+2,1,m),array(0,nd)];
  } else {
    error,"missing code.";
  }
  return lm;
}

/*-------------------------------------------------------------------------------------*/

func factorial(x){
/* DOCUMENT factorial(x)
 [floor(0.5+exp(lngamma(x+1)))]
 */
 return floor(0.5+exp(lngamma(x+1.)));
 }

/*-------------------------------------------------------------------------------------*/

func zatan (z, deg=)
/* DOCUMENT zatan(z, deg=)
  if (!is_void(deg)&&deg==1){
    return (180./pi)*atan(z.im, z.re);
  }else{
    return atan(z.im, z.re);
  }
 */
{
  if (typeof(z)!="complex")
    error,"zatan wants complex arg";

  m= z.re==0 & z.im==0;

  if (anyof(m))
    out= atan(z.im, z.re+m);
  else
    out= atan(z.im, z.re);

  if (deg==1)
    out*= 180./pi;

  return out;
}

/*-------------------------------------------------------------------------------------*/

func argexpi(x, deg=, wrap=){
/* DOCUMENT argexpi(x, deg=)
 returns the phase [-pi, pi] of exp(1i*x)
 */
  if (!is_void(deg)&&deg==1){
    pi1=180.;
    pi2=360.;
  }else if (!is_void(wrap)){
    pi1=wrap/2;
    pi2=wrap;
  }else{
    pi1=pi;
    pi2=2*pi;
  }
  x=x%pi2;
  return (abs(x)<=pi1)*x + (x+pi2)*(x<-pi1) + (x-pi2)*(x>pi1);
}

/*-------------------------- temp didactic junk from median in std.i -----*/

func average(x, which)
/* DOCUMENT average(x)
         or average(x, which)
     returns the average of the array X.  The search for the average takes
     place along the dimension of X specified by WHICH.  WHICH defaults
     to 1, meaning the first index of X.  The average function returns an
     array with one fewer dimension than its argument X (the WHICH
     dimension of X is missing in the result), in exact analogy with
     rank reducing index range functions.  If dimsof(X)(WHICH) is
     odd, the result will have the same data type as X; if even, the
     result will be a float or a double, since the average is defined
     as the arithmetic mean between the two central values in that
     case.
   SEE ALSO: sort
 */
{
  if (is_void(which)) which= 1;
  which= which;
  nw= numberof(which);
  dims= dimsof(x);
  w= where(which<1);
  if (numberof(w)) which(w)= dims(1)-which(w);
  xm= x;
  /**/ local xx;
  reshape, xx;
  for (wi=1;wi<=nw;wi++) {
    whi= which(wi);
    n= dims(1+whi);
    stride= 1;
    for (i=1 ; i<whi ; i++) stride*= dims(1+i);
    ldims= dims(1)-whi+1;  //hownany dims [whi,last]
    reshape, xx, &xm, structof(x), stride, grow(ldims, dims(1+whi:));
    //xx= reform(xm, grow(ldims+1, stride, dims(1+which:)));
    //yorick_stats()
    xm= xx(,avg,..);         // dims left of whi bundeled into first
    if (whi<dims(1)) dims(1+whi:-1)= dims(2+whi:0);
    --dims(1);
    dims= dims(:-1);
    xm= reform(xm,dims)
    which-= which>whi;
  }
  reshape, xx;
  return xm;
}
/*--------------------------------------------------------------------*/

func  pksamp (cin,rndx,&rpk,&cpk,&sent,win=,osf=,carrier=,dbgfig=,fcplx=)
/* DOCUMENT pksamp (cin,rndx,&rpk,&cpk,&sent,win=,osf=,dbgfig=)
   does peak analysis across a decimal index in the
   first dimension of cin.  This is "looped" on all
   subsequent indices.
   c=array(complex,[2,100,10]);
   c(50,)= 1+1i*1.0;
   rrq= array(49.0,10);
   crq= pksamp(c,rrq,rpk,cpk,s);
*/
{
  if (fcplx)
    d= dimsof(cin(1,..));
  else
    d= dimsof(cin);

  if (d(0)!=dimsof(rndx)(0))
    error,"Data and requested indices not conf.";

  n= d(2);
  if (is_void(win)) win=9;
  if (is_void(osf)) osf= 64;
  if (2 > win) error,"Warning: win == "+pr1(win);
  if (2 > osf) error,"Warning: osf == "+pr1(osf);

  if(d(1)>1)
    dd= _(d(1)-1,d(3:));
  else
    dd= [0];
  rndx+= array(0.0,dd);

  wval= 1;
  if (d(1)>1) wval= where(win/2 < rndx & rndx < (n - win + win/2));
  nval= numberof(wval);
  if (nval==0) {
    cpk= array(1i,dd);         // might want to have a non-valid flag
    return array(1i,dd);
  }

  ndx= long(rndx(wval)) - (win-1)/2;

  idx= indgen(0:win-1);  // offset (even+/- odd win), idx((win-1)/2+1)=0
  kdx= idx + (ndx + 1 + (wval-1)*n)(-,);
  kdx= kdx(*);

  //     grab a window around the sample;
  if (fcplx)
    cfft= cin(1,..)(kdx)+1i*cin(2,..)(kdx);
  else
    cfft= cin(kdx);

  cfft= reform(cfft,[2,win,nval]);

  //     convert to fouier domian;
  fft_inplace,cfft,[1,0];

  osfft= osf*win;
  covs= array(complex,[2,osfft,nval]);

  //     zeropad to oversample;
  offset= (win-1)/2;
  if (!is_void(carrier)){
    if (structof(carrier)==string){
      carrier= carrierest(a);
    }
    carrier= carrier%1;
    offset-= nint(carrier*win);
  }
  covs(1:win,)= roll(cfft,[offset,0]);
  covs= roll(covs,[-offset,0]);

  //     back to time/space
  fft_inplace,covs,[-1,0];

  //     sample at requested index
  rrsd= rndx(wval) - ndx;   // input rndx assumed 0-based  rndx==0 for cin(1)
  pkndxos= nint(osf*rrsd) + 1 + indgen(0:nval-1)*osfft;
  cndx= cpk= array(complex,dd);         // might want to have a non-valid flag
  cndx(wval)= covs(pkndxos)/win;

  //     sample at peak mag in vicinity of requested index
  aovs= abs(covs);
  movs= aovs(mxx,);
  cpk(wval)= covs(movs+indgen(0:nval-1)*osfft)/win;

  //     peak mag index and entropy (measured peakiness)
  rpk= sent= array(double,dd);          // might want to have a non-valid flag

  //w= where(movs > osfft/2);if (numberof(w))movs(w)-= osfft;   // warn when max at edge?
  rpk(wval)=  ndx + (movs - 1)/double(osf);

  aovs/= max(aovs(sum,)(-,),1e-40);
  sent(wval)= -(aovs*log(max(aovs,1e-40)))(sum,);

  // debug fig
  if (dbgfig==1) {
    fma;pli,aovs;
    plg,indgen(0:d(3)-1),pkndxos%osfft,color="red";
    plg,indgen(0:d(3)-1),movs,color="magenta";
  }

  return cndx;
}

/*-------------------------------------------------------------------------------------*/

func interpol2d (a, m1, m2, &carout, &fftws_in, &fftws_out, carrier=, parsev=, ambig=)
/* DOCUMENT interpol2d (a, m1, m2, carrier=, parsev=, ambig=)
   func interpol2d(a, m1, m2, carrier=)
   Sinc interpolation on reg grid: dimensions from dimsof(a) -> nint([m1,m2])
   nx=35;ny=67;dx=1.23;dy=0.384;x0=102.;y0=893;x=indgen(0:nx)*dx+x0;y=indgen(0:ny)*dy+y0;
   x1=x0+nx/pi*dx;y1=y0+ny/pi*dy;r=abs((x-x1),(y-y1)(-,));z=sinc(pi*r/abs(dx,dy));
   zz=interpol2d(z,200,200);
 */
{
  local ctr;
  if (!is_void(ambig)) {
    ck= roll(fft(a,1));
    cck= deamb(ck,ambig,ctr);
    a= fft(cck/numberof(a),-1);
    carrout= ambig(,*)(,min)+double(ctr)/dimsof(a)(2:);
  } 

  s= dimsof(a);
  if (s(1) != 2)
    error,"interpol2d requires 2dArray, fac1, fac2";

  n1= s(2);
  n2= s(3);
  m1= is_void(m1)? n1: long(m1+0.5);
  m2= is_void(m2)? n2: long(m2+0.5);

  if (n1>m1||n2>m2)
    error,"interpol2d requires m_i>size_i";

  b= array(complex,m1,m2);

  if (is_void(fftws_in)||anyof(*fftws_in(1)!=[n1,n2]))
    fftws_in= fft_setup(dimsof(a));

  // zero-iad to oversample;
  offset= ([n1,n2]-1)/2;
  if (is_void(ctr) && !is_void(carrier)) {
    if (structof(carrier)==string) {
      carrier= carrierest(a);
    } else {
      carrier= carrier%1;
    }
    carout= carrier;
    carrier= nint(carrier*[n1,n2]);
    offset-= carrier;
  }

  b(1:n1,1:n2)= roll(fft(a, [1,1], setup=fftws_in), (is_void(ctr)? offset: ctr));

  renorm= ba= 0;
  if (!(n1%2) && m1>n1 && structof(a)==complex) {
    ba+= (abs(b(n1,))^2)(sum);
    b(n1,)= 0;
    renorm= 1;
  }
  if (!(n2%2) && m2>n2 && structof(a)==complex) {
    ba+= (abs(b(,n2))^2)(sum);
    b(,n2)= 0;
    renorm= 1;
  }
  if (renorm && parsev) {
    bat= n1*n2*(abs(a)^2)(*)(sum);     //bat/(n1*n2);  //checking parseval
    b*= sqrt(bat/(bat-ba))/(n1*n2);    //(abs(b)^2)(*)(sum)*(n1*n2);
  } else{
    b/= (n1*n2);
  }

  b= roll(b, -offset);

  if (is_void(fftws_out))
    fftws_out= fft_setup(dimsof(b));
  fft_inplace, b, [-1,-1], setup= fftws_out;

  return structof(a)(b);
}

/*--------------------------------------------------------------*/

func  pksamp2d (cin, rndx, &rpk, &cpk, &covs, &bx, &sf, &imx, &cfft, \
                &fftws_in, &fftws_out, box=, osf=, carrier=,   \
                prox=, fcplx=, ambig=)
/* DOCUMENT pksamp2d (cin,rndx,&rpk,&cpk,&sent,box=,osf=,dbgfig=)
   does peak analysis across a decimal index in the
   first dimension of cin.  This is "looped" on all
   subsequent indices.
   cin:  array to sample/find local max
   rndx: location (0-based decimal index) to sample by NN after oversampling
   rpk: returned index (0-b. dec.) of local max
   cpk: oversampled local max value
   covs: oversampled array

   NOTE: the sampling at the requested location is NN after oversampling:
         residual is (rndx*osf-nint(rndx*osf))/double(osf);
   NOTE2: the max amp. of the covs array is at 0bdec
         rzzpk= (rpk-long(rndx)+(bx-1)/2)*sf;
   NOTE3: if (is_sream_with_single_member(cin)) no_out_of_bounds_protection

   NOTE4: Output self-consistant
          rpk == long(rndx)-(bx-1)/2 +(zmx-1.)/sf

   c=array(complex,[2,100,100]);
   c(50,50)= 1+1i*1.0;
   rrq= array(49.0,2);
   crq= pksamp2d(c,rrq,rpk,cpk);
 */
{
  box= [0,0]+(is_void(box)? 16: box);
  osf= [0,0]+(is_void(osf)? 16: osf);

  if (is_void(cin))
    return save(box,osf);

  bx= box;
  sf= osf;

  if (anyof(2 > box))
    error, "Warning: box == "+pr1(box);
  if (anyof(2 > osf))
    error, "Warning: osf == "+pr1(osf);

  ndx= long(rndx)-(box-1)/2;
  osfft= osf*box;

  cfft= extractarr((fcplx==1? _(3,2,box): _(2,box)), \
                    (is_stream(cin)? get_member(cin,(*get_vars(cin)(1))(1)): cin), \
                    (fcplx==1? _(0,ndx): ndx));

  if (fcplx && is_real(cfft))
    if (dimsof(cfft)(2)==2)
      cfft= cfft(1,..)+1i*cfft(2,..);
    else
      error,"Not FCPLX.";

  covs= interpol2d(cfft, osfft(1), osfft(2), caro, fftws_in, fftws_out, \
                   carrier=carrier, ambig=ambig);

  // sample at requested index
  rrsd= rndx - ndx;              // input rndx assumed 0-based  rndx==0 for cin(1)
  rndxos= nint(osf*rrsd);
  cndx= covs(rndxos(1)+1,rndxos(2)+1);

  w= where(abs(cndx)==0);
  if (numberof(w))
    cndx(w)= 1.0;

  movs= abs(covs)(*)(mxx)-1;
  movs= [movs%osfft(1),movs/osfft(1)];

  imx= movs+1;
  cpk= covs(imx(1),imx(2));

  //w= where(movs > osfft/2);if (numberof(w))movs(w)-= osfft;   // warn when max at edge?
  rpk=  ndx + movs/double(osf);

  //if (carrier(rms)) {  // put carrier back not necessary... done in interpol2d
  //  write,"pksamp2 carrier..",pr1(carrier),pr1(caro);
  //  cndx*= exp(1i*2*pi*(caro(+)*rndxos(+)));
  //  cpk*= exp(1i*2*pi*(caro(+)*movs(+)));
  //}

  return cndx;
}

func deamb (a, m, &ctr)
/* DOCUMENT deamb (a,m)
   A: Fourier analyzed signal
   M: ambiguity number, leading dimension is of length rank-of-A
   SEE ALSO:
 */
{
  da= dimsof(a);
  ra= da(1);
  dm= dimsof(m);
  if (dm(1)!=ra+1)
    error,"ambiguity number array incorrect dimensions.";
  if (!is_integer(m))
    error,"expecting integer ambiguity number.";
  mp= abs(m(,*)(,ptp))+1;
  mmn= m(,*)(,min);
  dam= _(2*ra,transpose([da(2:),mp])(*));
  aa= array(structof(a)(0),dam);
  if (ra==1) {
    for (i=1;i<=mp(1);i++) {
      msk= m(1,..)==(mmn(1)+i-1);
      if (anyof(msk) && (w=where(msk))(1)) {
        aaa= array(structof(a)(0),da);
        aaa(w)= a(w);
        aa(,i)= a(w);
      }
    }
  } else if (ra==2) {
    for (i=1;i<=mp(1);i++) {
      for (j=1;j<=mp(2);j++) {
        msk= (m(1,..)==(mmn(1)+i-1)) & (m(2,..)==(mmn(2)+j-1));
        if (anyof(msk) && (w=where(msk))(1)) {
          aaa= array(structof(a)(0),da);
          aaa(w)= a(w);
          aa(,i,,j)= aaa;
        }
      }
    }
  } else if (ra==3) {
    for (i=1;i<=mp(1);i++) {
      for (j=1;j<=mp(2);j++) {
        for (k=1;k<=mp(3);k++) {
          msk= (m(1,..)==(mmn(1)+i-1)) &          \
            (m(2,..)==(mmn(2)+j-1)) &            \
            (m(3,..)==(mmn(3)+k-1));
          if (anyof(msk) && (w=where(msk))(1)) {
            aaa= array(structof(a)(0),da);
            aaa(w)= a(w);
            aa(,i,,j,,k)= a(w);
        }
        }
      }
    }
  } else
      error,"rank>3 not allowed.";

  damp= _(da(1),(da(2:)*mp)(*));
  aa= reform(aa,damp);

  ctr= centroid(aa,norm=2)-1;

  return roll(aa,-ctr);
}

/*-------------------------------------------------------------------------------------*/

func interpol8(a, m, &drv, carrier=, deriv=, wrkspc=, parsev=){
/* DOCUMENT interpol8(a, m, &drv, carrier=, deriv=, wrkspc=)

 Sinc interpolation on reg grid: dimensions from dimsof(a) -> [1,nint(m)])
 to obtain derivative "drv" use "deriv=1" keyword
 wrkspc is from fft_setup([1, m]);
 carrier= is required to be in cycle/sample (ie. it will be multiplied by 2.pi.numberof(a))
          iff it is a string ("") then carrier is estimated as atan<a(i)*a~(i-1)>)/(2.pi)
 test---------------
 #include "dsp.i"
 #include "gauss_rp.i"
 #include "spline.i"
 #include "sincint.i"
 n= 200;
 m= 500;
 y= gauss_rp(n, 2);
 x= span(0, n-1.0, n);
 yy= interpol8(y, m);
 xx= span(0, m-1.0, m)*n/m;
 yyy= spline(y,x,xx);
 fma;plg, y, x; plg, yy, xx, color="red";plg, yyy, xx, color="blue"
 [(yy-yyy)(avg), (yy-yyy)(rms)];
  --test--2--
  t= tsincfltr ((decim=1024),(length=16),(fracb=0.7),(pedh=.05));
  n=100;m=300;
  f0=random();
  func test1(f0){
    x= filterdelta (t,n,(rrq=39.2))*exp(1i*2*pi/n*indgen(0:n-1)*f0*n);
    y= interpol8(x,m,carrier=f0);
    yy= interpol8(x*exp(-1i*2*pi/n*indgen(0:n-1)*f0*n),m)*
        exp(1i*2*pi/m*indgen(0:m-1)*f0*n);
    yyy= interpol8(x,m,carrier="d");
    fma;plg,roll(20*log(abs(fft(x))/n)),indgen(-n/2:n-n/2-1);
    plg,roll(20*log(abs(fft(y))/m+1e-30)),
          indgen(-m/2:m-m/2-1),color="red",type=3;
    plg,roll(20*log(abs(fft(yy))/m+1e-30)),
          indgen(-m/2:m-m/2-1),color="blue",type=4;
    plg,roll(20*log(abs(fft(yyy))/m+1e-30)),
          indgen(-m/2:m-m/2-1),color="magenta",type=2;
  }
  func test(f0){
    x= filterdelta (t,n,(rrq=39.2))*exp(1i*2*pi/n*indgen(0:n-1)*f0*n);
    fma;plg,roll(20*log(abs(fft(x))/n)),indgen(-n/2:n-n/2-1);
    plg,roll(20*log(abs(fft(interpol8(x,m,carrier=f0)))/m+1e-30)),
          indgen(-m/2:m-m/2-1),color="red";
  }
 */
  s= dimsof(a);
  if (s(1)!=1)error,"interpol8 requires1dArray, outDim";
  n= s(2);
  m= long(m+0.5);
  m2= (m-1)/2;
  if (n>m)error,"interpol8 requires m > size_a";

  if (is_void(wrkspc))wrkspc=fft_setup([1, m]);

  b= array(complex, m);
  n2= (n-1)/2;
  offset= n2;
  if (!is_void(carrier)){
    if (structof(carrier)==string){
      carrier= carrierest(a);
    }
    carrier= carrier%1;
    offset-= nint(carrier*n);
  }

  b(1:n)= roll(fft(a, 1), offset);
  if (!(n%2)&&structof(a)==complex){
    bat= n*(abs(a)^2)(sum);
    ba= abs(b(n))^2;
    b(n)= 0;
    if (parsev){
      b *= sqrt(bat/(bat-ba))/n;
    }else{
      b /= n;
    }
  }else{
    b /= n;
  }
  if (deriv){
    if (!is_void(carrier))m2 -= carrier*n;
    drv= b*(2*pi*1i/m)*span(-m2, m-m2-1, m);
    drv= roll(drv, -offset);
    fft_inplace, drv, -1, setup=wrkspc;
    drv= structof(a)(drv);
  }
  b= roll(b, -offset);
  fft_inplace, b, -1, setup=wrkspc;
  return structof(a)(b);
}

/*-------------------------------------------------------------------------------------*/

func interpol8n(a, m, &drv, &work, &workm, carrier=, deriv=)
/* DOCUMENT convcorreln(a, m, &drv, &work, &workm, carrier=, deriv=)
   SEE ALSO
 */
{
  typo= structof(a(1));
  n = dimsof(a);
  ndims = n(1);

  m+= array(0,ndims);
  dir= m>1;
  if (is_void(carrier)) carrier= 0;
  carrier+= array(0,ndims);

  nt = array(ndims, 1+ndims);
  for (i=1,p=1 ; i<=ndims ; ++i) {
    nt(1+i) = fft_good(n(i+1));
    p*= nt(i+1);
  }
  mt= nt*_(1,m);

  if (anyof(nt!=n)) {
    at = array(complex, nt);
    embedarr,a,at;
  } else {
    eq_nocopy,at,a;
  }
  if (typo!=complex) at= complex(at);

  if (is_void(work)) work= fft_setup(nt,dir);
  fft_inplace, at, dir, setup=work;
  at/= p;

  n2= (nt(2:)-1)/2;
  offset= n2;
  carrier= cycle(carrier,-.5,.5);
  offset-= nint(carrier*nt(2:));  // result between [0,nt]
  offset*= m>1;
  roll,at,offset;

  bt = array(complex, mt);
  embedarr,at,bt,-offset,wrap=1;

  if (is_void(workm)) workm= fft_setup(mt,-dir);
  fft_inplace, bt, -dir, setup=workm;

  return typo(extractarr(n*_(1,m),bt));
}

/*-------------------------------------------------------------------------------------*/

func fft_good(n,low=)
/* DOCUMENT fft_good(n)

   returns the smallest number of the form 2^x*3^y*5^z greater
   than or equal to n.  An fft of this length will be much faster
   than a number with larger prime factors; the speed difference
   can be an order of magnitude or more.

   For n>100, the worst cases result in a little over a 11% increase
   in n; for n>1000, the worst are a bit over 6%; still larger n are
   better yet.  The median increase for n<=10000 is about 1.5%.

   SEE ALSO: fft, fft_setup, convol
*/
{
  if (n<7) return max(n,1);
  logn= log(n);
  n5= 5.^indgen(0:long(logn/log(5.) + 1.e-6));  /* exact integers */
  n3= 3.^indgen(0:long(logn/log(3.) + 1.e-6));  /* exact integers */
  n35= n3*n5(-,);             /* fewer than 300 numbers for n<5e9 */
  n35= n35(where(n35<=n));
  n235= 2^long((logn-log(n35))/log(2.)+(low==1? 0.0: 0.999999)) * long(n35);
  if(low==1) {
    m= n235<n;
    if (anyof(m))
      n235= n235(where(m));
    else
      error,"fix algo";
  }
  return (low==1? max(n235): min(n235));
}

/*-------------------------------------------------------------------------------------*/

func fitline(y,x,sig)
/* DOCUMENT fitline (y,x)
 returns a vector of fit(1)=a fit(2)=b where y=a+bx
 This routine assumes unit error bars and does nothing fancy,
 more..better routines may be written some day.
*/
{
  dx= dimsof(x);
  dy= dimsof(y);
  if (is_void(sig)) sig=array(1.,dimsof(y));
  Sxx= (x^2/sig^2)(sum);
  Sxy= (x*y/sig^2)(sum);
  Sx = (x/sig^2)(sum);
  Sy = (y/sig^2)(sum);
  S  = (1/sig^2)(sum);
  del= (S*Sxx-(Sx)^2);
  a= (Sxx*Sy-Sx*Sxy)/del;
  b= (S*Sxy-Sx*Sy)/del;
  da= sqrt(Sxx/del);
  db= sqrt(S/del);
  xisqi= (((y-a-b*x)/sig)^2)(sum);
  return ([a,b,da,db,xisqi]);
}

#if 0
// require,"sincint.i"
/*-------------------------------------------------------------------------------------*/

func testscaleft(n, xintb, xinte, corrInDx, decim, length, fracb){
/* DOCUMENT  testinterpol(n, corrInDx, decim, length,fracb, expfac)
 */
  f= gauss_rp(n, corrInDx);
  xx= span(xintb,xinte,n);
  t= sincfilter_f(decim,length,fracb,0.0);
  ff= interpfilter(t(filter),f,0.,1.,xx,mode="periodic");
  gg= scaleft(f, xintb, xinte);
  fr= fitline(gg,ff);fr;
  ff= (fr(1)+fr(2)*ff)
  window,0;
  fma;
  plg,f,span(0,n-1,n);
  plg,ff,xx,type=0,marker='\2';plg,ff,xx,type="dash";
  plg,gg,xx,type=0,marker='\3';plg,gg,xx,type="dashdot";
  limits,xintb,xinte,"e","e",;
}
#endif

/*---------------------------------------------------------------------------*/

func scaleft(a, xB, xE, &scale){
/* DOCUMENT
  func scaleft(a, xB, xE, &scale)
  scaling with fft from xB to xE (assuming first a @ X=0.0, DeltaX=1.0)
  number of samples conserved
  ! Gibbs phenomenon... this needs some work
 */
  typea=typeof(a);
  s=dimsof(a);
  if (s(1)!=1)error,"scaleft requires 1d array: scaleft(a, xB, xE)";
  n= s(2);
  n2=(n-1)/2;
  next= 2^(long(log(double(n-1))/log(2.))+2);
  next2= (next-1)/2;
  npad2=(next-n)/2;
  zm= (xE-xB)/(2*(n-1.));
  scale= 2*zm;
  k= (2*pi/n)*roll(span(-n2, n-n2-1, n),-n2);
  a= fft(a, 1)*exp(1i*k*xB);
  a=  fft(a, -1)/n;
  pind= indgen(1:npad2);               //index for padding
  pcof= exp(-0.5*((pind-1.)/(.25*npad2))^2);   //damping coeff for padding
  pr= (a(0)+(a(0)-a(-1))*pind)*pcof;       //right padding
  if ((next-n)==(2*npad2+1)){
   pcof=grow(pcof,[0.]);
   pind=grow(pind,[npad2+1]);
  }
  pl= (a(1)+(a(1)-a(2))*pind)*pcof;        //left padding
  a=  fft(grow(a,pr,pl(::-1)),1);
  k= roll(span(-next2, next-next2-1, next),-next2);
  b= exp((1i*zm*2*pi/next)*k^2);
  b= b(1:n) * fft(fft(a*b,-1)*fft(conj(b),-1),1)(1:n)/next^2;
  if (typea!="complex")b=typeconv(typea,b);
  return b;
}

/*---------------------------------------------------------------------------*/

func fftconvol(h, x, &y0, &y1, npatch=, wrkspc=, hft=, radix2=){
/* DOCUMENT fftconvol(h, x, npatch=, wrkspc=, hft=, radix2=)
 convol - fft(s) convolution by patches (causal, overlap add method)
                                         ^i.e. tails == y(1:Nh) & y(Nx+1:Nx+Nh-1)
          y_{n=1,Nh+Nx-1}= SUM{k=1,Nx}[ h_n-k * x_k ]
 CALLING SEQUENCES
     y=convol(h,x,[npatch=], [wrspc=fft_setup([1, nfft!?])],
              [hft=fft(grow(h, array(0.,nfft!?-m)),1)], [radix2=1] )
 PARAMETERS
 x,h       :input sequences (h is a "short" sequence, x a "long" one)
 hft       :optional fourier analyzed convolution kernel (zero-end-padded to nfft length);
 npatch    :optional number of patches used in convolving; DEFAULT is 1
 wrkspc    :optional initialization parameters for fft's of size NFFT
 radix2    :optional if == 1 then
            NFFT= 2^(long(log((Nh+Nx-1)/npatch+(npatch>1)*Nh-1)/log(2.))+1); DEFAULT is 0
 DESCRIPTION
 calculates the convolution y= h*x of two discrete sequences by the overlap
 add method (fft).

 T. Michel 1998
 */
 sh= dimsof(h);
 m= sh(2);

 sx= dimsof(x);
 n= sx(2);

 typeout= typeof(x(1)*h(1));

 nt0= n + m - 1;

 if (!is_void(radix2)&&radix2==1){
   nfft=2^(long(log(nt0-1)/log(2.))+1); //speed
 }else{
   nfft= nt0;
 }

 if (!is_void(npatch)&&npatch>1){
   ll= nt0/long(npatch);
   if (!is_void(radix2)&&radix2==1){
     nfft2= 2^(long(log(m+ll-1-1)/log(2.))+1);
   }else{
     nfft2= m+ll-1;
   }
   if (is_void(wrkspc))wrkspc=fft_setup([1, nfft2]);
   if (is_void(hft))hft=fft(grow(h, array(0.,nfft2-m)),1,setup=wrkspc);
   y1= 0;
   y= fftconvol(h, x(1:ll), , y1, wrkspc=wrkspc, hft=hft, radix2=radix2);
   y0= y1;
   for (i=2;i<npatch;i++){
     y= grow(y, fftconvol(h, x((i-1)*ll+1:i*ll), y0, y1, wrkspc=wrkspc, hft=hft, radix2=radix2));
     y0= y1;
   }
   info,y;
   xn= grow(x((npatch-1)*ll+1:n), array(0, ll-n%ll));
   y= grow(y, fftconvol(h, xn, y0, , wrkspc=wrkspc, hft=hft, radix2=radix2)(1:n%ll+m-1) )
   return y;
 }

 if (is_void(npatch) || npatch == 1){

   if (is_void(wrkspc))wrkspc=fft_setup([1,nfft]);

   if (is_void(hft))hft=fft(grow(h, array(0.,nfft-m)), 1,setup=wrkspc);

   y= fft(grow(x,array(0.,nfft-n)), 1, setup=wrkspc) * hft;

   fft_inplace, y, -1, setup=wrkspc;

   y /= nfft;

   if (anyof(["int","long"]==typeout)){
     y= typeconv("double",y)+0.5;
     if (typeout == "int"){
       y= int(y)-(y<0);
     }else{
       y= long(y)-(y<0);
     }
   }else{
     y= typeconv(typeout,y);
   }

   if (npatch == 1)return y(1:nt0);

   if (!is_void(y0)&&!is_void(y1)){//away from edges update
     y1= y(n+1:nt0);
     y= y(1:n);
     y(1:nt0-n) +=  y0;
     return y;
   }else if (is_void(y0)&&!is_void(y1)){//initial update
     y1= y(n+1:nt0);
     y= y(1:n);
     return y;
   }else if (!is_void(y0)&&is_void(y1)){//final update
     y(1:nt0-n) +=  y0;
     return y;
   }
   return y(1:nt0);
 }
}

/*---------------------------------------------------------------------------*/

func cztz (x, m, dz, z0)
/* DOCUMENT cztz(x, m, z0, dz)
  chirp z-transform algorithm which calcultes the
  z-transform of x(0:N-1) on a spiral in the z-plane at the points
  Zk= z0 * dz^(-k) for k=0,1,...,M-1.
  i.e. czt(k)= SUM{ x(n) Zk^(-n)} for n=0,-,N-1
             = SUM{ x(n) [a*exp(j*theta)]^-n [w exp(-j*phi)]^nk } for n=0,-,N-1
    x     :Input data sequence
    m     :czt is evaluated at m points in z-plane, DEF=Nx
    w     :phsor multiplier
    dz    :Initial phasor
    cztz   :Chirp z-transform output
   #include "dsp.i"
   #include "gauss_rp.i"
   z= gauss_rp(360, 10.);                                                                                                                                  z=gaussm(random_n(300)+1i*random_n(300),5);
   zf=fft(z,1);
   fma;plg,z;plg,cztz(zf, 360, 1.0, exp(-1i*2*pi/360)).re/360,color="red",type="dash";
  SEE ALSO: czt
 */
{
  if (is_void(z0)||is_void(dz)) error,"no defaults here, see czt";

  z0+= 0*1i;
  dz+= 0*1i;

  return czt(x, m, abs(dz), atan(dz.im,dz.re), abs(z0), atan(z0.im,z0.re));
}

/*---------------------------------------------------------------------------*/

/*
func tczt1(void)
{
  m= 400;
  z= gauss_rp(m, 10.);
  zf= fft(z,1);
  wksp= [];
  animate,1;
  for(i=1;i<=m;i++){
    fma;
    plg,z;
    plg,czt(zf,m,1.0,-2*pi/m,1.0,2*pi/m*i,wksp).re/m,color="red",type="dash";
  }
  animate,0;
}
func tczt2(void)
{
  n= 400;
  x= span(0,n-1,n);
  z= gauss_rp(n, 10.);
  zf= fft(z,1);
  zf= _(zf,zf);
  x1=  274.4; x0= 328.7; // x from 0:n-1
  x1=0.0;x0=n-1;
  m= n/2;
  xx= span(x1,x0,m);
  fma;
    plg,z,x;
    plg,czt(zf,m,1.0,-2*pi*(x0-x1)/((n-1)*m),1.0,2*pi).re/n,xx,color="red",type="dash";
}

*/

func czt(x, m, w, phi, a, theta, &wksp)
/* DOCUMENT czx=czt(x,m,w,phi,a,theta, &wksp)
  chirp z-transform algorithm which calcultes the
  z-transform of x(0:N-1) on a spiral in the z-plane at points
  Zk= A*W^(-k)= [a*exp(j*theta)][w exp(-j*k*phi)]^(-k) for k=0,1,...,M-1.
  i.e. czt(k)= SUM{ x(n) Zk^(-n)} for n=0,-,N-1
             = SUM{ x(n) [a*exp(j*theta)]^-n [w exp(-j*phi)]^nk } for n=0,-,N-1
    x     :Input data sequence
    m     :czt is evaluated at m points in z-plane, DEF=Nx
    w     :Magnitude multiplier, DEF= 1
    phi   :Phase increment, DEF= -2*pi/m ** change sign for DIRECTION **
    a     :Initial magnitude, DEF= 1
    theta :Initial phase, DEF= 0
    czt   :Chirp z-transform output,  with ALL DEF == fft(x,-1)
  czt does fft test: abs(fft(indgen(0:9),-1)-czt(indgen(0:9)))
  SEE ALSO: cztz
 */
{
   n= dimsof(x)(2);
   if (is_void(m)) m= n;
   if (is_void(w)) w= 1.0;
   if (is_void(phi)) phi= -2*pi/m;
   if (is_void(theta)) theta= 0.0;
   if (is_void(a)) a= 1.0;

   //get the maximum of (n,m)
   nm= max(n,m);

   //create sequence h(n)=[w*exp(-j*phi)]**(-n*n/2)
   w= w * exp(-1i*phi);
   dix= span(-nm+1,nm-1,2*nm-1);    // double(indgen(-nm+1:nm+1))
   dix= -dix^2/2;
   h= exp(dix*log(w));

   //create g(n)
   a= a * exp(1i*theta);
   dix= span(0,n-1,n);
   g= exp(-dix*log(a))/h(nm:nm+n-1);  // positive freqs from 0:n=len_x
   g= x * g;

   //convolve h(n) and g(n), preserve m points and divide by h(n)
   //M freqs>0, including 0, folowed by N negative freqs, up to -1
   h= roll(h(nm-n+1:nm+m-1),-n+1);  // length N+M-1 ?==? fft_good
   g= convcorreln(h,g,wksp,n0=1,n1=m);
   return g/h(1:m);
}

/*---------------------------------------------------------------------------*/

func fftcirconvol(h, x, wrkspc=)
/* DOCUMENT fftcirconvol(h, x, wrkspc=)

   usage: example, BOXCAR averaging in circles
   -----
   m= 50;
   io= array(0.0, 10*m);
   io(2*m)= 1;
   iom= roll(fftcirconvol(io, array(1., m)/double(m)), -m/2);
  */
{
  typo= structof(x(1)*h(1));

  nx= dimsof(x)(2);
  nh= dimsof(h)(2);
  mx= max(nx, nh);
  xnx= double(nx);
  xnh= double(nh);

  if (is_void(wrkspc)) wrkspc= fft_setup([1, mx]);

  if (nh>nx){
    z= fft(h,1,setup=wrkspc) * fft(grow(x/xnh,array(0.,nh-nx)),1,setup=wrkspc);
  }else if (nx>nh){
    z= fft(grow(h/xnx,array(0.,nx-nh)),1,setup=wrkspc) * fft(x,1,setup=wrkspc);
  }else{
    z= fft(h/xnx,1,setup=wrkspc)*fft(x,1,setup=wrkspc);
  }

  fft_inplace,z,-1,setup=wrkspc;

  if (typo==int||typo==long) {
    z= double(z)+0.5;
    return typo(z)-(z<0);
  }else{
    return typo(z);
  }
}

/*---------------------------------------------------------------------------*/

func cubffft(x, dir, df, f0, m){
/*DOCUMENT cubffft(x, dir, df, f0, m)
 cubic chirp-z approx of finite fourier transform
 df ~ 1./m for full spectrum
 */
  nx= dimsof(x)(2);
  if (is_void(m))m=nx;
  if (is_void(df))df=1./m;
  if (is_void(f0))f0=0.;
  m2=(m-1)/2;
  y= czt(x, m, 1., 2*dir*pi*df, 1., 2*pi*f0);
  k= 2*pi*(roll(span(-m2*df,(m-m2-1)*df,m),-m2) + f0);
  w= ffft_weights(k)/n;
  ephi= exp(1i*k*nx);
  y= w(..,5)*(y+x(0)*conj(ephi))+w(..,1:4)(..,+)*x(1:4)(+)+ephi*(conj(w(..,1:4))(..,+)*x(0:-3:-1)(+));
  return y;
}

func ffft_weights(x,epsil=){
 if (is_void(epsil))epsil= 1e-8;
 if (is_scalar(x)){scal=1;x= x(,-);}
 y= x(,-:1:5);
 q= abs(x)<epsil;
 if (anyof(q)){
   w= where(q);
   tet= x(w);
   y(w,1)=  poly(tet, -2./3., 0., 1./45., 0., 103./15120., 0.,-169./226800.) -
    1i * tet * poly(tet, 2./45., 0., 2./105., 0.,-8./2835., 0., 86./467775.);
   y(w,2)= poly(tet, 7./24., 0., -7./180., 0., 5./3456., 0., -7./259200.) -
    1i * tet * poly(tet, 7./72., 0., -1./168., 0., 11./72576., 0., -13./5987520.);
   y(w,3)= poly(tet, -1./6., 0., 1./45., 0., -5./6048., 0., 1./64800.) -
    1i * tet * poly(tet, -7/90., 0., 1./210., 0., -11/90720., 0., 13/7484400.);
   y(w,4)= poly(tet, 1./24., 0., -1./180., 0., 5./24192, 0., -1./259200.) -
    1i * tet * poly(tet, 7./360, 0., -1./840., 0., 11./90720., 0., -13/29937600.);
   y(w,5)= poly(tet, 1., 0., 0., 0., -11./720., 0., 23./15120.);
 }
 if (anyof(!q)){
   w= where(!q);
   tet= x(w);
   tet2= tet^2;
   tet4= tet2^2;
   sintet= sin(tet);
   costet= cos(tet);
   cos2tet= cos(2*tet);
   y(w,1)= (((-42 + 5*tet2)+(6+tet2)*(8*costet-cos2tet))
       - 1i*(tet*(-12+6*tet2)+2*(6+tet2)*sintet*costet))/(6*tet4);
   y(w,2)= ((14*(3 - tet2)-7*(6+tet2)*costet)
      - 1i*(30*tet-5*(6+tet2)*sintet))/(6*tet4);
   y(w,3)= ((-4*(3 - tet2)+2*(6+tet2)*costet)
       - 1i*(-12*tet+2*(6+tet2)*sintet))/(3*tet4);
   y(w,4)= ((2*(3 - tet2)-(6+tet2)*costet)
       - 1i*(6*tet-(6+tet2)*sintet))/(6*tet4);
   y(w,5)= ((6.+ tet2)/(3.*tet4))*(3. - 4*cos(tet) + cos2tet);
 }
 if (scal)return shrink1(y);
 return y;
}

/*--------------------------------------------------------------------------*/

func hilbert(x, dir, wrkspc=){
/* DOCUMENT function z= hilbert(x, [dir, [, wrkspc=]])
 Routine for computing Hilbert transform of real or complex sequence
 Attn! in DSP the Hilbert transform has a "~loose" interpretation.
 NOTE1: for real input (structof!=complex) return x + 1i*hilbert(x)
        SEE analyt. (check: hilbert(indgen(4)).im;hilbert(complex(indgen(4))))
 NOTE2: for complex x, with x.im== 0:  hilbert(x).im== 0
 NOTE3: Z= S + iH;  phase atan(H/S); peak freq dH/dt
 mathwrks: hilbert(indgen(4))= [1+1i,2-1i,3-1i,4+1i]
 */
 typex= structof(x);
 dx= dimsof(x);

 di= dx(0)? array(0,dx(1)) : 1;
 if (dx(0))di(1)= 1;

 nx= dx(2);
 n2= (nx-1)/2;      // number of +/- freqs, != 0, or NYQ for even nx

 if (is_void(wrkspc))wrkspc=fft_setup(dx, nx*di);

 if (typex == complex){
   fft_inplace, x, di, setup=wrkspc;
   zi= -1i;
   if (!is_void(dir)) zi= zi * dir;
   x(2:n2+1,..) *= zi;    //multiplying by zi rotates counter c.w. 90 deg.
   x(-n2+1:0,..) *= -zi;
   x(1,..)= 0.0;
   if (nx%2==0)x(n2+2,..)= 0.0;
   fft_inplace, x, -di, setup=wrkspc;
 }else{                 // same as analyt below
   x= complex(x);
   fft_inplace, x, di, setup=wrkspc;
   x(2:n2+1,..) *= 2.0;
   x(-n2+1:0,..)= 0;
   fft_inplace, x, -di, setup=wrkspc;
 }
 return x/nx;
}

/*--------------------------------------------------------------------------*/

func analyt (x, wrkspc=)
/* DOCUMENT function z= analyt(x, [, wrkspc=])
 A signal which has no negative-frequency components is an analytic signal
 Routine for computing Hilbert transform of a real sequence
 and return the analytic : x + 1i * Hilbert(x)
 */
{
  typex= structof(x);
  dx= dimsof(x);

  di= dx(0)? array(0,dx(1)) : 1;
  if (dx(0))
    di(1)= 1;

  nx= dimsof(x)(2);
  n2= (nx-1)/2;

  if (typex == complex)
    error,"expecting a int/float/double sequence";
  else
    x= complex(x);

  zi= array(1.0/nx, nx);
  zi(2:n2+1)*= 2.0;          // x+1i*h(x): (1+1i*1i*sign(freq))
  zi(-n2+1:0)= 0.0;

  if (is_void(wrkspc))
    wrkspc= fft_setup(dx, nx*di);

  fft_inplace, x, di, setup=wrkspc;

  x*= zi;

  fft_inplace, x, -di, setup=wrkspc;

  return x;
}

/*--------------------------------------------------------------------------*/

func iq2ov(x)
/* DOCUMENT
  func tstiqov(n,t=)
  {
  if (t==1){
    zr=random(n);zr-=zr(avg);
    fma;plg,zr-ov2iq(iq2ov(zr));plg,zr,type=3;limits;
  }else if (t==2){
    zr=indgen(n);zr-=zr(avg);
    fma;plg,zr-ov2iq(iq2ov(zr));plg,zr,type=3;limits;
  }else if (t==3){
    z= exp(1i*pi*300*span(-1,1,n)^2);
    reshape,zr;reshape,zr,&z,double,[1,2*n];
    fma; plg,log(abs(fft(z))+1e-30),width=3;
    plg,log(abs(fft(iq2ov(zr)))/2+1e-30),color="red",type=3,width=3;
    plg,log(abs(fft(ov2iq(iq2ov(zr),cout=1)))+1e-30),color="blue",type=3,width=3;
  }
  }
 */
{
  ns= numberof(x);
  iod= ns%2;
  ns_2= ns/2;
  ns_4= ns/4;

  z= array(complex,ns_2+iod);
  zz= array(complex,ns);

  z(1:ns_2)= x(1:ns-iod:2) + 1i*x(2:ns-iod:2);
  if (iod)z(ns_2+1)= x(ns);

  z(1::2) *= -1;

  fft_inplace, z, 1;
  z /= ns_2;

  zz(:ns_2+iod)= z;
  if (!iod)zz(ns_2+1).im= 0;  // NYQ
  zz(1).im= 0;

  zz(ns_2+2:ns)= conj(zz(ns_2+iod:2:-1));

  fft_inplace, zz, -1;

  return zz.re;
}

/*--------------------------------------------------------------------------*/

func ov2iq(x,cout=)
{
  local y;

  ns= numberof(x);
  iod= ns%2;
  ns_2= ns/2
  ns_4= ns/4;

  z= array(complex,ns);
  zz= array(complex,ns_2+iod);

  z= complex(x);

  fft_inplace, z, 1;
  z /= ns;

  zz= z(:ns_2+iod);

  fft_inplace, zz, -1;

  zz(1::2) *= -1;

  if (cout){
    return zz;
  }else{
    reshape, y, &zz, double, [1,2*(ns_2+iod)];
    return y(1:ns);
  }
}

/*--------------------------------------------------------------------------*/

func slope(x, wrkspc=){
/* DOCUMENT function z= slope(x, [wrkspc=])
 z= slope of a real or complex sequence
 !!if Nx%2==0 && x==real double(integral(slope(complex(x))))= x
 */
 typex= typeof(x);
 nx= dimsof(x)(2);
 n2= (nx-1)/2;

 if (is_void(wrkspc))wrkspc=fft_setup([1, nx]);

 x= typeconv("complex", x);

 fft_inplace, x, 1, setup=wrkspc;

 k= (2*pi*1i/nx^2)*roll(span(-n2, nx-n2-1, nx),-n2);

 x *= k;

 fft_inplace, x, -1, setup=wrkspc;

 if (typeof(x)!=typex) x= typeconv(typex,x);

 return  x;
}

/*--------------------------------------------------------------------------*/

func integral(x, wrkspc=){
/* DOCUMENT function z= slope(x, [wrkspc=])
 z= slope of a real or complex sequence
 !!if Nx%2==0 && x==real double(integral(slope(complex(x))))= x
 */
 typex= typeof(x);
 nx= dimsof(x)(2);
 n2= (nx-1)/2;

 if (is_void(wrkspc))wrkspc=fft_setup([1, nx]);

 x= typeconv("complex", x);

 fft_inplace, x, 1, setup=wrkspc;

 x1= x(1);
 k= (2*pi*1i)*roll(span(-n2, nx-n2-1, nx),-n2);
 k(1)=1.;

 x /= k;

 x(1)= 0.;

 fft_inplace, x, -1, setup=wrkspc;

 x += x1*span(0, nx-1, nx);

 if (typeof(x)!=typex) x= typeconv(typex, x);

 return  x;
}

/*--------------------------------------------------------------------------*/

func fmpoly(n, deg){
/* DOCUMENT function fmpoly(x, deg)
 create a signal with a polynomial frequency modulation
 roll(zatan(fmpoly(n,1)),-(n-1)/2)(1)=0 if n is odd
 the max derivative of phase polynomial is the slope of fmpoly(n, 1)
 cof= (2*pi/n)/(deg*(double(n/2))^(deg-2.)); //MAX SLOPE (max d/dn) == pi == NYQUIST
 Usage
   x= fmpoly(n, deg)
 Inputs
   n    length of the signal
   deg  order of the polynomial phase (deg=2 -> linear chirp)
 Outputs
   x    signal
 */
 if (deg == 0)error,"don't know about 0th degree polynomial!";

 cof= (2*pi/n)/(deg*(double(n/2))^(deg-2.)); //MAX SLOPE (d/dn) == pi == NYQUIST

 return exp(1i*cof*(span(0,n-1,n)-(n-1)/2-(n%2==0)/2.)^deg);
}

/*--------------------------------------------------------------------------*/

func fmsin(n, p=, ph=){
/* DOCUMENT function fmsin(n, p, phs)
 fmsin -- create a signal with a sinusoidal frequency modulation
 Usage
   x= fmsin(N, p, phs)
 Inputs
   N    length of the signal
   p    number of cycles (optional, defaults to 1)
   ph   phase shift of the instantaneous frequency(optional, defaults to 0)
 Outputs
   x    signal [complex(n)]
 */

 if (is_void(p))p=1.;
 if (is_void(ph))ph=0.;

 xn= span(0., (n-1.)/n, n);

 return exp((-1i*n/(4*p))*sin((p*2*pi)*xn + ph));
}

/*--------------------------------------------------------------------------*/

struct peakAnl {
  double    maxPhase;
//  double  phaseGrad;
  double    maxPos;
  double    maxVal;
  double    fwhm;
  double    pslr;
  double    islr;
  double    pkint;
  double    pkint2;
  double    maxPosPred;
  double    maxPhasePred;
  double    fcar;
  double    fcarPred;
}
extern peakAnl
/* DOCUMENT
   SEE ALSO: peak_anl
 */


func peak_anl (zz0, &z, pkpos=, osf=, carrier=, islrtrsh=, islrwidth=, deg= , dbg=)
/* DOCUMENT  res= peak_anl(zz, z, &dx, wrkspc_m=, wrkspc_n=,  xTaper=, xscale =, deg= )

  res= struct peakAnl { double maxPhase; double phaseGrad; double maxPos;
                         double fwhm; double pslr; double islr;}
  zz is complex or real
  z is a complex work space provided (whose length is that of the oversampled z)
  OR z is the length (integer) of oversampled z
  fwhm is the full width at half maximum of POWER  |zz|^2

  NOTE on scale: a "dx" sample spacing should need to act as a multiplier on maxPos and fwm
                 it needs to go in as a divisor if we have a scale islrwidth (see below).

  x=span(-20,20,400)*pi;dx=x(2)-x(1);f=sinc(x);p= peak_anl(f);sinc(dx*p.fwhm/2)^2;


  usage example:
  -------------
  #include "dsp.i"

  m= 40;
  n= 6;
  mm= n*m;
  mm2=(mm-1)/2;
  write,format="Number of samples: %8ld\n",mm;

  b= - 2.0;
  write,format="Phase ramp rate b (as in phase ~ exp(i.b.2.pi.n/N) ): %8.2lg\n",b;

  f0= roll(grow(roll(hamming(m), m/2),array(double,(n-1)*m)),-m/2);
  p0= peak_anl(fft(f0));
  write,format="Peak Postion before shift: %8.3le\n",p0.maxPos;

  f= f0*exp(-1i*b*2*pi*roll(indgen(-mm2:mm-1-mm2),-mm2)/double(mm));
  p= peak_anl(fft(f));
  write,format="Peak Postion after shift: %8.3le\n",p.maxPos;
  write,format="Peak Shift: %8.3le\n",p.maxPos-p0.maxPos;

  ISLRWIDTH check
  x= span(-30,30,20001)*pi;dx=x(2)-x(1);f=sinc(x);
  10*log10((ml=integ(f^2,x,2*pi)-integ(f^2,x,-2*pi))/(integ(f^2,x,x(0))-ml));
  peakfit,f^2,x,dbg=1,islrwidth=4*pi;
  peak_anl(f,islrwidth=4*pi/dx,dbg=1);
  OR
  x= span(-30,30,20000);dx=x(2)-x(1);f=sinc(pi*x);
  10*log10((ml=integ(f^2,x,2)-integ(f^2,x,-2))/(integ(f^2,x,x(0))-ml));
  peakfit,f^2,x,dbg=1,islrwidth=4;
  peak_anl(f,islrwidth=4/dx,dbg=1);

  n=8000;m=300;p=n/2+1/pi;
  k=fftindgen(n)*2*pi/n;z=fft(exp(1i*k*p)*(abs(k)<pi/m))*m/n;
  window,3;fma;plg,abs(z),indgen(0:n-1);
           plg,abs(sinc(pi*((i=indgen(0:n-1))-p)/m)),i,color="red"
  limits,p,p+2*m,0,1.0;
  p= peak_anl(z,dbg=1);
  peak_anl(z,dbg=1,islrwidth=p.fwhm);
  peak_anl(z,dbg=1,islrwidth=4*m);

  range compressed cut at n(rg)m(az)

  func plrg(n,m){
    c=peak_anl(fslc.x(n+indgen(-31:32),m),z,osf=32,deg=1,islrwidth=4*9/8.);
    window,1;fma;plg,20*log10(abs(z)/c.maxVal),span(-31,32,64*32);
    s=swrite((c.maxPos-31)*5/3,c.maxVal,c.maxPhase,c.fwhm*5/3,c.pslr,c.islr,format="max-pos:%4.3fm, \
            max-val:%-#7.2g,\nmax-phs:%-#4.1fdeg,fwhm:%4.3fm, pslr:%4.3fdB, islr:%4.3fdB" );
    pltit,"Range Comp. Cut "+s,"Range Sample","dB",height=12;
  }
  SEE ALSO: peakAnl
 */
{
  local maxPos, maxVal, fwhm, islr, pkint;

  if (is_void(osf))osf= 10;

  if (typeof(zz0)!="complex")zz0= complex(zz0);
  zz= zz0;
  s= dimsof(zz);
  if (s(1)!=1)error,"peak_anl(zz, &z, &wrkspc_m, &wrkspc_n, & xTaper)";
  m= s(2);
  m2= (m-1)/2;

  n= osf*m;
  n2= (n-1)/2;

  expfac= double(n)/m;

  z= interpol8(zz, n, carrier=carrier);

  if (is_void(pkpos)) {
    pkpos= abs(z)(mxx);
  } else {
    pkpos= osf*(pkpos-1)+1;
  }
  maxPhase= zatan(z(pkpos(1)),deg=deg);

  xx= abs(z)^2;
  x= span(0.0, m-1.0/osf, n);

  peakfit, xx, x, maxPos, maxVal, fwhm, islr, pkint, pkpos=pkpos,\
                 islrtrsh=islrtrsh, islrwidth=islrwidth;
  //  maxPos  ~= (xx(mxx)-1)/expfac;

  // FWHM
  xx= roll(xx, -pkpos(1)+1);        //  max @ Origin

  mxxx= digitize(maxPos,x)-1;
  xx /= maxVal;
  nx= nint(ceil(expfac));
  t= xx(pcen)(dif);

  mnh= min(where(t(nx:0) > 0.))+nx-1;
  if ((nx+1)<=min(mxxx-nx,n-nx-1)) {
    lgc= t(nx+1:min(mxxx-nx,n-nx-1)) < 0.0;
    if (anyof(lgc)) {
      mxh= max(where(lgc))+nx;
    } else {
      mxh= mnh+nx;
    }
  } else {
    mxh= mnh+nx;
  }

  if (mnh >= mxh){
    j= [mnh,mxh];
    i= j(mnx);
    mnh= j(i);
    mxh= j(!(i-1)+1);
    if (mnh==mxh)mnh=2;
    if (mnh <=1)mnh=2;
    if (mxh >=n)mxh=n;
  }

  pslr= -10.*log10(max(xx(mnh:mxh)));

  if (dbg==1)
     write,format="MaxVal= %12lg, MaxPos= %8.3f, FWHM= %6.3f, "+\
       "PSLR= %7.3f, ISLR= %7.3f, pkint= %12lg\n",\
       maxVal, maxPos, fwhm, pslr, islr, pkint;

  return peakAnl(maxPos=maxPos,maxVal=maxVal,maxPhase=maxPhase,fwhm=fwhm,
                    pslr=pslr,islr=islr,pkint=pkint); //phaseGrad=phaseGrad,
}

/*--------------------------------------------------------------------------*/

func peakfit (f, x, &maxpos, &maxval, &fwhm, &islr, &pkint,\
              pkpos=,npoly=, islrtrsh=, islrwidth=, rmsfac=, dbg=)
/* DOCUMENT peakfit(f, x, &maxpos, &maxval, &fwhm, npoly=, islrtrsh=, rmsfac=);

   estimate peak position, value, and width for a real-valued, sampled f_i(x_i)

   in:
      f & x : function/value and positions var (ordinate & abcissa)
              NOTE: f has an implicit period of (x(0)-x(1))+x(dif)(avg)
   out:
      maxpos : estimated position of the polynomial interpolated maxima
      maxval : estimated value of the polynomial interpolated maxima
      fwhm : estimated with of the polynomial interpolated maxima at
             a value "islrtrsh" below maxval (maxval - islrtrsh) (see below)

   keywords:
      noly : number of polynomial coefficients used in fitting the peak [def: 5]
      islrtrsh : f-value factor between maxval-returned and the level at
                   computed width [def: 0.5]
      islrwidth: symmetric x-interval centered on maxpos assumed to main lobe in islr
      rmsfac :
      dbg :  request debug/warning info default is off [def: 0]
  func test{
  require,"dsp.i";
  f= random(60)+random(60)*(1.0/(span(-35,35,60)+1e-2))^2;
  f= interpol8(f, 1000);
  x= span(-20,20,1000);
  peakfit,f,x,dbg=1;}

  func test{
  x= span(-30,30,20000)*pi;dx=x(2)-x(1);f=sinc(x);
  10*log10((ml=integ(f^2,x,3*pi/2)-integ(f^2,x,-3*pi/2))/(integ(f^2,x,x(0))-ml));
  peakfit,f^2,x,dbg=1,islrwidth=3*pi;
  peak_anl(f,islrwidth=3*pi/dx,dbg=1);}

  p= peak_anl(fft(f0*exp(-1i*b*2*pi*roll(indgen(-mm2:mm-1-mm2),-mm2)/double(mm))),dbg=1);

 */
{
  maxpos= maxval= fwhm= islr= pkint= 0.0;

  inpt:

  if (is_void(npoly)) {
    npoly= 7;
  } else if (npoly<3) {
    error,"npoly must >=3";
  }

  fwhmtrsh= 0.5;
  minmaxfrac= 2.4; // enough room to get FWHM, will not work without that dyn range

  if (!is_void(islrtrsh)&&!is_void(islrwidth)) error,"overdef.";
  if (is_void(islrtrsh)&&is_void(islrwidth)) islrtrsh= fwhmtrsh;

  if (is_void(rmsfac))rmsfac= 2;
  if (is_void(dbg))dbg= 0;

  xl= x(0)-x(1);
  dx= x(dif)(avg);
  xp= xl+dx;

  nf= numberof(f);
  favg= f(avg);
  if (is_void(pkpos)) {
    maxidx= f(mxx);
    maxval= f(maxidx);
    ifwm= where(f/maxval>0.5);
    nfwm= numberof(ifwm);
    fbar= (nf*favg-maxval*nfwm)/(nf-nfwm);  // renove box maxval X nfwm from biased f_avg
  } else {           // creep
    maxidx= pkpos;
    do {
      maxval= f(maxidx);
      if (f(min(maxidx+1,nf))>maxval) maxidx= min(maxidx+1,nf);
      if (f(max(maxidx-1,1))>maxval) maxidx= max(maxidx-1,1);
    } while (f(maxidx)>maxval)
    maxval= f(maxidx);
    minidx= maxidx;
    do {
      minval= f(minidx);
      if (f(min(minidx+1,nf))<minval) minidx= min(minidx+1,nf);
      if (f(max(minidx-1,1))<minval) minidx= max(minidx-1,1);
    } while (f(minidx)<minval)
    ifwm= where(f/maxval>0.5);
    ifwm= ifwm(where(abs(ifwm-maxidx)<abs(maxidx-minidx)));
    nfwm= numberof(ifwm);
    fbar= (maxval+minval)/minmaxfrac;  // renove box maxval X nfwm from biased f_avg
  }
  maxpos= maxpos0= x(maxidx);
  if (nf==nfwm) return 1;
  fwhm= (nfwm-1)*dx;

  if (!is_void(islrwidth)) {
    ifwm= where(abs(x-x(maxidx))<islrwidth/2);
  } else {
    ifwm= where(f/maxval>islrtrsh);
  }
  if (numberof(ifwm)==numberof(f)) goto out;

  pkint= sum(f(ifwm));
  islr= 10*log10(pkint/(f(sum)-pkint));

  ip= im= maxidx;
  while (f(cycleIndex(im,1,nf))>fbar && im>-nf) {--im;}
  while (f(cycleIndex(ip,1,nf))>fbar && ip<2*nf) {++ip;}
  it= indgen(im:ip);
  nit= numberof(it);

  ic= cycleIndex(it, 1, nf);
  xfit= x(ic) + xp*((3*nf+(it-1))/nf-3);  //Ouch! risky biz
  ffit= f(ic);

  //ffit-interp(f,x,cycle(xfit,x(1),x(1)+xp));

  //if (abs(maxpos0 - cycle(xfit(ffit(mxx)),x(1),x(1)+xp))>1e-10)error,"max position error";

  xfit -= maxpos0;
  wfit= exp(-sqrt(abs((maxval-ffit)/(rmsfac*ffit))));

  nfit= max([3,min([npoly,nit-2])]);
  if (nfit==3) goto out;
  fitresraph= array(double, nfit);              // !!  variable NAME matters
  fitresraph(1)= 1.0;

  fitpar= lmfit(polyf, xfit, fitresraph, ffit, wfit, deriv=1, tol=1e-4);
  local ffitderiv1, ffitderiv2;
  ffitted= polyf(xfit, fitresraph, ffitderiv2, derivxx=1);
  ffitted= polyf(xfit, fitresraph, ffitderiv1, derivx=1);

  if (dbg==1) {
    fma;
    pltitle,strconcat(swrite(format=" %0.1le,",fitresraph(1:min([nfit,7]))));
    plg,ffitted/maxval,xfit,color="red",type="dashdot";
    plg,ffit/maxval,xfit,type="dot";
    plg,ffit/maxval-ffitted/maxval,xfit,color="cyan";
    plg,wfit,xfit,color="blue",type="dashdot";
  }

  w2= where(ffitderiv2<=0); // can creep right and left from max to tune
  if (is_array(w2)) {
    maxpos2= -fitresraph(2)/(2*fitresraph(3));
    maxval2= polyf(maxpos2, fitresraph);

    polyfraphlev= fwhmtrsh*maxval2;        // !!  variable NAME matters
    btmp= -(fitresraph(2)-2*fitresraph(3)*maxpos2);
    disc= btmp^2-4*polyfraphlev*fitresraph(3);

    maxpos= maxpos0 + maxpos2;

    if (disc<0) {
      if (npoly==3) return 1;
      npoly=3;
      goto inpt;
    }
    fwhm= (btmp-sqrt(disc))/fitresraph(3);

    ip= im= ffit(mxx);
    while (im>1 && ffitderiv2(im-1)<=0 ) {--im;}
    while (ip<nit && ffitderiv2(ip+1)<=0) {++ip;}
    xdmin= xfit(im);
    xdmax= xfit(ip);
    local fdrv;
    fjk= polyf([xdmin,xdmax],fitresraph,fdrv,derivx=1);
    if (fdrv(1)*fdrv(2)>0) {
      if (dbg==1)
         write,"exit null deriv";
      goto out;
    }

    maxpos2= nraphson(polyfraphderiv, xdmin, xdmax, 1.0e-6);
    maxval2= polyf(maxpos2 , fitresraph);
    maxpos= maxpos0 + maxpos2;

    polyfraphlev= fwhmtrsh*maxval2;   // !! var NAME matters - half-way down
    fitresraph(1) -= maxval2;         // max val is ~= 0
    ip= im= ffit(mxx);
    while (im>1 && ffitderiv1(im-1)>=0) {--im;}
    while (ip<nit && ffitderiv1(ip+1)<=0) {++ip;}

    fjk= polyf([xfit(im),maxpos2],fitresraph)+polyfraphlev;
    if (fjk(1)*fjk(2)>0) {
      if (dbg==1)
         write,"exit left",xfit(im), maxpos2;
      goto out;
    }
    xLeft= nraphson(polyfraph,xfit(im),maxpos2, 1.0e-6);

    fjk= polyf( [maxpos2,xfit(ip)], fitresraph)+polyfraphlev;
    if (fjk(1)*fjk(2)>0) {
      if (dbg==1)
         write,"exit right",maxpos2, xfit(ip);
      goto out;
    }
    xRight=  nraphson(polyfraph, maxpos2, xfit(ip), 1.0e-6);

    fwhm= xRight-xLeft;  //maxpos2 in the middle...

    if (!is_void(islrwidth)) {
      xRight= maxpos2+islrwidth/2.0;
      xLeft= maxpos2-islrwidth/2.0;
    } else {
      polyfraphlev= islrtrsh*maxval2;                            // !!  NAME matters
      //fitresraph(1) -= maxval2;
      ip= im= ffit(mxx);
      while(im>1 && ffitderiv1(im-1)>=0 ){--im;}
      while(ip<nit&&ffitderiv1(ip+1)<=0){++ip;}

      fjk= polyf( [xfit(im), maxpos2], fitresraph)+polyfraphlev;
      if (fjk(1)*fjk(2)>0) {
        if (dbg==1)
           write,"exit left",xfit(im), maxpos2;
        goto out;
      }
      xLeft= nraphson(polyfraph, xfit(im), maxpos2, 1.0e-6);

      fjk= polyf( [maxpos2, xfit(ip)], fitresraph)+polyfraphlev;
      if (fjk(1)*fjk(2)>0) {
        if (dbg==1)
           write,"exit right",maxpos2, xfit(ip);
        goto out;
      }
      xRight=  nraphson(polyfraph, maxpos2, xfit(ip), 1.0e-6);
    }
    // write,["left ","right "],[xLeft, xRight];
    // write,[maxval/spline(ffit,xfit, xRight),maxval/spline(ffit,xfit,xLeft)]; // should both be ~0.5

    // ISLR computation
    xRight= cycle(maxpos0 + xRight, x(1), x(1)+xp);
    xLeft= cycle(maxpos0 + xLeft, x(1), x(1)+xp);
    intAll= integ(f, x, x(0));
    intR= integ(f, x, xRight);
    intL= integ(f, x, xLeft);
    if (intL>intR) {
      pkint= intAll-intL+intR;
    } else {
      pkint= intR-intL;
    }
    islr= 10*log10(pkint/(intAll-pkint));
  } else {
    return 1;
  }
  if (dbg==1)
     write,format="MaxVal= %lg, MaxPos= %.3f, FWHM= %.3f, ISLR= %.3f, pkint= %lg\n",\
           maxval, maxpos, fwhm, islr, pkint;
  out:
  return 0;
}

func polyfraph (x, &f, &dfdx)
{
  dfdx= 0.0;
  f= polyf(x, fitresraph, dfdx, derivx=1)+polyfraphlev;
}
func polyfraphderiv (x, &f, &dfdx)
{
  f= 1.0;
  f_dump= polyf(x, fitresraph, f, derivx=1);
  dfdx= 1e-30;
}

/*--------------------------------------------------------------------------*/
func polyfmodpi (x,a,&grad,deriv=,derivx=,derivxx=)
{
  return (pi+polyf(x,a,grad,deriv=deriv,derivx=derivx,derivxx=derivxx))%(2*pi)-pi;
}
/*--------------------------------------------------------------------------*/

func unwrap (y,deg=,intout=,phtresh=)
/* DOCUMENT unwrap(y,deg=,intout=,phtresh=)
            where y == Exp{i*phi} or y= phi (turn deg=1 on if y and output in degrees)
            * wrapping  in first dimension of y
            * intout=1 output is unwrapping phase to be ADDED

 */
{
  if (!is_void(deg) && deg==1)
    y*= pi/180;

  if (is_void(phtresh))
    phtresh= pi;

  if (structof(y)==complex)
    ph= zatan(y);
  else
    ph= y;

  msk= array(short, dimsof(ph(:-1,..)));
  pd= ph(dif,..);
  w= where(pd>phtresh);
  if (numberof(w)>0)
    msk(w)= -1;
  w= where(pd<-phtresh);
  if (numberof(w)>0)
    msk(w)= 1;

  phuw= (intout==1? 0: ph) + msk(cum,..)*2*pi;

  if (deg==1)
    phuw*= 180/pi;

  return phuw;
}

/*--------------------------------------------------------------------------*/

func phase2dpic(z,n1,n2,m1,m2,mm1,mm2,winpic=,xbiname=,ybiname=,legend=,center=,deg=,carrier=){
/* DOCUMENT phase2dpic(z,n1,n2,m1,m2,mm1,mm2,winpic=,xbiname=,ybiname=,legend=)

 */

 dz= dimsof(z);

 if (is_void(m1))m1=20;
 if (is_void(m2))m2=20;
 if (is_void(mm1))mm1=5*m1;
 if (is_void(mm2))mm2=5*m2;

 if (is_void(xbiname))xbiname="x";
 if (is_void(ybiname))ybiname="y";

 if (!is_void(carrier)){
   carrierm= nint(carrier*[m1,m2]);
   carriermm= nint(carrier*[mm1,mm2]);
 }else{
   carrierm= [0,0];
   carriermm= [0,0];
 }

 z1= roll(z(n1-m1/2:n1-m1/2+m1-1,n2-m2/2:n2-m2/2+m2-1),[-m1/2,-m2/2]); //if max @ z(n1,n2) then zzz(1,1) max

 z2= fft(z1,[1,1]);

 if (center==1){
   z3= interpol2d(z1,mm1,mm2,carrier=carrier); //interpolate spectrum

   maxh= (indim(abs(z3)(*)(mxx),dimsof(z3))-1.0)*[m1*1./mm1,m2*1./mm2];maxh;  //displacemenent from  z(n1,n2)

   x= roll(span(-m1/2+carrierm(1),m1-1-m1/2+carrierm(1),m1)/m1,-m1/2+carrierm(1));
   y= roll(span(-m2/2+carrierm(2),m2-1-m2/2+carrierm(2),m2)/m2,-m2/2+carrierm(2));

   phasefix= 2*pi*((y*maxh(2))(-:1:m1,)+(x*maxh(1))(,-:1:m2));

   z2 *= exp(1i*phasefix);
   //zz= interpol2d(zzz,mm1,mm2);
   //maxh= (indim(abs(zz)(*)(mxx),dimsof(zz))-1.0)*[m1*1./mm1,m2*1./mm2];maxh;

 }

 z3= interpol2d(z2,mm1,mm2); //interpolate spectrum

 im= roll(zatan(z3),[mm1/2,mm2/2]-carriermm);

 if (!is_void(winpic)){window,winpic;fma;pli,im,cmin=-pi,cmax=pi;};

//  if (!is_void(winpic))window,winpic;fma;
//  //im= zatan(roll(fft(zzz)),deg=1);
//  im=log(abs(fft(fft(zzz,-1)*,+1)+1e-20);
//  l= span(im(*)(min),im(*)(max),27);
//  if (im(*)(min)==im(*)(max))l=span(im(*)(min)-1,im(*)(min)+1,27);
//  plfc,im,y(-:1:m1,),x(,-:1:m2),levs= l;
//  //plg,im(m1/2,);
//  plot_hbar,plfc_levs, plfc_colors;
//  pltit,"phase spectrum near pix ["+pr1(n1)+","+pr1(n2)+"]" ,xbiname+" bin #",ybiname+" bin #";
//  if (!is_void(legend))plt,legend,0.4,0.17,height=14,font="helvetica",justify="CA";

 return z3;
}

/*--------------------------------------------------------------------------*/

func deramp2d (c,&z1,&z2,fcplx=)
/* DOCUMENT deramp2d (c,fcplx=)
   joimtly deramp complex or fcomplex arrays c(r,a,slc#)
   across each two leading dimension
   z1, z2: mean samp-to-samp phase rotations
   in each dimension
 */
{
  d= dimsof(c);
  if (fcplx==1) {
    if (structof(c)!=float) error,"not fcplx.";
    z1= (Cmul(c(,1:-1,..)*c(,2:,..),conj2=1))(,*)(,avg);
    z2= (Cmul(c(,,1:-1)*c(,,2:),conj2=1))(,*)(,avg);
    z1= z1/max(1,abs(z1(1),z1(2)));
    z2= z2/max(1,abs(z1(2),z2(2)));
    z1= float(atan(z1(2),z1(1)));
    z2= float(atan(z2(2),z2(1)));
    return Cmul(c,Cexp(z1*indgen(d(3))+z2*indgen(d(4))(-,)));
  } else {
    z1= avg(c(1:-1,..)*conj(c(2:,..)));
    z2= avg(c(,1:-1,..)*conj(c(,2:,..)));
    z1= zatan(z1/max(1,abs(z1)));
    z2= zatan(z2/max(1,abs(z2)));
    return c*exp(1i*(z1*indgen(d(2))+z2*indgen(d(3))(-,)));
  }
}

/*-------------------------------------------------------------------------------------*/

func carrierest (a)
/* DOCUMENT dphi/dn/(2*pi) == fn= carrierest(z)
   FN/DT is frequency:  f==fn/dt
   SEE ALSO:
 */
{
  da= dimsof(a);
  out= array(double,da(1));
  off= array(0,da(1));
  b= array(structof(a),da);
  for (i=1;i<=da(1);i++) {
     off(*)= 0;off(i)= 1;
     b= conj(a)*roll(a,-off);
     out(i)= zatan(b(*)(avg))/(2*pi);
  }
  if (da(1)==1)
    out= out(1);
  return out;
}

func ptsim2d (n, m, bwf=, off=, car=, wght=)
/* DOCUMENT c= ptsim2d (n, m, bwf=, off=, car=, wght=)
   [N,M]: dimension lenfgths
   BWF=:  bandwidth *fractions* in ]0.,1.]
   OFF=:  in samples
   CAR=:  carrier, used straight c*= exp(1i*car*indgen(0:n-1));
   WGHT=: a= (1+WGHT)/2 then a+(1-a)*cos(k*res),  ie WGHT is the *constant* pedestal
     
   SEE ALSO:
 */
{
  bwf= [0.,0.]+(is_void(bwf)? 1.0: bwf);
  dm= [n,m];
  dmf= nint(bwf*dm);
  if (anyof(dmf>dm | dmf<=2))
    error,"bandwidth too small.";
  off= [0.,0.]+(is_void(off)? 0.0: off);
  car= [0.,0.]+(is_void(car)? 0.0: car);
  wght= [0.,0.]+(is_void(wght)? 1.0: wght);

  c= windex(hamming(dmf(1),(1+wght(1))/2),dm(1)) *      \
     windex(hamming(dmf(2),(1+wght(2))/2),dm(2))(-,);

  // offset
  if (abs(off(1))>0)
    c*= exp((1i*2*pi/n*off(1))*fftindgen(n));
  if (abs(off(2))>0)
    c*= exp((1i*2*pi/m*off(2))*fftindgen(m))(-,);

  // to time/space
  c= fft(c,-1);

  // carriers
  if (abs(car(1))>0)
    c*= exp((1i*car(1))*indgen(0:n-1));
  if (abs(car(2))>0)
    c*= exp((1i*car(2))*indgen(0:m-1))(-,);

  return c;
}
/*--------------------------------------------------------------------------*/

func peakanl2d (z, rndx, &rpk, &cpk, &fftws_in, &fftws_out, box=, osf=, ambig=, \
                winpic=, winanl=, nexpand=, xbiname=,                   \
                prox=,ybiname=,sunit=,legend=,carrier=,color=,advance=,deg=,scale=,\
                offset=,landscape=,width=,type=,islrwidth=,pkintlev=,pretit=,fcplx=)
/* DOCUMENT peakanl2d(z,n1,n2,m1,m2,mm1,mm2,winpic=,winanl=,
                nexpand=,xbiname=,ybiname=,legend=,carrier=,color=,advance=,deg=,scale=)
  Kinda dumb that this is only [rgpeak,azpeak] .. not everything is separable

  The scale keyword does not affect the requested sampling point(rndx/rpk) or the islrwidth

  (1) grab "z" arround [n1, n2]
  (2) size of "z" patch is of size [m1, m2] unless- /1/ it does not fit or
     /2/ "m1" or "m2" are set as == -1 in which case the wholw "z" row/col/both are grabbed

  peakanl2d(ptsim2d(256,512,off=[128,256.],bwf=.9,wght=.5),[128,256.],winanl=4,advance=1,carrier=0);

  z=roll(fft(fft_dist([2,300,200])<10));
  nx= 300;
  ny= 400;
  dx= 0.43432;
  dy= 1.343254;
  kx= fftindgen(nx)(,-:1:ny)*2*pi/(nx*dx);
  ky= fftindgen(ny)(-:1:nx,)*2*pi/(ny*dy);
  x0= [43.4324, 54.2434];
  z= fft((abs(kx)<0.4*pi/dx & abs(ky)<0.4*pi/dy)*exp(1i*(kx*x0(1)+ky*x0(2))));
  peakanl2d(z,x0/[dx,dy],winanl=4,advance=1,carrier=[0,0],scale=[dx,dy],islrwidth=4*[dx,dy])

  nx=1001;ny=2001;lx= ly=60;
  x= span(-lx/2,lx/2,nx);dx=x(2)-x(1);fx=sinc(pi*x/(3*dx));
  y= span(-ly/2,ly/2,ny);dy=y(2)-y(1);fy=sinc(pi*y/(3*dy));
  z= fx*fy(-,)+1i*0;
  ky= 2*pi/dy/4;
  z*= exp(-1i*ky*y)(-,);
  peakanl2d(z,[nx,ny]/2,winanl=0,advance=1,carrier=[0,-ky*dy/(2*pi)],box=[64,64],islrwidth=4*[3,3])


  TODO carrier detection:
  n=200; m=20;
  x=span(-1,1,n);y=-5*x^2+2*x-2*x^3;
  xm=x(y(mxx));
  y+=random_n(n);
  yy=spline(fitlsq(y, x, (xp=span(-1,1,n/m))),xp,x);xmm=x(yy(mxx));
  [xm,xmm](dif);
  fma;plg,y,x;plg,yy,x,color="red"
 */
{
  // sample Z at long(rndx) - (box-1)/2
  /**/local zz, bx, sf, zmx, cim;
  crq= pksamp2d (z, rndx, rpk, cpk, zz, bx, sf, zmx, cim, fftws_in, fftws_out, \
                 box=box, osf=osf, ambig=ambig, carrier=carrier, prox=prox, fcplx=fcplx);

  if (is_void(crq)) {
    write," out of bounds: pix/img_sz: "+pr1(rndx)+" "+pr1(dimsof(z));
    return array(peakAnl,2);
  }
  kar= carrierest(cim);

  sunit= is_void(sunit)?array(string(0),2):sunit+array(string(0),2);

  dovs= dimsof(zz);    //zz(1) == z(long(rndx)-(bx-1)/2+1) "sf" ovsampd

  m1= bx(1);
  m2= bx(2);
  mm1= dovs(2);  // == bx * sf
  mm2= dovs(3);

  n1= 1+long(rpk(1)+0.5);
  n2= 1+long(rpk(2)+0.5);

  if (is_void(nexpand)) nexpand= 5;
  if (is_void(pretit)) pretit= "";

  if (is_void(scale)) scale= 1.0;
  if (numberof(scale)==1) scale= [scale(1), scale(1)];
  scale= double(scale);

  maxabs = abs(zz(zmx(1),zmx(2)));
  if (maxabs==0)
    return array(peakAnl,2);

  /**/local zzz1, zzz2;
  xpeak = peak_anl(zz(*,zmx(2)),zzz1,osf=nexpand,pkpos=zmx(1),\
             islrwidth=(!is_void(islrwidth)?(islrwidth(1)*double(sf(1))):[]),deg=deg);

  xpeak.maxPosPred= scale(1)*rndx(1);

  xpeak.maxPos= scale(1)*(xpeak.maxPos/sf(1)+long(rndx(1))-(bx(1)-1)/2);

  xpeak.fwhm*= scale(1)/double(sf(1));

  xpeak.pkint*= scale(1)/double(sf(1));

  ypeak = peak_anl(zz(zmx(1),*),zzz2,osf=nexpand,pkpos=zmx(2),\
             islrwidth=(!is_void(islrwidth)?(islrwidth(2)*double(sf(2))):[]),deg=deg);

  ypeak.maxPosPred= scale(2)*rndx(2);

  ypeak.maxPos= scale(2)*(ypeak.maxPos/sf(2)+long(rndx(2))-(bx(2)-1)/2);

  ypeak.fwhm*= scale(2)/double(sf(2));

  ypeak.pkint*= scale(2)/double(sf(2));

  xpeak.maxVal= ypeak.maxVal= abs(cpk);              //sampled NN after osamp. in  pksamp2d

  xpeak.maxPhase= ypeak.maxPhase= zatan(cpk, deg=deg);

  xpeak.fcar= kar(1);
  ypeak.fcar= kar(2);

  azz= zz.re^2+zz.im^2;
  azzm= max(azz);
  if (is_void(pkintlev)) pkintlev= 10.0;
  m= azz/azzm < 10^(-pkintlev/10.0);
  if (anyof(m))
    xpeak.pkint2= sum(azz*m);
  else
    xpeak.pkint2= 0.0;
  azz= [];

  win= [m1,m2];
  offset= (win-1)/2;
  if (!is_void(carrier)) {
    if (structof(carrier)==string){
      carrier= carrierest(cim);
    }
    carrier= carrier%1;
  }
  offset-= nint(carrier*win);

  if (!is_void(winpic)) {
     window,winpic;
     if (landscape) {
       fpanels, 2, 1, xgap=(legend?3.0:2.0), advance = advance, landscape = 1,\
                      bmargin=1.8,tmargin = 1.2,lmargin=1.,rmargin=1.;
       print_format,35;
     }else{
       print_format,40;
       fpanels,1,2,xgap=1,ygap=(legend?3.4:2.4),advance=advance, bmargin=1.0, \
         tmargin = 1.0,lmargin=1.0,rmargin=1.2;
     }
     l = [-30,-25,-20,-15,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,-0.5,-0.4,-0.3,-0.2,-0.1,0]

     x= long(rndx(1))-(bx(1)-1)/2 - rndx(1) + span(0,bx(1),bx(1)*sf(1));
     y= long(rndx(2))-(bx(2)-1)/2 - rndx(2) + span(0,bx(2),bx(2)*sf(2));

     im = 20*log10(abs(zz/maxabs)+1e-34);
     if ((im(*)(avg)-im(*)(rms))<l(min)) l=2*l;
     mxh = im(*)(mxx);
     mxh = indim(mxh,dimsof(im));
     fnext,1;
     plfc,im,y(-:1:mm1,),x(,-:1:mm2),levs = l;
     limits;
     //limits,x(mxh(1))-2.5,x(mxh(1))+2.5,y(mxh(2))-2.5,y(mxh(2))+2.5;
     if (is_void(xbiname)) {
       xbiname1= "x sample #";
     } else {
       xbiname1= xbiname;
     }
     if (is_void(ybiname)) {
       ybiname1= "y sample #";
     } else {
       ybiname1= ybiname;
     }
     if (sunit(1)) xbiname1+= " ["+sunit(1)+"]";
     if (sunit(2)) ybiname1+= " ["+sunit(2)+"]";
     pltit,"dB power: max at ndx ["+pr1(n1)+","+pr1(n2)+"]" ,xbiname1,ybiname1,height=13,font="helveticaB";
     if (!is_void(legend)) plt,legend,0.4,0.17,height=14,font="helveticaB",justify="CA";
     if (landscape)
       plot_hbar,plfc_levs, plfc_colors;
     else
       plot_vbar,plfc_levs, plfc_colors;

     fnext,2;
     // -- ORIG -- im = zatan(zz, deg=1);
     // pliwrap,im,x(min),y(min),x(max),y(max),deg=1;

     // pltit,"Phase (deg)" ,xbiname1,ybiname1,height=13,font="helveticaB";
     // if (!is_void(legend)) plt,legend,0.4,0.17,height=14,font="helveticaB",justify="CA";
     // if (landscape)
     //   plot_hbar,nint(span(-180,180,361)),wrap=360;
     // else
     //   plot_vbar,nint(span(-180,180,361)),wrap=360;
     // -- debug
     im = roll(20*log10(abs(fft(cim)+1e-34)),offset);
     im/= max(im);
     pli,im;
     limits;
     pltit,"IRF Spectrum" ,"RgFreq","AzFreq",height=13,font="helveticaB";
  }

  if (!is_void(winanl)) {

     sepwin= numberof(winanl)==2;

     if (!sepwin) {
       //winkill,winanl;
       window,winanl;
       if (landscape) {
        fpanels, 2, 1, xgap=(legend?3.0:2.0), advance = advance, landscape = 1,\
                      bmargin=1.5,tmargin = 1.5,lmargin=0.6,rmargin=1.;
         print_format,35;
       }else{
         print_format,40;
         fpanels,1,2,xgap=1,ygap=(legend?3.8:2.8),advance=advance;
       }
     }else{
       print_format,40;
     }

     x= long(rndx(1))-(bx(1)-1)/2 - rndx(1) + span(0,bx(1),bx(1)*sf(1)*nexpand);
     y= long(rndx(2))-(bx(2)-1)/2 - rndx(2) + span(0,bx(2),bx(2)*sf(2)*nexpand);

     if (is_void(xbiname)) xbiname="x";
     if (is_void(ybiname)) ybiname="y";
     tit= pretit+[xbiname,ybiname]+" cut: ";

     xbiname=swrite(scale(1),n1,format=xbiname+" sample offset [%g"+sunit(1)+"] (0 ndx %4ld)");
     ybiname=swrite(scale(2),n2,format=ybiname+" sample offset [%g"+sunit(2)+"] (0 ndx %4ld)");

     dat = 20*log10(abs(zzz1/maxabs)+1e-20);
     datptp = dat(rms);
     datm = dat(max);
     if (!sepwin) {fnext;} else {window,winanl(1);if (advance) fma;}
     plg,dat-datm,x,color=color,width=width,type=type;
     plg,zatan(zzz1)*(datptp/5*pi) +( - 2*(datptp/4*pi)), x,type="dot",color=color,width=width;
     if (advance) {
       ppk= swrite(xpeak.maxPos-xpeak.maxPosPred, xpeak.maxVal,\
                   xpeak.maxPhase*(deg==1?1:180/pi), xpeak.fwhm, xpeak.pslr, xpeak.islr,\
       format="\nmx-pos:%4.2f"+sunit(1)+", mx-val:%-#7.2g, mx-phs:%-#4.1fdg,\nfwhm:%4.2f"+sunit(1)+ \
                   ", pslr:%4.2fdB, islr:%4.2fdB" );
       pltit,tit(1)+ppk,xbiname,"dB power",[-0.01,0,0.0],height=13,font="helveticaB";
     }
     limits,,,-60,10;

     if (!is_void(legend)) plt,legend,0.4,(sepwin?0.32:0.51),height=14,font="helveticaB",justify="CA";

     dat = 20*log10(abs(zzz2/maxabs)+1e-20);
     datptp = dat(rms);
     datm = dat(max);
     if (!sepwin) {fnext;}else{window,winanl(2);if (advance)fma;}
     plg,dat-datm,y,color=color,width=width,type=type;
     plg,zatan(zzz2)*(datptp/5*pi) +( - 2*(datptp/4*pi)), y,type="dot",color=color,width=width;
     if (advance) {
       ppk= swrite(ypeak.maxPos-ypeak.maxPosPred, ypeak.maxVal,\
                   ypeak.maxPhase*(deg==1?1:180/pi), ypeak.fwhm, ypeak.pslr, ypeak.islr,\
       format="\nmx-pos:%4.2f"+sunit(2)+", mx-val:%-#7.2g, mx-phs:%-#4.1fdg,\nfwhm:%4.2f"+sunit(2)+ \
                   ", pslr:%4.2fdB, islr:%4.2fdB" );
       pltit,tit(2)+ppk,ybiname,"dB power",[-0.01,0,0.0],height=13,font="helveticaB";
     }
     limits,,,-60,10;

     if (!is_void(legend)&&sepwin) plt,legend,0.4,0.32,height=14,font="helveticaB",justify="CA";

     print_format,-1; //reset
  }

  return [xpeak,ypeak];
}

/*------------------------------------------------------------------------*/

func bicub(x0,y0,z,xmin,xmax,ymin,ymax)
/* DOCUMENT z0=bicub(x0,y0,z,xmin,xmax,ymin,ymax)
  gives the bicubic interpolate of the two dimensional array Z, which is
  an equidistantly sampled (complex) function, at point(s) (X0,Y0). First index
  corresponds to x, second index corresponds to y. XMIN corresponds to z(1,)
  YMAX corresponds to z(,dimsof(z)(3)).  For the sake of speed, the user is
  responsible for not having any points (X0,Y0) in the outermost regions
  and beyond.

  Fast bicubic interpolation routine for equidistantly sampled 2D functions
  Author: Georg Michel <georg@itpgyro1.fzk.de>
  Date: 10/19/97
*/
{
  i0= int((x0-xmin)/(xmax-xmin)*(dimsof(z)(2)-1))+1;
  // for j0 the +1 is omitted because of the indexing scheme for zvec
  j0= int((y0-ymin)/(ymax-ymin)*(dimsof(z)(3)-1));
  t= ((x0-xmin)/(xmax-xmin)*(dimsof(z)(2)-1))%1;
  u= ((y0-ymin)/(ymax-ymin)*(dimsof(z)(3)-1))%1;
  posvec= [t^3*u^3, t^3*u^2, t^3*u, t^3, t^2*u^3, t^2*u^2, t^2*u, t^2,
           t*u^3, t*u^2, t*u, t, u^3, u^2, u, 1];
  fd=dimsof(z)(2);

  a6= 1.0/6;          // .1666666667
  a12= 1.0/12;        // .8333333333e-1
  a36= 1.0/36;        // .2777777778e-1
  a18= 1.0/18;        // .5555555556e-1
  a3= 1.0/3;          // .3333333333
  a9= 1.0/9;          // .1111111111

  zvec= [a36*z(i0-1+ fd*(j0-1))-a12*z(i0+
        fd*(j0-1))+ a12*z(i0+ 1+
        fd*(j0-1))-a36*z(i0+ 2+
        fd*(j0-1))-a12*z(i0-1+ fd*j0)+ .25*z(i0+
        fd*j0)-.25*z(i0+ 1+ fd*j0)+ a12*z(i0+ 2+
        fd*j0)+ a12*z(i0-1+ fd*(j0+ 1))-.25*z(i0+
        fd*(j0+ 1))+ .25*z(i0+ 1+ fd*(j0+
        1))-a12*z(i0+ 2+ fd*(j0+ 1))-a36*z(i0-1+
        fd*(j0+ 2))+ a12*z(i0+ fd*(j0+
        2))-a12*z(i0+ 1+ fd*(j0+ 2))+ a36*z(i0+
        2+ fd*(j0+ 2)), -a12*z(i0-1+ fd*(j0-1))+
        .25*z(i0+ fd*(j0-1))-.25*z(i0+ 1+ fd*(j0-1))+
        a12*z(i0+ 2+ fd*(j0-1))+ a6*z(i0-1+
        fd*j0)-.5*z(i0+ fd*j0)+ .5*z(i0+ 1+
        fd*j0)-a6*z(i0+ 2+ fd*j0)-a12*z(i0-1+
        fd*(j0+ 1))+ .25*z(i0+ fd*(j0+ 1))-.25*z(i0+
        1+ fd*(j0+ 1))+ a12*z(i0+ 2+ fd*(j0+ 1)),
        a18*z(i0-1+ fd*(j0-1))-a6*z(i0+
        fd*(j0-1))+ a6*z(i0+ 1+
        fd*(j0-1))-a18*z(i0+ 2+ fd*(j0-1))+
        a12*z(i0-1+ fd*j0)-.25*z(i0+ fd*j0)+
        .25*z(i0+ 1+ fd*j0)-a12*z(i0+ 2+
        fd*j0)-a6*z(i0-1+ fd*(j0+ 1))+ .5*z(i0+
        fd*(j0+ 1))-.5*z(i0+ 1+ fd*(j0+ 1))+
        a6*z(i0+ 2+ fd*(j0+ 1))+ a36*z(i0-1+
        fd*(j0+ 2))-a12*z(i0+ fd*(j0+ 2))+
        a12*z(i0+ 1+ fd*(j0+ 2))-a36*z(i0+ 2+
        fd*(j0+ 2)), -a6*z(i0-1+ fd*j0)+ .5*z(i0+
        fd*j0)-.5*z(i0+ 1+ fd*j0)+ a6*z(i0+ 2+
        fd*j0), -a12*z(i0-1+ fd*(j0-1))+ a6*z(i0+
        fd*(j0-1))-a12*z(i0+ 1+ fd*(j0-1))+
        .25*z(i0-1+ fd*j0)-.5*z(i0+ fd*j0)+
        .25*z(i0+ 1+ fd*j0)-.25*z(i0-1+ fd*(j0+ 1))+
        .5*z(i0+ fd*(j0+ 1))-.25*z(i0+ 1+ fd*(j0+
        1))+ a12*z(i0-1+ fd*(j0+ 2))-a6*z(i0+
        fd*(j0+ 2))+ a12*z(i0+ 1+ fd*(j0+ 2)),
        .25*z(i0-1+ fd*(j0-1))-.5*z(i0+ fd*(j0-1))+
        .25*z(i0+ 1+ fd*(j0-1))-.5*z(i0-1+ fd*j0)+
        1.*z(i0+ fd*j0)-.5*z(i0+ 1+ fd*j0)+
        .25*z(i0-1+ fd*(j0+ 1))-.5*z(i0+ fd*(j0+ 1))+
        .25*z(i0+ 1+ fd*(j0+ 1)), -a6*z(i0-1+
        fd*(j0-1))+ a3*z(i0+ fd*(j0-1))-a6*z(i0+ 1+
        fd*(j0-1))-.25*z(i0-1+ fd*j0)+ .5*z(i0+
        fd*j0)-.25*z(i0+ 1+ fd*j0)+ .5*z(i0-1+
        fd*(j0+ 1))-1.*z(i0+ fd*(j0+ 1))+ .5*z(i0+ 1+ fd*(j0+
        1))-a12*z(i0-1+ fd*(j0+ 2))+ a6*z(i0+
        fd*(j0+ 2))-a12*z(i0+ 1+ fd*(j0+ 2)),
        .5*z(i0-1+ fd*j0)-1.*z(i0+ fd*j0)+ .5*z(i0+
        1+ fd*j0), a18*z(i0-1+ fd*(j0-1))+
        a12*z(i0+ fd*(j0-1))-a6*z(i0+ 1+
        fd*(j0-1))+ a36*z(i0+ 2+
        fd*(j0-1))-a6*z(i0-1+ fd*j0)-.25*z(i0+
        fd*j0)+ .5*z(i0+ 1+ fd*j0)-a12*z(i0+ 2+
        fd*j0)+ a6*z(i0-1+ fd*(j0+ 1))+ .25*z(i0+
        fd*(j0+ 1))-.5*z(i0+ 1+ fd*(j0+ 1))+
        a12*z(i0+ 2+ fd*(j0+ 1))-a18*z(i0-1+
        fd*(j0+ 2))-a12*z(i0+ fd*(j0+ 2))+
        a6*z(i0+ 1+ fd*(j0+ 2))-a36*z(i0+ 2+
        fd*(j0+ 2)), -a6*z(i0-1+ fd*(j0-1))-.25*z(i0+
        fd*(j0-1))+ .5*z(i0+ 1+
        fd*(j0-1))-a12*z(i0+ 2+ fd*(j0-1))+
        a3*z(i0-1+ fd*j0)+ .5*z(i0+ fd*j0)-1.*z(i0+
        1+ fd*j0)+ a6*z(i0+ 2+ fd*j0)-a6*z(i0-1+
        fd*(j0+ 1))-.25*z(i0+ fd*(j0+ 1))+ .5*z(i0+
        1+ fd*(j0+ 1))-a12*z(i0+ 2+ fd*(j0+ 1)),
        a9*z(i0-1+ fd*(j0-1))+ a6*z(i0+
        fd*(j0-1))-a3*z(i0+ 1+ fd*(j0-1))+
        a18*z(i0+ 2+ fd*(j0-1))+ a6*z(i0-1+
        fd*j0)+ .25*z(i0+ fd*j0)-.5*z(i0+ 1+ fd*j0)+
        a12*z(i0+ 2+ fd*j0)-a3*z(i0-1+ fd*(j0+
        1))-.5*z(i0+ fd*(j0+ 1))+ 1.*z(i0+ 1+ fd*(j0+
        1))-a6*z(i0+ 2+ fd*(j0+ 1))+ a18*z(i0-1+
        fd*(j0+ 2))+ a12*z(i0+ fd*(j0+
        2))-a6*z(i0+ 1+ fd*(j0+ 2))+ a36*z(i0+ 2+
        fd*(j0+ 2)), -a3*z(i0-1+ fd*j0)-.5*z(i0+
        fd*j0)+ 1.*z(i0+ 1+ fd*j0)-a6*z(i0+ 2+ fd*j0),
        -a6*z(i0+ fd*(j0-1))+ .5*z(i0+
        fd*j0)-.5*z(i0+ fd*(j0+ 1))+ a6*z(i0+
        fd*(j0+ 2)), .5*z(i0+ fd*(j0-1))-1.*z(i0+ fd*j0)+
        .5*z(i0+ fd*(j0+ 1)), -a3*z(i0+
        fd*(j0-1))-.5*z(i0+ fd*j0)+ 1.*z(i0+ fd*(j0+
        1))-a6*z(i0+ fd*(j0+ 2)),1.*z(i0+ fd*j0)];

  return((posvec*zvec)(..,sum));
}

/*------------------------------------------------------------------------*/

func digitSig(csig,nbits,iq0ov1){
/* DOCUMENT: digitSig(csig,nbits,iq0ov1)
   returns LONG ("char" it for nbits= 8, for example)
   assumes signal is ~ [-2^(nbits-1),2^(nbits-1)]
 */
 dout= dimsof(csig);
 nlevel= 2^nbits;                          //number of ADC levels
 quant= 1.0;
 offset= quant*(nlevel-1)/2.0;              //dc offset for ADC
 mini= 0;
 maxi= nlevel-1;


 if (iq0ov1){
   return digitize(c.re,span(-0.5,nlevel-1.5,nlevel-1)) - 1;
 }else{
   dout(1) += 1;
   dout= _(dout(1),2,dout(2:0));
   bout= array(long,dout);
   bout(1,..)= digitize(c.re,span(-0.5,nlevel-1.5,nlevel-1)) - 1;
   bout(2,..)= digitize(c.im,span(-0.5,nlevel-1.5,nlevel-1)) - 1;
   return bout;
 }
}

/*------------------------------------------------------------------------*/

func fft_freq (dim)
/* DOCUMENT fft_freq(len)
     Return FFT frequencies along a dimension of length LEN.
   SEE ALSO: indgen, span, fft_dist. */

{
  error,"use fftindgen instead";
  return (u= indgen(0:dim-1)) - dim*(u > dim/2);
}

/*--------------- FFT UTILS---------------*/

func fftindgen (n, rll=, off=, inv=)
/* DOCUMENT ki= fftindgen (n) = (i=indgen(0:n-1)) - n*(i >= (n+1)/2);
   max freq (n odd; ki<0 if n%2) at [n/2+1]

   *** not complete/well-def !!! ***
   func fftindgen_test (n, rll, off, inv) {
     out= [];
     out= _(out,abs(fftindgen(n)(n/2+1))==n/2);
     out= _(out,roll(fftindgen(n))(n/2+1)==0);
     if (rll)
       out= _(out,fftindgen(n,rll=rll)(n/2+1)==0);
     else
       out= _(out,abs(fftindgen(n))(mxx)==(n/2+1));
     if (inv) 
       out= _(out,allof(fftindgen(fftindgen(n,rll=rll,off=off),inv=inv,rll=rll,off=off)==indgen(0:n-1))==1);
     return out;
   }
   for (n=3;n<5;n++)
     for (rll=0;rll<=1;rll++)
       for (off=0;off<=1;off++)
          for (inv=0;inv<=1;inv++)
            fftindgen_test(n, rll, off, inv);
            
   -- largest freq [<0  n odd, neg. one if even] in (roll(fftindgen(n))(1)
   -- roll(fftindgen(n)) == indgen(-n/2:n-1-n/2)

   n=3;   fftindgen(n)==[0,1,-1];    roll(fftindgen(n)) == [-1,0,1]
   n=4;   fftindgen(n)==[0,1,2,-1];  roll(fftindgen(n)) == [2,-1,0,1]
 */
{
  if (is_void(off))
    off= 0;
  if (inv) {
    m= numberof(n);
    if (rll) {
      return n+m/2;
    } else {
      n-= off;
      return n+m*(n<0);
    }
  } else {
    if (rll) {
      return indgen(-n/2:n-1-n/2);
    } else
      return (i=indgen(0:n-1))-n*(i>=(n+1)/2)+off;
  }
}

/*-----------------------------------------------*/

func fftdisambig (n, dt, fest)
/* DOCUMENT f= fftdisambig (n, dt, fest)
   b= 1/dt;
   df= b/n;
   f= fftindgen(n)*df;
   if (!is_void(fest))
     f+= nint((fest-f)*dt)/dt;
   example usage
   -------------
   f= fftdisambig(n,dt,carrierest(z)/dt);
   spectra:
   func pspect (z,n,dt,&fc,color=,marker=,type=)
   {
      dt= is_void(dt)? 1.0: double(dt);
      fcc= carrierest(z)/dt;
      if (!is_void(fc))
        fcc+= nint((fc-fcc)*dt)/dt;
      fc= fcc;
      f= fftdisambig(n,dt,fc);
      fc= f(1);
      nz= numberof(z);
      s= abs(fft(reform(z(*)(:n*(nz/n)),[2,n,nz/n]),[1,0]))(,avg);
      r= -f(mnx)+1;
      s= roll(s,r);
      f= roll(f,r);
      plg,s,f,color=color,marker=marker,type=type;
   }
   SEE ALSO:
*/
{
  dt= is_void(dt)? 1.0: double(dt);
  b= 1/dt;   // bandwith
  df= b/n;   // freq spacing
  f= fftindgen(n)*df;
  if (!is_void(fest))
    f+= nint((fest-f)*dt)*b;
  return f;
}

/*------------------------------------------------------------------------*/

func fft_dist (.., nyquist=, square=, half=)
/* DOCUMENT fft_dist(dimlist);
     Returns Euclidian lenght of spatial frequencies in frequel units for a
     FFT of dimensions DIMLIST.

     If keyword  NYQUIST is true,  the frequel coordinates get  rescaled so
     that the Nyquist frequency is  equal to NYQUIST along every dimension.
     This is obtained by using coordinates:
        (2.0*NYQUIST/DIM(i))*fft_indgen(DIM(i))
     along i-th dimension of lenght DIM(i).

     If  keyword  SQUARE is  true,  the square  of  the  Euclidian norm  is
     returned instead.

     If keyword HALF  is true, the length is only computed  for half of the
     spatial frequencies so that it can be used with a real to complex fft
     forward transform (the first dimension becomes DIM(1)/2 + 1).

   SEE ALSO: fft, fftindgen. */
{
  /* Build dimension list. */
  local  dims;
  while (more_args()) build_dimlist, dims, next_arg();
  dims= dims(2:);
  if (min(dims) <= 0) error, "negative value in dimension list";

  /* Build square radius array one dimension at a time, starting with the
     last dimension. */
  local r2;
  n= numberof(dims);
  if (nyquist) {
    s= 2*nyquist+array(0.0,dimsof(dims));
    for (k=n ; k>=1 ; --k) {
      dim= dims(k);
      u= (s(k)/dim)*(k==1 && half ? indgen(0:dim/2) : fftindgen(dim));
      r2= (k<n ? r2(-,..) + u*u : u*u);
    }
  } else {
    for (k=n ; k>=1 ; --k) {
      u= double((k==1 && half ? indgen(0:dims(k)/2) : fftindgen(dims(k))));
      r2= (k<n ? r2(-,..) + u*u : u*u);
    }
  }
  return (square ? r2 : sqrt(r2));
}
