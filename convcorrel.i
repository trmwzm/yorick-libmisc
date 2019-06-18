require, "yut.i";         //embedarr extractarr

func correl2d ( a, b, &mx, &work, &nt, srch=, off=)
/* DOCUMENT correl(a,b,srch=,off=)
  off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
  cross correlation of b with a, 
  -  dims do not have to be id., but
  the offset is == 0 for id. starting pix. [1,1]
  -  if an offset is specified, it will translate the output
  ie. the full offset will be above expression
 
  b may have an extra dimension if called as 
      c= correl2d(b,[a],srch=srch,off=off);

  usage example: -------
  a= random(n,m);
  a(n/2,m/2)= 1;
  b= img_pad(a,n+10,m+20,just=0);
  if (anyof(off)) b= roll(b,off);
  c= correl2d(a,b,srch=srch,off=off);
  off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
 */
{
  dd= dimsof(a(1,1,..),b(1,1,..));
  nconv= dd(1)==1? dd(2): 0;

  if (is_void(off)) 
    off= array(0,2+nconv>1);
  else
    if (nconv>0)
      off= _(off+[0,0],0);

  s0= 5;
  if (is_void(srch)) 
    srch= nconv? [s0,s0,0]: [s0,s0];
  else
    if (nconv>0)
      srch= _(srch+[0,0],0);
  
  db= dimsof(b)
  nb= db(2:);

  n0= nb-srch/2-off;
  n1= nb-1-srch/2+srch-off;
  if (nconv>0)
    n1(0)= nconv;

  //return convoln(a,b(::-1,::-1),n0=nb-srch/2-off,n1=nb-1-srch/2+srch-off);
  c= convcorreln(a,b,work,nt,n0=n0,n1=n1,correl=1);

  mx= off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
  return c;
}

func convcorreln (a, b, &work, &nt, n0=, n1=, zc=, correl=)
/* DOCUMENT convcorreln(a, b, &work, &nt, n0=, n1=, zc=, correl=)

     returns convolution of array A with array B.  This is naturally
     of length na+nb-1 where na=length of A, nb=length of B.  However,
     convoln returns a result the same size as A, which is extracted
     from the center of this full array.  Typically, B will be a much
     smaller array than A, which you are using to smooth A.  If the
     dimensions of B are odd, then the elements of the returned array
     will be centered as you would expect.

     In detail, for i=[1 to na+nb-1]
       result(i) = sum j=[max(1,1+i-nb) to min(na,i)] (A(j)*B(1+i-j))
     with this operation repeated along each dimension of A and B.

     The n0= and n1= keywords can be used to control the section of
     the full array that is actually returned, 1<=n0<n1<=na+nb-1.
     Both n0 and n1 must be vectors with length equal to the number
     of dimensions of A and B.  By default,
       n0 = (1+nb)/2
       n1 = na + n0 - 1
     which gives the expected centering when nb is odd.  If nb is
     even, you might want to consider returning a result of length
     na-1 which is "zone centered" relative to A.  You can do this
     by setting the zc=1 keyword, which gives n0 = 1 + nb/2 and
     n1 = n1 = na + n0 - 2 when nb is even.

     Note that if some dimension of B is length 1, there will be
     no smoothing along that dimension of A.  The pseudo-index -
     is useful for generating such dimensions.  For example, if x
     is a 2D array and y is a 1D smoothing function which you want
     to apply to the second dimension of x, use convoln(x, y(-,)).

   SEE ALSO: convol, gaussm, boxcar
 */
{
  typo= structof(a(1)*b(1));  
  na= dimsof(a);
  nb= dimsof(b);
  ndims= nb(1);
  if (na(1) != ndims)
    error, "a and b must have same numberof of dimensions";
  na= na(2:);
  nb= nb(2:);
  nc= na+nb-1;
  nt= array(ndims, 1+ndims);
  for (i=1 ; i<=ndims ; ++i)
    nt(1+i)= fft_good(nc(i));
  at= bt= array(complex, nt);
  nt= nt(2:);

  at= embedarr(a,at);
  bt= embedarr(b,bt,(correl?-nb+1:0),wrap=1);

  if (is_void(work))
    work= fft_setup(dimsof(at));
  fft_inplace, at, 1, setup=work;
  fft_inplace, bt, 1, setup=work;
  at= (correl?conj(bt):bt)*at;
  bt= [];
  fft_inplace, at, -1, setup=work;

  if (is_void(n0))
    n0= (1+nb)/2;
  if (is_void(n1))
    n1= na + n0 - 1;
  if (numberof(n0)!=ndims || numberof(n1)!=ndims)
    error, "n0=, n1= keywords must have length = number of dimensions";
  if (zc)
    n0 += (1+nb)%2;

  nt= numberof(at);
  at= extractarr(array(complex,_(ndims,n1-n0+1)),at,n0-1)/nt;
  if (typo==int||typo==long) {
    at= double(at)+0.5;
    return typo(at)-(at<0);
  } else
    return typo(at);
}

#if 0
func correl2d_sum (a, b, &mx, srch=, off=)
/* DOCUMENT c= correl(a, b, srch=, off=)
  off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
  cross correlation of b with a, 
  -  dims do not have to be id., but
  the offset is == 0 for id. starting pix. [1,1]
  -  if an offset is specified, it will translate the output
  ie. the full offset will be above expression
  usage example: -------
  n= m= 200;
  mx= [];
  a= random_n(n,m);
  b= img_pad(a,n+10,m+20,just=0);
  if (anyof(off)) b= roll(b,off);
  c= correl2d_sum(a,b,mx,srch=srch,off=off);
 */
{
  off= [0,0]+(is_void(off)? 0: off);
  srch= [0,0]+(is_void(srch)? 5: srch);

  db= dimsof(b)
  da= dimsof(a)
  dmx= max(da,db)+_(0,abs(off)+srch/2+1);

  bp= embedarr(b,array(double,dmx)); //0-padded b
  ao= off+[1,0]*indgen(-srch(1)/2:srch(1)-srch(1)/2-1)(-,..)+\
          [0,1]*indgen(-srch(2)/2:srch(2)-srch(2)/2-1)(-,-,..);

  bb= extractarr(da,bp,ao,wrap=1);

  c= (a*bb)(sum,sum,..);

  mx= off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
  return c;
}
#endif

#if 1
func test_correl (n, m, &w, srch=, off=, nit=, plot=)
{
  if (is_void(n))
    n= 200;
  if (is_void(m))
    m= n+100;
  if (is_void(off))
    off= [0,0];
  off+= [0,0];
  if (is_void(srch))
    srch= 3;
  srch+= [0,0];

  // A
  a= random(n,m);
  a(n/2,m/2)= 1;

  // B is integer shifted A
  b= embedarr(a,array(structof(a),n+10,m+20));
  if (anyof(off))
    b= roll(b,off)+0.1*roll(b,off-1);

  // time init
  t0= t= t1= t2= array(0.0,3);
  timer, t; t0= t;

  local mx;

  // // sum
  // if (is_void(nit))
  //   nit= 1;
  // for (i=1;i<=nit;i++)
  //   c= correl2d_sum(a,b,mx,srch=srch,off=off);
  // timer, t,t1;

  // fft
  for (i=1;i<=nit;i++)
    c= correl2d(a,b,mx,w,srch=srch,off=off);
  timer, t,t2;
  timer_print, "sum  ",t1,"fft   ",t2;

  // m= abs(c)>0.;
  // avg(m*abs(c-c2)/max(1e-30,c));

  if (plot) {
    window,0;fma;pli,c;
    // window,1;fma;pli,c2;
  }
  return c;
}
#endif

#if 0
// older implementation
func convcorrel (a, b, &work, &nt, n0=, n1=, correl=, wrap=)
/* DOCUMENT convcorrel(a,b,&work,&nt,n0=,n1=,correl=,wrap=)

     returns convolution of vector a with vector b, a vector
     of length na+nb-1 where na=numberof(a), nb=numberof(b).

     In detail, for i=[1 to na+nb-1]
       result(i) = sum j=[max(1,1+i-nb) to min(na,i)] (a(j)*b(1+i-j))

     The n0= and n1= keywords can be used to control the section of
     the full array that is actually returned, 1<=n0<n1<=na+nb-1.

     Use the convoln function for multi-dimensional convolution, and
     see gaussm and boxcar for smoothing array data.
     
     if wrap==1, there is no padding and the convolution wraps
     since there is no padding beware of making sure that the larger
     inout array is sized according to "fft_good" for optimization

   SEE ALSO: fft_good, fft, convoln, gaussm, boxcar
 */
{
  typo= structof(a(1)*b(1));  
  na= numberof(a);
  nb= numberof(b);
  if (wrap==1) {
    nc= max(na,nb);
    nt= nc;
  } else {
    nc= na+nb-1;
    nt= fft_good(nc);
  }
  at= bt= array(complex, nt);
  at(1:na)= a;
  if (correl==1)
    bt(-nb+1:)= b;
  else
    bt(1:nb)= b;
  
  if (is_void(work))
    work= fft_setup(dimsof(at));
  fft_inplace, at, 1, setup=work;
  fft_inplace, bt, 1, setup=work;
  c= (correl?conj(bt):bt)*at;
  fft_inplace, c, -1, setup=work;
  
  if (is_void(n0))
    n0= 1;
  if (is_void(n1))
    n1= nc;
  c= c(n0:n1)/nt;
  if (typo==int || typo==long) {
    c= double(c)+0.5;
    return typo(c)-(c<0);
  } else
    return typo(c);
}
#endif
