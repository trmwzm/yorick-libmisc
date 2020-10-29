require,"yut.i";
require,"dsp.i";

func wigner ( x, fs, nfreq, &t, &f , froll=)
/* DOCUMENT wigner( x, fs, nfreq, &t, &f )
   wigner -- Compute samples of the (type I) Wigner distribution.

   Usage
     tfd = wigner1(x, fs, nfreq, t, f)

   Inputs
     x      signal vector.  wigner1 assumes that x is sampled at the Nyquist
 	          rate and uses sinc interpolation to oversample by a factor of 2.
     fs     sampling frequency of x (optional, default is 1 sample/second)
     nfreq  number of samples to compute in the frequency direction, must
 	           be at least 2*length(x) (optional, defaults to 2*length(x))
     froll Ts=  set to 1 to center the null frequency in the second
   Outputs
     tfd  matrix containing the Wigner distribution of signal x.  If x has
 	       length N, then tfd will be 2N by 2N. (optional)
     t    vector of sampling times (optional)
     f    vector of frequency values (optional)

  If no output arguments are specified, then the Wigner distribution is
  displayed using ptfd(tfd, t, f).
 */
{
  typex = typeof(x);
  nx = dimsof(x)(2);
  nx2 = 2*nx;

  if(typex!="complex")x = typeconv("complex", x);

  // input check and, if necessary, assignment of default values
  if(is_void(nfreq))nfreq = nx2;
  if(is_void(fs))fs = 1.0;

  if(nfreq < nx2)error,"nfreq must be at least 2*length(x)";

  acf = lacf(x);		//[nx, nx2] ==  [mlag, nt]

  //negative tau
  info,acf;
  acf = grow(acf,array(complex, nx2, nfreq-nx2+1),conj(acf(..,nx:2:-1)));
  wig = double(fft(acf,[0,1]))/nx2;
  nw = dimsof(wig)(2);
  if(froll==1)wig = roll(wig,[0,-(nw-1)/2]);

  t = 1/(2*fs) * indgen(0:nx2-1);
  f = -fs/2+(fs/nx2)*indgen(0:nx2-1);

  return wig;
}

/*-------------------------------------------------------------------------*/

func choi_williams( x, &t, &f , fs=,  sigma=, froll=){
/* DOCUMENT choi_williams( x, fs, nfreq, &t, &f )
  wigner -- Compute samples of the (type I) Choi_Williams distribution.

   Usage
     tfd = choi_williams( x, fs, sigma, &t, &f , froll=)

   Inputs
     x      signal vector.  wigner1 assumes that x is sampled at the Nyquist
 	    rate and uses sinc interpolation to oversample by a factor of 2.
     fs     sampling frequency of x (optional, default is 1 sample/second)
     sigma spread of the kernel in the ambiguity plane (optional, defaults
           to 1e4)
     froll=  set to 1 to center the null frequency in the second
   Outputs
     tfd  matrix containing the Wigner distribution of signal x.  If x has
 	  length N, then tfd will be 2N by 2N. (optional)
     t    vector of sampling times (optional)
     f    vector of frequency values (optional)

  If no output arguments are specified, then the Wigner distribution is
  displayed using ptfd(tfd, t, f).
 */

 typex = typeof(x);
 nx = dimsof(x)(2);
 nx2 = 2*nx;

 if(typex!="complex")x = typeconv("complex", x);

 // input check and, if necessary, assignment of default values
 if(is_void(sigma))sigma = .2;
 if(is_void(fs))fs = 1.0;

 w = wigner(x, froll=1);

 w = fft(w,[1,1]);

 dk = 1./(sigma*nx);
 rp = (roll(span(-nx*dk,(nx2-nx-1)*dk,nx2),-nx))^2;
 ker = exp(-rp(-,)*rp(,-));

 w = double(fft(w*ker,[-1,-1]));

 t = 1/(2*fs) * indgen(0:nx2-1);
 f = -fs/2+(fs/nx2)*indgen(0:nx2-1);

 return w;
}

/*-------------------------------------------------------------------------*/

func lacf(x, mlag, type=){
/*DOCUMENT lacf(x, mlag, type=)
  Usage
    lacf = lacf(x, mlag, type=)

  Inputs
    x	   signal vector.  lacf1 assumes that x is sampled at the Nyquist
  	   rate and uses sinc interpolation to oversample by a factor of 2.
    mlag   maximum lag to compute.  must be <= length(x).
  	   (optional, defaults to length(x))
    type   type of acf wanted (1==Ddefault:continuous&aperiodic; 2== C1&periodic)

  Outputs
    lacf  matrix containing the local acf of signal x.  If x has
  	  length N, then lacf will be 2N by mlag. Since lacf is symmetric,
  	  it is only computed for positive lags.
 */

 typex = typeof(x);
 if(typex!="complex")x = typeconv("complex", x);
 nx = dimsof(x)(2);
 nx2 = 2*nx;

 // input check and, if necessary, assignment of default values
 if(is_void(mlag))mlag = nx;
 if(is_void(type))type = 1;

 if(type == 1 ){
   x = interpol8(x, nx2);
   y = array(complex, nx2, mlag);
   for(t=1; t<=nx2; t++){
     mtau = min(t, nx2-t+1);
     mtau = min(mtau, mlag);
     y(t, 1:mtau) = x(t:t+mtau-1) * conj(x(t:t-mtau+1:-1));
   }
 }else if(type==2){
   error,"need to write some code?!";
 }
 return typeconv(typex, y);
}

/*--------------------------------------------------------------*/

func tfd( x, fa, sigma, threshold, &min_t, &max_t ){
/*
 TFD Choi-Williams- and Wigner-Ville time-frequency distributions.

   USAGE:    y=tfd(x,fa,sigma,threshold)
 	     y=tfd(x,fa,sigma)
 	     y=tfd(x,fa)
 	     y=tfd(x)
 	     y=tfd(,,,,min_t, max_t)

 	     The default values for the non-defined parameters are

 	     fa        = round(dimsof(x)(2)/2)
 	     sigma     = "inf" == infty (i.e. a Wigner-Ville distribution)
 	     threshold = 0.001


   INPUT:
    x	       Complex input signal (column- or row vector). If signal is
 	       real, use hilbert(x).
    fa         length of frequency axis. (Default value: half the
 	       signal nx.)
    sigma      Degree of smoothing. The _smaller_ sigma, the _more_
 	       the crossterms are suppressed in the Choi-Williams
 	       distribution. Nice value to start with: sigma=1.
 	       If sigma increases the result tends to a Wigner-Ville
 	       distribution. If sigma="inf" (default) then an exact
 	       Wigner-Ville distribution is returned.
    threshold  Minimum weightfactor (>0). In the Choi-Williams tfd the
 	       exponential weighting function in expands to
 	       infinity. However terms that are multiplied with a very low
 	       weighting do add to the computation time, but do not add
 	       significantly to the final result. Using this parameter,
 	       only terms that have a weighting larger than "threshold" add
 	       to the result.

   OUTPUT:
    y	  -  Contains the distribution. Each row represents
 	     a frequency, each column a time instant.
    min_t -  first time-instant of distribution
    max_t -  final time-instant of distribution


  EXAMPLE 1:
  n = 40;x=exp(2*pi*1i*6*span(1./n,1.,n)^2);	  // Make a chirp
  fa=20; sigma="inf";			  //
  y=tfd(x,fa,sigma,,min_t, max_t);	  // Make a WVD
  fma;pli,abs(y);
  plg,(12.*indgen(min_t:max_t))/n,indgen(min_t:max_t) // Plot instantaneous frequency
 					  //
  EXAMPLE 2:				  //
  figure(1)				  //
  x=exp(2*pi*1i*6.*(indgen(1:80)/80.)^2)+	  // Two 'crossing' chirps
    exp(2*pi*1i*indgen(1:80)/80*(50-30*indgen(1:80)/80);
  fa=40; sigma="inf";			  //
  y=tfd(x,fa,sigma,,min_t, max_t);	  // Make a WVD
  contour((min_t:max_t),(1:fa), flipud(y))// Contour plot of WVD
  figure(2);sigma=1;			  //
  (y min_t max_t)=tfd(x,fa,sigma);	  // Make a CWD
  contour((min_t:max_t),(1:fa), flipud(y))// Contour plot of CWD
  hold on				  //
  plot((min_t:max_t),12*(min_t:max_t)/80) // Plot instantaneous frequencies
  plot((min_t:max_t),(50-60*(min_t:max_t)/80))
  hold off
 */

 typex = typeof(x);
 nx = dimsof(x)(2);
 n2 = (nx-1)/2;

 // input check and, if necessary, assignment of default values
 if(is_void(sigma))sigma = "inf";
 if(is_void(fa))fa = nx/2;
 if(is_void(threshold))threshold = 0.001

 if (threshold <= 0){
   error,"parameter \"threshold\" is chosen less than or equal to zero. ";
   disp("using threshold="+num2str(def_threshold)+" instead.");
   threshold = def_threshold;
 }

 // in the next line we determine the size of the
 // frequency window we will really use: window_use. (it should be uneven)
 // eg. if fa equals 8 then window_use becomes 7 and
 // if fa equals 7 then window_use also becomes 7.
 window_use = 2*nint(floor((fa-1)/2))+1;

 // calculate the number of timepoints at which the distribution will be
 // evaluated:
 nx_cwd = nx - window_use + 1;

 // initialize cwd:
 cwd = array(double, nx_cwd, fa);

 if(sigma != "inf"){
   rat = -log(threshold)/sigma;
 }else{
   rat = 0.
 }
 // calculate the maximum size of the windows over which we smooth:
 wm = 2*nint(floor((window_use-1)*sqrt(rat)));

 // calculate for each tau and mu what the weights are:
 wgts = array(double, (window_use-1)/2+1, wm+1);
 mu = indgen(-wm/2:wm/2);
 for(tau=0;tau<=(window_use-1)/2;tau++) {
   if ((tau != 0) && (sigma != "inf")){
     wgts(tau+1,1:wm+1) = exp(-mu*mu*sigma/(4*tau*tau));
   }else{
     wgts(tau+1,1:wm+1) = mu < 1;
   }
 }

 // n is the time variable:
 min_t = (window_use+1)/2;
 max_t = nx - (window_use-1)/2;
 for (n=min_t;n<=max_t;n++){
   // tau runs over the right part of window_use:
   for (tau=0;tau<=(window_use-1)/2;tau++){
     // summation over the second window to smooth the distribution:
     wm_tau = 2*nint(floor(2*tau*sqrt(rat)));
     mu_begin = -min(wm_tau/2,-1+min(n+tau,n-tau));
     mu_end   = min(wm_tau/2,nx-max(n+tau,n-tau));

     //compute, then scale the weights:
     weights = wgts(tau+1,mu_begin+wm/2+1:mu_end+wm/2+1);
     weights = weights/sum(weights);

     // and calculate the distribution:
     x1 = x(n+tau+mu_begin:n+tau+mu_end);
     x2 = x(n-tau+mu_begin:n-tau+mu_end);

     cwd(n-(window_use+1)/2+1,tau+1) = sum(weights*x1*conj(x2));
   }
 }

 // because the numbers in the cwd must be hermitic as a function
 // of tau, we can conjugate the found values of the cwd and copy
 // them to the other half of the cwd.
 if((fa%2)==0){
   // if fa is even then write a zero in the cwd:
   cwd(n-(window_use+1)/2+1,fa/2+1) = 0;
   for (tau=fa/2+2;tau<=fa;tau++){
     cwd(..,tau) = conj(cwd(..,fa+2-tau));
   }
 }else{
   for(tau=(fa+1)/2+1;tau<=fa;tau++){
     cwd(..,tau) = conj(cwd(..,fa+2-tau));
   }
 }

 // and calculate the one dimensional simultaneous ft:
 y = double(fft(cwd,[0,1]));

 return y;
}
