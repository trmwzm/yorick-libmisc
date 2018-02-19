require, "dsp.i";

/* adapted from  Éric Thiébaut */

func density(s, x, bw=, adjust=, verb=, kernel=, histo=, exact=,
             nsamples=, debug=)
/* DOCUMENT density(s, x)

     This function estimates the density of 1-D data S at positions X.
     Argument S gives the sampled data, argument X gives the position where to
     estimate the density.  The density is computed by convolving the sample
     density by a smoothing kernel with a bandwidth adapted to the sampled
     data.

     Set keyword HISTO true to compute an histogram rather than a (normalized)
     density.

     Keyword KERNEL can be used to specify the smoothing kernel to use.  The
     value of this keyword can be one of:

       "uniform"            "biweight"           "cosine"
       "triangle"           "triweight"          "cubbspline"
       "epanechnikov"       "normal"             "quadbspline"

     Case does not matter and the few first letters of the kernel name can be
     used as far as the abbreviated name is unique.  By default,
     Epanechnikov's kernel is used (which is theoretically the most
     efficient).

     Keyword BW can be used to specify the bandwidth.  If unspecified, the
     bandwidth is computed according to a rule that is optimal for Gaussian
     distributed data. If keyword VERB is true, the value of the bandwidth
     automatically computed is printed.  The theoretical optimal bandwidth is:

        BW = rms(S)/numberof(S)^0.2

     where rms(S) is the standard deviation of the sampled data.

     Keyword ADJUST can be used to tune the bandwidth, the actual bandwidth is
     ADJUST*BW. By default, ADJUST = 1.

     Keyword EXACT can be set true to use a very slow but exact convolution.
     By default, an approximate convolution is computed by means of FFT (fast
     Fourier transform).  For this fast computation, the number of samples can
     be set with keyword NSAMPLES (by default, NSAMPLES=1024) to tune the
     precision.


   EXAMPLE:
     include, "random.i";
     s = 3.2 + 2.1*random_n(200);
     x = span(-10.0, 10.0, 100);
     plg, density(s, x), x;


   REFERENCES
     [1] D. W. Scott, "Multivariate density estimation: theory, practice, and
         visualization," John Wiley & Sons (New York), 1992.

   SEE ALSO: hist.
 */
{
  // FIXME: there should be a special case for integer valued data

  /* Select the kernel (default is "Epanechnikov"). */
  if (is_void(kernel)) {
    kernel = 3;
  }
  if (is_obj(DENSITY_KERNEL,noop(kernel),1)<0 && is_string(kernel)) {
    m= strmatch(DENSITY_KERNEL(*,),kernel);
    if (anyof(m)) {
      w= where(m);
      if (numberof(w)>1)
        error, "kernel match not unique.";
      else
        ok= DENSITY_KERNEL(w(1));
    } else
      error,"no matching kernel: "+kernel;
  }
  ok= DENSITY_KERNEL(noop(kernel));

  /* Compute bandwidth. */
  ndata = numberof(s);
  if (is_void(bw)) {
    sigma = s(*)(rms);
    bw = sigma/ndata^0.2;
    if (verb) write, format="estimated bandwidth = %g\n", bw;
  }
  if (! is_void(adjust)) {
    bw *= adjust;
  }
  h = ok(bw)*bw;
  q = 1.0/(histo ? h : h*ndata);

  if (exact) {
    /* Compute density by convolving the data samples with the selected
       kernel. */
  slow:
    f = ok(fc);
    pdf = array(double, dimsof(x));
    n = numberof(x);
    for (i = 1; i <= n; ++i) {
      pdf(i) = q*f(h, s, x(i));
    }
    return pdf;
  }

  /* Compute an histogram HST on a regular grid using linear interpolation of
     data samples. */
  if (is_void(nsamples)) {
    nsamples = 1024;
  } else {
    nsamples = fft_good(nsamples);
  }
  xmin = min(x);
  xmax = max(x);
  if (xmin == xmax) {
    /* Use "slow" computation. */
    goto slow;
  }
  kernel_width = ok(w);
  smin = xmin - h*kernel_width;
  smax = xmax + h*kernel_width;
  s = s(where((s >= smin)&(s <= smax)));
  if (is_void(s)) return array(double, dimsof(x));
  smid = (smax + smin)/2.0;
  imid = (nsamples + 1)/2.0;
  step = (smax - smin)/(nsamples - 1);
  xs = step*(indgen(nsamples) - imid) + smid; // positions of samples
  u = (1.0/step)*(s - smid) + imid;
  v = floor(u);
  u -= v;
  i = long(v);
  v = [];
  hst = (histogram(i, 1.0 - u, top=nsamples) +
         histogram(i + 1, u, top=nsamples+1)(:-1));

  /* Convolve histogram HST with selected kernel using FFT. */
  ker = ok(f);
  ker = ker((step/h)*fftindgen(nsamples));
  if (h < 3.0*step) write, format="WARNING - %s\n",
                      "use more samples with keyword NSAMPLES in density() function";
  if (verb) write, format="refinement factor = %g\n", h/step;
  if (is_func(fftw_plan)) {
    forward = fftw_plan(nsamples, 1, real=1);
    backward = fftw_plan(nsamples, -1, real=1);
    ys = fftw(fftw(hst, forward)*fftw(ker, forward), backward);
  } else {
    ws = fft_setup([1,nsamples]);
    ys = double(fft(fft(hst, +1, setup=ws)*fft(ker, +1, setup=ws), -1, setup=ws));
  }
  if (debug) {
    plg, hst, xs, color="green";
    plg, (1.0/(h*nsamples*sum(ker)))*ys, xs, color="red";
    fft_plg, ker, color="blue", scale = step/h;
  }

  /* Finally interpolate sampled density. */
  return (q/nsamples)*interp(ys, xs, x);
}

// FIXME: [-1,1]?
func _density_kernel_uniform(t)
{
  t = abs(t);
  return (t < 0.5) + 0.5*(t == 0.5);
}

func _density_kernel_convolve_uniform(h, t, t0)
{
  return double(numberof(where(abs(t - t0) < h/2.0)));
}

func _density_kernel_triangle(t)
{
  return max(1.0 - abs(t), 0.0);
}

func _density_kernel_convolve_triangle(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    return sum(1.0 - (1.0/h)*u(i));
  }
  return 0.0;
}

func _density_kernel_Epanechnikov(t)
{
  return 0.75*max(0.0, 1.0 - t*t);
}

func _density_kernel_convolve_Epanechnikov(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    return 0.75*sum(1.0 - u*u);
  }
  return 0.0;
}

func _density_kernel_biweight(t)
{
  t = max(0.0, 1.0 - t*t);
  return 0.9375*t*t;
}

func _density_kernel_convolve_biweight(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    u = 1.0 - u*u;
    return 0.9375*sum(u*u);
  }
  return 0.0;
}

func _density_kernel_triweight(t)
{
  t = max(0.0, 1.0 - t*t);
  return 1.09375*t*t*t;
}

func _density_kernel_convolve_triweight(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    u = (1.0/h)*u(i);
    u = 1.0 - u*u;
    return 1.09375*sum(u*u*u);
  }
  return 0.0;
}

func _density_kernel_normal(t)
{
  return (1.0/sqrt(2*pi))*exp(-0.5*t*t);
}

func _density_kernel_convolve_normal(h, t, t0)
{
  u = t - t0;
  return sum(exp(-(0.5/(h*h))*u*u))/sqrt(2.0*pi);
}

func _density_kernel_cosine(t)
{
  k = array(double, dimsof(t));
  j = where(abs(t) < 1.0);
  if (is_array(j)) {
    k(j) = (pi/4.0)*cos((pi/2.0)*t(j));
  }
  return k;
}

func _density_kernel_convolve_cosine(h, t, t0)
{
  u = abs(t - t0);
  if (is_array((i = where(u < h)))) {
    return (pi/4)*sum(cos((pi/(2*h))*u(i)));
  }
  return 0.0;
}

func _density_kernel_quadratic_B_spline(t)
{
  k = array(double, dimsof(t));
  t = abs(t);
  if (is_array((i = where(t < 1.5)))) {
    t = t(i);
    test = (t <= 0.5);
    if (is_array((j = where(test)))) {
      u = t(j);
      k(i(j)) = 0.75 - u*u;
    }
    if (is_array((j = where(! test)))) {
      u = 1.5 - t(j);
      k(i(j)) = 0.5*u*u;
    }
  }
  return k;
}

func _density_kernel_convolve_quadratic_B_spline(h, t, t0)
{
  s = 0.0;
  u = abs(t - t0);
  if (is_array((i = where(u < 1.5*h)))) {
    u = (1.0/h)*u(i);
    test = (u <= 0.5);
    if (is_array((i = where(test)))) {
      v = u(i);
      s += 0.75*numberof(v) - sum(v*v);
    }
    if (is_array((i = where(! test)))) {
      v = 1.5 - u(i);
      s += 0.5*sum(v*v);
    }
  }
  return s;
}

func _density_kernel_cubic_B_spline(t)
{
  k = array(double, dimsof(t));
  t = abs(t);
  if ((is_array((i = where(t < 2.0))))) {
    test = ((t = t(i)) <= 1.0);
    if (is_array((j = where(test)))) {
      u = t(j);
      k(i(j)) = (0.5*u - 1.0)*u*u + (2.0/3.0);
    }
    if (is_array((j = where(! test)))) {
      u = 2.0 - t(j);
       k(i(j)) = (1.0/6.0)*u*u*u;
    }
  }
  return k;
}

func _density_kernel_convolve_cubic_B_spline(h, t, t0)
{
  s = 0.0;
  t = abs(t - t0);
  if ((is_array((j = where(t < 2.0*h))))) {
    q = 1.0/h;
    test = ((t = t(j)) <= h);
    if (is_array((i = where(test)))) {
      u = q*t(i);
      s = sum((0.5*u - 1.0)*u*u + (2.0/3.0));
    }
    if (is_array((i = where(! test)))) {
      u = 2.0 - q*t(i);
      s += (1.0/6.0)*sum(u*u*u);
    }
  }
  return s;
}

func density_test(s, x, adjust=)
{
  if (is_void(s)) {
    s = random_n(300);
  }
  if (is_void(x)) {
    t = (max(s) - min(s))*1E-2;
    x = span(min(s) - t, max(s) + t, 500);
  }
  n = DENSITY_KERNEL(*);
  colors = ["DeepSkyBlue","DarkOrange","SpringGreen","DarkRed",
            "DarkViolet","MediumBlue","Goldenrod","DarkKhaki",
            "SaddleBrown", "ForestGreen","DarkCyan"];
  for (i = 1; i <= n; ++i) {
    color = colors(i);
    kernel = DENSITY_KERNEL(*,i);
    plg, density(s, x, kernel=kernel, adjust=adjust), x, color=pl_get_color(color);
    write, format="%20s -> %s kernel\n", color, kernel;
  }
}

scratch= save(uniform,triangle,epanechnikov,biweight,triweight,normal, \
                     cosine,cubbspline,quadbspline);

DENSITY_KERNEL= save();

uniform= o= save();
save,o,f= _density_kernel_uniform;
save,o,fc= _density_kernel_convolve_uniform;
save,o,bw= 3.68622; /* =  uniform */
save,o,w= 1.0;
save,DENSITY_KERNEL,uniform;

triangle= o= save();
save,o,f= _density_kernel_triangle;
save,o,fc= _density_kernel_convolve_triangle;
save,o,bw= 2.57603; /* = (2^12*pi)^0.1  triangle */
save,o,w= 2.0;
save,DENSITY_KERNEL,triangle;

epanechnikov= o= save();
save,o,f= _density_kernel_Epanechnikov;
save,o,fc= _density_kernel_convolve_Epanechnikov;
save,o,bw= 2.34491; /* = (40*sqrt(pi))^0.2   Epanechnikov */
save,o,w= 2.0;
save,DENSITY_KERNEL,epanechnikov;

biweight= o= save();
save,o,f= _density_kernel_biweight;
save,o,fc= _density_kernel_convolve_biweight;
save,o,bw= 2.77794; /* =  biweight */
save,o,w= 2.0;
save,DENSITY_KERNEL,biweight;

triweight= o= save();
save,o,f= _density_kernel_triweight;
save,o,fc= _density_kernel_convolve_triweight;
save,o,bw= 3.15448; /* =  triweight */
save,o,w= 2.0;
save,DENSITY_KERNEL,triweight;

normal= o= save();
save,o,f= _density_kernel_normal
save,o,fc= _density_kernel_convolve_normal;
save,o,bw= 1.05922; /* = (4.0/3.0)^0.2  normal */
save,o,w= 75.2807;
save,DENSITY_KERNEL,normal;

cosine= o= save();
save,o,f= _density_kernel_cosine;
save,o,fc= _density_kernel_convolve_cosine;
save,o,bw= 2.40971; /* = (pi^13/36.0/(pi^2 - 8.0)^4)^0.1 cosine */
save,o,w= 2.0;
save,DENSITY_KERNEL,cosine;

cubbspline= o= save();
save,o,f= _density_kernel_quadratic_B_spline;
save,o,fc= _density_kernel_convolve_quadratic_B_spline;
save,o,bw= 1.82764; /* = (1208.0/105.0*sqrt(pi))^0.2 cubic B-spline */
save,o,w= 3.0;
save,DENSITY_KERNEL,cubbspline;

quadbspline= o= save();
save,o,f= _density_kernel_cubic_B_spline;
save,o,fc= _density_kernel_convolve_cubic_B_spline;
save,o,bw= 2.10768; /* quadratic B-spline */
save,o,w= 4.0;
save,DENSITY_KERNEL,quadbspline;
/* for a cubic B-spline, I found empirically:
 *   BW ~ 1.9 * sigma / n^(1/5)
 *
 * theoretical computation yields (see [1] p. 131, Theorem 6.1):
 *   BW ~ 1.82764 * sigma / n^(1/5)
 */
restore, scratch;