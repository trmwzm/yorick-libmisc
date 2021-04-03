require, "yut.i";
require, "dsp.i";

func gaupro_1 (m, sig=, expf=, gau=, cutlo=, cuthi=, pad=)
/* DOCUMENT

   cutlo, cuthi: in [0,0.5]
   gau:  gaussian correlation length in sample numbers
   expf: exponential weighting (1/f^expf)

   SEE ALSO:
*/
{
  m0= m;
  m= is_void(pad)? m: m+pad;
  fm= fftindgen(m)*1./m;
  fa= abs(fm);

  s= 1.0;
  if (!is_void(cutlo))
    s= merge2(s,0.,fa>cutlo);
  if (!is_void(cuthi))
    s= merge2(s,0.,fa<cuthi);

  if (!is_void(gau)) {
    c= sqrt(sqrt(2*pi)*gau);
    s= s*c*exp(-(pi*fm*gau)^2);
  }

  if (!is_void(expf)) {
    if (!is_void(gau))
      fa*= gau;
    fa(1)= 1;
    s= s*(2/fa^expf);
    s= s/sqrt(avg(s(2:)^2));
    s(1)= 0;
  }
  fm= fa= [];

  z= (random_n(2,m)*[1.,1i])(sum,)/sqrt(m);
  if (!is_void(sig))
    s= s*sig;
  z= z*s;

  z= fft(z).re;
  return z(:m0);
}

func gaupro_2 (m, n, sig=, expf=, gau=, cutlo=, cuthi=, pad=)
/* DOCUMENT

   cutlo, cuthi: in [0,0.5]
   gau:  gaussian correlation length in sample numbers
   expf: exponential weighting (1/f^expf)

   SEE ALSO:
*/
{
  pad= is_void(pad)? [0,0]: (numberof(pad)==1? [pad(1),pad(1)]: pad);
  m0= m; m+= pad(1);
  n0= n; n+= pad(2);

  fm= fftindgen(m)(,-:1:n)*1./m;
  fn= fftindgen(n)(-:1:m,)*1./n;
  fa= sqrt(fm^2+fn^2);

  s= 1.0;
  if (!is_void(cutlo))
    s= merge2(s,0.,fa>cutlo);
  if (!is_void(cuthi))
    s= merge2(s,0.,fa<cuthi);

  if (!is_void(gau)) {
    gau= numberof(gau)==1? [gau(1),gau(1)]: gau;
    c= sqrt(sqrt(2*pi)*gau(1));
    c= c*sqrt(sqrt(2*pi)*gau(2));
    s= s*c*exp(-pi^2*((fm*gau(1))^2+(fn*gau(2))^2));
  }

  if (!is_void(expf)) {
    if (!is_void(gau))
      fa*= (numberof(gau)==1?gau(1):gau(1)(-:1:m,-)+gau(2)(-,-:1:n));
    fa(1)= 1;
    s= s*(2/fa^(numberof(expf)==1?expf(1):expf(1)(-:1:m,-)+expf(2)(-,-:1:n)));
    s= s/sqrt(avg(s^2));
    s(1)= 0;
  }
  fm= fa= [];

  z= (random_n(2,m,n)*[1.,1i])(sum,..)/sqrt(m*n);
  if (!is_void(sig))
    s= s*sig/s(*)(rms);
  z= z*s;

  z= fft(z).re;
  return z(:m0,:n0);
}

func gauss_rp(n, corrInDn, seed= , zeromean=)
{
  /* DOCUMENT gauss_rp(n, corrInDn, nderiv)
   */
  extern pi;

  if (seed) random_seed, seed;

  nn= fft_good(n);

  h = array(complex,nn);

  pi= 4*atan(1.0);
  sq05= sqrt(0.5);

  dk= 2*pi/nn;
  const = sqrt( sqrt(pi) * corrInDn / n);

  nk= (nn-1)/2;

  h(2:nk+1) = sq05*random_n(nk) + (1i * sq05) *random_n(nk);
  h(1) = random_n([]);
  if (!nsamp%2)
    h(nk+2)= random_n([]);

  kx = indgen(0:nk+1) * dk;

  h(1:nk+2) *= exp(-0.125*(kx*corrInDn)^2) * const;

  if (!is_void(zeromean) && zeromean==1)
    h(1) = 0;

  h(-nk+1:nn) = conj(h(nk+1:2:-1));

  ws = fft_setup(dimsof(h), 1);
  h = fft(h, 1, setup=ws);

  h = h.re(1:n);

  //kx =shift( (findgen(nsamp)-nk)*dk, -nk)
  // h=fft(h,-1,/overwrite)
  // ;slop=zi*fft(h*kx,1)
  // ;curv=-fft(h*kx^2,1)
  //h=fft(h,1,/overwrite)

  return h;
}

func gauss_rs (n, m, corrInDn, corrInDm, seed=)
/* DOCUMENT gauss_rs(n, m, corrInDn, coorInDm, seed=)
 */
{
  if (seed && seed>0 && seed<1) random_seed, seed;            // otherwise randomization ? not ...

  h = array(complex, n, m);

  pi = 4.0 * atan(1.0);
  sq12 = sqrt(0.5);

  dkn = 2.0 * pi / n;
  dkm = 2.0 * pi / m;

  const = sqrt( sqrt(pi) * corrInDn / n);
  const *= sqrt( sqrt(pi) * corrInDm / m);

  nk=(n-1)/2;
  mk=(m-1)/2;

  h(1:nk+1,1:m) = sq12*(random_n(nk+1,m)+1i*random_n(nk+1,m));
  h(1,1) = random_n(1);
  if(!(m%2))h(1,mk+2) = random_n(1);

  kn = (indgen(0:nk) * dkn)(,-:1:m);
  km = roll(span(-mk*dkm,(m-mk-1)*dkm,m),-mk)(-:1:nk+1,);

  h(1:nk+1,) *= exp(-0.125*((kn*corrInDn)^2+(km*corrInDm)^2)) * const;

  h(1) = 0i; // !!! SPECIAL

  h(-nk+1:0,1) = conj(h(2:nk+1,1))(::-1,);
  h([1],-mk+1:0) = conj(h([1],2:mk+1))(,::-1);
  h(-nk+1:0,2:0) = conj(h(2:nk+1,2:0))(::-1,::-1);

  if(!(n%2)){
    kn = (nk+1)*dkn;
    h(nk+2,2:mk+1) = (random_n(mk) + 1i*random_n(mk))*sq12;
    h(nk+2,1) = random_n(1);
    if(!(m%2)){
      h(nk+2,mk+2) = random_n(1);
      km = indgen(mk+2)*dkm;
      h(nk+2,1:mk+2) *= const*exp(-0.125*((kn*corrInDn)^2+(km*corrInDm)^2));
      h([nk+2],-mk+1:0) = conj( h([nk+2],2:mk+1))(,::-1);
    } else {
      km = indgen(mk+1)*dkm;
      h(nk+2,1:mk+1) *= const*exp(-0.125*((kn*corrInDn)^2+(km*corrInDm)^2));
      h([nk+2],-mk+1:0) = conj( h([nk+2],2:mk+1))(,::-1);
    }
  }

  fft_inplace,h, 1, setup=fft_setup(dimsof(h));

  h = h.re;

  return h;
}

func randpoly (nx, np, nd, norm=)
/* DOCUMENT y= randpoly(nx,np[,nd][,norm=])
   polynomial with random coefficients
   Y= [ND x]NX polynomial,  min/max = [-.5,.5]
   NX: number of samples
   NP: polynomial order
   ND: if supplied, first dimension with own random coefs.
   SEE ALSO:
*/
{
  if (norm==1)
    r= random_n(nd,np);
  else
    r= 2*(random(nd,np)-0.5);
  x= span(-1.0,1.0,nx)(-:1:nd,..);
  dx= dimsof(x);
  if (is_void(nd)) {
    dx= [1,nx];
    nd= 1;
    r= r(-,..);
  }
  y= array(double,nd,nx,np);
  y(..,1)= 1.0;
  for (i=2;i<=np;i++)
    y(..,i)= y(..,i-1)*x;
  y= (y*r(,-,))(..,sum);
  c1= y(,max,)(,-,); c0= y(,min,)(,-,);
  ca= (c1+c0)/2; cd= (c1-c0);
  y= reform((y-ca)/cd,dx);
  return y;
}

func randgauss (nx, cor, nd)
/* DOCUMENT y= randgauss(nx,cor[,nd])
   1D gaussian random process with gaussian correlation
   NX: number of samples
   COR: correlation length (?)
   ND: if present, first dim out, ND different realizations
   SEE ALSO:
*/
{
  a= sqrt(cor*pi/2);
  return a*gaussm(random_n(nd,nx),_((nd? 0.: []),cor),fwhm=1);
}

func randgauss2 (nx, ny, corx, cory, nd)
/* DOCUMENT y= randgauss2(nx,ny,corx,cory,[,nd])
   2D gaussian random process with gaussian correlation
   NX: number of samples, first dim
   NY: number of samples, second dim
   CORX: correlation length first dim
   CORY: correlation length second dim
   ND: if present, first dim out, ND different realizations
   SEE ALSO:
*/
{
  a= sqrt(corx*cory)*pi/2;
  return a*gaussm(random_n(nd,nx,ny),_((nd? 0.: []),corx,cory),fwhm=1);
}

/*------------------------------------------------------------------------*/

func mkmo(s1, ds, ns, hp, &poserr, cp =, motgauss =, psfac =, seed =, padlen =,
          err =)
/* DOCUMENT  mkmo (s1, ds, ns, hp, cp=,motgauss=,psfac=,seed=,padlen=)
   MOTGAUSS= [[sig_x, sig_y, sig_z],[corl_x,corl_y,corl_z]];
               sig= 1pt RMS;         corl= 2pt correlation length.
   PSFAC=  [DS_PS/DS_RAD[-],DS_RAD_SIG,DS_RAD_COR_LEN[m]]
   Usage:
   ------
   SEE: test_mo
 */
{
  if (seed)
    random_seed, seed;

  if (is_void(padlen))
    padlen= 0.0;

  expf= 0.7;

  if (!is_void(psfac)) {
    dsv= ds/psfac(1);
    nsv= long(psfac(1)*ns+2*long(padlen/dsv+0.5));
    if (numberof(psfac)>1) {
      if (numberof(psfac)!=3)
        error,"expecting [DS_PS/DS_RADAR[-],DS_SIG,DS_COR_LEN[m]]";
      else
        dsv+= gaupro_1(nsv-1,gau=psfac(3)/dsv,expf=expf,sig=psfac(2));
      dsv*= ((ns-1)*ds+2*padlen)/sum(dsv);
      sv= s1 - padlen + dsv(cum);
    } else {
      sv= span(s1-padlen,s1+(ns-1)*ds+padlen,nsv);
    }
  } else {
    sv= s1+indgen(0:ns-1)*ds;
  }
  nsv= numberof(sv);
  dsv= sv(dif)(avg);

  antxyz= array(double,[2,3,nsv]);

  antxyz(1,)= sv;
  antxyz(2,)= 0.0;
  antxyz(3,)= hp;

  if (!is_void(motgauss)) {
    for (ic=1; ic<=3; ic++) {
      if (allof(motgauss(ic,)))
        antxyz(ic,..)+= gaupro_1(nsv,gau=motgauss(ic,2)/dsv, \
                                   expf=expf,sig=motgauss(ic,1));
    }
  }

  antxyz(2,..)-= antxyz(2,avg);
  antxyz(3,..)-= (antxyz(3,avg)-hp);

  if (!is_void(err)) {
    poserr= array(double,[2,3,nsv]);
    for (ic=1; ic<=3; ic++) {
      if (allof(err(ic,)))
        poserr(ic,..)+= gaupro_1(nsv,gau=err(ic,2)/dsv,        \
                                 expf=expf,sig=err(ic,1));
    }
  }
  if (!is_void(cp))
    antxyz(2,..)+= cp;

  return antxyz;
}

func mkbat (s1, s0, la, &s, ypaav=, ypagau=, psfac=, seed=, padlen=, esa=)
/* DOCUMENT ypa= mkbat (s1, s0, la, ypaav=, ypagau=, psfac=, seed=, padlen=, esa=)
            -OR-
            ypa= mkbat (s, ypaav=, ypagau=, seed=, esa=)
   *first* form S is computed using Nyquist LA and PSFAC
   *second form* S is an input

   PSFAC=  [DS_PS/DS_RAD[-],DS_RAD_SIG,DS_RAD_COR_LEN[m]]
   YPAGAU= [[sig_y, sig_p, sig_r],[corl_y,corl_p,corl_r]];
             sig= 1pt RMS;         corl= 2pt correlation length.

   Usage:
   ------
   ac= mkbat(...seed=.123, \
     ypagau=[[1.,4.,4.],[0.,700.,500]], \
     psfac=[1.5,.1,2000.],padlen=100);
 */
{
  if (seed)
    random_seed, seed;

  expf= 0.7;

  if (!is_void(s0) && !is_void(la)) {
    if (is_void(padlen))
      padlen= 0.0;

    lanq= la/2;  // Nyquist sampling
    nsa= long((s0-s1)/lanq);

    if (!is_void(psfac)) {
      dsv= lanq/psfac(1);
      nsv= long(psfac(1)*nsa+2*long(padlen/dsv+0.5));
      if (numberof(psfac)>1) {
        if (numberof(psfac)!=3)
          error,"expecting [DS_PS/DS_RADAR[-],DS_SIG,DS_COR_LEN[m]]";
        else
          dsv+= gaupro_1(nsv-1,gau=psfac(3)/dsv,expf=expf,sig=psfac(2));
        dsv*= ((nsa-1)*lanq+2*padlen)/sum(dsv);
        s= s1 - padlen + dsv(cum);
      } else {
        s= span(s1-padlen,s1+(nsa-1)*lanq+padlen,nsv);
      }
    } else {
      s= s1+indgen(0:nsa-1)*lanq;
    }
  } else {
    s= s1;
  }
  ns= numberof(s);
  dsav= s(dif)(avg);              // averages sampling point spacing

  ypaav= is_void(ypaav)? [0.,0,0]: ypaav;
  if (numberof(ypaav)<3)
    ypaav= _(ypaav,array(0.0,3-numberof(ypaav)));
  ypa= array(ypaav,ns);
  if (!is_void(ypagau) && dimsof(ypagau)(1)>1) {
    for (ic=1; ic<=numberof(ypagau(,1)); ic++) {
      if (allof(ypagau(ic,)))
        ypa(ic,..)+= gaupro_1(ns,gau=ypagau(ic,2),expf=expf,sig=ypagau(ic,1));
    }
  }
  if (esa==1)
    ypa(3,)= steer([1,0,0],ypa);  // additional args: cql, ls0 (look angle, target ls)

  return ypa;
}
