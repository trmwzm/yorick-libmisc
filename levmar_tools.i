require, "dsp.i";

func realo2ar (&o, &out, &j, membi=)
/* DOCUMENT flatten struct (oxy) of ONLY float arrays into a 1D array
   usage ---
   o= save(pi,e=exp(1),r=random(2,3),pi2=[2*pi]);
   or= realo2ar(o);    // o --> array
   or(1)= 0;
   oo= o(:);           // copy object
   realo2ar, oo, or;   // array --> o (o is preexisting!)
   oo(pi);

   MEMBI= returns an array of member indices in storage order
   o(*,realo2ar(o,membi=1));
   ATTN: membi works "simply" only for flat structure (no obj(obj))
   SEE ALSO:
*/
{
  if (is_void(out)) {
    dar= [];
    for (i=1;i<=o(*);i++){
      oi= o(noop(i));
      if (is_real(oi))
        if (membi==1)
          dar= _(dar,array(i,numberof(oi)));
        else
          dar= _(dar,reform(double(oi),numberof(oi)));
      else if (is_obj(oi))
        dar= _(dar,realo2ar(oi));
      else
        error,"incorrect type.";
    }
    return dar;
  } else if (is_numerical(out)) {
    if (is_void(j))
      j= 1;
    for (i=1;i<=o(*);i++){
      oi= o(noop(i));
      if (is_real(oi) && (n=numberof(oi))) {
        oi= reform(out(j:(j+=n-1)),dimsof(oi));
        save,o,noop(i),oi;
        j+= 1;
      } else if (is_obj)
        realo2ar,oi,out,j;
      else
        error,"incorrect input type.";
    }
  } else
    error,"incorrect input type.";
}

func polyfun(x, a, &grad)
/* DOCUMENT

 */
{
  da= dimsof(a);
  if (da(1)<1) {
    y= array(a(1),dimsof(x));
    if (grad==1)
      grad= array(1.0,dimsof(x));
    return y;
  }

  np= da(2);
  dx= dimsof(x);
  dy= grow(dx,np); dy(1) += 1;
  y= array(structof(x), dy);
  y(..,1)= 1.;
  for (i=2;i<=np;i++)
    y(..,i)= y(..,i-1)*x;

  if (grad==1)
    grad= y;

  return y(..,+)*a(+,..);
}

// chebsinc_lm closure
scratch= save(scratch, tmp);
tmp= save(eval_, fitparr);
func chebsinc_lm (base, void, lanta=, lam=, f0=)
{
  ob= base(:);

  if (is_void(lanta))
    lanta= 1.7;

  sol= 299792458.0;      // [m/s]
  f0= is_void(f0)? 430e6: f0;
  if (is_void(lam))
    lam= sol/f0;

  save,ob, lanta,lam;
  return closure(ob,eval_);
}
func fitparr (ncheb=, nline=, nbit=)
{
  if (is_void(ncheb))
    ncheb= 10;
  if (is_void(nline))
    nline= 0;
  if (is_void(nbit))
    nbit= 0;

  pop= save();
  save,pop, u0= 0.0;  // az boreisight
  save,pop, laf= 1.0; // az antenna length factor
  save,pop, sincexp= 2.0; // az sinc exponent 1-way power
  if (ncheb>0)
    save,pop, cheb= _(2.,(ncheb>1? array(0.,ncheb-1): []));
  if (nline>0)
    save,pop, linefac= array(1.0,nline);
  if (nbit>0)
    save,pop, bitp= _(1.,(nbit>1? array(0.,nbit-1): []));

  return pop;
}
func  eval_ (x, a, &dfda, u=, v=, lini=, bit=, keepdim=)
/* DOCUMENT
   x contains: ncheb, nline, nbit,
   u, v,
   chbint,
   lini (if nline>0),
   bit (if nbit>0)

   This is confusing Y= F(X, A, &DFDA) !?
   since closures are ok: what constants (non-fitable model vars)
   go in F and/or X
   let's try to stack'em
   SEE ALSO:
*/
{
  local ncheb, nline, nbit, cheb, linefac, bitp;

  //stack all constant paramaters
  fx= save([],use(),[],x);

  ncheb= fx(ncheb);
  nline= fx(nline);
  nbit= fx(nbit);

  // extract model specialization from X input
  pop= use_method(fitparr,ncheb=ncheb,nline=nline,nbit=nbit);
  // grab parameter names
  if (numberof(realo2ar(pop))!=numberof(a))
    error,"incorrect number of parameter X and A.";
  realo2ar,pop,a;

  // indices of derivatives
  popndx= use_method(fitparr,ncheb=ncheb,nline=nline,nbit=nbit);
  realo2ar,popndx,indgen(numberof(a));

  if (is_void(u))
    u= fx(u);
  if (is_void(v))
    v= fx(v);
  di= dimsof(u,v);
  zo= array(0.0,di);
  u+= zo;
  v+= zo;

  if (is_void(lini) && nline>0)
    lini= fx(lini);
  if (!is_void(lini))
    lini+= zo;
  if (is_void(bit) && nbit>0)
    bit= fx(bit);  // voltage
  if (!is_void(bit))
    bit+= zo;

  // AZ sinc factor
  snc= abs(sinc(pop(laf)*pi*fx(lanta)/fx(lam)*(u-pop(u0))));
  sncp= snc^pop(sincexp);

  // EL cheby
  if (ncheb>1)
    chb= cheby_eval(_(fx(chbint),pop(cheb)),v);
  else
    chb= 1.0;

  // line-number dependent factor
  if (nline>0)
    facl= pop(linefac)(lini);
  else
    facl= 1.0;

  // bit polynomial factor
  dbfac= dfda;
  if (nbit>1)
    facb= polyfun(bit,pop(bitp),dbfac);
  else
    facb= 1.0;

  // put factors together
  fac= facl*facb;
  y= fac*sncp*chb;

  // derivatives
  if (dfda==1) {
    dfda= array(0.0, _(di(1)+1,di(2:),numberof(a)));
    dsnc= dsincdx(pop(laf)*pi*fx(lanta)/fx(lam)*(u-pop(u0)));
    dsncp= pop(sincexp)*dsnc*snc^(pop(sincexp)-1);
    ndx= long(popndx(laf));
    dfda(..,ndx)= fac*dsncp*chb*pi*fx(lanta)/fx(lam)*(u-pop(u0));
    ndx= long(popndx(u0));
    dfda(..,ndx)= -fac*dsncp*chb*pop(laf)*pi*fx(lanta)/fx(lam);
    ndx= long(popndx(sincexp));
    dfda(..,ndx)= y*log(snc);
    if (ncheb>0) {
      ndx= long(popndx(cheb));
      for (i=1;i<=ncheb;i++) {
        cf= array(0.0,ncheb);
        cf(i)= 1.0;
        dfda(..,ndx(i))= cheby_eval(_(fx(chbint),cf),v);
      }
      dfda(..,ndx)*= fac*sncp;
    }
    if (nline>0) {
      ndx= long(popndx(linefac));
      for (i=1;i<=nline;i++) {
        m= lini==i;
        if(anyof(m))
          dfda(where(m),ndx(i))= 1.0;
      }
      dfda(..,ndx)*= facb*sncp*chb;
    }
    if (nbit>0) {
      ndx= long(popndx(bitp));
      dfda(..,ndx)= dbfac*facl*sncp*chb;
    }
    if (keepdim!=1)
      dfda= dfda(*,);
  }
  if (keepdim==1)
    return y;
  else
    return y(*);
}
chebsinc_lm= closure(chebsinc_lm, restore(tmp));
restore, scratch;

func test (&dg, n=, m=, ncheb=, nline=, nbit=)
{
  if (is_void(n))
    n= 100;
  if (is_void(m))
    m= n;
  if (is_void(ncheb))
     ncheb= 10;
  if (is_void(nline))
     nline= 0;
  if (is_void(nbit))
     nbit= 0;

  d= [2,m,n];
  chbint= [-.4,.4];

  x= save();
  save,x, ncheb, nline, nbit, chbint;
  save,x, v=span(chbint(1),chbint(2),d(2))(,-:1:d(3));
  save,x, u=span(-.3,.3,d(3))(-:1:d(2),);
  if (nbit>0)
    save,x, bit= array(1.,d);
  if (nline>0)
    save,x, lini= array(1,d);

  flm= chebsinc_lm();

  p0= flm.function(fitparr,ncheb=ncheb,nline=nline,nbit=nbit);

  a0= realo2ar(p0);

  xx= span(x(chbint)(1),x(chbint)(2),200);
  b= cheby_fit(exp(-4*(xx-x(chbint)(avg))^2/x(chbint)(dif)(1)^2),xx,ncheb-1);
  w= where(realo2ar(p0,membi=1)==p0(*,"cheb"));
  a0(w)= b(3:); // cut out bndr

  info,a0;
  // eval
  dg= 1;
  g= flm(x,a0,dg,keepdim=1);

  // checking partials
  nfit= numberof(a0);
  eps= 1e-3;
  inc= eps*abs(g)/avg(g);
  dg2= array(double,dimsof(dg));
  for (i=1; i<=nfit; i++) {
    a= a0;    // Copy current parameters
    a(i)+= inc(i);
    dg2(..,i)= (flm(x,a,keepdim=1)-g)/inc(i);
    d= (dg(..,i)-dg2(..,i))/abs(dg(..,i));
    fma;pli,d;
    pltitle_height=12;
    pltitle,pr1(i)+" "+pr1(statarr(d)(:-1));
    q=rdline(prompt="enter (q quits) ");
    if (q=="q")
      break;
  }

  return g;
}


#if 0
//a = levmar1(y, x, f, a0, avar, acovar)
// levmar1 skips autodetection and assumes
// Y= F(X, A ,&dfda)
// if you can return analytic dfda
//     if levmar will use dfda, dfda=1 on input
//     if levmar will not use dfda, dfda=[] on input
// In all cases, dfda is defined as:
// dfda(i,j) = partial[Y(i)] / partial[A(j)]

// looking at Y= F(X,A,&DFDA)
// design to put all immutable code/data in F (closure)
// and all code/data which is likely to change from
// regression to regression in X.
// In X also should be the init and end A0/A
// and the mutable A structure: A= realo2ar(OA), read



scratch= save(scratch, tmp);
tmp= save(eval_);
func lmf (base, par)
{
  ob= base(:);
  save,ob, par;
  return closure(ob,eval_);
}
func  eval_ (x, a, &dfda)
{
  dfda1= dfda2= dfda;
  f1= polyfun(x,use(par),dfda1);
  f2= polyfun(x,a,dfda2);
  f= f1*f2;
  if (dfda==1)
    dfda= dfda1*f2+f1*dfda2;
  //if (dfda==1)
  //  dfda= somefuncpartial(x,a,use(par));
  return f;
}
lmf= closure(lmf, restore(tmp));
restore, scratch;


//outputs:
tmp= save(levmar_chi2,levmar_chi20,levmar_lambda,levmar_neval);
//levmar_chi2;             //final value of chi2
//levmar_chi20;            //initial value of chi2 (for a0)
//levmar_lambda;           //final value of lambda
//levmar_neval;            //number of calls to F

//inputs:
levmar_itmax = 100;      //maximum number of gradient recalculations
levmar_tol = 1.e-7;      //stop when chi2 changes by less than this
levmar_lambda0 = 0.001;  //initial value of lambda
levmar_lambda1 = 1.e12;  //maximum permitted value of lambda
levmar_gain = 10.0;      //factor by which to change lambda
//levmar_aabs, levmar_arel, levmar_ada -- see levmar_partial

// test# 1
n= m= 200;
sig= 0.8;
x= random_n(n);
y= x^2+sig*(random(n)-0.5);
a0= [0.,0.,0.];
a= levmar1(y,x,polyfun,a0); // fit=, amin=, amax, wgt= [dims Y], lu=1 (replaces SVD)

xx= span(-3,3,m);
window,0;
fma;
plmk,y,x,msize=.3,marker=1;
plg,polyfun(xx,a),xx,color="red";
pltit,pr1(a0)+" to "+pr1(a)+" vs. "+pr1(aa),"X","Y";

// test# 2
n= m= 200;
sig= 0.8;
x= random_n(n);
aa= random(3)-0.5;
bb= random(3)-0.5;
f= lmf(bb);
y= f(x,aa)+sig*(random(n)-0.5);
a0= [0.,0.,0.];
a= levmar1(y,x,f,a0); // fit=, amin=, amax, wgt= [dims Y], lu=1 (replaces SVD)

xx= span(-3,3,m);
window,1;
fma;
plmk,y,x,msize=.3,marker=1;
plg,f(xx,a),xx,color="red";
pltit,pr1(a0)+" to "+pr1(a)+" vs. "+pr1(aa),"X","Y";

#endif
