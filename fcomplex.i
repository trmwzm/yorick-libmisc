/* fairly idiotic code to test ideas on how to deal
   with the lack of a "float complex" type in yorick.
   The "add_fcomplex_type" to binary file is directly
   from D. Munro. Essentially, best pracice is: don't
   do it. Use add_fcomplex_type to read float complex
   binary raw data as double complex. If that is too
   big for memory: then rewite the code, and if that
   is not an option read and hold as:
   array(float,[nd+1,2,n-1,...,n-nd])
   
   Four (4) complex number representations are used in
   the type casting stuff below. Internal names:
   ZD: yorick intrinsic (double) complex type
   ZF: yorick structure with float {Re, Im}
   CD: double array, first/fast dim. is length==2
   CF: float  array, first/fast dim. is length==2
   ... of cource is_complex and other intrinsics
   break on these type extensions.
 */
struct fcomplex
/* DOCUMENT struct fcomplex
    float re;
    float im;
  The ONLY purpose of this is testing IO performance
  compared to float[2,...] and fcomplex primitive
  type substitution for binary files.
  SEE ALSO:
 */
{
  float re;
  float im;
};

scratch= save(scratch, tmp);
tmp= save( cplx_zd_zd, cplx_zd_zf, cplx_zd_cd, cplx_zd_cf);
save, tmp, cplx_zf_zd, cplx_cd_zd, cplx_cf_zd;
save, tmp,typeq,typnm;
func _cplxcast_ (base,typeout,typein,z)
/* DOCUMENT _cplxcast_ (typeout,typein,z)
*/
{
  mnm= base(*,);
  m= strgrepm("cplx_[z,c][f,d]_[z,c][f,d]",mnm);
  ob= base(where(!m));
  w= where(m);
  ftbl= base(noop(w));

  save,ob,ftbl;

  return ob; // eval_ is an obj member, see closure/restore below
}
func cplx_zd_zd (zd)
{
  return zd;
}
func cplx_zd_zf (zf)
{
  z= array(complex,dimsof(zf));
  z.re= zf.re;
  z.im= zf.im;
  return z;
}
func cplx_zd_cd (cd)
{
  z= array(complex,dimsof(cd(1,..)));
  z.re= cd(1,..);
  z.im= cd(2,..);
  return z;
}
func cplx_zd_cf (cf)
{
  z= array(complex,dimsof(cf(1,..)));
  z.re= cf(1,..);
  z.im= cf(2,..);
  return z;
}
func cplx_zf_zd (zd)
{
  z= array(fcomplex,dimsof(zd));
  z.re= zd.re;
  z.im= zd.im;
  return z;
}
func cplx_cd_zd (zd)
{
  d= dimsof(zd);
  z= array(double,_(1+d(1),2,(d(1)>0? d(2:): [])));
  z(1,..)= zd.re;
  z(2,..)= zd.im;
  return z;
}
func cplx_cf_zd (zd)
{
  d= dimsof(zd);
  z= array(float,_(1+d(1),2,(d(1)>0? d(2:): [])));
  z(1,..)= zd.re;
  z(2,..)= zd.im;
  return z;
}
func  typeq (typ1,typ2)
{
  if (typ1==typ2 || nameof(typ1)==nameof(typ2))
    return 1;
  else
    return 0;
}
func  typnm (typ)
{
  if (typ==double)
    return "cd";
  else if (typ==float)
    return "cf";
  else if (typ==complex)
    return "zd";
  else if (nameof(typ)=="fcomplex")
    return "zf";
}
_cplxcast_= closure(_cplxcast_, restore(tmp));
restore, scratch;

local cplxcast_ext;
cplxcast_ext= _cplxcast_();

func complexcast (typo,z)
/* DOCUMENT complexcast (typo,z)
   Promote or demote type without warning.

   use
   ---
     zo= complexcast(typo,zi);
     tx0= complexcast(tx,[0.,0.]);

   SEE ALSO:
 */
{
  extern cplxcast_ext;
  if (!is_struct(typo))
    error,"output type required: complexcast(typout,z)";
  if (is_void(z))
    return void;
  cast= cplxcast_ext;
  typi= structof(z);
  d= dimsof(z);
  if (is_real(z) && (d(1)<1 || d(2)!=2))
    error,"need two components.";
  if (cast(typeq,typi,typo))
    return z;
  else {
    tni= cast(typnm,typi);
    tno= cast(typnm,typo);
    zi= cast(ftbl,"cplx_zd_"+tni,z);
    return cast(ftbl,"cplx_"+tno+"_zd",zi);
  }
}

func add_fcomplex_type(f,filemode)
/* DOCUMENT fo= add_fcomplex_type(fi,filemode)
   adds a *modified* complex binary file primitive
   with elements which are of type float instead of
   double.  This is meant for cases where the factor
   of 2 of heap size expansion is not a show stopper.
   Otherwise revert to floats [nd+1,2,n_1,..,n_nd]
   use:
   ---
   z=random_n(100,100)+1i*random_n(100,100);
   f=add_fcomplex_type("q","wb+")
   add_variable,f,-1,"x",structof(z),dimsof(z)
   f.x= z;
   close,f;
   sizeof(z);
   sizeof(open("q","rb",1));
   f= add_fcomplex_type("q","rb+");
   z(*)= 0;
   _read,f,0,z;
   sizeof(z);

   SEE ALSO:
 */
{
  if (is_string(f))
    f= open(f,(!is_void(filemode)? filemode: "wb"));
  else if (is_stream(f))
    if (!is_void(filemode))
      error,"filemode allowed for bin stream";
  else if (typeof(f)=="text_stream")
    error,"f type is string filename or binary stream";

  pri= get_primitives(f);

  install_struct,f,"double",pri(13),pri(14),pri(15),pri(19:25);
  // if(allof(pri(6:19:3)==-1))
  //   install_struct,f,"double",4,4,-1,[0,1,8,9,23,0,0x7f];
  // else
  //   install_struct,f,"double",4,4, 1,[0,1,8,9,23,0,0x7f];

  save, f, complex;

  return f;
}

func Cabs(x)
{
  tx= structof(x);
  sx= nameof(tx);
  if (tx==complex)
    return abs(x);
  else if (sx=="fcomplex")
    return abs(x.re,x.im);
  else if (dimsof(x)(2)==2)
    return tx(abs(x(1,..),x(2,..)));
  else
    error,"unexpected type";
}

func Cconj(x)
{
  tx= structof(x);
  sx= nameof(tx);
  if (tx==complex)
    return tx(re=x.re,im=-x.im);
  else if (sx=="fcomplex")
    return tx(re=x.re,im=-x.im);
  else
    return x*[1.f,-1.f];
}

func Caxpy(a,x,y)
/* DOCUMENT func Caxpy(a,x,y)
   reurn A * X + Y
   SEE ALSO:
 */
{
  extern cplxcast_ext;
  cast= cplxcast_ext;

  tx= structof(x);
  if (is_void(y))
    y= complexcast(tx,[0.,0.]);
  ty= structof(y);

  if (!cast(typeq,tx,ty))
    error,"no implicit type cast: tx!=ty";

  if (cast(typnm,tx)=="zf") {
    z= complexcast(float,Cmul(x,a))+complexcast(float,y);
    return complexcast(fcomplex,z);
  } else
    return Cmul(a,x)+y

  return y;
}

func Cexpi(typ,x)
{
  extern cplxcast_ext;
  cast= cplxcast_ext;

  // double
  if (cast(typnm,typ)=="zd")
    return exp(1i*x);
  if (cast(typnm,typ)=="cd")
    return complexcast(double,exp(1i*x));
  // float
  y= array(float,dimsof(x(-:1:2,..)));
  y(1,..)= cos(x);
  y(2,..)= sin(x);
  return complexcast(typ,y)
}

func Cmul (z1,z2,conj2=)
/* DOCUMENT  fCmult(z1,z2,conj2=)
   z(1,..)= z1(1,..)*z2(1,..) - z1(2,..)*z2(2,..);
   z(2,..)= z1(2,..)*z2(1,..) + z1(1,..)*z2(2,..);
   return z;
*/
{
  extern cplxcast_ext;
  cast= cplxcast_ext;

  t1= structof(z1);
  t2= structof(z2);

  if (!cast(typeq,t1,t2))
    error,"no implicit type cast: tx!=ty";

  t1n= cast(typnm,t1);
  if (t1n=="zd")
    return (conj2==1? z1*conj(z2): z1*z2);
  if (t1n=="cd")
    return cplxelprod(z1,z2,conj2=conj2);
  // floats
  z1= complexcast(float,z1);
  z2= complexcast(float,z2);
  z= cplxelprod(z1,z2,conj2=conj2);
  if (t1n=="cf")
    return z;
  if (t1n=="zf")
    return complexcast(fcomplex,z);
}

func cplxelprod (z1,z2,conj2=)
{
  if (is_void((d=dimsof(z1,z2))))
    error,"z1 z2 dims must be comformable.";

  z= array(structof(z1(1)*z2(1)),d);
  if (conj2==1) {
    z(1,..)= z1(1,..)*z2(1,..) + z1(2,..)*z2(2,..);
    z(2,..)= z1(2,..)*z2(1,..) - z1(1,..)*z2(2,..);
  } else {
    z(1,..)= z1(1,..)*z2(1,..) - z1(2,..)*z2(2,..);
    z(2,..)= z1(2,..)*z2(1,..) + z1(1,..)*z2(2,..);
  }
  return z;
}

#if 0
func test (n,f)
{
  n= is_void(n)? 1000: n;
  re= random_n(n);
  im= random_n(n);
  zd= re+1i*im;
  zf= array(fcomplex,n);
  zf.re= re;
  zf.im= im;
  cd= re(-:1:2,..);
  cd(2,..)= im;
  cf= float(re)(-:1:2,..);
  cf(2,..)= im;

  td= save();
  ts= save();

  ref= abs(zd);

  save,ts,string(0),"Cabs ZD";
  save,td,string(0),ref-Cabs(zd);

  save,ts,string(0),"Cabs ZF";
  save,td,string(0),ref-Cabs(zf);

  save,ts,string(0),"Cabs CD";
  save,td,string(0),ref-Cabs(cd);

  save,ts,string(0),"Cabs CF";
  save,td,string(0),ref-Cabs(cf);


  save,ts,string(0),"complexcast ZD->ZD";
  save,td,string(0),abs(zd-complexcast(complex,zd));

  save,ts,string(0),"complexcast Zf->ZD";
  save,td,string(0),abs(zd-complexcast(complex,zf));

  save,ts,string(0),"complexcast CD->ZD";
  save,td,string(0),abs(zd-complexcast(complex,cd));

  save,ts,string(0),"complexcast CF->ZD";
  save,td,string(0),abs(zd-complexcast(complex,cf));

  // that's only a start ... more tests required
  error;

  for (i=1;i<=td(*);i++) {
    write,f,format=ts(i*1) +                   \
      " mean: %8.2g; rms: %8.2g, ptp: %8.2g\n",         \
      td(1*i,*)(avg),td(1*i,*)(rms),td(1*i,*)(ptp);
  }
}
#endif
