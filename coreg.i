require, "dsp.i";
require, "poly_fit.i";
if (!is_void(mp_size)) mp_require, "mpool.i";;

func correl2d_fast(a, b, &work, srch=)
/* DOCUMENT correl(a,b,srch=)
  off+[(i=c(*)(mxx)-1)%srch(1)+1,i/srch(1)+1]-srch/2-1;
  cross correlation of b with a, 
  -  dims must be id., but
  the offset is == 0 for id. starting pix. [1,1]
  -  a can have a third dims over which correlation is repeated
  -  if an offset is specified, it will translate the output
  ie. the full offset will be above expression
  usage example: -------
  n=200;m=300;srch=[5,5];a= random_n(n,m);
  b=array(0,n,m);b(srch(1)/2+1,srch(2)/2+1)=1;
  statarr, a(:srch(1),:srch(2))-correl2d_fast(a,b,srch=srch)
 */
{
  srch= [0,0]+(is_void(srch)? 5: srch);

  typo= structof(a(1)*b(1));  
  da= dimsof(a);
  db= dimsof(b);
  if (da(1)==3 && db(1)==2)
    b= [b];
  db= dimsof(b);

  nd= da(1);
  dab= max(da,db);
  dt= array(nd, 1+nd);
  for (i= ; i<=2 ; ++i)
    dt(1+i)= fft_good(dab(i+1));
  if (nd==3) dt(0)= da(4);
  at= array(complex, dt);
  if (nd==3) dt(0)= 1;
  bt= array(complex, dt);

  at(1:da(2),1:da(3),..)= a;
  bt(1:db(2),1:db(3),..)= b;
  nt= numberof(bt);

  bt(1:srch(1)-srch(1)/2-1,..)= 0;  //framing
  bt(-srch(1)/2+1:,..)= 0;
  bt(,1:srch(2)-srch(2)/2-1,..)= 0;
  bt(,-srch(2)/2+1:,..)= 0;

  if (is_void(work)) work= fft_setup(dimsof(at),[1,1]);
  fft_inplace, at, [1,1], setup=work;
  fft_inplace, bt, [1,1], setup=work;
  at= conj(bt)*at;
  bt= [];

  fft_inplace, at, [-1,-1], setup=work;

  at= extractarr(array(complex,_(nd,srch,(nd==3?da(4):[]))),\
                 at,_(-srch/2,(nd==3?0:[])),wrap=1)/nt;

  if (typo==int||typo==long) {
    at= typo(double(at)+0.5);
    return typo(at)-(at<0);
  }else{
    return typo(at);
  }
}


func filecoreg(fref,f,dims=,refcen=,lag=,off=,fcplx=)
{
  if (is_sream(f)) 
    of= save(string(0),f);
  else if (3==is_obj(f))
    of= f;
  else
    error,"second arg, files_to_match, stream or oject";
  nf= of(*);

  oo= save();
  for (fi=1;fi<=nf;fi++) {
    save,oo,string(0),cpxcoreg(fref,of(0+fi),dims=dims,
              refcen=refcen,lag=lag,off=off,fcplx=fcplx);
  }
  return oo;
}

func jkcoreg(a,b,dims=,refcen=,lag=,off=,fcplx=)
{
  if (is_void(dims)) dims= 256;
  dims+= [0,0];
  if (is_void(search)) search= 4;
  search+= [0,0];
  if (is_void(refcen)) refcen= dims/2+search;
  refcen+= [0,0];  
  if (is_void(off)) off= 0;
  off+= [0,0];
}

func ampcoreg(a,b,&cov,&wrk,lag=,fcplx=,deramp=,osfac=,itmax=,tol=)
/* DOCUMENT ampcoreg(a,b,&wrk,&c,&p,lag=,fcplx=,deramp=,osfac=,itmax=)
  a amd b are conformable, b can have one exrtra trailing dim.
  lag= key defaults to one. meaning the shifts are assumed < 1.0px
  [1,1]== ampcoreg(x,roll(x,[1,1]))
 
 */
{
  d= dimsof(a);
  dd= dimsof(a,b);
  if (is_void(dd) || dd(1)>d(1)+1) error,"unconformable a & b";

  if (d(1)==dd(1))    // numberof slcs to match
    m= 1;
  else
    m= dd(0);

  if (is_void(lag)) lag= 2;
  lag+= [0,0];
  
  if (fcplx==1) {
    a= a(1,..)+1i*a(2,..);
    b= b(1,..)+1i*b(2,..);
    d= dimsof(a);
    dd= dimsof(a,b);
  }
  if (d(1)<dd(1)) a= [a];

  if (is_void(osfac)) osfac= 8;
  osfac*= [1,1];

  if (deramp==1) {
    error,"provide carrier, and code :p";
  }
  // worskspace
  if (is_void(wrk)) {
    wrk= array(pointer,3);
  }
  wrk1= *wrk(1);
  wrk2= *wrk(2);
  wrk3= *wrk(3);

  a= interpol8n(a,osfac,,wrk1,wrk2);
  b= interpol8n(b,_(osfac,(dd(1)==3?1:[])),,wrk1,wrk2);
  
  ol= osfac*lag;
  a= abs(a);    //detect
  b= abs(b);    //detect
  a1= avg(a);
  a2= sqrt(sum(a^2));
  a-= a1;
  b-= a1;
  a/= a2;
  b/= a2;
  c= correl2d_fast(b,a,wrk3,srch=2*ol+1);
  c/= max(c); 

  x1= span(-lag(1),lag(1),2*ol(1)+1);
  x2= span(-lag(2),lag(2),2*ol(2)+1);
  mx= c(*)(mxx);
  mx2= indim(mx,dimsof(c));
  x00= xin= [x1(mx2(1)),x2(mx2(2))];
  whgt= 1/(1+osfac(1)*(x1    -x00(1))^2/lag(1)^2+\
             osfac(2)*(x2(-,)-x00(2))^2/lag(2)^2)^2;
  
  p= poly2_fit(c,x1,x2(-,),8,whgt);

  it= 0;
  if (is_void(itmax)) itmax= 300;
  if (is_void(tol)) tol= 1e-3;
  aa= 0.1;
  m= 10;

#if 1
  dx= 1.0/(osfac-1);
  do {
    x0= x00;
    x1= x00(1)+span(-dx(1),dx(1),2*m+1);
    x2= x00(2)+span(-dx(2),dx(2),2*m+1);
    dx/= m;
    cc= poly2(x1,x2(-,),p);
    mx= cc(*)(mxx);
    mx2= indim(mx,dimsof(cc));
    x00= [x1(mx2(1)),x2(mx2(2))];
    d= x00-x0;
  } while (abs(d(1),d(2))>tol && it++<itmax &&\
           abs(x00(1))<lag(1) && abs(x00(2))<lag(2));
#endif
#if 0
  do { 
    x0= x00;
    d= poly2_deriv(x00(1),x00(2),p);
    x00+= d*aa;
    it++;
  } while (abs(d(1),d(2))>tol && it<itmax &&\
           abs(x00(1))<lag(1) && abs(x00(2))<lag(2));
  if (it==itmax) write,"reached max iterations.";
  if (poly2(x00(1),x00(2),p)<poly2(xin(1),xin(2),p)) x00= xin;
#endif

  hs= [poly2_deriv(x00(1)+aa,x00(2),p)-poly2_deriv(x00(1)-aa,x00(2),p),\
       poly2_deriv(x00(1),x00(2)+aa,p)-poly2_deriv(x00(1),x00(2)-aa,p)]/(2*aa);  //hessian at max
  hs= (hs+transpose(hs))/2;
  u= hs(1,1)*hs(2,2)-hs(1,2)^2;  //determinant, == product eigval
  if (u!=0)
    cov= -0.5*transpose(hs(::-1,::-1))/u;
  else
    cov= array(99,2,2);

  wrk(1)= &wrk1;
  wrk(2)= &wrk2;
  wrk(3)= &wrk3;

  return x00;
}


func coreg_test(lag=,osfac=,off=)
{
  local work, c;
  d= "/u/uav-r7/trm/ampcor/";
  dims= [3,2,6000,16000];
  f1= open(d+"mc2_1.dat","rb");
      add_variable,f1,-1,"x",float,dims;
  f2= open(d+"mc2_2.dat","rb");
      add_variable,f2,-1,"x",float,dims; 

  lag= [2,2];
  box= [64,64];
  t= tile(_(2,dims(3:)),box-2*lag);
  dt= dimsof(t(1,..));
  
  fd= open(d+"msr.dat","wb");
      add_variable,fd,-1,"x",double,_(3,2,dt(2:));
  fc= open(d+"msr_cov.dat","wb");
      add_variable,fc,-1,"x",double,_(3,3,dt(2:)); 
  
  f= open(d+"jk.txt","w");
  write,f,"# dimsof tiles: ",pr1(dt);
  //for (it=1;it<numberof(t(1,*));it++) {
  for (it=1;it<100;it++) {
    x1= extractarr(_(3,2,box),f1.x,_(0,t(,it)-1),wrap=1);
    x2= extractarr(_(3,2,box),f2.x,_(0,t(,it)-1),wrap=1);
    if (!is_void(off)) x2= float(roll(x2,_(0,off+[0,0])));
    dd= ampcoreg(x1,x2,c,work,lag=lag,osfac=osfac,fcplx=1);
    fd.x(,it)= dd;
    fc.x(,it)= c([1,4,2]);
    write,"tile #: ",pr1(it)," disp: ",pr1(dd)," cov: ",pr1(c([1,4,2]));
    write,f,t(1,it),t(2,it),dd(1),dd(2),c(1),c(4),c(2),\
            format="%ld  %ld  %6.3f  %6.3f  %6.3f  %6.3f  %6.3f\n";
  }
  close,f1;
  close,f2;
  close,fd;
  close,fc;
  close,f;
}

