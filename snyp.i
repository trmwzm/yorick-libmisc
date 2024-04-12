require, "yut.i";
require, "json.i";


/*---------------------------------------------------------*/
// warp_args to oxy: keyworda only
// args("argname") returns VOID if "argname" not in calling seq
func q (args)
{
   local k2;
   k2= anyof(args(-)=="k2")? args("k2"): "k2";
   return save(k1=args("k1"),k2);
}
wrap_args, q;

// warp_args to oxy: positional(first) + keyworda
func q (args)
{
   local k2;
   k2= anyof(args(-)=="k2")? args("k2"): "k2";
   o= save();
   for (i=1,o=save();i<=args(0);i++)  // positional
     save,o,args(-,i),args(i);
   k= args(-);
   for (i=1;i<=numberof(k);i++)       // keywords
     save,o,k(i),args(k(i));
   save,o,k2;                         // override, if k2 not in callseq
   return o;
}
wrap_args, q;

/*---------------------------------------------------------*/
// save/restore mechanics
a= 1; b= 2;             // l1
scratch= save(scratch, tmp);
{
  tmp= save(a,b);
  a= 10; b= 20;         // tmp members val are still that from l1
  x= restore(tmp);      // restore tmp's members values to those before l3
                        //   SET ITS ITS MEMBERS KEYS AND VALUES AS THOSE OF X with
                        //   current values, new[>l3] -- or old[<l3] if they were not reset
}
restore, scratch;       // switch scratch and tmp to l1 state: x(a)==10; x(b)==20; tmp==orig

/*---------------------------------------------------------*/
// pass keyword default values in oxobj O and allow override with keyword
func tst (o, k=)
{
  k= !is_void(k)? k: o(k);
  tmp= save(k);
  restore,o;              // ! everything not a keyword better be declared local !
  restore,tmp;
  return k;
}

/*---------------------------------------------------------*/
/* functor (?) function with stashed data*/
scratch= save(scratch, tmp);
tmp= save(eval_);
func funclos (base, void, key=)
/* DOCUMENT funclos (base, key=)
   f= funclos();
   save, f.function, key=pi;  //  modify initialization keyword
   f(2);                      //  6.28319
*/
{
  ob= base(:);

  if (is_void(key))
    key= random(3);
  save, ob, key             // TWO types of "closure" objects
                            // (*1) closure(func, data) OR (*2) closure(obj, memb)
  return closure(ob,eval_); // (*2->) eval_ is an OB obj member, see closure/restore below
}
func  eval_ (x, &f)
{
  f= sum(use(key)*x);
  return f;
}
funclos= closure(funclos, restore(tmp)); //(<-*1)
restore, scratch;

/*---------------------------------------------------------*/
/* regular oxy class */
scratch= save(scratch, tmp);
tmp= save(meth1, meth2);
func regobj (base, void, dat=)
/* DOCUMENT regobj (base,dat=)
*/
{
  ob= base(:);

  if (is_void(dat))
    dat= random(3);

  save,ob, dat;  // for use in meth2

  return ob;
}
func  meth1 (x, &f)
{
  return sum(use(dat)*x);
}
func  meth2 (void)
/* call as> o= regobj(); o(meth2,[])  */
{
  return avg(use(dat));
}
regobj= closure(regobj, restore(tmp));  // closure of the form (function, data);
                                        // where data is an object - which becomes "base"
restore, scratch;

/*---------------------------------------------------------*/
// wrap_args constructor
scratch= save(scratch, tmp);
tmp= save(m1, m2);
func ob (args)
/* DOCUMENT ob= new(k1= [,k2= ])
     K1=
   SEE ALSO:
 */
{

  local k2;
  if (anyof(args(-)=="k2")) k2 = args("k2");
  else k2= "default";

  q= save(k1=args("k1"), k2, [], args(1));
  return q;
}
wrap_args, ob;
func m1 (x)
{
   write,"m1";
   write,"use(k1):",use(k1);
   info,x;
}
func m2 (x)
{
   write,"m2";
   write,"use(k2):",use(k2);
   info,x;
}
ob= closure(ob, restore(tmp));
restore,scratch;

/*---------------------------------------------------------*/

scratch= save(scratch, tmp);
func tmp(args) {
  n = args(0);
  a = n? array(string, n) : [];
  for (i=1 ; i<=n ; ++i) a(i) = args(-,i);
  use, closure_data;
  closure_data;   /* print fixed closure data */
  a;          /* print positional argument names */
  args(-);  /* print keyword argument names */
}
wrap_args, tmp;
wrapped_closure = save(tmp, closure_data="Hello, world!");
wrapped_closure = closure(wrapped_closure, tmp);
restore, scratch;

/*---------------------------------------------------------*/

scratch= save(scratch,tmp);
tmp= save(add);
func obj (base, a)
/* DOCUMENT obj (base,dat)
*/
{
  ob= base(:);
  ob, type="obj";
  save, ob, a;

  return ob;
}
func  add (o)
{
  oo= use();
  if (!am_subroutine())
    oo= oo(:);
  oo, a=o(a)+oo(a);
  return oo;
}
obj= closure(obj, restore(tmp));
restore, scratch;

scratch= save(scratch,tmp);
tmp= save(mm);
func obj2 (base, a, p)
/* DOCUMENT obj2 (base)
*/
{
  ob= obj(a);

  if (is_void(p))
    p= 1;

  save, ob, [], base(:);
  ob, type="obj2", p=p;
  return ob;
}
func  mm (o)
{
  oo= use();
  if (!am_subroutine()) oo= oo(:);
  save, oo, a= oo(a)+o(a);
  return oo;
}
obj2= closure(obj2, restore(tmp));
restore, scratch;

a= [];
o1= obj(1);
o2= obj(2);
o3= o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;
a= [];
o1= obj2(1);
o2= obj2(2);
o3= o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;

/*-------------- same as above with explicit constructors ---------------*/

scratch= save(scratch,tmp);
tmp= save(add,mk);
func mk (a)
/* DOCUMENT
*/
{
  ob= use()(:);
  save,ob,type="obj";
  save,ob,a;

  return ob;
}
func  add (o)
{
  oo= use();
  if (!am_subroutine()) oo= oo(:);
  save,oo,a=o(a)+oo(a);
  return oo;
}
obj= closure(restore(tmp),mk);
restore,scratch;

scratch= save(scratch,tmp);
tmp= save(mm,mk); // added to obj[add,a]
func mk (a,p)
/* DOCUMENT
*/
{
  ob= obj(a);

  if (is_void(p))
    p= 1;

  save,ob,type="obj2",p;
  return ob;
}
func  mm (o)
{
  oo= use();
  if (!am_subroutine()) oo= oo(:);
  save, oo, a= oo(a)+o(a);
  return oo;
}
obj2= closure(restore(tmp),mk);
restore,scratch;

a= [];
o1= obj(1);
o2= obj(2);
o3= o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;
a= [];
o1= obj2(1);
o2= obj2(2);
o3= o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;


/*--------------------------------------------------------*/
/* testing framework */
scratch= save(scratch,tmp);
tmp= save();
save, tmp, f5,f7,print,tall;
func tstr (base,a)
{
  mnm= base(*,);
  m= strgrepm("f[0-9]",mnm);
  if (nallof(m))
    ob= base(where(!m));
  else
    ob= save();
  if (anyof(m))
    ftbl= base(where(m));
  else
    ftbl= [];
  save, ob, ftbl;
  save, ob, a,b=0;
  return ob;
}
func f5 (x)
{
  b+= 5*a*x;
  return 5*a*x;
}
func f7 (x)
{
  b+= 7*a*x;
  return 7*a*x;
}
func tall (x)
{
  use,ftbl,a,b;
  for (i=1;i<=ftbl(*);i++)
    ftbl,noop(i),x;
}
func print
{
  use,b;
  write,format="b= %i\n",b;
}
tstr= closure(tstr,restore(tmp));
restore, scratch;

/* --------------------  base/derived objects ------------------ */
scratch= save(scratch, tmp);
tmp= save(eval);
func oxo (base, data, type=)
{
  ob= base(:);

  op= save(oxo_a, oxo_b);
  ob, op=op;

  clsn= "oxo";
  typs= strpart(op(*,),strlen(clsn)+2:);
  if (is_void(y))
    return typs;
  if (is_void(type) || noneof(type==typs))
    error,"Must supply oxo TYPE, which is one of: "+(typs+" ")(sum);
  ob, type=type;

  ob, data=data;

  return op(clsn+"_"+type,ob);
}
func eval (x)
{
   error,"virtual func";
}
oxo= closure(oxo,restore(tmp)); restore, scratch;

scratch= save(scratch, tmp);
tmp= save(eval);
func oxo_a (base, ob)
{
  save, ob, [], base(:); //

  return ob;
}
func eval(x)
{
  use, data;
  return x(data);
}
oxo_a= closure(oxo_a,restore(tmp)); restore, scratch;

/*--------------------------  grab any sequence of args --------------------------------*/

scratch = save(scratch, tmp, jkobj_const, write, read, todox, fromdox, load, dump);

func jkobj_const (args)
{
  read= [];            //default init key
  sk= args(-);         //key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,"read")))
    read= args("read");
  if (nk>1)
    error,"only key allowed: read=.";
  if (nk==0 && args(*)==0)
    error,"args, or read= key requires.";

  data= save();
  for (i=1;i<=args(*);i++)
    save, data, args(*,i), args(i);

  save, use(), data;
  if (!is_void(read))
      use_method, read, read;
  return use();
}
wrap_args,jkobj_const;
func write (fnm)
{
    use, data;
    oxsave, createb(fnm), data;
}
func read (fnm)
{
    use, data;
    oxrestore, openb(fnm), data;
}
func todox (void)
{
  return oxprune(use(),nofunc=1);
}
func fromdox (dox)
{
  return oxmerge(use(),dox);
}
func dump (fnmout, json=, szmx=)
{
  write,format="Writing jkobj: %s\n",fnmout;
  o= use_method(todox,);

  if (json==1) {
    s= jsnox(jsbox(o,rootdir=dirname(fnmout),szmx=szmx));
    write,open(fnmout,"w"),s,format="%s";
  } else
    oxsave, (f=createb(fnmout)), o;
  return f;
}
func load (fnmin, json=)
{
  write,format="Reading jkobj: %s\n",fnmin;

  if (json==1)
    oo= oxjsb(oxjsn(text_lines(fnmin)));
  else
    oo= oxrestore((f=openb(fnmin)));
  save, use(), [], use_method(fromdox, oo);  // got that wrong, at first ...
  return f;
}

local jkobj;
/* DOCUMENT jkobj

   SEE ALSO:
 */
tmp= save(jkobj_const, write, read, todox, fromdox, load, dump);
jkobj = closure(tmp, jkobj_const);

restore, scratch;

/* --------------------  use_kwdflt  --------------------- */
o= save(a=pi,e=exp(1),c=sqrt(2));
func t(o, a=, e=) {use_kwdflt,o,a,e,c; a; e;c;}
c= 1;
t,o;
//   3.14159
//   2.71828
//   1       << *NOTE* C not a keyword, thus extern value if not defined
//   *OR* ( restore all members for convenience -- prob not a great idea)
func t(o, a=) {use_kwdflt,o,a; tmp=save(a); restore,o; restore,tmp; a;}
//   *NOTE: restoring O in func *CLOBBERS* all external values with member vals,
//          unless LOCAL.

scratch= save(scratch,tmp);
tmp= save(plg);
func graph (base,void)
{
  return base(:);
}
func plg (y, x, color=, type=) {
  use_kwdflt, use(), color, type;
  plg, y, x, color=color, type=type;
}
graph= closure(graph,restore(tmp));
restore, scratch;

g= graph();
g, color="blue",type=3;
fma;
g, plg, random(10), random(10);
g, plg, random(10), random(10),color="red";
g, plg, random(10), random(10),type=0;

/* functor (?) function with default data which can be set dynamically */
scratch= save(scratch, tmp);
tmp= save(eval_);
func funclos (base, key)
/* DOCUMENT funclos (key)
*/
{
  ob= base(:);

  ob, key= (is_void(key)? random(3): key);

  return closure(ob, eval_); // eval_ is an obj member, see closure/restore below
}
func  eval_ (x, &f, key=)
/* here the default values for kw are set with closure def, but
   kw defaults may still be overwritten with call
 */
{
  use_kwdflt, use(), key;
  f= sum(key*x);
  return f;
}
funclos= closure(funclos, restore(tmp));
restore, scratch;

scratch = save(scratch, tmp);
tmp= save(ob_new, ob_m1, ob_m2);
extern ob; // foc DOC
func ob_new(args)
/* DOCUMENT ob= ob_new(k1= [,k2= ])
     K1=
   SEE ALSO:
 */
{
  local k2;
  local ob_m1, ob_m2;
  restore, use, ob_m1, ob_m2;

  if (anyof(args(-)=="k2")) k2 = args("k2");
  else k2= "default";

  return save(k1=args("k1"), \
              k2, \
              ob_m1, \
              ob_m2);
}
wrap_args, ob_new;


/* -------------------------- hu? -------------------------- */
#if 0
func test (nx)
{
  p= max(.1,random(3,4));
  x1= 0.0; x0= 10.0;
  dx= (x0-x1)/(nx-1);
  x= span(x1,x0,nx);
  y= p*x(-,-,..);
  xp= 1/p;
  ip= long((xp-x1)/dx);  // index of 1/P in x
  f= (xp-(x1+ip*dx))/dx;
  jp= reform(indgen(numberof(xp)),dimsof(xp))+ip*numberof(xp);
  yp1= y(jp);
  yp0= y(min(jp+numberof(xp),numberof(y)));
  return yp1*(1-f)+yp0*f-1.0
}
#endif
