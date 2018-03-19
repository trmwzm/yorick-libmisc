/* functor (?) function with stashed data*/
scratch= save(scratch, tmp);
tmp= save(eval_);
func funclos (base,void,key=)
/* DOCUMENT funclos (base,key=)
*/
{
  ob= base(:);

  if (is_void(key))
    key= random(3);

  save,ob,key

  return closure(ob,eval_); // eval_ is an obj member, see closure/restore below
}
func  eval_ (x, &f)
{
  f= sum(use(key)*x);
  return f;
}
funclos= closure(funclos, restore(tmp));
restore, scratch;

/*---------------------------------------------------------*/
/* regular oxy class */
scratch= save(scratch, tmp);
tmp= save(meth1,meth2);
func regobj (base,void,dat=)
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
regobj= closure(regobj, restore(tmp));
restore, scratch;

/*---------------------------------------------------------*/

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

/*---------------------------------------------------------*/

tmp= save(add);
func obj (base,a)
/* DOCUMENT obj (base,dat)
*/
{
  ob= base(:);
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
obj= closure(obj, restore(tmp));

tmp= save(mm);
func obj2 (base,a,p)
/* DOCUMENT obj2 (base)
*/
{
  ob= obj(a);

  if (is_void(p))
    p= 1;

  save, ob, [], base(:);
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
obj2= closure(obj2, restore(tmp));

a= [];
o1=obj(1);
o2=obj(2);
o3=o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;
a= [];
o1=obj2(1);
o2=obj2(2);
o3=o1(add,o2);
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
o1=obj(1);
o2=obj(2);
o3=o1(add,o2);
if (o1(a)!=1 || o2(a)!=2 || o3(a)!=3 || !is_void(a)) error;
a= [];
o1=obj2(1);
o2=obj2(2);
o3=o1(add,o2);
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
