scratch= save(scratch, tmp, og_array, og_oxy, og_void);
tmp= save(add,add1,set,set1,view,copy,get,dump,og_write,og_typr);
func oconfig (base, name, .., help=)
/* DOCUMENT cfg= oconfig(name,[save(a,help="a bla",[flag=1],
                   open("my.cfg","r"), ...);
   arguments beyond NAME are passed as: cfg, add, ARGS(i)
   methods are: ADD, SET, RESET, GET, WRITE, READ
   add: 
       s, add, save(a,help="option a",[flag=1]);
       s, add, "b2",save(b,help="option b",[flag=1]); // renamed add b->b2
       s, add, c=sqrt(2);
       s, add, oconfig("cfg1",save(b,help=...));
       s, add, "cfg",oconfig("cfg1",save(b,help=...));// rename
       s, add, conf=oconfig("cfg1",save(b,help=...)); // rename
          declare a config option, which can be a config itself
          * second arg is CFG, or option object membspec and help string.
          * if not CFG, the *first* object member is the config parameter
          * use flag=1 if the key is to be used as a flag only. 
   help
       s, help;
          display options.
   set
       w= s(set, ["-a","7","pos"]);
          parse the command line, sets options.
          * first arg is the "chopped" command line: copy_argv()(4:)
          * output are the indices of non-key, positional, comand line args.
   copy           
       a= s(copy,"-a"[,v=1][,set=1])
          extract options, or check if they were set (use set=1).
          * first arg is key as it was declared using the add method.
          * use v=1 for a verbose value reset.
  
  -------- config object interface pre-plan:
  #include "oconfig.i"

  c1= oconfig("cfg1");
  c1,add,save(p1=pi,help="p1 is pi");
  c1,add,save(f1=0,help="f1 is flag",flag=1);
  c1,add,p2=sqrt(2);
  c1,add,"p3",[1,2];
  c1,add,"p4","sik";
  write,"c1 done";

  c=  oconfig("cfg");
  c,add,save(p=100,help="p is 100");
  c,add,c1;
  c,add,"config1",c1;
  c,add,conf=c1;

  c1,set,save(p1=pi,help="p1 is pi");
  c1,set,save(f1=0,help="f1 is flag",flag=1);
  c1,set,p2=sqrt(2);
  c1,set,"p3",[1,2];
  c1,set,"p4","sik";
  write,"c1 re-done";

  c,set,save(p=100,help="p is 100");
  c,set,c1;
  c,set,"config1",c1;
  c,set,conf=c1;

  cc= c(view,p,conf);
  save, cc(conf,p1), p1=100;
  c(val,conf,p1,p1);  // demonstrate that a view is a reference

  cc= c(copy,p,conf);
  cc,set,save(p=200,help="p is pi");
  cc(val,p,p);  // copy is a copy
  c(val,p,p);   // view is a reference
*/
{
  obj= base(:);  /* make copy of the dbase base class */

  if (is_void(name))
    name= "config";
  if (!is_string(name))
    error,"string which is config object name is expected.";
  
  if (is_void(help)) help= "";  // description

  is_config= 1;
  curdepth= 0;
  val= save();   // parameter group
  
  save, obj, val, name, help, is_config, curdepth;
  
  while (more_args()>0)
    if (is_obj((ar=next_arg())))
      obj, add, ar;
    else if(strgrepm("text stream",info(ar)(1)))
      obj, read, ar;
    else
      error,"oconfig(name,[save(val,help=)],[oconfig()],[open(fcfg,\"w\")]";
  return obj;
}
func add (args)
/* cfg, add, key=val;
   cfg, add,"key",val;
   cfg, add,save(key=val,help=...,[flag=...]);

 */
{
  use,val;
  sk= args(-);    // key strings
  nk= numberof(sk);
  for (i=1;i<=nk;i++) {
    nm= sk(i);
    void= use(add1,val,nm,args(nm));
  }
  i= 1;
  while (i<=args(0))  {
    ar= args(i);
    if (is_string(ar)) {
      i+= 1;
      oi= args(i);
      void= use(add1,val,ar,oi);
    } else
      void= use(add1,val,[],ar);
    i+= 1;
  }
}
wrap_args, add;
func add1 (val,nm,oo)
{
  if (is_void(nm))
    if (is_obj(oo))
      nm= oo(name);
    else
      error,"name needed with non-object config values.";

  if (!is_string(nm)) error,"expecting string type.";

  // disallow overwriting with add (overwriting, only that, allowed with set method)
  if (is_obj(val,noop(nm),1)>=0)
    error,"oconfig object already contains var: "+pr1(nm)(1);
    
  if (is_obj(oo))
    save,val,noop(nm),oo(:); 
  else
    save,val,noop(nm),oo;  // FIRST is key ....
}
func set (args)
{
  use, val;
  // command line 
  sk= args(-);    // key strings
  nk= numberof(sk);
  for (i=1;i<=nk;i++) {
    nm= sk(i);
    void= use(set1,val,nm,args(nm));
  }
  i= 1;
  while (i<=args(0))  {
    ar= args(i);
    if (is_string(ar)) {
      i+= 1;
      oi= args(i);
      void= use(set1,val,ar,oi);
    } else
      void= use(set1,val,[],ar);
    i+= 1;
  }
}
wrap_args, set;
func set1 (val,nm,oo)
{
  // mo name
  if (is_void(nm))
    nm= oo(name);
  else
    error,"name needed with non-object config values.";

  if (!is_string(nm)) error,"expecting string type.";

  // overwriting only
  if (is_obj(val,noop(nm),1)<0)
    error,"oconfig does not contain var: "+pr1(nm)(1);

  oi= val(noop(nm));
  if (is_obj(oi)) {
    if (!is_obj(oo))
      error,"set a child oconfig whith new oconfig, or set its members.";
    si= oi(*,);
    so= oo(*,);
    si= si(sort(si));
    so= so(sort(so));
    if (anyof(so!=si)) 
      error,"oconfig does not have identical members as target.";
    for (i=1;i<=oo;i++)
      if (srtuctof(oo(so(i)))!=srtuctof(oo(so(i))) ||
        anyof(dimsof(oo(so(i)))!=dimsof(oo(so(i))))) 
        error,"overwriting with different value.";
    save,val,noop(nm),oo(:);
  } else {                // no checks on types and dims... bad?
    if (srtuctof(oo(so(i)))!=srtuctof(oo(so(i))) ||
        anyof(dimsof(oo(so(i)))!=dimsof(oo(so(i))))) 
        error,"overwriting with different value.";
    if (is_obj(oo)>0) {
      if (oo(*,1)!=nm)
        oo= save(noop(nm),oo(1),[],((oo(*)>1)? oo(2:): []));
    } else {
      oo= save(noop(nm),oo,[],((oi(*)>1)? oi(2:): []));
    }
    save,val,noop(nm),oo;  // FIRST is key ....
    if (is_obj(oo,help,1)<0) save,oo,help= ""; 
    if (is_obj(oo,flag,1)<0) save,oo,flag= 0; 
    // if (is_obj(oo,type,1)<0) save,oo,type= structof(oo(1)); 
    // if (is_obj(oo,dims,1)<0) save,oo,dims= dimsof(oo(1));
    save,val,noop(nm),oo; 
  }
}
func view (args)
{
  use, val;
  // views into oconfig params or sub-configs allowing modifications
  if (numberof(args(-))) 
    error,"no keywords allowed.";
  n= args(0);
  out= save();
  for (i=1;i<=n;i++)
    save, out, noop(args(-,i)), val(noop(args(-,i)));
  return out;
}
wrap_args, view;
func copy (args)
{
  oo= use(:);
  val= oo(val);
  if (numberof(args(-))) 
    error,"no keywords allowed.";
  n= args(0);
  out= save();
  for (i=1;i<=n;i++)
    save, out, noop(args(-,i)), val(noop(args(-,i)));
  save,oo,val=out;
  return oo;
}
wrap_args, copy;
func get (args)
{
  use, val;
  if (numberof(args(-))) 
    error,"no keywords allowed."; // verbose key - print help:valname:val
  n= args(0);
  out= val;
  for (i=1;i<=n;i++)
    out= out(noop(args(-,i)));
  if (is_obj(oo,is_config,1)<0) out= out(noop(args(-,n)));
  return out;
}
wrap_args, get;
func dump (prefix=, maxary=, maxchild=, maxdepth=)
/* */
{
  use, val, name;
  save, use, curdepth=0;
  if (is_void(maxary)) 
    maxary=5;
  if (is_void(maxchild)) 
    maxchild=20;
  if (is_void(maxdepth)) 
    maxdepth=5;
  output= swrite(format="config group name: %s\n",name);
  void= use(og_write, val, name, (is_void(prefix) ? "" : prefix), 0);
  if (am_subroutine())
    write, format="%s", output;
  return output;
}
func og_write (obj, name, prefix, stage, help) 
{
  use,curdepth,og_typr;
  curdepth++;
  if (stage == 1)
    prefix += [" |-", " | "];
  else if (stage == 2)
    prefix += [" `-", "   "];
  else
    prefix += ["", ""];
  nm= typeof(obj);
  if (og_typr(*,noop(nm)))
    void= use(og_typr, noop(nm), use(), obj, name, prefix(1), prefix(2),help);
  else
    output += swrite(format="%s %s (%s)\n", prefix(1), name, nm);
  curdepth--;
}
og_typr= save();
func og_oxy (this, obj, name, prefix1, prefix2, help) 
{
  count= obj(*);
  is_config= is_obj(obj,is_config,1)>=0;
  if (is_config==1) {
    output += swrite(format="%s %s (config_object, %d %s)\n",\
        prefix1, name, count, (count == 1 ? "entry" : "entries"));
    if (curdepth == maxdepth || count > maxchild)
      return;
    for (i= 1; i<=count; i++) {
      key= obj(*,i);
      if (!key) key= "(nil)";
      if (key!="is_config")
        void= this(og_write, obj(noop(i)), key, prefix2, 1 + (i==count));
    }
  } else {
    if (curdepth == maxdepth || count > maxchild)
      return;
    for (i= 1; i <= count; i++) {
      key= obj(*,i);
      if (!key) key= "(nil)";
      if (name==key && i<=count)
        void= this(og_write, obj(noop(i)), key, prefix2, (i == count), obj(help));
      else if (is_obj(obj(noop(i))))
        void= this(og_write, obj(noop(i)), key, prefix2, 1+ (i == count));
    }
  }
}
save, og_typr, oxy_object=og_oxy;
func og_array (this, obj, name, prefix1, prefix2, help)
{
  descr= typeof(obj);
  dims= dimsof(obj);
  n= numberof(dims);
  if (is_void(help)) help= "";
  k= 1;
  while (++k <= n) {
    descr += swrite(format=",%d", dims(k));
  }
  if (numberof(obj) <= maxary) {
    output += swrite(format="%s %s= %s;  // %s [%s]\n",prefix1,name,\
       sum(print(obj)),help,descr);
  } else {
    output += swrite(format="%s %s (%s)\n", prefix1, name, descr);
  }
}
save, og_typr,\
  float=og_array,\
  double=og_array,\
  complex=og_array,\
  char=og_array,\
  short=og_array,\
  string=og_array,\
  int=og_array,\
  long=og_array;
func og_void (this, obj, name, prefix1, prefix2, help) 
{
  output += swrite(format="%s %s (void) []\n", prefix1, name);
}
save, og_typr, void=og_void;
/*
func write ()
// include-able ascii dump, with precision issues...
{
}
func read ()
{
}
 */
oconfig= closure(oconfig, restore(tmp));
restore, scratch;

