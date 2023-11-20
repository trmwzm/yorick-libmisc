require, "fcomplex.i";
require, "xplot.i";

OXY_VOID_VAL= "oxy_void";

func eval(code, tmp=, debug=)
/* DOCUMENT eval, code;
   -or- eval(code);
   Evaluates CODE given as a string or as an array of strings (considered
   as  different lines  in the  script).  Since  CODE can  be dynamically
   build,   this  routine   allows  the   execution  of   virtually  (see
   hints/restrictions below)  any Yorick's code  (e.g. dynamic definition
   of  structures,  of functions,  etc.).   For  instance, the  following
   statement defines a new structure:
   eval, "struct NewStruct {string a; long b; float c, d;}";

   Since  the script  gets evaluated  at the  scope level  of  the "eval"
   routine some local variables of the  "eval" routine may be used in the
   script:
   "eval_tmp"    contains  the  name of  the temporary script  file and
   must not be changed by the script;
   "eval_debug"  contains the value of  the keyword DEBUG and  must not
   be changed by the script;
   "eval_code"   contains the value of the argument CODE;
   "eval_result" is  returned by  "eval", its  contents may  be defined
   into the script to provide a returned value.
   Note: impredictible  results may  occur if CODE  changes the  value of
   symbols "eval_tmp" and "eval_debug".

   Keyword TMP  can be  used to  specify the file  name of  the temporary
   script.  The default file name is:
   "$YORICK_EVAL_TMP"      if environment variable "YORICK_EVAL_TMP" is
   set;
   "/tmp/$USER-eval_tmp.i" if environment variable "USER" set;
   "~/.eval_tmp.i"         otherwise.
   If  keyword DEBUG  is true  (non-zero and  non-nil), the  name  of the
   temporary file is printed out and the file is not removed.


   SEE ALSO: include. */
{
  /* Dump script into a temporary file. */
  if (is_void(tmp)) {
    /* Create default name for yorick temporary code. */
    tmp= get_env("YORICK_EVAL_TMP");
    if (! tmp) {
      tmp= get_env("USER");
      tmp= (tmp ? "/tmp/"+tmp+"-" : "~/.") + "eval_tmp.i";
    }
  }
  write, format="%s\n", open(tmp, "w"), code;

  /* Use "eval_" prefix in order to somewhat protect local variables
     from caller's code. */
  local eval_result, eval_tmp, eval_code, eval_debug;
  eq_nocopy, eval_tmp,   tmp;
  eq_nocopy, eval_debug, debug;
  eq_nocopy, eval_code,  code;

  /* Source script and return result. */
  include, eval_tmp, 1;
  if (eval_debug) write, format="Yorick code written in \"%s\"\n", eval_tmp;
  else remove, eval_tmp;
  return eval_result;
}

func abspath(dir)
/*  DOCUMENT abspath(dir)
    make directory -- and parents ---
    returns absolute path.
*/
{
  if (is_void(dir))
    dir= "./";
  mkdirp, dir;
  pw0= get_cwd();
  cd,dir;
  dir= get_cwd();
  cd,pw0;
  return dir;
}

func pwd(nil)
/* DOCUMENT pwd
   -or- pwd()
   Prints out (subroutine form) or returns (function form) full path
   of current working directory.

   SEE ALSO: cd, lsdir. */
{
  if (! is_void(nil)) error, "unexpected non-nil argument";
  dir= cd(".");
  if (am_subroutine()) write, format="%s\n", dir;
  else return dir;
}

local unref; /* needed for documentation */
func __unref(&x) /* interpreted version */
/* DOCUMENT unref(x)
   returns X, destroying X in the process (useful to deal with temporary
   big arrays).  Written after Yorick's FAQ.
   SEE ALSO: eq_nocopy, swap. */
{
  local y;
  eq_nocopy, y, x;
  x= [];
  return y;
}
/* replace non-builtin function by interpreted one */
if (is_func(unref) != 2) unref=  __unref;

local swap; /* needed for documentation */
func __swap(&a, &b) /* interpreted version */
/* DOCUMENT swap, a, b;
   Exchanges  the contents  of variables  A and  B without  requiring any
   temporary copy.
   SEE ALSO: eq_nocopy, unref. */
{
  local tmp;
  eq_nocopy, tmp, a;
  eq_nocopy, a, b;
  eq_nocopy, b, tmp;
}
/* replace non-builtin function by interpreted one */
if (is_func(swap) != 2) swap=  __swap;


/*---------------------------------------------------------------------------*/

func topcpu (nil)
{
  if (! is_void(nil)) error, "unexpected non-nil argument";
  unm= rdline(popen("uname -n",0));
  if (!strmatch(unm,"mahuika")&&!strmatch(unm,"uavproc")) error,"sw update needed for other systems.";
  nu= strmatch(unm,"mahuika")? 128: 16;

  rc=  ["RCfile for \"top with windows\"		# shameless braggin'",\
        "Id:a, Mode_altscr=0, Mode_irixps=1, Delay_time=5.000, Curwin=0",\
        "Def	fieldscur=AEHIOQTWKNMbcdfgjplrsuvyzX",\
        "	winflags=30009, sortindx=10, maxtasks=1",\
        "	summclr=1, msgsclr=1, headclr=3, taskclr=1",\
        "Job	fieldscur=ABcefgjlrstuvyzMKNHIWOPQDX",\
        "	winflags=62777, sortindx=0, maxtasks=0",\
        "	summclr=6, msgsclr=6, headclr=7, taskclr=6",\
        "Mem	fieldscur=ANOPQRSTUVbcdefgjlmyzWHIKX",\
        "	winflags=62777, sortindx=13, maxtasks=0",\
        "	summclr=5, msgsclr=5, headclr=4, taskclr=5",\
        "Usr	fieldscur=ABDECGfhijlopqrstuvyzMKNWX",\
        "	winflags=62777, sortindx=4, maxtasks=0",\
        "	summclr=3, msgsclr=3, headclr=2, taskclr=3"];

  q= write(open(get_home()+".toprc","w"),rc,format="%s\n");
  tnm= "/tmp/.topq";
  system,"top bd00.50n2 | grep Cpu | tail -n"+pr1(nu)+" > "+tnm;
  rename,get_home()+".toprc",get_home()+".toprc_yo";
  l= text_lines(tnm);
  remove,tnm;

  x= array(0.0,nu);
  sread,strpart(l,8:12),x;

  return x;
}

/*---------------------------------------------------------------------------*/

func nint (x)
/* DOCUMENT nint(x)
   returns nearest integer (int)
*/
{
  x= x +.5;
  return long(x)-long(x<=0);
}

func is_null(var)
/* DOCUMENT is_null(var)
 * is true if VAR is void, or a null pointer
 * SEE_ALSO: is_integer, is_number, is_array, typeof
 */
{
  vartype= typeof(var);

  return (vartype == "void") ||
    ( (vartype == "pointer") && !dimsof(var)(1) && (var == pointer(0)) );
}

/*-------------------------------------------------------------------------------------*/

func struct_element(stru, &name, &type, &strutype)
/* DOCUMENT struct_element, stru, name, type, strutype;
   -or-  struct_element(stru,,type)
   -or-  ...

   Extract as string format :
   name : name of each element of the structure
   type : the type of each element of the structure, as typeof(stru.elem)
   struname : the typeof of the structure as typeof(stru);

   The function return name.
   The stru input could be a struct_definition or a variable.

   SEE ALSO: struct_include, struct_set_value, struct_fitsRead
*/
{
  local name,type;
  if (typeof(stru) != "struct_definition")
    stru= structof(stru);

  /* print a representation of the structure */
  stru= print(stru);

  if (stru(1)=="[]") {
    name= type= string([]);
    return name;
  }

  /* find the name of each element */
  strutype= strpart(stru(1),8:-2);

  if (numberof(stru)==2) {
    name= type= string([]);
    return name;
  }

  stru= strtrim(stru(2:-1));
  stru= strtok(stru," ");

  name= strpart(stru(2,),1:-1);
  type= stru(1,);

  return name;
}

/*-------------------------------------------------------------------------------------*/

func is_member(stru,membstr,membtyp)
/* DOCUMENT is_member(stru,membstr[,membtyp])
   test whether a structure member name [and type] is found in object
   SEE ALSOL struct_element
*/
{
  local name,type;
  if (typeof(stru) != "struct_definition") stru= structof(stru);
  struct_element, stru, name, type;
  msk= strtok(name,"(")(1,..) == membstr;
  fnd= anyof(msk);
  if (!fnd)return 0;
  if (!is_void(membtyp)) {
    if (structof(membtyp)!=string)membtyp= nameof(membtyp);
    if (membtyp == type(where(msk))(1))return 1;else return 0;
  }
  return 1;
}

/*-------------------------------------------------------------------------------------*/

func structeq (structDef1, structDef2, noname =) {
  /* DOCUMENT structeq(structDef1, structDef2, noname =)
     in principle equivalent to structDef1==structDef2
     BUT this may help getting around the problem encountered
     when the struture is reDEFINED after the structure instance is allocated
     in which case structof(structInst)==structDef2 is FALSE.
     The noname=1 flag enables to check the structure CONTENT only.
  */

  if (is_void(structDef1)||is_void(structDef2))return 0;

  if (typeof(structDef1) != "struct_definition"
      ||typeof(structDef2)  !="struct_definition")error,"structeq(structDef1, structDef2)";

  if (structDef1==structDef2)return 1;

  if (!is_void(noname)&&noname==1) {
    arInst= print(structDef1)(2:-1);
    arDef= print(structDef2)(2:-1);
  } else {
    arInst= print(structDef1)(1:-1);
    arDef= print(structDef2)(1:-1);
  }
  if (numberof(arInst)!=numberof(arDef))return 0;
  weq= arInst!=arDef;
  if (anyof(weq)) {
    weq= where(weq);
    arInst= arInst(weq)
      arDef= arDef(weq)
      } else {
    return 1;
  }
  nl= weq(sum);
  for (i=1;i<=nl;i++) {
    weq= strmatch(arInst,arDef(i));
    if (noneof(weq)) {
      return 0;
    } else {
      arInst= arInst(where(!weq));
    }
  }
  return 1;
}

func ref(var,noscalar=)
/* DOCUMENT ref(var,noscalar=)
   Returns reference to ("address of") variable VAR,
   or null, if IS_VOID(VAR) is true.
   If NOSCALAR==1, a null value is returned if IS_SCALAR(VAR) is true.
   (Extension of operator "&" Yorick-style; no-op in IDL)
   SEE ALSO: is_scalar
*/
{
  if (param_set(noscalar)) {
    if (is_scalar(var) && !(typeof(var) == "struct_instance"))
      return pointer([]);
  }
  if (is_null(var)) return pointer([]);
  return &var;
}

func deref(ptr)
/* DOCUMENT deref(ptr)
   Deference pointer PTR, but return null value if PTR is itself null.
   (Error-free version of Yorick-style "*" operator; no-op in IDL)
   SEE ALSO: ref, *
*/
{

  NULL= [];

  if (is_null(ptr)) return NULL;

  return *ptr;
}

func find_in_dir (din, nm, dir=, reg=, quiet=, take1=, hid=)
/* DOCUMENT x= find_in_dir(din, nm, dir=, quiet=, take1=, hid=)
   Exact match, or regex match if REG==1,file or directory search:
   X is the list of (or first if TAKE1==1) file-,
   or directory- (if DIR==1,) -instance found in directory tree rooted at DIN
q   which matches NM. Matching is done with Void.
   STRGREPM X is returned if a match is not found and QUIET==1.
   Error is called if QUIET is not set to 1 and no match is found.
   If HID==1, "hidden" directories are added to search path.
   pathfun.i has FIND_IN_PATH, for yorick file searches in get_path()
   WARNING: slow when searching large directory trees!
   SEE ALSO: find_in_path
 */
{
  // list dir and sub-dirs
  local d;
  din= din=="" || din==string(0) || is_void(din)? "./": din;
  din= diradd(din);
  f= lsdir(din,d);
  nf= numberof(f);
  nd= numberof(d);
  if (nd>0)
    if (hid!=1) {
      m= strpart(d,1:1)!=".";
      if (anyof(m)) {
        d= d(where(m));
        nd= numberof(d);
      }
    }

  // input dir check
  if (structof(f)==long)
    if (quiet)
        return [];
      else
        error,"Input directory: "+din+" not found.";
  
  // search
  i= 0;
  dd= ddd= [];
  x= dir==1? d: f;
  m= nf==0? []: (reg==1? strgrepm(nm,x): nm==x);
  if (nf>0 && anyof(m)) {
    w= take1==1? where(m)(1): where(m);
    ddd= diradd(din,f(w));
    if (take1==1)
      return ddd;
  }
  while (i++<nd && (take1!=1 || (is_void(dd) && take1==1)))
    ddd= _(ddd,(dd=find_in_dir(diradd(din,d(i)),nm,dir=dir,reg=reg, \
                               quiet=1,take1=take1,hid=hid)));

  if (!is_void(ddd))
    return ddd;

  if (quiet==1)
    return [];
  else
    error,(dir==1? "Directory: ": "File: ")+nm+" not found in "+din+".";
}

func file_first_alt (f, ..)
/* DOCUMENT f= file_first_alt(f1 [, f2 [, f3...]])
   F: first available file from alternatives: F1, F2...
   void if not found.
   SEE ALSO:
*/
{
  if (is_void(f))
    return [];
  if (open(f,"r",1))
    return f;
  else
    while (more_args())
      if (open((f=next_arg()),"r",1))
        return f;
  return [];
}

/* ------------------------------------------------------------------------ */

func check_file (fnm,.., quiet=, errmsg=)
/* DOCUMENT check_file (fnm,.., quiet=, errmsg=)
 */
{
  list= _lst(fnm(1));
  nf= 1;
  while (nf<numberof(fnm))
    list= _cat(list,fnm(1+nf++));

  while (more_args()) {
    fnm= next_arg();
    list= _cat( list, fnm(1));
    nfi= 1;
    while (nfi<numberof(fnm))
      list= _cat(list,fnm(1+nfi++));
    nf+= nfi
      }
  for (j=1;j<=nf;j++)
    if (!open((fnm=_car(list,j)),"r",1))
      if (quiet)
        return 0;
      else
        error,(is_void(errmsg)? "File": errmsg)+": "+fnm+" not found";

  return 1;
}

/* ------------------------------------------------------------------------ */

func check_dir (fnm,..,quiet=)
/* DOCUMENT check_dir (fnm)
 */
{
  list= _lst(fnm(1));
  nf= 1;
  while (nf<numberof(fnm)) 
    list= _cat(list,fnm(1+nf++));

  while (more_args()) {
    fnm= next_arg();
    list= _cat( list, fnm(1));
    nfi= 1;
    while (nfi<numberof(fnm)) 
      list= _cat(list,fnm(1+nfi++));
    nf+= nfi
      }
  for (j=1;j<=nf;j++)
    if (structof(lsdir((fnm=_car(list,j))))==long)
      if (quiet)
        return 0;
      else
        error,"***Directory: "+fnm+" not found";

  return 1;
}

/* -------------------------------------------------------------------*/

func diradd (s1, s2)
/* DOCUMENT diradd(s1,s2) == s1+s2
   unless s2 is ABSOLUTE path in that case == s2
   if S2 is void or string(0) or "". return s1 WITH "/" appended
   SEE ALSO:
*/
{
  s1= merge2(s1,s1+"/",strpart(s1,0:0)=="/");
  if (is_void(s2))
    return s1;
  return merge2(s2,s1+s2,strpart(s2,1:1)=="/");
}

/*--------------------------------------------------------------------*/

func strtimestamp (dum)
{
  s= timestamp();
  s= streplace(s,strfind("  ",s,n=1)," ");
  return streplace(s,strfind(" ",s,n=5),"_")
    }

/* -------------------------------------------------------------------*/

func waitff (fls, tlim, &t, dt=)
/* DOCUMENT waitff (fls,tlim,&t,dt=)
   wait for files FLS, with max TLIM (secs), check at interval DT (secs, default 5)
   returns time in secs.
   CALLED as a subroutine will raise error, else return 0 for success, >0 for error
   SEE ALSO:
*/
{
  t= 0;
  dt= is_void(dt)? 5: dt;
  l= 0;  // all files E
  tlim= is_void(tlim)? 60: tlim;
  while (!l && t<tlim) {
    for (l=1,i=1;i<=numberof(fls);i++)
      l&= !is_void(open(fls(i),"r",1));
    pause, dt*1000;
    t+= 1;
  }
  t= max(1,t-2);
  if (t+2>=tlim) {
    if (am_subroutine())
      error,"alloted time elapsed: "+pr1(tlim)+"hr, missing one of: "+ \
        ([fls]+" ")(*)(sum);
    else
      return t;
  }
  return 0
}

/* ------------------------------------------------------------------------ */

func runwaitsafe (cmd,&t,v=,tlim=,dt=)
/* DOCUMENT runwaitsafe (cmd,v=,tlim=)

   SEE ALSO:
*/
{
  utime= 0;
  timestamp, utime;
  rnd= long(random()*1e6);
  stmp= swrite(rnd,utime,format="/tmp/tic-%ld-ut-%ld");
  cmd= strpart(cmd,0:0)==";"? strpart(cmd,1:-1): cmd;
  cmd2= "rm -f "+stmp+";"+cmd+"; touch "+stmp+";";
  if (v==1)
    write,cmd2,stmp;
  sysafe,cmd2;
  if (waitff(stmp,tlim,t,dt=dt)>0) {
    if (am_subroutine())
      error;
    else
      return t;
  }
  remove,stmp;
  return 0;
}

/* ------------------------------------------------------------------------ */

func sread_n(s, &n0, &n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9)
/* DOCUMENT sread_n, f, n0, n1, n2, ...
   grabs the next numbers N0, N1, N2, ... from string s, skipping over
   any whitespace, comma, semicolon, or colon delimited tokens which
   are not numbers.  (Actually, only the first and last characters of
   the token have to look like a number -- 4xxx3 would be read as 4.)
   ***WARNING*** at most ten Ns are allowed
   The Ns can be arrays, provided all have the same dimensions.
   EXAMPLE:
   a=b=c=[1,2]
   sread_n,"1;2;3;4;5;6",a,b,c;a;b;c;

   SEE ALSO: sread, read_n, rdline
*/
{
  n= numberof(n0);
  for (i=1 ; i<=n ; i++) {
    sread_n_worker, s, n0, i;   if (is_void(n1)) continue;
    sread_n_worker, s, n1, i;   if (is_void(n2)) continue;
    sread_n_worker, s, n2, i;   if (is_void(n3)) continue;
    sread_n_worker, s, n3, i;   if (is_void(n4)) continue;
    sread_n_worker, s, n4, i;   if (is_void(n5)) continue;
    sread_n_worker, s, n5, i;   if (is_void(n6)) continue;
    sread_n_worker, s, n6, i;   if (is_void(n7)) continue;
    sread_n_worker, s, n7, i;   if (is_void(n8)) continue;
    sread_n_worker, s, n8, i;   if (is_void(n9)) continue;
    sread_n_worker, s, n9, i;
  }
}

func sread_n_worker(&s, &var, i)
{
  /* indirect flag necessary because can't store back into a
     scalar using var(i)=... (sigh) */
  if (indirect=dimsof(var)(1)) value= structof(var)();
  while (s) {
    tok= strtok(s, ",;: \t");
    s= tok(2);
    len= strlen(tok(1));
    if (len && strmatch("0123456789.",strpart(tok(1), len:len)) &&
        (indirect? sread(tok(1), value) : sread(tok(1), var))) {
      if (indirect) var(i)= value;
      return;
    }
  }
}


/* ------------------------------------------------------------------------ */

func dim_conform( dimsof_a, dimsof_b, &dimout)
/* DOCUMENT  dim_conform(dimsof_a, dimsof_b, )
   Returns 1 if the two arrays with dimensions DIMSOF_A and DIMSOF_B are
   conformable. dimout is as dimout=dimsof(a,b)
   void dims are interpreted as no-op not as op wiyh void.
*/
{
  if (is_void(dimsof_a) && is_void(dimsof_b)) return 1;
  if ((is_void(dimsof_a)&&!is_void((dimout=dimsof_b))) || \
      (is_void(dimsof_b)&&!is_void((dimout=dimsof_a)))) return 1;

  if (numberof(dimsof_a)!=dimsof_a(1)+1) error,"incorrect rank/doms layout";
  if (numberof(dimsof_b)!=dimsof_b(1)+1) error,"incorrect rank/doms layout";

  ndima= dimsof_a(1);
  ndimb= dimsof_b(1);

  if (ndima==0||ndimb==0) return 1;

  dima= dimsof_a(2:1+ndima);
  dimb= dimsof_b(2:1+ndimb);

  dimout=[];
  if (ndima<=ndimb) {
    if (nallof(dimb(:ndima)==dima|dima==1|dimb(:ndima)==1)) {
      return 0;
    } else {
      dima= max(dima,dimb(:ndima));
      dimout= ndima==ndimb? _(ndimb,dima): _(ndimb,dima,dimb(ndima+1:));
    }
  } else if (ndima>ndimb) {
    if (nallof(dima(:ndimb)==dimb|dimb==1|dima(:ndimb)==1)) {
      return 0;
    } else {
      dimb= max(dimb,dima(:ndimb));
      dimout= ndima==ndimb? _(ndima,dimb): _(ndima,dimb,dima(ndimb+1:));
    }
  }

  return 1;
}

/* ------------------------------------------------------------------------ */

func matmatseq (a, b, atr=, btr=)
/* DOCUMENT c= matmatseq(a,b)
   lame kludge for c(,,..)= a(,+,..)*b(+,,..)
   where (..)'s are id!
*/
{
  if (atr&&btr) {
    c= array(structof(a(1)*b(1)),dimsof(a(1,..)(-,..),b(,1,..)(,-,..)));  // do the broadcasting
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) for (j=1;j<=dc(3);j++) c(i,j,..)= (a(,i,..)*b(j,..))(sum,..);
  } else if (atr) {
    c= array(structof(a(1)*b(1)),dimsof(a(1,..)(-,..),b(1,..)(-,..)));  // do the broadcasting
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) for (j=1;j<=dc(3);j++) c(i,j,..)= (a(,i,..)*b(,j,..))(sum,..);
  } else if (btr) {
    c= array(structof(a(1)*b(1)),dimsof(a(,1,..)(,-,..),b(,1,..)(,-,..)));  // do the broadcasting
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) for (j=1;j<=dc(3);j++) c(i,j,..)= (a(i,..)*b(j,..))(sum,..);
  } else {
    c= array(structof(a(1)*b(1)),dimsof(a(,1,..)(,-,..),b(1,..)(-,..)));  // do the broadcasting
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) for (j=1;j<=dc(3);j++) c(i,j,..)= (a(i,..)*b(,j,..))(sum,..);
  }
  return c;
}

/*---------------------------------------------------------------------------*/

func matvecseq (a, b, atr=)
/* DOCUMENT c= matvecseq(a,b)
   lame kludge for c(,..)= a(,+,..)*b(+,..)
   if atr==1 same kludge for c(,..)= a(+,..)*b(+,..)
   where .. are id!
*/
{
  if (atr) {
    dab= dimsof(a(1,..),b(1,..)(-,..));
    c= array(structof(a(1)*b(1)),dab);  // do the broadcasting
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) c(i,..)= (a(,i,..)*b)(sum,..);
  } else {
    dab= dimsof(a(,1,..),b(1,..)(-,..));
    c= array(structof(a(1)*b(1)),dab);
    dc= dimsof(c);
    for (i=1;i<=dc(2);i++) c(i,..)= (a(i,..)*b)(sum,..);
  }
  return c;
}

/*---------------------------------------------------------------------------*/

levi_civita= array(double,3,3,3);
levi_civita(1,2,3)=levi_civita(3,1,2)=levi_civita(2,3,1)=1;
levi_civita(3,2,1)=levi_civita(2,1,3)=levi_civita(1,3,2)=-1;

func crossvec (a,b,seq=)
/* DOCUMENT crossvec(a,b,seq=)
   for one to one sequence of cross products, specify seq == 1
*/
{
  da= dimsof(a);
  db= dimsof(b);

  if (da(2)!=3||db(2)!=3) error,"expects a=[3,..] b=[3,..]";

  if (seq==1) {
    c= array(structof(a),dimsof(a,b));
    c(1,..)= a(2,..)*b(3,..)-a(3,..)*b(2,..);
    c(2,..) =-a(1,..)*b(3,..)+a(3,..)*b(1,..);
    c(3,..)= a(1,..)*b(2,..)-a(2,..)*b(1,..);
    return c;
  } else {
    return (levi_civita(,+,)*a(+,..))(,+,..)*b(+,..);
  }
}

func dotcross (a,b,c,seq=) {
  /* DOCUMENT dotcross(a,b,c,seq=)
     for one to one sequence of dotcross products, specify seq == 1
  */
  da= dimsof(a);
  db= dimsof(b);
  dc= dimsof(c);

  if (da(2)!=3||db(2)!=3||dc(2)!=3) error,"expects a=[3,..] b=[3,..] c=[3,..]";

  if (seq==1) {
    d=  a(1,..)*(b(2,..)*c(3,..)-b(3,..)*c(2,..));
    d-= a(2,..)*(b(1,..)+c(3,..)-b(3,..)*c(1,..));
    d+= a(3,..)*(b(1,..)*c(2,..)-b(2,..)*c(1,..));
    return d;
  } else {
    return ((levi_civita(,+,)*a(+,..))(,+,..)*b(+,..))(+,..)*c(+,..);
  }
}

func rotvec (a,w,q,seq=) {
  /* DOCUMENT rotvec (a,w,q,seq=)
     a: vector to rotate
     w: rotation vector axis and rotation: right-handed w/ hat(w)
     q: rotation angle [rtadians]
  */
  da= dimsof(a);
  dw= dimsof(w);

  if (da(1)==1 || dw(1)==1) seq= 1;

  if (da(2)!=3||dw(2)!=3) error,"expects a=[3,..] w=[3,..]";

  uw= w/abs(w(1,..),w(2,..),w(3,..));

  if (seq==1) {
    apara= (uw*a)(sum,..)(-,)*uw;
    aperp= a-apara;
    return apara+cos(q)(-,..)*aperp+sin(q)(-,..)*crossvec(uw,aperp,seq=1);
  } else {
    error,"dunno how to do this: use seq==1 or fix me.";
    apara= (uw(+,..)*a(+,..))()*uw;
    aperp= a-apara;
    return apara+cos(q)*aperp+sin(q)*crossvec(uw,aperp);
  }
}

/*---------------------------------------------------------------------------*/

func normvec(a, &norm) {norm= abs(a(1,..),a(2,..),a(3,..)); return a/norm(-,);}
func normproj(a, norm) {unorm= normvec(norm); return a-unorm*(a*unorm)(sum,..)(-,..);}

/*---------------------------------------------------------------------------*/

func strconcat(strarr,spacer)
{
  if (is_void(spacer))
    spacer= string(0);
  return strpart((strarr+spacer)(sum),:-strlen(spacer));
}

/*---------------------------------------------------------------------------*/

func strtranslate(s, tr)
/* DOCUMENT sp= strtranslate(s, tr);
   Convert a string or an array of strings given a translation table TR.
   TR must be an array of 256 char (this is not checked).

   SEE ALSO: strtolower, strtoupper, strtrtable.
*/
{
  d= dimsof(s);
  if (d(1)==0)
    if (s==string(0))
      return string(0);
    else
      return string(&tr(1+*pointer(s)));

  r= array(string, d);
  w= where(s!=string(0));
  n= numberof(w);
  for (i=1; i<=n; i++)
    r(w(i))= string(&tr(1+*pointer(s(w(i)))));

  return r;
}

/*---------------------------------------------------------------------------*/

func strtrtable(in, out, &tr)
/* DOCUMENT tr= strtrtable(in, out);
   -or- strtrtable, in, out, tr;
   Create or modify translation table TR so that characters that belongs to
   IN array will produce corresponding characters in OUT array.  IN and OUT
   must be conformable arrays of char's.

   SEE ALSO: strtranslate, strtolower, strtoupper.
*/
{
  if (is_void(tr))
    tr= char(indgen(0:255));
  tr(in+1)= char(out);
  //tr(in)= char(out);
  return tr;
}

/*---------------------------------------------------------------------------*/

func rjmread (f, delim)
{
  if (is_void(delim))
    delim= " ";
  l= text_lines(f);
  n= (strpart(l(1),strword(l(1),delim,100))!=string(0))(sum);
  return strpart(l,strword(l,delim,n));
}

/*----------------------------------------------------------------------------*/

func readtokey (&f, key, typ, dims, oldbmark, &newbmark, \
                delim=, rewind=, verbose=, valfirst=)
/* DOCUMENT: readtokey(&f,key,typ,dims,oldbmark,&newbmark,
   delim=,rewind=,verbose=,valfirst=)
   F is file (then returned as handle) or handle
*/
{
  if (typeof(f)=="string") {
    f= open(f,"r");
  } else {
    if (rewind==1)
      while(strpart(print(bookmark(f))(2),1:1)!="/"){backup,f;}
    else
      if (!is_void(oldbmark))
        backup, f, oldbmark;
      else
        oldbmark= bookmark(f);
  }

  key= is_void(key)? "": strtrim(key);

  if (is_void(delim))
    delim = ":;=~\t\n";

  for (i=1;i<=numberof(key);i++)
    while((line=rdline(f)) && !strmatch(line,key(i)));
  backup, f;
  newbmark= bookmark(f);

  keyVal= strtok(line,delim);

  if(is_void(dims))
    dims = [0];
  dims0 = dims;                     //local copy

  is_complex = 0;
  if (typ==complex) {
    is_complex= 1;
    if (dims(1)==0)
      dims0= [1,2];
    else
      dims0= _(dims(1)+1,2,dims(2,..));
  }
  if (is_void(typ))
    typ= structof("");
  is_string= typ==string? 1: 0;

  out= array(typ,dims0);

  vali= valfirst==1? 1: 2;
  if (is_string) {
    stmp= keyVal(vali);
    if (stmp==string(0))
      stmp="";
    if (dims0(1)!=0)
      words = strwords(stmp,nwords,delim=" ;:,");
    else
      words = strtrim(stmp);
    out(*) = words;
  } else
    sread_n, keyVal(vali), out;

  if (!is_void(verbose) && verbose==1)
    print,key,out;

  return out;
}

/*---------------------------------------------------------------------------*/

func writetokey (fnm, key, str, delim=, verbose=, valfirst=)
/* DOCUMENT: writetokey(fnm,key,str,
   delim=,verbose=,valfirst=)
   F is file (then returned as handle) or handle
*/
{
  if (structof(fnm)!=string)
    error,"expecting filename.";

  key= is_void(key)? "": strtrim(key);

  if (is_void(delim))
    delim = ":;=~\t\n";

  ll= text_lines(fnm);
  n= numberof(ll);
  for (j=1,i=1;i<=numberof(key);i++)
    while(!strmatch(ll(j),key(i)) && j<n) j++;
  line= ll(j);

  ss= strchar(".()");          // table () -> .
  t = strtrtable(ss(2),ss(1));
  strtrtable,ss(3),ss(1),t;
  keyVal= strtok(line,delim);
  idx= strgrep(strtranslate(keyVal(1),t)+" *["+delim+"]",line);
  dl= strpart(line,idx(2):idx(2));

  vali= valfirst==1? 1: 2;
  keyVal(vali)= str;
  sout= keyVal(1)+dl+" "+keyVal(2);
  ll(j)= sout;
  write,open(fnm,"w"),ll,format="%s\n";

  if (!is_void(verbose) && verbose==1)
    write,key,dl,sout,format="key: %s, - delim: %s - out: %s\n";
}

/*---------------------------------------------------------------------------*/

func readtextlog (fnm, delim=, comment=, n=)
/* DOCUMENT  ra= readtextlog(fnm,delim=,comment=,n=)

   SEE ALSO:
*/
{
  l= text_lines(fnm);
  if (is_void(comment))
    comment= "#";
  mc= strgrep("^"+comment,l)(2,..)==strlen(comment);
  if (anyof(mc))
    l= l(where(!mc));
  if (is_void(delim))
    delim= " ";
  n= is_void(n)? 20: n;
  ll= strpart(l,strword(l,delim,n));
  w1= where(ll(,1)!=string(0));
  return tonum(ll(w1,..));
}

/*---------------------------------------------------------------------------*/

func embedarr (args)
/* DOCUMENT embedar (a,&b,off,wrap=)
   embedar (a,f.x,off,wrap=)
   embeds a in b at index offset "off" (defaults to 0 in each dim)
   if in_place for b, use as a subroutine
   when b is of the form f.x use as subroutine
   if wrap==1 b indices are wrapped when outside of dimensions
   use wrap==1 also allows the use of negative offsets
   SEE ALSO: extractarr
*/
{  //keyword processing
  wrap= [];            //default init key
  sk= args(-);         //key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,"wrap")))
    wrap= args("wrap");
  if (nk>1)
    error,"only key allowed: wrap."

      //positional arg processing
      if (args(0)<2 || args(0)>3)
        error,"embedarr (a,b,[off],[wrap=])";
  a= args(1);
  off= args(0)>2? args(3): 0; // default val if missing

  db= dimsof(args(2,:));
  da= dimsof(a);
  ndim= da(1);
  off+= array(0,ndim);
  if (da(1) != db(1))
    error, "a and b must have same numberof of dimensions";

  da= da(2:);
  db= db(2:);

  // IA: indexing B in A:  B(IA)= A;
  ia= indgen(0:da(ndim)-1)+off(ndim,..);
  if (wrap==1)
    ia= ia%db(ndim) + db(ndim)*(ia<0);
  else {
    m= array(char(0),_(ndim,da));
    mm= ia>=0 & ia<db(ndim);
    if (anyof(mm)) {
      w= where(mm);
      m(..,w)= 1;
    }
  }
  for (i=ndim-1 ; i>=1 ; --i) {
    iia= indgen(0:da(i)-1)+off(i,..);
    if (wrap==1)
      iia= iia%db(i) + db(i)*(iia<0);
    else {
      mm= iia>=0 & iia<db(i);
      if (anyof(mm)) {
        w= where(mm);
        mi= array(char(0),da(i));
        mi(w)= 1;
        for (j=1;j<i;j++)
          mi= mi(-,..);
        m*= mi;
      }
    }
    ia= db(i)*ia(-,..) + iia;
  }
  ia += 1;

  if (wrap==1) {
    if (!am_subroutine()) {
      b= args(2);
      b(ia)= a;
      return b;
    } else {
      args(2,:)(ia)= a;
    }
  } else {
    if (anyof(m)) {
      w= where(m);
      ia= ia(w);
      a= a(w);
      if (!am_subroutine()) {
        b= args(2);
        b(ia)= a;
        return b;
      } else {
        args(2,:)(ia)= a;
      }
    } else
      return args(2,:);
  }
}
wrap_args, embedarr;

func extractarr (args)
/* DOCUMENT extractarr (a,b,off,wrap=)
   extractarr (a,f.x,off,wrap=)
   extract a from b at index offset "off" (defaults to 0)
   if in_place (for a,) use as subroutine
   if wrap==1, b indices are wrapped on dimensions
   use wrap==1 also allows using negative offset
   usage -------
   b= array(0.,100,100); a=random(10,10)
   pli,extractarr(array(double,10,10),embedarr(a,b,-3,wrap=1),-3,wrap=1)
   SEE ALSO: embedarr
*/
{
  // keyword processing
  wrap= [];            // default init key
  sk= args(-);         // key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,"wrap")))
    wrap= args("wrap");
  if (nk>1)
    error,"only key allowed: wrap.";

  // positional arg processing
  if (args(0)<2 || args(0)>3)
    error,"extractarr (a,b,[off],[wrap=])";
  a= args(1);
  off= args(0)>2? args(3): 0; // default val if missing

  db= dimsof(args(2,:));
  ida= 0;
  if (structof(a)==long && a(1)==numberof(a)-1) {
    da= a;
    ida= 1;
    if (am_subroutine())
      error,"call as func.";
  } else {
    da= dimsof(a);
  }

  ndim= da(1);
  off+= array(0,ndim);
  // if (dimsof(off)(1)>1 || (dimsof(off)(1)>0 && dimsof(off)(2)!=ndim))
  //   error,"broken now -- use to work (?).";

  if (da(1) != db(1))
    error, "a and b must have same numberof of dimensions";

  da= da(2:);
  db= db(2:);

  ia= indgen(0:da(ndim)-1)+off(ndim,..)(-,..);
  if (wrap==1)
    ia= ia%db(ndim) + db(ndim)*(ia<0);
  else {
    m= array(char(0),_(ndim,da));
    mm= ia>=0 & ia<db(ndim);
    if (anyof(mm)) {
      w= where(mm);
      m(..,w)= 1;
    }
  }
  for (i=ndim-1 ; i>=1 ; --i) {
    iia= indgen(0:da(i)-1)+off(i,..)(-,..);
    if (wrap==1)
      iia= iia%db(i) + db(i)*(iia<0);
    else {
      mm= iia>=0 & iia<db(i);
      if (anyof(mm)) {
        w= where(mm);
        mi= array(char(0),da(i));
        mi(w)= 1;
        for (j=1;j<i;j++)
          mi= mi(-,..);
        m*= mi;
      }
    }
    ia= db(i)*ia(-,..) + iia;
  }
  ia += 1;

  // extract A from B, which is agrs(2)
  if (wrap==1) {
    if (ida) { // not a subrout
      a= args(2,:)(ia);
    } else {
      if (!am_subroutine())
        a= args(2,:)(ia);
      else
        args, 1, args(2,:)(ia);
    }
    ia= [];
  } else {
    if (anyof(m)) {
      w= where(m);
      ia= ia(w);
      if (ida) { // not a subroutine
        a= array(structof(args(2,:))(0),_(ndim,da));
        a(w)= args(2,:)(ia);
      } else {
        if (!am_subroutine()) {
          b= a;
          b(w)= args(2,:)(ia);
          return b;
        } else {
          a(w)= args(2,:)(ia);
          args, 1, a;
        }
      }
    } else {
      if (ida) {
        a= array(structof(args(2,:))(0),_(ndim,da));
        return a;
      }
      if (!am_subroutine())
        return a;
    }
  }

  return a;
}
wrap_args, extractarr;

/*---------------------------------------------------------------------------*/

func moveop(op,f,n)
/* DOCUMENT
   minimum/max/median of the previous N samples
   for samples after N (N-1 first samples are replicated.)
   F has one dimension
   OP is MIN, or MAX, or MEDIAN
   SEE ALSO:
*/
{
  // dim f is N, dim f(i:-n+i) is N-(i-1)-n+i=N-n+1
  sop= info(op)(1);
  ff= array(structof(f),n,numberof(f)-n+1);
  for (i=1;i<=n;i++)
    ff(i,..)= f(i:-n+i);
  if (strmatch(sop,"min")) {
    return _(f(:n-1),ff(min,..));
  } else if (strmatch(sop,"max")) {
    return _(f(:n-1),ff(max,..));
  } else if (strmatch(sop,"median")) {
    return _(f(:n-1),median(ff,1));
  } else errror,sop+" operation not {min,max,median}";
}

func statarr(x,l,cmt=)
{
  slab= "      Max           Min             PTP            AVG            RMS";
  if (is_void(cmt))
    cmt= string(0);
  else
    cmt+= " ";
  if (is_void(x)) {
    write,cmt+slab;
    return;
  }
  if (am_subroutine()) {
    if (structof(x)==complex) {
      write,cmt+"real part: ";
      statarr,x.re;
      write,"imaginary part: ";
      statarr,x.im,1;
    } else {
      if (l!=1) {
        write,cmt+"Number of elements :"+pr1(numberof(x));
        write,slab;
      }
      write,[x(*)(max),x(*)(min),x(*)(ptp),x(*)(avg),x(*)(rms)];
    }
  } else {
    if (structof(x)==complex)
      return transpose([statarr(x.re),statarr(x.im)]);
    else
      return [x(*)(max),x(*)(min),x(*)(ptp),x(*)(avg),x(*)(rms)];
  }
}

/*---------------------------------------------------------------------------------------------------*/

func cycleIndex (i,init,end)
/* DOCUMENT cycleIndex (i,init,end)
   returns nearest intefer (int)
*/
{
  if (!is_integer((j=i(1)*init(1)*end(1)))) {
    j= structof(j)(1);
    return cycle(j*i,j*init,j*end);
  }
  ii= i-init;
  n= end - init+1;
  ii= (ii%n)*(ii>=0)+(((ii+1)%n)+n-1)*(ii<0);
  return ii+init;
}

/*---------------------------------------------------------------------------*/

func cycle(x,xinit,xend) {
  /* DOCUMENT cycle(x,xinit,xend)
   */
  return x-floor((x-xinit)/(xend-xinit))*(xend-xinit);}

/*---------------------------------------------------------------------------*/

func indexarr (d,n1,n0,strd)
/* DOCUMENT i= indexarr(d,n1,n0,strd)
   D: dimsof(array to partition)
   N1: starting index
   N0: end index
   STRD: stride
   SEE ALSO:
*/
{
  nd= d(1);
  d= d(2:);
  n1= is_void(n1)? array(1,nd): n1;
  n0= is_void(n0)? d: n0;
  strd= is_void(strd)? array(1,nd): strd;

  // broadcast and throw if not conformable
  z= array(0,nd);
  n1+= z;
  n0+= z;
  strd+= z;

  n1= merge2(n1+d,n1,n1<=0);
  n0= merge2(n0+d,n0,n0<=0);

  ia= indgen(n1(nd)-1:n0(nd)-1:strd(nd));
  for (i=nd-1 ; i>=1 ; --i)
    ia = d(i)*ia(-,..) + indgen(n1(i)-1:n0(i)-1:strd(i));
  ia += 1;
  return ia;
}

/*---------------------------------------------------------------------------*/

func indim(n, dim, ..,ndxdimlast=) {
  /* DOCUMENT indim(n, dim, ..,ndxdimlast=)
     returns each individual dimension index corresponding to (*) index n given
     dimlist "dim".
     comparing * indices: i-dimin(indim(i,dx),dx)

     EX: mx=x(*)(mxx);mx==dimin(indim(mx,dimsof(x)),dimsof(x))
  */
  while (more_args()) build_dimlist, dim, next_arg();
  out= dim(2:0);
  if (numberof(n)>1) {
    if (ndxdimlast==1) {
      out=array(0,numberof(n),dim(1));
    } else {
      out=array(0,dim(1),numberof(n));
    }
  }
  dimp= dim; dimp(1)= 1;
  n1= n-1;
  for (i=2, j=1; i<=dim(1); i++, j++) dimp(i)= dimp(j) * dim(i);
  for (i=dim(1); i>0; i--) {
    if (ndxdimlast==1) {
      out(..,i)= n1 / dimp(i)+1;
    } else {
      out(i,..)= n1 / dimp(i)+1;
    }
    n1= n1 % dimp(i);
  }
  return out;
}

/*---------------------------------------------------------------------------*/

func dimin(n, dim, ..,ndxdimlast=) {
  /* DOCUMENT dimin(n, dim, ..,ndxdimlast=)
     returns (*) index given each individual dimension and dimlist "dim".
     memoTrick dimin "diminishes dimension"
     EX: mx=x(*)(mxx);mx==dimin(indim(mx,dimsof(x)),dimsof(x))
  */
  while (more_args()) build_dimlist, dim, next_arg();
  //   if (numberof(n)!=dim(1)) {
  //     print,"dimin in trm.i: need dimnin(n,dim,..) where numberof(n)==dim(1)"
  //     return [];
  //   }
  out= 0;
  dimp= dim; dimp(1)= 1;
  n1= [n-1];
  for (i=2, j=1; i<=dim(1); i++, j++) dimp(i)= dimp(j) * dim(i);
  if (ndxdimlast==1&&!dimsof(n)(1)==0) {
    for (i=dim(1); i>0; i--)out += n1(..,i,) * dimp(i);
  } else {
    for (i=dim(1); i>0; i--)out += n1(i,..) * dimp(i);
  }
  return shrink1(out+1);
}

/* ------------------------------------------------------------------------ */

func digitize_nn(x, bins)
/* DOCUMENT

   SEE ALSO:
*/
{
  n= numberof(bins);
  i= max(2,min(digitize(x,bins),n));
  m= x-b(i-1)<b(i)-x;
  return merge2(i-1,i,m);
}

/* ------------------------------------------------------------------------ */

func shrink1(x)
/* DOCUMENT shrink1(x)
 *    returns array X reshaped according to its initial dimension list DIMLIST.
 *    except unary dimensions which are removed.
 * SEE ALSO: array, dimsof, reform
 */
{
  if (is_scalar(x))return x;
  dims= dimsof(x);
  if (allof(dims(2:0)==1))return x(1);
  w= (dims(2:0)==1);
  if (anyof(w)) {
    dims=grow(dims(1)-w(sum),dims(where(!w)+1));
  }
  if (dims(1)) {
    y= array(structof(x), dims);
    y(*)= x(*);   /* will blow up if lengths differ */
  }

  return y;
}

/*---------------------------------------------------------------------------*/

func numbfromdims (dims)
{
  if (numberof(dims)!=(dims(1)+1)) error,"non-conforming dims";
  for (i=2,n=1;i<=(dims(1)+1);i++)n*=dims(i);
  return n;
}

/*-----------------------------------------------------------------------*/

func discrete2 (x,plot=,count=)
/* DOCUMENT discrete(x)
   return an array(typeof(x),[2,number_of_different_X,count_of_these_values]
*/
{
  s= sort(x);
  xsd= _(x(s)(dif),1);
  wo= where(xsd!=0);
  if (numberof(wo)>1) {
    dis= x(s)(wo); // set of disjoint values
    if (count==1 || plot==1) {
      cnt= histo(x,dis)(2:);
      if (plot==1)
        pldj,dis,array(0,numberof(dis)),dis,cnt;
      return [dis,cnt];
    } else
      return dis;
  } else {
    if (count==1)
      return [x(1),[numberof(x)]];
    return x(1);
  }
}

/*-----------------------------------------------------------------------*/

func discrete (x)
/* DOCUMENT discrete(x)
   return an array(typeof(x),[2,number_of_different_X,count_of_these_values]
*/
{
  s= sort(x);
  xs= x(s);
  if (dimsof(xs)(1)==0 || dimsof(xs)(2)==1) {
    xs= [xs];
    xsd= [1];
  } else
    xsd= _(xs(2:)!=xs(:-1),1);
  wo= where(xsd!=0);
  if (numberof(wo)>=1) {
    dis= xs(wo); // set of disjoint values
    return dis;
  } else
    return x(1);
}

/*-----------------------------------------------------------------------*/

func intersect (x,y)
/* DOCUMENT
   Return the elements common to two given arrays.
   z= intersect(x,y)
   x, y= arrays (not necessarily same size).  in
   z   = array of elements in common.         out
   Note: if z is a scalar 0 then no elements were
   in common.
*/
{
  if (numberof(x)==1)
    if (anyof(y==x(1)))
      return x;
    else
      return;
  if (numberof(y)==1)
    if (anyof(x==y(1)))
      return y;
    else
      return;

  xs= _(discrete(x), discrete(y));
  xs= xs(sort(xs));
  xsd= _(xs(2:)!=xs(:-1),1);
  wo= where(xsd==0);

  if (numberof(wo)>=1)
    return xs(wo);
  else
    return;
}

/*-----------------------------------------------------------------------*/

func  setminus (x,y)
/* DOCUMENT
   Return the elements common to two given arrays.
   z= setminus(x,y)
   x, y= arrays (not necessarily same size).  in
   z   = array of elements in x that are not in y.    out
   Note: if z is a scalar 0 then no elements were
   in common.
*/
{
  require, "msort.i";
  dx= discrete(x);
  dy= discrete(y);
  xs= _(dx, dy);   // Merge the 2 arrays.
  msk= _(array(1, numberof(dx)), array(2, numberof(dy)));
  sxs= msort(xs,msk);
  xs= xs(sxs);              // Sort.
  msk= msk(sxs);
  xsd= _(xs(2:)!=xs(:-1), 1);
  wo= where(xsd==1&msk==1);
  if (numberof(wo)>=1) {
    return xs(wo);
  } else {return;}
}

/*-----------------------------------------------------------------------*/

func settest(picks,mainset,subset=)
/* DOCUMENT settest(picks,mainset)
   Checks that all elements in picks belong to
   the mainset;
   if (subset==1)no errors & subsets only the picks
   which belong to mainset
   [1,1]= settest([1,1,2],[1],subset=1)
*/
{
  if (is_void(picks))return;
  np= numberof(picks);
  out= [];
  for (ip=1;ip<=np;ip++) {
    blg= picks(ip)==mainset;
    if (noneof(blg)) {
      if (subset!=1)error;
    } else {
      if (subset==1)out= _(out,picks(ip));
    }
  }
  if (subset==1)return out;
  return picks;
}

/*-----------------------------------------------------------------------*/
/*---------------------------------------------------------------------------*/

func polyf(x, a, &grad, deriv=, derivx =, derivxx=)
/* DOCUMENT polyf(x, a, &grad, deriv=, derivx =)
   res=lmfit(polyf,x,a,y,x^2)"
   plg,polyf(x,a),x,type="dash"
   polyf= x^(i-1)(..,i:1:np)(..,+)*a(+,..) ,  i=1,..,np=dimsof(a)(2)     sorta
*/
{
  if (deriv==1 && (derivx==1 || derivxx==1))
    error,"single deriv at a time.";
  da= dimsof(a);
  if (da(1)<1) {
    if (derivx==1 || derivxx==1)
      grad= array(double,dimsof(x));
    if (deriv==1)
      grad= 1.0;
    else
      if (derivx!=1 && derivxx!=1)
        grad= [];
    y= array(a(1),dimsof(x));
    return y;
  }

  np= da(2);
  dx= dimsof(x);
  dy= grow(dx,np); dy(1) += 1;
  y= array(structof(x), dy);
  y(..,1)= 1.;
  for (i=2;i<=np;i++) y(..,i)= y(..,i-1)*x;
  if (is_void(deriv)) deriv=0;
  if (is_void(derivx)) derivx=0;

  if (derivx && deriv) {
    error,"grad returns EITHER deriv_a (deriv=1) OR deriv_x (derivx=1)";
  }

  if (deriv) {
    grad= y;
  } else if (derivx && np>1) {
    b= a(2:0,..) * indgen(np-1);
    grad= y(..,1:-1)(..,+)*b(+,..);
  } else if (derivxx && np>2) {
    b= a(3:0,..) * indgen(np-2)*indgen(2:np-1);
    grad= y(..,1:-2)(..,+)*b(+,..);
  }

  y= y(..,+)*a(+,..);
  return y;
}

/*-------------------------------------------------------------------------------------*/

struct pytfx{pointer x; long signx; long signa;}

  func pytf(x, a, &grad, deriv=, derivx =) {
  /* DOCUMENT pytf(x, a, &grad, deriv=, derivx =)
     Where: x  ==  struct pytfx{pointer x; long signx; long signa;}
     x.signa and z.signx default to +1
     function pytf == a(1) + a(2)*(*x.x) + a(3)*SQRT(x.signa * a(4)^2 + x.signx * (*x.x)^2)
     Example: x=span(-1.,1.,200);plg,pytf(pytfx(x=&x,signa=1,signx=1),10),x
     to use with
     lmfit(f,x,&a,y,w,fit=,correl=,stdev=,gain=,tol=,deriv=,itmax=,lambda=,
     eps=,monte_carlo=)
  */
  dx= dimsof(*x.x);
  if (x.signa==0)x.signa=1;
  if (x.signx==0)x.signx=1;
  y= sqrt(x.signa * a(4)^2 + x.signx * (*x.x)^2);
  if (is_void(deriv))deriv=0;
  if (is_void(derivx))derivx=0;
  if (!deriv&&!derivx) {         //donnot compute any derivatives
    return a(1) + a(2)*(*x.x) +  a(3)*y;
  } else if (deriv||derivx) {     //compute one of the derivatives
    if (derivx&&!deriv) {
      grad= a(2) + a(3)*x.signx*(*x.x)/y;
    } else if (deriv&&!derivx) {
      dgrad= grow(dx,4); dgrad(1) += 1;
      grad= array(structof(*x.x), dgrad);
      grad(..,3)= y
        grad(..,4)= a(3)*x.signa*a(4)/y;
      grad(..,2)= *x.x;
      grad(..,1)= 1;
    } else if (derivx&&deriv) {
      error,"grad returns EITHER deriv_a (deriv=1) OR deriv_x (derivx=1)"
        }
    return a(1) + a(2)*(*x.x) +  a(3)*y;
  }
}

/*-------------------------------------------------------------------------------------*/

func gauss2df (xy, a, &grad, deriv=)
/* DOCUMENT gauss2df (xy, a, &grad, deriv=)
   I0 * exp(-0.5*(X^2+Y^2))/(1+a(7)*X^2)/(1+a(8)*Y^2)
   x= xy(..,1)
   y= xy(..,2)
   X= ((x-x0)*cos(alpha)+(y-y0)*sin(alpha))/dx
   Y= ((y-y0)*cos(alpha)-(x-x0)*sin(alpha))/dy
   I0= a(1)  // mag
   x0= a(2)  // x center
   y0= a(3)  // y center
   dx= a(4)  // x sigma
   dy=a(5)   // y sigma
   alpha=a(6)  // rotation angle

   Works with lmfit, and can return derivates.
   Notes: FHWM=sigma*2*sqrt(2*alog(2)); sum(gauss2d)=2*pi*I0*dx*dy
   adapted from E. Thiebaut
   SEE ALSO:
*/
{
  npar= numberof(a);
  if (npar>8) error,"too many parameters.";
  eps= 1e-100;
  if (abs(a(4))<eps)
    dx1= sign(a(4))/eps;
  else
    dx1= 1./a(4);
  if (npar>=5) {
    if (abs(a(5))<eps)
      dy1= sign(a(5))/eps;
    else
      dy1= 1./a(5);
  } else
    dy1= dx1;

  alpha= npar>=6?a(6):0.;

  X= ((deltax=(xy(..,1)-(x0=a(2))))*(cosa=cos(alpha))+
      (deltay=(xy(..,2)-(y0=a(3))))*(sina=sin(alpha)))*dx1;
  Y= (deltay*cosa-deltax*sina)*dy1;

  u1= exp(-0.5*(r2=(X^2+Y^2)));
  res1= a(1)*u1;

  pol= 1.0;
  if (npar>=7) pol*= 1/(1+a(7)*deltax^2);
  if (npar>=8) pol*= 1/(1+a(8)*deltay^2);
  res= res1*pol;

  if (deriv) {
    grad= array(1.,dimsof(X),npar);
    grad(..,1)= u1;
    grad(..,2)= ((cosa*dx1)*X-(sina*dy1)*Y)*res;
    grad(..,3)= ((sina*dx1)*X+(cosa*dy1)*Y)*res;
    grad(..,4)= dx1*X^2*res;  //X^2
    if (npar>=5)
      grad(..,5)= dy1*Y^2*res;
    else
      grad(..,4)+= dy1*Y^2*res; // assume comon with param for both

    if (npar>=6) grad(..,6)= X*Y*(dy1/dx1-dx1/dy1)*res;//<==

    if (npar>=7) {
      grad(..,2)+= ((cosa*dx1)*X-(sina*dy1)*Y)*res;
      grad(..,3)+= ((sina*dx1)*X+(cosa*dy1)*Y)*res;
      grad(..,4)+= dx1*X*res;  //X^2
      if (npar>=5)
        grad(..,5)+= dy1*Y*res;
      else
        grad(..,4)+= dy1*Y*res; // assume comon with param for both

    }
  }

  return res;
}
/*-----------------------------------------------------------------*/

func interp2reg (z, x1, x0, y1, y0, xp, yp, outside=,bad=)
/* DOCUMENT zp= interp2reg(z(ix,iy),x1,x0,y1,y0,xp,yp,outside =)
   interpolated regularly gridded z(nX, nY) & [xp,yp] where x/y=span(x/y1,x/y0,nx/y)

   Points outside the mesh get the value 0.0, unless the outside
   keyword is non-nil, in which case they get that value.
   SEE ALSO: interp2 in digit2.i
*/
{
  d= dimsof(z);
  if (d(1)!=2)
    error,"only two dimensions allowed";
  n= d([2,3]);

  if (is_void(y1) && is_void(y0)) {
    y1= 1.0;
    y0= double(n(2));
  }
  if (is_void(x1) && is_void(x0)) {
    x1= 1.0;
    x0= double(n(1));
  }

  if (is_void(xp)) { // assume the sampling points are given as [2, 2([x,y]), #_of_xyp]
    xp= yp(1,..);
    yp= yp(2,..);
    if (is_scalar(xp)) {
      nx= 1;
    } else {
      nx= dimsof(yp)(2);
    }
  } else {
    nx= numberof(yp);
    if (numberof(xp)!=nx)
      error,"yp and xp (in order) must be two id. dim. arrays";
  }

  dout= dimsof(xp+yp);

  if (anyof(dout!=dimsof(xp)))
    xp= xp + array(structof(xp(1)),dout);
  if (anyof(dout!=dimsof(yp)))
    yp= yp + array(structof(yp(1)),dout);

  stridex= float(x0 - x1)/(n(1)-1);
  stridey= float(y0 - y1)/(n(2)-1);

  xi= (xp - x1)/stridex;
  yj= (yp - y1)/stridey;

  i= long(xi);
  j= long(yj);

  xi= xi - i;
  yj= yj - j;

  i +=1;
  j +=1;

  if (is_void(outside))
    outside= 0.;
  out= array(structof(z)(outside), dimsof(yp));

  w= where(i >= 1 & i <= (n(1)-1) & j >= 1 & j <= (n(2)-1));
  if (numberof(w)>0) {
    if (is_void(bad)) {
      ij= transpose([i(w),j(w)]);
      xi= xi(w);
      yj= yj(w);
      out(w)=  (1-xi) * (1-yj) *  z(dimin(ij          ,d)) +
        (1-xi) *    yj  *  z(dimin(ij+[0,1](,-),d)) +
        xi  *    yj  *  z(dimin(ij+[1,1](,-),d)) +
        xi  * (1-yj) *  z(dimin(ij+[1,0](,-),d));
    } else {
      ij= transpose([i(w),j(w)]);
      xi= xi(w);
      yj= yj(w);
      z1= z(dimin(ij          ,d));
      z2= z(dimin(ij+[0,1](,-),d));
      z3= z(dimin(ij+[1,1](,-),d));
      z4= z(dimin(ij+[1,0](,-),d));
      ww= where(z1==bad|z2==bad|z3==bad|z4==bad);
      out(w)=  (1-xi)*(1-yj)*z1 + (1-xi)*yj*z2 + xi*yj*z3 + xi*(1-yj)*z4;
      if (numberof(ww))out(w(ww))= outside;
    }
  }
  return out;
}

/*-------------------------------------------------------------------------------------*/

func poly2(x, c) {
  /* DOCUMENT poly2(x,c)
     returns the polynomial  c(1,..) + c(2,..)*x + ... + c(N,..)*X^N
     The data type and dimensions of the result, and conformability rules
     for the inputs are identical to those of the expression above.
     It doesn't get any UGLIER than this ...
     SEE ALSO: poly
  */
  if (is_scalar(c))c=[c];
  d= dimsof(c);
  d1= d(2);
  if (d1 == 1)return poly(x,c(1,..));
  if (d1 == 2)return poly(x,c(1,..),c(2,..));
  if (d1 == 3)return poly(x,c(1,..),c(2,..),c(3,..));
  if (d1 == 4)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..));
  if (d1 == 5)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..));
  if (d1 == 6)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..));
  if (d1 == 7)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..));
  if (d1 == 8)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..));
  if (d1 == 9)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..));
  if (d1 ==10)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..));
  if (d1 ==11)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..));
  if (d1 ==12)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..));
  if (d1 ==13)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..),c(13,..));
  if (d1 ==14)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..),c(13,..),c(14,..));
  if (d1 ==15)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..),c(13,..),c(14,..),c(15,..));
  if (d1 ==16)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..),c(13,..),c(14,..),c(15,..),c(16,..));
  if (d1 ==17)return poly(x,c(1,..),c(2,..),c(3,..),c(4,..),c(5,..),c(6,..),c(7,..),c(8,..),c(9,..),
                          c(10,..),c(11,..),c(12,..),c(13,..),c(14,..),c(15,..),c(16,..),c(17,..));
  n= 17;
  if (d1 > n)return poly2(x,c(1:n)) + x^n*poly2(x,c(n+1:0));
}

/*-------------------------------------------------------------------------------------*/

func centroid (a,norm=)
{
  ab= abs(a);
  if (norm && norm>1)
    ab= ab^norm;
  ab/= sum(ab);
  da= dimsof(a);
  out= array(0,da(1));
  for (i=1;i<=da(1);i++) {
    ind= indgen(da(i+1));
    for (j=1;j<i;j++)
      ind= ind(-,);
    out(i)= nint(sum(ind*ab));
  }
  return out;
}

/*-------------------------------------------------------------------------------------*/

func sumintpow(n,a)
/* DOCUMENT sumintpow(n,a)
   FUN!: returns double= SUM_{1:n} n^a, n & a are positive integers
*/
{
  n= long(n);
  a= long(a);
  if (n<0 || a<0)error,"sumintpow args are positive integers";
  cof= array(double, a+1, a+1);
  cof(1,1)= 1.;
  for (i=2;i<=a+1;i++) {
    cof(1:i-1,i)= cof(1:i-1,i-1) * (i-1.)/(i+1.-indgen(1:i-1));
    cof(i,i)= 1 - cof(1:i-1,i)(sum);
  }
  return poly2(n,grow([0.],cof(::-1,0)));
}

/*-------------------------------------------------------------------------------------*/

func tile (dd,dtlo,center=)
/* DOCUMENT tile (dd,dtlo)
   OBSOLETE: use tile.i
   2D rectangular tiling of 2D rectangular domain.  Output is
   base index (lower left in "x-y/dim1-2" pic.) of each tile.
   dd=    data dimension as in dimsof(data)
   dtlo=  dimension ONLY (no preceeding rank) of output tile.
   The tile is just the chunk size wich, in tiling with overlap,
   is also the non-overlapping tile dimension (output dimensions.)

*/
{
  dtlo*= array(1,dd(1));
  if (center==1)
    off= (dd(2:)%dtlo)/2;
  else
    off= array(0,dd(1));

  dt= long(ceil((dd-_(0,off))*1.0/_(1,dtlo))); // d-dimensional tile index dims
  for (n=1,i=1;i<=dt(1);i++)
    n*= dt(i+1);
  out= reform((indim(indgen(n),dt)-1)*dtlo+1,_(dt(1)+1,dt(1),dt(2:)))+off;
  return out;
}

func equidx (y, x, n)
/* DOCUMENT xx= equidx(y,x,n); // dimsof(x)==[1,n]
   XX are the abcissas of N equidistent points in XY plane, along Y(X) curve.
   Y is either comformable with X, or has an additional *leading* dimension
   X coordinate to sample:
   m= 20;
   n= 200;
   x= span(0,1,n);
   y= gaussm(random_n(n),40)*10;
   xx= equidx(y,x,m); yy= interp(y,x,xx);
   xxx= span(0,1,m); yyy= interp(y,x,xxx);
   dd= y-interp(yy,xx,x); ddd= y-interp(yyy,xxx,x);
   statarr,dd;
   statarr,ddd;
   fma;
   plg,y,x;
   plg,yy,xx,color="red";
   plg,yyy,xxx,color="blue";
   SEE ALSO:
*/
{
  if (dimsof(x)(1)!=1)
    error,"Expecting 1-dim array.";
  if (dimsof(y)(1)==1)
    y= y(-,..);
  d= sqrt((y(,dif)(,pcen)^2)(sum,)+x(dif)(pcen)^2)(cum);
  return interp(x,d(:-1),span(0,d(-1),n));
}

/*-------------------------------------------------------------------------------------*/

func tile_ndx (it,dd,dtlo,ovl,&din,&dout,&tin,&tout)
/* DOCUMENT  tile_ndx (it,dd,dtlo,&din,&dout,&tin,&tout)
   OBSOLETE: use tile.i
   first inputs id as those of "tile";
   dd=    data dimension as in dimsof(data)
   dtlo=  dimension ONLY (no preceeding rank) of output tile.
   ovl=   one-side overlap (0 if none)
   USAGE
   ====
   test driver.....
   func tile_test (dum)
   {
   dd= [2,100,400];
   dtlo= [14,41];
   ovl= [5,6];
   local din, dout, tin, tout;
   tile_ndx, (it=tile(dd,dtlo)), dd, dtlo, ovl, din, dout, tin, tout;
   tl= array(0.0,_(dd(1),dtlo+2*ovl));
   im= roll(span(0,1,dd(2))*span(0,1,dd(3))(-,));
   im2= array(structof(im),dimsof(im));
   for (i=1;i<=numberof(it(1,));i++) {
   tl(*)= 0;
   tl(tin(1,1,i):tin(2,1,i),tin(1,2,i):tin(2,2,i))=
   im(din(1,1,i):din(2,1,i),din(1,2,i):din(2,2,i));
   im2(dout(1,1,i):dout(2,1,i),dout(1,2,i):dout(2,2,i))=
   tl(tout(1,1,i):tout(2,1,i),tout(1,2,i):tout(2,2,i));
   pli,im2;
   }
   plmk,it(2,*)-1,it(1,*)-1,width=30,color="red",marker=1,msize=.3;
   return im-im2;
   }
*/
{
  n= dd(1);
  dtlo*= array(1,n);
  ovl*= array(1,n);
  dtli= dtlo+2*ovl;

  // data in/out indices (second index is min/max)
  dout= [it, min(it+dtlo-1,dd(2:))];               //regular tiling of size dtlo
  din= [max(1,it-ovl),min(it+dtli-ovl-1,dd(2:))];

  // chip-tile in/out indices (second index is min/max)
  tout= [ovl+1,ovl+dout(..,2)-dout(..,1)+1];
  tin= [din(..,1)+ovl-it+1,ovl-it+din(..,2)+1];

  if (dimsof(it)(1)>=dd(1)) {
    din= transpose(din,2);
    tin= transpose(tin,2);
    dout= transpose(dout,2);
    tout= transpose(tout,2);
  }
}

/*--------------------------------------------------------------------*/

func normal (f,y,x,&area,center=)
/* DOCUMENT normal(f,y,x,&area)
   normal at zone center
*/
{
  /* Start with the two median vectors across each zone.  */

  if (center==1) {
    f= ptcen(f);
    x= ptcen(x);
    y= ptcen(y);
  }

  fdz= f(dif,zcen);
  fzd= f(zcen,dif);
  xdz= x(dif,zcen);
  xzd= x(zcen,dif);
  ydz= y(dif,zcen);
  yzd= y(zcen,dif);

  /* Estimate the gradient at the center of each cell.  */

  d= dimsof(f);
  grad= array(double,_(3,3,d(2)-1,d(3)-1));

  area= xdz*yzd - xzd*ydz;
  grad(1,..)= -(fdz*yzd - fzd*ydz)/area;
  grad(2,..)= -(xdz*fzd - xzd*fdz)/area;
  grad(3,..)= 1;
  grad/= abs(grad(1,..),grad(2,..),grad(3,..))(-,);

  area= abs(area);

  return grad;
}

/* ------------------------------------------------------------------------ */

func firstupcase(s,n=,p=)
{
  if (is_void(n))
    n= 30;
  if (is_void(p))
    p= "(^[A-Z])|(( +)([a-z]))";
  s= streplace(s,[0,1],strcase(1,strpart(s,1:1)));
  w= strgrep(p,s,sub=[1,2],n=n);
  ss= strpart(s,w);
  ss= strcase(1,ss);
  return streplace(s,w,ss);
}

/* ------------------------------------------------------------------------ */

func strplt(s,nospace=)
{
  if (s==string(0))
    return s;
  if (nospace!=1) {
    q= strpart(s, strword(s,"_",20));
    w= where(q!=string(0));
    if (numberof(w)>1) {
      q(w(:-1))+="!_";
      s= q(sum);
    }
  } else {
    q= strpart(s, strword(s," ",20));
    w= where(q!=string(0));
    if (numberof(w)>1) {
      q(w(:-1))+="-";
      s= q(sum);
    }
  }
  return s;
}


/*---------------------------------------------------------------------------*/

func strsplit(stra, delim)
/* DOCUMENT strsplit(str, delim)
 * Decompose an  array, or scalar, string STRA into a group of, or single if scalar,
 * array of component strings which are separated by string DELIM in original.
 * IFF DELIM is not found anywhere in array, return original string array
 * (Reverses the action of STRCOMBINE)
 * SEE ALSO: strrepl, strcombine
 */
{
  n= numberof(stra);
  out= save();

  if (anyof((m=strgrepm("^.*"+delim,stra)))) {
    w= where(m);
    be= strfind(delim,stra(w),n=50);
    j= 0;
    for (i=1;i<=n;i++) {
      if (m(i)) {
        j+= 1;
        k= be(1:min(where(be(,j)<0))-1,j);
        save,out,string(0),strpart(stra(w(j)),grow([0],k));
      } else
        save,out,string(0),stra(i);
    }
    if (n==1)
      out= out(1);
  } else
    out= stra;

  return out;
}

/* ------------------------------------------------------------------------ */

func strcombine (stra, delim)
/* DOCUMENT strcombine(stra,delim)
 * Concatenate an array of strings STRA, or a gtoup of such arrays, into a
 * single string, or an array of sting in 2nd case, where each original substring
 * separated by character DELIM (which should not occur in
 * any of the strings.
 * If DELIM is the null string, simply all concatenate all the strings in
 * STRA.
 * (Reverses the action of STRSPLIT)
 * SEE ALSO: strrepl, strsplit
 */
{
  if (is_group(stra)) {
    n= stra(*);
    out= array(string,n);
    for (i=1;i<=n;i++)
      out(i)= strcombine(stra(noop(i)),delim);
    return out;
  }

  catstr= "";
  nstr= numberof(stra);
  if (delim!="")
    if (anyof(strgrepm(delim,stra)))
      error, "Error - delimiter occurs in string";
  for (j=1;j<=nstr;j++) {
    if (strlen(stra(j))>0) {
      if (j==1) {
        catstr= stra(j);
      } else {
        catstr= catstr+delim+stra(j);
      }
    }
  }
  return catstr;
}

func stripcode (l)
/* DOCUMENT
   remove comments and continuations from code for easier editing.
   ony used in oxread -- which should only be used with oxwrite
   SEE ALSO: oxread
*/
{
  // remove double backlash comments and empty lines
  // .. beginning of line
  m= !strgrepm("^[ \t]*\/\/",l);
  if (anyof(m))
    l= l(where(m));
  // .. @end of line,
  m= strgrepm("(.+)(\/\/[^\/;]*$)+",l);
  if (anyof(m)) {
    ss= l(where(m));
    l(where(m))= strpart(ss,strgrep("(.+)(\/\/[^\/;]*$)+",ss,sub=1));
  }
  l= strtrim(l);
  l= l(where(l));

  // other comments
  sgip= strgrep("\/\\*",l);
  sgim= strgrep("\\*\/",l);
  o= save();
  w= where(sgip(2,..)>=0);
  for (i=1;i<=numberof(w);i++) {
    wi= w(i);
    j= sgip(..,wi);
    if (sgim(2,wi)>=0) { // open close on same line
      l(wi)= streplace(l(wi),[sgip(1,wi),sgim(2,wi)],"");
    } else {
      l(wi)= streplace(l(wi),[sgip(1,wi),strlen(l(wi))],"");
      k= 0;
      while (sgim(2,wi+(++k))<0)
        l(wi+k)= string(0);
      l(wi+k)= streplace(l(wi+k),[0,sgim(2,wi+k)],"");
    }
  }
  l= strtrim(l);
  l= l(where(l));

  // process line continuations
  while (anyof((m=strgrepm("\\\\$",l)))) {
    w= where(m);
    l(w)= strpart(l(w),:-1);
    l(w)+= l(w+1);
    l= l(where(_(0,m(:-1))));
  }
  return l;
}

/* ------------------------------------------------------------------------- */

func nameofstream(f)
{
  print_format,300;
  sf= print(f);
  print_format;
  if (typeof(f)=="stream") {
    return strconcat(strtrim(strtok(sf,":")([4,2])));
  } else if (typeof(f)=="text_stream") {
    return strtrim(strtok(strtok(sf,":")(4),":")(2));
  } else {
    error,"not a stream";
  }
}

/* ------------------------------------------------------------------------ */

func typeconv(typestr,var)
/* DOCUMENT typeconv(typestr,var)
 * Convert variable VAR to type TYPESTR, and return the converted variable
 * TYPESTR may be one of the following:
 * "char", "int", "long", "float", "double", "complex", "fcomplex", "string"
 * NOTE: for "fcomplex" imaginary parts are ZERO, and are the "inner-most" index
 *       2 of 2.
 * SEE ALSO: typeof, array
 */
{

  str = strcase(0,typestr);

  if (str == "char") {
    return ( char(var));
  } else if (str == "short") {
    return ( short(var));
  } else if (str == "int") {
    return ( int(var));
  } else if (str == "long") {
    return ( long(var));
  } else if (str == "float") {
    return ( float(var));
  } else if (str == "double") {
    return ( double(var));
  } else if (str == "complex") {
    return ( complex(var));
  } else if (str == "fcomplex") {
    dvar = dimsof(var);
    tvar = typeof(var);
    if(tvar == "complex"){
      return complexcast(float,var);
    }else if(tvar == "float"){
      return transpose([var,array(0.0f,dvar)],[dvar(1)+1,1]);
    }else if(tvar == "double"){
      return transpose([float(var),array(0.0f,dvar)],[dvar(1)+1,1]);
    }else if(is_integer(var)){
      return typeconv(str, float(var));
    }else{
      error,"var is "+tvar;
    }
  } else if (str == "string") {
    return ( string(var));
  } else {
    error, "Invalid type '" + str + "'";
  }

}

/*-------------------------------------------------------------------*/

func plot_hbar (levs0, colors, offset=, wrap=, off=)
/* DOCUMENT plot_hbar, levs, colors

   plot a horizontal color bar below the viewport

   levs defines the levels seperating the region
   colors define the index into the color bar to be used between each level

   if (is_void(levs) || is_void(colors)) error," both levs and colors must be defined";

*/
{

  // coords of viewport
  port= viewport();
  xd= port(2)-port(1);
  yd= port(4)-port(3);

  if (is_void(offset)) offset= 0.2;

  xl= port(1);
  xr= xl + xd;
  yb= port(3)-yd*offset;
  yt= yb + yd*offset/4.;

  ylabs= yb - 0.02;
  levs= levs0;
  if (numberof((w=where(abs(levs)/levs(rms)<1e-12)))) levs(w)= 0; // kludge
  nlevs= numberof(levs);
  nc= nlevs+1;

  zcol= array(short,1,nc);
  zcol(1,)= indgen(1:nc:1);

  //plot colors
  lastsys= plsys_get();
  plsys, 0
    if (!is_void(colors)) {
      if (dimsof(colors)(0)==3 && structof(colors)==char)
        pli, [colors], xl, yb, xr, yt;
      else
        pli, char(transpose(colors(zcol))), xl, yb, xr, yt;
    } else {
      if (!is_void(wrap))
        pliwrap, transpose(zcol), xl, yb, xr, yt, wrap=wrap, off=off;
      else
        pli, transpose(zcol), xl, yb, xr, yt;
    }


  //plot labels;
  if (nc > 10) {
    // we want no more than 10 labels
    num_colors_per_label= nc/10.
      nlabs= int(nc/(int(num_colors_per_label)+1));
    nskip= nlevs/nlabs; // number of levels to skip;
    //read, prompt="debug",dummy;
  } else {
    nlabs= nc;
    nskip= 1;
  }

  //print, "nlevs, nlabs, nskip", nlevs, nlabs, nskip;

  xndc= span(xl,xr,nlabs);
  yndc= ylabs;
  xdd= xd/(nlevs+1);
  for (i=1;i<=nlevs;i=i+nskip)
    plt,swrite(levs(i),format="%.3g"),xl+i*xdd,ylabs,height=12,justify="CH";

  pldj, xl, yb, xr, yb;
  pldj, xl, yt, xr, yt;
  pldj, xl, yb, xl, yt;
  pldj, xr, yb, xr, yt;

  plsys_set, lastsys;

}

/*---------------------------------------------------------------------------*/

func plt_clic(strg, q, orient=, height=, justify=, font=, color=, hide=, opaque=,prompt=)
/* DOCUMENT plt_clic(strg, orient=, height=, justify=, font=, color=, hide=, opaque=,prompt=)
   plt, strg, q(5), q(6),font=font, justify=justify, height=height, orient=orient,\
   color=color,hide=hide,opaque=opaque
   [x_pressed, y_pressed, x_released, y_released,
   xndc_pressed, yndc_pressed, xndc_released, yndc_released,
   system, button, modifiers]
*/
{
  if (prompt==1)
    write,"clic in plot window to position string\n"+strg;
  if (is_void(q)) {
    do {q= mouse(-1,0,"");} while(is_void(q));q;
  }
  if (is_void(height))
    height=pltitle_height;
  if (is_void(justify))
    justify="CH";
  if (is_void(font))
    font=pltitle_font;

  plt, strg, q(5), q(6),font=font, justify=justify, height=height, orient=orient,\
    color=color,hide=hide,opaque=opaque;
  return q;
}

/*---------------------------------------------------------------------------*/

func pltit(title, xtitle, ytitle, adjust, height=, font=, under=,right=)
/* DOCUMENT pltit, title, xtitle, ytitle, [deltay,deltax,deltay]
   Plot TITLE centered above the coordinate system for any of the
   standard Gist styles.
   Plot XTITLE horizontally under the viewport and YTITLE vertically
   to the left of the viewport.  If the tick numbers interfere with
   the labels, you can specify the [DELTAX,DELTAY] in NDC units to
   displace the labels.  (Especially for the y title, the adjustment
   may depend on how many digits the numbers on your scale actually
   have.)  Note that DELTAX moves YTITLE and DELTAY moves XTITLE.
   borrows viewport from current plotting coordinate system
   DEFAULTS: pltitle_height= 18;
   pltitle_font= "helvetica";

   SEE ALSO: plt, pltitle, xytitles
*/
{
  if (is_void(adjust)) adjust= [0.,0.,0.];
  if (is_void(height)) height= 12;
  if (is_void(font)) font= pltitle_font;

  if (strpart(font,0:0)!="B")font += "B";

  port= viewport();

  if (under) {
    if (!is_void(title))title=strplt(title);
    if (!is_void(xtitle))xtitle=strplt(xtitle);
    if (!is_void(ytitle))ytitle=strplt(ytitle);
  }

  if (title && strlen(title))
    plt, title, port(zcen:1:2)(1), port(4)+ 0.02+ adjust(1),
      font=font, justify="CB", height=height;
  if (xtitle && strlen(xtitle))
    plt, xtitle, port(zcen:1:2)(1), port(3)-0.04+adjust(3),
      font=font, justify="CT", height=height;
  if (ytitle && strlen(ytitle))
    if (right) {
      plt, ytitle, port(2)+0.065-adjust(2), port(zcen:3:4)(1),
        font=font, justify="CH", height=height, orient=1;
    } else {
      plt, ytitle, port(1)-0.065+adjust(2), port(zcen:3:4)(1),
        font=font, justify="CH", height=height, orient=1;
    }

}

func randrgb(a,b,c,d)
{
  require, "color.i";
  return transpose(to_rgb([random(a,b,c,d)*470,1,0.5*(1+random(a,b,c,d))]));
}
/*---------------------------------------------------------------------------*/
/* ------------------------------------------------------------------------ */

func colorbar(cmin, cmax, offset=, colors=, tit=, height=)
/* DOCUMENT colorbar
   colorbar, cmin, cmax, offset=, colors=, tit=, height=
   draw a color bar to the right of the plot.  If CMIN and CMAX
   are specified, label the top and bottom of the bar with those
   numbers.
   SEE ALSO: plot_hbar
*/
{
  thisys= plsys();

  plsys,thisys;          // why is this necessary !?!?!

  port= viewport(); //[xmin,xmax,ymin,ymax]

  wx= port(2)-port(1);
  wy= port(4)-port(3);

  if (is_void(offset))offset= [0.,0.];
  if (numberof(offset)<2)offset= [0.,0.];

  //  ^ off viewport                  ^   width
  x0= 0.03 + port(2) + offset(1); x1= 0.05*wx + x0;
  //wy*= 0.8;
  y0= port(3:4)(avg)-wy/2;  y1= wy  + y0;

  xmid= (x0+x1)/2.;

  plsys,0;

  if (is_void(colors))colors=span(0,1,200);
  pli, colors(-,), x0, y0, x1, y1, legend=""; //x0, y0, x1, y1

  plg, [y0,y1,y1,y0],[x1,x1,x0,x0], closed=1,
    marks=0,color="fg",width=1,type=1,legend="";

  plsys, thisys;

  height= !is_void(height)? height: 14;
  if (!is_void(cmin)) {
    plt, swrite(cmin,format="%-#06.3lg"), xmid, y0-0.004, justify="CT", height=height;
    plt, swrite(cmax,format="%-#06.3lg"), xmid, y1 , justify="CB", height=height;
  }

  if (tit)
    plt, tit, xmid, y1+0.02 , justify="CB", height=height;
}

func pltitre (title, opaque=)
/* DOCUMENT pltitre, title, opaque=;
   Plot TITLE centered above the coordinate system for any of the
   standard Gist styles.  You may want to customize this for other
   plot styles.
   The external variables pltitle_height, pltitle_font, pltitle_xadj,
   and pltitle_yadj determine the font and position of the title,
   if you want to change those.
   SEE ALSO: plt, xytitles
*/
{
  port = viewport();
  x = port(zcen:1:2)(1) + pltitle_xadj;
  y = port(4) + pltitle_yadj;
  plt, title, x, y, font=pltitle_font, justify="CB", \
    height=pltitle_height,opaque=opaque;
}

/*----------------------------------------------------------------------*/

func pleb(y, x, dx=, dy=, mfill=, color=, width=, marker=, msize=, type=)
/* DOCUMENT pleb, y, x, dx=dx, dy=dy
   plots Y vs. X with error bars.

   Uncertainty on X and/or Y are specified with the dx= and dy= keywords.
   X and Y must have same dimensions, dx= and dy= must be conformable
   with X (or Y).  Either dx or dy may be nil for no error bar in that
   direction.  Scalar dx or dy gives equal error bars at all points,
   dimsof(dx)==dimsof(X), etc., gives different error bar at each point.
   dx= and dy= may also have a trailing dimension of length 2 in order
   to get asymmetric error bars; dx(..,1) is the lower error bar length,
   and dx(..,2) is the upper error bar length in that case, etc.

   If marker=, msize=, or width= is specified, markers are positioned
   at X, Y using plmk.  Use the mfill=1 keyword to get filled markers
   (width>=10. in plmk; width= refers to error bar width in pleb).

   EXAMPLE:
   x= [0, 1, 2, 3];
   y= [0, 2, 4, 7];
   pleb, y, x, dx=0.2, dy=[0.3, 0.4, 0.5, 0.3], mfill=1;
   Uncertainties on dx are the same for all X, and those
   on Y are different for each value of Y.  Filled markers
   will be displayed at (X, Y).

   KEYWORDS: color, width, marker, msize
   dx     uncertainty on X
   dy     uncertainty on Y

   SEE ALSO: plmk, pldj
*/
{
  if (is_void(dx)) dx= 0.;
  if (is_void(dy)) dy= 0.;

  if (is_void(x)) x= indgen(numberof(y));

  xmin= x-dx;
  xmax= x+dx;
  if (numberof(x) != numberof(xmin)) {
    xmin= xmin(..,1);
    xmax= xmax(..,2);
  }

  ymin= y-dy;
  ymax= y+dy;
  if (numberof(y) != numberof(ymin)) {
    ymin= ymin(..,1);
    ymax= ymax(..,2);
  }

  pldj, x, ymin, x, ymax, color=color, width=width, legend="",type=type;
  pldj, xmin, y, xmax, y, color=color, width=width, legend="",type=type;
  if (!is_void(marker) || !is_void(msize) || !is_void(mfill))
    plmk, y, x, color=color, msize=msize, marker=marker,
      width=(mfill? 20.: width);
}

/*----------------------------------------------------------*/

func plots(x,y,z,type=,marker=,xr=,yr=,zr=,cage=)
/* DOCUMENT plots,x,y,z,type=,marker=,xr=,yr=,zr=
   plot lines/points in 3-D

   n=20000;
   t=acos(2*(random(n)-0.5));
   p=random(n)*2*pi;
   x=sin(t)*cos(p);
   y=sin(t)*sin(p);
   z= cos(t);
   clear3
   plots,x,y,z,type=0,marker='\1',xr=[-1,1],yr=[-1,1],zr=[-1,1];
   spin3, 400;

*/
{
  require, "plwf.i";
  if (_draw3) {
    xyz= _nxt(x);
    type= _nxt(x);
    marker= _nxt(x);
    get3_xy,xyz,x0,y0;
    plg,y0,x0,type=type,marker=marker;}
  else {
    if (is_void(xr)) xr=[min(x),max(x)];
    if (is_void(yr)) yr=[min(y),max(y)];
    if (is_void(zr)) zr=[min(z),max(z)];
    if (is_void(type)) type=1;
    if (is_void(marker)) marker=0;
    window3;
    orient3;
    light3;
    cage3,(is_void(cage)? 0: cage);
    // limit3,min(x),max(x),min(y),max(y),min(z),max(z)
    limit3,xr(1),xr(2),yr(1),yr(2),zr(1),zr(2);
    xyz=transpose([x,y,z],2);
    get3_xy,xyz,x0,y0;
    plg,y0,x0,type=type,marker=marker;
    set3_object,plots,_lst(xyz,type,marker);}
}

/*----------------------------------------------------------*/

/* pre-defined common color  names
// fma; for(i=1;i<=clox(*);i++) plg,random(5),color=clox(noop(i));
extern clox;
/* DOCUMENT clox;
   common color RGB in oxy-object, members:
     White,Black,Blue,Red,Green,Cyan,Yellow,Magenta,Lightblue,
     Darkgreen,Darkgreen2,Violett,Orange,Darkred,GreyBlue,GreyRed,
     GreyGreen,GreyCyan,GreyOrange
   SEE ALSO: clgnu
 */
clox= save();
clox, White   = [255,255,255];
clox, Black   = [0,0,0];
clox, Blue    = [0,0,255];
clox, Red     = [255,0,0];
clox, Green   = [0,255,0];
clox, Cyan    = [0,255,255];
clox, Yellow  = [255,255,0];
clox, Magenta = [255,0,255];
clox, Lightblue = [150,200,255];
clox, Darkgreen = [50,170,50];
clox, Darkgreen2 = [50,140,50];
clox, Violett = [250,50,250];
clox, Orange  = [255,150,0];
clox, Darkred = [170,30,0];
clox, GreyBlue = [200,200,255];
clox, GreyRed  = [255,150,150];
clox, GreyGreen = [200,255,200];
clox, GreyCyan = [200,255,255];
clox, GreyOrange = [255,220,120];

extern clgnu;
/* DOCUMENT clgnu;
   array of color indices for plg coinciding
   with gnuplot colors: gnuplot#1 is clgnu(1) for plg ...
   SEE ALSO:
 */
/* gnuplot colors */
/* black, red, green, blue, magenta, cyan, yellow */
clgnu = [-3, -5, -6, -7, -9, -8, -10];
// fma; for(i=1;i<=numberof(clgnu);i++) plg,random(5),color=clgnu(i);

func palette_shade (args)
/* DOCUMENT palette_shade, GreyGreen [, npal=];
   OR palette_shade, "GreyGreen" [, npal=];
   Sets the palette to contain shades from color "color" to white.
*/
{
   sk= args(-);              //key strings
   nk= numberof(sk);
   if (nk && anyof(strmatch(sk,"npal")))
      npal= args("npal");
   else
      npal= 240;
   if (nk>1)
      error,"only MPAL= key allowed:> palette_shade, clrnm [, npal=];";

   //positional arg processing
   if (args(0)>1)
      error,"palette_shade, clrnm, [npal=])";
   color= args(0)==0? "Black": (args(0,1)==0? args(-,1): args(1));

   cl= clox(noop(color));
   mn = 50;
   red   = span(min(cl(1),mn), 255, npal);
   green = span(min(cl(2),mn), 255, npal);
   blue  = span(min(cl(3),mn), 255, npal);

   palette, red, green, blue;
}
wrap_args, palette_shade;

func palette_cmy (npal=)
/* DOCUMENT palette_cmy[, npal=];
   Sets the palette to CMY, for phase or dem bitmaps
   usage:
   fma;pli,bytscl(height%200);palette_cmy,npal=240;
*/
{
  npal= is_void(npal)? 200: npal;
  np3= npal/3;
  mn= char(0);

  cr= char(span(mn, 255, np3)+0.5);
  on= array(char(255),np3);

  // teal to purple
  red= cr;
  green= on-cr;
  blue= on;
  // purple to yellow
  red= _(red,on);
  green= _(green,cr);
  blue= _(blue,on-cr);
  // yellow to teal
  l= 3*np3<npal;
  red= _(red,on-cr,(l? mn: []));
  green= _(green,on,(l? on(1): []));
  blue= _(blue,cr,(l? on(1): []));

  palette, red, green, blue;
}

/* ------------------------------------------------------------------------ */

func sread_n(s, &n0, &n1, &n2, &n3, &n4, &n5, &n6, &n7, &n8, &n9)
/* DOCUMENT sread_n, f, n0, n1, n2, ...
   grabs the next numbers N0, N1, N2, ... from string s, skipping over
   any whitespace, comma, semicolon, or colon delimited tokens which
   are not numbers.  (Actually, only the first and last characters of
   the token have to look like a number -- 4xxx3 would be read as 4.)
   ***WARNING*** at most ten Ns are allowed
   The Ns can be arrays, provided all have the same dimensions.
   EXAMPLE:
   a=b=c=[1,2]
   sread_n,"1;2;3;4;5;6",a,b,c;a;b;c;

   SEE ALSO: sread, read_n, rdline
*/
{
  n= numberof(n0);
  for (i=1 ; i<=n ; i++) {
    sread_n_worker, s, n0, i;   if (is_void(n1)) continue;
    sread_n_worker, s, n1, i;   if (is_void(n2)) continue;
    sread_n_worker, s, n2, i;   if (is_void(n3)) continue;
    sread_n_worker, s, n3, i;   if (is_void(n4)) continue;
    sread_n_worker, s, n4, i;   if (is_void(n5)) continue;
    sread_n_worker, s, n5, i;   if (is_void(n6)) continue;
    sread_n_worker, s, n6, i;   if (is_void(n7)) continue;
    sread_n_worker, s, n7, i;   if (is_void(n8)) continue;
    sread_n_worker, s, n8, i;   if (is_void(n9)) continue;
    sread_n_worker, s, n9, i;
  }
}

func sread_n_worker(&s, &var, i)
{
  /* indirect flag necessary because can't store back into a
     scalar using var(i)=... (sigh) */
  if (indirect=dimsof(var)(1)) value= structof(var)();
  while (s) {
    tok= strtok(s, ",;: \t");
    s= tok(2);
    len= strlen(tok(1));
    if (len && strmatch("0123456789.",strpart(tok(1), len:len)) &&
        (indirect? sread(tok(1), value) : sread(tok(1), var))) {
      if (indirect) var(i)= value;
      return;
    }
  }
}

func loginclude (outfnmroot)
/* DOCUMENT c= loginclude (outfnmroot)
   takes a root file name OUTFNMROOT, and write out a copy of the
   CURRENT_INCLUDE prefaced by a commented header
   consisting the included file and directory path
   Returns C which is the current_include() as a string;
   SEE ALSO:
*/
{
  mkdirp,dirname(outfnmroot);
  st= strtimestamp();
  c= current_include();
  lognm= strpart(outfnmroot,-1:)==".i"? outfnmroot: outfnmroot+".i";
  doc= "// "+["","current dir: "]+rdline(popen("uname -a;pwd;",0),2);
  if (!is_void(c) && lognm!=c)
    write,open(lognm,"w"), \
      _(swrite(c,st,format="// included: %s, on: %s"),doc,"",text_lines(c)), \
      format="%s\n";
  return c;
}

/* ------------------------------------------------------------------------ */
/* ------------------------------------------------------------------------ */

func hcsuff(fnm,dpi=)
/* DOCUMENT hcsuff(fnm,dpi=)
   uses suffix *.abc to pick output format
   from "eps" "pdf" "png" "jpeg" "epsi"
   out of that set or no suffix then "ps".
   dpi is reset from default 72 to 120
   for *.jpeg *.png
*/
{
  dir0= get_cwd();
  dir= dirname(fnm);
  fnm= basename(fnm);
  if (lsdir(dir)!=0)
    cd,dir;
  else
    error,dir+": is not a directory";

  s= fnm;
  do {ss=strtok(s,".");} while((s=ss(2)))
    rtnm= strpart(fnm,1:-strlen(ss(1)));

  if (is_void(dpi)) dpi= 120;

  if (ss(1)=="eps")
    eps, fnm;
  else if (ss(1)=="pdf")
    pdf, fnm;
  else if (ss(1)=="png")
    png, fnm, dpi= dpi;
  else if (ss(1)=="jpeg")
    jpeg, fnm, dpi= dpi;
  else if (ss(1)=="epsi")
    epsi, fnm;
  else
    hcps, fnm;

  cd, dir0;
  return dir+"/"+fnm;
}

func pliwrap (x, x0, y0, x1, y1, deg=, wrap=, off=)
{
  if (is_void(wrap)) {
    cmin= -pi;
    cmax= pi;
    if (deg==1) {
      cmin= -180;
      cmax= 180;
    }
  } else {
    if (is_void(off)) off= 0;
    cmin= off;
    cmax= off+wrap;
  }
  ca= (cmin+cmax)/2;
  b= bytscl(x,cmin=cmin,cmax=ca);
  pli, merge2(b,max(b)-bytscl(x,cmin=ca,cmax=cmax),x<ca),\
    x0, y0, x1, y1;
}

func plot_vbar (levs0, colors, offset=, wrap=, off=)
/* DOCUMENT plot_vbar, levs, colors

   plot a horizontal color bar below the viewport

   levs defines the levels seperating the region
   colors define the index into the color bar to be used between each level

   if (is_void(levs) || is_void(colors)) error," both levs and colors must be defined";

*/
{

  // coords of viewport
  port= viewport();
  xd= port(2)-port(1);
  yd= port(4)-port(3);

  if (is_void(offset)) offset= 0.12;

  xl= port(2)+yd*offset/5;
  xr= xl + yd*offset/3;
  yb= port(3);
  yt= yb+yd;

  xlabs= xr + 0.02;

  levs= levs0;
  if (numberof((w=where(abs(levs)/levs(rms)<1e-12)))) levs(w)= 0; // kludge
  nlevs= numberof(levs);
  nc= nlevs+1;

  zcol= array(short,1,nc);
  zcol(1,)= indgen(1:nc:1);

  //plot colors
  lastsys= plsys_get();
  plsys, 0;
  if (!is_void(colors)) {
    xpli,  span(0,1,nlevs)(-,), xl, yb, xr, yt, cmap=colors;
  } else {
    if (!is_void(wrap))
      pliwrap, zcol, xl, yb, xr, yt, wrap=wrap, off=off;
    else
      pli, zcol, xl, yb, xr, yt;
  }


  //plot labels;
  if (nc > 10) {
    // we want no more than 10 labels
    num_colors_per_label= nc/10.
      nlabs= int(nc/(int(num_colors_per_label)+1));
    nskip= nlevs/nlabs; // number of levels to skip;
    //read, prompt="debug",dummy;
  } else {
    nlabs= nc;
    nskip= 1;
  }

  //print, "nlevs, nlabs, nskip", nlevs, nlabs, nskip;

  ydd= yd/(nlevs+1);
  for (i=1;i<=nlevs;i=i+nskip)
    plt,swrite(levs(i),format="%.3g"),xlabs,yb+i*ydd,height=12,justify="LH";

  pldj, xl, yb, xr, yb;
  pldj, xl, yt, xr, yt;
  pldj, xl, yb, xl, yt;
  pldj, xr, yb, xr, yt;

  plsys_set, lastsys;

}
/* ------------------------------------------------------------------------ */

extern plsys_coord;
plsys_coord= 1;

func plsys_set (n)
/* DOCUMENT plfsys_set(n)

   store the coordinate system away for later access with plsys_get
   then call plsys. This allows one to retrieve the current coordinate
   system so you can reset it after working in a particular system.

   SEE ALSO: plsys, plsys_get
*/
{
  extern plsys_coord;
  plsys_coord= n;
  plsys,n;
}

func plsys_get(n)
/* DOCUMENT plsys_get

   return the coordinate system last set with plsys_set
   This allows one to retrieve the current coordinate
   system so you can reset it after working in a particular
   coordinate system.

   SEE ALSO: plsys, plsys_set
*/
{
  extern plsys_coord;
  return plsys_coord;
}
/*---------------------------------------------------------------------*/

func limitseq (n1,n2,q,panel=)
/* DOCUMENT limitseq(n1,n2,q)
   window,n1;
   if (is_void(q)) {
   q=limits();
   } else {
   limits,q;
   }
   window,n2;
   limits,q;
   window,n1;
   return q
*/
{
  n0= window();
  if (panel==1) {fnext,n1;} else {window,n1;}
  if (is_void(q)) {
    q= limits();
  } else {
    limits,q(1),q(2),q(3),q(4);
  }
  if (panel==1) {fnext,n2;} else {window,n2;}
  limits,q(1),q(2),q(3),q(4);
  if (panel!=1) window,n0;
  return q;
}
/*---------------------------------------------------------------------*/

func limitsexp (frac,panel=)
/* DOCUMENT
 */
{
  if (is_void(frac)) frac= 0.06;
  if (!is_void(panel)) {fnext,n1;}
  limits;
  q= limits();
  q1p= (q(1)+q(2))/2;
  q2p= (q(3)+q(4))/2;
  q1m= (q(2)-q(1))/2;
  q2m= (q(4)-q(3))/2;
  frac+= 1;
  limits,q1p-frac*q1m,q1p+frac*q1m,q2p-frac*q2m,q2p+frac*q2m;

  return limits();
}

func vparray(nx,ny,xm,xM,ym,yM,xgap=,ygap=)
/* DOCUMENT
   require,"plot.i";
   nx= 2;
   ny= 5;
   xm= 70; xM= 880; ym= 70; yM= 680;
   vp= vparray(nx,ny,xm,xM,ym,yM,xgap=35,ygap=25);
   xwindow,window(),height=600,width=920,units=2, \
   viewport=vp,size=8,xopt=0,yopt=0;
   for (i=1;i<=nx*ny;i++) {
   plsys,i;
   plt,swrite(i),1.9,.8,tosys=1,justify="CH";
   plg,[0,1];
   }
   SEE ALSO:
*/
{
  xg= is_void(xgap)? 0: xgap/2;
  yg= is_void(ygap)? 0: ygap/2;

  x= span(xm+xg,xM-xg,nx+1);
  y= span(ym+yg,yM-yg,ny+1);
  xy= transpose([x(..,-:1:ny+1), y(-:1:nx+1,..)],2);

  plm,xy(2,..),xy(1,..);
  i= indgen(nx);
  j= indgen(ny);

  ij1= ((j(-,..)-1)*(nx+1)+i)(*);
  ij2= ij1+1;
  ij3= ij1+(nx+1);

  vp= transpose([xy(1,ij1)+xg,xy(1,ij2)-xg,xy(2,ij1)+yg,xy(2,ij3)-yg]);
  ii= reform(indgen(nx*ny),[2,nx,ny]);
  vp= vp(,ii(,::-1));
  return vp;
}

func assign (args)
/* DOCUMENT assign, ary, v1, v2, v3, ...
   Assigns the values in an array to the specified variables. For example:

   > assign, [2, 4, 6], a, b, c
   > a
   2
   > b
   4
   > c
   6

   Any number of variables may be given. If there are more variables than there
   are values in ARY, then the remaining variables are set to [].
*/
{
  ary= args(1);
  size= is_obj(ary)? ary(*): numberof(ary);
  for(i=1; i<args(0); i++)
    args, i+1, (i <= size ? ary(noop(i)) : []);
}
wrap_args, assign;

/************************************************************************/

func lssys (directory, &dirs, dir=)
/* DOCUMENT lsltr(directory, &dirs)

   returns a list of files sorted in order of date

   SEE ALSO: strmatch, strpart, where
*/
{
  if (is_void(directory)) directory= ".";
  system, "ls -Lapc1 "+directory+"> /tmp/.dirContent";
  files= rdfile("/tmp/.dirContent");
  files= files(where(files!="./"));
  files= files(where(files!="../"));
  files= files(where(files!=string(nil)));
  msk= strpart(files,0:0) == "/";
  if (anyof(msk)) dirs= files(where(msk));
  if (nallof(msk)) files= files(where(!msk));
  if (!is_void(dirs)) dirs= strtok(dirs, "/")(1, );
  system, "rm /tmp/.dirContent";
  return files;
}


func save_rec(args)
/* DOCUMENT save_rec, f, a, b, ...,  _suf=, _prf=;
   simple klunky-kludgy PDB save with variable name obfuscation.
   no introspection...
   underscored keywords to lower chance of variable name clashes...
   SEE ALSO: restore_rec
*/
{
  sk= args(-); //key strings
  nk= numberof(sk);
  k1= ["_prf","_suf"];
  if (nk!=0) {
    if (anyof(strmatch(sk,k1(1))))
      prf= args(k1(1));
    else
      prf= "";
    if (anyof(strmatch(sk,k1(2))))
      suf= args(k1(2));
    else
      suf= "";
  }
  prf= (is_string(prf)? prf: pr1(prf));
  suf= (is_string(suf)? suf: pr1(suf));

  for (i=1;i<=nk;i++) {
    nm= sk(i);
    if (noneof(strmatch(k1,nm)))
      save, args(1), prf+nm+suf, args(nm);
  }
  i= 2;
  while (i<=args(0))  {
    ar= args(i);
    if (is_string(ar)) {
      i+= 1;
      oi= args(i);
      if (is_void(oi))
        save, args(1), prf+ar+suf, OXY_VOID_VAL;
      else if (is_func(oi)==0) // used to thow error
        save, args(1), prf+ar+suf, oi;
    } else
      if (!is_void(ar) && is_func(ar)==0) // not sure
        save, args(1), prf+args(-,i)+suf, ar;
    i+= 1;
  }
}
wrap_args, save_rec;

func restore_rec(args)
/* DOCUMENT restore_rec, f, a, b, ..., _suf=, _prf=;
   simple klunky-kludgy PDB variable name obfuscation.
   no introspection...
   unerscored keywords to avoid variable name clashes...
   SEE ALSO: save_rec
*/
{
  sk= args(-); //key strings
  nk= numberof(sk);
  k1= ["_prf","_suf"];
  if (nk!=0) {
    if (anyof(strmatch(sk,k1(1))))
      prf= args(k1(1));
    else
      prf= "";
    if (anyof(strmatch(sk,k1(2))))
      suf= args(k1(2));
    else
      suf= "";
  }
  prf= (is_string(prf)? prf: pr1(prf));
  suf= (is_string(suf)? suf: pr1(suf));

  local x;
  i= 2;
  while (i<=args(0))  {
    ar= args(i);
    if (is_string(ar)) {
      i+= 1;
      oi= args(i);
      restore, args(1), prf+ar+suf, x;
    } else
      restore, args(1), prf+args(-,i)+suf, x;
    if (is_scalar(x) && x==OXY_VOID_VAL) write,"void";
    args, i, x;
    i+= 1;
  }
}
wrap_args, restore_rec;

func is_group (o)
/* DOCUMENT l= is_group(o);
   check if all members are anonymous
   is_group(save(string(0),a,string(0),b,string(0),c,string(0),d,string(0),e))
   SEE ALSO:
*/
{
  return is_obj(o)>0 && !is_stream(o) && allof(o(*,)==string(0));
}

func is_oxgrar (o, &s, &d)
/* DOCUMENT is_oxgrar (o)
   checks that all members are anonymous, and that members are
   all of the same type, therefore transferable to a yorick array:
   numerical, string, or pointer
   SEE ALSO:
*/
{
  if (is_obj(o)==0)
    return 0;
  if (!is_group(o))
    return 0;
  if (o(*)==0)
    return 1;

  o1= o(1);
  if (is_obj(o1)) {
    local s1, d1;
    l= is_oxgrar(o1,s1,d1);
    i= 1;
    if (l)
      while (i++<o(*)) {
        if (!is_oxgrar(o(1*i),s,d) || s!=s1 || nallof(d==d1)) {
          l= 0;
          break;
        }
      }
    return l;
  }

  s= structof(o1);
  d= dimsof(o1);
  l= is_numerical(o1) || is_string(o1) || is_pointer(o1);
  i= 1;
  if (l)
    while (i++<o(*)) {
      oi= o(1*i);
      if (s!=structof(oi) || nallof(d==dimsof(oi))) {
        l= 0;
        break;
      }
    }
  return l;
}

func oxgrar_dims_wrkr(o, &s, &d)
{
  o1= o(1);
  l= is_numerical(o1) || is_string(o1) || is_pointer(o1);
  if (l) {
    s= structof(o1);
    d= _(o(*),numberof(o1));
  } else if (is_obj(o1)) {
    local s1, d1;
    oxgrar_dims_wrkr, o1, s1, d1;
    d= _(o(*),d1);
    s= s1;
  } else
    error,"unrecognized type: expected obj or (numerical-string-pointer).";
  return;
}

func arr_oxgr (o, &ier, row=)
/* DOCUMENT arr_oxgr (o)
   copy oxy group to array, or reverse cast,
   depending on input type - oxy obj, or array

   test:
   x= random(5,4,3,2);
   statarr,x-arr_oxgr(arr_oxgr(x));
   statarr,x-arr_oxgr(arr_oxgr(x,row=1),row=1);
   SEE ALSO:
*/
{
  if (is_obj(o)) {
    if (!is_oxgrar(o))
      {ier= 1; return o;}

    if (o(*)==0)
      return [];

    local s,d;
    oxgrar_dims_wrkr, o, s, d;
    if (!row)
      d= d(::-1);

    d= _(numberof(d),d);
    out= array(s(0),d);
    if (d(1)>2 && is_obj(o(1)))
      for (i=1;i<=(!row? d(0): d(2)) && ier!=1;i++)
        if (!row)
          out(..,i)= arr_oxgr(o(1*i),ier,row=row);
        else
          out(i,..)= arr_oxgr(o(1*i),ier,row=row);
    else
      for (i=1;i<=(!row? d(0): d(2));i++)
        if (!row)
          out(..,i)= o(1*i);
        else
          out(i,..)= o(1*i);
  } else {
    if (is_void(o))
      return save();
    if (!is_numerical(o(1)) && !is_string(o(1)) && !is_pointer(o(1)))
      if (ier==1)
        return [];
      else
        error,"not an array. expecting array of: number[s], string[s], pointer[s].";
    d= dimsof(o);
    out= save();
    if (d(1)>2)
      for (i=1;i<=(!row? d(0): d(2)) && ier!=1;i++)
        if (!row)
          save, out, string(0), arr_oxgr(o(..,i),ier,row=row);
        else
          save, out, string(0), arr_oxgr(o(i,..),ier,row=row);
    else
      for (i=1;i<=(!row? d(0): d(2));i++)
        if (!row)
          save, out, string(0), o(..,i);
        else
          save, out, string(0), o(i,..);
  }

  return ier==1? o: out;
}

func oxcopy (o)
/* DOCUMENT oxcopy (o)
   recursive oxy object copy
*/
{
  oo= o(*)>0? o(:): save();
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    if (is_obj(oi))
      save,oo,o(*,i),oxcopy(oi);
  }
  return oo;
}

func oxprune (o, nofunc=, nostream=, notextstream=, novoid=)
/* DOCUMENT oxprune (o, nofunc=, nostream=, notextstream=)
   recursive oxy object copy
*/
{
  oo= save();
  if (typeof(o)=="closure" && (is_obj(o.function) || is_obj(o.data)))
    o= (is_obj(o.function)? o.function: o.data);
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    if (is_obj(oi))
      save,oo,o(*,i),oxprune(oi,nofunc=nofunc,nostream=nostream, \
                             notextstream=notextstream,novoid=novoid);
    else if (typeof(oi)=="closure" && (is_obj(oi.function) || is_obj(oi.data)))
      save,oo,o(*,i),oxprune((is_obj(oi.function)? oi.function: oi.data), \
                             nofunc=nofunc,nostream=nostream,           \
                             notextstream=notextstream,novoid=novoid);
    else if ( !(nofunc && is_func(oi)) && \
              !(novoid && is_void(oi)) && \
              !(nostream && is_stream(oi)) && \
              !(notextstream && typeof(oi)=="text_stream"))
      save, oo, o(*,i), oi;
  }
  return oo;
}

func oxmerge (o, oo)
/* DOCUMENT o3= oxtypeq(o1,o2);
   merge O2's members into O1'
   o= dbase("a","b","c");
   for (i=1;i<10;i++)
   o,add,save(a=random(10),b=random(10),c=random(10),d=random_n());
   oo= oxmerge(dbase(),oxprune(o,nofunc=1));
   oxtypeq(o,oo);
   SEE ALSO:
*/
{
  if (!is_void(oo)) {
    ou= save([],o,[],oo);  // clobber o's members with oo's
    // if O was clobbered restore O's members which aren't in OO
    for (i=1; i<=o(*); i++) {
      oi= o(noop(i));
      if (is_obj(oi) && is_obj(oo,o(*,i),1)>=0) {
        ooi= oo(o(*,i));
        save,ou,o(*,i),oxmerge(oi, ooi);
      }
    }
  } else
    ou= o;
  if (am_subroutine())
    o= ou;
  return ou;
}

func oxtypeq (o1, o2, nodim=)
/* DOCUMENT oxtypeq (o1,o2[,nodim=1])
   recursive oxy object types and dimesion(+rank) checks ONLY
   NODIM==1 checks type and rank only
*/
{
  return oxeq(o1,o2,(nodim==1? 0: 1));
}


func oxeq (o1, o2, strict)
/* DOCUMENT oxeq (o1, o2[, strict])
   checks that all member names and values are identical
   STRICT= 2, check *types*, *dimensions* - including *rank*, and *values*
   STRICT= 1, check *types*, (*rank*) and *dimensions*
   STRICT= void OR 0, *DEFAULT* check *types* and *ranks*
   ORDER may be different, BUT unnamed members MUST keep
   order between themselves, object-to-object
   NOTE: values identity checks are DUMB FIXME:
   SEE ALSO:
*/
{
  strict= is_void(strict)? 0: strict;
  // id. # of members ?
  if(is_obj(o1)) {
    if (o1(*)!=o2(*))
      return 0;
    n= o1(*);
    s1= o1(*,);
    s2= o2(*,);
    w1= where(s1);      // where named
    nnm= numberof(w1);  // how many named
    w2= where(s2);
    if (numberof(w2)!=nnm)
      return 0;
    wn2= where(!s2);
    ss1= nnm? o2(*,s1(w1)): []; // O2 indices of O1 named members
    if (nnm && anyof(ss1==0))   // id. # named members, all found or False
      return 0;
    else
      l= 1;

    i= 0;                   // all members count
    j= 0;                   // named members of O1 count
    k= 0;                   // un-named membr of O1 count
    while (l==1 && i++<n) {
      o1i= o1(noop(i));
      o2i= s1(i)? o2(ss1(++j)): o2(wn2(++k));
      l&= oxeq(o1i,o2i,strict);
    }
  } else {
    l= structof(o1)==structof(o2);     // type check
    d1= dimsof(o1); d2= dimsof(o2);
    l&= d1(1)==d2(1);                  // rank check
    if (l && strict==1)
      l&= allof(d1==d2);               // dimesion check
    if (l && strict==2)
      l&= allof(o1==o2);               // value check
  }
  return l;
}

GRPOXSV= "_grp_";

func oxsave (args)
/* DOCUMENT oxsave, f, ob;
   saves oxy groups as PDB's if each member is writable.
   No type checks! only groups, arrays, pointers(?)...
   "group" objects (nil membname) not allowed
   NOTE**
   **** Cannot mix groups and regular objects on id. node ****
   NOT save(string(0),a,"b",b,string(0),c) NOT
   usage:
   ------
   o= save(pi);
   o1= save(o,p1=1,p2=[]);
   o2= save(o1,p2=2,p3=[]);
   o22= o2(:);
   o3= save(o2,o22,p3=3,p4=[]);

   o32= o3(:);
   oxsave,createb("q.opdb"),o3;
   o3= [];
   oxrestore,openb("q.opdb"),o3;
   oxtypeq(o3,o32);

   oxwrite,open("q.i","w"),o3,"ob";
   include, "q.i", 1;
   oxtypeq(o3,ob);
   SEE ALSO: oxread,oxcopy,oxtypeq,oxwrite;
*/
{
  sk= args(-);         // key strings, one reserved key
  nk= numberof(sk);
  k1= "_wrk_";
  if (nk!=0) {
    if (anyof(sk==k1))
      _wrk_= args(k1);
    else
      _wrk_= [];
  }
  if (_wrk_!=1) {
    for (i=1;i<=nk;i++) {
      nm= sk(i);
      o= args(nm);
      if (nm!=k1)
        if (is_obj(o))
          oxsave,args(1),noop(nm),o,_wrk_=1;
        else if (typeof(o)=="closure") {
          if (!is_func(o.function))
            o= o.function;
          else if (!is_func(o.data))
            o= o.data;
          if (is_obj(o))
            oxsave,args(1),noop(nm),o,_wrk_=1;
        } else
          save,args(1),noop(nm),o;
    }
    i= 2;
    while (i<=args(0))  {
      ar= args(i);
      if (args(0,i)==1 && is_string(ar) && is_scalar(ar)) {
        i+= 1;
        o= args(i);
        if (is_void(o))
          error,"cannot save void";
        if (is_obj(o))
          oxsave,args(1),noop(ar),o,_wrk_=1;
        else if (typeof(o)=="closure") {
          if (!is_func(o.function))
            o= o.function;
          else if (!is_func(o.data))
            o= o.data;
          if (is_obj(o))
            oxsave,args(1),noop(ar),o,_wrk_=1;
        } else
          save,args(1),noop(ar),o;
      } else {
        if (is_obj(ar))
          oxsave,args(1),args(-,i),ar,_wrk_=1;
        else if (typeof(ar)=="closure") {
          if (!is_func(ar.function))
            ar= ar.function;
          else if (!is_func(ar.data))
            ar= ar.data;
          if (is_obj(ar))
            oxsave,args(1),args(-,i),ar,_wrk_=1;
        } else
          save,args(1),args(-,i),ar;
      }
      i+= 1;
    }
  } else if (_wrk_==1) {
    if (is_string(args(1)))
      f= createb(args(1));
    else
      f= args(1);
    if (args(0,4)==2)
      onm= save(string(0),args(2));
    else
      onm= args(4);
    o= args(3);
    for (i=1,nm=""; i<=onm(*); i++)
      nm+= onm(noop(i))+".";
    if (typeof(o)=="closure") {
      if (!is_func(o.function))
        o= o.function;
      else if (!is_func(o.data))
        o= o.data;
      //save, onm, string(0), vnm;
      f= oxsave(f,args(2),o,onm,_wrk_=1);
    } else {
      for (i=1,in=1,ino=1; i<=o(*); i++) {
        oi= o(noop(i));
        if (typeof(oi)=="closure") {
          if (!is_func(oi.function))
            oi= oi.function;
          else if (!is_func(oi.data))
            oi= oi.data;
          save, onm, string(0), o(*,i);
          f= oxsave(f,o(*,i),oi,onm,_wrk_=1);
        } else if (is_obj(oi)) {
          vnm= o(*,i);
          if (vnm==string(0))
            vnm= swrite(ino++,format=GRPOXSV+"%03i");
          save, onm, string(0), vnm;
          f= oxsave(f,vnm,oi,onm,_wrk_=1);
        } else {
          vnm= o(*,i);
          if (vnm==string(0))
            vnm= swrite(in++,format=GRPOXSV+"%03i");
          save_rec,f,vnm,oi,_prf=nm;
        }
      }
    }
    if (onm(*)>1)
      onm= onm(:-1);
    else
      onm= save();
    args,4,onm;
  }
  return f;
}
wrap_args, oxsave;

func oxrestore (args)
/* DOCUMENT ob=  oxrerstore (f);
   reads oxy groups from PDB.
   SEE ALSO: oxsave,oxcopy,oxtypeq,oxwrite;
*/
{
  sk= args(-);              // key strings
  if ((nk=numberof(sk))>1)
    error;
  _wrk_= [];
  k1= "_wrk_";
  if (nk==1)
    if (sk(1)==k1)
      _wrk_= args(k1);
    else
      error;

  if (_wrk_!=1) {
    local x;
    i= 2;
    if (am_subroutine())
      while (i<=args(0))  {
        ar= args(i);
        if (is_string(ar) && args(0,i)==1) {
          i+= 1;
          oi= args(i);
          oxrestore,args(1),ar,x,_wrk_=1;
        } else
          oxrestore,args(1),args(-,i),x,_wrk_=1;
        args, i, x;
        i+= 1;
      }
    else {
      // -- assume there is only one var
      p= get_vars(args(1));
      ar= strtok((*p(1))(1),".")(1);
      oxrestore,args(1),ar,x,_wrk_=1;
      return x;
    }
  } else {
    vars= get_vars(args(1));
    addrs= get_addrs(args(1));

    names= *vars(1);
    n= numberof(names);
    if (n>0)
      names= names(sort(*addrs(1))); // names= names(sort(names));
    else
      error;

    vo= args(2);
    m= strgrepm("^"+vo+"$|^"+vo+"\\.",names);

    if (noneof(m))
      error,"Variable "+vo+" not found";
    names= names(where(m));

    if (numberof(names)==1 && strpart(names(1),1:1)!=".") {
      args,3,get_member(args(1),names(1));
    } else {
      oo= save();
      for (i=1; i<=numberof(names) ; i++) {
        nm= names(i);
        val= get_member(args(1),nm);
        on= strtok(nm,".",30);          // max depth
        on= on(where(on));
        oj= oo;
        for (j=1; j<numberof(on); j++) {
          isg= strgrepm(GRPOXSV+"[0-9][0-9][0-9]",on(j));
          if (isg==1) {
            ig= 0;
            if (sread(on(j),ig,format=GRPOXSV+"%d")>0) {
              if(ig>oj(*))
                save,oj,string(0),save();
            } else {
              error, "group index read error";
            }
          } else {
            if (is_obj(oj,on(j),1)<0)
              save,oj,on(j),save();
          }
          oj= oj((isg? noop(ig): on(j)));
        }
        if (is_scalar(val) && val==OXY_VOID_VAL)
          val= [];
        save,oj,on(0),val;
      }
      args,3,oo(1);
    }
  }
}
wrap_args,oxrestore;

func oxwrite (args)
/* DOCUMENT oxwrite, ob, ["object name", if void  "cfg"];
   writes a yorick source file with oxy-object definition:  !! only POD !!

   o3= save(a, b=3 ,c ,d=pi, hl="hello");
   oxwrite,open("q.i","w"),o3,obnm= "this";
   ... later ..
   local this;
   include,"q.i",1;  // THIS object instanciated
   TODO: fix for groups
   SEE ALSO: oxsave, oxread, oxcopy, oxtypeq;
*/
{
  if (args(*)!=2)
    error,"oxwrite, open(fnm,\"w\"), ob[, obnm=]; // SEE > help,oxwrite";
  local obnm;
  obnm= is_void(args(*,))? (args(0,2)==1? "cfg": args(*,2)): args("obnm");
  return  oxwrite_wrkr(args(1),args(2),obnm);
}
wrap_args,oxwrite;

func oxwrite_wrkr (f, o, &onm, lvl)
{
  if (is_string(f))
    f= open(f,"w");

  if (is_void(onm))
    onm= "cfg";

  // object indentation level
  nidnt= 2;
  lvl= is_void(lvl)? 1: lvl+1;
  idnt1= lvl==1? "": strchar(char(array(32,nidnt*(lvl-1))));
  idnt2= strchar(char(array(32,nidnt*lvl)));

  // only on call
  is_scratch= 0;
  if (is_string(onm)) {
    onm= save(string(0),onm);
    // stash scratch
    s= info(o);
    ws= where(strmatch(s,"= object with"));
    if (numberof(ws)) {
      is_scratch= 1;
      s= discrete(strtrim(strtok(s(ws),"=")(1,)));
      write,f,swrite(s,format=",%s")(sum),\
        format="scratch= save(scratch%s);\n\n";
    }
  }
  s= swrite(onm(0),format="\n"+idnt1+"%s= save(); {");
  write,f,s,format="%s\n";

  print_format,1000,1,float="%.12e",double="%.12e",complex="%.12e+%.12ei";
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    if (is_obj(oi)) {
      save, onm, string(0), o(*,i);
      f= oxwrite_wrkr(f,oi,onm,lvl);
    } else {
      s= swrite(onm(0),o(*,i),print(oi)(*)(sum),\
                format=idnt2+(is_func(oi)? "// ": "")+"%s, %s"+(o(*,i)? "=": "[],")+" %s;");
      write,f,s,format="%s\n";
    }
  }
  print_format,-1,-1;

  if (onm(*)>1) {
    write,f,onm(-1),onm(0),format=idnt1+"} save, %s, %s;\n";
    onm= onm(:-1);
  } else {
    write,f,"}",format="%s\n";
    write,f,"";
    if (is_scratch==1)
      write,f,"restore, scratch;",format="%s\n";
  }
  return f;
}

func oxread (fnm, &xtnm)
/* DOCUMENT o= oxread(fnm[,xtnm]);
   Include yorick code from file FNM. OXREAD id designed to be used only with
   file whithin which one, or many, oxy objects are defined, and nothing else -
   objects whose member values are array data types: strings, numerical,...
   or void(?), OXREAD then return those objects.  There is *NO* attempt to
   protect the caller from other yorick expressions which may be in FNM.
   If a single object is defined in the file, it is returned as such,
   otherwise, if several object are defined, then the returned object
   contains all such objects with their "code" name as members.
   XTNM is the name of the only (or of first if many) object in the code.

   SEE ALSO: oxwrite
 */
{
  // read file, unless recursive call with comment/continuation already removed
  isra= 0
  if (numberof(fnm)>1) {
    isra= 1;
    l= fnm;
  } else {
    if (check_file(fnm,quiet=1)==0)
      error,"file not found: "+fnm;
    l= rdfile(open(fnm,"r"));
    l= stripcode(l);
  }
  nl= numberof(l);

  // split files base on restore/save - blah= save(blah,...); ; ; ;restore, blah;
  // to read concatenated oxwrite output files
  mrs= strgrepm("^ *restore,",l);
  wrs= where(mrs);
  nrs= numberof(wrs);
  // LRS array of all restored object names
  o= strsplit(l(wrs),",");
  if (is_obj(o)) {
    lrs= array(string,o(*));
      for (i=1;i<=nrs;i++)
        lrs= _(lrs,o(noop(i),2));    // only accept restore sub. call with single arg.
  } else
    lrs= o(2);
  lrs= strtrim(lrs);
  for (i=1;i<=numberof(lrs);i++)   // chop end ";"
    if (strpart(lrs(i),0:0)==";")
      lrs(i)= strpart(lrs(i),:-1);

  // all objects defined in file
  msv= strgrepm("=.*save",l);
  wsv= where(msv);
  if (numberof(wsv)==0)
    error,"no objects defined in file.";
  n= numberof(wsv);
  // LSV array of all object names
  o= strsplit(l(wsv),"=");
  if (is_obj(o)) {
    lsv= array(string,n);
    for (i=1;i<=n;i++)
      lsv(i)= o(noop(i),1);
  } else
    lsv= o(1);

  // if multiple restore, break up file and process separately
  if (nrs>1) {
    out= save();
    j= 1;
    for (i=1;i<=nrs;i++) {
      xtnm= string(0);
      oi= oxread(l(j:wrs(i)),xtnm);
      if (xtnm)
        save,out,noop(xtnm),oi;
      j+= wrs(i);
    }
    return out;         // return object with orig member names
  }

  // restored object content
  isv= where(lsv==lrs)(1);
  sob= strtrim(strsplit(o(noop(isv),2),","))(2:);
  if (strpart(sob(0),0:0)==";")
    sob(0)= strpart(sob(0),1:-1);
  if (strpart(sob(0),0:0)==")")
    sob(0)= strpart(sob(0),1:-1);
  else
    error,"extecpting a parenthesis in: "+sob(0);

  // prune restored and saved out of all defined, left with extern
  m= array(1,numberof(lsv));
  for (i=1;i<=numberof(lrs);i++)
    m&= lsv!=lrs(i);
  for (i=1;i<=numberof(sob);i++)
    m&= lsv!=sob(i);

  // extern object names, coma concatenate
  if (noneof(m))
    error,"no extern object found";
  wxt= where(m);
  nxt= numberof(wxt);
  sxt= lsv(wxt);
  xtnm= sxt(1);
  sxtc= strcombine(sxt,",");

  // local single object
  __tmp__= [];
  stmp= "__tmp__";

  // add original extern object name to saved ones
  if (nrs>0) {
    s= l(isv);
    ss= strtok(s,",",numberof(sob)+5);
    ss= ss(where(ss));
    ss= _(ss(:-1),lsv(wxt),ss(0));
    l(isv)= strcombine(ss,",");
    // assemble
    irst= wrs(1);
    l= _(l(:irst-1),(nxt>1? stmp+"= save("+sxtc+");": stmp+"= "+sxtc+";" ),l(irst:));
  } else {
    l0= "scratch= save(scratch,"+sxtc+");";
    l1= nxt>1? stmp+"= save("+sxtc+");": stmp+"= "+sxtc+";";
    l2= "restore, scratch;";
    l= _(l0,l,l1,l2);
  }

  // do the real  work
  include,l,1;

  return __tmp__;
}

func oxisfunc (o)
{
  n= o(*);
  m= array(0,n);
  for (i=1;i<=n;i++)
    m(i)= is_func(o(noop(i)));
  return m;
}

func oxedit (args)
/* DOCUMENT oxedit(ox,m1,m2,m3,...,val,tcheck=,dcheck=,showval=);
   simple editing of nested member values with optional type/dim checks
   return 0 if sucess 1 if error (...hum, not yet)
   parse args backwards, the last VAR or  {MEMBSPEC[=], VAL} is edited
   no checks on value unless TCHECK=1 (struct) or DCHECK=1 (dims)
   SHOWVAL=1 prints old/new values
   ! TCHECK DCHECK SHOWVAL cannot be ox member names
   USE:
   a= save(b=save(c=save(d=save(e=pi))));
   e= 5;
   oxedit,a,b,c,d,e; a(b,c,d,e);
   oxedit,a,b,c,d,"e",1; a(b,c,d,e);
   oxedit,a,b,c,d,e=2; a(b,c,d,e);

   SEE ALSO: restore_rec
*/
{
  sk= args(-); //key strings
  nk= numberof(sk);
  k1= ["tcheck","dcheck","showval"];
  tcheck= dcheck= showval= [];
  if (nk!=0) {
    if (anyof(strmatch(sk,k1(1))))
      tcheck= args(k1(1));
    if (anyof(strmatch(sk,k1(2))))
      dcheck= args(k1(2));
    if (anyof(strmatch(sk,k1(3))))
      showval= args(k1(3));
  }
  nkk= (k1(-,)==sk)(*)(sum);
  if (nk>nkk+1)
    error,"only one ox value key allowed";
  else if (nk==nkk+1) {
    ki=1;while(anyof(k1==sk(ki)))ki++;
  } else
    ki=0;
  i= 2;
  o= args(1);
  while (i<args(0)+(ki>0)-is_string(args(args(0)-1))) {
    ai= args(i++);
    if (is_string(ai))
      o= o(noop(ai));
    else
      o= o(args(-,i-1));
  }
  if (is_string(args(args(0)-1))) {
    nm= args(args(0)-1);
    val= args(args(0));
  } else if (ki>0) {
    nm= sk(ki);
    val= args(sk(ki));
  } else {
    nm= args(-,i);
    val= args(i);
  }
  if (is_obj(o,noop(nm),1)<0)
    error,"no such member: "+nm;
  oo= o(noop(nm));
  if (tcheck==1 && structof(oo)!=structof(val))
    error,"type mismatch.";
  if (dcheck==1 && anyof(dimsof(oo)!=dimsof(val)))
    error,"rank and/or dims mismatch.";
  if (showval==1)
    write,print(oo)(1),print(val)(1),format="Edit old: %s\nEdit new: %s\n";
  save, o, noop(nm), val;
  return 0;
}
wrap_args, oxedit;

func oxlua(args)
/* DOCUMENT s= oxlua(o); // s: array(string)
   o= oxlua(s); .. *not yet* ..
   FMT: format integer or decimal, SEE totxt.
*/
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"string or ox, optional fmt (see totxt).";
  local fmt;
  if (args(0)>1) fmt= args(2);
  tablen= 4;
  if (is_obj(args(1))) {
    if (args(0,1)==0)
      onm= args(-,1);
    else
      onm= "cfg";
    o= args(1);
    s= "";
    oxlua_wrkr,s,onm,o,fmt;
    return s;
  }
}
wrap_args,oxlua;
func oxlua_wrkr (&s,onm,o,nt,fmt)
{
  if (is_void(nt))
    nt= 0;
  t= (nt? strchar(array('\x20',nt*tablen)): "");
  t2= strchar(array('\x20',(nt+1)*tablen));
  s+= t+(!onm? "": onm+"= ")+"{\n";
  on= o(*);
  for (i=1;i<=on;i++) {
    oi= o(noop(i));
    if (is_obj(oi))
      oxlua_wrkr,s,o(*,i),oi,nt+1;
    else
      s+= t2+(!o(*,i)? "": o(*,i)+"= ")+oxlua_pr(oi,fmt);
    s+= (i==on? "": ",")+"\n";
  }
  s+= t+"}";
}
func oxlua_pr (a,fmt)
{
  if (is_void(fmt)) fmt=-0.12;
  if (is_integer(a)) fmt= abs(fmt); //  no hex
  da= dimsof(a);
  sa= totxt(a,fmt);
  if (da(1)>0) {
    n= numberof(a);
    s= da(:-1)
      ra= da(1);
    for(i=1;i<=ra;i++)
      s(i)=(i==1? 1: s(i)*s(i-1));
    da= da(2:);

    m= array(0,n);
    for (i=1;i<ra;i++) {
      wp= indgen(1:n:s(i+1));
      ws= indgen(s(i+1):n:s(i+1));
      m(wp)-= 1; //pre
      m(ws)+= 1; //suf
    }
    m(1)-= 1;
    m(0)+= 1;
    b= strchar("{}");
    for (i=1;i<=n;i++) {
      if (m(i)>0)
        sa(i)= sa(i)+strchar(array(b(2),m(i)));
      if (m(i)<0)
        sa(i)= strchar(array(b(1),-m(i)))+sa(i);
      if (i<n)
        sa(i)+= ",";
    }
    return sa(*)(sum);
  } else
    return sa;
}
func oxnml(args)
/* DOCUMENT s= oxnml(o[,fmt]); // s: array(string)
   o= oxnml(s); .. *not yet* ..
   FMT: format integer or decimal, SEE totxt.
*/
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))>1)
    error,"oxnml, string or ox,[optional fmt (see totxt)][, f90=1].";
  local fmt;
  if (args(0)>1) fmt= args(2);
  f90= 0;
  if (numberof(args(-)))
    if (args(-)(1)=="f90")
      if (args("f90")==1) f90=1;
      else
        error,"unknown keyword.";
  if (is_obj(args(1))) {
    if (args(0,1)==0)
      onm= strcase(0,args(-,1));
    else
      onm= "cfg";
    o= args(1);
    if (f90==1)
      s= "type "+onm+"_t\n";
    else
      s= "&"+onm+"\n";
    // members
    on= o(*);
    for (i=1;i<=on;i++) {
      oi= o(noop(i));
      if (is_obj(oi)) error,"Straight ox structures only.";
      if (!o(*,i)) error,"keyed members only.";
      if (f90==1) {
        doi= dimsof(oi);
        if (doi(1)>0)
          noi= numberof(oi);
        else
          noi= 0;
        if (is_real(oi))
          t= "real(gp)";
        else if (is_integer(oi))
          t= "integer(i4)";
        else if (is_string(oi))
          t= "character(len=128)";
        sd= (noi==0? "": swrite(noi,format="(%i)"));
        s+= "  "+t+" :: "+strcase(0,o(*,i))+sd;
        s+= (i==on? "\nend type "+onm+"_t": "")+"\n";
      } else {
        sa= totxt(oi,fmt);
        s+= " "+strcase(0,o(*,i))+"= "+strpart((sa+" ")(*)(sum),1:-1);
        s+= (i==on? "/": ",")+"\n";
      }
    }
    return s;
  }
}
wrap_args,oxnml;

func oxmap (f,oo,..)
/* DOCUMENT ob=  oxmap (f,oi1,oi2,oi3,..);
   map func F on one/multiple group objects

   oo(i)= f(oi1(i)); // for 1 input
   oo(i)= f(oi1(i),oi2(i),oi3(i),oi4(i)); // for 4 inputs

   SEE ALSO:
*/
{
  if (!is_void(oo))
    oi= save(string(0),oo);
  n= more_args();  // numberof input groups
  for (i=1 ; i<=n ; i++)
    save, oi, string(0), next_arg();
  n= oi(*);
  if (n==0 || n>8)
    error,"... limited to 1 to 8 inputs :)";

  for (o= save(),i=1 ; i<=oo(*) ; i++)
    if (n==1)
      save, o, string(0), f(oi(1,noop(i)));
    else if (n==2)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)));
    else if (n==3)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)));
    else if (n==4)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)),oi(4,noop(i)));
    else if (n==5)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)),oi(4,noop(i)), \
                            oi(5,noop(i)));
    else if (n==6)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)),oi(4,noop(i)), \
                            oi(5,noop(i)),oi(6,noop(i)));
    else if (n==7)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)),oi(4,noop(i)), \
                            oi(5,noop(i)),oi(6,noop(i)),oi(7,noop(i)));
    else if (n==8)
      save, o, string(0), f(oi(1,noop(i)),oi(2,noop(i)),oi(3,noop(i)),oi(4,noop(i)), \
                            oi(5,noop(i)),oi(6,noop(i)),oi(7,noop(i)),oi(8,noop(i)));
  return o;
}

scratch= save(scratch,tmp);
tmp= save(add,pop);
func oxlist (base,..,flat=)
/* DOCUMENT l= oxlist();
   l= oxlist(a1, a2, ..);
   l, add, a1, a2;
   l, add, a1, oxlist(b1, b2, ..);
   l, pop, 2;
   l, pop, 1:3; // return L(1:3)
   *!* all pointers *!*
   o= save();
   l= oxlist(o);
   save,o,pi;
   l(l,1,pi)==pi;
   PURPOSE: general list object, with self-referencing/pointers if sublist/tree,
   unless FLAT==1 in which case sub(ox)lists are unrolled/inserted inline.
   add: append any number of arguments. If one arg is
   an OXLIST object and FLAT==1, only insert "inline" its list/group.
   pop: if void arg then arg==1, if argument is an integer N smaller
   than list length, remove N last list members, if arg N is a range,
   extract list(N)
   IF subroutine call, the list is cropped to REMAINING objects,
   IF function call, the returned OXLIST containing SELECTED objects,
   original object unchanged.
   !CAUTION! calling oxlist() is identical to oxlist([])

   EXAMPLE:
   l= oxlist(pi,save(ji="ho",string(0),[]),sin);
   l,add,indgen(3);
   l,add,oxlist(create("q"));
   l,pop,2;
   SEE ALSO:
*/
{
  ob= base(:);
  l= save();
  save,ob,l;
  save, ob, membs=ob(*,);
  n= more_args();
  write,n,format="more_args starts= %i\n";
  while ((i=more_args())>0) {
    write,i,format="more_args add= %i\n";
    ob,add,next_arg(),flat=flat;
  }
  return ob;
}
func pop (n)
{
  use, l;
  n= is_void(n)? 1: n;
  if (is_range(n)) {
    m= array(char,l(*));
    m(n)= 1;
  }
  if (am_subroutine())
    if (is_range(n))
      l= l(where(!m));
    else
      l= l(*)>n? l(1:-n): save();
  else {
    if (is_range(n))
      ll= l(where(m));
    else
      ll= n>0? l(-n+1:): save();
    o= oxlist();
    save,o,l=ll;
    return o;
  }
}
func add (o,..,flat=)
{
  use,l,membs;
  do {
    if (flat==1 && \
        is_obj(o)>0 && \
        o(*)==numberof(membs)+1 && \
        allof((o(*,)(-,:-1)==membs)(,sum)))
      save,l,[],o(l);
    else
      save,l,string(0),o;
    o= next_arg();
  } while (more_args()>0)
    }
oxlist= closure(oxlist,restore(tmp));
restore, scratch;

func oxdir (dn)
/* DOCUMENT oxdir (dn)
   scan dir tree and output a void-valued oxy objext with keys==file/dir-names;
   use:
   ----
   func rdrdf (o, dn)
   {
   oo= save();
   dn= strpart(dn,0:0)=="/"? dn: dn+"/";
   for (i=1;i<=o(*);i++) {
   if (is_void(o(noop(i))))
   p= rdf(dn+o(*,i));
   else if (is_obj(o(noop(i)))>0)
   rdrdf,o(noop(i)),dn+o(*,i);
   else
   error,"unknown type";
   }
   }
   rdrdf,oxdir(dn),dn;

   SEE ALSO:
*/
{
  dn= strpart(dn,0:0)=="/"? dn: dn+"/";
  local d;
  l= lsdir(dn,d);
  if (structof(l)==long)
    error,"not a directory";
  o= save();
  for (i=1;i<=numberof(l);i++)
    if (l(i)!=string(0))
      save,o,l(i),[];
    else
      l(i);
  for (i=1;i<=numberof(d);i++)
    if (d(i)!=string(0))
      save,o,d(i),oxdir(dn+d(i));
    else
      d(i);
  return o;
}

func oxarr (args)
/* DOCUMENT  oxarr (o,mbnm,bad=,ireg=)
   extract array from a (oxy) group of oxy objects with member name MBNM
   for (o=save(),i=1;i<=3;i++)
   save,o,string(0),save(a=random(5));
   b= oxarr(o,"a");
*/
{
  sk= args(-); //key strings
  nk= numberof(sk);
  k1= "bad";
  k2= "ireg";
  if (nk!=0)
    if (anyof(sk==k1))
      bad= args(k1);
    else
      bad= 0;
  if (nk!=0)
    if (anyof(sk==k2))
      ireg= args(k2);
    else
      ireg= [];
  o= args(1);
  n= o(*);
  mbnm= (args(0,2)==0)? args(-,2): ((args(0,2)==1)? args(2): []);
  if (is_void(mbnm))
    error,"need MEMBNM string or reference name";

  for (i=1;i<=n;i++)
    if (is_obj(o(noop(i)),noop(mbnm),1)<0)
      error,"no such member name in any of the objects in group.";

  if (ireg) {
    nn= array(0,n);
    for (i=1;i<=n;i++)
      if (is_obj(o(noop(i)),noop(mbnm),1)>-1)
        nn(i)= numberof(o(noop(i),noop(mbnm)));
    out= array(structof(o(1,noop(mbnm)))(bad),long(nn(sum)));
    for (j=0,i=1;i<=n;i++)
      if (is_obj(o(noop(i)),noop(mbnm),1)>-1) {
        out(j+1:j+nn(i))= o(noop(i))(noop(mbnm))(*);
        j+= nn(i);
      }
  } else {
    out1= array(structof(o(1,noop(mbnm)))(bad),dimsof(o(1,noop(mbnm))));
    out= array(out1,n);
    for (i=1;i<=n;i++)
      if (is_obj(o(noop(i)),noop(mbnm),1)>-1)
        out(..,i)= o(noop(i))(noop(mbnm));
  }
  return out;
}
wrap_args, oxarr;

scratch= save(scratch, tmp);
tmp= save(eval_);
func funox (base, fun, prot=)
/* DOCUMENT funox (fun, prot=)
   transform any function into a closure function accepting
   a single object argument which collects all original args.

   f= funox(sumintpow);
   f(save(string(0),30,string(0),3))==sumintpow(30,3)
   a= random_n(20,20);
   f= funclos(centroid);
   f(save(string(0),a,"norm",2));
*/
{
  ob= base(:);

  if (is_func(fun)==0)
    error,"missing FUN arg.";
  s= !is_void(prot)? prot: info(fun)(1);
  clos= 0;
  if (strgrepm("closure",s)) {
    s= fun.function_name==string(0)? \
      info(fun.function)(1): \
      info(fun.function(1))(1);
    clos= 1;
  }
  if (strgrepm(s,"builtin"))
    error,"cannot get calling seq.";
  s2= strtok(s,"(");
  fnm= strtok(s2(1)," ")(2);
  s2= strpart(s2(2),:-1);
  s2= strpart(s2,strword(s2,",",20));
  s2= s2(where(s2));
  mk= strgrepm("=$",s2);
  wk= where(mk);
  wp= where(!mk);
  for (i=1,prot=save();i<=numberof(wp);i++)  // positional
    save,prot,string(0),save(arg=[],nm=s2(wp(i)));
  for (i=1;i<=numberof(wk);i++)           // keywords
    save,prot,s2(wk(i)),[];

  np= numberof(where(prot(*,)==string(0)));
  save, ob, fnm, prot, np;

  return closure(ob,eval_);
}
func  eval_ (o)
{
  use, fnm, prot, np;
  npo= numberof(where(o(*,)==string(0)));
  n= min(np,npo);
  s= fnm+"(";
  for (i=1;i<=n;i++)
    s+= (i==1? "": ", ")+swrite(i,format="o(noop(%i))");
  for (i=1;i<=prot(*);i++)
    if (o(*,prot(*,i))>0)
      s+= (i==1? "": ", ")+  \
        swrite(prot(*,i),prot(*,i),format="%s=o(\"%s\")");
  s+= ")";

  return exec(s);
}
funox= closure(funox, restore(tmp)); //(<-*1)
restore, scratch;

func duplicateb(fstrmi,fstrmo,vars,nvars)
/* DOCUMENT duplicateb(fstrmi,fstrmo,vars,nvars)

   Save all the variables in FSTRMI listed in VARS but not in NVARS to
   the file FSTRMO (thus duplicateb). If VARS is void, it is initialized
   with the variable names present in FSTRMI.

   SEE ALSO:
*/
{
  name_list= (is_void(vars)? (*get_vars(fstrmi)(1)): vars);
  if (!is_void(nvars)) {
    idx= where((name_list(,-)!=nvars(-,))(,sum));
    if (is_array(idx))
      name_list= name_list(idx);
    else
      return;
  }
  for(i=1;i<=numberof(name_list);i++)
    ssave, fstrmo,name_list(i),get_member(fstrmi,name_list(i));
}

func use_kwdflt (args)
/* DOCUMENT use_kwdflt,use(),kw1,kw2,...
   sets default value of listed keywords from context object,
   if found as members.
   --- usage#1 ---
   > o= save(a=pi,e=exp(1),c=sqrt(2));
   > func t(o, a=, e=) {use_kdef,o,a,e,c; a; e;c;}
   > c= 1;
   > t,o;
   3.14159
   2.71828
   1       << *NOTE* C not a keyword, thus extern value if not defined
   *OR* ( restore all members for convenience -- prob not a great idea)
   > func t(o, a=) {use_kwdflt,o,a; tmp=save(a); restore,o; restore,tmp; a;}
   *NOTE: restoring O in func *CLOBBERS* all external values with member vals,
   unless LOCAL.
   --- usage#2 ---
   > scratch= save(scratch,tmp);
   > tmp= save(plg_);
   > func graph (base,void) {
   return base(:);
   }
   > func plg_ (y, x, color=, type=) {
   use_kwdflt, use(), color, type;
   plg, y, x, color=color, type=type;
   }
   > graph= closure(graph,restore(tmp));
   > restore, scratch;
   ...
   > g= graph();
   > g, color="blue",type=3;
   > fma;
   > g, plg_, random(10), random(10);
   > g, plg_, random(10), random(10),color="red";
   > g, plg_, random(10), random(10),type=0;
*/
{
  obj= args(1);
  if (is_void(obj))
    return []; /* do nothing if not in oxy context */
  if (!is_obj(obj))
    error, "expecting an oxy object has first argument";
  for (i=2; i<=args(0); i++)
    if (is_void(args(i)) && args(0,i)==0 && is_obj(obj,args(-,i),1)==0)
      args, i, obj(args(-,i));
}
wrap_args, use_kwdflt;
