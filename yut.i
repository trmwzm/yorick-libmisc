require, "fcomplex.i";

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
  x= x +.5
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

func struct_element(stru,&name,&type,&strutype)
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
  if (typeof(stru) != "struct_definition") stru= structof(stru);

  /* print a representation of the structure */
  stru= print(stru);

  if (stru(1)=="[]") {name= type= string([]); return name;}
  /* find the name of each element */
  strutype= strpart(stru(1),8:-2);
  if (numberof(stru)==2) {name= type= string([]); return name;}
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

func structeq(structDef1, structDef2, noname =) {
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

/* ------------------------------------------------------------------------ */

func check_file (fnm,..,quiet=)
/* DOCUMENT check_file (fnm)
 */
{
  list= _lst(fnm(1));
  nf= 1;
  while (nf<numberof(fnm)) list= _cat(list,fnm(1+nf++));

  while (more_args()) {
    fnm= next_arg();
    list= _cat( list, fnm(1));
    nfi= 1;
    while (nfi<numberof(fnm)) list= _cat(list,fnm(1+nfi++));
    nf+= nfi
  }
  for (j=1;j<=nf;j++)
    if (!open((fnm=_car(list,j)),"r",1))
      if (quiet)
        return 0;
      else
        error,"***File: "+fnm+" not found";

  return 1;

}
/* ------------------------------------------------------------------------ */

func check_dir (fnm,..,quiet=)
/* DOCUMENT check_dir (fnm)
 */
{
  list= _lst(fnm(1));
  nf= 1;
  while (nf<numberof(fnm)) list= _cat(list,fnm(1+nf++));

  while (more_args()) {
    fnm= next_arg();
    list= _cat( list, fnm(1));
    nfi= 1;
    while (nfi<numberof(fnm)) list= _cat(list,fnm(1+nfi++));
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

func diradd(s1,s2)
/* DOCUMENT diradd(s1,s2)==s1+s2
   unless s2 is ABSOLUTE path in that case == s2
   SEE ALSO:
 */
{
  return (strpart(s2,1:1)=="/"? s2: s1+s2);
}

/* -------------------------------------------------------------------*/

func waitff(fls,tlim,dt=)
/* DOCUMENT waitff (fls,tlim,dt=)
   wait for files FLS, with max TLIM (secs), check at interval DT (secs, default 5)
   returns time in secs.
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
  if (t>=tlim)
    error,"alloted time elapsed: "+pr1(tlim)+"hr";
  return t-2;
}

/* ------------------------------------------------------------------------ */

func runwaitsafe (cmd,v=,tlim=,dt=)
/* DOCUMENT runwaitsafe (cmd,v=,tlim=)

   SEE ALSO:
 */
{
  utime= 0;
  timestamp, utime;
  rnd= long(random()*1e6);
  stmp= swrite(rnd,utime,format="tic-%ld-ut-%ld");
  cmd= strpart(cmd,0:0)==";"? strpart(cmd,1:-1): cmd;
  cmd2= "rm -f "+stmp+";"+cmd+"; touch "+stmp+";";
  if (v==1)
    write,cmd2;
  sysafe,cmd2;
  t= waitff(stmp,tlim,dt=dt);
  remove,stmp;
  return t;
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

func crossvec (a,b,seq=) {
/* DOCUMENT crossvec(a,b,seq=)
   for one to one sequence of cross products, specify seq == 1
 */
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
  if (is_voi(spacer))
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

  if (da(1) != db(1))
    error, "a and b must have same numberof of dimensions";

  da= da(2:);
  db= db(2:);

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
        a= array(structof(args(2,:)),_(ndim,da));
        a(w)= args(2,:)(ia);
      } else {
        if (!am_subroutine()) {
          b= a;
          b(w)= args(2,:)(ia);
          return b;
        } else {
          a(w)= args(2,:)(ia);
        }
      }
    } else {
      if (ida) {
        a= array(structof(args(2,:)),_(ndim,da));
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
    error,"grad returns EITHER deriv_a (deriv=1) OR deriv_x (derivx=1)"
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
  for (n=1,i=1;i<=dt(1);i++)n*= dt(i+1);
  out= reform((indim(indgen(n),dt)-1)*dtlo+1,_(dt(1)+1,dt(1),dt(2:)))+off;
  return out;
}

/*-------------------------------------------------------------------------------------*/

func tile_ndx (it,dd,dtlo,ovl,&din,&dout,&tin,&tout)
/* DOCUMENT  tile_ndx (it,dd,dtlo,&din,&dout,&tin,&tout)
  first inputs id as those of "tile";
  dd=    data dimension as in dimsof(data)
  dtlo=  dimension ONLY (no preceeding rank) of output tile.
  ovl=   one-side overlap (0 if none)
  USAGE
  ====
  test driver.....
  func tile_test (dum)
  {
    dd= [2,100,400]
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

func strsplit( str, delim)
/* DOCUMENT strsplit(str, delim)
 * Decompose string STR into an array of component strings separated by
 * character DELIM, and return an array of the component strings.
 * If DELIM is the null string, split string into its constituent one-character
 * strings.
 * (Reverses the action of STRCOMBINE)
 * SEE ALSO: strrepl, strcombine
 */
{
  NULL= [];

  if (delim == "") {
    // Split string into individual character strings
    nlen= strlen(str);
    if (nlen == 0) return  str;
    str2= array("",nlen);
    for (j=1; j <= nlen; j++) str2(j)= strmid( str, j-1, 1);
    return  str2;
  }

  str2= str;

  str_array= NULL;
  while ( ((i= strpos(str2,delim))) >= 0 ) {
    if (i == 0) {
      grow, str_array, "";
    } else {
      grow, str_array, strmid(str2,0,i);
    }
    str2= strmid(str2,i+1,strlen(str2)-i-1);
  }

  grow, str_array, str2;

  return  str_array;

}

/* ------------------------------------------------------------------------ */

func strcombine( str_array, delim)
/* DOCUMENT strcombine(str_array,delim)
 * Concatenate an array of strings STR_ARRAY into a single string,
 * with each substring separated by character DELIM (which should not occur in
 * any of the strings.
 * If DELIM is the null string, simply all concatenate all the strings in
 * STR_ARRAY.
 * (Reverses the action of STRSPLIT)
 * SEE ALSO: strrepl, strsplit
 */
{
  NULL= [];

  catstr= "";
  nstr= numberof(str_array);
  for (j=1; j <= nstr; j++) {
    if (delim != "") {
      if (strpos(str_array(j), delim) >= 0)
        error, "Error - delimiter occurs in string";
    }
    if (j == 1) {
      catstr= str_array(j);
    } else {
      catstr= catstr + delim + str_array(j);
    }
  }

  return catstr;

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
    if (dimsof(colors)(2)==3&&structof(colors)==char)
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
  for (i=1;i<=nlevs;i=i+nskip)plt,pr1(levs(i)),xl + i*xdd,ylabs,height=12,justify="CH";

  pldj, xl, yb, xr, yb;
  pldj, xl, yt, xr, yt;
  pldj, xl, yb, xl, yt;
  pldj, xr, yb, xr, yt;

  plsys_set, lastsys;

}

/*---------------------------------------------------------------------------*/

func plt_clic(strg, orient=, height=, justify=, font=, color=, hide=, opaque=,prompt=)
/* DOCUMENT plt_clic(strg, orient=, height=, justify=, font=, color=, hide=, opaque=,prompt=)
   plt, strg, q(5), q(6),font=font, justify=justify, height=height, orient=orient,\
       color=color,hide=hide,opaque=opaque
   [x_pressed, y_pressed, x_released, y_released,
    xndc_pressed, yndc_pressed, xndc_released, yndc_released,
    system, button, modifiers]
 */
{
  if (prompt==1)write,"clic in plot window to position string\n"+strg;
  do {q= mouse(-1,0,"");} while(is_void(q));q;

  if (is_void(height)) height=pltitle_height;
  if (is_void(justify)) justify="CH";
  if (is_void(font)) font=pltitle_font;

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

func colorbar(cmin, cmax,offset=, colors=, tit=)
/* DOCUMENT colorbar
            colorbar, cmin, cmax
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

  if (!is_void(cmin)) {
    plt, swrite(cmin,format="%-#06.3lg"), xmid, y0-0.004, justify="CT";
    plt, swrite(cmax,format="%-#06.3lg"), xmid, y1 , justify="CB";
  }

  if (tit) plt, tit, xmid, y1+0.02 , justify="CB",height=14;
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
  plsys, 0
  if (!is_void(colors)) {
    pli, char(colors(zcol)), xl, yb, xr, yt;
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
  for (i=1;i<=nlevs;i=i+nskip)plt,pr1(levs(i)),xlabs,yb + i*ydd,height=12,justify="LH";

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
      if (!is_void(oi) && is_func(oi)==0) // used to thow error
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
    args, i, x;
    i+= 1;
  }
}
wrap_args, restore_rec;

func oxcopy (o)
/* DOCUMENT oxcopy (o)
   recursive oxy object copy
 */
{
  oo= o(:);
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    if (is_obj(oi))
      save,oo,o(*,i),oxcopy(oi);
  }
  return oo;
}

func oxtypeq (o1,o2,nodim=)
/* DOCUMENT oxtypeq (o1,o2)
   recursive oxy object type/dimesion checks
 */
{
  s1= o1(*,);
  s2= o2(*,);
  if (numberof(s1)) s1= s1(sort(s2));
  if (numberof(s2)) s2= s2(sort(s2));
  if (anyof(s1!=s2))
    return 0
  else
    tf= 1;

  for (i=1; i<=o1(*); i++) {
    o1i= s1(i)? o1(s1(i)): o1(noop(i));
    o2i= s2(i)? o2(s2(i)): o2(noop(i));
    if (is_obj(o1i))
      tf*= oxtypeq(o1i,o2i);
    else
      tf*= structof(o1i)==structof(o2i) &&\
           (nodim==1 || allof(dimsof(o1i)==dimsof(o2i)));
  }
  return tf;
}

func oxyeq (o1,o2,nodim=)
/* DOCUMENT oxyeq (o1,o2)
   recursive oxy object identity
 */
{
  s1= o1(*,);
  s2= o2(*,);
  ss2= sort(s2);
  if (numberof(s1))
    s1= s1(ss2);
  if (numberof(s2))
    s2= s2(ss2);
  if (anyof(s1!=s2))
    return 0
    else
      tf= 1;

  for (i=1; i<=o1(*); i++) {
    o1i= s1(i)? o1(s1(i)): o1(noop(i));
    o2i= s2(i)? o2(s2(i)): o2(noop(i));
    if (is_obj(o1i)) {
      tf*= oxyeq(o1i,o2i);
    } else {
      if (is_numerical(o1i)) {
        tt= allof(o1i==o2i);
        if (tt==0) {
          write,(s1(i)? s1(i): pr1(i)),format="warning: problem with %s - ";
          write,2.0*avg(o1i-o2i)/avg(o1i+o2i),format="mean rel. diff.: %lg\n";
        }
      } else if (is_string(o1i)) {
        tt= allof(o1i==o2i);
        if (tt==0) {
          write,(s1(i)? s1(i): pr1(i)),format="warning: problem with %s - ";
          write,o1i,o2i,format="string comparison s1 vs. s2: %s != %s\n";
        }
      }
      tf*= structof(o1i)==structof(o2i) &&               \
        (nodim==1 || allof(dimsof(o1i)==dimsof(o2i))) && \
        tt;
    }
  }
  return tf;
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
   o1= save(o,p1=1);
   o2= save(o1,p2=2);
   o22= o2(:);
   o3= save(o2,o22,p3=3);

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
  sk= args(-); //key strings, one reserved key
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
      if (nm!=k1)
        if (is_obj(args(nm)))
          oxsave,args(1),noop(nm),args(nm),_wrk_=1;
        else
          save,args(1),noop(nm),args(nm);
    }
    i= 2;
    while (i<=args(0))  {
      ar= args(i);
      if (args(0,i)==1 && is_string(ar) && is_scalar(ar)) {
        i+= 1;
        oi= args(i);
        if (is_void(oi)) error,"cannot save void";
        if (is_obj(oi))
          oxsave,args(1),noop(ar),oi,_wrk_=1;
        else
          save,args(1),noop(ar),oi;
      } else
        if (is_obj(ar))
          oxsave,args(1),args(-,i),ar,_wrk_=1;
        else
          save,args(1),args(-,i),ar;
      i+= 1;
    }
  } else if (_wrk_==1) {
    if (args(0,4)==2)
      onm= save(string(0),args(2));
    else
      onm= args(4);
    nm= "";
    for (i=1; i<=onm(*); i++) nm+= onm(noop(i))+".";
    if (is_string(args(1)))
      f= createb(args(1));
    else
      f= args(1);
    o= args(3);
    for (i=1,in=1,ino=1; i<=o(*); i++) {
      oi= o(noop(i));
      if (is_obj(oi)) {
        vnm= o(*,i);
        // if (strmatch(vnm,".") || vnm==string(0))
        //   error,"group name cannot contain \".\" or be string(0): "+vnm;
        if (vnm==string(0))
          vnm= swrite(ino++,format=GRPOXSV+"%03i")
        save, onm, string(0), vnm;
        f= oxsave(f,vnm,oi,onm,_wrk_=1);
      } else {
        vnm= o(*,i);
        // if (strmatch(vnm,".") || vnm==string(0))
        //   error,"variable name cannot contain \".\" or be string(0): "+vnm;
        if (vnm==string(0))
          vnm= swrite(in++,format=GRPOXSV+"%03i")
        save_rec,f,vnm,oi,_prf=nm;
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
        save,oj,on(0),val;
      }
      args,3,oo(1);
    }
  }
}
wrap_args,oxrestore;

func oxwrite (f,o,&onm,odoc)
/* DOCUMENT oxwrite, ob, "obname", odoc;
   #include "qq.i";
   oxwrite,open("q.i","w"),o3,"ob",oxcomdoc ("qq.i","ob");
   TODO: fix for groups
   SEE ALSO: oxsave,oxread,oxcopy,oxtypeq;
 */
{
  if (is_string(f)) f= open(f,"w");

  if (is_void(onm)) onm= "cfg";

  tablen= 26;

  if (is_string(onm)) {
    onm= save(string(0),onm);
    // stash scratch
    s= info(o);
    ws= where(strmatch(s,"= object with"));
    if (numberof(ws)) {
      s= discrete(strtrim(strtok(s(ws),"=")(1,)));
      write,f,swrite(s,format=",%s")(sum),\
        format="scratch= save(scratch%s);\n\n";
    }
  }
  s= swrite(onm(0),format="%s= save();");
  sl= tablen*(strlen(s)/tablen+1)-strlen(s);

  if (is_obj(odoc,"help",1)>=0 && odoc("help")!="")
    if ((sh=odoc("help"))!="")
      s= swrite(s,"// "+sh,format="%s"+array(" ",sl)(sum)+"%s");
  write,f,s,format="%s\n";

  print_format,float="%.9g",double="%.9g",complex="%.9g+%.9gi";
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    if (is_obj(oi)) {
      save, onm, string(0), o(*,i);
      f= oxwrite(f,oi,onm,(!is_void(odoc)? odoc(i+1):[]));
    } else {
      s= swrite(onm(0),o(*,i),pr1(oi)(*)(sum),\
             format="save, %s, %s"+(o(*,i)? "=": "string(0),")+" %s;");
      sl= tablen*(strlen(s)/tablen+1)-strlen(s);
      if (is_obj(odoc) && i+1<=odoc(*))
        if (odoc(i+1)!="")
          s= swrite(s,"// "+odoc(i+1),format="%s"+array(" ",sl)(sum)+"%s");
      write,f,s,format="%s\n";
    }
  }
  print_format;

  if (onm(*)>1) {
    write,f,onm(-1),onm(0),onm(0),format="save, %s, %s= %s(:);\n";
    onm= onm(:-1);
  } else
    write,f,"\nrestore, scratch;",format="%s\n";
  return f;
}
func oxisfunc (o)
{
  n= o(*);
  m= array(0,n);
  for (i=1;i<=n;i++)
    m(i)= is_func(o(noop(i)));
  return m;
}
func oxcomdoc (f,nm)
/* DOCUMENT
 */
{
  l= text_lines(f);
  ln= strlen(l);
  ms= !strgrepm("scratch",l);
  s1= strgrep("^ *save.*=",l,n=1);
  s11= strgrep("^ *save.*= .*\\( *: *\\)",l,n=1);
  m1= s1(2,)>0 & ms & !(s11(2,)>0);
  s2= strgrep("^.*= *save *\\(",l,n=1);
  m2= s2(2,)>0 & ms;
  s3= strgrep("//",l,n=1);
  // comments
  m= s3(2,)>0;
  // lines to edit
  mm= (m1 | m2);
  c= array("",numberof(ln));
  if (anyof(m)) {
    w= where(m);
    s3(1,w)= s3(2,w);
    s3(2,w)= ln(w);
    c(w)= strtrim(strpart(l,s3))(w);
  }
  if (anyof(mm)) {
    ww= where(mm);
    l(ww)= (strpart(l,s2)+strpart(l,s1))(ww);
  }
  if (anyof(m2)) {
    w2= where(m2);
    l(w2)+= "help=";
  }
  l(ww)+= swrite(c(ww),format="\"%s\"");
  if (anyof(m1)) {
    w1= where(m1);
    l(w1)+= ";";
  }
  if (anyof(m2)) {
    w2= where(m2);
    l(w2)+= ");";
  }
  if (is_void(nm))
    return l;
  l= streplace(l,strgrep("^ *"+nm+" *=",l,n=1),"_o_=");
  l= streplace(l,strgrep(", *"+nm+" *,",l,n=1),",_o_,");
  include,l,1;
  return _o_;
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
  while (more_args()>0)
    ob,add,next_arg(),flat=flat;
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

func use_kdef (args)
/* DOCUMENT use_kdef,use(),kw1,kw2,...
   sets default value of listed keywords from context object,
   if found as members.
   usage ....
   graph= save(plg_);
   func plg_ (y, x, color=, type=) {
     use_kdef, use(), color, type;
     plg, y, x, color=color, type=type;
   }
   graph= restore(graph);
   save, graph, color="blue",type=3;
   fma;
   graph, plg_, random(10), random(10);
   graph, plg_, random(10), random(10),color="red";
   graph, plg_, random(10), random(10),type=0;
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
wrap_args, use_kdef;
