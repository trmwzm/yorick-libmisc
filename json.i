require, "yut.i";
require, "ieee.i";

scratch= save(scratch, jsn2ox, jsnox_scan, jsnox_match, jsnox_cut, jsnox_split, jsnox_parse, \
              ox2jsn, oxjsn_wrkr, oxjsn_scan, oxjsn_pr, ojd, str2str, oxjsb_write, oxjsb_out, \
              oxjsb_wrap, jsbox_in, jsbox_read, jsbox_add_fcomplex);
/* NOTES:
   1. empty array value "[]" ought to map to yorick void [] (?)
   2. empty object value "{}" ought to map to yorick empty object save() (?)
   3. what is null  (could make it string(0) ?)
   4. what is true/false (0/1)
   5. ...
 */
// contant ASCII values
// ascst= save();
// bslash= 0x5c;
// fslash= 0x2f;
// dquote= 0x22;
// lbrace= 0x7b;
// rbrace= 0x7d
// lbracket= 0x5b;
// rbracket= 0x5d;
// minus= 0x2d;
// plus= 0x2b;

// escapes = [[0x22,0x22],        // \" quote
//            [0x2f,0x2f],        // \/ forward slash
//            [0x62,0x08],        // \b backspace
//            [0x66,0x0c],        // \f form feed
//            [0x6e,0x0a],        // \n newline
//            [0x72,0x0d],        // \r carriage return
//            [0x74,0x09],        // \t tab
//            [0x5c,0x5c]];       // \\ bslash

 // digit= char(indgen('0':'9'));
// digit19= char(indgen('1':'9'));
// hexdigit= char(grow(indgen('0':'9'), indgen('a':'f'), indgen('A':'F')));

ojd= save();
save, ojd, "[]", [];
save, ojd, "null", string(0);
save, ojd, "{}", save();
save, ojd, "true", 1;
save, ojd, "false", 0;
save, ojd, "[{}]", save(); // this one not

func ox2jsn (args)
{
  // expepected args: oxy_object/json-value, FMT=
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"string or ox, optional fmt (see totxt).";

  // one and only one argument
  if (args(0,1)==0)
    onm= args(-,1);
  else
    onm= [];
  ob= args(1);

  // FMT= keyword argument
  local fmt;
  if (args(0)>1)
    fmt= args(2);

  tablen= 2;
  quotestring= 1;

  s= "";
  nt= 0;

  if (!is_obj(ob) && !is_void(onm))
    {s+= "\{"+"\n"; nt+= 1;}
  if (is_obj(ob) && !is_void(onm))
    {s+= "\{"+"\n"; nt+= 1;}
  use_method,oxjsn_wrkr,s,ob,onm,nt;
  if (!is_obj(ob) && !is_void(onm))
    s+= "\n\}\n";
  if (is_obj(ob) && !is_void(onm))
    s+= "\n\}\n";
  return s;
}
wrap_args,ox2jsn;

func jsn2ox(s)
{
  if (is_void(s))
    error,"string (or string array) single arg.";
  if (!is_string(s))    // JSON -> OX
    error,"string (or string array) single arg.";
  if (dimsof(s)(1)>0) {
    g= strgrep("^[^\\]*\\\\\\\\[^\"]+", s);                // standard: *NO* comments in json
    w= where(g(dif,)>0);
    if (numberof(w))
      s(w)= strpart(s(w),g(,w)-[0,2]);
    // g= strgrep("^[^//]*//", s);                // "//" separated comments couuld be path!!
    // w= where(g(dif,)>0);
    // if (numberof(w))
    //   s(w)= strpart(s(w),g(,w)-[0,2]);
    s= s(sum);
  }
  s2= use_method(str2str,s);
  // remove new lines and tabs
  for (i=1;i<=dimsof(s2)(2);i++)
    s2(i,1)= streplace(s2(i,1),strgrep("[\t\n\r\b\f ]",s2(i,1),n=max(1,strlen(s2(i,1)))),"");
  s2l= strlen(s2);
  s= use_method(str2str,s2);
  sl= strlen(s);

  // put back (single) string spaces
  // length
  m2= array(1,sl);
  if (dimsof(s2)(1)>1) {
    for (i=1,k=0;i<=dimsof(s2l)(2);i++) {
      for(j=1;j<=s2l(i,2);j++)
        m2(k+s2l(i,1)+j)= 0;
      k+= s2l(i,1)+s2l(i,2);
    }
  }

  // brackets & co everywhere but in strings (& m2)
  m= strchar(s)(:-1)==strchar("{")(1) & m2;
  m-= strchar(s)(:-1)==strchar("}")(1) & m2;
  mbc= m(cum);
  m= strchar(s)(:-1)==strchar("[")(1) & m2;
  m-= strchar(s)(:-1)==strchar("]")(1) & m2;
  mbs= m(cum);
  // commas
  mcm= strchar(s)(:-1)==strchar(",")(1) & m2;
  // colons
  mcl= strchar(s)(:-1)==strchar(":")(1) & m2;
  // out
  os= save(s,sl,mbc,mbs,mcm,mcl);  // order matters!

  return use_method(jsnox_parse,os);
}

func oxjsn_wrkr (&s, ob, onm, &nt)       // JSON -> OX
{
  extern fmt,tablen,quotestring;
  slast= strpart(s,0:0);
  //
  t= (nt? strchar(array('\x20',nt*tablen)): "");
  tq= strchar(array('\x20',tablen));
  if (is_oxgrar(ob)) {
    local ier;
    oo= arr_oxgr(ob,ier);
    if (!is_void(oo) && ier!=1)
      ob= oo;
  }
  // only for user input not an object
  if (!is_obj(ob)) {
    s+= (onm? t+"\""+onm+"\": ": "")+use_method(oxjsn_pr,ob,fmt);
    return;
  }
  s+= (onm? t+"\""+onm+"\":\n": "");
  if (is_group(ob))
    s+= t+"[";
  else
    s+= t+"{";
  nt+= 1;
  on= ob(*);
  s+= on>0? "\n": "";
  for (i=1;i<=on;i++) {
    oi= ob(noop(i));
    oni= ob(*,i);
    use_method,oxjsn_wrkr,s,oi,oni,nt;
    s+= (i==on? "": ",\n");
  }
  nt-= 1;
  s0= strpart(s,0:0);
  s+= (s0=="\n" || s0=="{")? "": "\n";
  if (is_group(ob))
    s+= (s0== "["? "": t)+"\]";
  else
    s+= (s0== "{"? "": t)+"\}";
}

func oxjsn_pr (a,fmt)
{
  extern quotestring;
  if (is_void(fmt))
    fmt=-0.12;
  if (is_integer(a))
    fmt= abs(fmt); //  no hex
  if (is_void(a))
    return "[]";
  da= dimsof(a);
  q= quotestring==1? "\"": "";
  if (is_string(a)) {
    sa= q+strtrim(a)+q;
    w= where(a==string(0));
    if (numberof(w))
      sa(w)= "null";
  } else {
    print_format,2000;
    sa= totxt(a,fmt)(1);
    print_format,-1;
  }

  if (da(1)>0) {
    n= numberof(a);
    s= da(:-1);
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
    b= strchar("[]");
    for (i=1;i<=n;i++) {
      if (m(i)>0)
        sa(i)= sa(i)+strchar(array(b(2),m(i)));
      if (m(i)<0)
        sa(i)= strchar(array(b(1),-m(i)))+sa(i);
      if (i<n)
        sa(i)+= ", ";
    }
    return sa(*)(sum);
  } else
    return sa;
}

func jsnox_scan (os, i, cnm)
{
  if (!is_obj(os))
    error,"expecting object.";
  icnm= os(*,cnm);
  if (noneof(cnm==os(*,3:)) || icnm<3)
    error,"unexpected scan target, allowed: "+os(*,3:);
  l= os(sl);
  if (i<1 || i>l)
    error,"scan start index out of string.";
  while (i<=l && ((icnm<5 && os(noop(cnm),i+1)==os(noop(cnm),i)) || \
                  (icnm>4 && !os(noop(cnm),i))))
    i= i+1;
  return max(i,1);
}

func jsnox_match (os, i)       // JSON -> OX
{
  if (!is_obj(os))
    error,"expecting object.";
  p= os(sl)+1;
  if (i<1 || i>p-1)
    error,"invalid search start index: "+pr1(i);
  iscr= strpart(os(s),i:i)=="{";
  issq= strpart(os(s),i:i)=="[";
  if (!iscr && !issq)
    error,"Expecting { or [";
  i0= i;
  j= 0;
  m0= mi= iscr? os(mbc,i): os(mbs,i);
  while(j==0 || (j>0 && i<=p && mi!=m0)) {
    if (j==0 && mi!=m0)
      j= 1;
    mi= iscr? os(mbc,i): os(mbs,i);
    i++;
  }
  return min(os(sl),i-2);
}

func jsnox_cut (os,i,j)
{
  if (!is_obj(os))
    error,"expecting object.";
  if (j<i)
    error,"substring index end<start.";

  ss= strpart(os(s),i:j);
  return save(s= ss,                                      \
              sl= strlen(ss),                             \
              mbc= os(mbc,i:j+1),                         \
              mbs= os(mbs,i:j+1),                         \
              mcm= os(mcm,i:j),                           \
              mcl= os(mcl,i:j));
}

func jsnox_split (os, &isar)
// splits CONTENT of bracket
// ISAR== 0: object, or val                   : object with named members
//        1: arrays of strings or numbers     : arrays of string or numbers
//        2/3: arrays of obects/arrays        : object of UNnames members
//        4:                                  : string or number
//
// 2 cases: (1) k:v,k:v,... (2) v,v,...
// where value V could be string/number/(special val not impl.)/object-in-brackets
{
  if (!is_obj(os))
    error,"expecting object.";
  // find out if (1) k:v,k:v, ... or
  //             (2) v,v,v -- the array case, within square brackets
  isar= 0;   // 0-object, 1-array of strings/numbers, 2: array of objects, 3: array of arrays
  i= 1;
  do {
    if (os(mcm,i))                  // comma first, must be array
      { isar= 1; break; }
    else if (os(mcl,i))             // colon first, must be object, isar==0
      break;
    else if (os(mbc,i+1)>os(mbc,i)) // curly brackets first, array of objects
      { isar= 2; break; }
    else if (os(mbs,i+1)>os(mbs,i)) // square bracket first, array of arrays
      { isar= 3; break; }
  } while (i++<os(sl));

  if (i==os(sl)+1) {
    ss= streplace(os(s),strgrep("\"",os(s),n=max(1,os(sl))),"");
    n= tonum(ss);
    isi= strgrepm("[.edED]",ss)==0;
    isar= 4;
    return (n>-1e99? (isi? long(n): n): ss);
  }
  // do the split
  k= 1;
  l= 1;
  o= save();
  do {
    if (isar==0) {
      i= k;
      l= use_method(jsnox_scan,os,i,"mcl");
      if (anyof(strpart(os(s),l+1:l+1)==["[","{"])) {
        j= use_method(jsnox_match,os,l+1);
      } else {
        j= use_method(jsnox_scan,os,l,"mcm")-1;
      }
    } else if (isar==1) {
      i= k;
      j= use_method(jsnox_scan,os,i,"mcm")-1;
    } else if (isar>1) {   // array of objects(2) or arrays(3)
      if (strpart(os(s),k:k)==",")
        k= k+1;
      if ((ch=strpart(os(s),k:k))!=(isar==2? "{": "[")) // <<<<<<<<<<< BUG!!
        error,"Expecting brakcet and got: "+ch;
      i= k+1;
      j= use_method(jsnox_match,os,k)-1;
    } else
      error,"unrecognized sequence.";
    // char following last closed bracket
    k= j+2;
    if (isar==0)
      save, o, strpart(os(s),i:l-1), use_method(jsnox_cut,os,l+1,j);
    else if (isar==1)
      save, o, string(0), use_method(jsnox_cut,os,i,j)(s);
    else
      if (j>i+1)
        save, o, string(0), use_method(jsnox_cut,os,i,j);
      else
        save, o, string(0), os(s);
  } while (k<os(sl));

  if (isar==1)  {
    a= array(string,o(*));
    for (i=1;i<=o(*);i++)
      a(i)= o(noop(i));
    if (tonum(a(1))>-1e+99 && allof((nump=tonum(a))>-1e99)) {  // the string is an array of numericals
      isi= allof(strgrepm("[.edED]",a)==0);
      a= isi? long(nump): nump;
    }
    o= a;
  }
  return o;
}

func jsnox_parse (os)
// takes a bracket and returns its content in object o
// ISAR== 0: object, or val                   : object with named members
//        1: arrays of strings or numbers     : arrays of string or numbers
//        2/3: arrays of obects/arrays        : object of UNnames members
//        4:                                  : string or number
{
  use,ojd;
  if (!is_obj(os))
    if (is_obj(ojd,noop(os),1)>=0)
      return ojd(noop(os));
    else
      error,"expecting object.";
  local isar;

  // deal with peeling outer {}
  i= min(use_method(jsnox_scan,os,1,"mbc"),use_method(jsnox_scan,os,1,"mbs"));
  ss= strpart(os(s),1:i);
  if (anyof(strtrim(ss)==["{","["])) {
    j= use_method(jsnox_match,os,i);
    if (j==strlen(os(s)))
      if (j<=(i+1))
        return use_method(jsnox_parse,os(s));
      else
        os= use_method(jsnox_cut,os,i+1,j-1);
  }
  ss= i= [];

  oi= use_method(jsnox_split,os,isar);
  if (isar==0) {                    // 0: object
    o= save();
    for (i=1;i<=oi(*);i++) {
      mbn= streplace(oi(*,i),strgrep("\"",oi(*,i),n=4),"");
      save,o,noop(mbn),use_method(jsnox_parse,oi(noop(i)));
    }
  } else if (isar==4 || isar==1) {  // 1: arrays of strings OR numbers
    o= oi;
    if (is_string(oi)) {
      oi= strtrim(oi);
      if (numberof((w=where(oi=="null"))))
        o(w)= string(0);
      else {
        wt= where(oi=="true");
        wf= where(oi=="false");
        nt= numberof(wt); nf= numberof(wf);
        if (nt || nf) {
          if (nt+nf==numberof(oi)) {
            o= array(0,dimsof(oi));
            if (numberof(wt))
              o(wt)= 1;
            if (numberof(wf))
              o(wf)= 0;
          } else
            error,"mixed types string and bool.";
        }
      }
    }
  } else if (isar==2 || isar==3) {   // 2/3: arrays of obects/arrays OR 4:string or number
    o= save();
    for (i=1;i<=oi(*);i++)
      save,o,string(0),use_method(jsnox_parse,oi(noop(i)));
  }
  return o;
}

func str2str (s)
{
  l0= 100;
  k= 1;
  if (is_scalar(s)) {
    w= where(strchar(s)(:-1)==strchar("\"")(1));
    nw= numberof(w);
    if (nw==0) {
      s1= string(0);
      s2= s;
    } else if (nw%2==0) {
      w(::2)-= 1;
      s1= _(strpart(s,w),string(0));
      s2= strpart(s,_(0,w,strlen(s)));
    } else
      error,"unbalanced quotes!";
    return [s2,s1];
  } if (is_array(s)) {
    if (dimsof(s)(0)!=2)
      error,"expecting last dimension==2, 1:unquoted, 2:quoted segments.";
    return transpose(s)(*)(sum);
  }
}

func oxjsb_wrap (args)
{
  if (args(*)!=1)
    error,"arg required.";
  k= ["szmx","rootdir","onm","append","update","fcomplex","ocall"];
  kk= args(*,);
  for (i=1;i<=numberof(kk);i++)
    if (noneof(kk(i)==k))
      error,"unknown keyword: "+kk(i);
  return use_method(oxjsb_write, args(1), \
                    szmx= args("szmx"), \
                    rootdir= args("rootdir"), \
                    onm= args("onm"), \
                    append= args("append"), \
                    update= args("update"), \
                    fcomplex= args("fcomplex"), \
                    ocall= args(*,1));
}
wrap_args, oxjsb_wrap;

func oxjsb_write (o, szmx=, rootdir=, onm=, append=, update=, fcomplex=, ocall=)
{
  szmx= is_void(szmx)? 200: szmx;
  oo= save();
  rootdir= is_void(rootdir)? ".": rootdir;
  onm= is_void(onm)? "": onm;

  ig= is_group(o);

  if (strpart(rootdir,0:0)=="/")
    rootdir= strpart(rootdir,:-1);
  if (strpart(onm,0:0)=="/")
    onm= strpart(onm,:-1);

  if (!is_obj(o))
    return ((is_numerical(o) && sizeof(o)>szmx && ocall!=string(0))?
            use_method(oxjsb_out,rootdir+"/"+ocall, o,          \
                       append=append,update=update,fcomplex=fcomplex): o);
  
  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    oinm= o(*,i);
    if (ig)
      oinm+= swrite(i,format="grp_%04d");
    if (is_obj(oi))
      save, oo, noop(oinm), use_method(oxjsb_write, oi, szmx=szmx,rootdir=rootdir,onm=onm+"/"+oinm, \
                                       append=append,update=update,fcomplex=fcomplex);
    else
      save, oo, noop(oinm),                                             \
        ((is_numerical(oi) && sizeof(oi)>szmx)? use_method(oxjsb_out,rootdir+onm+"/"+oinm, oi, \
                append=append,update=update,fcomplex=fcomplex): oi);
  }
  return oo;
}

func oxjsb_out (fnm, x, append=, update=, fcomplex=)
// &BINIO
//  T%FNM="out.dat",
//  T%TNM="real(sp)            ",
//  T%KND=4          ,
//  T%TSZ=32         ,
//  T%RK=2          ,
//  T%SHP=26880      ,70         , 2*0          ,
//  T%SZ=1881600    ,
{
  mkdirp, dirname(fnm);
 
  fd= fnm+".dat";
  fj= fnm+".jsb";
 
  if (append==1)
    f= open(fd,"ab");
  else if (update==1)
    f= open(fd,"r+b");
  else
    f= open(fd,"wb");

  if (fcomplex==1)
    use_method(jsbox_add_fcomplex,f);

  if (structof(x)==complex)
    save, f, complex;

  _write, f, 0, x;

  fsz= sizeof(f);

  dx= dimsof(x);
  
  oj= save(fnm= fd, \
           tnm= (fcomplex? "fcomplex": typeof(x)), \
           tsz= sizeof(structof(x)), \
           rk= dx(1), \
           shp= dx(2:), \
           bige= merge2(0,1,native_fix(2,1)==-1));

  for (i=1,p=1;i<oj(rk);i++)
    p*= oj(shp,i);
  if (append==1)
    save, oj, shp=_(oj(shp,:-1),fsz/(oj(tsz)*p));

  write, open(fj,"w"), oxjsn(noop(oj)), format="%s\n";
  oj= save(json_raw_obj=oj);

  return oj;
}

func jsbox_read (o, rootdir=, onm=, memapsz=)
{
  oo= save();
  rootdir= is_void(rootdir)? ".": rootdir;
  onm= is_void(onm)? "": onm;
  if (strpart(rootdir,0:0)=="/")
    rootdir= strpart(rootdir,:-1);
  if (strpart(onm,0:0)=="/")
    onm= strpart(onm,:-1);

  for (i=1; i<=o(*); i++) {
    oi= o(noop(i));
    oinm= o(*,i);
    if (o(*)==1 && is_obj(o,json_raw_obj,1)>=0)
      return use_method(jsbox_in, rootdir+onm+"/"+oinm, oi, memapsz=memapsz);
    if (is_obj(oi)) {
      if (oi(*)==0)
        return oi;
      if (allof(strgrepm("grp_[0-9][0-9][0-9][0-9]",oi(*,)))) {
        for (oi2=save(),i=1;i<=oi(*);i++)
          save, oi2, string(0), oi(noop(i));
        oi= oi2;
      }
      if (oi(*)==1 && is_obj(oi,json_raw_obj,1)>=0)
        save, oo, noop(oinm), use_method(jsbox_in, oi(json_raw_obj), memapsz=memapsz);
      else
        save, oo, noop(oinm), use_method(jsbox_read, oi, rootdir=rootdir,onm=onm+("/"+oinm), \
                                         memapsz=memapsz);
    } else
      save, oo, noop(oinm), oi;
  }
  return oo;
}

func jsbox_in (o, memapsz=)
// fnm = array(string)
// tnm = array(string)
// tsz = array(long)
// rk = array(long)
// shp = array(long,1)
// bige = array(string)
{
  // open file
  f= open(o(fnm),"rb");
  fsz= sizeof(f);

  // define type
  tmap= save(float=float, double=double, complex=complex, fcomplex=complex,\
             char=char, short=short, int=int,long=long);
  t= tmap(o(tnm));

  // special type/mods: complex and fcomplex 
  if (o(tnm)=="fcomplex") 
    use_method(jsbox_add_fcomplex,f);
  else if (o(tnm)=="complex")
    save, f, complex;

  // dims
  xdims= _(o(rk),o(shp));

  // mem map if requested ... always named "x"
  if (!is_void(memapsz) && fsz>memapsz) {
    add_variable, f, 0, "x", t, xdims;
    return f;
  }

  // allocate and read
  x= array(t(0),xdims)
  if (sizeof(x)!=fsz)
    error,"file size/metadata mismatch.";
  _read, f, 0, x;
  
  return x;
}

func jsbox_add_fcomplex (&f)
{
  pri= get_primitives(f);
  install_struct, f, "double", pri(13), pri(14), pri(15), pri(19:25);
  save, f, complex;
}

local oxjsn;
/* DOCUMENT s= ox2jsn(o, fmt=);        // S: array(string)
   ox2jsn: json (Javascript Object Notation) parsing to/from yorick save object (restricted:
   no anonymous members, except to store as *named* values arrays of sub-objects)
   JSON is a collection of name/values, or member/member-name, (also named:  object,
   record, struct, dictionary, hash table, keyed list, or associative array). Values may be
   ordered list of values (also named: array, vector, list, or sequence.)
   FMT: format integer or decimal, SEE totxt.
*/
oxjsn = save(ox2jsn, oxjsn_wrkr, oxjsn_scan, oxjsn_pr);
oxjsn = closure(oxjsn, ox2jsn);

local jsnox;
/* DOCUMENT o= jsn2ox(s); // o: oxy object
   ox2jsn: json (javascript object notation) to/from yorick save object (restricted:
   no anonymous members, except to store as *named* values arrays of sub-objects)
   JSON is a collection of name/values, or member/member-name, (also named:  object,
   record, struct, dictionary, hash table, keyed list, or associative array). Values may be
   ordered list of values (also named: array, vector, list, or sequence.)
   1. empty array value "[]" map to yorick void "[]"
   2. empty object value "{}" map to yorick empty object "save()"
   3. null  maps to "string(0)"
   4. true/false 0/1
   WARNING: multidimensional arrays are JSON arrays of arrays and area
            *NOT (YET?)* re-casted to yorick arrays(..) but stay as groups of rank-1 arrays...
*/
jsnox = save(jsn2ox, jsnox_scan, jsnox_match, jsnox_cut, jsnox_split, jsnox_parse, ojd, str2str);
jsnox = closure(jsnox, jsn2ox);

local oxjsb;
/* DOCUMENT p= oxjsb(o, szmx=, rootdir=, onm=, append=, update=, fcomplex=);
   traverse an oxy data-only tree and write out numerical values/arrays larger
   than SZMX bytes.
   -- usage --
   oo= oxjsb(save(a=save(b=random(200))));
   b= random(200);
   o= jsbox(oxjsb(b));                    // o==b
   SEE ALSO:
 */
oxjsb = save(oxjsb_write, oxjsb_out, jsbox_add_fcomplex, oxjsb_wrap);
oxjsb = closure(oxjsb, oxjsb_wrap);

local jsbox;
/* DOCUMENT 
   
   SEE ALSO:
 */
jsbox= save(jsbox_read, jsbox_in, jsbox_add_fcomplex);
jsbox= closure(jsbox, jsbox_read);
restore, scratch;


#if 0

dir= "/home/trm/dev/yorick/yorick-json/samples/";
l= lsdir(dir);
l= l(where(strgrepm(".json$",l)));

ojd= save();
for (i=1;i<=numberof(l);i++) {
  write,format="parsing file: %s\n",l(i);
  s= text_lines(dir+l(i));
  o= jsnox(s);
  ss= oxjsn(o);
  save,ojd,strpart(l(i),:-5),save(s,o,ss);
}


// // dict special VAL cases


#endif

#if 0
ssep= "- - - - - - - - - - - - - - - - - - - - - - - - - - -";
write,oxjsn(noop(pi)),format="%s vs. 3.141592653590e+00 \n";
write,oxjsn(pi),format="%s vs. {\"pi\": 3.141592653590e+00}\n";
write,ssep,format="%s\n";
hi= "hi";
write,oxjsn(noop(hi)),format="%s vs. \"hi\"\n";
write,oxjsn(hi),format="%s vs. {\"hi\": \"hi\"}\n";
write,ssep,format="%s\n";
pp=[pi,pi];
write,oxjsn(noop(pp)),format="%s vs. [3.141592653590e+00,3.141592653590e+00] \n";
write,oxjsn(pp),format="%s vs. {\"pp\": [3.141592653590e+00,3.141592653590e+00]}\n";
write,ssep,format="%s\n";

pp=[hi,hi];
write,oxjsn(noop(pp)),format="%s[\"hi\",\"hi\"] \n";
write,oxjsn(pp),format="%s{\"pp\": [\"hi\",\"hi\"]}\n";
write,ssep,format="%s\n";

pp= save(s="hi");
write,oxjsn(noop(pp)),format="%s{\"s\": \"hi\"} \n";
write,oxjsn(pp),format="%s{\"pp\": {\"s\": \"hi\"}}\n";
write,ssep,format="%s\n";

pp= save(string(0),save(s="hi"));
write,oxjsn(noop(pp)),format="%s[{\"s\": \"hi\"}] \n";
write,oxjsn(pp),format="%s{\"pp\": [{\"s\": \"hi\"}]}\n";
write,ssep,format="%s\n";

scratch= save(scratch,d,q,days,months);
cfg= save(); {
  months= save(); {
    save, months, [], "Jan";
    save, months, [], "Feb";
    save, months, [], "Dec";
  } save, cfg, months;

  days= save(); {
    save, days, [], "Mon";
    save, days, [], "Tue";
  } save, cfg, days;

  d= save(); {

    months= save(); {
      save, months, [], "Jan";
      save, months, [], "Dec";
    } save, d, months;

    days= save(); {
      save, days, [], "Mon";
      save, days, [], "Sun";
    } save, d, days;
  } save, cfg, d=save(string(0),d,string(0),d);
  q= save(); {
    q, x0= -25.0;
    q, x1= 25.0;
    q, nx= 100;
  } cfg, grid=save(string(0),q);
}
restore, scratch;
write,oxjsn(cfg),format="%s\n";
write,ssep,format="%s\n";

scratch= save(scratch,tmp,tmp2,tmp3);
cfg= save();
cfg, type= "Feature";
cfg, id= "NC.J051";
tmp= save();
tmp, coordinates= [-122.007835,37.312901];
tmp, type= "Point";
cfg, geometry= tmp;
cfg, stuff= "inbetween";
tmp= save();
tmp, network= "NC";
tmp, intensity_flag= "";
tmp2= save();
tmp3= save();
tmp3, name= "sa(3.0)";
tmp3, sigma= 0.89;
tmp3, value= 3.75;
save, tmp2, string(0), tmp3;
tmp3= save();
tmp3, name= "sa(1.0)";
tmp3, sigma= 0.75;
tmp3, value= 3.62;
save, tmp2, string(0), tmp3;
// ...more anonymous tmp3 group member into tmp2
tmp, mmi_from_pgm= tmp2;
tmp, distance= 104.211;
cfg, properties= tmp;
restore, scratch;
write,oxjsn(cfg),format="%s\n";
write,ssep,format="%s\n";

#endif

#if 0
scratch= save(scratch,tmp,tmp2,tmp3);
cfg= save();
cfg, type= "Feature";
cfg, id= "NC.J051";
tmp= save();
tmp, coordinates= [-122.007835,37.312901];
tmp, type= "Point";
cfg, geometry= tmp;
cfg, stuff= "inbetween";
tmp= save();
tmp, network= "NC";
tmp, intensity_flag= "";
tmp2= save();
tmp3= save();
tmp3, name= "sa(3.0)";
tmp3, sigma= 0.89;
tmp3, value= 3.75;
save, tmp2, string(0), tmp3;
tmp3= save();
tmp3, name= "sa(1.0)";
tmp3, sigma= 0.75;
tmp3, value= 3.62;
save, tmp2, string(0), tmp3;
// ...more anonymous tmp3 group member into tmp2
tmp, mmi_from_pgm= tmp2;
tmp, distance= 104.211;
cfg, properties= tmp;
cfg, morestuff= "after";
restore, scratch;
s= oxjsn(cfg);
#endif
