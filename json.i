require, "yut.i";

scratch= save(scratch, jsnox, jsnox_wrap, jsnox_scan, jsnox_match, jsnox_cut, jsnox_split, jsnox_parse, \
               oxjsn, oxjsn_wrkr, oxjsn_scan, oxjsn_pr);
func oxjsn(args)
    /* DOCUMENT s= ox2jsn(o, fmt=); // s: array(string)
                o= ox2jsn(s); // o: oxy object
       ox2jsn: json (javascript object notation) to/from yorick save object (restricted:
       no anonymous members, except to store as *named* values arrays of sub-objects)
       JSON is a collection of name/values, or member/member-name, (also named:  object,
       record, struct, dictionary, hash table, keyed list, or associative array). Values may be
       ordered list of values (also named: array, vector, list, or sequence.)
       FMT: format integer or decimal, SEE totxt.
    */
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"string or ox, optional fmt (see totxt).";
  local fmt;
  if (args(0)>1)
    fmt= args(2);
  tablen= 2;
  quotestring= 1;
  if (args(0,1)==0)
    onm= args(-,1);
  else
    onm= [];
  ob= args(1);
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
wrap_args,oxjsn;

func jsnox(args)
    /* DOCUMENT s= ox2jsn(o, fmt=); // s: array(string)
                o= jsn2ox(s); // o: oxy object
       ox2jsn: json (javascript object notation) to/from yorick save object (restricted:
       no anonymous members, except to store as *named* values arrays of sub-objects)
       JSON is a collection of name/values, or member/member-name, (also named:  object,
       record, struct, dictionary, hash table, keyed list, or associative array). Values may be
       ordered list of values (also named: array, vector, list, or sequence.)
       FMT: format integer or decimal, SEE totxt.
    */
{
  if (args(0)==0 || args(0)>1 || numberof(args(-))!=0)
    error,"string (or string array) single arg.";
  if (!is_string(args(1)))    // JSON -> OX
    error,"string (or string array) single arg.";
  s= use_method(jsnox_wrap,args(1));
  return use_method(jsnox_parse,s);
}
wrap_args,jsnox;

func oxjsn_wrkr (&s, ob, onm, &nt)       // JSON -> OX
{
  extern fmt,tablen,quotestring;
  slast= strpart(s,0:0);
  //
  t= (nt? strchar(array('\x20',nt*tablen)): "");
  tq= strchar(array('\x20',tablen));
  if (is_oxgrar(ob)) {
    oo= arr_oxgr(ob,1);
    if (!is_void(oo))
      ob= oo;
  }
  // only for user input not an object
  if (!is_obj(ob)) {
    s+= (onm? t+"\""+onm+"\": ": "")+use_method(oxjsn_pr,ob,fmt);
    return;
  }
  s+= (onm? t+"\""+onm+"\":\n": "");
  if (is_group(ob))
    s+= t+"[\n";
  else
    s+= t+"{\n";
  nt+= 1;
  on= ob(*);
  for (i=1;i<=on;i++) {
    oi= ob(noop(i));
    oni= ob(*,i);
    use_method,oxjsn_wrkr,s,oi,oni,nt;
    s+= (i==on? "": ",\n");
  }
  nt-= 1;
  s+= "\n";
  if (is_group(ob))
    s+= t+"\]";
  else
    s+= t+"\}";
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
  sa= is_string(a)? q+a+q: totxt(a,fmt);

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

func jsnox_wrap (s)
{
  if (dimsof(s)(1)>0) {
    g= strgrep("^[^\\]*\\\\", s);                // standard: *NO* comments in json
    w= where(g(dif,)>0);
    if (numberof(w)) 
      s(w)= strpart(s(w),g(,w)-[0,2]);
    // g= strgrep("^[^//]*//", s);                // "//" separated comments couuld be path!!
    // w= where(g(dif,)>0);
    // if (numberof(w)) 
    //   s(w)= strpart(s(w),g(,w)-[0,2]);
    s= s(sum);
  }
  // remove new lines and tabs
  s= streplace(s,strgrep("[\t\n\r]",s,n=max(1,strlen(s))),"");
  // replace string space with stars
  s= streplace(s,strgrep("(\"[a-zA-Z0-9_]*( )+[a-zA-Z0-9_]+\")",s,sub=2,n=max(1,strlen(s)/10)),"*");
  // remove spaces
  s= streplace(s,strgrep(" ",s,n=max(1,strlen(s))),"");
  // replace in-string spaces
  s= streplace(s,strgrep("\\*",s,n=max(1,strlen(s)))," ");

  // put back (single) string spaces
  // length
  sl= strlen(s);
  // brackets
  m= strchar(s)(:-1)==strchar("{")(1);
  m-= strchar(s)(:-1)==strchar("}")(1);
  mbc= m(cum);
  m= strchar(s)(:-1)==strchar("[")(1);
  m-= strchar(s)(:-1)==strchar("]")(1);
  mbs= m(cum);
  // commas
  mcm= strchar(s)(:-1)==strchar(",")(1);
  // colons
  mcl= strchar(s)(:-1)==strchar(":")(1);
  // out
  return save(s,sl,mbc,mbs,mcm,mcl);  // order matters!
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
// ISAR== 0: object                          O: object with named members
//        1: arrays of strings or numbers     : arrays of string or numbers
//        2/3: arrays of obects/arrays        : object of UNnames members
//        4:                                  : string or number
//        
// 2 cases: (1) k:v,k:v,... (2) v,v,...
// where value V could be string/number/(special val not impl.)/object-in-brackets
{
  if (!is_obj(os))
    error,"expecting object.";
  // find out if (1) k:v,k:v, ... or (2) v,v,v -- the array case, within square brackets
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
    } else if (isar>1) {
      if (strpart(os(s),k:k)==",")
        k= k+1;
      i= k+1;
      if (strpart(os(s),k:k)!=(isar==2? "{": "[")) // <<<<<<<<<<< BUG!!
        error;
      j= use_method(jsnox_scan,os,i,(isar==2? "mbc": "mbs"))-1;
    } else 
      error,"unrecognized sequence.";
    k= j+2;
    if (isar==0)
      save, o, strpart(os(s),i:l-1), use_method(jsnox_cut,os,l+1,j);
    else if (isar==1)
      save, o, string(0), use_method(jsnox_cut,os,i,j)(s);
    else
      save, o, string(0), use_method(jsnox_cut,os,i,j);

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
{
  if (!is_obj(os))
    error,"expecting object.";
  local isar;

  // deal with peeling outer {}
  i= min(use_method(jsnox_scan,os,1,"mbc"),use_method(jsnox_scan,os,1,"mbs"));
  ss= strpart(os(s),1:i);
  issq= 0;
  if (anyof(strtrim(ss)==["{","["])) {
    issq= strpart(os(s),i:i)=="[";
    j= use_method(jsnox_match,os,i);
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
    o= (oi=="null")? []: oi;
  } else if (isar==2 || isar==3) {   // 2/3: arrays of obects/arrays OR 4:string or number
    o= save();
    for (i=1;i<=oi(*);i++)
      save,o,string(0),use_method(jsnox_parse,oi(noop(i)));
  }
  return o;
}

ox2jsn = save(oxjsn, oxjsn_wrkr, oxjsn_scan, oxjsn_pr);
ox2jsn = closure(ox2jsn, oxjsn);
jsn2ox = save(jsnox, jsnox_wrap, jsnox_scan, jsnox_match, jsnox_cut, jsnox_split, jsnox_parse);
jsn2ox = closure(jsn2ox, jsnox);
restore, scratch;

#if 0
ssep= "- - - - - - - - - - - - - - - - - - - - - - - - - - -";
write,ox2jsn(noop(pi)),format="%s3.141592653590e+00 \n";
write,ox2jsn(pi),format="%s{\"pi\": 3.141592653590e+00}\n";
write,ssep,format="%s\n";
hi= "hi";
write,ox2jsn(noop(hi)),format="%s\"hi\"\n";
write,ox2jsn(hi),format="%s{\"hi\": \"hi\"}\n";
write,ssep,format="%s\n";
pp=[pi,pi];
write,ox2jsn(noop(pp)),format="%s[3.141592653590e+00,3.141592653590e+00] \n";
write,ox2jsn(pp),format="%s{\"pp\": [3.141592653590e+00,3.141592653590e+00]}\n";
write,ssep,format="%s\n";

pp=[hi,hi];
write,ox2jsn(noop(pp)),format="%s[\"hi\",\"hi\"] \n";
write,ox2jsn(pp),format="%s{\"pp\": [\"hi\",\"hi\"]}\n";
write,ssep,format="%s\n";

pp= save(s="hi");
write,ox2jsn(noop(pp)),format="%s{\"s\": \"hi\"} \n";
write,ox2jsn(pp),format="%s{\"pp\": {\"s\": \"hi\"}}\n";
write,ssep,format="%s\n";

pp= save(string(0),save(s="hi"));
write,ox2jsn(noop(pp)),format="%s[{\"s\": \"hi\"}] \n";
write,ox2jsn(pp),format="%s{\"pp\": [{\"s\": \"hi\"}]}\n";
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
write,ox2jsn(cfg),format="%s\n";
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
write,ox2jsn(cfg),format="%s\n";
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
s= ox2jsn(cfg);
#endif
