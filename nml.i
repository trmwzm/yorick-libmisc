require, "yut.i";

func oxnml(args)
/* DOCUMENT s= oxnml(o[,fmt]); // s: array(string)
   o= oxnml(s); .. *not yet* ..
   FMT: format integer or decimal, SEE totxt.
*/
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"oxnml, string or ox,[optional fmt (see totxt)].";

  // optional format for TOTXT
  local fmt;
  if (args(0)>1)
    fmt= args(2);

  // string array or object input
  if (is_obj(args(1))) {
    // get ox vairable name
    if (args(0,1)==0)
      onm= strcase(0,args(-,1));
    else
      onm= "cfg";

    o= args(1);
    on= o(*);
    ns0= 100;
    s0= array(string,ns0);
    s= s0;
    is= 1;
    ns= ns0;
    for (i=1;i<=on;i++) {
      oi= o(noop(i));
      oinm= strcase(0,o(*,i));
      if (!oinm)
        error,"keyed members only.";
      if (is>ns)
        {s= _(s,s0); ns+= ns0;}
      s(is++)= "&"+oinm;
      for (j=1;j<=(oi(*));j++) {
        v= oi(noop(j));
        k= strcase(0,oi(*,j));
        sa= totxt(v,fmt);
        mn= v==string(0);
        if (anyof(mn)) {
          w= where(mn);
          sa(w)= "\"\"";
        }
        if (is>ns)
          {s= _(s,s0); ns+= ns0;}
         s(is++)= "  "+k+"= "+strpart((sa+", ")(*)(sum),1:-2);
      }
      if (is>=ns)
        {s= _(s,s0); ns+= ns0;}
      s(is++)= "/";
      s(is++)= "";
    }
    return s(:is-1);
  } else {
    ll= args(1);
    // check arg single string FNM taken as filename, or its lines [text_lines(FNM)]
    if (!is_string(ll))
      error,"expecting oxy or string(s).";
    // handle the FNM case
    if (numberof(ll)==1 && check_file(ll(1),quiet=1))
      ll= text_lines(ll(1));
    // remove lead-/trail-ing blanks
    ll= strtrim(ll,3);
    // dealing with comments (not well: does not accept !!! in quoted values)
    sg= strgrep("^[A-Za-z0-9_= /&\\\"]*!",ll);
    mec= sg(2,..)>=0;
    if (anyof(mec)) {
      w= where(mec);
      sg(2,w)-= 1;
      ll(w)= strpart(ll(w),sg(,w));
    }
    // eliminate empty lines
    ll= ll(where(ll!=string(0)));
    // where do namelists start (&) and end (/)
    ws= where(strpart(ll,1:1)=="&");
    we= where(strlen(ll)==1 & strpart(ll,1:1)=="/");
    if (numberof(ws)!=numberof(we))
      error,"NML syntax error: unmatched & and /.";
    n= numberof(ws);
    // namelist names, chop leading "&", trim, lowercase
    nml= strtrim(strcase(0,strpart(ll(ws),2:)),3);
    // char maps for string values
    v_chmap= strtrtable(34,32);             // double quote to space
    v_chmap= strtrtable(39,32,v_chmap);     // single quote to space
    // char maps for complex values
    v_zmap= strtrtable(40,32);              // left ( to space
    v_zmap= strtrtable(41,32,v_zmap);       // right ) to space
    // namelists loop
    for (i=1,o=save();i<=n;i++)
      if (we(i)-1>ws(i)+1)
        save,o,nml(i),oxnml_read_wrkr(ll(ws(i)+1:we(i)-1));
      else
        save,o,nml(i),save();  // namelist is empty
    return o;
  }
}
wrap_args,oxnml;

func oxnml_read_wrkr (ll)
{
  extern v_chmap, v_zmap;
  // number of lines in namelist
  n= numberof(ll);
  // comma continuation
  meq= strgrepm("=",ll);
  l= array(string,n);
  for (i=1,adl=0,j=1;i<=n;i++) {
    if (adl==0)
      l(j)= ll(i);
    else
      l(j)+= (strpart(l(j),0:0)==","? ll(i): ","+ll(i));
    adl= i==n || meq(i+1)==0;
    j+= adl==0;
  }
  n= j;
  l= l(:n);
  // build key-val object with namelist content
  for (i=1,o=save();i<=n;i++) {
    // remove trailing commas
    if (strpart(l(i),0:0)==",")
      l(i)= strpart(l(i),:-1);
    // extract key/val
    kv= strtrim(strtok(l(i),"="),3);
    if (kv(2)==string(0))
      error,"EQUAL (=) not found.";
    // derived type : key = A%B
    sg= strgrep("%",kv(1));
    if (sg(2)>=0) {  // *** TODO *** recursive
      ks= [strpart(kv(1),1:sg(2)-1), \
           strpart(kv(1),sg(2)+1:)];
      ks= strcase(0,ks);
      if (is_obj(o,ks(1),1)<0)
        save,o,ks(1),save();
      oi= o(ks(1));
      k= ks(2);
    } else {
      oi= o;
      k= strcase(0,kv(2));
    }
    // array words separation and strings ...
    // !!! FIXME space in strings will become arrays of words !!!!
    // ss="\"blih\"ffhjflfflfhfbluhkkjjfdgd"
    // strpart(ss,strgrep("(\"bl[a-z]h\")*.*(bl[a-z]h)+.*",ss,sub=2));
    // remove single and double quotes: too bad if numerical val in quoted string !
    v= strtrim(strtranslate(kv(2),v_chmap),3);
    // array and separator detection
    sep= (strgrepm(",",v)? ",": (strgrepm(" ",v)? " ": string(0)));
    if (sep) {
      m= (strchar(v)(:-1)==strchar(sep)(1))(sum); // numbreof commas
      v= strpart(v,strword(v,sep,m+1));
      v= v(where(v!=string(0)));
      v= strtrim(v,3);
      v= oxnml_read_wrkr_rpt(v);
      vnum= tonum(v);
      isn= vnum(1)>-1e99;
      isi= strgrepm("[.edED]",v(1))==0;
      v= (isn? (isi? long(vnum): vnum): v);
    } else {
      v= strtrim(v,3);
      v= oxnml_read_wrkr_rpt(v);
      vnum= tonum(v);
      isi= strgrepm("[.edED]",v)==0;
      v= (vnum>-1e99? (isi? long(vnum): vnum): v);
    }
    // complex
    if (is_string(v)) {
      m= strgrepm("^\\(|\\)",v);
      if (allof(m)) {
        if (numberof(v)%2==1)
          error,"expecting even Re and Im # of values.";
        v= tonum(strtrim(strtranslate(v,v_zmap),3));
        v= v(1::2)+1i*v(2::2);
      }
    }
    save,oi,noop(k),v;
  }
  return o;
}

func oxnml_read_wrkr_rpt (v)
{
  // dealing with repeats
  sg= strgrep("[0-9]+\\*",v);
  m= sg(2,..)>=0;
  if (anyof(m)) {
    w= where(m);
    rpt= long(tonum(strpart(strpart(v(w),sg(,w)),:-1)));
    o= (w(1)>1? save([],v(1:w(1)-1)): save());
    for (j=1;j<=numberof(w);j++)
      save,o,[],array(strpart(v(w(j)),sg(2,w(j))+1:),rpt(j));
    if (w(0)<numberof(v))
      save,o,[],v(w(0)+1:);
    vv= array(string(0),numberof(v)-numberof(rpt)+sum(rpt));
    for (j=1,i=1;j<=o(*);j++) {
      n= numberof(o(noop(j)));
      vv(i:i+n-1)= o(noop(j));
      i= i+n;
    }
    v= vv;
  }
  return v;
}

func oxnml_write_wrkr (o)
{

}

#if 0
o= save();
save,o,"fasdf",1;
save,o,"fsdfs%dfsdfs%fsf",2;
save,o,"fsdfs%dfsdfs%dff",3;
save,o,"fafgr",4;
save,o,"fsdfs%dfsdfs",5;
save,o,"fsdfs%dfsdfs%fsf%eyg",6;

func oxt (o, ni, &no)
{
  no= string(0);
  sg= strtok(ni,"%");
  if (sg(2)) {
    if (is_obj(o,sg(1),1)<0)
      save,o,sg(1),save();
    oo= oxt(o(sg(1)),sg(2),no);
  } else {
    no= ni;
    oo= o;
  }
  return o;
}

oo= save();
for (i=1; i<=o(*); i++) {
  ot= oxt(oo,o(*,i),nm);
  i;
  nm;
  save,ot,noop(nm),o(noop(i));
  ot(*,);ot(0);
  write,"\n";
}

o= save();
save,o,"fasdf",1;
save,o,"fsdfs%dfsdfs%fsf",2;
save,o,"fsdfs%dfsdfs%dff",3;
save,o,"fafgr",4;
save,o,"fsdfs%dfsdfs",5;
save,o,"fsdfs%dfsdfs%fsf%eyg",6;

func strtox (o, ni, sep=)
/* DOCUMENT strtox (o, ni, sep=)
   use strings sequence joined by separator  as keys for OXY save
   // usage:
   o= save();
   save,o,"fasdf",1;
   save,o,"fsdfs%dfsdfs%fsf",2;
   //
   oo= save();
   for (i=1; i<=o(*); i++)
     save,oxt(oo,o(*,i),nm),noop(nm),o(noop(i));
   SEE ALSO:
 */
{
  sep= (is_void(sep)? "%": sep);
  no= string(0);
  sg= strtok(ni,sep);
  if (sg(2)) {
    if (is_obj(o,sg(1),1)<0)
      save,o,sg(1),save();
    oo= oxt(o(sg(1)),sg(2),no);
  } else {
    no= ni;
    oo= o;
  }
  return o;
}
func toxstr (o)
/* DOCUMENT
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


oo= save();
for (i=1; i<=o(*); i++)
  save,strtox(oo,o(*,i),nm),noop(nm),o(noop(i));

ooo= save();
for (i=1; i<=o(*); i++) {
  val= toxstr (o)
  save,strtox(oo,o(*,i),nm),noop(nm),o(noop(i));


#endif
