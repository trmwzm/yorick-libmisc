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
      if (is>ns)
        {s= _(s,s0); ns+= ns0;}
      s(is++)= "/";
    }
    return s(:is-1);
  } else {
    ll= args(1);
    if (!is_string(ll))
      error,"expecting oxy or string(s).";
    if (numberof(ll)==1 && check_file(ll(1),quiet=1))
      ll= text_lines(ll(1));
    ll= strtrim(ll,3);
    ll= ll(where(ll!=string(0)));
    ws= where(strpart(ll,1:1)=="&");
    we= where(strlen(ll)==1 & strpart(ll,1:1)=="/");
    if (numberof(ws)!=numberof(we))
      error,"NML syntax error: unmatched & and /.";
    n= numberof(ws);
    k_chmap= strtrtable(37,32); // % to space
    nml= strcase(0,strpart(ll(ws),2:));
    v_chmap= strtrtable(39,32); // single to double quotes
    for (i=1,o=save();i<=n;i++)
      if (we(i)-1>ws(i)+1)
        save,o,nml(i),oxnml_wrkr(ll(ws(i)+1:we(i)-1));
      else
        save,o,nml(i),save();  // empty
    return o;
  }
}
wrap_args,oxnml;

func oxnml_wrkr (ll)
{
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
  for (i=1,o=save();i<=n;i++) {
    if (strpart(l(i),0:0)==",")
      l(i)= strpart(l(i),:-1);
    kv= strtrim(strtok(l(i),"="),3);
    if (kv(2)==string(0))
      error,"EQUAL (=) not found.";
    k= strtranslate(kv(1),k_chmap);
    v= strtranslate(kv(2),v_chmap);
    if (strgrepm(",",v)) {
      m= (strchar(v)(:-1)==char(44))(sum); // numbreof commas
      v= strpart(v,strword(v,",",m+1));
      v= v(where(v!=string(0)));
      v= strtrim(v,3);
      vnum= tonum(v);  // *** TODO *** : deal with - "3423, 2323, 2*0"
      isn= vnum(1)>-1e99;
      isi= strgrepm("[.edED]",v(1))==0;
      v= (isn? (isi? long(vnum): vnum): v);
    } else {
      v= strtrim(v,3);
      vnum= tonum(v);
      isi= strgrepm("[.edED]",v)==0;
      v= (vnum>-1e99? (isi? long(vnum): vnum): v);
    }
    save,o,noop(k),v;
  }
  return o;
}

// &BINIO
//  T%FNM="out_feb23/antsch.dat                        ",
//  T%TNM="real(dp)            ",
//  T%KND=8          ,
//  T%TSZ=64         ,
//  T%RK=2          ,
//  T%SHP=3          ,113956     , 2*0          ,
//  T%SZ=341868     ,
//  /
