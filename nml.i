require, "yut.i";

scratch= save(scratch, nml2ox, ox2nml, read_wrkr, read_wrkr_rpt, write_wrkr, \
              nmlval, unquoted);

func nml2ox (ll)
{
  // check arg single string FNM taken as filename, or its lines [text_lines(FNM)]
  if (!is_string(ll))
    error,"expecting 1 string(s) argument.";
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
  // char maps for complex values
  // namelists loop
  for (i=1,o=save();i<=n;i++)
    if (we(i)-1>=ws(i)+1)
      save,o,nml(i),use_method(read_wrkr,ll(ws(i)+1:we(i)-1));
    else
      save,o,nml(i),save();  // namelist is empty
  return o;
}

func ox2nml (o)
{
  if (!is_obj(o))
    error,"expecting an oxy object.";
  on= o(*);
  ns0= 100;
  s0= array(string,ns0);
  s= s0;
  is= 1;
  ns= ns0;
  for (i=1;i<=on;i++) {
    oi= toxstr(o(noop(i)));
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
}

func read_wrkr (ll)
{
  extern v_zmap;
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
    // key - could be type/composite
    k= strcase(0,kv(1));
    v= use_method(nmlval,kv(2));
    local nm;
    save,strtox(o,k,nm),noop(nm),v;
  }
  return o;
}

func nmlval (v)
{
  // trim
  v= strtrim(v,3);

  // delim detection
  c= strchar(v);
  mqt= use_method(unquoted,c);
  ss= strchar(strchar(v)*mqt);
  s= ss(where(ss));
  delim= (s && allof(strgrepm(",",s))? ",": " ");   // ? all or none ?  ... or some
  if (!delim)
    error,"could not detect delimitator.";

  /* make mask that is 1 for delim, -1 for eos */
  mask= (c==strchar(delim)(1))-(c=='\0');
  mask*= mqt;  // 0 where text, in beteen o

  if (anyof(mask))
    c(where(mask))= '\0';
  s= strchar(c);
  list = where((strpart(s,1:1)=="\"") & (strpart(s,0:0)=="\""));
  if (numberof(list))
    s(list) = strpart(s(list),2:-1);
  list = where((strpart(s,1:1)=="\'") & (strpart(s,0:0)=="\'"));
  if (numberof(list))
    s(list) = strpart(s(list),2:-1);
  s= strtrim(s(where(s)),3);
  s= s(where(s!=delim));

  v= s;
  if (numberof(v)>1) {
    v= use_method(read_wrkr_rpt,v);
    vnum= tonum(v);
    isn= vnum(1)>-1e99;
    isi= strgrepm("[.edED]",v(1))==0;
    v= (isn? (isi? long(vnum): vnum): v);
  } else {
    v= v(1);
    v= strtrim(v,3);
    v= use_method(read_wrkr_rpt,v);
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
  return v;
}

func read_wrkr_rpt (v)
{
  // dealing with repeats as "2*100"
  sg= strgrep("[0-9]+\\*",v);
  m= sg(2,..)>=0;
  if (anyof(m)) {
    w= where(m);
    rpt= long(tonum(strpart(strpart(v(w),sg(,w)),:-1)));
    o= (w(1)>1? save([],v(1:w(1)-1)): save());
    nw= numberof(w);
    for (j=1;j<=nw;j++) {
      save,o,[],array(strpart(v(w(j)),sg(2,w(j))+1:),rpt(j));
      if (j<nw && w(j+1)!=w(j)+1)
        save,o,[],v(w(j)+1:w(j+1)-1);
    }
    if (w(0)<numberof(v))
      save,o,[],v(w(0)+1:);
    vv= array(string(0),numberof(v)-numberof(rpt)+sum(rpt));
    for (j=1,i=1;j<=o(*);j++) {
      n= numberof(o(noop(j)));
      vv(i:i+n-1)= o(noop(j));
      i= i+n;
    }
    v= vv;
    list = where((strpart(v,1:1)=="\"") & (strpart(v,0:0)=="\""));
    if (numberof(list))
      v(list) = strpart(v(list),2:-1);
    list = where((strpart(v,1:1)=="\'") & (strpart(v,0:0)=="\'"));
    if (numberof(list))
      v(list) = strpart(v(list),2:-1);
  }
  return v;
}

func write_wrkr (o)
{

}

func unquoted(c)
{
  quotes= (c == '"' | c == ''' );
  list= where(quotes);
  if (numberof(list) < 2)
    return char(!quotes);
  /*  note that if numberof(list) is odd, we know the quoting is incorrect */
  /* mark open quotes as 1, character following close quotes as -1 */
  list= list(2::2);  // where close quote
  quotes(list) = 0;  /* close quote itself is part of the quote */
  if (list(0) == numberof(c)-1) {
    if (numberof(list) < 2)
      return char(!quotes(psum));
    list = list(1:-1);
  }
  --quotes(list+1);  /* if was open quote, don't open, else close */
  return char(!quotes(psum));
}

local nmlox;
/* DOCUMENT nmlox (ll)
   NMLOX: read a fortran namelist file, return an OXY representation
   LL: file name, or array of strings from TEXT_LINES(file_name) for ex.

   SEE ALSO: oxnml
 */
nmlox= save(nml2ox, read_wrkr, read_wrkr_rpt, nmlval, unquoted);
nmlox= closure(nmlox, nml2ox);

local oxnml;

/* DOCUMENT

   SEE ALSO:
 */
oxnml=  save(ox2nml, write_wrkr);
oxnml= closure(oxnml, ox2nml);

restore, scratch;
