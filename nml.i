require, "yut.i";
require, "rwflat.i";

scratch= save(scratch, nml2ox, ox2nml, read_wrkr, read_wrkr_rpt, write_wrkr, \
              nmlval, unquoted, oxbnml_read);

func nml2ox (ll, readbin=)
{
  // check arg single string FNM taken as filename, or its lines [text_lines(FNM)]
  if (!is_string(ll))
    error,"expecting 1 string(s) argument.";
  // handle the FNM case
  if (numberof(ll)==1 && check_file(ll(1),quiet=1)) {
    nmlfnm= ll(1);
    ll= text_lines(nmlfnm);
  } else {
    if (readbin==1)
      error,"oxnml readbin=1, first arg must be file/path name.";
    nmlfnm= string(0);
  }
  // remove lead-/trail-ing blanks
  ll= strtrim(ll,3);
  // dealing with comments (not well: does not accept !!! in quoted values)
  sg= strgrep("^[-A-Za-z0-9_=*.%, /&\\\"]*!",use_method(unquoted,ll));
  mec= sg(2,..)>=0;
  if (anyof(mec)) {
    w= where(mec);
    sg(2,w)-= 1;
    ll(w)= strpart(ll(w),sg(,w));
  }
  // eliminate empty lines
  // *** FIXME *** might want to be able to deal with LL="&dat;x=2;\"
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
  parent_tr= strtrtable(40,32);                  // parenthesis
  parent_tr= strtrtable(41,32,parent_tr);
  // namelists loop
  for (i=1,o=save();i<=n;i++)
    if (we(i)-1>=ws(i)+1)
      save,o,nml(i),use_method(read_wrkr,ll(ws(i)+1:we(i)-1));
    else
      save,o,nml(i),save();  // namelist is empty
  if (nmlfnm)
    save,o,"nml_filename",nmlfnm;
  if (readbin==1)
    oxbnml_read,o,dirname(nmlfnm);
  return o;
}

func ox2nml (o, fmt=)
{
  if (!is_obj(o))
    error,"expecting an oxy object.";
  // TOTEXT format
  if (is_void(fmt))
    fmt=-0.12;
  if (is_integer(a))
    fmt= abs(fmt); //  no hex

  brack_tr= strtrtable(91,32);
  brack_tr= strtrtable(93,32,brack_tr);         // square brackets

  on= o(*);
  ns0= 100;
  s0= array(string,ns0);
  s= s0;
  is= 1;
  ns= ns0;
  for (i=1;i<=on;i++) {
    if (is_obj(o(noop(i)))) {
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
        sa= strtrim(strtranslate(sa,brack_tr),3);
        if (is>ns)
          {s= _(s,s0); ns+= ns0;}
        s(is++)= "  "+k+"= "+strpart((sa+", ")(*)(sum),1:-2);
      }
      if (is>=ns)
        {s= _(s,s0); ns+= ns0;}
      s(is++)= "/";
      s(is++)= "";
    }
  }
  return s(:is-1);
}

func read_wrkr (ll)
{
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
    if (kv(2)==string(0) && strpart(l(i),0:0)!="=")
      error,"EQUAL (=) not found.";
    // key - could be type/composite
    k= strcase(0,kv(1));
    // eval values
    v= use_method(nmlval,kv(2));          // where it happens

    local nm;
    save,strtox(o,k,nm),noop(nm),v;
  }
  return o;
}

func nmlval (v)
{
  extern parent_tr;
  // trim
  v= strtrim(v,3);

  if (v==string(0))
    return v;
  // delim detection
  c= strchar(v);
  mqt= use_method(unquoted,c);
  ss= strchar(strchar(v)*mqt);
  w= where(ss);
  if (numberof(w)==0)
    s= ss;
  else
    s= ss(where(ss));
  delim= (s(1) && allof(strgrepm(",",s))? ",": " ");   // ? all or none ?  ... or some
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
  if (is_scalar(s) && s==string(0))
    return s;
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
    m= strgrepm("^\\( *[0-9.edED+-]+|[0-9.edED+-]+ *\\)$",v);
    if (allof(m)) {
      if (numberof(v)%2==0) {
        v= tonum(strtrim(strtranslate(v,parent_tr),3));
        v= v(1::2)+1i*v(2::2);
      }
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
  isstr= is_string(c);
  if (isstr)
    c= strchar(c);
  mq= (c == '"' | c == ''' );
  w= where(mq);
  if (numberof(w) < 2)
    if (isstr)
      return strchar(c);
    else
      return char(!mq);
  // note that if numberof(w) is odd, we know the quoting is incorrect
  // mark open mq as 1, character following close mq as -1
  w= w(2::2);  // where close quote
  mq(w) = 0;   // close quote itself is part of the quote
  --mq(w+1);  // if was open quote, don't open, else close
  m= char(!mq(psum));
  if (isstr) {
    cc= c*m;
    cc(where(m=='\0')) = char(32);
    return strchar(cc);
  } else {
    return m
  }
}
func oxbnml_read (o, dnm)
{
  for (i=1;i<=o(*);i++) {
    oi= o(noop(i));
    oinm= o(*,i);
    if (is_obj(oi)) {
      save,o,noop(oinm),oxbnml_read(oi,dnm);
    } else {
      s= strpart(oinm,-4:0);
      if (s=="_bnml") {
        if (is_string(oi))
          save,o,strpart(oinm,1:-5),readbnml(pathjoin([dnm,oi]),);
      }
    }
  }
  return o;
}

local oxnml;
/* DOCUMENT o= oxnml (ll)
   OXNML: read a fortran namelist file, return an OXY representation
   LL: file name, or array of strings from TEXT_LINES(file_name) for ex.

   SEE ALSO: nmlox
 */
oxnml= save(nml2ox, read_wrkr, read_wrkr_rpt, nmlval, unquoted, oxbnml_read);
oxnml= closure(oxnml, nml2ox);

local nmlox;
/* DOCUMENT l= nmlox (o, fmt=)
   NMLOX: print an oxy object fortran as a fortran namelist file.
   FMT: optional format to use in TOTXT call (> help,totxt)

   SEE ALSO: oxnml
 */
nmlox=  save(ox2nml, write_wrkr);
nmlox= closure(nmlox, ox2nml);

restore, scratch;


func readbnml (ll, &onml)
/* DOCUMENT x= readbnml(ll, &onml);
   read array data in binary format, as described in namelist formated metadata
   in filename LL (*.bnml).  LL can also be an array of lines from that file,
   but in this case directory information is lost.
   IF the binary filename in the metadata is an absolute file path, then it is
   read as-is, otherwise the metadata binary filename is taken to be relative
   to the directory of the metadata.
   ONML: optional oxy group from namelist metadata

   SEE ALSO: writebnml
 */
{
  // handle the FNM case
  if (numberof(ll)==1 && check_file(ll(1),quiet=1)) {
    fnm= ll(1);
    ll= text_lines(ll(1));
  } else
    fnm= "./";   // data assumed dir local
  // read binio nml
  onml= oxnml(ll);
  local tnm, rk, shp;
  ftmp= onml(binio,fnm);
  if (strpart(ftmp,1:1)=="/" && check_dir(dirname(ftmp)))
    bfnm= ftmp;
  else
    bfnm= dirname(fnm)+"/"+ftmp;
  tnm= onml(binio,tnm);
  tsz= onml(binio,tsz);
  typ= [];
  if (tnm=="character")
    if (tsz==8)
      typ= char;
  if (tnm=="integer")
    if (tsz==16)
      typ= short;
    else if (tsz==32)
      typ= int;
    else if (tsz==64)
      typ= long;
  if (tnm=="real")
    if (tsz==32)
      typ= float;
    else if (tsz==64)
      typ= double;
  if (tnm=="complex")
    if (tsz==32)
      typ= fcomplex;
    else if (tsz==64)
      typ= complex;
  if (is_void(typ))
    error,"unknown type";
  rk= onml(binio,rk);
  shp= onml(binio,shp);
  return readFlat(bfnm,typ,_(rk,shp(1:rk)));
}

func writebnml (a, fnm)
/* DOCUMENT writebnml (a, fnm)
   write array A in "raw/local" binary filename FNM[.DAT],
   -- .DAT added if not present --
   and write its associated metadata
   as a fortran-formatted namelist in file FNM+".bnml"
   SEE ALSO: readbnml
 */
{
  typ= structof(a);
  tsz= sizeof(typ)*8;
  da= dimsof(a);
  rk= da(1);
  if (rk>4)
    error,"max rank is 4.";
  shp= da(2:);

  for (i=1,sz=1;i<=rk;i++)
    sz*= shp(i);

  if (rk<4)
    shp= _(shp,array(0,4-rk));

  dnm= dirname(fnm);
  if (typ==char)
    tnm= "character";
  else if (typ==short)
    tnm= "integer";
  else if (typ==int)
    tnm= "integer";
  else if (typ==long)
    tnm= "integer";
  else if (typ=float)
    tnm= "real";
  else if (typ==double)
    tnm= "real";
  else if (typ==fcomplex)
    tnm= "complex";
  else if (typ==complex)
    tnm= "complex";
  else
    error,"unknown type struct.";

  dnm= dirname(fnm);
  bnm= basename(fnm);
  if (strpart(bnm,-3:)==".dat")
    bnm= strpart(bnm,:-4);
  if (strpart(bnm,-4:)==".bnml")
    bnm= strpart(bnm,:-5);

  f= pathjoin([dnm,bnm+".dat"]);
  writeFlat,a,f;
  o= save();
  o= save("binio",save());
  ot= o(binio);
  save,ot,fnm=basename(f);
  save,ot,tnm,tsz,rk,shp,sz;

  f= pathjoin([dnm,bnm+".bnml"]);
  ll= nmlox(o);
  return write(open(f,"w"),ll,format="%s\n");
}
