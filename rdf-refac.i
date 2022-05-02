require,"yut.i";

// Keyword   (Units*)   [dimensions*]   {element*}   Operator    Values   Comments*
// * == optional
// keys not case sensitive, spaces==tabs, but number of space is significant
// reserved characters not in keys: (,  ),   [,   ],   {,   },  <,  >,  =,  !,  and ;
// reserved characters not in values: ;
// reserved keywords: COMMENT, OPERATOR, PREFIX, SUFFIX, and INCLUDE
// multiple values in Units or Value fields are separated with: commas, tabs, or spaces.

scratch= save(scratch, tmp);
tmp= save(get,put,set,unitcof,fixunit,item,add,getline,getfile,mkk,mkv);
func rdf (base,f,vo)
/* DOCUMENT rdf();
            rdf(f); // with F as a filename string, or textstream
            rdf("key",save(val="val",unit="unit",comment="comment"));
            rdf(["k1,"k2],save(string(0),save(val,unit,comment),string(0),save(val,unit,comment));

   ... use ...
   fn= "jprp.pcf";
   o= rdf(fn);
   ll= o(get,"starting lat/lon",double,[1,2],unit="deg");
   o,set,"starting lat/lon",ll-1,unit="rad";
   o,put,create("~/qqq.rdf"),upper=1,orig=1;
   fn2= "/u/mah-r12a/trm/spl-pband/jprp-pcal/test2/uavsar.icf";
   oo= rdf(fn2);
   o,add,oo;

   o= rdf();
   o=rdf("single key",save(val="1",unit="m",comment="more"));
   o(get,"single key",long,[0]);
   o(get,"single key",long);
   o(get,"single key",string);

   o,add,rdf("another key",save(val="2",unit="m/s",comment="speed"));

*/
{
  ob= base(:);

  // defaults
  scom= [";","#","!"];    // comment delim
  sop= "=";               // key val operator
  sres= "][(){}<>=!;";  // reserved chars n key/unit, in addition sres=";" in value field
  // stash
  save, ob, scom, sop, sres;

  // setup units
  univ= ob(unitcof,);
  // setup val num format translation table
  pr1_table = strtrtable(44,32);
  strtrtable,91,32,pr1_table;
  strtrtable,93,32,pr1_table;
  // stash
  save, ob, univ, pr1_table;

  // read file: handle backlash continuations and includes
  if (structof(f)==string) {
    if (is_void(vo)) {
      fnm= f;
      f= open(f,"r");
    } else if (is_obj(vo)>0) {
      // deal with multiple key/val inputs
      if (is_scalar(f)) {
        f= [f];
        vo= save(string(0),vo);
      }
      n= numberof(f);
      ikv= indgen(n);
      txt= array(string,n);
      kv= save();
      for (i=1;i<=n;i++) {
        if (allof((vo(noop(i),*,)(-,)==["val","unit","comment"])(sum,))) {
          kk= ob(mkk,f(i));
          txt(i)= ob(item,kk,ob(mkv,vo(noop(i),val)),vo(noop(i),unit),  \
                     vo(noop(i),comment),tab=1,tablen=tablen);
          save, kv, noop(kk), vo(noop(i));
        } else
          error,"value object ought to have: val, unit, comment members.";
      }
      save, ob, txt, ikv, kv;
      return ob;
    } else
      error,"expecting scalar or group of save(val,unit,comment).";
  } else if (typeof(f)=="text_stream") {
    fnm= nameofstream(f);
  } else if (is_void(f)) {   // empty object
    save,ob,txt=[],ikv=[],kv=save();
    return ob;
  }

  txt= ob(getfile,f,dirname(fnm));
  // stash, ready to be processed, save "original," minus includes & continuations
  save, ob, txt;

  // process COMMENT =
  m= strgrepm("^ *comment *(\\( *[ &-]* *\\)|) *=",txt,case=1);
  if (anyof(m)) {   // RESET comment delim default
    s= txt(where(m)(1));   // take first, dump subsequent
    scom= [strpart(s,strgrep("^ *(comment|COMMENT) *(\\( *[ &-]* *\\)|) *= *(.*) *$",s,sub=3))];
    save, ob, scom;
  }
  ncom= numberof(scom);
  icom= array(0,[2,numberof(txt),ncom]);
  for (i=1;i<=ncom;i++)
    icom(..,i)= strfind(scom(i),txt)(1,..);

  // *TODO* deal with OPERATOR = ... which I have never seen in an RDF file
  // find operator (=)
  weq= strfind(sop, txt)(1,..);

  // look for valid key=val, excluding comment character declaration(s)
  m= !m;
  for (i=1;i<=ncom;i++)
    m&= weq<icom(..,i);

  // get all the fields
  txt= strtrim(strpart(txt,strword(txt,scom(sum),2)));
  cm= txt(2,..);                                            // comments
  txt= txt(1,..);                                           // keyval
  txt= strtrim(strpart(txt,strword(txt,sop,2)));
  val= txt(2,..);                                         // vals
  txt= txt(1,..);
  unit= strpart(txt,strgrep("\\([-a-zA-Z0-9,^\\/\\*]*\\)$",txt));  // "(units)", nil no conversion, - no unit.
  unit= strpart(unit,2:-1);                                        // "(units)"
  key= strtrim(strpart(txt,strgrep("[^"+sres+"]+",txt)));

  // look for valid key=val
  m&= key!=string(0);

  ikv= where(m);
  if (numberof(ikv)==0)
    error,"RDF file void.";
  else
    txt= txt(ikv);
  cm= cm(ikv); val= val(ikv); unit= unit(ikv); key= key(ikv);

  // check & format keys:
  key= ob(mkk,key);

  // handle PRE/SUF-FIX
  for (sf="",pf="",ls=0,lp=0,i=1;i<=numberof(key);i++) {
    if (key(i)=="suffix") {
      if (val(i)==string(0)) {
        sf= "";
        ls= 0;
      } else {
        sf= " "+ob(mkk,val(i));
        ls= 1;
      }

    } else if (key(i)=="prefix") {
      if (val(i)==string(0)) {
        pf= "";
        lp= 0;
      } else {
        pf= ob(mkk,val(i))+" ";
        lp= 1;
      }
    } else {
      if (lp)
        key(i)= pf+key(i);
      if (ls)
        key(i)= key(i)+sf;
    }
  }
  ikv= where(key!="suffix" & key!="prefix");
  if (numberof(ikv)==0)
    error,"RDF file void.";
  else
    txt= txt(ikv);
  save,ob,ikv;
  cm= cm(ikv); val= val(ikv); unit= unit(ikv); key= key(ikv);

  // format units
  strcase,0,unit;

  // make key-val database
  kv= save();
  nun= 10;
  for (i=1;i<=numberof(key);i++) {
    if (unit(i)!=string(0) && unit(i)!="" && unit(i)!="&" && unit(i)!="-") {
      us= strpart(unit(i),strword(unit(i)," ,",nun)); //" ,*/^"
      m= us!=string(0);
      if (anyof(m)) {
        us= us(where(m));
        if (anyof((m=is_obj(univ,us,1)<0)))
          error,"unknown unit: "+(us(where(m))+" ")(sum)+".";
      } else
        unit(i)= string(0);
    }
    save,kv,key(i),save(val=val(i),comment=cm(i),unit=unit(i));
  }

  save, ob, kv;  // what to do

  return ob;
}
func add (o)
{
  save, use, txt=_(use(txt),o(txt)), \
             ikv=_(use(ikv),o(ikv)+numberof(txt));
  save, use(kv), [], o(kv);
}
func getfile (f, dir)
{
  txt= rdfile(f);

  // TBP deal with the backlash continuation
  txt2= array(string,numberof(txt));
  i= j= 1;
  do {
    txt2(j++)= use_method(getline,txt,i);
  } while (i<=numberof(txt));
  txt= txt2(:j-1);
  txt2= [];

  m= strgrepm("^include *=",txt,case=1);
  if (is_void(dir))
    dir= "./";
  else
    dir= strpart(dir,0:0)=="/"? dir: dir+"/";
  if (anyof(m)) {
    w= where(m);
    if (numberof(w))
      inc= txt(where(m));
    for (ot=save(),i=1,j=1;i<=numberof(w);i++) {
      if (j<w(i))
        save, ot, string(0), txt(j:w(i)-1);
      j= w(i)+1;
      s= inc(i);
      fnm=strpart(s,strgrep("(^ *[iI][nN][cC][lL][uU][dD][eE] *=) *(.*$)",s,sub=2));
      fnm= strpart(fnm,1:1)=="/"? fnm: dir+fnm;
      save, ot, string(0),use_method(getfile,open(fnm,"r"),dir);
      if (i==numberof(w) && w(i)<numberof(txt))
        save, ot, string(0), txt(j:0);
    }
    txt= [];
    for (i=1;i<=ot(*);i++)
      txt= _(txt,ot(noop(i)));
  }

  return txt;
}
func getline (t,&i)
{
  // grab l=t(i); if end \ crop & add next(=i+1) to l & blank (i+1)
  n= numberof(t);
  if (i>n || i<0) {
    i= i+1;
    return string(0);
  } else {
    l= strtrim(t(i));
    l= strgrepm("^\\\\",l)? strpart(l,2:0): l;
    i= i+1;
    l= strgrepm("\\\\$",l)? strpart(l,1:-1)+use_method(getline,t,i): l;
    return l;
  }
}
func add (o)
{
  use, txt, ikv, kv;
  len= numberof(txt);
  n= kv(*);
  txt= _(txt,o(txt));
  ikv= _(ikv,o(ikv)+len);
  save,kv,[],o(kv);
}
func set (key, val, unit=, comment=)
{
  use, kv, univ;
  if (is_void(unit))
    unit= string(0);
  if (is_void(comment))
    comment= string(0);
  si= strfind("  ",key);
  while (anyof(si(2,)!=-1)) {   // replace double spaces
    key= streplace(key,si," ");
    si= strfind("  ",key);
  }
  key= strcase(0,strtrim(key));

  if (is_obj(kv,noop(key),1)<0)
    error,"key not found";

  v= kv(noop(key));
  save, v, val=use_method(mkv,val);

  nun= 10;
  us= strpart(unit,strword(unit," ,",nun)); //" ,*/^"
  m= us!=string(0);
  if (anyof(m)) {
    us= us(where(m));
    if (anyof((m=is_obj(univ,us,1)<0)))
      error,"unknown unit: "+(us(where(m))+" ")(sum)+".";
  }
  save, v, unit;
  save, v, comment;
  save, kv, noop(key), v;
}
func get (kk,typestrct,dims,unit=,verbose=,quiet=)
{
  use, kv, fixunit;

  kk= strcase(0,strtrim(kk));
  si= strfind("  ",kk);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    kk= streplace(kk,si," ");
    si= strfind("  ",kk);
  }

  if (is_obj(kv,noop(kk),1)<0)
    if (quiet)
      return [];
    else
      error,"key not found";
  v= kv(noop(kk));

  if (is_void(typestrct))
    return v(val);

  if (is_void(dims))
    dims= [0];
  dims0= dims;                     //local copy

  local x;
  if (structof(typestrct)==string) {
    eval,"x= "+typestrct+"(0)";
    typestrct=  structof(x);
  }

  if(typestrct==complex)
    if (dims(1)==0)
      dims0= [1,2];
    else
      dims0 =_(dims(1)+1,2,dims(2,..));

  // allocate out
  out= array(typestrct,dims0);
  nout= numberof(out);

  // requetsed type matches units?
  if (v(unit)!="" && v(unit)!=string(0) && v(unit)!="-")
    if (v(unit)=="&" && typestrct!=string)
      error,"mismatch requested type conflict with stated units.";

  if (v(val)!=string(0)) {
    stmp= v(val);
    if (typestrct==string) {
      out= reform(stmp,dims);
    } else {
      stmp= strtrim(strpart(stmp,strword(stmp," :,",nout+1)));
      stmp= stmp(where(stmp));
      nwords= numberof(stmp);
      if (nwords!=nout)
        error,"dimension mismatch, provide matching dims.";
      for (i=1;i<=nwords;i++)
        if (!sread(stmp(i),out(i)))
          error,"error reading variable.";
    }
    out= use_method(fixunit,out,uin=v(unit),uout=unit);
  }
  if (typestrct==complex)
    out= out(1,..)+1i*out(2,..);

  if (dims(1)==0) {
    if (verbose==1)
      write,key,": ",print(out(1));
    return out(1);
  } else {
    if (verbose==1)
      write,key,": ",print(out);
    return out;
  }
  return out;
}
func put (fo,upper=,tablen=,orig=)
{
  use, kv, item, ikv;

  if (is_void(kv)) {
    write,"rdf is void. No file written.",format="WANING: %s\n";
    return [];
  }

  for (ot=save(),i=1;i<=kv(*);i++) {
    key= kv(*,i);
    if (upper==1)
      key= firstupcase(key);
    save, ot, string(0), use_method(item,key,kv(noop(i),val),kv(noop(i),unit), \
                                    kv(noop(i),comment),tab=1,tablen=tablen);
  }

  if (orig==1) {
    t= use(txt);
    for (i=1;i<=numberof(ikv);i++)
      t(ikv(i))= ot(noop(i));
    ot= save();
    for (i=1;i<=numberof(t);i++)
      save, ot, string(0), t(i);
  }

  if (typeof(fo)!="text_stream")
    fo= create(fo);

  for (i=1;i<=ot(*);i++)
    write,fo,ot(noop(i)),linesize=200,format="%s\n";

  return fo;
}
func mkk (key) // make/check key[s]
{
  use, scom, sop, sres;
  m= !strgrepm("^[^"+sres+"]+$",key);
  if (anyof(m))
    error,"restricted char in key";
  key= strcase(0,strtrim(key));
  si= strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces **NOT STANDARD**
    key= streplace(key,si," ");
    si= strfind("  ",key);
  }
  return key;
}
func mkv (val)
{
  if (structof(val)==string) {
    return val;
  } else {
    sv= structof(val);
    if (sv==double||sv==float) {
      s= [swrite(val, format="%#.16g ")+" "](sum);
    } else if (sv==long||sv==int||sv==short||sv==char) {
      s= [swrite(val, format="%d")+" "](sum);
    } else if (sv==complex) {
      s=  [swrite(val.re,val.im,format="(%#.16g, %#.16g) ")+" "](sum);
    } else if (sv==string) {
      s= [swrite(val, format="%s")+" "](sum);
    }
    s= streplace(s,strfind("[",s,n=20),"");
    s= streplace(s,strfind("]",s,n=20),"");
    return s;
  }
}
func item (key,val,unit,comment,tab=,tablen=)
{
  use, pr1_table;

  if (is_void(tablen))
    tablen= 26;
  key= strtrim(key);
  si= strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    key= streplace(key,si," ");
    si= strfind("  ",key);
  }
  unit= strtrim(strcase(0,unit));
  if (unit!=string(0) && !is_void(unit))
    unit= " ("+unit+") ";
  else
    unit= string(0);
  if (tab) {
    kl= strlen(key);
    ul= strlen(unit);
    tl= tablen*((kl+ul)/tablen+1);
    sout= swrite(key,unit,\
      format="%"+pr1(kl)+"s%+"+pr1(tl-kl)+"s");
  } else {
    sout= key + unit;
  }
  sout+= "= ";
  sout+= strtranslate(use_method(mkv,val),pr1_table);
  sout+= "  ";
  if (!is_void(comment) && comment!=string(0) && comment!="")
    sout+= "  ! "+comment;

  return sout;
}
func fixunit (dat,uin=,uout=)
{
  // dat, scalar or array, if not string (else scalar only)
  // unit-in[out] is a scalar string with a single descriptor, or several, comma separated
  use, univ;
  if (is_string(dat))
    return dat;
  d= dimsof(dat);
  nd= numberof(dat);

  uin= is_void(uin)? string(0): strcase(0,strtrim(uin));
  uout= is_void(uout)? string(0): strcase(0,strtrim(uout));

  nun= 10;
  s= strpart(uin,strword(uin," ,",nun));
  m= s!=string(0);
  uin= anyof(m)? s(where(m)): string(0);
  nuin= numberof(uin);
  if (nuin>1 && nuin!=nd)
    error,"numbreof unit descriptors IN not same as numberof values.";
  uin+= array(string(0),nd);
  uin= reform(uin,d);

  s= strpart(uout,strword(uout," ,",nun));
  m= s!=string(0);
  uout= anyof(m)? s(where(m)): string(0);
  nuout= numberof(uout);
  if (nuout>1 && nuout!=nd)
    error,"numbreof unit descriptors OUT not same as numberof values.";
  uout+= array(string(0),nd);
  uout= reform(uout,d);

  for (i=1;i<=nd;i++) {
    if (uin(i)!=string(0) && uin(i)!="-" && uin(i)!="&") {
      tu= univ(uin(i));
      if (strgrepm("^db",uin(i)))
        dat(i)= 10^(dat(i)/10);
      if (numberof(tu)==2)
        dat(i)+=tu(2);
      dat(i)*= tu(1);
    }
  }
  for (i=1;i<=nd;i++) {
    if (uout(i)!=string(0) && uout(i)!="-" && uout(i)!="&") {
      tu= univ(uout(i));
      dat(i)/= tu(1);
      if (numberof(tu)==2)
        dat(i)-=tu(2);
      if (strgrepm("^db",uout(i)))
        dat(i)= 10*log10(dat(i));
    }
  }
  return dat;
}
func unitcof (dum)
{
  u= save();
  //length
  save,u,"nm",1.0e-9,"um",1.0e-6,"mm",1.0e-3,"cm",1.0e-2,"m", 1.0,"km",1.0e+3,"in",2.54e-2,"ft",3.048e-1,"mi",1.609344e3;

  // area
  save,u,"mm*mm",1.0e-6,"cm*cm",1.0e-4,"m*m",1.0,"km*km",1.0e+6,"in*in",6.4516e-4,"ft*ft",9.290304e-2,"mi*mi",2.58995511e6;

  // time
  save,u,"ns",1.0e-9,"us",1.0e-6,"ms",1.0e-3,"s",1.0,"sec",1.0,"min",60.0,"hr",3600.0,"day",24*3600.0;

  // velocity
  save,u,"cm/s",0.01,"m/s",1.0,"km/s",1000.0,"km/hr",1000.0/(24*3600),"ft/s",3.04878e-1,"mi/hr",4.4704e-1;

  // power
  save,u,"mw",1.0e-3,"w",1.0,"kw",1.0e3,"dbm",1.0e-3,"dbw",1.0;

  // frequency
  save,u,"hz",	1.0,"khz",1.0e3,"mhz",1.0e6,"ghz",1.0e9;

  // angle
  //save,u,"deg",1.0,"rad",180./pi,"arc",1.0/3600;
  save,u,"deg",pi/180.,"rad",1.0,"arc",1.0/3600*pi/180;

  // data
  save,u,"bits",1.0,"kbits",1.0e3,"mbits",1.0e6,"bytes",8.0,"kbytes",8320.0,"mbytes",8388608.0,"words",32.0;

  // data rate
  save,u,"bits/s",1.0,"kbits/s",1.0e3,"mbits/s",1.0e6,"bytes/s",8.0,"kbytes/s",8320.0,"mbytes/s",8388608.0,"baud",0.0;

  // temperature
  save,u,"deg c",1.0,"deg k",[1.0,273],"deg f",[0.555556,-32];

  // ratio
  save,u,"-",1.0,"db",1.0;

  // fringe rate
  save,u,"deg/m",1.0,"rad/m",180/pi;

  // pixels
  save,u,"pixels",1.0,"pixel",1.0,"deg/pixel",pi/180;

  return u;
}
rdf= closure(rdf, restore(tmp));
restore, scratch;

/*---------------------------------------------------------------------------------------------------*/
