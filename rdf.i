require,"yut.i";
require,"yeti.i";

func rdf (f,&key,&val,&unit,&cm)
/* DOCUMENT rdf (&f,&key,&val,&unit,&cm)
 */
{
  if(structof(f)==string)f= open(f,"r");
  txt= rdfile(f);
  strtrim, txt;
  wbang= where(strpart(txt,1:1)!=";");
  if(numberof(wbang)) txt= txt(wbang);
  wbang= where(strpart(txt,1:1)!="!");
  if(numberof(wbang)) txt= txt(wbang);
  wbang= strfind("!;", txt);
  weq= strfind("=", txt);
  vmsk= wbang(1,..)>0 & weq(2,..)>0 & weq(1,..)<wbang(1,..);
  if(anyof(vmsk)){
    wval= where(vmsk);
    txt= txt(wval);
    weq= weq(,wval);
    wbang= wbang(,wval);
  }
  txt= strpart(txt,strword(txt,"!;",2));
  cm= strtrim(txt(2,));                                            // comments (Evoid)
  txt= strtrim(txt(1,));                                           // keyval
  txt= strpart(txt,strword(txt,"=",2));
  val= strtrim(txt(2,..));                                         // vals (Evoid)
  txt= strtrim(txt(1,));
  unit= strpart(txt,strgrep("\\([a-zA-Z0-9,^\\/\\*]*\\)$",txt));   // "(units)" Evoid
  unit= strpart(unit,2:-1);                                        // "(units)" Evoid
  key= strcase(0,strtrim(strpart(txt,strgrep("(^[a-zA-Z0-9\+\, \._\/\\-]*)",txt))));
  key= key(where(key!=string(0)));

  wk = key=="suffix";
  if(anyof(wk)){              // dealing with suffix kludge
    iwk = wk(cum)(2:)+1;
    suf = strcase(0,val(where(wk)));
    suf = _("",strpart(suf,strword(suf,"\"",2))(1,..));
    key += " "+suf(iwk);
    key = strtrim(key);
    wk = where(!wk);
    key = key(wk);
    val = val(wk);
    unit = unit(wk);
    cm = cm(wk);
  }

  si = strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    key = streplace(key,si," ");
    si = strfind("  ",key);
  }

  h = h_new();
  for(i=1;i<=numberof(key);i++){
    tmp = [val(i),unit(i),cm(i),pr1(i)];
    h_set,h,key(i),[val(i),unit(i),cm(i),pr1(i)];
  }

  return h;
}

/*---------------------------------------------------------------------------------------------------*/

func rdfset (&obj, key, val, unit, comment, n)
/* DOCUMENT  rdfset (&obj, key, val, unit, comment, n)
 */
{
  if(is_void(unit))unit= "-";
  if(is_void(comment))comment= string(0);
  si = strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    key = streplace(key,si," ");
    si = strfind("  ",key);
  }
  key= strcase(0,strtrim(key));
  if(is_void(n)){
    tmp= obj(key);if(is_void(tmp))error,"key not found";
    n= tmp(4);
  }
  if (is_integer(n)) n= pr1(n);
  if(structof(val)==string){
    h_set, obj, key, [val,unit,comment,n];
  }else{
    sv= structof(val);
    if (sv==double||sv==float){
      s= [swrite(val, format="%#.16g ")+" "](sum);
    }else if (sv==long||sv==int||sv==short||sv==char){
      s= [swrite(val, format="%d")+" "](sum);
    }else if (sv==complex){
      s=  [swrite(val.re,val.im,format="(%#.16g, %#.16g) ")+" "](sum);
    }else if (sv==string){
      s= [swrite(val, format="%s")+" "](sum);
    }
    s= streplace(s,strfind("[",s,n=20),"");
    s= streplace(s,strfind("]",s,n=20),"");
    h_set, obj, key, [s,unit,comment,n];
  }
  return obj;
}

/*---------------------------------------------------------------------------------------------------*/

func rdfread (&f,key,typestrct,dims,unit=,verbose=)
/* DOCUMENT  rdfread (&f,key,typestrct,dims,unit=)
 */
{
  if(is_void(typestrct))return f;

  // are we ready to hash if not, roll it!
  if(structof(f)==string||is_stream(f))f = rdf(f);

  si = strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    key = streplace(key,si," ");
    si = strfind("  ",key);
  }
  tmp = f(strcase(0,strtrim(key)));
  if(is_void(tmp))error,"key not found: "+key;

  if(is_void(dims))dims = [0];
  dims0 = dims;                     //local copy

  if(typeof(typestrct)=="string")typestrct =  structof(typeconv(typestrct,0));

  iscplx = 0;
  if(typestrct==complex){
    iscplx = 1;
    if(dims(1)==0){
      dims0 = [1,2];
    }else{
      dims0 =_(dims(1)+1,2,dims(2,..));
    }
  }

  istring = 0;
  if(typestrct==string)istring = 1;

  out = array(typestrct,dims0);

  if(tmp(1)!=string(0)){
    stmp = tmp(1);
    if(istring){
      if(stmp==string(0))stmp="";
      out = strtrim(strpart(stmp,strword(stmp,";:,",6)));  //strtrim(strwords(stmp,nwords,delim=";:,"));
      out= out(where(out));
      nwords= numberof(out);
      if (nwords==1) out = out(1);
      dims = dimsof(out);
    }else{
      sread_n,stmp,out;
    }
    if (!istring) out= rdf_fixunit(out, unitin=tmp(2), unitout=unit);
  }
  if(iscplx)out = out(1,..)+1i*out(2,..);

  if(dims(1)==0){
    if(verbose==1)write,key,": ",print(out(1));return out(1);
  }else{
    if(verbose==1)write,key,": ",print(out);return out;
  }
  return out;
}

/*---------------------------------------------------------------------------------------------------*/

func rdfwrite (obj,fo,upper=,tablen=)
/* DOCUMENT rdfwrite (obj,fo,upper=,tablen=)
 */
{
  key_list = h_keys(obj);
  if (is_void(key_list)) return;
  n = numberof(key_list);
  itm = array(string,n);
  for(i=1;i<=n;i++){
    itm(i)= h_get(obj, key_list(i))(4);
  }
  i= array(long,n);
  sread,itm,i;
  si= sort(i);
  key_list= key_list(si);
  if (upper==1)
    ku= firstupcase(key_list);
  else
    eq_nocopy, ku, key_list;
  for(i=1;i<=n;i++){
    itm= h_get(obj, key_list(i));
    fo = rdfitem(fo,ku(i),itm(1),itm(2),itm(3),\
                 tab=1,tablen=tablen);
  }
  return fo;
}

/*---------------------------------------------------------------------------------------------------*/

func rdfitem (f,key,val,unit,comment,update=,tab=,tablen=)
/* DOCUMENT rdfitem (f,key,val,unit,comment,update=)
 */
{
  if (is_void(tablen)) tablen= 26;
  if(is_void(unit)||unit==string(0))unit="-";
  unit= strtrim(unit);
  key= strtrim(key);
  unit= " ("+unit+") ";
  if (tab) {
    kl= strlen(key);
    ul= strlen(unit);
    tl= tablen*((kl+ul)/tablen+1);
    sout= swrite(key,unit,\
      format="%"+pr1(kl)+"s%+"+pr1(tl-kl)+"s");
  } else {
    sout = key + unit;
  }
  sout += "= ";
  if(structof(val)!=string){
    sout += strtranslate(pr1(val),PR1_TABLE);
  }else{
    sout += val;
  }
  sout += "  ";
  if(!is_void(comment)&&comment!=string(0)&&comment!="")sout += "  !"+comment;

  if(update){
    if(typeof(f)!="text_stream")f = open(f,"r");
    nlm= 2000;
    lines = rdline(f, nlm);
    fnm = nameofstream(f);
    close,f;
    nl = sum(lines!=string(0));
    if(nl==nlm)write,"WARNING: increase max lines in writeRDFitem";
    w = where(strmatch(strcase(0,lines),strcase(0,key)));
    nw = numberof(w);
    if(nw==1){
      lines(w) = sout;
    }else if(nw>1){
      error,"multiple key matches found";
    }else{
      error,"KEY NOT FOUND! "+key;
    }
    if (nl){
      f = open(fnm,"w");
      write, f, lines(1:nl), format="%s\n",linesize=160;
    }
    return f;
  }
  if (typeof(f)!="text_stream") f = create(f);

  write,f,sout,linesize=200,format="%s\n";

  return f;
}

/*---------------------------------------------------------------------------------------------------*/

func rdf_fixunit (dat,unitin=,unitout=)
{
  if (!is_void(unitin)) unitin= strcase(0,unitin);
  if (!is_void(unitout)) unitout= strcase(0,unitout);

  dd = dimsof(dat);
  nd = numberof(dat);

  if(!is_void(unitin)){
    un = strtrim(unitin);
    un = strpart(un,strword(un,",",10));
    wun = un!=string(0);
    if(anyof(wun)){
      un = un(where(wun));
      nun = numberof(un);
      if(nun==nd){
        for(i=1;i<=nd;i++){
           tu = RDF_UNITS(un(i));
           if(numberof(tu)==2)dat(i)+=tu(2);
           dat(i) *= tu(1);
        }
      }else if(nun==1){
        tu = RDF_UNITS(un(1));
        if(numberof(tu)==2)dat +=tu(2);
        dat *= tu(1);
      }else{
        error,"incorrect numberof units in RDF file";
      }
    }
  }
  if(!is_void(unitout)){
    un = strtrim(unitout);
    un = strpart(un,strword(un,",",10));
    wun = un!=string(0);
    if(anyof(wun)){
      un = un(where(wun));
      nun = numberof(un);
      if(nun==nd){
        for(i=1;i<=nd;i++){
           tu = RDF_UNITS(un(i));
           dat(i) /= tu(1);
           if(numberof(tu)==2)dat(i)-=tu(2);
        }
      }else if(nun==1){
        tu = RDF_UNITS(un(1));
        dat /= tu(1);
        if(numberof(tu)==2)dat -=tu(2);
      }else{
        error,"incorrect numberof units in RDF file";
      }
    }
  }
  return dat;
}

/*---------------------------------------------------------------------------------------------------*/

func rdf_unitconv (&u)
{
  u = h_new();
  //length
  h_set,u,"nm",1.0e-9,"um",1.0e-6,"mm",1.0e-3,"cm",1.0e-2,"m", 1.0,"km",1.0e+3,"in",2.54e-2,"ft",3.048e-1,"mi",1.609344e3;

  // area
  h_set,u,"mm*mm",1.0e-6,"cm*cm",1.0e-4,"m*m",1.0,"km*km",1.0e+6,"in*in",6.4516e-4,"ft*ft",9.290304e-2,"mi*mi",2.58995511e6;

  // time
  h_set,u,"ns",1.0e-9,"us",1.0e-6,"ms",1.0e-3,"s",1.0,"sec",1.0,"min",60.0,"hr",3600.0,"day",24*3600.0;

  // velocity
  h_set,u,"cm/s",0.01,"m/s",1.0,"km/s",1000.0,"km/hr",1000.0/(24*3600),"ft/s",3.04878e-1,"mi/hr",4.4704e-1;

  // power
  h_set,u,"mw",1.0e-3,"w",1.0,"kw",1.0e3,"dbm",1.0e-3,"dbw",1.0;

  // frequency
  h_set,u,"hz",	1.0,"khz",1.0e3,"mhz",1.0e6,"ghz",1.0e9;

  // angle
  //h_set,u,"deg",1.0,"rad",180./pi,"arc",1.0/3600;
  h_set,u,"deg",pi/180.,"rad",1.0,"arc",1.0/3600*pi/180;

  // data
  h_set,u,"bits",1.0,"kbits",1.0e3,"mbits",1.0e6,"bytes",8.0,"kbytes",8320.0,"mbytes",8388608.0,"words",32.0;

  // data rate
  h_set,u,"bits/s",1.0,"kbits/s",1.0e3,"mbits/s",1.0e6,"bytes/s",8.0,"kbytes/s",8320.0,"mbytes/s",8388608.0,"baud",0.0;

  // temperature
  h_set,u,"deg c",1.0,"deg k",[1.0,273],"deg f",[0.555556,-32];

  // ratio
  h_set,u,"-",1.0,"db",1.0;

  // fringe rate
  h_set,u,"deg/m",1.0,"rad/m",180/pi;

  // pixels
  h_set,u,"pixels",1.0,"pixel",1.0,"deg/pixel",pi/180;

  return u;
}

rdf_unitconv, RDF_UNITS;
