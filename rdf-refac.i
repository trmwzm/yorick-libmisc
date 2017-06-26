require,"yut.i";

scratch= save(scratch, tmp);
tmp= save(get,put,set,unitconv,fixunit,item)
func rdf (base,f,&key,&val,&unit,&cm)
/* DOCUMENT rdf (&f,&key,&val,&unit,&cm)
 */
{
  ob= base(:);

   // read file
  if (structof(f)==string)
    f= open(f,"r");
  txt= rdfile(f);
  save, ob, txt;

  // start processing, find comment char
  strtrim, txt;
  ltxt= strlen(txt);
  scom= [";","#","!"];
  ncom= numberof(scom);
  icom= array(0,[2,numberof(txt),ncom]);
  for (i=1;i<=numberof(scom);i++)
    icom(..,i)= strfind(scom(i),txt)(1,..);

  // find =
  weq= strfind("=", txt)(1,..);

  // look for valid key=val
  for (m=0,i=1;i<=numberof(scom);i++)
    m|= weq<icom(..,i);

  ikv= where(m);
  if (numberof(ikv)==0)
    error,"RDF file void.";
  else
    txt= txt(ikv);
  save,ob,ikv;

  txt= strpart(txt,strword(txt,"!;",2));
  cm= strtrim(txt(2,));                                            // comments
  txt= strtrim(txt(1,));                                           // keyval
  txt= strpart(txt,strword(txt,"=",2));
  val= strtrim(txt(2,..));                                         // vals
  txt= strtrim(txt(1,));
  unit= strpart(txt,strgrep("\\([-a-zA-Z0-9,^\\/\\*]*\\)$",txt));  // "(units)", nil no conversion, - no unit.
  unit= strpart(unit,2:-1);                                        // "(units)"
  key= strcase(0,strtrim(strpart(txt,strgrep("(^[a-zA-Z0-9\, \._\/\\-]*)",txt))));
  key= key(where(key!=string(0)));

  wk= key=="suffix";
  if (anyof(wk)) {              // dealing with suffix !kludge!
    iwk= wk(cum)(2:)+1;
    suf= strcase(0,val(where(wk)));
    suf= _("",strpart(suf,strword(suf,"\"",2))(1,..));
    key+= " "+suf(iwk);
    key= strtrim(key);
    wk= where(!wk);
    key= key(wk);
    val= val(wk);
    unit= unit(wk);
    cm= cm(wk);
  }
  strcase,0,unit;

  si= strfind("  ",key);
  while (anyof(si(2,)!=-1)) {   // replace double spaces
    key= streplace(key,si," ");
    si= strfind("  ",key);
  }

  kv= save();
  univ= ob(unitconv,);
  nun= 10;
  for (i=1;i<=numberof(key);i++) {
    if (unit(i)!=string(0)) {
      us= strpart(unit(i),strword(unit(i)," ,*/^",nun));
      m= us!=string(0);
      if (anyof(m)) {
        us= us(where(m));
        for (j=1;j<=numberof(us);j++)
          if (is_obj(univ,us(j),1)<0)
            error,"unknown unit: "+us(j)+".";
      } else
        unit(i)= string(0);
    }
    save,kv,key(i),save(val=val(i),comment=cm(i),unit=unit(i),textline=ikv(i));
  }
  save, ob, kv, univ;

  return ob;
}
func set (key, val, unit, comment)
{
  use, kv;
  if (is_void(unit))
    unit= "-";
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
  if (structof(val)==string) {
    save, v, val=val;
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
    save, v, val=s;
  }
}
func get (key,typestrct,dims,unit=,verbose=)
{
  use, kv;

  key= strcase(0,strtrim(key));
  si= strfind("  ",key);
  while(anyof(si(2,)!=-1)){   // replace double spaces
    key= streplace(key,si," ");
    si= strfind("  ",key);
  }

  if (is_obj(kv,noop(key),1)<0)
    error,"key not found";
  v= kv(noop(key));

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

  // 
  out= array(typestrct,dims0);
  nout= numberof(out);
 
  if (v(val)!=string(0)) {
    stmp= v(val);
    stmp= strtrim(strpart(stmp,strword(stmp," :,",nout+1)));
    stmp= stmp(where(stmp));
    nwords= numberof(stmp);
    if (nwords!=nout)
      error,"dimension mismatch, provide matching dims.";
    if (typestrct==string) {
      out= reform(stmp,dims);
    } else {
      for (i=1;i<=nwords;i++)
        if (!sread(stmp(i),out(i)))
          error,"error reading variable.";
    }
    if (typestrct!=string)
      out= use_method(fixunit,out,unitin=v(unit),unitout=unit);
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
func put (fo,upper=,tablen=)
{
  use, kv;
  key_list= kv(*,); 
  if (is_void(key_list))
    return;
  n= numberof(key_list);
  itm= array(string,n);
  for (i=1;i<=n;i++) {
    itm(i)= kv(noop(i),textline);
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
    fo= use_method(item,fo,ku(i),itm(1),itm(2),itm(3),  \
                 tab=1,tablen=tablen);
  }
  return fo;
}
func item (f,key,val,unit,comment,update=,tab=,tablen=)
{
  if (is_void(tablen))
    tablen= 26;
  key= strtrim(key);
  unit= strtrim(unit);
  if (unit!=string(0)&&!is_void(unit))
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
  if (structof(val)!=string) {
    sout+= strtranslate(pr1(val),PR1_TABLE);
  } else {
    sout += val;
  }
  sout+= "  ";
  if (!is_void(comment)&&comment!=string(0)&&comment!="")
    sout+= "  !"+comment;
  
  if (update) {
    if(typeof(f)!="text_stream")
      f= open(f,"r");
    nlm= 2000;
    lines= rdline(f, nlm);
    fnm= nameofstream(f);
    close,f;
    nl= sum(lines!=string(0));
    if (nl==nlm)
      write,"WARNING: increase max lines in writeRDFitem";
    w= where(strmatch(strcase(0,lines),strcase(0,key)));
    nw= numberof(w);
    if (nw==1) {
      lines(w)= sout;
    } else if(nw>1) {
      error,"multiple key matches found";
    } else {
      error,"KEY NOT FOUND! "+key;
    }
    if (nl) {
      f= open(fnm,"w");
      write, f, lines(1:nl), format="%s\n",linesize=160;
    }
    return f;
  }
  if (typeof(f)!="text_stream")
    f= create(f);
  
  write,f,sout,linesize=200,format="%s\n";
  
  return f;
}
func fixunit (dat,unitin=,unitout=)
{
  use, univ;

  if (!is_void(unitin))
    unitin= strcase(0,unitin);
  if (!is_void(unitout))
    unitout= strcase(0,unitout);

  dd= dimsof(dat);
  nd= numberof(dat);

  if (!is_void(unitin)) {
    un= strtrim(unitin);
    un= strpart(un,strword(un,",",10));
    wun= un!=string(0);
    if (anyof(wun)) {
      un= un(where(wun));
      nun= numberof(un);
      if (nun==nd) {
        for (i=1;i<=nd;i++) {
           tu= univ(un(i));
           if(numberof(tu)==2)dat(i)+=tu(2);
           dat(i) *= tu(1);
        }
      } else if (nun==1) {
        tu= univ(un(1));
        if(numberof(tu)==2)dat +=tu(2);
        dat *= tu(1);
      } else {
        error,"incorrect numberof units in RDF file";       
      }
    }
  }
  if (!is_void(unitout)) {
    un= strtrim(unitout);
    un= strpart(un,strword(un,",",10));
    wun= un!=string(0);
    if (anyof(wun)) {
      un= un(where(wun));
      nun= numberof(un);
      if (nun==nd) {
        for (i=1;i<=nd;i++) {
           tu= univ(un(i));
           dat(i) /= tu(1);
           if(numberof(tu)==2)dat(i)-=tu(2);
        }
      } else if(nun==1) {
        tu= univ(un(1));
        dat /= tu(1);
        if (numberof(tu)==2)dat -=tu(2) ;
      } else {
        error,"incorrect numberof units in RDF file";       
      }
    }
  }
  return dat;  
}
func unitconv (dum)
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

