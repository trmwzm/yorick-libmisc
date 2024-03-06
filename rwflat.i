require, "fcomplex.i";
require, "yut.i";
require, "nml.i";

/* -------------------------------------------------------------------------*/

func writeFlat (x,fnm,offset,append=,update=,fcomplex=)
/* DOCUMENT writeFlat(x,f,offset)
 typeof(h);dimsof(h);
 "double"
 [2,100,100]
 writeFlat,h,"jk"
 readFlat,g,"jk","double",[2,100,100]
 NOTE: fcomplex must be done at opening time ONLY!
 */
{
  if (is_void(x)) error,"array void";
  if(is_void(offset))offset=0;

  if (typeof(fnm)!="stream"){
    if(append==1){
      f = open(fnm,"ab");
    }else if(update==1){
      f = open(fnm,"r+b");
    }else{
      f = open(fnm,"wb");
    }
  }else{
    f= fnm;
    if(!is_void(append)&&append==1){
      filnm = nameofstream(f);
      f=[];f = open(filnm,"ab");
    }
  }
  if (fcomplex==1)
    add_fcomplex_type,f;

  if(typeof(x)=="complex"){
    save,f,complex;
    info,x;
  }

  _write,f,offset,x;

  return f;
}

/* ------------------------------------------------------------------------- */
/* ------------------------------------------------------------------------- */

func readFlat(f, typ, dims, offset, name=, sl=,pc=,i86=,sun=,sgi64=)
  /* DOCUMENT readFlat(&f,typ,dims,offset)
     typeof(h);dimsof(h);
     "double"
     [2,100,100]
     writeFlat,h,"jk"
     g = readFlat("jk","double",[2,100,100])
     IF(is_tring("x"))returns f.x f=readFlat("x",f,"fcomplex",[1])
     NOTE :a missing last dimension (noted by dims(1)!=#dims) is
     computed to fill file size
     :can also be used to install a struct "fcomplex" bi is_void(dims)
     f = readFlat( ,"fileName","fcomplex")
     add_variable, f, offset, "jk", typ, dims;
  */
{
  isstream = 0;
  if (typeof(f)!="stream") {
    f= open(f,"rb");
    isstream = 1;
  }

  if (pc==1)
    pc_primitives,f;
  if (i86==1)
    i86_primitives,f;
  if (sun==1)
    sun_primitives,f;
  if (sgi64==1)
    sgi64_primitives,f;

  if (typeof(typ)!="string") {
    typstrct= typ;
    if(typ==fcomplex)
      typ= "fcomplex";
    else
      typ= typeof(typ(0));
  } else
    typstrct = structof(typeconv(typ, 0.));

  if (!is_void(dims) && numberof(dims)==dims(1)) {
    for (nt=1,i=1;i<=dims(1)-1;i++)
      nt*= dims(1+i);
    sf= sizeof(f);
    if (!is_void(offset))
      sf-= offset;
    dims= long(_(dims,sf/(sizeof(typeconv(typ, 0.))*nt)));
  }

  if (typ!="fcomplex") {
    zero= typeconv(typ,0); 
    szero= sizeof(zero);
  } else {
    add_fcomplex_type, f;
    zero= complex(0);
    szero= 2*sizeof(float);
  }

  if (!is_void(name) && structof(name)==string) {
    if (is_void(offset))
      offset= -1;
    add_variable,f, offset, name, structof(zero), dims;
    return f;
  } else {
    if (is_void(offset))
      offset= 0;
    if (!is_void(sl))
      offset+= sl*szero*dims(2);
    if (!is_void(dims))
      x= array(zero,dims);
    else
      x= zero;
   
    if (typ=="complex")
      save, f, complex;
    _read, f, offset, x;

    return x;
  }
}

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
   write array A in "raw/local" binary filename FNM, and write its associated metadata
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

  writeFlat,a,fnm;
  o= save();
  o= save("binio",save());
  ot= o(binio);
  save,ot,fnm=basename(fnm);
  save,ot,tnm,tsz,rk,shp,sz;

  ll= nmlox(o);
  return write(open(fnm+".bnml","w"),ll,format="%s\n");
}
