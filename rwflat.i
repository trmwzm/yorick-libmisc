require, "fcomplex.i";
require, "yut.i";

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
