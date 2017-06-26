
func qplooks (args)
/* DOCUMENT out= qplooks (a,b,l1,l2,&intf,hdr=,mag=,fcplx=,bkl=, \
                          norm=,verbose=,normlook=,powout=)
   qplooks reads complex array data from 1[2] files/arr, FA & FB,
   and forms A*conj(B), then take "looks" L1xL2 -- in first/fast and
   second dimensions.  All computation done in double-complex.
   Output is double-complex, double if absolute val |A*conj(B)|
   is requested (MAG==1). If FCPLX==1, output is float, with
   a first dimension of length 2 for complex, unless MAG==1:
   float-complex if FCPLX==1, float is both FCPLX==1 && MAG==1.
   
   If NORM=1, <A*conj(B)>/sqrt(<|A|^2>)
   If NORM=2, <A*conj(B)>/sqrt(<|B|^2>)
   If NORM=3, <A*conj(B)>/sqrt(<|A|^2>)/sqrt(<|B|^2>) ~ correlation
   
   If is_void(B) qplooks multilooks file/array A.
   NORMLOOK=1 will do averages (1/#looks) instead of sums where <>

   OPTIONAL outout INTF is *not* affected by FCLX, and always complex,
   and is *not* look normalized.

   POWOUT==1 switches OUT to interefrogram power (float rith fcplx==1)

   out   :output array
   fa    :array of binary struct with
   fb    :array of binary struct with
   hdr   :dump output file struct info in the named text file
   mag   :dump float(|<a*conj(b)>|) instead of <a*conj(b)>
   fcplx :dump float(<a*conj(b)>)
   blk   :block length [2000 default]
   verbose :runtime info [off]

   output disk file
   ================
   recl= dimi(2);
   na= dimi(3);
   reclo= recl/l1;
   nao= na/l2;
   
   dimo= (mag==1? [2,reclo,nao]: (fcplx==1? [3,2,reclo,nao]: [2,reclo,nao]));
   typo= (mag==1? (fcplx==1? float: double): (fcplx==1? float: complex));
   if (mag==1)
     if (fcplx==1)
       out= array(float,[2,recl/l1,na/l2]);
     else
       out= array(double,[2,recl/l1,na/l2]);
   else
     if (fcplx==1)
       out= array(float,[3,2,recl/l1,na/l2]);
     else
       out= array(comlplex,[2,recl/l1,na/l2]);
   
   fout= open(fnm,"wb+");
   add_variable, fout,offset,"x",typo,dimo;

   recl= 2622;
   typi= fcomplex;  // only to associate float complex file to double cplx
                    // array
   a= readFlat("a.slc",typi,[2,recl],name="x");
   b= readFlat("b.slc",typi,[2,recl],name="x");
   qplooks,o.x,a.x,b.x,2,8,mag=1;
 */
{
  // - keyword arg processing
  s= ["hdr","mag","fcplx","bkl","norm","normlook","powout","verbose"];
  hdr= mag= fcplx= norm= powout= verbose= normlook= [];  // key init
  bkl= 2000;
  sk= args(-);        // key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,"hdr"))) hdr= args("hdr");
  if (nk && anyof(strmatch(sk,"mag"))) mag= args("mag");
  if (nk && anyof(strmatch(sk,"fcplx"))) fcplx= args("fcplx");
  if (nk && anyof(strmatch(sk,"bkl"))) bkl= args("bkl");
  if (nk && anyof(strmatch(sk,"norm"))) norm= args("norm");
  if (nk && anyof(strmatch(sk,"powout"))) powout= args("powout");
  if (nk && anyof(strmatch(sk,"normlook"))) normlook= args("normlook");
  if (nk && anyof(strmatch(sk,"verbose"))) verbose= args("verbose");
  i=0; while (i<nk && strmatch(s,sk(i+1))(sum)) i++;
  if (i<nk) error,"unkown keyword: "+sk(i+1);

  // - positional arg processing
  if (args(0)<2 || args(0)>5) error, \
    "qplooks,fa,fb,l1,l2,&int,hdr=,mag=,fcplx=;";

  // - assert A complex
  typa= structof(args(1,:));
  if (nameof(typa)!="complex")
    error,"A: expecting complex input";

  // - assert B complex, if provided
  if (!is_void(args(2,:)) && (typb=structof(args(2,:))))
    if (nameof(typb)!="complex")
      error,"B: expecting complex input";

  // - assert equal input dims
  dimi = dimsof(args(1,:));
  dimi2= dimsof(args(2,:));
  if (anyof(dimi!=dimi2))
    write,"WARNING: Unmatched dimensions: "+pr1(dimi)+"!="+pr1(dimi2);

  // - data dimensions 
  recl= dimi(2);
  na= dimi(3);

  // - default look = [1,1]
  l1= (args(0)>2? args(3): 1);
  l2= (args(0)>=4? args(4): 1);
  l12= l1*l2;

  // - process number of look normaliztion factor
  if (normlook==1) {
    if (norm==1 || norm==2)    // --- (1/N) / sqrt(1/N)
      nlkfac= 1.0f/sqrt(l12);  // float to cover both real types
    else if (norm==3)
      nlkfac= 1.0f;
    else
      nlkfac= 1.0f/(l12);      // --- (1/N)
    if (verbose==1)
      write,format="Look number factor (reciprocal): %.5f (%.5f)\n",\
        nlkfac,1/nlkfac;
  }
  if (is_void(args(2,:)))
    if (norm>1)
      error,"norm>2 not allowed, multilooking a single file.";
  
  // - processing in blocks (2nd dimension): block length and number
  // make sure block length is a l2 multiple
  bkl= bkl-bkl%l2;
  nbk= na/bkl+(na%bkl>0);

  // - do not take partial l2 looks
  na-= na%l2;

  // - allocate output
  if (mag==1 || powout==1)
    if (fcplx==1)
      out= array(float,[2,recl/l1,na/l2]);
    else
      out= array(double,[2,recl/l1,na/l2]);
  else
    if (fcplx==1)
      out= array(float,[3,2,recl/l1,na/l2]);
    else
      out= array(comlplex,[2,recl/l1,na/l2]);
                   
  // - allocate optional interferogram output
  if (args(0)==5)
    args, 5, array(complex,[2,recl/l1,na/l2]);
      
  if (verbose==1) {
    write,format="#blocks: %s\n",pr1(nbk);
    write,format="number of looks dims 1,2: %i, %i\n",l1,l2;
    if (!is_void(norm))
      write,format="Type of normalization: %i\n",norm;
  }
  
  noo= 1;
  ddoo= [];
  for (i=1;i<=nbk;i++) {
    if (verbose==1)
      write,"block: "+pr1(i);
    na1= (i-1)*bkl+1;
    na0= min(i*bkl,na);
    // - input data 1 block
    dat1= args(1,:)(,na1:na0);
    // - input data 2 block, if exist
    if (!is_void(args(2,:)))
      dat2= args(2,:)(,na1:na0);
    else 
      dat2= 1;
    // - interferogram, memory hog but needed for norm=
    dat= dat1*conj(dat2);
    // - crop if partial range l1 looks
    if (recl%l1)
      dat= dat(:-recl%l1,..);
    // - looked down dimensions and allocate ...
    ddo= [2,recl-recl%1,na0-na1+1]/[1,l1,l2];
    // ... only if size has changed (stash previous dims)
    if (anyof(ddoo!=ddo))
      dato= array(complex,ddo);
    else
      dato(*)= 0;  // if reallocate, NULL
    ddoo= ddo;
    // - reshape for looks
    dat= reform(dat,[4,l1,ddo(2),l2,ddo(3)]);
    // - take looks
    dato= dat(sum,,sum,);
    // - stach interferogram, if requested
    if (args(0)==5)
      args(5,:)(..,noo:noo+ddo(3)-1)= dato;
    
    // - normalization, if !is_void(norm)
    if (norm==1 || norm==3) {
      // - take magnitude looks, then square root
      dat1= reform(dat1.re^2+dat1.im^2,[4,l1,ddo(2),l2,ddo(3)]);
      dat1= sqrt(dat1(sum,,sum,));
      m1= dat1==0;  // protect against zeroes
      if (anyof(m1)) 
        dat1(where(m1))= 1.0;
      // - normalize
      if (norm==1)
        if (powout==1)
          dato= dat1;
        else
          dato/= dat1;
    }
    if (norm==2 || norm==3) {
      dat2= reform(dat2.re^2+dat2.im^2,[4,l1,ddo(2),l2,ddo(3)]);
      dat2= sqrt(dat2(sum,,sum,));
      m2= dat2==0;
      if (anyof(m2))
        dat2(where(m2))= 1.0;
      if (norm==2)
        if (powout==1)
          dato= dat2;
        else
          dato/= dat2;
    }
    if (norm==3)
        if (powout==1)
          dato= dat1*dat2;
        else
          dato/= dat1*dat2;
    // - nulling bad values not needed since power is null.
    //m1*= m2;
    //if (anyof(m1))
    //  dato(where(m1))= 0.0;

    // - output type casting
    if (mag==1 || powout==1) {
      dato= abs(dato);
      if (fcplx==1)
        dato= float(dato);
    } else {
      if (fcplx==1) {
        d= dimsof(dato);
        df= array(float,_(1+d(1),2,(d(1)>0? d(2:): [])));
        df(1,..)= dato.re;
        df(2,..)= dato.im;
        dato= [];
      }
    }
    // - look normalization factor
    if (normlook==1)
      if (powout==1)
        dato/= nlkfac;
      else
        dato*= nlkfac;
    // - write out
    out(..,noo:noo+ddo(3)-1)= dato;
    // - step in second dimension
    noo+= ddo(3);
  }
  
  if (verbose==1)
    write,format="last record written %i\n",noo-1;

  return out;
}
wrap_args, qplooks;

/*--------------------------------------------------------*/

func flcopy (args)
/* DOCUMENT flcopy (flo.x,fli.x,rgi1,rgi2,bkl=,verbose=)
   copy from-to 2D arrays X associated with binary files
   FLI.X: input stream array
   FLO.X: output stram array whic hmust be open as WB+!!
   RGI1:  index#1 range into FLI.X
   RGI2:  index#3 range into FLI.X
   BLK=   defaut 2nd index FLI read in blocking length
   VERBISE= sic
   NOTE: the range[s] to be copied must match the
   previous array declaration !
  
   nin=9900;
   fc= 2;
   fi= open(fnmi,"rb");
   di= [2,fc*nin,sizeof(fi)/(fc*sizeof(float)*nin)];
   add_variable,fi,-1,"x",float,di;

   fnmo="jk";
   fo= open(fnmo,"wb+"); // !!!! wb+ !!!!!
   no1= 500;
   no2= 5000;
   add_variable,fo,-1,"x",float,[2,fc*no1,no2];
   flcopy,fo.x,fi.x,1:2*no1*fc:2,100+1:100+2*no2:2,verbose=1;
   close,fo;

   Note that the range must have the number of elements
   declared in out flo dimension
 */
{
  // keyword arg processing
  s= ["bkl","verbose"];
  verbose= [];  // key init
  bkl= 2000;
  sk= args(-);        // key strings
  nk= numberof(sk);
  if (nk && anyof(strmatch(sk,"bkl"))) 
    bkl= args("bkl");
  if (nk && anyof(strmatch(sk,"verbose"))) 
    verbose= args("verbose");
  i=0; 
  while (i<nk && strmatch(s,sk(i+1))(sum)) 
    i++;
  if (i<nk) error,"unkown keyword: "+sk(i+1);

  // positional arg processing
  if (args(0)<3 || args(0)>4) error,              \
    "flcopy,flo,fli,i1:i1,[i2:i2],bkl=,verbose=";

  dimo= dimsof(args(1,:));
  dimi= dimsof(args(2,:));

  // first index and range
  rgi1= args(3);
  in1= indgen(dimi(2))(rgi1);
  if ((no1=numberof(in1))!=dimo(2))
    error,"number of values index:1 in and out do not match... in="+pr1(no1)+\
          "out="+pr1(dimo(2));

  // second index and range
  if (args(0)==4) 
    rgi2= args(4);
  else
    rgi2= :;
  in2= indgen(dimi(3))(rgi2);
  i21= in2(1);
  i22= in2(0);
  i23= in2(2)-in2(1);   
  i2= indgen(0:(i22-i21)/i23);   // in2==(i21+(i2-1)*i23)
  if ((no2=numberof(in2))!=dimo(3))
    error,"number of values index:2 in and out do not match... in="+pr1(no2)+\
          "out="+pr1(dimo(3));

  na= i22-i21+1;           // how many read
  nbk= na/bkl+(na%bkl>0);

  if (verbose==1) {
    write,format="#blocks: %s\n",pr1(nbk);
    write,format="dims  in: %s\n",pr1([dimi]);
    write,format="dims out: %s\n",pr1([dimo]);
  }
  noo= 0;
  for (i=1;i<=nbk;i++) {
    if (verbose==1)
      write,"block: "+pr1(i);
    na1= (i-1)*bkl+i21;
    na0= min(i*bkl+i21-1,i22);
    dat= args(2,:)(rgi1,na1:na0);
    w= where(in2>=na1 & in2<=na0);
    j1= (na1-i21)/i23;
    j2= (na0-i21)/i23;
    j1= i21+j1*i23-na1+1;
    j2= i21+j2*i23-na1+1;
    dat= dat(:,j1:j2:i23);
    no= (j2-j1)/i23+1;
    args(1,:)(..,noo+1:noo+no)= dat;
    noo+= no;
  }
  if (verbose==1)
    write,format="number of record written %i\n",noo;
}
wrap_args, flcopy;

