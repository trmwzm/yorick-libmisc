scratch= save(scratch, tmp);
tmp= save(fillid, dumpid, fill, dump, indim, dimin);
func tile (base, dim, dimt, ovl=, margin=)
/* DOCUMENT t= tile (dim, dimt, ovl=, margin=)
   Function produces tiling object T. The tiling is used for the tiled
   processing of N-dimensional arrays. Margins (MARGIN) of unprocessed
   data and ovrelapping (OVL) of tiles is allowed (each of them may be set
   indpendently for each dimension.)  As much data as possible is grabbed
   to fill the tile, and similarly to dump it back.
   DIM and DIMT are list of dimensions -- or scalars if all dims equal (one of them at least
   must be an array of length equal to rank of tiling) -- of the
   array to be tiled (DIM) and of all tiles (DIMT.)
   OVL= gives the one-side width of overlap in tiling.
   MARGIN= on output, width of un-tiled data at boundary of original array.

   METHODS
   ------
   FILL: tile= T(fill,from_array,tile_number[,badval=]); // 0<tile_number<=T(out,nt)
   DUMP: T, dump, to_array, tile, tile_number;
   BADVAL= FILL keyword (value to fill tile with if outside of input array.)

   USE
   ---
   to= tile(dim,dimt[,ovl=][,margin=]);
   a= random(_(nd,dim));
   b= array(0.0,_(nd,dim));
   for (i=1;i<=to(out,nt);i++)
     to,dump,b,to(fill,a,i),i;
   SEE ALSO:
 */
{
  obj= base(:);
  dm= dimsof(dim,dimt);
  d= dm(0);
  z= array(0,dm);
  dim+= z;                         // base ra dimensions
  dimt+= z;                        // input tile ra dimensions
  ovl= is_void(ovl)? z: ovl+z;
  margin= is_void(margin)? z: margin+z;
  badid= -100;
  save, obj, d, dim, dimt, ovl, margin, badid;

  out= save();
  save, out, tshp= dimt-2*ovl;     // size(s) of the output tile chunk
  for (tsz=1,i=1;i<=d;i++)
    tsz*= out(tshp,noop(i));       // total number of elements output per tile
  save, out, tsz;
  save, out, tra= (dim-2*margin)/out(tshp)+((dim-2*margin)%out(tshp)>0);  // dimensions ra of tiles
  for (nt=1,i=1;i<=d;i++)
    nt*= out(tra,noop(i));
  save, out, nt;                  // number of tiles
  save, obj, out;

  in= save();
  for (tsz=1,i=1;i<=d;i++)
    tsz*= dimt(i);
  save, in, tsz;
  save, obj, in;

  return obj;
}
func fillid (ti)
// map all tile indices to array indices; if out of array (w/o margins), set to BADID
{
  use, d, dim, dimt, ovl, margin, badid, in, out;
  if (ti<1 || ti>out(nt))
    error,"tile index out of bounds.";
  tdi= use_method(indim,ti,out(tra));            // tile indices from its number, and tile ra
  tdi= (tdi-1)*out(tshp)+margin;                 // transform from tile ra indices to base ra indices.
  tdi-= ovl;
  tdi+= use_method(indim,indgen(in(tsz)),dimt);  // the whole input tile
  m= tdi<1 | tdi>dim;
  m= m(sum,..)>0;
  tdi= use_method(dimin,tdi,dim);
  if (anyof(m))
    tdi(where(m))= badid;
  return tdi;
}
func dumpid (ti)
// map all tile indices to array indices; if in tile overlap zone, or out of ra (w/o margin,) set to BADID
{
  use, d, dim, dimt, ovl, margin, badid, in, out;
  if (ti<1 || ti>out(nt))
    error,"tile index out of bounds.";
  tdi= use_method(indim,ti,out(tra));           // tile indices from its number, and tile ra
  tdi= (tdi-1)*out(tshp)+margin;                // transform from tile ra indices to base ra indices.
  tdi-= ovl;
  tli= use_method(indim,indgen(in(tsz)),dimt);
  m1= tli<=ovl | tli>(dimt-ovl);
  m1= m1(sum,..)>0;
  tdi+= tli;  // the whole input tile
  m2= tdi<=margin | tdi>(dim-margin);           // 
  m2= m2(sum,..)>0;
  m= m1 | m2;
  tdi= use_method(dimin,tdi,dim);
  if (anyof(m))
    tdi(where(m))= badid;
  return tdi;
}
func fill (a, ti, badval=)
{
  use, d, dim, dimt, badid, out;
  if (nallof(dimsof(a)==_(d,dim)))
    error,"incorrect input array dimensions.";
  if (ti<1 || ti>out(nt))
    error,"tile index out of bounds.";
  badval= is_void(badval)? 0: badval;

  t= array(structof(a), _(d,dimt));
  w= use_method(fillid,ti);
  m= w!=badid;
  if (anyof(m)) {
    wm= where(m);
    t(wm)= a(w(wm));
  }
  if (anyof(!m))
    t(where(!m))= badval;
    
  return t;
}
func dump (a, t, ti)
{
  use, d, dim, dimt, badid, out;
  if (nallof(dimsof(a)==_(d,dim)))
    error,"incorrect input array dimensions.";
  if (nallof(dimsof(t)==_(d,dimt)))
    error,"incorrect input tile array dimensions.";
  if (ti<1 || ti>out(nt))
    error,"tile index out of bounds.";

  w= use_method(dumpid,ti);
  m= w!=badid;
  if (anyof(m)) {
    wm= where(m);
    a(w(wm))= t(wm);
  }
  return t;
}
func indim (n, dim)
{
  nd= numberof(dim);
  nn= numberof(n);

  // array of products of dimensions below/left
  dimp= array(1,nd+1);
  for (i=2;i<=nd+1;i++)
    dimp(i)= dimp(i-1)*dim(i-1);

  // compute indices from last
  out= array(0,nd,(dimsof(n)(1)>0? nn: []));
  for (i=nd,n1=n(*)-1; i>0; i--) {
    out(i,..)= n1/dimp(i)+1;
    n1= n1%dimp(i);
  }
  return out;
}
func dimin (n, dim)
{
  nd= numberof(dim);
  nn= numberof(n)/nd;

  dimp= array(1,nd+1);
  for (i=2;i<=nd+1;i++)
    dimp(i)= dimp(i-1)*dim(i-1);

  n1= n-1;
  out= array(0,(dimsof(n)(1)>1? nn: []));
  for (i=nd;i>0;i--)
      out+= n1(i,..)*dimp(i);
  return out+1;
}
tile= closure(tile,restore(tmp));
restore, scratch;

#if 0
// INDEX CONVERSIONS
func test1 (nd)
{
  dimt= max(1,long(random(nd)*20));
  margin= min(long(random(nd)*20),dimt-1);
  dim= max(1,long(random(nd)*20)*dimt+2*margin);
  a= random(_(nd,dim));

  t= tile(dim,dimt);
  mx= a(*)(mxx);
  mxd= t.indim(mx,dim);
  write,nd,mx==t.dimin(mxd,dim),       format="test1 #dims = %d, mxx       = True? %d\n";

  mx2= array(mx,2);
  write,nd,allof(mx==t.dimin(mxd,dim)),format="test1 #dims = %d, [mxx,mxx] = True? %d\n";
}
// ARRAYS
func test2 (nd, plot=)
{
  dimt= max(1,long(random(nd)*10));
  margin= min(long(random(nd)*10),dimt-1);
  dim= max(1,long(random(nd)*10)*dimt+2*margin);
  a= random(_(nd,dim));

  to= tile(dim,dimt);

  b= array(0.0,_(nd,dim));
  for (i=1;i<=to(out,nt);i++) {
    to,dump,b,to(fill,a,i),i;
  }
    if (plot==1) {
    window,0;fma;pli,a(,*),cmin=0,cmax=1;limits;
    window,1;fma;pli,b(,*),cmin=0,cmax=1;limits;
  }
  a-= b;
  a= a(*);
  res= [a(avg),a(ptp)];
  write,nd,res(sum),format="test2 #dims = %d, avg+ptp = Null? %f\n";
}
func chopmargin (&a, margin)
{
  nd= dimsof(a)(1);
  if (nd==1)
    a= a(margin(1)+1:-margin(1));
  else if (nd==2)
    a= a(margin(1)+1:-margin(1),margin(2)+1:-margin(2));
  else if (nd==3)
    a= a(margin(1)+1:-margin(1),margin(2)+1:-margin(2),margin(3)+1:-margin(3));
  else if (nd==4)
    a= a(margin(1)+1:-margin(1),margin(2)+1:-margin(2),margin(3)+1:-margin(3),\
         margin(4)+1:-margin(4));
  else if (nd==5)
    a= a(margin(1)+1:-margin(1),margin(2)+1:-margin(2),margin(3)+1:-margin(3),
         margin(4)+1:-margin(4),margin(5)+1:-margin(5));
  else
    error;
}
// MARGIN
func test3 (nd, plot=)
{
  dimt= max(1,long(random(nd)*10));
  margin= max(0,min(long(random(nd)*10),dimt/2-1));
  dim= max(1,long(random(nd)*10)*dimt+2*margin+1);
  a= random(_(nd,dim));

  to= tile(dim,dimt,margin=margin);

  b= array(0.0,_(nd,dim));
  for (i=1;i<=to(out,nt);i++) {
    to,dump,b,to(fill,a,i),i;
  }
  if (plot==1) {
    window,0;fma;pli,a(,*),cmin=0,cmax=1;limits;
    window,1;fma;pli,b(,*),cmin=0,cmax=1;limits;
  }
  a-= b;
  chopmargin, a, margin;
  a= a(*);
  res= [a(avg),a(ptp)];
  write,nd,res(sum),format="test3 #dims = %d, avg+ptp = Null? %f\n";
}
// OVERLAP
func test4 (nd,plot=)
{
  dimt= max(1,long(random(nd)*10));
  margin= max(0,min(long(random(nd)*10),dimt/2-1));
  dim= max(1,long(random(nd)*10)*dimt+2*margin);
  a= random(_(nd,dim));

  to= tile(dim,dimt,ovl=margin);

  b= array(0.0,_(nd,dim));
  for (i=1;i<=to(out,nt);i++) {
    to,dump,b,to(fill,a,i),i;
  }
  if (plot==1) {
    window,0;fma;pli,a(,*),cmin=0,cmax=1;limits;
    window,1;fma;pli,b(,*),cmin=0,cmax=1;limits;
    window,2;fma;pli,a(,*)-b(,*),cmin=0,cmax=1;limits;
  }
  a-= b;
  a= a(*);
  res= [a(avg),a(ptp)];
  write,nd,res(sum),format="test4 #dims = %d, avg+ptp = Null? %f\n";
}
// OVERLAP & MARGIN
func test5 (nd,plot=)
{
  dimt= max(1,long(random(nd)*10));
  margin= max(0,min(long(random(nd)*10),dimt/2-1));
  ovl= max(0,min(long(random(nd)*10),dimt/2-1));
  dim= max(1,long(random(nd)*10)*dimt+2*margin+2*ovl);
  a= random(_(nd,dim));

  to= tile(dim,dimt,ovl=ovl,margin=margin);

  b= array(0.0,_(nd,dim));
  for (i=1;i<=to(out,nt);i++) {
    to,dump,b,to(fill,a,i),i;
  }
  if (plot==1) {
    window,0;fma;pli,a(,*),cmin=0,cmax=1;limits;
    window,1;fma;pli,b(,*),cmin=0,cmax=1;limits;
    window,2;fma;pli,a(,*)-b(,*),cmin=0,cmax=1;limits;
  }
  a-= b;
  chopmargin, a, margin;
  a= a(*);
  res= [a(avg),a(ptp)];
  write,nd,res(sum),format="test5 #dims = %d, avg+ptp = Null? %f\n";
}
func testall(nd)
{
  test1,nd;
  test2,nd;
  test3,nd;
  test4,nd;
  test5,nd;
}
for (i=1;i<=1;i++)
  for (nd=1;nd<=4;nd++)
    testall,nd;

#endif

#if 0
nd= 2;
dim= [40,40];
dimt= [24,24];
to= tile(dim,dimt,ovl=2);
a= random(_(nd,dim));
b= array(0.0,_(nd,dim));
for (i=1;i<=to(out,nt);i++)
  to,dump,b,to(fill,a,i),i;
window,0;fma;pli,a,cmin=0,cmax=1;limits;
window,1;fma;pli,to(fill,a,1),cmin=0,cmax=1;limits;
// to,dump,b,to(fill,a,1),1;
window,2;fma;pli,b,cmin=0,cmax=1;limits;
window,3;fma;pli,a-b,cmin=0,cmax=1;limits;
#endif
