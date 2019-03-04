scratch= save(scratch, tmp);
tmp= save(fillid, dumpid, fill, dump, indim, dimin);
func tile (base, dim, dimt, ovl=, offset=)
{
  obj= base(:);
  z= array(0,dimsof(dim,dimt));
  dim+= z;dimt+= z;
  ovl= is_void(ovl)? z: ovl+z;
  offset= is_void(offset)? z: offset+z;
  save, obj, dim, dimt, ovl, offset;

  badid= -100;
  dimtmo= dimt-2*ovl;
  tilid= (dim-offset)/dimtmo+((dim-offset)%dimtmo>0);
  save, obj, badid, dimtmo, tilid;

  return obj;
}
func fillid (ti, &ovl)
{
  
}
func dumpid (ti, &ovl)
{

}
func fill (a, ti, t, badval=)
{
  
}
func dump (a, ti, t, badval=)
{
   
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

#if 1
nd= 3;
dimt= max(1,long(random(nd)*20));
offset= min(long(random(nd)*20),dimt-1);
dim= long(random(nd)*20)*dimt+offset;
a= random(_(nd,dim));

t= tile(dim,dimt);
mx= a(*)(mxx);
mxd= t.indim(mx,dim);
write,mx==t.dimin(mxd,dim),format="mxx = True? %d\n";

mx2= array(mx,2);
write,allof(mx==t.dimin(mxd,dim)),format="[mxx,mxx] = True? %d\n";
#endif
