func inpolygon (ypol0, xpol0, yi, xi, noversamp=, tresh=, batchsz=)
/* DOCUMENT (ypol, xpol, yi, xi, noversamp=, tresh=, batchsz=)
   mask dimsof(xi,yi) indicating (==1) where
             (xi,yi) are inside the (x,y) REGULAR polygon.
   noveramp= boundary oversampling [10]
   batchsz= work chunking (depends on poly size) [1000]
   tresh= full circle deviations accepted

  USAGE:
  //create polygone (x,y)
  func foo(nr,np=) {
    if (is_void(nr))
    nr= 5000;
    np= (np? np: 14);
    phi= span(0,2*pi*(1-1./np),np);
    xpol= (1+0.2*sin(6*phi))*cos(phi);
    ypol= (1+0.2*sin(6*phi))*sin(phi);
    // random points (xi,yi)
    xi= 4*(random(nr)-0.5);
    yi= 4*(random(nr)-0.5);
    fma;plg,ypol,xpol;
    plg,yi,xi,type=0,marker=1,closed=1;
    msk= inpolygon( xpol, ypol, xi, yi);
    w= where(msk)
    plg,yi(w),xi(w),type=0,marker=1,color="red";
  }
 */
{
 local wrk;

 dout= dimsof(xi,yi);
 msk= zero= array(0,dout);
 nmsk= numberof(msk);

 if (is_void(noversamp)) noversamp=10;
 if (is_void(tresh)) tresh=0.01;
 if (is_void(batchsz)) batchsz=2000;

 // splinterpol8 polygone
 xpol= _(xpol0, xpol0(1));
 ypol= _(ypol0, ypol0(1));
 npol= numberof(xpol);
 if (noversamp>1) {
   ii= _(span(1.0, npol, long(noversamp-1+0.5)*npol),indgen(npol)); //corners in
   ii= ii(sort(ii));
   x= interp(xpol, indgen(npol), ii);
   y= interp(ypol, indgen(npol), ii);
 } else if (noversamp>0&noversamp<1) {
   nstep= nint(1/noversamp);
   x= _(xpol0(::nstep), xpol0(1));
   y= _(ypol0(::nstep), ypol0(1));
 } else if (noversamp==0||noversamp==1) {
   x= xpol;
   y= ypol;
 }
 np= noversamp*npol;
 npm= np-1;

 if (batchsz>0) {
   xpc= xpol(avg);
   ypc= ypol(avg);
   rad= abs(xpol-xpc,ypol-ypc);
   //radmin= rad(min);
   radmin= 0;
   radmax= rad(max);

   rats= abs((xi+zero)-xpc,(yi+zero)-ypc);
   w= where(rats<radmin);
   nw= numberof(w);
   if (nw) {
    msk(w)= 1;
    if (nw==nmsk) return msk;
   }
   wrk= where(rats>=radmin&rats<=radmax);
   nwrk= numberof(wrk);
   if (nwrk==0) return msk;
   for(ipt=1;ipt<=nwrk;ipt+=batchsz) {
     w= indgen(ipt:min([ipt+batchsz-1,nwrk]));
     msk(wrk(w))= inpolygon(y,x,(yi+zero)(wrk(w)),(xi+zero)(wrk(w)),\
              noversamp=0,tresh=tresh,batchsz=0)
   }
   return msk;
 } else if (batchsz==0) {
   x1= x(:-1)(,-)-(xi+zero)(*)(-,); // npm X nmsk
   y1= y(:-1)(,-)-(yi+zero)(*)(-,);
   x2= x(2:)(,-)-(xi+zero)(*)(-,);  // npm X nmsk
   y2= y(2:)(,-)-(yi+zero)(*)(-,);
 }

 dp= x2*x1 + y1*y2;         // dot-product
 cp= x1*y2 - y1*x2;         // cross-product

 return  abs(atan(cp,dp)(sum,)%2*pi) > tresh;
}

func segintersect (p0, p1, p2, p3, &p, noend=)
/* DOCUMENT m= segintersect(p0, p1, p1, p2, p3, &p)
   Returns 1 if the lines intersect, otherwise 0. In addition, if the lines
   intersect the intersection point may be stored in the floats i_x and i_y.
   If segments p0-p1 p2-p3 intersect at endpoints, intersect is accepted
   as valid.

   p0= random_n(2,n)*random(1,n);
   p1= random_n(2,n)*random(1,n);
   p2= random_n(2,n)*random(1,n);
   p3= random_n(2,n)*random(1,n);
   m= segintersect(p0,p1,p2,p3,p);
   fma;
   pldj,p0(1,),p0(2,),p1(1,),p1(2,);
   pldj,p2(1,),p2(2,),p3(1,),p3(2,),color="red";
   if(anyof(m)&&(w=where(m))(1))
     plg,p(2,w),p(1,w),type=0,color="blue",marker=2;
   SEE ALSO:
*/
{
  eq_nocopy, p0_x, p0(1,..);
  eq_nocopy, p0_y, p0(2,..);
  eq_nocopy, p1_x, p1(1,..);
  eq_nocopy, p1_y, p1(2,..);
  eq_nocopy, p2_x, p2(1,..);
  eq_nocopy, p2_y, p2(2,..);
  eq_nocopy, p3_x, p3(1,..);
  eq_nocopy, p3_y, p3(2,..);

  s1_x= p1_x-p0_x;
  s1_y= p1_y-p0_y;
  s2_x= p3_x-p2_x;
  s2_y= p3_y-p2_y;

  s= (-s1_y*(p0_x-p2_x)+s1_x*(p0_y-p2_y))/(-s2_x*s1_y+s1_x*s2_y);
  t= ( s2_x*(p0_y-p2_y)-s2_y*(p0_x-p2_x))/(-s2_x*s1_y+s1_x*s2_y);

  p= array(0.,dimsof(p1));
  if (noend==1)
    m= s>0 & s<1 & t>0 & t<1;
  else
    m= s>=0 & s<=1 & t>=0 & t<=1;
  if (anyof(m)) { // Collision detected
    w= where(m);
    p(1,w)= p0_x(w)+(t(w)*s1_x(w));
    p(2,w)= p0_y(w)+(t(w)* s1_y(w));
  }
  return m;
}

func polygonselfintersect (p, closed=)
/* DOCUMENT
   test if the polygon is "simple," ie. a flat shape consisting of straight,
   non-intersecting line segments.
   n=3;p= random_n(2,n);p=p(,cum);
   fma;plg,p(2,),p(1,),closed=1;
   polygonselfintersect(p);
   SEE ALSO:
 */
{
  n= numberof(p(1,));
  np= (closed==1? n: n+1);
  m= 0;
  for (i=0; i<np-1; i++)
     for (j=i+1; j<np-1; j++)
        m|= segintersect(p(,i+1),p(,(i+1)%n+1),                       \
                         p(,j+1),p(,(j+1)%n+1),noend=1);
  return m;

}
