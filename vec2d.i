func vec2dintsct (v11,v10,v21,v20,&v11d,&v22d)
/* DOCUMENT vec2dintsct (v11,v10,v21,v20)
   compute intersection point
   "1" is origin
   "0" is end
   v11=random(2); v10=random(2); v21=random(2); v20=random(2)
   v=vec2dintsct(v11,v10,v21,v20,v11d);
   v11d;
   fma;pldj,v10(2),v10(1),v11(2),v11(1);
       pldj,v20(2),v20(1),v21(2),v21(1),color="red";
       pldj,v(2),v(1),v11(2),v11(1),type=3;
       pldj,v(2),v(1),v10(2),v10(1),type=3;
       pldj,v(2),v(1),v21(2),v21(1),color="red",type=3;
       pldj,v(2),v(1),v20(2),v20(1),color="red",type=3;
 */
{
  d= dimsof(v11,v10,v21,v20);
  zo= array(0.0,d);

  v1= v10+zo-v11;
  v2= v20+zo-v21;
  vv= v11+zo-v21;

  d= (v1(::-1,)*v2)(dif,);
  wd= where(d==0.0);
  if (numberof(wd)) d(wd)= 1.0;
  v11d= (v2(::-1,)*vv)(dif,)/d;
  if (numberof(wd)) v11d(wd)= 1.e99;

  v22d= (v1(::-1,)*vv)(dif,)/d;
  if (numberof(wd)) v22d(wd)= 1.e99;

  return v11+v11d*v1;
}

#if 0

func poly2poly(p1, p2)
/*
 DOCUMENT: x = poly2poly(p1, p2)
 intersection of two 2d polygons p1 and p2.

 inputs:
   p1 and p2 are 2 X n_vertices
   they might or might not be wrapped around
 output:
   x is two-row array, each column is an intersecting point

 author: bruno luong <brunoluong@yahoo.com>
 history:
     original 20-may-2010
 */
{
  // wrap around: pad the first point to the end if necessary
  if (p1(1,1)!=p1(1,0)||p1(2,1)!=p1(2,1))
    p1 = _(p1,p1(:,1));
  if (p2(1,1)!=p2(1,0)||p2(2,1)!=p2(2,1))
    p2 = _(p2,p2(:,1));
  
  // swap p1 p2 so that we loop on a smaller one
  if (dimsof(p1)(3) > dimsof(p2)(3))
    restore,save(p1=p2,p2=p1);

  // increment the intermediate results by this amount
  increment = 10;
  // empty buffer
  x = [];
  filled = 0;
  sizec = 0;
  // loop over segments of p1
  for (n=2;n<=dimsof(p1)(3);n++) {
    cn = seg2poly(p1(:,n-1:n), p2);
    m = dimsof(cn)(3);
    filled = filled+m;
    // buffer too small
    if (sizec < filled) {
      sizec = filled+increment;
      x(2,sizec) = 0;
    }
    // store the result
    x(:,filled+(-m+1:0)) = cn;
  }
  // remove the tail
  x= x(:,filled+1:0);

  return x;
}

%
function x = seg2poly(s1, p)
 function x = seg2poly(s1, p)
 check if a line segment s1 intersects with a polygon p.
 inputs:
   s is (2 x 2) where
     s(:,1) is the first point
     s(:,2) is the the second point of the segment.
   p is (2 x n) array, each column is a vertices
 output
   x is (2 x m) array, each column is an intersecting point

   author: bruno luong <brunoluong@yahoo.com>
   history:
       original 20-may-2010

 translate so that first point is origin
a = s1(:,1);
m = bsxfun(@minus, p, a);
b = s1(:,2)-a;
 check if the points are on the left/right side
x = [b(2) -b(1)]*m;
sx = sign(x);
 x -coordinates has opposite signs
ind = sx(1:end-1).*sx(2:end) <= 0;
if any(ind)
    ind = find(ind);
    % cross point to the y-axis (along the segment)
    x1 = x(ind);
    x2 = x(ind+1);
    d = b.'/(b(1)^2+b(2)^2);
    y1 = d*m(:,ind);
    y2 = d*m(:,ind+1);
    dx = x2-x1;
    % we won't bother with the degenerate case of dx=0 and x1=0
    y = (y1.*x2-y2.*x1)./dx;
    % check if the cross point is inside the segment
    ind = y>=0 & y<1;
    if any(ind)
        x = bsxfun(@plus, a, b*y(ind));
    else
        x = zeros(2,0);
    end
else
    x = zeros(2,0);
end

end % seg2poly

#endif
