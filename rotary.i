require, "random.i";

/*--------------------------------------------------------------------------------------------*/
/*-------------------------------- Cartesian Vector ------------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(norm, unit, scale, add, dot, _dotext, levi_civita, cross, _crossext);
save,tmp, prll, perp, rot, dist, to_sph, from_sph, _random, dims, ndx;

func cartvec (base, cart, .., rand=, unit=, sph=)
/* DOCUMENT ox= cartvec (cart, rand=, unit=, sph=)
   CARTVEC:  module, or collection of functions, manipulating arrays, with a leading
   dimension storing coordinates, as cartesian vectors. Number of dimensions is arbitrary.
   ALSO, construct cartesian coordinate arrays of randomly distributed vectors.

   -----------
   The constructor takes:
   -- a cartesian (or spherical, SPH==1) coordinate array CART of double(N,...),
   if spherical, leading dimension of length==3 AND assume (r, theta[0,pi], phi[0,2*pi])
   o= cartvec (array(double,3,d1,d2,...));
   -- a CARTVEC object, or
   o= cartvec (vec);
   -- rank and dimensions for generating random UID vectors in a sperical ball of radius 1.
   A rankdim array, or individual dimension lengths, with RAND==1.  Optional UNIT=1 for spherical shell.
   o= cartvec (3,5,7,rand=1,[unit=1]);
   o= cartvec ([3,3,5,7],rand=1,[unit=1])
   NOTES:
   ------
   (1) calling methods which returns a CARTVEC as a subroutine operates on coordinates in-place.
   (2) some binary vector methods (DOT, CROSS, ...) operates "one-on-one" on multidimensional arrays
   by default, this means that coordinate arrays must be conformable according to yorick. Upon
   request (ALL=1,) binary operators are applied "all-on-all" (as a matrix inner product)
   across each dimension. Variables returned will ALL=1 have dims which are concatenated
   input dims, in left-to-right operator order.
   (3) methods are coded to be callable by derived classes while using said subclasse as output type.
   methods:
   --------
   (all examples: v= cartvec(random(3,5)); w= cartvec(random(3,5,2)); //  broadcasting)
   (V is a CARTVEC instance, A is a real constant, possibly comformable to a single coordinate)
   NORM([]):      L2 norm -- example:: v(norm,);
   UNIT([]):      normalize the vector -- example:: v(unit,)(norm,);
   SCALE(A):      scalar multiplication -- example:: v(scale,pi)(norm,)/v(norm,);
   ADD(V,ALL=):   vector addition -- example:: v(add,w(scale,-1))(add,w)(add,v(scale,-1))(norm,);
   DOT(V,ALL=):   inner product -- example:: w(dot,v)-v(dot,w);
   CROSS(V,ALL=): vector product with supplied vector -- example:: w(cross,v)(add,v(cross,w))(norm,)
   PRLL(V,ALL=):  component parallel to supplied vector V, which is == V^(this.dot.V^)
   -- example::  w(add,v(perp,w))(prll,w)(add,w(scale,-1))(norm,);
   PERP(V,ALL=):  component perpendicular to supplied vector V,   which is == V^.cross.(this.cross.V^)
   -- example:: v(cross,w(perp,v))(add,w(cross,v))(norm,);
   ROT(V,A,ALL=): right-hand rotation about supplied vector, by supplied angle
   -- example:: v,rot,w,pi/3;
   DIST(V,ALL=):  distance to other vector --example:: v(dist,w,all=1)-transpose(w(dist,v,all=1),-1)
   DIMS([]);      returns rankdims of the array of vectors [rank,dim_1_length,dim_2_length, ...,dim_[rank]_length]
   NDX:           indexing;
   _RANDOM(n1,n2,..,UNIT=) random vector with uniform direction angle density.
   TO_SPH():      spherical coordinates (r,theta,phi) THETA: angle to Z [0,pi],  PHI: to X in XY [0,2*pi]
*/
{
  ob= base(:);

  // -- construct from random components in sphere or spherical shell --
  if (rand==1) {
    while (more_args())
      build_dimlist, cart, next_arg();
    if (is_void(cart))
      cart= [0];
    if (is_scalar(cart))
      cart= [1,cart];
    return ob(_random, cart, unit=unit);
  }
  // -- default construct is from coordinates --

  if (sph==1)
    cart= ob(from_sph,cart);

  // if (dimsof(cart)(2)!=3) error,"expecting three components";

  if (unit==1)
    cart/= sqrt(ob(norm,cart))(-,..);

  return cart;
}
func norm (cart)
{
  return sqrt((cart^2)(sum,..));
}
func scale (a, cart)
{
  if (am_subroutine())
    if (is_scalar(a))
      cart*= a;
    else
      cart*= a(-,..);
  return cart;
}
func unit (void)
{
  n= use_method(norm);

  if (am_subroutine())
    use_method,scale,1/n;
  else
    return use_method(scale,1/n);
}
func add (vec2,all=)
{
  local cart
    oo= use();

  cart= vec2(cart);
  if (all==1)
    for (i=2;i<=dimsof(oo(cart))(1);i++)
      cart= cart(,-,..);

  if (!am_subroutine()) oo= oo(:);
  save, oo, cart=cart+oo(cart);
  return oo;
}
func dot (vec2,all=)
{
  if (all==1)
    return use_method(_dotext,vec2);
  else
    return (use(cart)*vec2(cart))(sum,..);
}
func _dotext (vec2)
{
  return use(cart,+,..)*vec2(cart)(+,..);
}
func levi_civita (void)
{
  lc = array(double,3,3,3);
  lc(1,2,3)= lc(3,1,2)= lc(2,3,1)= 1;
  lc(3,2,1)= lc(2,1,3)= lc(1,3,2)=-1;
  return lc;
}
func cross (vec2,all=)
{
  local cart;
  if (dimsof(use(cart))(2)!=3) error,"expecting three components";

  if (all==1) {
    if (am_subroutine())
      use_method,_crossext,vec2;
    else
      return use_method(_crossext,vec2);
  }
  else
    {
      oo= use();
      cart= oo(cart);
      v2= vec2(cart);
      c = array(structof(cart),dimsof(cart,v2));
      c(1,..) = cart(2,..)*v2(3,..)-cart(3,..)*v2(2,..);
      c(2,..) =-cart(1,..)*v2(3,..)+cart(3,..)*v2(1,..);
      c(3,..) = cart(1,..)*v2(2,..)-cart(2,..)*v2(1,..);
      if (!am_subroutine()) oo= oo(:);
      save, oo, cart= c;
      return oo;
    }
}
func _crossext (vec2)
{
  local cart;
  oo= use();
  cart= oo(cart);
  if (dimsof(cart)(2)!=3) error,"expecting three components";

  v2= vec2(cart);

  lc= use_method(levi_civita);

  c= array(structof(cart),dimsof(cart,v2));

  c=  (lc(,+,)*cart(+,..))(,+,..)*v2(+,..);

  if (!am_subroutine()) oo= oo(:);
  save, oo, cart=c;

  return oo;
}
func prll (n,all=)
{
  use, cart;

  nu= n(unit,);
  a= use_method(dot,nu,all=all);

  if (am_subroutine())
    cart= nu(scale,a)(cart);
  else
    return nu(scale,a);
}
func perp (n,all=)
{
  // also
  //   u=n(unit,);
  //  return u(cross,use_method(cross,u));
  use, cart;

  vp= use_method(prll,n,all=all);
  vp,scale,-1;

  if (am_subroutine())
    cart = use_method(add,vp)(cart);
  else
    return use_method(add,vp);
}
func rot (w,q,all=)
{
  if (is_void(q))
    {
      q= w(norm,);
      w= w(scale,1/q);
    }
  cq= cos(q);
  sq= sin(q);
  vp= use_method(prll,w,all=all);
  vpp= use_method(perp,w,all=all);
  vpp2= w(cross,vpp);
  vpp,scale,cq;
  vpp2,scale,sq;
  vp,add,vpp;
  vp,add,vpp2;
  if (am_subroutine())
    {
      use,cart;
      cart= vp(cart);
    }
  else
    return vp;
}
func dist (vec2,all=)
{
  vp= use_method(add,vec2(scale,-1),all=all);
  return vp(norm,);
}
func _random (.., unit=)
{
  dimlist= next_arg();
  while (more_args()) build_dimlist, dimlist, next_arg();

  d3= _(dimlist(1)+1,3,(dimlist(1)>0? dimlist(2:): []));
  a= array(double,d3);

  t= acos(2*(random(dimlist)-0.5));
  p= random(dimlist)*2*pi;

  a(1,..)= sin(t)*cos(p);
  a(2,..)= sin(t)*sin(p);
  a(3,..)= cos(t);

  if (unit!=1)
    a*= random(dimlist)(-,..);

  // not sure how to generalize contruction to derived classes ... this might work
  o= use();
  save,o,cart=a;
  return o;
}
func dims (void)
{
  return dimsof(use(cart)(1,..));
}
func ndx (i)
{
  o= use();
  if (!am_subroutine()) o= o(:);
  save, o, cart= o(cart)(,i);
  return o;
}
func to_sph (void)
{
  use, cart;
  sph= array(0.0,dimsof(cart));
  r= use_method(norm);
  t= acos(cart(3,..)/r);
  sph(3,..)= atan(cart(2,..),cart(1,..));
  sph(2,..)= t;
  sph(1,..)= r;
  return sph;
}
func from_sph (sph)
{
  cart= array(0.0,dimsof(sph));
  s= sin(sph(2,..));
  cart(1,..)= sph(1,..)*s*cos(sph(3,..));
  cart(2,..)= sph(1,..)*s*sin(sph(3,..));
  cart(3,..)= sph(1,..)*cos(sph(2,..));
  return cart;
}
cartvec= closure(cartvec, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*---------------------------------- Quaternion ----------------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch,tmp);
tmp= save(norm,scale,unit,adj,inv,add,mult,_multext,rdiv,angle,axis,print,dot,_dotext);
save, tmp, power,quatvec,rot,dist,dims,ndx;

func quaternion (base,quat,a,b,..,unit=,rand=)
/* DOCUMENT quaternion (quat,a,b,..,unit=,rand=)
   quaternions objects for representing and manipulating rotations.
   use:
   ----
   q1= quaternion(cartvec([[1,0,0.0],[0,1,0],[0,0,1]]),[.2,.3,.4]); // broadcast vectors to angles
   q1, print;
   q3(mult,q2(mult,q1))(euler,[1,2,3])

   constructors:
   -------------
   DOUBLE ARRAY(4,..):            quaternion(array(double,4,...));
   UNIT CARTVEC, DOUBLE ANGLE: quaternion(cartvec(2,5,rand=1,unit=1),pi/3);
   CARTVEC (NORM==ANGLE):      quaternion(cartvec(2,5,rand=1)(scale,pi/3));
   DIMLIST, RAND=1:               quaternion(2,5,rand=1);  // -- uniform radius 2*pi ball
   COPY:                          quaternion(quaternion());

   methods:
   --------
   V is a CARTVEC instance, A is a real constant, possibly comformable to components
   NORM([]):      L2 norm -- example:: v(norm,);
   UNIT([]):      normalize the vector -- example:: v(unit,)(norm,);
   SCALE(A):      scalar multiplication -- example:: v(scale,pi)(norm,)/v(norm,);
   ADJ:           return the adjoint;
   INV:           return the inverse;
   ADD(V,ALL=):   vector addition -- example:: v(add,w(scale,-1))(add,w)(add,v(scale,-1))(norm,);
   DOT(V,ALL=):   inner product -- example:: w(dot,v)-v(dot,w);
   CROSS(V,ALL=): vector product with supplied vector -- example:: w(cross,v)(add,v(cross,w))(norm,)
   ROT(V):        rotate vector using quaternion;
   MULT:          the resulting rotation order in q1(mult,q2) is q2 FIRST then q1
                  v= cartvec([1,0,0]); q= quaternion(cartvec([[1,0,0],[0,1,0],[0,0,1]]),[pi,pi,pi]/2);
                  qq=q(ndx,2)(mult,q(ndx,1)(mult,q(ndx,3));  qq(rot,v)(cart);
   RDIV:
   POWER:
   QUATVEC:       makes a quaternion-multipliable vector, private use
   ANGLE:         return the rotation angle around AXIS;
   AXIS:          returns the axis of rotation, see ANGLE;
   PRINT(deg=):   print AXIS and ANGLE;
   DIST(V,ALL=):  distance to other quat --example:: v(dist,w,all=1)-transpose(w(dist,v,all=1),-1)
   RANDOM(n1,n2,..,UNIT=) random vector with uniform direction angle density.
*/
{
  local cart;

  ob= base(:);
  save,ob,type= "quaternion";

  // -- CONSTRUCT --
  // -- copy --
  if (is_obj(quat,"quat",1)==0 && quat(type)==ob(type) && is_void(a))
    {
      quat= quat(quat);
    }
  // -- from quaternion components as quat(4,..) --
  else if (is_numerical(quat) && rand!=1 && is_void(a) && dimsof(quat)(2)==4)
    {
      if (dimsof(quat)(2)!=4) error,"expecting four components";
    }
  // -- from unit vector and angle --
  else if (is_obj(quat,"cart",1)==0 && quat(type)=="cartvec" && is_real(a) && is_void(b))
    {
      uv= quat(unit,);
      cart= uv(cart);
      uv= [];
      d= dimsof(cart,a(-,..)); d(2)= 4;
      quat= array(double,d);
      quat(1,..)= cos(a/2);
      quat(2:,..)= sin(a(-,..)/2)*cart;
    }
  // -- from a vector whose norm is the rotation angle --
  else if (is_obj(quat,"cart",1)==0 && quat(type)=="cartvec" && is_void(a))
    {
      a= quat(norm,);
      uv= quat(unit,);
      return quaternion(uv,a); // -- constructor iteration
    }
  // -- random construction using cartvec and rotation angles --
  else if (rand==1)
    {
      local d;
      build_dimlist, d, quat;
      build_dimlist, d, a;
      build_dimlist, d, b;
      while (more_args()) build_dimlist, d, next_arg();
      v= cartvec(d,rand=1);
      v,scale,2*pi;
      return quaternion(v); // -- constructor iteration
    }

  save,ob,quat;

  return ob;
}
func norm (void)
{
  return abs(use(quat,1,..),use(quat,2,..),use(quat,3,..),use(quat,4,..));
}
func scale (a)
{
  oo= use();  // entire object, ok if not base.
  if (!am_subroutine()) oo= oo(:);
  save,oo,quat= oo(quat)*a(-,..);
  return oo;
}
func dot (q2,all=)
{
  if (all==1)
    return use_method(_dotext,q2);
  else
    return (use(quat)*q2(quat))(sum,..);
}
func _dotext (q2)
{
  return use(quat,+,..)*q2(quat)(+,..);
}
func unit (void)
{
  a= use_method(norm);
  if (am_subroutine())
    use_method,scale,1/a;
  else
    return use_method(scale,1/a);
}
func adj (void)
{
  a= [1,-1,-1,-1];
  oo= use();  // entire object, ok if not base.
  if (!am_subroutine()) oo= oo(:);
  save,oo,quat= oo(quat)*a;
  return oo;
}
func inv (void)
{
  n= use_method(norm);

  if (am_subroutine()) {
    use_method,adj;
    use_method,scale,1/n^2;
    return;
  } else {
    q= use_method(adj);
    q,scale,1/n^2;
    return q;
  }
}
func add (q2,all=)
{
  v2= q2(quat);

  if (all==1)
    for (i=2;i<=dimsof(use(quat))(1);i++)
      v2= v2(,-,..);

  oo= use();
  if (!am_subroutine()) oo= oo(:);
  save,oo,quat= oo(quat)+v2;
  return oo;
}
func mult (q2,all=)
{
  if (all==1) {
    if (am_subroutine())
      use_method,_multext,q2;
    else
      return use_method(_multext,q2);
  } else {
    q1= use(quat);
    q2= q2(quat);
    c = array(double,dimsof(q1,q2));
    c(1,..)= q1(1,..)*q2(1,..)-q1(2,..)*q2(2,..)-q1(3,..)*q2(3,..)-q1(4,..)*q2(4,..);
    c(2,..)= q1(1,..)*q2(2,..)+q1(2,..)*q2(1,..)+q1(3,..)*q2(4,..)-q1(4,..)*q2(3,..);
    c(3,..)= q1(1,..)*q2(3,..)+q1(3,..)*q2(1,..)+q1(4,..)*q2(2,..)-q1(2,..)*q2(4,..);
    c(4,..)= q1(1,..)*q2(4,..)+q1(4,..)*q2(1,..)+q1(2,..)*q2(3,..)-q1(3,..)*q2(2,..);
    oo= use();
    if (!am_subroutine()) oo= oo(:);
    save,oo,quat= c;
    return oo;
  }
}
func _multext (q2)
{
  q1= use(quat)
    q2= q2(quat);

  d= dimsof(q1(-,..),q2(-,..));
  d(2)= 4;
  c = array(double,d);

  c(1,1,..)=  q1(1,..);
  c(1,2,..)= -q1(2,..);
  c(1,3,..)= -q1(3,..);
  c(1,4,..)= -q1(4,..);
  c(2,1,..)=  q1(2,..);
  c(2,2,..)=  q1(1,..);
  c(2,3,..)= -q1(4,..);
  c(2,4,..)=  q1(3,..);
  c(3,1,..)=  q1(3,..);
  c(3,2,..)=  q1(4,..);
  c(3,3,..)=  q1(1,..);
  c(3,4,..)= -q1(2,..);
  c(4,1,..)=  q1(4,..);
  c(4,2,..)= -q1(3,..);
  c(4,3,..)=  q1(2,..);
  c(4,4,..)=  q1(1,..);

  c= c(,+,..)*q2(+,..);

  oo= use();
  if (!am_subroutine()) oo= oo(:);
  save,oo,quat= c;
  return oo;
}
func rdiv (q2)
{
  use_method,inv;

  if (am_subroutine()) {
    use_method,inv;
    use_method,mult,q2;
  } else {
    q1= use_method(inv);
    return q1(mult,q2);
  }
}
func angle (q2,all=)
{
  if (is_void(q2))
    return 2*acos(use(quat,1,..));

  return 2*acos(use_method(dot,q2,all=all));
}
func axis (void)
{
  eps= 1e-8;
  a= sin(use_method(angle,)/2)(-,..);
  m= abs(a)<=eps;

  vc= merge2([1.0,0,0],use(quat,2:4,..)/max(eps,a),m);
  return cartvec(vc);
}
func print (f,deg=)
{
  v= use_method(axis)(cart);
  a= use_method(angle);
  write,f,v(1,..),v(2,..),v(3,..),format="quaternion axis: %.8f %.8f %.8f\n";
  if (deg==1)
    write,f,a*180/pi,format="quaternion rotation angle [deg]: %.8f\n";
  else
    write,f,a,format="quaternion rotation angle [rad]: %.8f\n";
}
func power (n)
{
  if (am_subroutine()) {
    if (n<0)
      use_method,adj;
    qq= use()(:);
    for (i=2;i<=abs(n);i++)
      use_method,mult,qq;
  } else {
    if (n<0)
      v= use_method(adj);
    else
      v= use()(:);
    vv= v(:);
    for (i=2;i<=abs(n);i++)
      vv,mult,v;
    return vv;
  }
}
func quatvec (v)
{
  vc= v(cart);
  d= dimsof(vc); d(2)= 4;
  qv= array(double,d);
  qv(2:,..)= vc;
  return quaternion(qv);
}
func rot (v)
{
  q= use_method(unit);
  qv= q(mult,use_method(quatvec,v))(mult,q(adj,));
  o= v(:);
  save,o,cart=qv(quat)(2:,..);
  return o;
}
func dist (q2,all=)
{
  qp= use_method(add,q2(scale,-1),all=all);
  return qp(norm,);
}
func dims (void)
{
  return dimsof(use(quat)(1,..));
}
func ndx (i)
{
  o= use();
  if (!am_subroutine()) o= o(:);
  save, o, quat= o(quat)(,i);
  return o;
}
quaternion= closure(quaternion, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*------------------------------------- Rotations --------------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch,tmp);
tmp= save(euler,rotmat,quatdcm);

func rotation (base,quat,a,b,..,rand=)
/* DOCUMENT
 */
{
  local cart,quat;

  ob= quaternion();
  save, ob, [], base(:);
  save, ob, type= "rotation";

  // -- CONSTRUCTORS --
  // -- copy or, from quaternion--
  if (is_obj(quat,"quat",1)==0 && is_void(a))
    {
      quat= quat(quat);
    }
  // -- from quaternion components as quat(4,..) --
  else if (is_numerical(quat) && rand!=1 && is_void(a) && dimsof(quat)(2)==4)
    {
      if (dimsof(quat)(2)!=4) error,"expecting four components";
    }
  // -- from DCM or direction cosine/rotation matrix as quat(3,3,..) --
  else if (is_numerical(quat) && rand!=1 && is_void(a) && dimsof(quat)(2)==3 \
           && (dimsof(quat)(1)>1 && dimsof(quat)(3)==3))
    {
      return ob(quatdcm,quat);
    }
  // -- from unit vector and angle --
  else if (is_obj(quat,"cart",1)==0 && quat(type)=="cartvec" && is_real(a) && is_void(b))
    {
      uv= quat(unit,);
      cart= uv(cart);
      uv= [];
      d= dimsof(cart,a(-,..)); d(2)= 4;
      quat= array(double,d);
      quat(1,..)= cos(a/2);
      quat(2:,..)= sin(a(-,..)/2)*cart;
    }
  // -- from a vector whose norm is the rotation angle --
  else if (is_obj(quat,"cart",1)==0 && quat(type)=="cartvec" && is_void(a))
    {
      a= quat(norm,);
      uv= quat(unit,);
      return rotation(uv,a); // -- constructor iteration
    }
  // -- from euler angles (quat) and their axes (a)
  else if (is_real(quat) && is_integer(a) && \
           numberof(quat(,1))==(n=numberof(a(,1))) && is_void(b))
    {
      q= rotation(cartvec(histogram(a(n,..),top=3)),quat(n,..));
      for (i=n-1;i>0;i--)
        q,mult,rotation(cartvec(histogram(a(i,..),top=3)),quat(i,..));
      return rotation(q);
    }
  // -- random construction using cartvec and rotation angles --
  else if (rand==1)
    {
      local d;
      build_dimlist, d, quat;
      build_dimlist, d, a;
      build_dimlist, d, b;
      while (more_args()) build_dimlist, d, next_arg();
      v= cartvec(d,rand=1);
      v,scale,2*pi;
      return rotation(v); // -- constructor iteration
    }
  else
    error,"unrecognized quaternion construction";

  save,ob,quat;

  return ob;
}
func euler (axes)
{
  if (is_void(axes)) error,"axes -- [1,3,2],... -- required";

  use,quat;

  q1= quat(1,..);
  q2= quat(2,..);
  q3= quat(3,..);
  q4= quat(4,..);

  od= dimsof(quat);
  od(2)= 3;

  eul= array(double,od);

  // -- 121
  if (allof(axes==[1,2,1]))
    {
      eul(1,..)= atan((q2*q3-q4*q1),(q2*q4+q3*q1));
      eul(2,..)= acos(q1^2+q2^2-q3^2-q4^2);
      eul(3,..)= atan((q2*q3+q4*q1),(q3*q1-q2*q4));
    }
  // -- 123
  else if (allof(axes==[1,2,3]))
    {
      eul(1,..)= atan(2*(q2*q1+q4*q3),(q1^2-q2^2-q3^2+q4^2));
      eul(2,..)= asin(2*(q3*q1-q2*q4));
      eul(3,..)= atan(2*(q2*q3+q4*q1),(q1^2+q2^2-q3^2-q4^2));
    }
  // -- 131
  else if (allof(axes==[1,3,1]))
    {
      eul(1,..)= atan((q2*q4+q3*q1),(q4*q1-q2*q3));
      eul(2,..)= acos(q1^2+q2^2-q3^2-q4^2);
      eul(3,..)= atan((q2*q4-q3*q1),(q2*q3+q4*q1));
    }
  // -- 132
  else if (allof(axes==[1,3,2]))
    {
      eul(1,..)= atan(2*(q2*q1-q4*q3),(q1^2-q2^2+q3^2-q4^2));
      eul(2,..)= asin(2*(q2*q3+q4*q1));
      eul(3,..)= atan(2*(q3*q1-q2*q4),(q1^2+q2^2-q3^2-q4^2));
    }
  // -- 212
  else if (allof(axes==[2,1,2]))
    {
      eul(1,..)= atan((q2*q3+q4*q1),(q2*q1-q3*q4));
      eul(2,..)= acos(q1^2-q2^2+q3^2-q4^2);
      eul(3,..)= atan((q2*q3-q4*q1),(q2*q1+q3*q4));
    }
  // -- 213
  else if (allof(axes==[2,1,3]))
    {
      eul(1,..)= atan(2*(q3*q1-q4*q2),(q1^2-q2^2-q3^2+q4^2));
      eul(2,..)= asin(2*(q2*q1+q3*q4));
      eul(3,..)= atan(2*(q4*q1-q2*q3),(q1^2-q2^2+q3^2-q4^2));
    }
  // -- 231
  else if (allof(axes==[2,3,1]))
    {
      eul(1,..)= atan(2*(q2*q4+q3*q1),(q1^2+q2^2-q3^2-q4^2));
      eul(2,..)= asin(2*(q4*q1-q2*q3));
      eul(3,..)= atan(2*(q2*q1+q3*q4),(q1^2-q2^2+q3^2-q4^2));
    }
  // -- 232
  else if (allof(axes==[2,3,2]))
    {
      eul(1,..)= atan((q3*q4-q2*q1),(q2*q3+q4*q1));
      eul(2,..)= acos(q1^2-q2^2+q3^2-q4^2);
      eul(3,..)= atan((q2*q1+q3*q4),(q4*q1-q2*q3));
    }
  // -- 312
  else if (allof(axes==[3,1,2]))
    {
      eul(1,..)= atan(2*(q2*q3+q4*q1),(q1^2-q2^2+q3^2-q4^2));
      eul(2,..)= asin(2*(q2*q1-q3*q4));
      eul(3,..)= atan(2*(q2*q4+q3*q1),(q1^2-q2^2-q3^2+q4^2));
    }
  // -- 313
  else if (allof(axes==[3,1,3]))
    {
      eul(1,..)= atan((q2*q4-q3*q1),(q2*q1+q3*q4));
      eul(2,..)= acos(q1^2-q2^2-q3^2+q4^2);
      eul(3,..)= atan((q2*q4+q3*q1),(q2*q1-q3*q4));
    }
  // -- 321
  else if (allof(axes==[3,2,1]))
    {
      eul(1,..)= atan(2*(q4*q1-q2*q3),(q1^2+q2^2-q3^2-q4^2));
      eul(2,..)= asin(2*(q2*q4+q3*q1));
      eul(3,..)= atan(2*(q2*q1-q3*q4),(q1^2-q2^2-q3^2+q4^2));
    }
  // -- 323
  else if (allof(axes==[3,2,3]))
    {
      eul(1,..)= atan((q2*q1+q3*q4),(q3*q1-q2*q4));
      eul(2,..)= acos(q1^2-q2^2-q3^2+q4^2);
      eul(3,..)= atan((q3*q4-q2*q1),(q2*q4+q3*q1));
    }

  eul%= 2*pi;

  return eul;
}
func rotmat (void,inv=)
// direction cosine matrix (DCM) from quaternion member
{
  d= use_method(dims);
  v1= cartvec(array([[1,0,0],[0,1,0],[0,0,1]],d));
  if (inv==1)
    {
      q= use_method(adj);
      q= quaternion(q(quat)(,-,..));
    }
  else
    {
      q= quaternion(use(quat)(,-,..));
    }
  v= q(rot,v1);
  return v(cart);
}
func quatdcm (rot)
// quaternion from DCM/rotation matrix
{
  d= dimsof(rot(1,..));
  d(2)= 4;
  quat= array(double,d)

    m= rot(sum,sum,..)==0.0;
  if (anyof(m))
    quat(,,where(m))= 0.0;

  if (anyof(!m))
    {
      w= where(!m);
      quat(1,w)= 0.5*sqrt(max(0.0,rot(1,1,w)+rot(2,2,w)+rot(3,3,w)+1));
      mm= quat(1,w)==0.0;
      if (anyof(mm))
        {
          ww= w(where(mm));
          quat(2,ww)= sqrt(max(0.0,-0.5*(rot(2,2,ww)+rot(3,3,ww))))*sign(-rot(2,3,ww));
          quat(3,ww)= sqrt(max(0.0,-0.5*(rot(1,1,ww)+rot(3,3,ww))))*sign(-rot(1,3,ww));
          quat(4,ww)= sqrt(max(0.0,-0.5*(rot(1,1,ww)+rot(2,2,ww))))*sign(-rot(1,2,ww));
        }
      if (anyof(!mm))
        {
          ww= w(where(!mm));
          quat(2,ww)= 0.25*(rot(3,2,ww)-rot(2,3,ww))/quat(1,ww);
          quat(3,ww)= 0.25*(rot(1,3,ww)-rot(3,1,ww))/quat(1,ww);
          quat(4,ww)= 0.25*(rot(2,1,ww)-rot(1,2,ww))/quat(1,ww);
        }
    }
  oo= use()(:);  // in case context object is an object derived from parent rotation
  save, oo, quat;
  return oo;
}
rotation= closure(rotation, restore(tmp));
restore, scratch;

/*------------------ tests --------------------*/

#if 0
func print1 (s)
{
  ss= strchar(array(char(0x20),max(1,lmax -strlen(s))));
  write,f,format=s+ss+"mean: %8.2g; rms: %8.2g\n",n(*)(avg),n(*)(rms);
}
func checkVectorCart (f,N,M)
{
  if (is_void(N)) N= 10;
  if (is_void(M)) M= 5;

  v= cartvec(N,rand=1,unit=1);
  w= cartvec(N,M,rand=1,unit=1);

  lmax= 54;

  write,f,"cartvec test:",format="%s\n";
  write,f,"===============:",format="%s\n";

  n= w(norm,)-1;
  print1,"norm of unit random vector";

  w,unit,;
  n= w(norm,)-1;
  print1,"in place unit vector norm";

  ws= w(scale,(x=random(w(dims,))));
  n= ws(norm,)-x;
  print1,"scaled unit vector norm check";

  vv= v(add,w)(add,ws)(add,ws(scale,-1))(add,w(scale,-1));
  n= vv(dist,v);
  print1,"vector addition check";

  n= w(dot,ws)-x;
  print1,"dot product of parallel v's and scale check";

  ws, _random, N, M;
  vv= v(cross,w)(cross,ws); // -- (v x w) x ws = -v(ws.w) + w(ws.w)
  ww= v(scale,-ws(dot,w))(add,w(scale,ws(dot,v)));
  n= ww(dist,vv);
  print1,"vector triple product ax(bxc)= b(a.c)-c(a.b) check";

  vv= v(cross,w)
    n= vv(norm,)^2-v(norm,)^2*w(norm,)^2+v(dot,w)^2;
  print1,"lagrange identity |axb|^2=a^2b^2-a.b^2  check";

  vv= v(cross,w)
    vv,prll,w;
  n= vv(norm,);
  print1,"parallel part check";

  vv= w(unit,)(cross,v(cross,w(unit,)))
    ww= v(perp,w);
  n= vv(dist,ww);
  print1,"perpendicular part check";

  w= cartvec(N,M,rand=1)
    w, scale, 2*pi;
  n= v(rot,w)(perp,w)(unit,)(dot,v(perp,w)(unit,))-cos(w(norm,));
  print1,"rotation check";

  w= cartvec(N,M,rand=1)
    v= cartvec(w(to_sph,),sph=1);
  n= w(dist,v);
  print1,"to spherical coordinates and back";

  write,f,"",format="%s\n";
}
func checkQuaternion (f,N,M)
{
  if (is_void(N)) N= 10;
  if (is_void(M)) M= 5;

  v= quaternion(N,rand=1);
  w= quaternion(N,M,rand=1);

  lmax= 54;

  write,f,"quaternion test:",format="%s\n";
  write,f,"===============:",format="%s\n";

  n= w(norm,)-1;
  print1,"norm of quaternion";

  n= w(inv,)(mult,w)(dist,quaternion([1,0,0,0]));
  print1,"inverse of quaternion";

  n= w(mult,v)(adj,)(dist,v(adj,)(mult,w(adj,)));
  print1,"adjoints and multiplication";

  vw= cartvec(w(quat,2:,..));
  aw= w(quat,1,..);
  vv= cartvec(v(quat,2:,..));
  av= v(quat,1,..);
  wv= w(mult,v);
  vwv= cartvec(wv(quat,2:,..));
  n= wv(quat,1,..)-aw*av+vw(dot,vv);
  n= _(n,vw(cross,vv)(add,vw(scale,av))(add,vv(scale,aw))(dist,vwv));
  print1,"multiplication check: algebraic vs. vector";

  v= cartvec(N,M,rand=1,unit=1);
  a= random(N)*2*pi;
  q= quaternion(v,a)
    n= q(angle,)-a;
  n= _(n,q(axis,)(dist,v));
  print1,"(vector,angle) [de-]construstion";

  v= cartvec(N,M,rand=1);
  v,scale,2*pi;
  a= v(norm,);
  q= quaternion(v)
    n= q(angle,)-a;
  n= _(n,q(axis,)(dist,v(unit,)));
  print1,"Rodriques vector [de-]construstion";

  q= cartvec(N,rand=1);
  w1= cartvec(N,M,rand=1);
  w1, scale, 2*pi;
  ww1= quaternion(w1);

  qq= q(rot,w1);
  qqq= ww1(rot,q);
  n= qq(dist,qqq);
  print1,"vector vs. quaternion rotation";

  w2= cartvec(N,M,rand=1);
  w2, scale, 2*pi;
  ww2= quaternion(w2);

  qq= q(rot,w1)(rot,w2);
  ww= ww2(mult,ww1);
  qqq= ww(rot,q);
  n= qq(dist,qqq);
  w2= ww2= qqq= qq= [];
  print1,"vector vs. quaternion rotation sequence";

  pow= 6;
  w1= cartvec(N,M,rand=1);
  w1, scale, 2*pi;
  ww1= quaternion(w1);
  qq= ww1(rot,q);
  w1, scale, 1.0/pow;
  ww1= quaternion(w1);
  qqq= ww1(power,pow)(rot,q);
  n= qq(dist,qqq);
  print1,pr1(pow)+"-th power rotation sequence";

  write,f,"",format="%s\n";
}
func checkRotation (f,N,M)
{
  if (is_void(N)) N= 10;
  if (is_void(M)) M= 5;

  lmax= 54;

  write,f,"rotation test:",format="%s\n";
  write,f,"=============:",format="%s\n";

  v= quaternion(N,rand=1);
  w= quaternion(N,M,rand=1);

  q= cartvec(N,rand=1);
  w1= cartvec(N,M,rand=1);
  w1, scale, 2*pi;
  ww1= rotation(w1);

  qq= ww1(rot,q);
  m= ww1(rotmat,);
  qqq= cartvec((m*q(cart)(-,..))(,sum,..));
  n= qq(dist,qqq);
  print1,"rotation vs. dcm rotation";

  v= cartvec(N,M,rand=1);
  q= rotation(N,M,rand=1);
  qq= rotation(q(rotmat,));
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to dcm rotation and back (degen. pb!)";

  ie= [1,2,3];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

  ie= [3,1,3];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

  ie= [3,2,1];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

  ie= [3,2,3];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

  ie= [2,3,2];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

  ie= [1,3,1];
  a= q(euler,ie);
  qq= rotation(a,ie);
  n= q(rot,v)(dist,qq(rot,v));
  print1,"rotation to euler and back: axes "+pr1(ie);

}
checkVectorCart,;
checkQuaternion,;
checkRotation,;
#endif
