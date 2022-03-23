require, "rotation.i";
require, "zroots.i";

/*----------------------------------------------------------------------*/
/*
SPHERICAL: (data members || methods)
veclatlonh (
  latlonh,peg ||
  to_xyz,to_pxyz,to_sch )
vecxyz (
  peg,cart ||
  to_latlonh,to_pxyz,to_sch,to_XYZ)
vecpxyz (
  cart,peg ||
  to_xyz,to_sch,to_latlonh )

ELLIPSOID;
ellipsoid (
  majax,eccentsq1,minax,flatn,eccentsq2 ||
  eastradcurv,northradcurv,locradcurv,print )
pegpoint (
  elp,lat,lon,hdg,ad,eastrad,northrad,rotXYZxyz,XYZOff,schOff ||
  print )
vecXYZ (
  cart ||
  to_llh,to_xyz,to_pxyz,to_sch )
vecllh (
  llh,elp ||
  to_XYZ,to_xyz,to_pxyz,to_sch )
vecsch
 */
/*----------------------------------------------------------------------*/

RADEG= pi/180.;
DEGRA= 1/RADEG;

MAJORAXIS= 6378137.0;            // -- WGS-84 Ellipsoid Semi-major Axis --  == a == Equatorial Radius
FIRSTECCENTR2= 6.6943799901e-3;  // -- WGS-84 Ellipsoid First Eccentricity Squared (a^2-b^2)/a^2 -- b == Polar Radius

/*--------------------------------------------------------------------------------------------*/
/*-------------------------------------- SPHERICAL -------------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------------*/
/*----------------------------------- latlonh vector -----------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(to_xyz,to_pxyz,to_sch,to_XYZ,_random);

func veclatlonh (base,latlonh,peg,..,rand=)
/* DOCUMENT ox= veclatlonh (peg,latlonh)
   spherical lat/lon/height vector object constructor [height is perp. above sphere of radius RE]
  ! DO NOT confuse with ELLIPTICAL llh or XYZ

  construct:
  ==========
  veclatlonh(veclatlonh(..));   // copy
  veclatlonh(latlonh(1:3,..),peg);  // from array(3,..) [[lat,lon,height],..]
  veclatlonh(peg,6,5,rand=1);       // random uniform density ball of unit radius
  veclatlonh(peg,[2,6,5],rand=1);   //    "    "    "   "

   LATLONH(1:3,..) :
     1: lat -- theta or ~latitude in (+/-) pi/2
     2: lon -- phi or ~longitude  in (+/-) pi
     3: hgt -- height above RE

   PEG is an pegpoint object (see bellow).

   If PEG is at xyz=[peg(rad),0,0] in SCH sphere, with hat(S) in sphere equator,
   lat == c/radius
   lon == s/radius, hat(S) = hat(LON)
   hgt == height above sphere (meters)

   methods:
   ========
   to_xyz -- convert spherical coordinates into peg point xyz vector object
   to_pxyz -- convert into peg point pxyz vector object
   to_sch -- convert into SCH vector object
   to_XYZ -- convert into XYZ vector object

   ATTN: peg xyz system is different from pxyz.
   XYZ / PXYZ are just PERMUTATION and TRANSLATION of the
   usual cartesian xyz attached with latlonh (xy is latitude=0 plane,...)

   XYZ(1:3,..)
   xyz of spherical system with theta/latitude measured from (x,y) and thus in [-pi/2,pi/2].
   Another convention, not used here, is theta mesured from z axis (then theta in [0,pi].)
   1: x axis is peg zenith, and height=peg radius on sch sphere
   2: y axis parallel to tangent to track at peg.
   3: z axis is cross track left direction.
   PXYZ(1:3,..) peg point (pp) [peg(rad),0,0] xyz vector (meters)
   1: px  tangent to track at pp, and along track  (==y)
   2: py  cross track (left)                       (==z)
   3: pz  zenith at pp                  Translated (==x)
 */
{
  ob= base(:);

  save, ob, type= "veclatlonh";

  if (allof(is_obj(latlonh,["latlonh","peg"],1))==0 && latlonh(type)==ob(type))
  {
    peg= latlonh(peg);
    latlonh= latlonh(latlonh);
  }
  // -- construct from random components in sphere or spherical shell --
  if (rand==1) {
    latlonh0= latlonh;
    while (more_args()) build_dimlist, peg, next_arg();
    if (is_void(peg)) peg= [0];
    if (is_scalar(peg)) peg= [1,peg];
    latlonh= ob(_random,peg);
    peg= latlonh0;
  }
  else
  {
    if (!is_void(latlonh) &&
         dimsof(latlonh)(2)!=3) error,"expecting leading dim==3";
  }

  if (is_void(peg)) error,"peg void.";

  save,ob,latlonh,peg;

  return ob;
}
func to_xyz (void)
{
  xyz= array(double, dimsof(use(latlonh)));

  h= use(latlonh,3,..) + use(peg,rad);

  coslat= cos(use(latlonh,1,..));

  xyz(1,..)= h * coslat * cos(use(latlonh,2,..));
  xyz(2,..)= h * coslat * sin(use(latlonh,2,..));
  xyz(3,..)= h * sin(use(latlonh,1,..));

  return vecxyz(xyz,use(peg));
}
func to_pxyz (void)
{
  use, latlonh;

  pxyz= array(double, dimsof(latlonh));

  h= latlonh(3,..)+use(peg,rad);

  coslat= cos(latlonh(1,..));

  pxyz(3,..)= h*coslat*cos(latlonh(2,..))-use(peg,rad);
  pxyz(1,..)= h*coslat*sin(latlonh(2,..));
  pxyz(2,..)= h*sin(latlonh(1,..));

  return vecpxyz(pxyz,use(peg));
}
func to_sch (void)
{
  out= use(latlonh);           // lon>s lat>c h-h
  re= use(peg,rad);

  return vecsch(out([2,1,3],..)*[re,re,1],use(peg));
}
func to_XYZ(void)
{
  xyz= use_method(to_xyz);
  return xyz(to_XYZ,);
}
func _random (..)
{
  dimlist= next_arg();
  while (more_args()) build_dimlist, dimlist, next_arg();

  d3= _(dimlist(1)+1,3,(dimlist(1)>0? dimlist(2:): []));
  a= array(double,d3);

  a(1,..)= asin(2*(random(dimlist)-0.5));
  a(2,..)= random(dimlist)*2*pi;
  a(3,..)= random(dimlist);

  return a;
}
veclatlonh= closure(veclatlonh, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*--------------------------------------- xyz vector -----------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(to_latlonh,to_pxyz,to_sch,to_XYZ);

func vecxyz (base,cart,peg,..,unit=,rand=)
/* DOCUMENT ox= vecxyz (cart,peg)
  vector in peg point sphere in coordinate system xyz.
  This is nothing more than a cartesian vector object VECCART with an
  added radius of curvature member (that of the peg) RE.

  VEC(1:3,..) peg point xyz vector (meters)
      1: x    zenith, with origin local sch sphere radius from geodetic center
      2: y    // to tangent to track @ geodetic center (sch sphere center)
      3: z    cross track (left)

  method TO_LATLONH converts peg point xyz coordinates
  into lon,lat,height coordinates in that sch sphere.
  Uses peg radius of curvature only

  peg(rad)    ellispoid semi-major axis

  to_pxyz
  converts cartesian xyz into "pxyz" peg point (pp) pxyz where peg is at xyz=[re,0,0].
  pxyz's origin is at the pp, and
  x<->pz  (also translated pz=x-re)
  y<->px
  z<->py
  cartesian coordinates. These are just PERMUTATION and TRANSLATION of the
  usual cartesian xyz attached with latlonh (xy is latitude=0 plane,...)

  pxyz(1:3,..) peg point (pp) [peg(rad),0,0] xyz vector (meters)
   1: px  tangent to track at pp, and along track  (==y)
   2: py  cross track (left)                       (==z)
   3: pz  zenith at pp                  Translated (==x)

  to_sch
  peg point xyz coordinates into spherical sch coordinates
  ATTN: peg xyz system is different from pxyz.
  latlonh(1:3,..) :
     1: lat -- theta or ~latitude in (+/-) pi/2
     2: lon -- phi or ~longitude  in (+/-) pi
     3: hgt -- height above RE
  xyz=random(3,40000)*1e7;
  max(vecxyz(xyz,6e6)(to_latlonh,)(to_xyz,)(cart)-xyz)

 */
{
  ob= vectorCart();
  save, ob, [], base(:);
  save, ob, type= "vecxyz";

  if (is_obj(cart,"cart",1)==0) {
    cart= cart(cart);
  }
  // -- construct from random components in sphere or spherical shell --
  if (rand==1) {
    cart0= cart;
    while (more_args()) build_dimlist, peg, next_arg();
    if (is_void(peg)) peg= [0];
    if (is_scalar(peg)) peg= [1,peg];
    cart= ob(_random,peg,unit=unit)(cart);
    peg= cart0;
  }
  else
  {
    if (!is_void(cart) &&
         dimsof(cart)(2)!=3) error,"expecting leading dim==3";
  }

  if (is_void(peg)) error,"peg void.";

  save,ob,peg,cart;

  return ob;
}
func to_latlonh (void)
{
  latlonh= array(double, dimsof(use(cart)));

  r_p= sqrt((use(cart)^2)(sum,..));

  latlonh(1,..)= asin(use(cart,3,..)/r_p);
  latlonh(2,..)= atan(use(cart,2,..),use(cart,1,..));
  latlonh(3,..)= r_p - use(peg,rad);

  return veclatlonh(latlonh,use(peg));
}
func to_pxyz (void)
{
  return vecpxyz(use(cart,[2,3,1],..)-[0,0,use(peg,rad)],use(peg));
}
func to_sch (void)
{
  out= use_method(to_latlonh)(latlonh);
  re= use(peg,rad);

  return vecsch(out([2,1,3],..)*[re,re,1],use(peg));
}
func to_XYZ (void)
{
  XYZxyz= use(peg,rotXYZxyz);
  return vecXYZ(XYZxyz(rot,use())(add,use(peg,XYZOff)));
}
vecxyz= closure(vecxyz, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*-------------------------------------- pxyz vector -----------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(to_xyz,to_sch,to_latlonh,to_XYZ);

func vecpxyz (base,cart,peg,..,rand=,unit=)
/* DOCUMENT
  to_latlonh (void)
  peg point pxyz coor. conversion into  lon,lat,height coordinates in
  the sch sphere.
  latlonh(1:3,..)
  1: lat       latitude (deg -90 to 90)
  2: lon       longitude (deg -180 to 180)
  3: hgt       height above sphere (meters)

  to_xyz
  px->y  py->z  pz->x

  to_sch(pxyz, peg)
  peg point pxyz coor.  into spherical sch
  provide peg or only radius of curvature (peg.rad)

  RADIUS         ellispoid semi-major axis
  pxyz(1:3,..) peg point xyz vector (meters)
  latlonh(1:3,..) sic:
  1: lon       longitude (deg -180 to 180)
  2: lat       latitude (deg -90 to 90)
  3: hgt       height above ellipsoid (meters)

 */
{
  ob= vectorCart();
  save, ob, [], base(:);
  save, ob, type= "vecpxyz";

  if (is_obj(cart,"cart",1)==0) {
    cart= cart(cart);
  }
  // -- construct from random components in sphere or spherical shell --
  if (rand==1) {
    cart0= cart;
    while (more_args()) build_dimlist, peg, next_arg();
    if (is_void(peg)) peg= [0];
    if (is_scalar(peg)) peg= [1,peg];
    cart= ob(_random,peg,unit=unit)(cart);
    peg= cart0;
  }
  else
  {
    if (!is_void(cart))
      if (dimsof(cart)(2)!=3) error,"expecting leading dim==3";
  }

  if (is_void(peg)) error,"peg void.";

  save,ob,cart,peg;

  return ob;
}
func to_xyz (void)
{
  return vecxyz(use(cart,[3,1,2],..)+[use(peg,rad),0,0],use(peg));
}
func to_latlonh (void)
{
  latlonh= use(cart);

  latlonh(3,..) += use(peg,rad);

  r_p= sqrt((latlonh^2)(sum,..));

  latlonh(1,..)= asin(use(cart,2,..)/r_p);
  latlonh(2,..)= atan(use(cart,1,..),latlonh(3,..));
  latlonh(3,..)= r_p - use(peg,rad);

  return veclatlonh(latlonh,use(peg));
}
func to_sch (void)
{
  out= use_method(to_latlonh)(latlonh);
  re= use(peg,rad);

  return vecsch(out([2,1,3],..)*[re,re,1],use(peg));
}
func to_XYZ(void)
{
  xyz= use_method(to_xyz);
  return xyz(to_XYZ,);
}
vecpxyz= closure(vecpxyz, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*-----------------------------------------  Ellispoid ---------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch,tmp);
tmp= save(eastradcurv,northradcurv,locradcurv,print);

func ellipsoid (base,majax,eccentsq1,elp=,wgs84=,planet=)
/* DOCUMENT ox= ellipsoid (majax,eccentsq1,elp=,wgs84=,planet=)
   ellipsoid opbject: provide elp=save(majax=,eccentsq1=) or as keywords
     double majax;     //  Semi-minor Axis == Equatorial Radius
     double eccentsq1; //  First Eccentricity Squared (a^2-b^2)/a^2 -- b == Polar Radius
     double minax;     //  Semi-minor Axis
     double flatn;     //  flattening
     double eccentsq2; //  Second Eccentricity Squared (a^2-b^2)/b^2
 */
{
  ob= base(:);

  save, ob, type= "ellipsoid";

  if (allof(is_obj(elp,["majax","eccentsq1"],1))==0 && \
    elp(type)==ob(type))
  {
    majax= elp(majax);                     // -- Ellipsoid Semi-minor Axis == Equatorial Radius
    eccentsq1= elp(eccentsq1);             // -- Ellipsoid First Eccentricity Squared (a^2-b^2)/a^2 -- b == Polar Radius
  }
  else if (wgs84==1)
  {
    majax= MAJORAXIS;
    eccentsq1= FIRSTECCENTR2;
  }
  else if (is_string(planet))
  {
    pln= save();
    pln,mercury= [2439.7,2439.7]*1e3;
    pln,venus= [6051.8,6051.8]*1e3;
    pln,earth= [MAJORAXIS, MAJORAXIS*sqrt(1-FIRSTECCENTR2)];
    pln,mars= [3397.62,3379.3845]*1e3;
    pln,jupiter= [71492.0,67136.5562]*1e3;
    pln,saturn= [60268.0,54890.7686]*1e3;
    pln,uranus= [25559.0,24986.1354]*1e3;
    pln,neptune= [24764.0,24347.6551]*1e3;
    pln,pluto= [1195.0,1195.0]*1e3;
    majax= pln(planet,1);
    eccentsq1= 1-(pln(planet,2)/majax)^2;
  }
  else
  {
    if (is_void(majax)||is_void(eccentsq1)) error,"majax and eccentsq1 args required";
  }
  majax= double(majax);
  eccentsq1= double(eccentsq1);

  minax= majax*sqrt(1-eccentsq1);          // -- Ellipsoid Semi-minor Axis
  flatn= (majax-minax)/majax;              // -- Ellipsoid flattening
  eccentsq2= eccentsq1/(1-eccentsq1);      // -- Ellipsoid Second Eccentricity Squared (a^2-b^2)/b^2

  save,ob,majax,eccentsq1,minax,flatn,eccentsq2;

  return ob;
}
func eastradcurv (lat)
/* DOCUMENT:  eastradcurv(lat)
 */
{
 return use(majax)/sqrt(1-use(eccentsq1)*sin(lat)^2);
}
func northradcurv (lat)
/* DOCUMENT: northradcurv(lat)
 */
{
  return use(majax)*(1-use(eccentsq1)) / \
         (1-use(eccentsq1)*sin(lat)^2)^(1.5);
}
func locradcurv (lat,hdg)
/* DOCUMENT: locradcurv(hdg,lat)
 */
{
  return use_method(eastradcurv,lat)*use_method(northradcurv,lat) /\
         (use_method(eastradcurv,lat)*cos(hdg)^2 +\
          use_method(northradcurv,lat)*sin(hdg)^2);
}
func  print (f)
{
  write,f,"ellipsoid parameters:",format="%s\n";
  write,f,use(majax),    format="  semi-minor axis or equatorial radius (a): %.10lg\n";
  write,f,use(minax),    format="  semi-minor axis or polar radius (b): %.10lg\n";
  write,f,use(eccentsq1),format="  first eccentricity squared (a^2-b^2)/a^2,  b is polar radius: %.10lg\n";
  write,f,use(flatn),    format="  flattening: %.10lg\n";
  write,f,use(eccentsq2),format="  second eccentricity squared (a^2-b^2)/b^2: %.10lg\n";
}
ellipsoid= closure(ellipsoid, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*------------------------------------------- Peg Point --------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(print);

func pegpoint (base,elp,lat,lon,hdg,peg=,deg=)
/* DOCUMENT pegpoint (elp,lat,lon,hdg,peg=,deg=)

  rotXYZxyz transforms xyz into XYZ  where:

  xyz(1:3,..) peg point xyz vector (meters)
      1: x    zenith, ==0 at local sch sphere radius, sphere centered @ geodetic center
      2: y    // to tangent to track @ geodetic center (sch sphere center)
      3: z    cross track (left)
  latlonh(1:3,..) :
      1: lat       latitude (rad -pi/2 to pi/2)  == c/radius
      2: lon       longitude (rad -pi to pi)     == s/radius
      3: hgt       height above sphere (meters)
 */
{
  ob= base(:);

  save, ob, type= "pegpoint";

  if (allof(is_obj(peg,["elp","lat","lon","hdg"],1))==0 && \
    peg(type)==ob(type))
  {
    lat= peg(lat);
    lon= peg(lon);
    hdg= peg(hdg)
    elp= peg(elp);
  }
  else if (is_struct(structof(peg)))
  {
      lat= peg.lat;
      lon= peg.lon;
      hdg= peg.hdg;
      elp= ellipsoid(wgs84=1);  // kludge ... assume that old peg is wgs84
  }
  else
  {
    if (is_void(elp)||is_void(lat)||is_void(lon)||is_void(hdg))
      error,"elp,lat,lon,hdg keys required";
    if (deg==1) {
      lat*= RADEG;
      lon*= RADEG;
      hdg*= RADEG;
    }
  }
  lat= double(lat);
  lon= double(lon);
  hdg= double(hdg)

  save,ob,elp,lat,lon,hdg;

  rad= elp(locradcurv,lat,hdg);
  eastrad= elp(eastradcurv,lat);
  northrad= elp(northradcurv,lat);

  rotXYZxyz= rotation([pi/2-hdg,-lat,lon],[1,2,3]);
  XYZOff= vecllh([lat,lon,0.0],elp)(to_XYZ,)(add,rotXYZxyz(rot,vectorCart([-rad,0,0])));
  schOff= rotXYZxyz(inv,)(rot,XYZOff)(scale,-1);

  save,ob,rad,eastrad,northrad,rotXYZxyz,XYZOff,schOff;

  return ob;
}
func  print (f)
{
  write,f,"pegpoint parameters: ",format="%s\n";
  write,f,use(lat)*DEGRA,    format="  latitude[deg]: %.10lg\n";
  write,f,use(lon)*DEGRA,    format="  longitude[deg]: %.10lg\n";
  write,f,use(hdg)*DEGRA,    format="  heading[deg]: %.10lg\n";
  write,f,use(rad),          format="  local curvature radius[m]: %.10lg\n";
  // use_method(elp,eastradcurv,use(lat));
}
pegpoint= closure(pegpoint, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*------------------------------------------- XYZ vector -------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(to_llh,to_xyz,to_pxyz,to_sch);

func vecXYZ (base,cart,..,rand=,unit=)
/* DOCUMENT (base,cart)
   vectorCart subclass with different name member and 3D inforced.

   construction:
   -------------
   vecXYZ(vecXYZ());
   vecXYZ(array(float,3,...));
   vecXYZ(3,2,1,rand=1[,unit=1]);
   vecXYZ([3,3,2,1],rand=1[,unit=1]);  // rankdim array used for specifying dimensions

   CART are really any old cartesian coordinates bu they are assumed to be ECEF, and thus
   *must* own ellipsoid ELP -- which must be provided. (ELP does not belong here.)
   Since this inherits from VECCART components data is member CART.
   Z-axis along the axis of the ellipsoid, positive northward
   X- and Y-axis in the plane of the equator, X-axis positive toward 0 degrees longitude
      and Y-axis positive toward 90 degrees east longitude

  methods:
  ========
  TO_LLH -- to_llh (elp,iter=, exact=, eps=, itermax=)
            from CARTESIAN (XYZ) to GEOGRAPHIC (LatLonHeight:llh) coordinates
            use iter=1 for speed
  tests:
  ======
  llh= random(3,4,5)*[pi/2,pi,1e7];
  avg(vecllh(llh,ellipsoid(wgs84=1))(to_XYZ,)(to_llh,)(llh)-llh);
  xyz=random(3,4,5)*1e7;
  max(vecXYZ(xyz,ellipsoid(wgs84=1))(to_llh,)(to_XYZ,)(cart)-xyz);
 */
{
  ob= vectorCart();
  save, ob, [], base(:);
  save, ob, type= "vecXYZ";

  if (is_obj(cart,"cart",1)==0)
  {
    cart= cart(cart);
  }
  // -- construct from random components in sphere or spherical shell --
  if (rand==1)
  {
    while (more_args()) build_dimlist, cart, next_arg();
    if (is_void(cart)) cart= [0];
    if (is_scalar(cart)) cart= [1,cart];
    cart= ob(_random,cart,unit=unit)(cart);
  }
  else
  {
    if (!is_void(cart) && dimsof(cart)(2)!=3) error,"expecting leading dim==3";
  }

  save,ob,cart;

  return ob;
}
func to_llh (elp,iter=, exact=, eps=, itermax=)
{
  if (!is_void(void)) error,"no args allowed";
  if (is_void(itermax)) itermax= 40;
  if (is_void(eps)) eps= 1.0e-14;
  if (is_void(exact)) exact= 1;

  local XYZ;
  eq_nocopy, XYZ, use(cart);

  dXYZ= dimsof(XYZ);
  s1= dXYZ(1)==1;

  llh= array(structof(XYZ), dXYZ);

  if (iter==1) {
    if (s1) {
      prad= sqrt(XYZ(1)^2 + XYZ(2)^2);
      llh(1)= lat= atan(XYZ(3), prad);
      itr= 0;
      do {
        lat= llh(1);
        ecurv= elp(eastradcurv,lat);
        tmp= prad / cos(lat) - ecurv;
        llh(3)= tmp;
        tmp= 1.0 - elp(eccentsq1) * ecurv / ( ecurv + tmp);
        llh(1)= atan(XYZ(3), prad*tmp );
      } while ((abs(lat-llh(1)) > eps) && (++itr<itermax))
      if (itr==itermax) error("exceeding Max iterations");
      llh(2)= atan(XYZ(2),XYZ(1));
    } else {
      prad= sqrt(XYZ(1,..)^2 + XYZ(2,..)^2);
      llh(1,..)= lat= atan(XYZ(3,..), prad);
      w= indgen(numberof(lat));
      itr= 0;
      do {
        lat(w)= llh(1,w);
        ecurv= elp(eastradcurv,lat(w));
        tmp= prad(w) / cos(lat(w)) - ecurv;
        llh(3,w)= tmp;
        tmp= 1.0 - elp(eccentsq1) * ecurv / ( ecurv + tmp);
        llh(1,w)= atan(XYZ(3,w), prad(w)*tmp );
        w= where(abs(lat-llh(1,..)) > eps);
      } while (is_array(w) && (++itr<itermax))
      if (itr==itermax)
        error("exceeding Max iterations");
      llh(2,..)= atan(XYZ(2,..),XYZ(1,..));
    }
  } else  if (exact==1) {
    prad= sqrt(XYZ(1,..)^2 + XYZ(2,..)^2);  // could throw if == 0

    b= abs(elp(majax) - elp(majax) * elp(flatn))*sign(XYZ(3,..));

    E= ((XYZ(3,..) + b) * b / elp(majax) - elp(majax)) / prad;
    F= ((XYZ(3,..) - b) * b / elp(majax) + elp(majax)) / prad;
    P= (E * F + 1.0) * 4.0 / 3.0;
    Q= (E * E - F * F) * 2.0;
    D= P*P*P + Q*Q;

    msk= D>=0.0;
    msk0= !msk;
    v1= v0= [];
    if (anyof(msk)) {
        wt= where(msk);
        s= sqrt(D(wt)) + Q(wt);
        s= exp(log(abs(s)) / 3.0) *sign(s);
        v1= P(wt) / s - s;
        v1= -(Q(wt) + Q(wt) + v1*v1*v1) / (3.0 * P(wt));
    }
    if (anyof(msk0)) {
        wt= where(msk0);
        v0= 2.0 * sqrt(-P(wt)) * cos(acos(Q(wt) / P(wt) / sqrt(-P(wt))) / 3.0);
    }
    v= merge(v1, v0, msk);

    G= 0.5 * (E + sqrt(E*E + v));
    t= sqrt(G*G + (F - v * G) / (G + G - E)) - G;
    llh(1,..)= atan((1.0 - t*t) * elp(majax) / (2.0 * b * t));
    llh(3,..)= (prad - elp(majax) * t) * cos(llh(1,..)) + (XYZ(3,..) - b) * sin(llh(1,..));

    // Determine the east longitude (0 <= lon < 2*pi)
    msk= XYZ(3,..)==0.0;
    msk0= !msk;
    v1= v0= [];
    if (anyof(msk)) {
        wt= where(msk);
        v1= pi - (pi/2)*sign(XYZ(2,wt));
    }
    if (anyof(msk0)) {
        wt= where(msk0);
        v0= atan(XYZ(2,wt), XYZ(1,wt));
        //v0(where(v0<0.0)) += 2*pi;
    }
    llh(2,..)= merge(v1, v0, msk);
  } else {
    llh(2,..)= atan(XYZ(2,..),XYZ(1,..));
    prad= sqrt(XYZ(1,..)^2 + XYZ(2,..)^2);
    alpha= atan(XYZ(3,..)/(prad*sqrt(1 - elp(eccentsq1))));
    llh(1,..)= atan((XYZ(3,..) + elp(eccentsq2)*elp(minax)*sin(alpha)^3),\
                     (prad - elp(eccentsq1)*elp(majax)*cos(alpha)^3));
    // -- ellipsoid height
    llh(3,..)= prad/cos(llh(1,..)) - elp(eastradcurv,llh(1,..));
  }

  return vecllh(llh,elp);
}
func to_xyz (peg)
{
  xyzXYZ= peg(rotXYZxyz,inv,);
  return vecxyz(xyzXYZ(rot,use())(add,peg(schOff)),peg);
}
func to_pxyz (peg)
{
  return use_method(to_xyz,peg)(to_pxyz,);
}
func to_sch (peg)
{
  return use_method(to_xyz,peg)(to_sch,);
}
vecXYZ= closure(vecXYZ, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*------------------------------------------- llh vector -------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save(scratch, tmp);
tmp= save(to_XYZ,to_xyz,to_pxyz,to_sch,XYZ_in_enu,_random);

func vecllh (base,llh,elp,..,rand=)
/* DOCUMENT ox= vecllh (llh,elp)
   ellipsoidal or Geographic [[Lat,Lon,Height],..] or llh for short
   no Geoid

   methods:
   ========
   to_XYZ --
   to_xyz -- from GEOGRAPHIC (LatLonHeight: llh) to CARTESIAN (XYZ) coordinates
   to_pxyz --
   to_sch --

 */
{
  ob= base(:);

  save, ob, type= "vecllh";

  if (allof(is_obj(llh,["llh","elp"],1))==0 && llh(type)==ob(type))
  {
    elp= llh(elp);
    llh= llh(llh);
  }
  // -- construct from random components in sphere or spherical shell --
  if (rand==1) {
    llh0= llh;
    while (more_args()) build_dimlist, elp, next_arg();
    if (is_void(elp)) elp= [0];
    if (is_scalar(elp)) elp= [1,elp];
    llh= ob(_random,elp);
    elp= llh0;
  }
  else
  {
    if (!is_void(llh))
      if (dimsof(llh)(2)!=3) error,"expecting leading dim==3";
  }

  if (is_void(elp)) error,"elp key required";

  save,ob,elp,llh;

  return ob;
}
func to_XYZ (void)
{
  XYZ= array(structof(use(llh)), dimsof(use(llh)));

  eastRadius= use(elp,eastradcurv,use(llh,1,..));

  XYZ(1,..)= XYZ(2,..)= (eastRadius + use(llh,3,..))*cos(use(llh,1,..));

  XYZ(1,..)*= cos(use(llh,2,..));
  XYZ(2,..)*= sin(use(llh,2,..));
  XYZ(3,..)= (eastRadius*(1 - use(elp,eccentsq1)) + use(llh,3,..))*sin(use(llh,1,..));

  return vecXYZ(XYZ,use(elp));  // copy safer ?
}
func to_xyz (peg)
{
  XYZ= use_method(to_XYZ);
  return XYZ(to_xyz,peg);
}
func to_pxyz (peg)
{
  XYZ= use_method(to_XYZ);
  return XYZ(to_pxyz,peg);
}
func to_sch (peg)
{
  XYZ= use_method(to_XYZ);
  return XYZ(to_sch,peg);
}
func XYZ_in_enu (void)
/* DOCUMENT: XYZ_in_enu(latlon)
   geodetic latlon (ie wgs84)
   Takes a lat and lon and returns a change of basis matrix from ENU to
   geocentric coordinates.
 */
{
  flip= rotation([[0,1,0],[0,0,1],[1,0,0]]);
  return rotation(flip(mult,rotation(llh(1:2,..)*[-1,1],[1,2])));
}
func _random (..)
{
  dimlist= next_arg();
  while (more_args()) build_dimlist, dimlist, next_arg();

  d3= _(dimlist(1)+1,3,(dimlist(1)>0? dimlist(2:): []));
  a= array(double,d3);

  a(1,..)= asin(2*(random(dimlist)-0.5));
  a(2,..)= random(dimlist)*2*pi;
  a(3,..)= random(dimlist);

  return a;
}
vecllh= closure(vecllh, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------------------------------*/
/*--------------------------------------- sch vector -----------------------------------------*/
/*--------------------------------------------------------------------------------------------*/

scratch= save (scratch, tmp);
tmp= save (to_xyz,to_pxyz,to_latlonh,to_XYZ,xyz_tprot,pxyz_tprot,XYZ_tprot)
save, tmp, tprot_xyz,tprot_pxyz,tprot_XYZ;

func vecsch (base,sch,peg,..,rand=)
/* DOCUMENT ox= vecsch (sch,peg)

   to_xyz (void)
   this program converts sch coor. in sch sphere
   into (ER!) xyz coor.
   provide peg or radius of curvature only (peg.rad)
   peg.rad       radius of the local sch sphere typically peg.rad= locradcurv(peg.hdg,peg.lat)= peg.rad
   xyz(1:3,..) peg point xyz vector (meters)
       1: x    zenith, ==0 at local sch sphere radius, sphere centered @ geodetic center
       2: y    // to tangent to track @ geodetic center (sch sphere center)
       3: z    cross track (left)
   to_pxyz(sch,peg)
   this program converts sch coor. in sch sphere
   into peg point spherical (Not WGS84) xyz coor.
   provide peg or radius of curvature only (peg.rad)

   pxyz_tprot (void)
   rotation operator whose DCM (rotmat method) gives the
   sch unit tangantial basis vecors (second index) in peg pxyz coordinates (first
   index) i.e.  pxyz_in_sch(sch,peg)(i,j)= hat_pxyz_i dot hat_sch_j
   xyz_tprot (void)
   rotation operator whose DCM (rotmat method) gives the
   sch unit tangantial basis vecors (second index) in xyz coordinates (first
   index) i.e.  xyz_in_sch(sch,peg)(i,j)= hat_xyz_i dot hat_sch_j
   provide peg or radius of curvature only (peg.rad)
   tprot_xyz (void)
   sch unit tangantial basis vecors (first index) in peg xyz coordinates
   i.e.  sch_in_xyz(i,j)= hat_sch_i dot hat_xyz_j
   provide peg or radius of curvature only (peg.rad)
   tprot_pxyz (void)
   sch unit tangantial basis vecors (first index) in peg xyz coordinates
   i.e.  DCM is sch_in_pxyz(i,j)= hat_sch_i dot hat_pxyz_j
   XYZ_tprot (void)
   sch unit tangantial basis vecors (first index) in peg XYZ coordinates
   i.e.  DCM is sch_in_XYZ(i,j)= hat_sch_i dot hat_XYZ_j
 */
{
  ob= base(:);

  save, ob, type= "vecsch";

  if (allof(is_obj(sch,["sch","peg"],1))==0 && sch(type)==ob(type))
  {
    peg= sch(peg);
    sch= sch(sch);
  }
   // -- construct from random components in sphere or spherical shell --
  if (rand==1)
  {
    while (more_args()) build_dimlist, peg, next_arg();
    if (is_void(peg)) peg= [0];
    if (is_scalar(peg)) peg= [1,peg];
    return veclatlonh(sch,peg,rand=1);
  }
  else
  {
    if (!is_void(sch))
      if (dimsof(sch)(2)!=3) error,"expecting leading dim==3";
  }

  if (is_void(peg)) error,"peg required";

  save,ob,peg,sch;

  return ob;
}
func to_xyz (void)
{
  v= use_method(to_latlonh);
  return v(to_xyz,) ;
}
func to_pxyz (void)
{
  v= use_method(to_latlonh);
  return v(to_pxyz,);
}
func to_latlonh (void)
{
  re= use(peg,rad);
  return veclatlonh(use(sch,[2,1,3],..)/[re,re,1],use(peg));
}
func to_XYZ (void)
{
  v= use_method(to_xyz);
  return v(to_XYZ,);
}
func xyz_tprot (void)
{
  flip= rotation([[0,1,0],[0,0,1],[1,0,0]]);
  return rotation(flip(mult,use_method(pxyz_tprot)));
}
func pxyz_tprot (void)
{
  llh= use_method(to_latlonh)(latlonh);
  return rotation(llh(1:2,..)*[-1,1],[1,2]);
}
func tprot_xyz (void)
{
  out= use_method(xyz_tprot);
  return rotation(out(inv,));
}
func tprot_pxyz (void)
{
  out= use_method(pxyz_tprot);
  return rotation(out(inv,));
}
func XYZ_tprot (void)
{
  return rotation(use(peg,rotXYZxyz)(mult,use_method(xyz_tprot,)));
}
func tprot_XYZ (void)
{
  out= use_method(XYZ_tprot);
  return rotation(out(inv,));
}
vecsch= closure(vecsch, restore(tmp));
restore, scratch;

/*--------------------------------------------------------------------*/
/*---------------- GEOCENTRIC/ELLIPTICAL XYZ  ------------------------*/
/*--------------------------------------------------------------------*/

func body_in_TCN (TCNypr)
/* DOCUMENT: body_in_TCN(TCNypr)
    converts the TCN Euler angle sequence [get the body fixed (ijk ==frw/starboard/down)
   starting from TCN (Track/CrossTrack/Nadir] into a change of basis matrix between the TCN
    and ijk frame.

    NOTES: Body fixed frame oriented so that

           i=   BFX= oriented along fulselage (in direction of flight)
           j=   BFY= oriented along right wing
           k=   BFZ= oriented downward

    TCN is the tangent space to the ellipsoid at the xyz point of the
    platform.  N is normal to the ellipsoid pointing from platform to
    Earth.  T is in the direction of the projection of the velocity into
    the plane normal to N.  C completes the triad TCN: C= N x T.

    ijk is the body fixed coordinate system.  When the attitude angles
    are all zero these axes coincide with the TCN axes.

    The attitude is specified by an axis sequence and angles for each
    axis.  The axis sequence specifies the order in which the elementary
    rotations are to be applied to move the ijk triad from TCN to the
    tilted position of the platform.  The angles are given as a
    counterclockwise angle as viewed from the direction in which the
    axis points; that is, the angle is consistent with the right hand
    rule for the rotation of the ijk triad.  Typically, the axis sequence
    is yaw, then  pitch, and then roll.  The change of basis matrix to
    go from TCN to ijk in this case is given by [1,r]*[2,p]*[3,y] where
    [i,alpha] represents the elementary rotation matrix for a rotation
    about the i-axis through angle alpha.  The elementary rotation matrices
    are defined in Euler.C.  The change of basis matrix to go from ijk
    to TCN is the transpose of this matrix, which is [3,-y]*[2,-p]*[1,-r].

    NOTES: BODY fixed frame oriented so that
         I=   BFX= oriented along fulselage
         J=   BFY= oriented along left wing
         K=   BFZ= oriented alond upward body fixed vector

     i= I,    j= -J,   k= -K   &&    S= T,  C= -C,  H= -N
 */
 // note that TCN_in_body= rotEuler(-TCNypr(::-1,..));
{
  return rotEuler(TCNypr,[3,2,1]);
}

/*--------------------------------------------------------------------*/

func TCN_in_body (TCNypr)
/* DOCUMENT:  TCN_in_body(TCNypr)(1:3,1:3,..)

    TCN_in_body= rotEuler(-TCNypr(::-1,..));

   SEE ALSO: body_in_TCN
 */
{
  return transpose(body_in_TCN(TCNypr),[1,2]);
}
/*--------------------------------------------------------------------*/

func BODY_in_TCN (TCNypr)
/* DOCUMENT:  TCN_in_BODY(TCNypr)(1:3,1:3,..)

    TCN_in_body= rotEuler(-TCNypr(::-1,..));
    then right multiply by diag [1, -1, -1] to flip j=-J and k=-K

   SEE ALSO: body_in_TCN
 */
{
  return flip23_l(body_in_TCN(TCNypr));
}

/*--------------------------------------------------------------------*/

func TCN_in_BODY (TCNypr)
/* DOCUMENT:  TCN_in_BODY(TCNypr)(1:3,1:3,..)

    TCN_in_body= rotEuler(-TCNypr(::-1,..));
    then right multiply by diag [1, -1, -1] to flip j=-J and k=-K

   SEE ALSO: body_in_TCN
 */
{
  return flip23_r(TCN_in_body(TCNypr));
}

/*--------------------------------------------------------------------*/

func TCN_in_sch (XYZ, XYZdot, peg)
/* DOCUMENT: TCN_in_sch(XYZ, XYZdot, peg)
   compute the change of basis matrix from a local SCH frame to a local TCN frame
 */
{
  sch= XYZ_to_sch(XYZ, peg);

  return matmatseq(TCN_in_XYZ(XYZ, XYZdot), XYZ_in_sch(sch, peg));
}
/*--------------------------------------------------------------------*/

func sch_in_TCN (XYZ, XYZdot, peg)
/* DOCUMENT: sch_in_TCN(XYZ, XYZdot, peg)
   compute the change of basis matrix from a local TCN frame to a local SCH frame
 */
{
  return transpose(TCN_in_sch(XYZ, XYZdot, peg),[1,2]);
}
/*--------------------------------------------------------------------*/

func locNormElpXYZ (llh)
/* DOCUMENT: locNormElpXYZ(llh)
   returns said vector in XYZ;)
 */
{
  nrm= array(double, dimsof(llh));  // UP normal

  coslat= cos(llh(1,..));

  nrm(1,..)= coslat * cos(llh(2,..));
  nrm(2,..)= coslat * sin(llh(2,..));
  nrm(3,..)= sin(llh(1,..));

  return nrm;
}

/*--------------------------------------------------------------------*/

func XYZ_in_TCN (XYZ, XYZdot)
/* DOCUMENT: XYZ_in_TCN(XYZ, XYZdot)
   computes the transformation matrix from XYZ to a local TCN frame
   see ~sh tcnbasis
 */
{
  dx= dimsof(XYZ);

  ndn= -locNormElpXYZ(XYZ_to_llh(XYZ));

  nc= crossvec(ndn, XYZdot,seq=1);
  nc= nc/abs(nc(1,..),nc(2,..),nc(3,..))(-,);
  nt= crossvec(nc, ndn,seq=1);

  out= array(double,_(dx(1)+1,3,dx(2:)));
  out(,1,..)= nt;
  out(,2,..)= nc;
  out(,3,..)= ndn;

  return out;
}

/*--------------------------------------------------------------------*/

func TCN_in_XYZ (XYZ, XYZdot)
/* DOCUMENT: TCN_in_XYZ(XYZ, XYZdot)
   SEE ALSO: XYZ_in_TCN
 */
{
  return transpose(XYZ_in_TCN(XYZ, XYZdot),[1,2]);
}

/*--------------------------------------------------------------------*/

func sch_in_BODY (SCHypr)
/* DOCUMENT: sch_in_BODY(SCHypr(1:3,..))(1:3,1:3,..)
    converts the SCH Euler  angle sequence into a change of basis matrix between the BODY fixed
    frame (IJK) and SCH frame.

    Get the BODY frame by rotEuler SCH : BODY == rot1(R)*rot2(P)*rot3(Y) X SCH

    BODY_in_SCH= Fw*Fz* rotEuler(SCHypr, [3,2,1]) *Fw*Fz (Fw==Flip wings, Fz==Flip zenith)
    SCH_in_BODY= Fw*Fz* rotEuler(-SCHrpy, [1,2,3]) *Fw*Fz (Fw==Flip wings, Fz==Flip zenith)

    Note that SCHypr is directly modeled against
    TCNypr which is body(==ijk)_in_TCN(TCNypr)= rotEuler(TCNypr,[3,2,1]);

    NOTES: BODY fixed frame oriented so that
         I=   BFX= oriented along fulselage
         J=   BFY= oriented along left wing
         K=   BFZ= oriented alond upward BODY fixed vector

         j= -J  ,  k= -K       S= T , C= -C ,  H= -N
 */
{
  return flip23_lr(rotEuler(-SCHypr(::-1,..)));
}

/*--------------------------------------------------------------------*/

func BODY_in_sch (SCHypr)
/* DOCUMENT: BODY_in_sch(SCHypr(1:3,..))(1:3,1:3,..)
   also == flip23_lr(rotEuler(SCHypr,[3,2,1]));
   This also means that SCHyr will be:
   SCHypr= eulerRot(fwfz*BODY_in_sch*fwfz,[3,2,1])
   -or-
   SCHypr= eulerRot(body_in_sch*fwfz,[3,2,1])
   SEE ALSO: sch_in_BODY
 */
{
  return transpose(sch_in_BODY(SCHypr),[1,2]);
}

/*--------------------------------------------------------------------*/

func TCNypr_to_SCHypr (TCNypr, XYZ, XYZdot, peg)
/* DOCUMENT:
   since:
   BODY_in_sch(SCHypr)= flip23_lr(rotEuler(SCHypr,[3,2,1]));
   have:
   rotEuler(SCHypr,[3,2,1])= flip23_lr(BODY_in_sch(SCHypr))
   thus, compute body_in_sch and flip_RIGHT
   SCHypr euler angles produce BODY in sch frame (see BODY_in_sch)
   SCHypr= eulerRot(body_in_sch(TCNypr, XYZ, XYZdot, peg)#Diag[1,-1,-1],[3,2,1]);

   see also body_in_TCN == rotEuler(TCNypr, [3,2,1])
         or TCNypr == eulerRot(body_in_TCN, [3,2,1])
 */
{
 mat= matmatseq(body_in_TCN(TCNypr), TCN_in_sch(XYZ, XYZdot, peg));  // body_in_sch, but we need body_in_"TCN"

  return eulerRot(flip23_r(mat),[3,2,1]);
}

/*--------------------------------------------------------------------*/

func SCHypr_to_TCNypr (SCHypr, XYZ, XYZdot, peg)
  /* DOCUMENT:
     since:  body_in_TCN == rotEuler(TCNypr, [3,2,1]),
     get:    TCNypr == eulerRot(body_in_TCN, [3,2,1])

     compute body_in_TCN as flip23_l(BODY_in_sch#sch_in_TCN)
   */
{
  mat= matmatseq(BODY_in_sch(SCHypr), sch_in_TCN(XYZ, XYZdot, peg));  // body_in_sch, but we need body_in_"TCN"

  return eulerRot(flip23_l(mat),[3,2,1]);
}

/*--------------------------------------------------------------------*/
func flip23_r (mat)
/* DOCUMENT: flip23_r (mat)
   flip sign of 2&3 (out of 3) coordinates on right
   SEE ALSO: flip23_l (mat) flip23_lr (mat)
 */
{
  mat(1,2,..) *= -1;
  mat(1,3,..) *= -1;
  mat(2,2,..) *= -1;
  mat(2,3,..) *= -1;
  mat(3,2,..) *= -1;
  mat(3,3,..) *= -1;
  return mat;
}
func flip23_l (mat)
/* DOCUMENT: flip23_l (mat)
   flip sign of 2&3 (out of 3) coordinates on left
   SEE ALSO: flip23_lr (mat) flip23_r (mat)
 */
{
  mat(2,1,..) *= -1;
  mat(3,1,..) *= -1;
  mat(2,2,..) *= -1;
  mat(3,2,..) *= -1;
  mat(2,3,..) *= -1;
  mat(3,3,..) *= -1;
  return mat;
}
func flip23_lr(mat)
/* DOCUMENT: flip23_lr(mat)
   flip sign of 2&3 (out of 3) coordinates on left and right
   SEE ALSO: flip23_l (mat) flip23_r (mat)
 */
{
  mat(2,1,..) *= -1;
  mat(3,1,..) *= -1;
  mat(1,2,..) *= -1;
  mat(1,3,..) *= -1;
  return mat;
}

/*--------------------------------------------------------------------*/

func rotEuler (rot, axisseq)
/* DOCUMENT:
   euler rotation matrix order of (1-to-3) rotation described in axisseq which defaults to [1,2,3] where:
   euler_i,j= rotated_i_hat dot unrotated_j_hat  i.e. transform unrotated coor into rotated coor
                          or
               rotated_i_hat dot original_j_hat   i.e. transform original coor into rotated coor

   In this notation the euler roation matrix is  rotated_in_original(_coor)
   rot may have (3,..) dims; in that case mat is (1:3,1:3,..)
   an Euler angle sequence IJK are multiplied in the order Mat= RotK * RotJ * RotI

   identities:
   ===========
   [a,b,c]==eulerRot(rotEuler([a,b,c], [m,n,p]),[m,n,p]);  a,b,c in [-pi,pi],  m,n,p in {1,2,3}
   [a,b,c]== -1*eulerRot(transpose(rotEuler([a,b,c],[m,n,p])),[p,n,m]);
   rotEuler([a,b,c], [m,n,p])==transpose(rotEuler(-[a,b,c](::-1),[p,n,m]));
 */
{
  if (is_void(axisseq)) axisseq= [1,2,3];

  permute= [2,3,1];

  c= cos(rot);

  ia= _(axisseq(1:2),permute(axisseq(2)));

  if(ia(3)==ia(1)) {
    ia(3)= permute(axisseq(1));
    s= -sin(rot);
  } else {
    s= sin(rot);
  }

  d= dimsof(rot);
  d(1) += 1;
  mat= array(double,_(d(1),3,d(2:)));

  if (axisseq(3)==axisseq(1)) {
    s3c2= s(3,..)*c(2,..);
    c3c2= c(3,..)*c(2,..);

    mat(ia(1),ia(1),..)= c(2,..);
    mat(ia(1),ia(2),..)= s(2,..)*s(1,..);
    mat(ia(1),ia(3),..)= -s(2,..)*c(1,..);

    mat(ia(2),ia(1),..)= s(3,..)*s(2,..);
    mat(ia(2),ia(2),..)= c(3,..)*c(1,..) - s3c2*s(1,..);
    mat(ia(2),ia(3),..)= c(3,..)*s(1,..) + s3c2*c(1,..);

    mat(ia(3),ia(1),..)= c(3,..)*s(2,..);
    mat(ia(3),ia(2),..)= -s(3,..)*c(1,..) - c3c2*s(1,..);
    mat(ia(3),ia(3),..)= -s(3,..)*s(1,..) + c3c2*c(1,..);
  } else {
    s3s2= s(3,..)*s(2,..);
    c3s2= c(3,..)*s(2,..);

    //first row
    mat(ia(1),ia(1),..)= c(3,..)*c(2,..);
    mat(ia(1),ia(2),..)= s(3,..)*c(1,..) + c3s2*s(1,..);
    mat(ia(1),ia(3),..)= s(3,..)*s(1,..) - c3s2*c(1,..);

    //second row
    mat(ia(2),ia(1),..)= -s(3,..)*c(2,..);
    mat(ia(2),ia(2),..)= c(3,..)*c(1,..) - s3s2*s(1,..);
    mat(ia(2),ia(3),..)= c(3,..)*s(1,..) + s3s2*c(1,..);

    //third row
    mat(ia(3),ia(1),..)= s(2,..);
    mat(ia(3),ia(2),..)= -c(2,..)*s(1,..);
    mat(ia(3),ia(3),..)= c(2,..)*c(1,..);
  }

  return mat;
}

/*--------------------------------------------------------------------*/

func eulerRot (rot, axisseq)
/* DOCUMENT: eulerRot(rot,axisseq)
   rot == arbitary rotation matrix (could be a sequence of rotations a(3,3,N,M,I...)
   func (subroutine),
   This routine will take a rotation matrix
   and factor it into an sequence of three Euler angle rotations
   about a specified Euler angle sequence.
   tests:
   rotEuler([10,-5,40]*RADEG,[1,2,3])-transpose(rotEuler(-[10,-5,40](::-1)*RADEG,[3,2,1]))
   or
   rotEuler([10,-5,40]*RADEG,[1,2,3])(,+)*rotEuler(-[10,-5,40](::-1)*RADEG,[3,2,1])(+,)
 */
{
  dr= dimsof(rot);
  if (is_void(axisseq)) axisseq= [1,2,3];
  euler= array(double,_(dr(1)-1,dr(3:)));
  s= array(long,3);

  if (axisseq(1) == axisseq(3)) {
    m= axisseq(1);
    i1= axisseq(2);
    if (m == 1) {
      if (i1 == 2) {
        i2= 3;
        s(1)= -1;
        s(3)= 1;
      } else {
        i2= 2;
        s(1)= 1;
        s(3)= -1;
      }
    } else if (m == 2) {
      if (i1 == 1) {
        i2= 3;
        s(1)= 1;
        s(3)= -1;
      } else {
        i2= 1;
        s(1)= -1;
        s(3)= 1;
      }
    } else if (m == 3) {
      if (i1 == 2) {
        i2= 1;
        s(1)= 1;
        s(3)= -1;
      } else {
        i2= 2;
        s(1)= -1;
        s(3)= 1;
      }
    }
    msk= rot(m,m,..)<=1&rot(m,m,..)>=-1;
    wt= where(msk);
    wf= where(!msk);
    if (is_array(wt))ft= acos(rot(m,m,wt));
    if (is_array(ws))ff= array(0.0,numberof(wf));
    euler(2,..)= merge(ft, ff, msk);
    euler(3,..)= atan(rot(i1,m,..),s(3)*rot(i2,m,..));
    euler(1,..)= atan(rot(m,i1,..),s(1)*rot(m,i2,..));
  } else {
    i1= axisseq(3);
    i2= axisseq(1);
    m= axisseq(2);
    si= permutSign(axisseq);
    msk= rot(i1,i2,..)<=1&rot(i1,i2,..)>=-1;
    wt= where(msk);
    wf= where(!msk);
    if (is_array(wt))ft= si*asin(rot(i1,i2,wt));
    if (is_array(ws))ff= array(pi/2,numberof(wf));
    euler(2,..)= merge(ft, ff, msk);
    euler(1,..)= atan(-si*rot(i1,m,..),rot(i1,i1,..));
    euler(3,..)= atan(-si*rot(m,i2,..),rot(i2,i2,..));
  }
  return euler;
}

/*--------------------------------------------------------------------------*/

func permutSign(a)
{
  n= numberof(a);
  if(anyof(a(sort(a))!=indgen(n)))
    error,"not a permutation";

  b= a;
  s= 1;
  for (i=1; i<=n-1; i++){
    j= min(where(b==i));
    if ( j != i ) {
      b([i,j])= b([j,i]);
      s= -s;
    }
  }
  return s;
}

/*--------------------------------------------------------------------*/

func sch_in_XYZ(sch, peg)
/* DOCUMENT:  sch_in_XYZ(sch, peg)
   SEE ALSO: XYZ_in_sch
 */
{
  return transpose(XYZ_in_sch(sch, peg),[1,2]);
}

/*--------------------------------------------------------------------*/

func dist_sch (a, b, peg)
/* DOCUMENT: dist_sch(a, b, peg)
   ||a - b||; all sch vectors.
 */
{
  return sqrt(((sch_to_pxyz(a,peg) - sch_to_pxyz(b,peg))^2)(sum,..));
}

/*--------------------------------------------------------------------*/

func add_sch(a, b, peg)
/* DOCUMENT: c= add_sch(a, b, peg)
   c= a + b; all sch vectors.  addition carried out in peg xyz
 */
{
  return pxyz_to_sch(sch_to_pxyz(a, peg) + sch_to_pxyz(b, peg), peg);
}
/*--------------------------------------------------------------------*/

func pos_sch (v2, v1, peg)
/* DOCUMENT: v21= pos_sch(v2, v1, peg)
   v21= v2 - v1; all sch vectors.  v2 position vector from v1,
                                    v1 local sch tangent base
 */
{
  d= dimsof(v2);
  if(d(1)>1)d(3:)=1;
  reform(v1,d);

  return (sch_to_pxyz(v2, peg) - sch_to_pxyz(v1, peg))(+,..)*pxyz_in_sch(v1, peg)(+,..);   //
}

/*--------------------------------------------------------------------*/

func xyz_to_XYZ (xyz, peg)
{
  d= dimsof(xyz);
  if(d(1)>1)d(3:)= 1;

  return peg.rotXYZxyz(..,+)*xyz(+,..) + reform(peg.XYZOff, d);
}

/*--------------------------------------------------------------------*/

func XYZ_to_xyz(XYZ, peg){
/* DOCUMENT:
 */
 d= dimsof(XYZ);
 if(d(1)>1)d(3:)= 1;

 xyzXYZ= rotXYZxyz(inv,);

 return peg.xyzXYZ(..,+)*XYZ(+,..) + reform(peg.schOff, d);}

/*--------------------------------------------------------------------*/

func sch_to_XYZ(sch, peg){
/* DOCUMENT:
 */
 d= dimsof(sch);
 if(d(1)>1)d(3:)= 1;

 return xyz_to_XYZ(sch_to_xyz(sch, peg), peg);}

/*--------------------------------------------------------------------*/

func XYZ_to_sch(XYZ,peg){
/* DOCUMENT:
 */
 d= dimsof(XYZ);
 if(d(1)>1)d(3:)= 1;

 return xyz_to_sch(XYZ_to_xyz(XYZ, peg), peg);}

/*--------------------------------------------------------------------*/

func pxyz_to_XYZ(pxyz, peg){
/* DOCUMENT:
 */
 return xyz_to_XYZ(pxyz_to_xyz(pxyz, peg), peg);}

/*--------------------------------------------------------------------*/

func XYZ_to_pxyz(XYZ, peg){
/* DOCUMENT:
 */
 return xyz_to_pxyz(XYZ_to_xyz(XYZ, peg), peg);}

/*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*/

func sch_to_llh(sch, peg){
/* DOCUMENT:sch_to_llh(sch, peg)
   XYZ_to_llh(sch_to_XYZ(sch,peg))
 */
 return XYZ_to_llh(sch_to_XYZ(sch,peg));}

/*--------------------------------------------------------------------*/

func llh_to_sch(llh, peg){
/* DOCUMENT: llh_to_sch(llh, peg)
   XYZ_to_sch(llh_to_XYZ(llh),peg);
 */
 return XYZ_to_sch(llh_to_XYZ(llh),peg);}

/*--------------------------------------------------------------------*/

func xyz_in_BODY(sch,attitude,peg){
/* DOCUMENT:  xyz_in_BODY(sch,attitude,peg)
 */
 return matmatseq(xyz_in_sch(sch, peg), sch_in_BODY(attitude));}

/*--------------------------------------------------------------------*/

func BODY_in_xyz(sch,attitude,peg){
/* DOCUMENT:  BODY_in_xyz(sch,attitude,peg)
 */
 return transpose(xyz_in_BODY(sch,attitude,peg),[1,2]);}

/*--------------------------------------------------------------------*/

func axisrot(rot,&axis,&angle){
/* DOCUMENT: axisrot(rot,&axis,&angle)
   rot == arbitary rotation matrix (could be a sequence of rotations a(3,3,N,M,I...)
   func (subroutine), find the Euler axis of rotation and the rotation angle
   about that axis.
 */

 trace= diag(rot,[1,2])(sum,..);
 w3= trace>3.;
 w1= trace<-1.;
 wg= (!w3)&(!w1);
 iw3= where(w3);
 iw1= where(w1);
 iwg= where(wg);

 if(numberof(iw3))trace(iw3)= 3.;
 if(numberof(iw1))trace(iw1)= 3.;
 cosangle= (trace-1)/2;
 angle= acos(cosangle);

 axis= array(structof(rot),dimsof(rot(1,..)));

 if(numberof(iwg)){
   sinrot= sin(angle(iwg));
   axis(1,iwg)= (.5/sinrot)*(rot(2,3,iwg) - rot(3,2,iwg));
   axis(2,iwg)= (.5/sinrot)*(rot(3,1,iwg) - rot(1,3,iwg));
   axis(3,iwg)= (.5/sinrot)*(rot(1,2,iwg) - rot(2,1,iwg));
 }
 if(numberof(iw1)){
   axis(1,iw1)= sqrt((1. + rot(1,1,iw1))/2.);
   axis(2,iw1)= sqrt((1. + rot(2,2,iw1))/2.);
   axis(3,iw1)= sqrt((1. + rot(3,3,iw1))/2.);
   w= where(rot(1,2,iw1) < 0);
   if(numberof(w)){
     w2= where(rot(2,3,iw1(w)) <= 0);
     if(numberof(w2))axis(1,iw1(w(w2))) *= -1;
     w2= where(rot(2,3,iw1(w)) > 0);
     if(numberof(w2))axis(2,iw1(w(w2))) *= -1;
   }
   w= where(rot(1,2,iw1) > 0);
   if(numberof(w)){
     w2= where(rot(2,3,iw1(w)) <= 0);
     if(numberof(w2))axis(3,iw1(w(w2))) *= -1;
   }
   w= where(rot(1,2,iw1) == 0);
   if(numberof(w)){
     w2= where(axis(1,iw1(w)) == 0);
     if(numberof(w2))w4= where(rot(2,3,iw1(w(w2))) < 0);
     if(numberof(w4))axis(2,iw1(w(w2(w4)))) *= -1;
     w2= where(axis(1,iw1(w)) != 0);
     if(numberof(w2))w4= where(rot(3,1,iw1(w(w2))) < 0);
     if(numberof(w4))axis(2,iw1(w(w2(w4)))) *= -1;
   }
 }
 if(numberof(iw3)){
   axis(1,iw3)= 0.;
   axis(2,iw3)= 0.;
   axis(3,iw3)= 0.;
 }
}

/*--------------------------------------------------------------------*/

func sch_in_sch(schi,schj,pegi,pegj,seq=){
/* DOCUMENT: sch_in_sch(schi,schj,peg,seq=)
   EITER schi(j) has dimsof(schi(j))==[1,3]  AND schj(i) has dimsof(schj(i))==[n,3,..]
   OR (seq=1) schi abd schj havre id. dimension [n,3,..] and the results is "one to one"
   ! same peg !
 */
 if(!is_void(seq)&&seq==1){
   return matmatseq(sch_in_XYZ(schi,pegi),XYZ_in_sch(schj,pegj));
 }else{
   return sch_in_XYZ(schi,pegi)(,+,..)*XYZ_in_sch(schj,pegj)(+,..);
 }
}

/*--------------------------------------------------------------------*/

func sch_to_sch(sch, pegin, pegout){
/* DOCUMENT:sch_to_sch(sch, pegin, pegout)
 */
 return XYZ_to_sch(sch_to_XYZ(sch,pegin),pegout);}

/*--------------------------------------------------------------------*/

func schvel_to_schvel (schvel, sch, pegin, pegout)
/* DOCUMENT: schvel_to_schvel (schvel, sch, pegin, pegout)
 */
{
  XYZvel= schvel_to_XYZvel(schvel,sch,pegin);
  schout= sch_to_sch(sch,pegin,pegout);
  return XYZvel_to_schvel(XYZvel, schout, pegout);
}

/*--------------------------------------------------------------------*/

func XYZvel_to_schvel (XYZvel, sch, peg)
/* DOCUMENT: XYZvel_to_schvel (XYZvel,sch, peg)
 */
{
  return matvecseq(sch_in_XYZ(sch,peg),XYZvel)
}

/*--------------------------------------------------------------------*/

func schvel_to_XYZvel (schvel, sch, peg)
/* DOCUMENT: schvel_to_XYZvel (schvel, sch, peg)
 */
{
  return matvecseq(XYZ_in_sch(sch,peg),schvel)
}

/*--------------------------------------------------------------------*/

func schypr_to_schypr (schypr, schi, pegin, pegout)
/* DOCUMENT: schvel_to_XYZvel (schvel, sch, peg)
 */
{
  scho= sch_to_sch(schi, pegin, pegout);
  bodsch= BODY_in_sch(schypr);
  schio= sch_in_sch(schi,scho,pegin,pegout,seq=1);
  mat= matmatseq(bodsch,schio);  //scho_bod
  mat(2,1,..) *= -1;      //flipwings (right to left) and nadir to zenith
  mat(3,1,..) *= -1;      // flipping TWICE: ijk->IJK and "TCN"->sch
  mat(1,2,..) *= -1;
  mat(1,3,..) *= -1;

  return eulerRot(mat,[3,2,1]);
}

/*--------------------------------------------------------------------*/

func schposvec (sch1, sch2, peg)
/* DOCUMENT: schposvec(sch1,sch2,peg)
   returns the r21= r2-r1 vector in sch1 basis
   this works for one sch1 and one or many sch2(3,..)
   WATCH confrmability a bit quirky: iff nonconformable
   the non-leading dimensions of sch2 are appended to
   those of sch1.
  provide peg or only radius of curvature (peg.rad)
 */
{
  d1= dimsof(sch1);
  d2= dimsof(sch2);
  d12= dimsof(sch1,sch2);
  if (noneof(d12)) {
    d12= _(d1(1)+d2(1)-1,3,array(1,d1(1)-1),d2(3:));
    sch2= reform(sch2, d12);
  } else {
    sch2+= array(structof(sch2(1)), d12);
  }
  mm= sch_in_xyz(sch1,peg);
  dx= sch_to_xyz(sch2, peg) - sch_to_xyz(sch1, peg);
  return matvecseq(mm,dx);
}
/*--------------------------------------------------------------------*/

func posvecsch (sch,posvec,peg)
/* DOCUMENT: posvecsch(sch,posvec,peg)
   returns the sch coordinate(s) for a point with
   posvec "tangent plane" sch coordinates
  provide peg or only radius of curvature (peg.rad)
 */
{
  posvec_xyz= xyz_in_sch(sch,peg)(,+,..)*posvec(+,..);

  return xyz_to_sch(sch_to_xyz(sch, peg) + posvec_xyz, peg);
}

/*--------------------------------------------------------------------*/

func enu_in_XYZ (latlon)
/* DOCUMENT: enu_in_XYZ(latlon)
   Takes a lat and lon and returns a change of basis matrix from
   geocentric coordinates to ENU.
 */
{
  return transpose(XYZ_in_enu(latlon),[1,2]);
}

/*--------------------------------------------------------------------*/

func enu_in_sch (sch ,peg)
/* DOCUMENT enu_in_sch(sch ,peg) This routine computes the transformation
   matrix from a local SCH frame to ENU coordinates.
 */
{
  return matmatseq(enu_in_XYZ(sch_to_llh(sch,peg)(1:2,..)),XYZ_in_sch(sch, peg));
}

/*--------------------------------------------------------------------*/

func sch_in_enu (sch,peg)
/* DOCUMENT sch_in_enu(sch,peg) This routine computes the transformation
   matrix from ENU coordinates to local SCH frames.
 */
{
  return transpose(enu_in_sch(sch ,peg),[1,2]);
}

/*--------------------------------------------------------------------*/

func enu1_in_enu2 (ll2,ll1)
/* DOCUMENT: enu1_in_enu2(ll2,ll1)
   ENU basis transformation between to ll1(1,..)-ll1(2,..)
   points [ ll == ll1(1,..)_ll1(2,..) == (2,..)]

 */
{
  clt1= cos(ll1(1,..));
  slt1= sin(ll1(1,..));
  clo1= cos(ll1(2,..));
  slo1= sin(ll1(2,..));

  clt2= cos(ll2(1,..));
  slt2= sin(ll2(1,..));
  clo2= cos(ll2(2,..));
  slo2= sin(ll2(2,..));

  t1=  ll2(2,..) - ll1(2,..);
  if(dimsof(t1)(1)==0)t1=[t1];
  w= where(abs(t1) > pi);
  if(numberof(w)&&anyof(w))t1(w)= (abs(t1(w))-2*pi)*sign(t1(w));

  cdel= cos(t1);
  sdel= sin(t1);

  if(dimsof(ll1)(1)>1){
    d= dimsof(ll1);
    if(dimsof(ll2)(1)>1)error,"only one of ll1/ll2 can be an array of vectors";
  }else if(dimsof(ll2)(1)>1){
    d= dimsof(ll2);
  }else{
    d= [1,3];
  }

  if(d(1)==1){
    mat= array(double, [2,3,3]);
  }else{
    mat= array(double, _(d(1)+1,3,d(2:0)));
  }

  mat(1,1,..)= cdel;
  mat(1,2,..)= -slt2*sdel;
  mat(1,3,..)= clt2*sdel;

  mat(2,1,..)= slt1*sdel;
  mat(2,2,..)= slt1*slt2*cdel + clt1*clt2;
  mat(2,3,..)= -clt2*slt1*cdel + slt2*clt1;

  mat(3,1,..)= -clt1*sdel;
  mat(3,2,..)= -slt2*clt1*cdel + clt2*slt1;
  mat(3,3,..)= clt1*clt2*cdel + slt1*slt2;

  return mat;
}

/*---------------------------------------------------------------------------*/

func tgtXYZ_from_lookXYZ (XYZ, lookXYZ, &range ,badval=)

/* DOCUMENT tgtXYZ= intersect( XYZ,           // XYZ position of XYZ
                                lookXYZ,       // look vector in XYZ cordinates
                                &range,        // range from XYZ to tgtXYZ
                                badval= )     // value where no solution
                                               // tgtXYZ on ellipsoid in XYZ coordinates
   from Scott Hensley)

   propagates the look vector of the XYZ down to an ellipsoidal planet surface.
 */
{
  if(is_void(badval))badval= 0.0;
  maj2= elp.majax^2;

  tgtXYZ= array(badval, dimsof(XYZ, lookXYZ));
  min2= array(maj2*(1.0 - elp.eccentsq1), dimsof(tgtXYZ(1,..)));  // for conformability

  a= ((lookXYZ(1,..)^2 + lookXYZ(2,..)^2)/maj2)  + lookXYZ(3,..)^2/min2;
  b= 2*((lookXYZ(1,..)*XYZ(1,..) + lookXYZ(2,..)*XYZ(2,..))/maj2 + lookXYZ(3,..)*XYZ(3,..)/min2);
  c= (XYZ(1,..)^2 + XYZ(2,..)^2)/maj2 + XYZ(3,..)^2/min2 - 1.0;

  range= rangep= rangem= array(badval, dimsof(b));

  radical= (b^2 - 4.0*a*c);
  msk= radical>=0;
  if(anyof(msk)){
    wt= where(radical>=0);
    rangep(wt)= -( b(wt) + sqrt(radical(wt)) )/(2*a(wt));  // not sure why that root ....
    rangem(wt)= -( b(wt) - sqrt(radical(wt)) )/(2*a(wt));  // not sure why that root ....
    if(is_scalar(range)){
      range(wt)= [rangep(wt),rangem(wt)](min);
    }else{
      range(wt)= [rangep(wt),rangem(wt)](,min);
    }
    //  range(wt)= -( b(wt) + sqrt(radical(wt)) )/(2*a(wt));  // not sure why that root ....
  }
  tgtXYZ= merge2(XYZ+range(-,)*lookXYZ, array(badval, 3), transpose(array(msk,3),2));

  return tgtXYZ;
}

/*---------------------------------------------------------------------------*/

func lookXYZ_from_range (posXYZ, XYZant, range, geoid, &nrts, badval=, sinsign=, twosides=)
/* DOCUMENT: lookXYZ= lookant_from_range (
                     posXYZ,           // XYZ position of platform
                     XYZant,           // antenna to XYZ Change of Basis matrix
                     range,            // Range from platform to target: input
                     sinsign=,         // which sign root (sinang) is requested +1 is default
                     boostell=         // Boost the size of the ellipsoid by that factor
                                       // MAJORAXIS *= boostell.  !! conform with range
                     twosides=         // 0 or 1,  sic
                     badval=)
   This routine computes the elevation look angle
  to intersect ellipsoid at given range and assumed zero azimuth angle.
  This should ;) work with  CONFORMABLE (up-to-1:2:3 components which are handeled)
  posXYZ, XYZant, and range.  That is range & sinsign & boostell conformable with posXYZ(1,..) & XYZant(1,1,..)
  IMPORTANT NOTE: if(twosides) a TRAILING dimension of size 2 is
        added to conformed range+XYZant inputs. Index==1 <- root >0,  Index==2 <- root <0

  adapted from Eric Gurrola
*/
{
  if(is_void(badval))badval= 0.0;
  if(is_void(geoid))geoid= 0.0;

  posLlh= XYZ_to_llh(posXYZ);

  boostell= (1+geoid/(elp.majax*sqrt(1-sin(posLlh(1,1,..))^2*elp.eccentsq1)))(-,..)

  lookant= array(badval, dimsof(posXYZ))*array(1.0,dimsof(range(-,)));
  d5= dx= dimsof(lookant);
  d5(2)= 5;

  ee= array((1.0 - elp.eccentsq1), dimsof(lookant(1,)));
  rat= ee*(elp.majax*boostell)^2/range^2;   // (R_pol/Range)^2
  P= posXYZ/range(-,);  //broadcasting ...

  A= ee*(XYZant(1,2,..)^2 + XYZant(2,2,..)^2 - XYZant(1,3,..)^2 - XYZant(2,3,..)^2) + XYZant(3,2,..)^2 - XYZant(3,3,..)^2;

  B= 2*(ee*(P(1,..)*XYZant(1,2,..) + P(2,..)*XYZant(2,2,..)) + P(3,..)*XYZant(3,2,..));

  C= 2*(ee*(XYZant(1,2,..)*XYZant(1,3,..) + XYZant(2,2,..)*XYZant(2,3,..)) + XYZant(3,2,..)*XYZant(3,3,..));

  D= 2*(ee*(P(1,..)*XYZant(1,3,..) + P(2,..)*XYZant(2,3,..)) + P(3,..)*XYZant(3,3,..));

  E= rat - ee*(P(1,..)^2 + P(2,..)^2) - P(3,..)^2 - ee*(XYZant(1,3,..)^2+XYZant(2,3,..)^2) - XYZant(3,3,..)^2;

  F= array(double, d5);
  F(1,..)= E^2 - D^2;
  F(2,..)= -2*(C*D + E*B);
  F(3,..)= B^2 + D^2 - C^2 - 2*A*E;
  F(4,..)= 2*(A*B + C*D);
  F(5,..)= A^2 + C^2;

  badvv= 10000.0;
  if(!is_void(roots4)){
    z= roots4(F, nrts, badval=badvv);
  }else{
    //write,"we need roots4";
    d4= dx; d4(2)=4;
    z= array(10000.0,d4);
    for(i=1;i<=numberof(z(1,..));i++){
      zz= zroots(F(,*)(,i));
      wt= where(abs(zz.im)>1e-16);
      if(numberof(wt))zz(wt)= badvv;
      z(,*)(,i)= zz.re;
    }
  }
  dr= dimsof(z)(2);

  msk_p= z>=0 & z<1.0;
  msk1d_p= msk_p(sum,..);if(anyof(msk1d_p>1))error,"double positive root4 problem";
  msk_n= z<0 & z>-1.0;
  msk1d_n= msk_n(sum,..);if(anyof(msk1d_n>1))error,"double negative root4 problem";

  if(twosides && !is_void(sinsign)) error,"conflicting requests on requested root sign";

  if(twosides){
    //////positive roots
    lookant_p= lookant;
    w= where(msk1d_p);
    nw= numberof(w);
    if(nw){
      sinang= z(where(msk_p)); // assume only one is picked
      lll= transpose(_([array(0.0,nw)],[sinang],[sqrt(1-sinang^2)]),2);
      lookant_p(,w)= lll;
    }
    w= where(lookant_p==badval);
    lookant_p= matvecseq(XYZant, lookant_p);  // makes this into lookXYZ  //broadcasting PB...
    if(is_array(w))lookant_p(w)=badval;
    //////next negative roots
    lookant_n= lookant;
    w= where(msk1d_n);
    nw= numberof(w);
    if(nw){
      sinang= z(where(msk_n)); // assume only one is picked
      lll= transpose(_([array(0.0,nw)],[sinang],[sqrt(1-sinang^2)]),2);
      lookant_n(,w)= lll;
    }
    w= where(lookant_n==badval);
    lookant_n= matvecseq(XYZant, lookant_n);  // makes this into lookXYZ  //broadcasting PB...
    if(is_array(w))lookant_n(w)=badval;
    lookant= [lookant_p, lookant_n];
  }else{
    if(is_void(sinsign)){
      sinsign= array(int(1), dimsof(A));
    }else if(numberof(sinsign)==1){
      sinsign= array(int(nint(sinsign(1))), dimsof(A));
    }else{
      sinsign= int(nint(sinsign));
    }
    w_p= where(sinsign>0);
    if(is_array(w_p))msk_n(,wp)=0;  // remove umwanted GOOD roots
    w_n= where(sinsign<0);
    if(is_array(w_n))msk_p(,wn)=0;
    msk1d= (msk_p+msk_n)(sum,);if(anyof(msk1d>1))error,"double root4 problem";
    w= where(msk1d);

    if(is_array(w)){
      sinang= merge(z(where(msk_p)),z(where(msk_n)),sinsign(w)>0);
      lookant(1,w)= 0.0;
      lookant(2,w)= sinang;           //  <0  for right looking,  >0 for left looking
      lookant(3,w)= sqrt(1-sinang^2);
      lookant(,w)= matvecseq(XYZant, lookant(,w));  // makes this into lookXYZ
    }
    w= where(!msk1d);
    if(is_array(w))lookant(,w)=badval;
  }
  return lookant;

}

/*---------------------------------------------------------------------------*/

func BODY_in_antface (yawpitchroll)
/* DOCUMENT: BODY_in_antface (yawpitchroll(1:3,..))(1:3,1:3,..)

   converts a TCN Euler angle sequence into a change of basis: matrix antface to BODY.
   Rotations takes the BODY (~SCH) fixed (1st idx) frame (IJK) into the antface
   frame (2nd idx).

   Frame Defs:
    (1)  BODY fixed frame oriented so that
         I=   BFX= oriented along fulselage
         J=   BFY= oriented along left wing
         K=   BFZ= oriented alond upward body fixed vector

         j= -J  ,  k= -K       S= T , C= -C ,  H= -N

    (2)  Antenna Face frame   (SCH- or BODY- like)
          U along track-azimuth,
          Br boresight direction but towards RIGHT (standing up looking forwards ;)
          Vn NADIR-range

    BODY_in_antface= Fw*Fz* rotEuler(ypr, [3,2,1])  (Fw==Flip wings, Fz==Flip zenith)
    antface_in_BODY=  rotEuler(-rpy, [1,2,3]) *Fw*Fz (Fw==Flip wings, Fz==Flip zenith)

    Yaw Pitch Roll take ijk (lowercase!) into antenna-face ijk-like coor [nose, boresight RIGHT, DOWN]
 */
{
  mat= rotEuler(-yawpitchroll(::-1,..));  // rot3(-Y)*rot2(-P)*rot1(-R)
                                           // == transpose(rotEuler(yawpitchroll,[3,2,1]))
                                           // [rot1(R)*rot2(P)*rot3(Y)]T == 1st line

//  mat(2,1,..) *= -1;      //flipwings TWICE (right to left) and nadir to zenith
//  mat(3,1,..) *= -1;      //
//  mat(1,2,..) *= -1;      //
//  mat(1,3,..) *= -1;      //

  mat(2,1,..) *= -1;      // body->BODY flip ONCE wings (right to left) and nadir to zenith
  mat(3,1,..) *= -1;
  mat(2,2,..) *= -1;
  mat(3,2,..) *= -1;
  mat(2,3,..) *= -1;
  mat(3,3,..) *= -1;

  return mat;
}

/*---------------------------------------------------------------------------*/

func antface_in_BODY(yawpitchroll){
/* DOCUMENT: antface_in_BODY(yawpitchroll(1:3,..))(1:3,1:3,..)

   SEE ALSO: BODY_in_antface
 */
 return transpose(antface_in_BODY(yawpitchroll),[1,2]);}

/*---------------------------------------------------------------------------*/

func elecface_in_antface(azelangle){
/* DOCUMENT:
   SEE ALSO: antface_in_elecface(azelangle)
 */
 return transpose(antface_in_elecface(azelangle),[1,2]);}

/*---------------------------------------------------------------------------*/

func antface_in_elecface(azelangle){
/* DOCUMENT:

    elecface_in_antface= rotEuler(elaz, [1,3])   !!! TRM SCHEME see kuxyz_azel!!!
    antface_in_elecface= rotEuler(-azel, [3,1])

   TEST:  elecface_in_antface([pi/4,pi/4])(2,) == elecface_in_antface([pi/4,pi/4])(2,)
   SEE ALSO: elecface_in_antface(azelangle)
   elecface/antface are [ ~Along_track, boresight_LEFT, ~short_antenna_axis_ZENITH]

 */
 azelangle= azelangle([1,2,2],..);
 azelangle(2,..)= 0;
 mat= rotEuler(-azelangle,[3,2,1]);

 mat(2,1,..) *= -1;      //flipwings (right to left) and nadir to zenith
 mat(3,1,..) *= -1;      // flipping TWICE: antface  & elecface
 mat(1,2,..) *= -1;
 mat(1,3,..) *= -1;

 return mat;}

/*---------------------------------------------------------------------------*/

func azel_kaf(kaf){
/* DOCUMENT azel_kaf(kaf)
   given the ANTENA FACE  coordinates of a UNIT direction
   scompute azimuth and elevation angles.
   [angles in RADIANS]
   SEE ALSO: kuxyz_azel
 */
 return azel_kuxyz(kaf([1,3,2],..));}

/*---------------------------------------------------------------------------*/

func kaf_azel(azelangle){
/* DOCUMENT kaf_azel(azelangle)
   gets ANTENA FACE (along track-azimuth,boresight_left,zenith-range)
   coordinates of a UNIT direction
   specified by an azimuth and elevation angle
   angel are in RADIANS
   TO ACTIVATE: uncomment the desired piece of code
   Test azel_kuxyz(kuxyz_azel([pi/4,pi/4])) - [pi/4,pi/4]
   SEE ALSO: azel_kuxyz
 */
  return kuxyz_azel(azelangle)([1,3,2],..);
}

/*---------------------------------------------------------------------------*/

func kuxyz_azel(azelangle){
/* DOCUMENT kuxyz_azel([az_angle,el_angle])
   defines the XYZ projections of a UNIT direction
   specified by an azimuth and elevation angle
   angel are in RADIANS
   The xyz in kuxyz is NOT in the Antenna Face,  z==boresight (NOT +/-Y for Left/Right looking == AF)
   TO ACTIVATE: uncomment the desired piece of code
   SEE ALSO: azel_kuxyz
 */
  din= dimsof(azelangle);
  if(din(2)!=2)error,"expecting two angles: AZIMUTH and ELEVATION";
  dout= din;
  dout(2)= 3;
  out= array(double, dout);
// TRM
// Theta : (azimuth or AZ) is the angle between the observation direction
//         and its orthogonal projection in the YZ plane. (its also
//         the complementary angle, pi/2-a,  of the angle "a" between
//         the observation direction and the X direction)
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the YX plane and the Z direction.
//
  out(1,..)= sin(azelangle(1,..));
  out(2,..)= cos(azelangle(1,..))*sin(azelangle(2,..));
  out(3,..)= cos(azelangle(1,..))*cos(azelangle(2,..));
///////////////////////////////////////////////////////////////////////////
// AFRL's angles !!! trm's X Y Z == AFRL's X -Z Y
// Theta : (azimuth or AZ) is the angle between the observation direction
//         and the short axis of the antenna panel Y
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the XZ plane and the X direction.
//
//  out(1,..)= sin(azelangle(2,..))*cos(azelangle(1,..));
//  out(2,..)= cos(azelangle(2,..));
//  out(3,..)= sin(azelangle(2,..))*sin(azelangle(1,..));
///////////////////////////////////////////////////////////////////////////
// ERIC's angles
// Theta : (azimuth or AZ) is the angle between the perp-projection of the observation/look direction
//         in the XZ-antenna face and the along track axis of the antenna panel X
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the AF-YZ  plane and the AF-Z direction.
//
//   den= sqrt( 1.0 - cos(azelangle(2,..))^2*sin(azelangle(1,..))^2);
//
//   out(1,..)= sin(azelangle(2,..))*sin(azelangle(1,..))/den;
//   out(2,..)= sin(azelangle(2,..))*cos(azelangle(1,..))/den;
//   out(3,..)= cos(azelangle(2,..))*cos(azelangle(1,..))/den;

 return out;}

/*---------------------------------------------------------------------------*/

func azel_kuxyz(kuxyz){
/* DOCUMENT azel_kuxyz(kuxyz)
   given the XYZ projections of a UNIT direction
   scompute the azimuth and elevation angles.
   angel are in RADIANS
   TO ACTIVATE: uncomment the desired piece of code
   Test: azel_kuxyz(kuxyz_azel([pi/4,pi/4])) - [pi/4,pi/4]

   SEE ALSO: kuxyz_azel
 */
  din= dimsof(kuxyz);
  if(din(2)<2||din(2)>3)error,"expecting two angles: AZIMUTH and ELEVATION";
  dout= din;
  dout(2)= 2;
  out= array(double,dout);
// TRM
// Theta : (azimuth or AZ) is the angle between the observation direction
//         and its orthogonal projection in the YZ plane. (its also
//         the complementary angle, pi/2-a,  of the angle "a" between
//         the observation direction and the X direction)
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the YX plane and the Z direction.
//
  out(1,..)= asin(kuxyz(1,..));
  out(2,..)= asin(kuxyz(2,..)/sqrt(1-kuxyz(1,..)^2));
///////////////////////////////////////////////////////////////////////////
// AFRL's angles !!! trm's X Y Z == AFRL's X -Z Y
// Theta : (azimuth or AZ) is the angle between the observation direction
//         and the short axis of the antenna panel Y
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the XZ plane and the X direction.
//
// out= [acos(kuxy(1)/sqrt(1-kuxy(2)^2)),acos(kuxy(2))];
///////////////////////////////////////////////////////////////////////////
// ERIC's angles
// Theta : (azimuth or AZ) is the angle between the perp-projection of the observation/look direction
//         in the XZ-antenna face and the along track axis of the antenna panel X
//
// Phi : (elevation or EL) is the angle between the orthog. projection
//       of the observation direction in the AF-YZ  plane and the AF-Z direction.
//
// out(1,..)= sign(kuxyz(1,..))*acos(kuxyz(1,..)/abs(kuxyzv(1,..),kuxyz(2,..)))
// out(2,..)= sign(kuxyz(2,..))*acos(kuxyz(3,..)/abs(kuxyzv(2,..),kuxyz(3,..)))
 return out;}

/*---------------------------------------------------------------------------*/

func sheading (s,peg)
/* DOCUMENT: sheading(s,peg)
  func computes the reference
  path track angle (sometimes called heading) at a point s. Explicitly,
  angle of the tangent vector to the reference path makes
  with respect to the north axis at postion.
 */
{
  //get lat,lon of point
  ds= dimsof(s);
  if(ds(1)>1){
    sch= array(double,_(ds(1)+1,3,ds(2:)));
  }else{
    sch= array(double,3);
  }
  tv1= sch;
  sch(1,..)= s;
  llh= sch_to_llh(sch,peg);

  //tangent vector in enu frame
  tv1(1,..)= cos(s/peg.rad)*sin(peg.hdg);  //unit vector in ENU @ peg
  tv1(2,..)= cos(s/peg.rad)*cos(peg.hdg);
  tv1(3,..)= -sin(s/peg.rad);

  //conversion from pegged ENU frame to point on reference track ENU frame
  mat= enu1_in_enu2([peg.lat,peg.lon],llh([1,2],..));
  tv2= mat(,+,..)*tv1(+,..); //tangent @ s in ENU @ s

  //compute reference heading
  return atan(tv2(1,..),tv2(2,..));
}

/*---------------------------------------------------------------------------*/

func normimgpln(att){
/* DOCUMENT:
        this routine computes the normal to the
        imaging plane.
        att= [YAW, PITCH, ROLL(not used)]
        The normal can be dotted with a look vector in tangent
        plane sch [see schposvec] @ platform/antenna to determine
        if a point is in the imaging plane
        NOTE: strictly speaking, the attitude should be
              some sort of weighted average over the aperture length...
 */
  out= att;
  out(1,..)= cos(att(2,..))*cos(att(1,..))
  out(2,..)= -cos(att(2,..))*sin(att(1,..))
  out(3,..)= sin(att(2,..))
  return out;
}

/*--------------------------------------------------------------------*/

func sch_to_utm(sch, peg,&zone,&grid,ne=){
/* DOCUMENT:ssch_to_utm(sch, peg,&zone,&grid,ne=)
 */
 llh= sch_to_llh(sch, peg);
 utm= llh;
 utm(1:2,..)= latlon_to_utm(llh(1:2,..),zone,grid,ne=ne);
 return utm;
}

/*--------------------------------------------------------------------*/

func utm_to_sch(utm, zone, grid, peg, ne=){
/* DOCUMENT: utm_to_sch(utm, zone, grid, peg, ne=)
 */
 sch= utm;
 sch(1:2,)= utm_to_latlon(sch(1:2,),zone,grid,ne=ne);
 sch(3,..)= utm(3,..);
 return llh_to_sch(sch, peg);
}

/*-------------------------------------------------------------------------------------------*/

func latlon_to_utm(ll,&zone,&grid,deg=,dms=,ne=){
/* DOCUMENT: latlon_to_utm(ll,&zone,&grid,deg=,dms=,ne=)
                          translated from Scott Hensley's f77 utmtoll.f;
   ;
   FUNCTIONAL DESCRIPTION: This routine converts between lat;
   lon and utm coordinates for a datum determined from the input ;
   a and e2.;
   a                       !ellispoid semi-major axis;
   e2                      !ellipsoid eccentricity squared  ;
   v(2)                    !Easting , Northing UNLESS ne=1 then NORTHING EASTING
   lat                     !latitude (deg -90 to 90);
   lon                     !longitude (deg -180 to 180);
   zone                    !UTM zone;UPPERCASE !!!!
   grid                    !UTM North-South grid;

   dms==1 means ll= [320856,-772803]
   check at http://www.dmap.co.uk/ll2tm.htm
 */

  if(ne==1){
    ieast= 2;
    inorth= 1;
  }else{
    ieast= 1;
    inorth= 2;
  }

  GRIDARR= ['C','D','E','F','G','H','J','K','L','M','N','P','Q','R','S','T','U','V','W','X'];
  K0= .99960 ;   //scale at center ;
  LAT0= 0.0;
  FE= 500000.0;
  FN= [0.0,10000000.0];

  a= elp.majax;
  e2= elp.eccentsq1;

  ep2= e2/(1.0 - e2);
  e4= e2^2;
  e6= e2^3;

  lat= ll(1,*);
  lon= ll(2,*);

  dv= dimsof(ll);
  shrk1= 0;
  if(dv(1)==1){
    dv= _(dv,1);
    dv(1)= 2;
    shrk1= 1
  }
  v= array(double,dv);

  if(dms==1){
    lat= dms2deci(lat)*RADEG;
    lon= dms2deci(lon)*RADEG;
    deg= 0;
  }

  if(deg==1){
    lat= lat*RADEG;
    lon= lon*RADEG;
  }

  zone= long(((lon+3.0*pi)%(2.0*pi))/(RADEG*6.0)) + 1;
  w= where(zone<1);if(numberof(w))zone(w)= 1;
  w= where(zone>60);if(numberof(w))zone(w)= 60;

  lon0= -pi + 6.0*RADEG*(zone-1) + 3.0*RADEG;

  n= a/sqrt(1.0 - e2*sin(lat)^2);
  t= tan(lat)^2;
  t2= t^2;
  c= ep2*cos(lat)^2;
  ba= (lon - lon0)*cos(lat);
  a2= ba^2;
  a3= ba*a2 ;
  a4= ba*a3;
  a5= ba*a4;
  a6= ba*a5;
  m= a*((1.0-e2/4 - 3.0*e4/64.0 - \
      5.0*e6/256.0)*lat - (3.0*e2/8.0 + 3.0*e4/32.0 + \
      45.0*e6/1024.0)*sin(2.0*lat) +  (15.0*e4/256.0 + \
      45.0*e6/1024.0)*sin(4.0*lat) - (35.0*e6/3072.0)*sin(6.0*lat));

  m0= a*((1.0-e2/4 - 3.0*e4/64.0 - \
       5.0*e6/256.0)*LAT0 - (3.0*e2/8.0 + 3.0*e4/32.0 + \
       45.0*e6/1024.0)*sin(2.0*LAT0) +  (15.0*e4/256.0 + \
       45.0*e6/1024.0)*sin(4.0*LAT0) - (35.0*e6/3072.0)*sin(6.0*LAT0));

  v(ieast,*)= K0*n*(ba+(1.0-t+c)*a3/6.0 + (5.0-18.0*t+t2+72.0*c-58.0*ep2)*a5/120.0);
  v(ieast,..) += FE;

  v(inorth,*)= K0*(m - m0 + n*tan(lat)*( a2/2.0 + (5.0-t+9.0*c+4.0*c^2)* \
        (a4/24.0) + (61.0-58.0*t+t2+600.0*c - 330.0*ep2)*(a6/720.0) ));

  ww= lat>0;
  if(anyof(ww)){
     w= where(ww);
     v(inorth,w) += FN(1);
  }

  if(anyof(!ww)){
     w= where(!ww);
     if(grid=='A'){
     }else if(grid =='Z'){
        v(inorth,w) += 2.0*FN(2);
     }else{
        v(inorth,w) += FN(2);
     }
  }

  gi= long((lat/RADEG+80.0)/8.0) + 1;
  w= where(gi<1);if(numberof(w))gi(w)= 1;
  w= where(gi>20);if(numberof(w))gi(w)= 20;

  grid= spell(string(&char(GRIDARR(gi))));

  if(shrk1==1)v= v(..,1);

  return v; //  (EASTING:NORTHING,..)
}

/*-------------------------------------------------------------------------------------------*/

func utm_to_latlon (v0,zone,grid,deg=,dms=,ne=)
/* DOCUMENT: utm_to_latlon(v,zone,grid,deg=,dms=)
                          translated from Scott Hensley's f77 utmtoll.f;
   ;
   FUNCTIONAL DESCRIPTION: This routine converts between lat;
   lon and utm coordinates for a datum determined from the input ;
   a and e2.;
   a                       !ellispoid semi-major axis;
   e2                      !ellipsoid eccentricity squared  ;
   v(2)                    !Easting , Northing switch if ne==1;
   lat                     !latitude (deg -90 to 90);
   lon                     !longitude (deg -180 to 180);
   zone                    !UTM zone; UPPERCASE !!!!
   grid                    !UTM North-South grid;

   dms==1 means ll= [320856,-772803]
   check at http://www.dmap.co.uk/ll2tm.htm
 */
{
  if(ne==1){
    ieast= 2;
    inorth= 1;
  }else{
    ieast= 1;
    inorth= 2;
  }

  v= v0; //local copy

  K0= .99960 ;   //scale at center ;
  LAT0= 0.0;
  FE= 500000.0;
  FN= [0.0,10000000.0];

  a= elp.majax;
  e2= elp.eccentsq1;

  ep2= e2/(1.0 - e2);
  e4= e2^2;
  e6= e2^3;

  gridlong= long(strchararr(grid));

  dl= dimsof(v);
  shrk1= 0;
  if(dl(1)==1){
    dl= _(dl,1);
    dl(1)= 2;
    shrk1= 1
  }

  ll= array(double,dl);

  v(ieast,..) -= FE;

  ww= (grid =='Z')&(v(inorth,) >= FN(2));
  if(anyof(ww)){
     w= where(ww);
     v(inorth,w) -= 2.0*FN(2)  ;
  }

  ww= (gridlong >=  long(*pointer("C"))(1))&(gridlong <=  long(*pointer("M"))(1));
  if(anyof(ww)){
     w= where(ww);
        v(inorth,w) -= FN(2);
  }                 // else assume Northern hemisphere;

  lon0= -pi + 6.0*RADEG*(zone-1) + 3.0*RADEG;

  et= sqrt(1.0-e2);
  e1= (1.0-et)/(1.0+et);
  e12= e1^2;
  e13= e1*e12;
  e14= e1*e13;

  m= v(inorth,..)/K0;

  mu= m/(a*(1.0-e2/4.0-3.0*e4/64.0-5.0*e6/256.0));

  lat1= mu + (3.0*e1/2.0-27.0*e13/32.0)*sin(2.0*mu) + \
              (21.0*e12/16.0-55.0*e14/32.0)*sin(4.0*mu) +  \
              (151.0*e13/96.0)*sin(6.0*mu) +(1097.0*e14/512.0)*sin(8.0*mu) ;

  n= a/sqrt(1.0 - e2*sin(lat1)^2);
  r= (a*(1.0-e2))/sqrt(1.0 - e2*sin(lat1)^2)^3;
  t= tan(lat1)^2;
  t2= t^2;
  c= ep2*cos(lat1)^2;
  c2= c^2;
  d= v(ieast,..)/(n*K0);
  d2= d^2;
  d3= d2*d;
  d4= d3*d;
  d5= d4*d;
  d6= d5*d;

  ll(1,*)= lat1 - (n*tan(lat1)/r)*(d2/2.0 - \
        (5.0+3.0*t+10.0*c-4.0*c2-9.0*ep2)*d4/24.0 + \
        (61.0+90*t+298.0*c+45.0*t2-252.0*ep2-3.0*c2)* \
        (d6/720.0));
  ll(2,*)= lon0 + (d - (1.0+2.0*t+c)*d3/6.0 + \
        (5.0-2.0*c+28.0*t-3.0*c2+8.0*ep2+24.0*t2)*\
        (d5/120.0))/cos(lat1);

  if(dms==1){
    ll= deci2dms(ll*DEGRA);
    deg= 0;
  }

  if(deg==1){
    ll= ll*DEGRA
  }


  if(shrk1==1)ll= ll(..,1);

  return ll; //  (LAT:LON,..)

}

/*---------------------------------------------------------------*/

func geodesicHdg (Ll1,Ll0,e2=,maj=)

/* DOCUMENT: geodesicHdg(startLl,endLl,e2=,maj=)
   history: trm <- Eric Gurrola <- geo_hdg.f by Scott Hensley

   FUNCTIONAL DESCRIPTION: This routine computes the heading along a geodesic
   for either an ellipitical or spherical earth given the initial latitude
   and longitude and the final latitude and longitude.

   NOTES: These results are based on the geo memo
      "Summary of Mocomp Reference Line Determination Study" , IOM 3346-93-163
   and the paper
      "A Rigourous Non-iterative Procedure for Rapid Inverse Solution of Very
       Long Geodesics" by E. M. Sadano, Bulletin Geodesique 1958

   ll1=[ 36.7045537544539,-118.3705622136666]*RADEG
   ll0=[36.9256953340567,-118.6961765111672]*RADEG
   geodesicHdg(ll1,ll0)*DEGRA+49.7153048375955

*/
{
  if (allof(Ll1==Ll0)) return pi/2;  //single point -> heading is 90...why not ;/

  lati= Ll1(1);
  loni= Ll1(2);
  latf= Ll0(1);
  lonf= Ll0(2);

  if (is_void(e2)) e2= elp.eccentsq1;
  if (is_void(maj)) maj= elp.majax;

  if (e2==0.0) {             // spherical formula
    sinlati= sin(lati);
    coslati= cos(lati);
    tanlatf= tan(latf);

    t1=  lonf - loni;
    abst1= abs(t1);
    if (abst1>pi) {
      t1= -(2*pi - abst1)*sign(t1);
    }

    sinlon= sin(t1);
    coslon= cos(t1);
    t2= coslati*tanlatf - sinlati*coslon;

    return atan(sinlon,t2);
  } else {                  // ellipsoid formulation
    first= 0;
    e= sqrt(e2);
    ome2= 1.0 - e2;
    sqrtome2= sqrt(ome2);
    f= 1.0 - sqrtome2;
    ep= e*f/(e2-f);
    n= f/e2;
    k1= (16.0*e2*n^2 + ep^2)/ep^2;
    k2= (16.0*e2*n^2)/(16.0*e2*n^2 + ep^2);
    k3= (16.0*e2*n^2)/ep^2;
    k4= (16.0*n - ep^2)/(16.0*e2*n^2 + ep^2);
    k5= 16.0/(e2*(16.0*e2*n^2 + ep^2));

    tanlati= tan(lati);
    tanlatf= tan(latf);
    l=  abs(lonf-loni);
    lsign= lonf - loni;
    if (abs(lsign)>pi) {
      lsign= -(2.0*pi-l)*sign(-lsign);
    }
    sinlon= sin(l);
    coslon= cos(l);

    tanbetai= sqrtome2*tanlati;
    tanbetaf= sqrtome2*tanlatf;

    cosbetai= 1.0/sqrt(1.0 + tanbetai^2);
    cosbetaf= 1.0/sqrt(1.0 + tanbetaf^2);
    sinbetai= tanbetai*cosbetai;
    sinbetaf= tanbetaf*cosbetaf;

    ac= sinbetai*sinbetaf;
    bc= cosbetai*cosbetaf;

    cosphi= ac + bc*coslon;
    sinphi= sign(sinlon)*sqrt(1.0 - min(cosphi^2,1.0));
    phi= abs(atan(sinphi,cosphi));

    if (maj*abs(phi)>1.0e-6) {
      ca= (bc*sinlon)/sinphi;
      cb= ca^2;
      cc= (cosphi*(1.0 - cb))/k1;
      cd= (-2.0*ac)/k1;
      ce= -ac*k2;
      cf= k3*cc;
      cg= phi^2/sinphi;

      x= ((phi*(k4 + cb) + sinphi*(cc + cd) +
              cg*(cf + ce))*ca)/k5;

      lambda= l + x;

      sinlam= sin(lambda);
      coslam= cos(lambda);

      t2= (tanbetaf*cosbetai - coslam*sinbetai);
      sinlon= sinlam*sign(lsign);

      geohdg= atan(sinlon,t2);

      return geohdg;
    } else {
      return 0.0;
    }
  }
}

/*---------------------------------------------------------------*/

func latLonGetPoint(Ll,dist,hdg){

/* DOCUMENT: geodesicHdg(startLl,endLl)
   history: trm <--Eric Gurrola <-- geo_hdg.f by Scott Hensley... Ouch

   returns a LatLon point from a certain
   heading, at a certain distance.
   Currently assumes geocentric earth,
   and dist is in meters

   This one is tough in general for an ellipsoid.
   For small distances (say 100 km) we can use
   a spherical approximation of the surface in
   the direction specified, orienting the sphere
   equator in that direction.

   trm says... this could be fixed easily: solve
   for actual distance (geodesicDist) no?
 */

  posXYZ= llh_to_XYZ([Ll(1), Ll(2), 0.0 ]);

  peg= fill_peg(pegpoint(lat=Ll(1),lon=Ll(2),hdg=hdg))

  posSch= XYZ_to_sch(posXYZ,peg);

  posSch(1) += dist;

  posLlh= XYZ_to_llh(sch_to_XYZ(posSch,peg));

  return(posLlh(1:2));

}

/*---------------------------------------------------------------*/

func geodesicDist(Ll1,Ll0){

/* DOCUMENT: geodesicHdg(startLl,endLl)
   history: trm <--Eric Gurrola <-- geo_hdg.f by Scott Hensley... Ouch

   FUNCTIONAL DESCRIPTION: This routine computes the distance along a geodesic
   for either an ellipitical or spherical earth given the initial latitude
   and longitude and the final latitude and longitude.

   NOTES: These results are based on the memo

     "Summary of Mocomp Reference Line Determination Study" , IOM 3346-93-163

   and the paper

     "A Rigourous Non-iterative Procedure for Rapid Inverse Solution of Very
      Long Geodesics" by E. M. Sadano, Bulletine Geodesique 1958
*/
  lati= Ll1(1);
  loni= Ll1(2);
  latf= Ll0(1);
  lonf= Ll0(2);

  e2= elp.eccentsq1;
  maj= elp.majax;

  if( e2 == 0 ){   // use the simpler spherical formula

    sinlati= sin(lati);
    coslati= cos(lati);
    sinlatf= sin(latf);
    coslatf= cos(latf);

    t1=  lonf - loni;
    abst1= abs(t1);
    if( abst1 > pi ){
      t1= (2.0*pi - abs(t1))*sign(-t1);
    }

    coslon= cos(t1);
    t2= coslati*coslatf*coslon + sinlati*sinlatf;

    geodis= maj*acos(t2);

    return geodis;

  } else{   // use the full ellipsoid formulation

    first= 0;
    e= sqrt(e2);
    ome2= 1.0 - e2;
    sqrtome2= sqrt(ome2);
    b0= maj*sqrtome2;
    f= 1.0 - sqrtome2;
    ep= e*f/(e2-f);
    n= f/e2;
    k1= (16.0*e2*n^2 + ep^2)/ep^2;
    k2= (16.0*e2*n^2)/(16.0*e2*n^2 + ep^2);
    k3= (16.0*e2*n^2)/ep^2;
    k4= (16.0*n - ep^2)/(16.0*e2*n^2 + ep^2);
    k5= 16.0/(e2*(16.0*e2*n^2 + ep^2));

    tanlati= tan(lati);
    tanlatf= tan(latf);
    l =  abs(lonf-loni);
    sinlon= sin(l);
    coslon= cos(l);

    tanbetai= sqrtome2*tanlati;
    tanbetaf= sqrtome2*tanlatf;

    cosbetai= 1.0/sqrt(1.0 + tanbetai^2);
    cosbetaf= 1.0/sqrt(1.0 + tanbetaf^2);
    sinbetai= tanbetai*cosbetai;
    sinbetaf= tanbetaf*cosbetaf;

    ac= sinbetai*sinbetaf;
    bc= cosbetai*cosbetaf;

    cosphi= ac + bc*coslon;
    sinphi= sign(sinlon)*sqrt(1.0 - min([cosphi^2,1.0]));

    phi= abs(atan(sinphi,cosphi));

    if(maj*abs(phi) > 1.0e-6){

      ca= (bc*sinlon)/sinphi;
      cb= ca^2;
      cc= (cosphi*(1.0 - cb))/k1;
      cd= (-2.0*ac)/k1;
      ce= -ac*k2;
      cf= k3*cc;
      cg= phi^2/sinphi;

      x= ((phi*(k4 + cb) + sinphi*(cc + cd) +  cg*(cf + ce))*ca)/k5;

      lambda= l + x;

      sinlam= sin(lambda);
      coslam= cos(lambda);

      cosph0= ac + bc*coslam;
      sinph0= sign(sinlam)*sqrt(1.0 - cosph0^2);

      phi0= abs(atan(sinph0,cosph0));

      sin2phi= 2.0*sinph0*cosph0;

      cosbeta0= (bc*sinlam)/sinph0;
      q= 1.0 - cosbeta0^2;
      cos2sig= (2.0*ac - q*cosph0)/q;
      cos4sig= 2.0*(cos2sig^2 - 0.5);

      ch= b0*( 1.0 + (q*ep^2)/4.0 - (3.0*q^2*ep^4)/64.0 );
      ci= b0*((q*ep^2)/4.0 - ((q^2)*ep^4)/16.0);
      cj= (q^2*b0*ep^4)/128.0;

      t2= (tanbetaf*cosbetai - coslam*sinbetai);

      geodis= ch*phi0 + ci*sinph0*cos2sig - cj*sin2phi*cos4sig;

      return geodis;

    } else{
      return 0.0;
    }

  }

}

/*---------------------------------------------------------------------------------------*/

func geo2geodetic(gcoord, planet=, equatorial_radius=, polar_radius=)
/* DOCUMENT geo2geodetic(gcoord,planet=, equatorial_radius=, polar_radius=)
  name:
        geo2geodetic

  purpose:
        convert from geographic/planetographic to geodetic coordinates
  explanation:
        converts from geographic (latitude, longitude, altitude) to geodetic
        (latitude, longitude, altitude).  in geographic coordinates, the
            earth is assumed a perfect sphere with a radius equal to its equatorial
                radius. the geodetic (or ellipsoidal) coordinate system takes into
                account the earth's oblateness.

        geographic and geodetic longitudes are identical.
                geodetic latitude is the angle between local zenith and the equatorial plane.
                geographic and geodetic altitudes are both the closest distance between
                the satellite and the ground.

        the planet keyword allows a similar transformation for the other
        planets  (planetographic to planetodetic coordinates).

        the equatorial_radius and polar_radius keywords allow the
        transformation for any ellipsoid.

        latitudes and longitudes are expressed in radians, altitudes in m.

        ref: stephen p.  keeler and yves nievergelt, "computing geodetic
        coordinates", siam rev. vol. 40, no. 2, pp. 300-309, june 1998

        planterary constants from "allen's astrophysical quantities",
        fourth ed., (2000)

  calling sequence:
        ecoord=geo2geodetic(gcoord,[ planet=,equatorial_radius=, polar_radius=])

  input:
        gcoord= a 3-element array of geographic [latitude,longitude,altitude],
                 or an array [3,n] of n such coordinates.


  optional keyword input:
        planet= keyword specifying planet (default is earth).   the planet
                 may be specified either as an integer (1-9) or as one of the
                 (case-independent) strings 'mercury','venus','earth','mars',
                 'jupiter','saturn','uranus','neptune', or 'pluto'

        equatorial_radius : self-explanatory. in m. if not set, planet's
                 value is used.
        polar_radius : self-explanatory. in m. if not set, planet's value is
                 used.

  output:
       a 3-element array of geodetic/planetodetic [latitude,longitude,altitude],
         or an array [3,n] of n such coordinates, double precision.

  common blocks:
        none

  restrictions:

        whereas the conversion from geodetic to geographic coordinates is given
        by an exact, analytical formula, the conversion from geographic to
        geodetic isn't. approximative iterations (as used here) exist, but tend
        to become less good with increasing eccentricity and altitude.
        the formula used in this routine should give correct results within
        six digits for all spatial locations, for an ellipsoid (planet) with
        an eccentricity similar to or less than earth's.
        more accurate results can be obtained via calculus, needing a
        non-determined amount of iterations.
        in any case,
           yo> print,geodetic2geo(geo2geodetic(gcoord)) - gcoord
        is a pretty good way to evaluate the accuracy of geo2geodetic.pro.

  examples:

        locate the geographic north pole, altitude 0., in geodetic coordinates
        yo> geo=[90.d0,0.d0,0.d0]
        yo> geod=geo2geodetic(geo); convert to equivalent geodetic coordinates
        yo> print,geod
        90.000000       0.0000000       21.385000

        as above, but for the case of mars
        yo> geod=geo2geodetic(geo,planet='mars')
        yo> print,geod
        90.000000       0.0000000       18.235500

  modification history:
        written by pascal saint-hilaire (shilaire@astro.phys.ethz.ch), may 2002
        generalized for all solar system planets by robert l. marcialis
                (umpire@lpl.arizona.edu), may 2002
        modified 2002/05/18, psh: added keywords equatorial_radius and
                polar_radius
 */
{
  sz_gcoord= dimsof(gcoord);
  out= array(double, sz_gcoord);

  if(sz_gcoord(2) != 3)error,"3 coordinates (lat, lon, hgt) must be specified";

  if(is_void(planet)){
    re= elp.majax;
    rp= elp.minax;
  }else{
    if(structof(planet) == string){
      iplnt= where(["mercury","venus","earth","mars","jupiter","saturn",\
          "uranus","neptune","pluto"]==planet);
      if(!numberof(iplnt))error,"dont know planet named "+planet;
    }else{
      iplnt= long(planet);
      if(iplnt>9||iplnt<1)error,"planet number unknown: "+pr1(iplnt);
    }
    requator= [2439.7, 6051.80, elp.majax*1e-3, 3397.62,  71492.0,\
              60268.0, 25559.0, 24764.0, 1195.0]*1.0e3;
    rpole= [2439.7, 6051.8, elp.minax*1e-3, 3379.3845, 67136.5562,\
           54890.7686, 24986.1354, 24347.6551, 1195.0]*1.0e3;

    re= requator(iplnt)*1.0e3;   // equatorial radius
    rp= rpole(iplnt)*1.0e3;     // polar radius
  }

  if(equatorial_radius)re= double(equatorial_radius(1));
  if(polar_radius)rp= double(polar_radius(1));

  e= sqrt(re^2 - rp^2)/re;    // flattening= (re-rp)/re  [not needed, here]

  glat= double(gcoord(1,..));
  glon= double(gcoord(2,..));
  galt= re + double(gcoord(3,..));

  clat=  cos(glat);
  x= galt * clat * cos(glon);
  y= galt * clat * sin(glon);
  z= galt * sin(glat);

  r= sqrt(x^2+y^2);

  s= sqrt(r^2 + z ^2) * (1 - re*sqrt((1-e^2)/((1-e^2)*r^2 + z^2)));
  t0= 1 + s*sqrt(1- (e*z)^2/(r^2 + z^2) ) /re;
  dzeta1= z * t0;
  xi1= r*(t0 - e^2);
  rho1= sqrt(xi1^2 + dzeta1^2);
  c1= xi1/rho1;
  s1= dzeta1/rho1;
  b1= re/sqrt(1- (e*s1)^2);
  u1= b1*c1;
  w1= b1*s1*(1- e^2);

  out(1,..)= atan(s1,c1);
  out(2,..)= glon;
  out(3,..)= sqrt((r - u1)^2 + (z - w1)^2);

  return out;
}

/*-------------------------------------------------------------------------------*/

func geodetic2geo(ecoord, planet=, equatorial_radius=, polar_radius=)
/* DOCUMENT geodetic2geo(ecoord, planet=, equatorial_radius=, polar_radius=)
   name:
         geodetic2geo

   purpose:
         convert from geodetic (or planetodetic) to geographic coordinates
   explanation:
         converts from geodetic (latitude, longitude, altitude) to geographic
         (latitude, longitude, altitude).  in geographic coordinates, the
         earth is assumed a perfect sphere with a radius equal to its equatorial
         radius. the geodetic (or ellipsoidal) coordinate system takes into
         account the earth's oblateness.

         geographic and geodetic longitudes are identical.
         geodetic latitude is the angle between local zenith and the equatorial
         plane.   geographic and geodetic altitudes are both the closest distance
         between the satellite and the ground.

         the planet keyword allows a similar transformation for the other
         planets  (planetodetic to planetographic coordinates).

         the equatorial_radius and polar_radius keywords allow the
         transformation for any ellipsoid.

         latitudes and longitudes are expressed in radians, altitudes in m.

         ref: stephen p.  keeler and yves nievergelt, "computing geodetic
         coordinates", siam rev. vol. 40, no. 2, pp. 300-309, june 1998
         planetary constants from "allen's astrophysical quantities",
         fourth ed., (2000)

   calling sequence:
         gcoord= geodetic2geo(ecoord, [ planet= ] )

   input:
         ecoord= a 3-element array of geodetic [latitude,longitude,altitude],
                  or an array [3,n] of n such coordinates.

   optional keyword input:
         planet= keyword specifying planet (default is earth).   the planet
                  may be specified either as an integer (1-9) or as one of the
                  (case-independent) strings 'mercury','venus','earth','mars',
                  'jupiter','saturn','uranus','neptune', or 'pluto'

         equatorial_radius : self-explanatory. in m. if not set, planet's value
                  is used.   numeric scalar
         polar_radius : self-explanatory. in m. if not set, planet's value is
                   used.   numeric scalar

   output:
         a 3-element array of geographic [latitude,longitude,altitude], or an
           array [3,n] of n such coordinates, double precision

         the geographic and geodetic longitudes will be identical.
   common blocks:
         none

   examples:

         yo> geod=[90,0,0]  ; north pole, altitude 0., in geodetic coordinates
         yo> geo=geodetic2geo(geod)
         yo> print,geo
         90.000000       0.0000000      -21.385000

         as above, but the equivalent planetographic coordinates for mars
         yo> geod=geodetic2geo(geod,planet='mars');
         yo> print,geod
         90.000000       0.0000000      -18.235500

   modification history:
         written by pascal saint-hilaire (shilaire@astro.phys.ethz.ch),
                    may 2002

         generalized for all solar system planets by robert l. marcialis
                 (umpire@lpl.arizona.edu), may 2002

         modified 2002/05/18, psh: added keywords equatorial_radius and
                  polar_radius
 */

{
  sz_ecoord= dimsof(ecoord);
  out= array(double, sz_ecoord);

  if(sz_ecoord(2) != 3)error,"3 coordinates (lat, lon, hgt) must be specified";

  if(is_void(planet)){
    re= elp.majax;
    rp= elp.minax;
  }else{
    if(structof(planet) == string){
      iplnt= where(["mercury","venus","earth","mars","jupiter","saturn",\
          "uranus","neptune","pluto"]==planet);
      if(!numberof(iplnt))error,"dont know planet named "+planet;
    }else{
      iplnt= long(planet);
      if(iplnt>9||iplnt<1)error,"planet number unknown: "+pr1(iplnt);
    }
    requator= [2439.7, 6051.80, elp.majax*1e-3, 3397.62,  71492.0,\
              60268.0, 25559.0, 24764.0, 1195.0];
    rpole= [2439.7, 6051.8, elp.minax*1e-3, 3379.3845, 67136.5562,\
           54890.7686, 24986.1354, 24347.6551, 1195.0];

    re= requator(iplnt)*1.0e3;   // equatorial radius
    rp= rpole(iplnt)*1.0e3;     // polar radius
  }

  if(equatorial_radius)re= double(equatorial_radius(1));
  if(polar_radius)rp= double(polar_radius(1));

  e= sqrt(re^2 - rp^2)/re; // flattening= (re-rp)/re  [not needed, here]

  elat= double(ecoord(1,..));
  elon= double(ecoord(2,..));
  ealt= double(ecoord(3,..));

  beta= sqrt(1-(e*sin(elat))^2);
  r= (re/beta + ealt)*cos(elat);
  z= (re*(1-e^2)/beta + ealt)*sin(elat);

  out(1,..)= atan(z,r);
  out(2,..)= elon;
  out(3,..)= sqrt(r^2+z^2) - re;

  return out;
}

/*--------------------------------------------------------------------*/
/*--------------------------- Junk Follows ---------------------------*/
/*--------------------------------------------------------------------*/

func test1_sch (n)
{
  peg= pegpoint(lat=34.00796203014731,lon=-117.9904397308693,hdg= 44.99773962912072,deg=1);

  fill_peg, peg;

  scale= 1e4;

  //sch= [spanl(1,1e4,n)(-,),spanl(1,1e4,n)(,-),array(double,n)];

  orig= peg.rad*[0,pi/2,0];

  vec1= scale*random(3,n);

  vec2= scale*random(3,n);

  dot= (sch_to_pxyz(vec1) * sch_to_pxyz(vec2))(..,sum);

  vec1= add_sch(orig(-,), vec1);
  vec2= add_sch(orig(-,), vec2);

  dotb= (pos_sch(vec1, orig) * pos_sch(vec2, orig))(..,sum);

  abs(dotb-dot)(avg);
}

/*--------------------------------------------------------------------*/

func test2_sch (n)
{
  scale= 1e4;

  //sch= [spanl(1,1e4,n)(-,),spanl(1,1e4,n)(,-),array(double,n)];

  orig= RADIUS*[0,0,0];

  vec1= scale*random(n,3);

  vec2= scale*random(n,3);

  dist= dist_sch(vec1, vec2, peg);

  vec1= add_sch(orig(-,), vec1);
  vec2= add_sch(orig(-,), vec2);

  distb= dist_sch(vec1, vec2);

  abs(distb-dist)(avg);
}

#if 1
func print1 (s)
{
  ss= strchar(array(char(0x20),max(1,lmax -strlen(s))));
  write,f,format=s+ss+"mean: %8.2g; rms: %8.2g\n",n(*)(avg),n(*)(rms);
}
func checkSphericalCosys (f,N,M)
{
   if (is_void(N)) N= 10;
   if (is_void(M)) M= 5;

   v= vectorCart(N,rand=1,unit=1);
   w= vectorCart(N,M,rand=1,unit=1);

   lmax= 54;

   p= pegpoint(ellipsoid(wgs84=1),30*pi/180,-120*pi/180,10*pi/180);

   write,f,"sphericalcosys:",format="%s\n";
   write,f,"==============:",format="%s\n";

   v= vecxyz(p,N,M,rand=1)(to_latlonh,);
   vv= v(to_xyz,)(to_latlonh,);
   n= v(to_xyz,)(dist,vv(to_xyz,));
   vv= v(to_pxyz,)(to_latlonh,);
   n= _(n,v(to_pxyz,)(dist,vv(to_pxyz,)));
   print1,"latlonh to [p]xyz and back";

   v= vecxyz(p,N,M,rand=1);
   vv= v(to_latlonh,)(to_xyz,);
   n= v(dist,vv);
   v= vecpxyz(p,N,M,rand=1);
   vv= v(to_latlonh,)(to_pxyz,);
   n= _(n,v(dist,vv));
   print1,"[p]xyz to latlonh and back";

   v= vecxyz(p,N,M,rand=1);
   vv= v(to_sch,)(to_xyz,);
   n= v(dist,vv);
   v= vecpxyz(p,N,M,rand=1);
   vv= v(to_sch,)(to_pxyz,);
   n= _(n,v(dist,vv));
   print1,"[p]xyz to sch and back";

   v= vecxyz(p,N,M,rand=1);
   vv= v(to_latlonh,)(to_sch,)(to_xyz,);
   n= v(dist,vv);
   v= vecpxyz(p,N,M,rand=1);
   vv= v(to_latlonh,)(to_sch,)(to_pxyz,);
   n= _(n,v(dist,vv));
   print1,"[p]xyz to latlonh to sch and back";

   write,"";
}
func checkEllipticalCosys (f,N,M)
{
   if (is_void(N)) N= 10;
   if (is_void(M)) M= 5;

   lat= 30*pi/180;
   lon= -120*pi/180;
   hdg= 10*pi/180;

   e= ellipsoid(wgs84=1);
   p= pegpoint(e,lat,lon,hdg);

   v= vecXYZ(N,rand=1);
   w= vecXYZ(N,M,rand=1);

   lmax= 54;

   write,f,"ellipticalcosys:",format="%s\n";
   write,f,"===============:",format="%s\n";

   vv= v(to_llh,e)(to_XYZ,);
   n= v(dist,vv);
   print1,"XYZ to llh and back";

   vv= vv(to_llh,e);
   n= v(dist,vv(to_XYZ,));
   print1,"llh to XYZ and back";

   v= vecxyz(p,N,M,rand=1);
   n= v(dist,v(to_XYZ,)(to_xyz,v(peg)));
   print1,"xyz to XYZ and back";

   v= vecpxyz(p,N,M,rand=1);
   n= v(dist,v(to_XYZ,)(to_pxyz,v(peg)));
   print1,"pxyz to XYZ and back";

   v= vecXYZ(N,rand=1);
   n= v(dist,v(to_sch,p)(to_XYZ,));
   print1,"XYZ to sch and back";

   v= vecpxyz(p,N,M,rand=1);
   n= v(dist,v(to_XYZ,)(to_pxyz,v(peg)));
   print1,"pxyz to XYZ and back";

   v= vecpxyz(p,N,M,rand=1);
   w= vecpxyz(p,N,M,rand=1);
   z= v(add,w);
   ww= v(to_sch,)(tprot_pxyz,)(rot,w);
   n= v(add,v(to_sch,)(pxyz_tprot,)(rot,ww))(dist,z);
   print1,"sch tangent plane pxyz vector addition";

   v= vecxyz(p,N,M,rand=1);
   w= vecxyz(p,N,M,rand=1);
   z= v(add,w);
   ww= v(to_sch,)(tprot_xyz,)(rot,w);
   n= v(add,v(to_sch,)(xyz_tprot,)(rot,ww))(dist,z);
   print1,"sch tangent plane xyz vector addition";

   v= vecXYZ(N,M,rand=1);
   w= vecXYZ(N,M,rand=1);
   z= v(add,w); 
   ww= v(to_sch,p)(tprot_XYZ,)(rot,w);
   n= v(add,v(to_sch,p)(XYZ_tprot,)(rot,ww))(dist,z);
   print1,"sch tangent plane XYZ vector addition";

   //n= v(dist,v(to_pxyz,p)(to_XYZ,p));
   //print1,"XYZ to pxyz and back";

   //n= v(dist,v(to_sch,p)(to_XYZ,p));
   //print1,"XYZ to sch and back";

   write,"";
}
checkSphericalCosys,;
checkEllipticalCosys,;
#endif
