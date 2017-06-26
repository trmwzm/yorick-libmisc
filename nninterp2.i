require, "digit2.i";

func nninterp2(y0,x0, z,y,x,reg, &ondx, outside=)
/* DOCUMENT z0= nninterp2(y0,x0, z,y,x,,  &ondx)
         or z0= nninterp2(y0,x0, z,y,x,reg, &ondx)

     return the nearest neighbor interpolate of the function Z(X,Y) at the
     points (X0,Y0).  The X, Y, and optional REG arrays specify a
     quadrilateral mesh as for the plm function.  The Z values are
     specified at the vertices of this mesh, so Z must have the
     same dimensions as X and Y.

     Points outside the mesh get the value 0.0, unless the outside
     keyword is non-nil, in which case they get that value.

   SEE ALSO: interp2, interp, digit2, mesh_loc, plm
 */
{
  scalar= !dimsof(x0,y0)(1);
  if (scalar) { x0= [x0];  y0= [y0]; }
  ndx= digit2(y0,x0, y,x,reg, pt=1);
  mask= ndx>0;

  ondx= save();  //returned list of (1) where inside 
                 //                 (2) where eval 
                 //                 (3) where outside
  /* first handle points inside mesh */
  list= where(mask);
  save,ondx,string(0),list;
  wz= [];
  if (numberof(list)) {
    ndx= ndx(list);
    zero= array(0., dimsof(x0,y0));
    x0= (x0+zero)(list);
    y0= (y0+zero)(list);

    /* here are corners of the interpolation tets */
    m= dimsof(y)(2);
    x00= [x(ndx-m-1), y(ndx-m-1)];// z00= z(ndx-m-1);
    x10= [x(ndx-m), y(ndx-m)];    // z10= z(ndx-m);
    x11= [x(ndx), y(ndx)];        // z11= z(ndx);
    x01= [x(ndx-1), y(ndx-1)];    // z01= z(ndx-1);

    mxi= [abs(x00(..,1)-x0,x00(..,2)-y0),\
          abs(x10(..,1)-x0,x10(..,2)-y0),\
          abs(x01(..,1)-x0,x01(..,2)-y0),\
          abs(x11(..,1)-x0,x11(..,2)-y0)](..,mnx);
   
    wz= ndx -[m+1,m,1,0](mxi);

    za= z(wz);
  }
  save,ondx,string(0),wz;

  /* just punt on points outside mesh */
  list= where(!mask);
  save,ondx,string(0),list;
  if (numberof(list)) {
    if (is_void(outside)) outside= 0.0;
    zb= array(outside, numberof(list));
  }

  if (scalar) mask= mask(1);
  return merge(za,zb,mask);
}
func interp21(y0,x0, z,y,x,reg, &ondx, outside=)
/* DOCUMENT z0= nninterp2(y0,x0, z,y,x,,  &ondx)
         or z0= nninterp2(y0,x0, z,y,x,reg, &ondx)

     return the nearest neighbor interpolate of the function Z(X,Y) at the
     points (X0,Y0).  The X, Y, and optional REG arrays specify a
     quadrilateral mesh as for the plm function.  The Z values are
     specified at the vertices of this mesh, so Z must have the
     same dimensions as X and Y.

     Points outside the mesh get the value 0.0, unless the outside
     keyword is non-nil, in which case they get that value.

   SEE ALSO: interp2, interp, digit2, mesh_loc, plm
 */
{
  scalar= !dimsof(x0,y0)(1);
  if (scalar) { x0= [x0];  y0= [y0]; }
  ndx= digit2(y0,x0, y,x,reg, pt=1);
  mask= ndx>0;

  ondx= save();  //returned list of (1) where inside 
                 //                 (2) where eval 
                 //                 (3) where outside
  /* first handle points inside mesh */
  list= where(mask);
  save,ondx,string(0),list;
  wz= [];
  if (numberof(list)) {
    ndx= ndx(list);
    zero= array(0., dimsof(x0,y0));
    x0= (x0+zero)(list);
    y0= (y0+zero)(list);

    /* here are corners of the interpolation tets */
    m= dimsof(y)(2);

    x00= x(ndx-m-1);
    y00= y(ndx-m-1);
    z00= z(ndx-m-1);

    x10= x(ndx-m);
    y10= y(ndx-m);
    z10= z(ndx-m);

    //x11= x(ndx);
    y11= y(ndx);
    z11= z(ndx);

    //x01= x(ndx-1);
    y01= y(ndx-1);
    z01= z(ndx-1);

    // 2nd index lin interp
    dy= y01-y00;
    f= (y0-y00)/dy;
    z0= z00*(1-f)+z01*f;
    
    // 2nd index lin interp
    dy= y11-y10;
    f= (y0-y10)/dy;
    z1= z10*(1-f)+z11*f;
    dy= [];

    // 1nd index lin interp
    dx= x10-x00;  //assumed == x11-x01
    f= (x0-x00)/dx;
    za= z0*(1-f)+z1*f;
    dx= f= [];
  }
  save,ondx,string(0),wz;

  /* just punt on points outside mesh */
  list= where(!mask);
  save,ondx,string(0),list;
  if (numberof(list)) {
    if (is_void(outside)) outside= 0.0;
    zb= array(outside, numberof(list));
  }

  if (scalar) mask= mask(1);
  return merge(za,zb,mask);
}


