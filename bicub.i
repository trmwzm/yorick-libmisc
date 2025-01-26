// Fast bicubic interpolation routine for equidistantly sampled 2D functions
// Author: Georg Michel <georg@itpgyro1.fzk.de>
// Date: 10/19/97

func bicub(x0,y0,z,xmin,xmax,ymin,ymax) 
/* DOCUMENT z0=bicub(x0,y0,z,xmin,xmax,ymin,ymax)
  gives the bicubic interpolate of the two dimensional array Z, which is
  an equidistantly sampled (complex) function, at point(s) (X0,Y0). First index
  corresponds to x, second index corresponds to y. XMIN corresponds to z(1,)
  YMAX corresponds to z(,dimsof(z)(3)).  For the sake of speed, the user is
  responsible for not having any points (X0,Y0) in the outermost regions
  and beyond.  
*/
{
  xmin= double(xmin);
  xmax= double(xmax);
  ymin= double(ymin);
  ymax= double(ymax);
  i0= int((x0-xmin)/(xmax-xmin)*(dimsof(z)(2)-1))+1;
  // for j0 the +1 is omitted because of the indexing scheme for zvec
  j0= int((y0-ymin)/(ymax-ymin)*(dimsof(z)(3)-1));
  t= ((x0-xmin)/(xmax-xmin)*(dimsof(z)(2)-1))%1;
  u= ((y0-ymin)/(ymax-ymin)*(dimsof(z)(3)-1))%1;
  posvec= [t^3*u^3, t^3*u^2, t^3*u, t^3, t^2*u^3, t^2*u^2, t^2*u, t^2, 
           t*u^3, t*u^2, t*u, t, u^3, u^2, u, 1];
  fd=dimsof(z)(2);
  c1= 1/36.0;   // .2777777778e-1
  c2= 1/12.0;   // .8333333333e-1
  c3= 1/6.0;    // .1666666667
  c4= 1/18.0;   // .5555555556e-1
  c5= 1/3.0;    // .3333333333
  c6= 1/9.0;    // .1111111111

  zvec= [c1*z(i0-1+ fd*(j0-1))-c2*z(i0+
        fd*(j0-1))+ c2*z(i0+ 1+
        fd*(j0-1))-c1*z(i0+ 2+
        fd*(j0-1))-c2*z(i0-1+ fd*j0)+ .25*z(i0+
        fd*j0)-.25*z(i0+ 1+ fd*j0)+ c2*z(i0+ 2+
        fd*j0)+ c2*z(i0-1+ fd*(j0+ 1))-.25*z(i0+
        fd*(j0+ 1))+ .25*z(i0+ 1+ fd*(j0+
        1))-c2*z(i0+ 2+ fd*(j0+ 1))-c1*z(i0-1+
        fd*(j0+ 2))+ c2*z(i0+ fd*(j0+
        2))-c2*z(i0+ 1+ fd*(j0+ 2))+ c1*z(i0+
        2+ fd*(j0+ 2)), -c2*z(i0-1+ fd*(j0-1))+
        .25*z(i0+ fd*(j0-1))-.25*z(i0+ 1+ fd*(j0-1))+
        c2*z(i0+ 2+ fd*(j0-1))+ c3*z(i0-1+
        fd*j0)-.5*z(i0+ fd*j0)+ .5*z(i0+ 1+
        fd*j0)-c3*z(i0+ 2+ fd*j0)-c2*z(i0-1+
        fd*(j0+ 1))+ .25*z(i0+ fd*(j0+ 1))-.25*z(i0+
        1+ fd*(j0+ 1))+ c2*z(i0+ 2+ fd*(j0+ 1)),
        c4*z(i0-1+ fd*(j0-1))-c3*z(i0+
        fd*(j0-1))+ c3*z(i0+ 1+
        fd*(j0-1))-c4*z(i0+ 2+ fd*(j0-1))+
        c2*z(i0-1+ fd*j0)-.25*z(i0+ fd*j0)+
        .25*z(i0+ 1+ fd*j0)-c2*z(i0+ 2+
        fd*j0)-c3*z(i0-1+ fd*(j0+ 1))+ .5*z(i0+
        fd*(j0+ 1))-.5*z(i0+ 1+ fd*(j0+ 1))+
        c3*z(i0+ 2+ fd*(j0+ 1))+ c1*z(i0-1+
        fd*(j0+ 2))-c2*z(i0+ fd*(j0+ 2))+
        c2*z(i0+ 1+ fd*(j0+ 2))-c1*z(i0+ 2+
        fd*(j0+ 2)), -c3*z(i0-1+ fd*j0)+ .5*z(i0+
        fd*j0)-.5*z(i0+ 1+ fd*j0)+ c3*z(i0+ 2+
        fd*j0), -c2*z(i0-1+ fd*(j0-1))+ c3*z(i0+
        fd*(j0-1))-c2*z(i0+ 1+ fd*(j0-1))+
        .25*z(i0-1+ fd*j0)-.5*z(i0+ fd*j0)+
        .25*z(i0+ 1+ fd*j0)-.25*z(i0-1+ fd*(j0+ 1))+
        .5*z(i0+ fd*(j0+ 1))-.25*z(i0+ 1+ fd*(j0+
        1))+ c2*z(i0-1+ fd*(j0+ 2))-c3*z(i0+
        fd*(j0+ 2))+ c2*z(i0+ 1+ fd*(j0+ 2)),
        .25*z(i0-1+ fd*(j0-1))-.5*z(i0+ fd*(j0-1))+
        .25*z(i0+ 1+ fd*(j0-1))-.5*z(i0-1+ fd*j0)+
        1.*z(i0+ fd*j0)-.5*z(i0+ 1+ fd*j0)+
        .25*z(i0-1+ fd*(j0+ 1))-.5*z(i0+ fd*(j0+ 1))+
        .25*z(i0+ 1+ fd*(j0+ 1)), -c3*z(i0-1+
        fd*(j0-1))+ c5*z(i0+ fd*(j0-1))-c3*z(i0+ 1+
        fd*(j0-1))-.25*z(i0-1+ fd*j0)+ .5*z(i0+
        fd*j0)-.25*z(i0+ 1+ fd*j0)+ .5*z(i0-1+
        fd*(j0+ 1))-1.*z(i0+ fd*(j0+ 1))+ .5*z(i0+ 1+ fd*(j0+
        1))-c2*z(i0-1+ fd*(j0+ 2))+ c3*z(i0+
        fd*(j0+ 2))-c2*z(i0+ 1+ fd*(j0+ 2)),
        .5*z(i0-1+ fd*j0)-1.*z(i0+ fd*j0)+ .5*z(i0+
        1+ fd*j0), c4*z(i0-1+ fd*(j0-1))+
        c2*z(i0+ fd*(j0-1))-c3*z(i0+ 1+
        fd*(j0-1))+ c1*z(i0+ 2+
        fd*(j0-1))-c3*z(i0-1+ fd*j0)-.25*z(i0+
        fd*j0)+ .5*z(i0+ 1+ fd*j0)-c2*z(i0+ 2+
        fd*j0)+ c3*z(i0-1+ fd*(j0+ 1))+ .25*z(i0+
        fd*(j0+ 1))-.5*z(i0+ 1+ fd*(j0+ 1))+
        c2*z(i0+ 2+ fd*(j0+ 1))-c4*z(i0-1+
        fd*(j0+ 2))-c2*z(i0+ fd*(j0+ 2))+
        c3*z(i0+ 1+ fd*(j0+ 2))-c1*z(i0+ 2+
        fd*(j0+ 2)), -c3*z(i0-1+ fd*(j0-1))-.25*z(i0+
        fd*(j0-1))+ .5*z(i0+ 1+
        fd*(j0-1))-c2*z(i0+ 2+ fd*(j0-1))+
        c5*z(i0-1+ fd*j0)+ .5*z(i0+ fd*j0)-1.*z(i0+
        1+ fd*j0)+ c3*z(i0+ 2+ fd*j0)-c3*z(i0-1+
        fd*(j0+ 1))-.25*z(i0+ fd*(j0+ 1))+ .5*z(i0+
        1+ fd*(j0+ 1))-c2*z(i0+ 2+ fd*(j0+ 1)),
        c6*z(i0-1+ fd*(j0-1))+ c3*z(i0+
        fd*(j0-1))-c5*z(i0+ 1+ fd*(j0-1))+
        c4*z(i0+ 2+ fd*(j0-1))+ c3*z(i0-1+
        fd*j0)+ .25*z(i0+ fd*j0)-.5*z(i0+ 1+ fd*j0)+
        c2*z(i0+ 2+ fd*j0)-c5*z(i0-1+ fd*(j0+
        1))-.5*z(i0+ fd*(j0+ 1))+ 1.*z(i0+ 1+ fd*(j0+
        1))-c3*z(i0+ 2+ fd*(j0+ 1))+ c4*z(i0-1+
        fd*(j0+ 2))+ c2*z(i0+ fd*(j0+
        2))-c3*z(i0+ 1+ fd*(j0+ 2))+ c1*z(i0+ 2+
        fd*(j0+ 2)), -c5*z(i0-1+ fd*j0)-.5*z(i0+
        fd*j0)+ 1.*z(i0+ 1+ fd*j0)-c3*z(i0+ 2+ fd*j0),
        -c3*z(i0+ fd*(j0-1))+ .5*z(i0+
        fd*j0)-.5*z(i0+ fd*(j0+ 1))+ c3*z(i0+
        fd*(j0+ 2)), .5*z(i0+ fd*(j0-1))-1.*z(i0+ fd*j0)+
        .5*z(i0+ fd*(j0+ 1)), -c5*z(i0+
        fd*(j0-1))-.5*z(i0+ fd*j0)+ 1.*z(i0+ fd*(j0+
        1))-c3*z(i0+ fd*(j0+ 2)),1.*z(i0+ fd*j0)];
  return((posvec*zvec)(..,sum)); 
}
