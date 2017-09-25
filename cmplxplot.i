// Plot routine for a scalar complex function of a complex variable
// Author: Georg Michel <georg@itpgyro1.fzk.de>
// Date: 11/08/97

require, "digit2.i"

func cmplxplot(z, u)
/* DOCUMENT cmplxplot, z, u
   plots a scalar complex function of a complex variable. Z and U are
   two dimensional complex arrays. U contains the sampling points as a
   quadrilateral mesh and Z contains the corresponding function
   values. In the plot, the color represents the argument of Z in a
   cyclic colormap. 2/3*pi is red, -2/3*pi is blue and 0 is green. So
   there are no color steps between +pi and -pi. The contour lines
   represent the absolute value of Z.
*/
{
   resx=100;
   resy=100;
   s=[[sqrt(2./3.),0,1/sqrt(3)],[-1/sqrt(2*3.0),1/sqrt(2.),1/sqrt(3)],
      [-1/sqrt(2*3.0),-1/sqrt(2.),1/sqrt(3)]];
   phi=span(-pi,pi,256);
   vec=array(double,3,256);
   vec(1,)=cos(phi);
   vec(2,)=sin(phi);
   vec(3,)=1/sqrt(2)(-);
   f=s(+,)*vec(+,);
   palette,bytscl(f(2,),top=255),bytscl(f(1,),top=255),bytscl(f(3,),top=255);
   xmin=min(u.re);
   xmax=max(u.re);
   ymin=min(u.im);
   ymax=max(u.im);
   zi=array(complex,resx,resy);
   zi.re=interp2(span(ymin,ymax,resy)(-:1:resx,),span(xmin,xmax,resx)(,-:1:resy),
         z.re,u.im,u.re);
   zi.im=interp2(span(ymin,ymax,resy)(-:1:resx,),span(xmin,xmax,resx)(,-:1:resy),
         z.im,u.im,u.re);
   // Caution: on some platforms atan(0,0) crashes !
   arr=bytscl(atan(zi.im,zi.re+1e-200),cmin=-pi,cmax=pi);
   pli,arr, xmin,ymin,xmax,ymax;
   plc,abs(z),u.im,u.re,marks=0;
}
