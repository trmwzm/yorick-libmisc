
func binavrg (z, x, fac, &x10, badval=, badvalout=)
/* DOCUMENT: binavrg (z, x, fac, &x10, badval=, badvalout=)
   ni= dimsof(z)(0);
   output gris: go= span(0.5,ni+0.5,no+1)(zcen);
*/ 
{
  dz= dimsof(z);
  if (dz(1)!=1)
    error;
  ni= dz(2);

  if (is_void(badval))
    badval= 0;
  if (is_void(badvalout))
    badvalout= badval;
  
  if (is_void(fac))
    error;
 
  if(structof(fac)==long||structof(fac)==int||\
     structof(fac)==short){
    no= fac;
  } else {
    no= min(max(long(fac*ni+0.5),1),ni);
  }

  x1= x(min);
  x0= x(max);

  dx= (x0-x1)/no;
  x10= [x1-dx/2,x0+dx/2];
  go= span(x10(1),x10(2),no+1);

  wgd= where(z!=badval);
  li= digitize(x(wgd),go)-1;
  zo= array(double(badvalout),no);

  hz= histogram(li,top=no);
  wh= where(hz!=0);
  zo(wh)= histogram(li,z(wgd))(wh)/hz(wh);

  x10+[+1,-1]*x10(dif)/(no+1);
  return zo;
}

/*------------------------------------------------------------------*/

func imbinavrg (z, fac1, fac2, badval=, badvalout=)
/* DOCUMENT: imbinavrg (z, fac1, fac2, badval=, badvalout=)
   ni1/2= dimsof(z)(2/3)
   output gris: go1= span(0.5,ni1+0.5,no1+1)(zcen);
   go2= span(0.5,ni2+0.5,no2+1)(zcen);
*/ 
{
  dz= dimsof(z);
  if(dz(1)!=2)error;
  ni1= dz(2);
  ni2= dz(3);
  if(is_void(badval))badval=0;
  if(is_void(badvalout))badvalout=badval;
  
  if(is_void(fac1))error;
  if(is_void(fac2))fac2=fac1;
 
  if(structof(fac1)==long||structof(fac1)==int||\
     structof(fac1)==short){
    no1= fac1;
  }else{
    no1= min(max(long(fac1*ni1+0.5),1),ni1);
  }
  if(structof(fac2)==long||structof(fac2)==int||\
     structof(fac2)==short){
    no2= fac2;
  }else{
    no2= min(max(long(fac2*ni2+0.5),1),ni2);
  }
  
  go1= span(0.5,ni1+0.5,no1+1);
  go2= span(0.5,ni2+0.5,no2+1);

  wgd= where(z!=badval);
  igd= indim(wgd,dz);
 
  li1= digitize(igd(1,),go1)-1;
  li2= digitize(igd(2,),go2)-1;

  zo= array(structof(z)(badvalout), no1, no2);

  zndx= (li1+(li2-1)*no1);          // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

  hz= histogram(zndx, top=no1*no2);
  wh= where(hz!=0);
  zo(wh)= histogram(zndx, z(wgd))(wh)/hz(wh);

  //go1= go1(zcen);
  //go2= go2(zcen);
  return zo;
}

/*------------------------------------------------------------------*/

func xyzbinavrg (x,y,z, n1, n2, badval=, badvalout=)
/* DOCUMENT xyzbinavrg
   where x refers to 1st index in output grid
   y refers to 2nd index in output grid
   z is the value in output grid
   output gris: go1= span(0.5,ni1+0.5,no1+1)(zcen);
   go2= span(0.5,ni2+0.5,no2+1)(zcen);
   n=24
   getModGrid_init, n, rstr=1;  
   din= [3,GRIDAR(n).nxgrid,GRIDAR(n).ngridface,GRIDAR(n).nygrid];
   dpt= readFlat(TRMROOT+GRIDAR(n).griddir+"Depth.data",float,din);
   fma;pli,xyzbinavrg (MLON,MLAT,addedges(dpt), 360, 180, badval=MODATNAN, badvalout=0)
*/ 
{
  dz= dimsof(z);
  dy= dimsof(y);
  dx= dimsof(x);
  if(anyof(dz!=dx)||anyof(dy!=dx))error;

  ni= numberof(z);

  if(is_void(badval))badval=0;
  if(is_void(badvalout))badvalout=badval;
  
  if(is_void(n1))error;
  if(is_void(n2))n2=n1;
 
  if(structof(n1)!=long&&structof(n1)!=int&&\
     structof(n1)!=short)error;
  if(structof(n2)!=long&&structof(n2)!=int&&\
     structof(n2)!=short)error;
 
  xmnmx= [x(*)(min),x(*)(max)];
  ymnmx= [y(*)(min),y(*)(max)];
 
  lx= xmnmx(dif)(1);
  ly= ymnmx(dif)(1);

  go1= span(xmnmx(1)-2.*lx/n1,xmnmx(2)+2.*lx/n1,n1+1);
  go2= span(ymnmx(1)-2.*ly/n2,ymnmx(2)+2.*ly/n2,n2+1);

  wgd= where(z!=badval);
 
  li1= digitize(x(wgd),go1)-1;
  li2= digitize(y(wgd),go2)-1;

  zo= array(structof(z)(badvalout), n1, n2);

  zndx= (li1+(li2-1)*n1);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

  hz= histogram(zndx, top=n1*n2);
  wh= where(hz!=0);
  zo(wh)= histogram(zndx, z(wgd))(wh)/hz(wh);

  //go1= go1(zcen);
  //go2= go2(zcen);
  return zo;
}
/*------------------------------------------------------------------*/

func gridbinavrg (gi1, gi2, z, go1, go2, &zndx, badval=, badvalout=,reuse=,proptresh=)
/* DOCUMENT gridbinavrg (gi1, gi2, z, go1, go2, &zndx, badval=, badvalout=,reuse=,proptresh=) 
   gridbinavrg z on grid gi1/gi2 to go1/go2  
   in and out grids are assumed to be SEPARABLE (xgrid * ygrid) "ll" type grid
   gi/o/1/2 also assumed monotonically increasing
   if there is a third dimension to z (nimberof(gi1), numberof(gi2), NZ)\
   the binargerage is carried out NZ times
*/ 
{
  di1= dimsof(gi1);
  di2= dimsof(gi2);
  dz= dimsof(z);

 
  if(dz(2)!=di1(2)||dz(3)!=di2(2))error;
 
  if(dz(1)==3){
    n3= dz(4);
  }else{
    n3= 1;
  }

  ni= numberof(z);

  if(is_void(badval))badval=0;
  if(is_void(badvalout))badvalout=badval;
  
  if(is_void(go1)||is_void(go2))error;
  n1= numberof(go1);
  n2= numberof(go2);
 
  zo= array(structof(z)(badvalout), n1, n2, n3);
  zo1= array(structof(z)(badvalout), n1, n2);

  gbo1= go1(pcen);gbo1(1)-=gbo1(2)-gbo1(1);gbo1(0)+=gbo1(0)-gbo1(-1);
  gbo2= go2(pcen);gbo2(1)-=gbo2(2)-gbo2(1);gbo2(0)+=gbo2(0)-gbo2(-1);

  if(reuse!=1||is_void(zndx)){                // kludgy stuff
    li1= digitize(gi1(,-:1:di2(2)),gbo1)-1;
    mt= li1==n1+1;if(anyof(mt))li1(where(mt))=1; // U case
    li2= digitize(gi2(-:1:di1(2),),gbo2)-1;
    mt= li2==n2+1;if(anyof(mt))li2(where(mt))=n2; // V case 

    zndx= (li1+(li2-1)*n1);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)
  }

  for(iz=1;iz<=n3;iz++){
    z1= z(,,iz);
    zo1(*)= badvalout;
    wgd= where(z1!=badval);

    zndxz= zndx(wgd);    // [x,y,z](wp) assigned to [xg,yg,zg](zndx)

    hz= histogram(zndxz, top=n1*n2);
    if(!is_void(proptresh)){
      countmin= proptresh*hz(max);
    }else{
      countmin= 0;
    }
    wh= where(hz>countmin);
    zo1(wh)= histogram(zndxz, z1(wgd))(wh)/hz(wh);

    if(nz==1)return zo1;

    zo(,,iz)= zo1;
  }
  return zo;
}
/*------------------------------------------------------------------*/
