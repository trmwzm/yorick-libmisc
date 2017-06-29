func histchop (x, frac, &it, nbin=)
/* DOCUMENT histchop (x, frac, &it, nbin=)
   x= random_n(3000);
   nbin= 30;
   xbb= histchop(x,.999,it,nbin=nbin);it;
   fma;
   histo,x,span(xbb(1),xbb(2),nbin),plot=1
 */
{
  x= x(*);   // local copy
  n= numberof(x);
  xpp= abs(x(*)(ptp));
  if (is_void(frac)) frac= 0.95;
  nf= long(frac*n);
  if (is_void(nbin)) nbin= max(20,n/100);
  xm= median(x);
  x-= xm;
  lo= ((m=(x<0))*x)(sum)/max(1,m(sum));
  hi= ((m=(x>0))*x)(sum)/max(1,m(sum));
  fi= xpp/(hi-lo);
  it= 0;
  do {
    it++;
    xbb= fi*[lo,hi];
    odx= xbb(dif)/(nbin-1);
    h= histogram(min(max(1,long((x-xbb(1))/odx+2)),nbin+1),top=nbin+1)(2:-1);
    fi*= frac;
  } while (h(sum)>nf && fi>1e-2)
  return xbb+xm; 
}

/*-----------------------------------------------------------------------*/

func histo (x,bins,plot=,color=,type=,normalize=,frac=,nbin=,width=)
/* DOCUMENT histo(x,bins,plot=,color=,type=,normalize=)
   nb = numberof(bins);
   histo = histogram(digitize(x,bins),top = nb + 1);
    
   frac= keyword does automatic bounds (keep fraction) nbins=
         fma;histo,random(1000),frac=.97,nbin=22,plot=1
   plot=1 does a "bar" plot
   plot=2 just plots a bar plot, x becomes histogram where
          numberof(x) = numberof(bins)+1
          example histo,_(0,16_counts,0),span(-0.5,15.5,17),plot=2;
   h= histogram(digitize(x,b),top = numberof(b) + 1);
   plh,h(2:-1),b(:-1),just=1;
   plg,[0,h(2)],[b(1),b(1)];
   plg,[0,h(-1)],[b(0),b(0)];
   SEE: histchop
 */
{
  if (!is_void(frac)) {
    xbb= histchop(x,frac,nbin=nbin);
    bins= span(xbb(1),xbb(2),nbin);
  }

  nb = numberof(bins);

  nx= numberof(x);

  if(plot==2){
     h = x;
  }else{
     h = histogram(digitize(x,bins),top = nb + 1);
     if(normalize) h/= double(nx);
  }

   if(!is_void(plot)||color||type||width){
      hh = h;
      if (normalize==1) hh /= (hh(2:-1)*bins(dif))(sum);
      if (normalize==2) hh /= nx;
      hh(1) = 0; hh(0) = 0;
      bb= array(structof(bins),2,nb);bb(1,) = bins;bb(2,) = bins;
      hhh = array(structof(hh),2,nb);hhh(1,)=hh(1:-1);hhh(2,)=hh(2:0);
      plg,hhh(*),bb(*),color=color,type=type,width=width;
  }

  return h;
}

/*-----------------------------------------------------------------------*/
