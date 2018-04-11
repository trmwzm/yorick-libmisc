func oxsxp(args)
    /* DOCUMENT s= oxsxp(o); // s: array(string)
       o= oxsxp(s); .. *not yet* ..
       FMT: format integer or decimal, SEE totxt.
    */
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"string or ox, optional fmt (see totxt).";
  local fmt;
  if (args(0)>1) fmt= args(2);
  tablen= 4;
  if (is_obj(args(1))) {
    if (args(0,1)==0)
      onm= args(-,1);
    else
      onm= "cfg";
    o= args(1);
    s= "";
    oxsxp_wrkr,s,onm,o,fmt;
    return s;
  }
}
wrap_args,oxsxp;
func oxsxp_wrkr (&s,onm,o,nt,fmt)
{
  if (is_void(nt))
    nt= 0;
  t= (nt? strchar(array('\x20',nt*tablen)): "");
  t2= strchar(array('\x20',(nt+1)*tablen));
  s+= t+"("+(!onm? "": onm+" ")+"\n";
  on= o(*);
  for (i=1;i<=on;i++) {
    oi= o(noop(i));
    if (is_obj(oi)) {
      oxsxp_wrkr,s,o(*,i),oi,nt+1;
    } else {
      ss= oxsxp_pr(oi,fmt);
      p=  strpart(ss,0:0)==")";
      s+= (i==1? t2: " ")+(!o(*,i)? "": "("+o(*,i)+"\n")+t2+ss;
      s+= p? "\n": "";o(*,i);
    }
    s+= (i==on || is_obj(oi) || p )? "\n": "";
  }
  s+= t+")";
}
func oxsxp_pr (a,fmt)
{
  if (is_void(fmt))
    fmt=-0.12;
  if (is_integer(a))
    fmt= abs(fmt); //  no hex
  da= dimsof(a);
  sa= is_string(a)? a: totxt(a,fmt);

  if (da(1)>0) {
    n= numberof(a);
    s= da(:-1)
        ra= da(1);
    for(i=1;i<=ra;i++)
      s(i)=(i==1? 1: s(i)*s(i-1));
    da= da(2:);

    m= array(0,n);
    for (i=1;i<ra;i++) {
      wp= indgen(1:n:s(i+1));
      ws= indgen(s(i+1):n:s(i+1));
      m(wp)-= 1; //pre
      m(ws)+= 1; //suf
    }
    m(1)-= 1;
    m(0)+= 1;
    b= strchar("()");
    for (i=1;i<=n;i++) {
      if (m(i)>0)
        sa(i)= sa(i)+strchar(array(b(2),m(i)));
      if (m(i)<0)
        sa(i)= strchar(array(b(1),-m(i)))+sa(i);
      if (i<n)
        sa(i)+= " ";
    }
    return sa(*)(sum);
  } else
    return sa;
}

#if 1
osxp= save();
osxp, s1= ["(","  ;; input grid specification","  (grid","    (","       (x0 -25.0)", \
           "       (x1  25.0)","       (nx 100)","    ))","  ;; output file", \
           "  ;; (output foo.sxp)","  (output foo.sxp)", \
           "  (polynomial (1.0 1.1 1.2 1.3 1.4))",")"];
osxp, s2= ["("," (months (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))", \
           " (days (Mon Tue Wed Thu Fri Sat Sun))",")"];
osxp, s3= ["(calendar_analysis_report ((month 11) (name Nov) (input_calendar ((months (Jan Feb", \
           " Mar Apr May Jun Jul Aug Sep Oct Nov Dec)) (days (Mon Tue Wed Thu Fri Sat Sun)))",")))"];
osxp, s4= ["((grid ((x0 -25.0) (x1 25.0) (nx 100))) (xs (-25.00000000000000 -24.49494949494949",
           " -23.98989898989899 -23.48484848484848 -22.97979797979798 -22.47474747474747 -21.96969696969697", \
           " 23.98989898989899 24.49494949494949 25.00000000000000)) (ys (527286.0000000000 ", \
           "485592.2020715658 446421.6406647993 409670.3857727454 375236.6935311963 343021.0062186911", \
           " 282705.1853047567 310616.0839802571 340544.8430932583 372586.4716267767 406838.1647065763", \
           " 443399.3036011674 482371.4557218075 523858.3746225012 567966.0000000000)) (version 1.46beta))"];

scratch= save(scratch,d,days,months);
cfg= save(); {

  months= save(); {
    save, months, [], "Jan";
    save, months, [], "Feb";
    save, months, [], "Dec";
  } save, cfg, months;

  days= save(); {
    save, days, [], "Mon";
    save, days, [], "Tue";
  } save, cfg, days;

  d= save(); {

    months= save(); {
      save, months, [], "Jan";
      save, months, [], "Dec";
    } save, d, months;

    days= save(); {
      save, days, [], "Mon";
      save, days, [], "Sun";
    } save, d, days;
  } save, cfg, d;
}
restore, scratch;
write,oxsxp(cfg),format="%s\n";
write,"",format="= done =\n";

scratch= save(scratch,q);
cfg= save(); {
  q= save(); {
    q, x0= -25.0;
    q, x1= 25.0;
    q, nx= 100;
  } cfg, grid=q;

  cfg, output= "foo.sxp";
  cfg, polynomial= [1.0, 1.1, 1.2, 1.3, 1.4];
}
restore, scratch;
write,oxsxp(cfg),format="%s\n";
write,"",format="= done =\n";
#endif
