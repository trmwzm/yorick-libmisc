require, "yut.i";

scratch= save(scratch, tmp, scmox_wrkr, oxscm_wrkr, oxscm_scan, oxscm_pr)
func oxscm(args)
    /* DOCUMENT s= oxscm(o); // s: array(string)
       o= oxscm(s); .. *not yet* ..
       FMT: format integer or decimal, SEE totxt.
    */
{
  if (args(0)==0 || args(0)>2 || numberof(args(-))!=0)
    error,"string or ox, optional fmt (see totxt).";
  local fmt;
  if (args(0)>1)
    fmt= args(2);
  tablen= 4;
  if (is_obj(args(1))) {
    if (args(0,1)==0)
      onm= args(-,1);
    else
      onm= "cfg";
    s= "";
    use_method,oxscm_wrkr,s,onm,args(1),fmt;
    return s;
  } else if (is_string(args(1))) {
    s= args(1);
    if (dimsof(s)(1)>0) {
      g= strgrep("^[^;]*;", s);
      w= where(g(2,dif)>0);
      if (numberof(w))
        s(w)= strpart(s(w),g(,w)-[0,2]);
      s= s(sum);
    }
    s= streplace(s,strgrep("[\t\n\r]",s, \
                           n=max(1,strlen(s)/5)),"");
    m= strchar(s)==strchar("(")(1);
    m-= strchar(s)==strchar(")")(1);
    m= m(cum);
    mm= [];
    i= strgrep("^[^(]*\\(",s)(2);  // dump what's before first (
    ss= use_method(oxscm_scan,s,m,i,mm);
    o= save();
    use_method,scmox_wrkr,o,ss,mm;
    return o;
  }
}
wrap_args,oxscm;
func scmox_wrkr (o,s,m)
{
  if (is_void(o))
    o= save();
  if (strlen(s)==0)
    return o;
  mm= [];
  im= strgrep("^[^(]*\\(",s)(2);
  nm= (im<2? string(0): strtrim(strpart(s,:im-1)));
  op= where(m(2:)>m(1:-1)&m(2:)==m(im+1));
  if (numberof(op)) {
    oo= save();
    for (ip=1;ip<=numberof(op);ip++) {
      i= op(ip);
      ss= use_method(oxscm_scan,s,m,i,mm);
      if (ss==string(0))
        save, oo, string(0), [];
      else
        use_method,scmox_wrkr,oo,noop(ss),mm;
    }
    if (oo(*)==1 && oo(*,1)==string(0) && is_array(oo(1)))
      save,o,noop(nm),oo(1);
    else
      save,o,noop(nm),oo;
  } else {
    p= strtrim(strtok(s," "));
    pp= tonum(p);
    if (pp(1)>-1e+99) {
      q= strpart(s,strword(s," ",100));
      m= q!=string(0);
      if (anyof(m)) {
        q= q(where(m));
        isi= allof(strgrepm("[.edED]",q)==0);
        save, o, string(0), (isi? long(tonum(q)): tonum(q));
      }
    } else
      save, o, p(1), (pp(2)>-1e+99? (strgrepm("[.edED]",p(2))? pp(2): long(pp(2))): p(2));
  }
  return o;
}
func oxscm_wrkr (&s,onm,o,nt,fmt)
{
  if (is_void(nt))
    nt= 0;
  t= (nt? strchar(array('\x20',nt*tablen)): "");
  t2= strchar(array('\x20',(nt+1)*tablen));
  s+= t+"("+(!onm? "": onm+" ")+"\n";
  on= o(*);
  for (i=1;i<=on;i++) {
    oi= o(noop(i));
    oni= o(*,i);
    if (is_oxgrar(oi))
      oi= arr_oxgr(oi);
    if (is_obj(oi))
      use_method,oxscm_wrkr,s,oni,oi,nt+1;
    else
      s+= t2+"("+oni+" "+use_method(oxscm_pr,oi,fmt)+")\n";
  }
  s+= t+")\n";
}
func oxscm_scan (s, m, i, &mm)
// I marks open ( returns index of balancing )
{
  p= numberof(m);
  if (i<1 || i>p-1)
    error,"invalid search start index: "+pr1(i);
  if (strpart(s,i:i)!="(")
    error,"Expecting (";
  i0= i;
  j= 0;
  m0= m(i);
  while(j==0 || (j>0 && i<p && m(i)!=m0)) {
    if (j==0 && m(i)!=m0)
      j= 1;
    i++
  }
  i= i-1;
  if (strpart(s,i:i)!=")")
    error,"Expecting )";
  if (i<=i0+1)
    ss= string(0);
  else {
    ss= strpart(s,i0+1:i-1);
    mm= m(i0+1:i+1);
  }
  return ss;
}
func oxscm_pr (a,fmt)
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
ox2scm = save(oxscm, scmox_wrkr, oxscm_wrkr, oxscm_scan, oxscm_pr);
ox2scm = closure(ox2scm, oxscm);
restore, scratch;


#if 0
oscm= save();
oscm, s1= ["(","  ;; input grid specification","  (grid","    (","       (x0 -25.0)", \
           "       (x1  25.0)","       (nx 100)","    ))","  ;; output file", \
           "  ;; (output foo.scm)","  (output foo.scm)", \
           "  (polynomial (1.0 1.1 1.2 1.3 1.4))",")"];
oscm, s2= ["("," (months (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec))", \
           " (days (Mon Tue Wed Thu Fri Sat Sun))",")"];
oscm, s3= ["(calendar_analysis_report ((month 11) (name Nov) (input_calendar ((months (Jan Feb", \
           " Mar Apr May Jun Jul Aug Sep Oct Nov Dec)) (days (Mon Tue Wed Thu Fri Sat Sun)))",")))"];
oscm, s4= ["((grid ((x0 -25.0) (x1 25.0) (nx 100))) (xs (-25.00000000000000 -24.49494949494949",
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
write,oxscm(cfg),format="%s\n";
write,"",format="= done =\n";

scratch= save(scratch,q);
cfg= save(); {
  q= save(); {
    q, x0= -25.0;
    q, x1= 25.0;
    q, nx= 100;
  } cfg, grid=save(string(0),q);

  cfg, output= "foo.scm";
  cfg, polynomial= [1.0, 1.1, 1.2, 1.3, 1.4];
}
restore, scratch;

q= oxscm(cfg);
write,q,format="%s\n";
write,"",format="= done =\n";
o= oxscm(q);
info,o;

q= "\
 (\
  (grid\
    (\
       (x0 -25.0)\
       (x1  25.0)\
       (nx 100)\
    ))\
  (output foo.scm)\
  (polynomial (1.0 1.1 1.2 1.3 1.4))\
)";

o= oxscm(q);
info,o;

#endif
