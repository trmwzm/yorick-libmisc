scratch = save(scratch, tmp);
tmp = save(add, set, get, help);

/* DOCUMENT s= shellargs();
            s= shellargs("-a", save(val=1,help="option a"[,flag=1])) 
   s is a shell argument parsing object
   methods:
   -------
     add
         s, add, "-a", save(val=1,help="option a"[,flag=1]);
            declare or register an option.
            * first arg is comand line key string.
            * second arg is object with default val and help string.
            * use flag=1 if the key is to be used as a flag only. 
     help
         s, help;
            display options.
     set
         w= s(set, ["-a","7","pos"]);
            parse the command line, sets options.
            * first arg is the "chopped" command line: get_argv()(4:)
            * output are the indices of non-key, positional, comand line args.
     get           
         a= s(get,"-a"[,v=1][,set=1])
            extract options, or check if they were set (use set=1).
            * first arg is key as it was declared using the add method.
            * use v=1 for a verbose value reset.

   template script:
   ---------------
  #!yorick -i
  write,"^D quits on error";
  require,"shellargs.i";

  ar= get_argv();
  ar1= 4;

  opt= shellargs();
  opt, add, "-a", save(val=2,help="set option a");  // use flag=1 if no command line value

  if ((nar=numberof(ar))<ar1) {
    write,"USAGE  : doit input";
    write,"OPTIONS:";
    opt, help;
    quit;
  } 

  ar= ar(ar1:);

  wp= opt(set,ar);

  if ((np=numberof(wp))) input= ar(wp);
  if (np>1) error,"too many positional args: "+pr1(np)

  a= opt(get,"-a",v=1);
  //to check if option was set
  if (opt(get,"-a",set=1)) write,"option a was set";
  quit;
 */
func shellargs(base, ..)
{
  obj= base(:);  /* make copy of the dbase base class */
  dat= save();
  save, obj, dat;
  while (more_args()>1) obj, add, next_arg(), next_arg();
  return obj;
}
func add(..)
{
  use,dat;
  while (more_args()) {
    a1= next_arg();
    a2= next_arg();
    if (!is_obj(a2)) error,"not obj save(val=,help=(string),[flag=1 for key-flag]).";
    if (is_obj(a2,val,1)<0) save,a2,val= 0; 
    if (is_obj(a2,help,1)<0) save,a2,help= ""; 
    if (is_obj(a2,flag,1)<0) save,a2,flag= 0; 
    if (is_obj(a2,type,1)<0) save,a2,type= structof(a2.val); 
    if (is_obj(a2,set,1)<0) save,a2,set= 0;
    if (is_obj(a2,dims,1)<0) save,a2,dims= dimsof(a2.val);
    if (structof(a1)==string &&
        structof(a2.help)==string) save, dat, noop(a1), a2;
    else
      error,"provide save(val=,help=,[flag=1 for key-flag]))";
  }
}
func set(ar)
{
  // inited args
  use, dat;
  if ((nai=dat(*))==0||is_void(ar)) return; 
  // command line (stripped?)
  local mf;            // numeric args mask
  num= tonum(ar,mf);
  wf= where(mf);      // num arg
  nwf= numberof(wf);
  ws= where(!mf);     // string arg
  nws= numberof(ws);
  if (nws) mk= (strpart(ar,1:1)=="-")&!mf;
  wk= where(mk);
  nwk= numberof(wk);  // keyword or keyflag arg
  // processing
  na= numberof(ar);
  mall= array(0,na);
  if (nwk) {          // any keyword or keyflags in args
    for (ia=1;ia<=nai;ia++) {  // loop inited flags
      da= dat(*,ia);
      if (strlen(da) &&\
          numberof((w=where(ar(wk)==da)))) {  // arg key match
        if (numberof(w)>1) 
          error,"duplicate key: "+da;
        else
          w= w(1);
        if (!dat(noop(ia)).flag) {  // keyword
          typ= dat(noop(ia)).type;
          dims= dat(noop(ia)).dims;
          for (n=1,i=1;i<=dims(1);i++) n*=dims(i+1);  // n=numberof(val)  
          iv= dims(0)==0? wk(w)+1: indgen(wk(w)+1:wk(w)+n);
          // if (w(1)==na||anyof(iv>numberof(mk))||anyof(mk(iv))) 
          if (w==na||iv(1)>numberof(mk)||anyof(mk(iv))) // donnot know which is right - below?
            error,"keyword needs a value: "+da; 
          if (dims(0)==0) 
            save,dat(noop(ia)),val=\
            (typ==string? ar(iv): typ(num(iv)));
          else
            save,dat(noop(ia)),val=\
            (typ==string? reform(ar(iv),dims) :reform(typ(num(iv)),dims));
          save,dat(noop(ia)),set= 1;
          mall(wk(w):wk(w)+n)= 1;     // flag processed args
        } else {                      // keyflag
          save,dat(noop(ia)),val= 1;
          save,dat(noop(ia)),set= 1;
          mall(wk(w))= 1;     // flag processed args
        } 
      }
    }
  }
  if (anyof((m=!mall & mk))) error,"unknown key: "+ar(where(m))(sum);
  mp= !mall;
  // bad-idea/hidden-fearture (default positional args) turn on if any (def. pos. arg)
  if (numberof(where(strlen(dat(*,))==0))) {
    if (anyof(mp)) {   // any positional args
      wp= where(mp);
      np= numberof(wp);
      for (ip=0,ia=1;ia<=nai&&ip<np;ia++) {  // loop inited flags
        da= dat(*,ia);
        if (strlen(da)==0) {
          ip+= 1;
          typ= dat(noop(ia)).type;
          save,dat(noop(ia)),val= (typ==string? ar(wp(ip)) :typ(num(wp(ip))));
          save,dat(noop(ia)),set= 1;
          mall(wp(ip))= 1;
        }
      }
    }
    if (anyof((m=!mall & mp))) error,"too many positional args: "+ar(where(m))(sum);
  }
  return where(mp);
}
func get(kp,v=,set=)
{
  restore, use, dat;
  da= dat(*,);
  mp= strlen(da)==0;
  wp= where(mp);
  np= numberof(wp);
  if (structof(kp)==string) {        // key-word or -flag arg
    if (noneof(strmatch(da,kp))) error,"unknown key.";
    if (set==1) 
      out= dat(noop(kp)).set; 
    else
      out= dat(noop(kp)).val; 
    hlp= dat(noop(kp)).help;
  } else if (structof(kp)==long||structof(kp)==int) {   // positional arg
    if (kp>np) error,"not that many positional args.";
    if (set==1) 
      out= dat(wp(kp)).set; 
    else
      out= dat(wp(kp)).val;
    hlp= dat(wp(kp)).help;
  } else {
    error,"incorrect arg type.";
  }
  if (v==1) {
    s= swrite(out);
    if ((ns=numberof(s))>1) 
      s= "["+(s(1:min(3,ns))+", ")(sum)+(ns>3? "...]": "]");
    write,hlp,s,format="Option: %-30s %s\n";
  }
  return out;
}
func help 
{
  restore, use, dat;
  na= dat(*);
  da= dat(*,);
  mp= strlen(da)==0;
  wp= where(mp);
  np= numberof(wp);
  wk= where(!mp);
  if (keyonly!=1)
    for (ip=1;ip<=np;ip++) {
      typ= dat(wp(ip)).type;
      typcst= typ==string? typ : double;
      write,ip,dat(wp(ip)).help,nameof(typ),typcst(dat(wp(ip)).val),\
        format="  Pos#% i:  %-30s [def = %s(%"+(typ==string?"s":"g")+")]\n";
    }
  for (ik=1;ik<=na-np;ik++) {
    typ= dat(wk(ik)).type;
    dims= dat(wk(ik)).dims;
    typcst= typ==string? typ : double;
    write,da(wk(ik)),dat(wk(ik)).help,nameof(typ),\
      typcst(dat(wk(ik)).val)(1),\
      (dat(wk(ik)).flag? "is_flag": ""),\
      (dims(0)==0? "": "dims="+pr1(dims)),\
      format="%8s:  %-30s [def = %s(%"+(typ==string?"s":"g")+")] %s %s\n";
  }
}
shellargs = closure(shellargs, restore(tmp));
restore, scratch;
