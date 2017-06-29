func prettyprint(name,a,fd=)
/* DOCUMENT prettyprint,name,structure,fd=
   Print the ascii represention of structure "a" suitable
   for editting by hand and for subsequent #including. If fd
   is given will write to that file descriptor (default is
   stdout). (nil) strings are replaced by string(0) so resulting
   code is executable.  The string name is used as the name to
   assign the structure to.  Also calling prettyprint with a
   structure name, prints an executable structure definition.
 */
{
  local e,s,i,len,str,ll,n;
  //This is a simple case
  if("struct_definition"==typeof(a)) {
    strs=print(a);
    for(i=1;i<=numberof(strs);i++){
      write,fd,format="%s\n",strs(i);
    }
    return;
  } else {
    print_format,2000;
    strs=print(a);
    print_format,-1;
  }
  // make a long string of the structure
  n=numberof(strs);
  s="";
  for(i=1;i<=n;i++) if(strpart(s,0:0)!="\\") s+=strs(i);
  else s=strpart(s,1:-1)+strs(i);
  e=*pointer(s);
  //flags for char's in strings, so can iqnore
  ss= e=='\42'; //quote symbol
  ss(2:) &= !(e(:-1)=='\\');//correct for escape char
  ss= !((ss(psum))%2);
  //match brackets () which are used in structure definitions
  bb= ss&(e=='(');
  n=where(ss&(e==')'));
  if(numberof(n)) bb(n)= -1;
  w=where(bb);
  bb = bb(psum);//nesting depth
  //print out pieces
  if (!is_void(name)) write,fd,format="%s=",name;
  if(numberof(w)) {
    prpiece,fd,"",strpart(s,1:w(1)),ss(1:w(1));
  } else {
    prpiece,fd,"",s,ss(1:-1);
    return;
  }
  tabs_prev= str_prev= sss_prev= []
  for(i=2;i<=numberof(w);i++) {
    //indents
    tabs="  ";
    for(j=1;j<bb(w(i-1));j++) {tabs+="  ";}
    //need to look ahead to handle annoying (nil) strings
    str=strpart(s,w(i-1)+1:w(i));
    sss=ss(w(i-1)+1:w(i));
    if(i<numberof(w)) if(strpart(s,w(i)+1:w(i+1))=="nil)"){
      //merge and replace nil), correct ss flags
      str=strpart(str,1:-1)+"string(0)"+strpart(s,w(i+1)+1:w(i+2));
      no=numberof(sss);
      sss=array(1,no+8);
      sss(1:no)=ss(w(i-1)+1:w(i));
      grow,sss,ss(w(i+1)+1:w(i+2))
        i+=2;
    }
    if (str_prev&&i>2&&strpart(str_prev,0:0)==")"&&
        strpart(str,1:1)!="]"&&strpart(s,w(i):w(i))!=")")
      write,fd,tabs+",";
    prpiece,fd,tabs,str,sss;
    tabs_prev= tabs;str_prev= str; sss_prev=sss;
  }
  prpiece,fd,tabs,strpart(s,w(0)+1:0),ss(w(0)+1:0);
}

func prpiece(fd,tabs,s,ss)
  /* DOCUMENT prpiece,fd,tabs,s,ss
     Break up strings on variables, for prettyprint.
   */
{
  //special case
  if(s=="") return;
  //special case
  if(s==")") {
    write,fd,format="%s%s\n",tabs,s;
    return;
  }
  //special case of ',' after a ')'
  if(strpart(s,1:1)==",") {
    s=strpart(s,2:0);
    ss=ss(2:0);
  }
  maxlen=60; // maximum length of string
  // find all commas not in array definitions (ie not in [..])
  //   to use as break points.
  e=*pointer(s);
  if (numberof(ss)==numberof(e)-1) e=e(1:-1);
  ll= ss&(e=='[');
  n=where(ss&(e==']'));
  if(numberof(n)) ll(n)= -1;
  ll = ll(psum);
  if(e(1)!='[') { //Special case, not an array definition
    eold1=e(1); e(1)=',';
    eold0=e(0); e(0)=',';
    ll=where( (ll==0) & (ss&e==','));
    e(1)=eold1;
    e(0)=eold0;
  }
  else ll=where( (ll==0) & (ss&e==','));
  if(strpart(s,0:0)==")") s+=",";
  if(numberof(ll)<=1) { //force printing of string
    ll=[1,numberof(e)];
  }
  ll(1)-= 1;
  // loop through each definition and fold long ones on commas
  for(i=2;i<=numberof(ll);i++) {
    write,fd,format="%s",tabs;
    str=strpart(s,ll(i-1)+1:ll(i)); // pick a section
    len=ll(i)-ll(i-1)+1;
    offset=0;
    // fold if too long
    while(len>maxlen) {
      n=where(e(ll(i-1)+offset+1:ll(i-1)+offset+maxlen+1)==',');
      if(numberof(n)==0) {
        break; //no comma to split on, print it all
      }
      else {
        write,fd,format="%s\n%s%s",strpart(str,1:n(0)),tabs,"  ";
        str=strpart(str,n(0)+1:0);
        len -= n(0);
        offset+=n(0);
      }
    }
    write,fd,format="%s%s\n",tabs,str; // print last one
  }
} 

func info_struct(sname,max_content_length=,fd=)
/* DOCUMENT print_struct,sname
   Nice printout of a structure content.
   Sname should be a structure element or structure array,
   passed as a STRING.
   ex: if ss is a structure array:
   print_struct,"ss"
   or print_struct,"ss(1)"
   SEE ALSO:
 */
{
  if (typeof(sname)!="string") error,"print_struct argument must be a string";
  local strvar;

  if (max_content_length==[]) max_content_length = 36;
 
  include,["strvar="+sname],1;

  for (ne=1;ne<=numberof(strvar);ne++) {
    if (ne>1) write,fd,format="%s\n","";
    if (numberof(strvar)==1) {
      write,fd,format="Structure %s\n",sname;
    } else {
      write,fd,format="Structure %s(%d)\n",sname,ne;
    }
    d = strpart(strtok(sum(print(strvar(ne))),"(")(2),1:-1);
    d = strtok(d,"\"",1001);
    // strings are all odd members:
    s = d(indgen(500)*2);
    w = where(s);
    if (numberof(w)) {
      s(w) = streplace(s(w),strfind("=",s(w)),"\024");
      s(w) =  "\""+s(w)+"\"";
      d(indgen(500)*2) = s;
    }
    d = sum(d);
    d = strtok(d,"=",1000);
    d = d(where(d));
    nkeys = numberof(d)-1;
    keys = vals = [];
    grow,keys,d(1);
    // keys:
    for (i=2;i<=nkeys;i++) {
      r = strtok(d(i),",",1000);
      r = r(where(r));
      val = sum(r(1:-1)+",");
      val = strpart(val,1:-1);
      grow,keys,r(0);
      grow,vals,val;
    }
    grow,vals,d(nkeys+1);

    vals = streplace(vals,strfind("\024",vals),"=");

    maxl = max(strlen(keys));
    fmt = "%-"+swrite(format="%d",maxl)+"s (%1s%-6s,";
    fmt3 = ") = %s\n";
    for (i=1;i<=nkeys;i++) {
      com = swrite(format="to = typeof(%s(%d).%s)",sname,ne,keys(i));
      include,[com],1;
      if (to=="pointer") {
        prefix = "&";
        com = swrite(format="to = typeof(*%s(%d).%s)",sname,ne,keys(i));
        include,[com],1;
        com = swrite(format="dof = dimsof(*%s(%d).%s)",sname,ne,keys(i));
        include,[com],1;
        if (numberof(dof)>0) {
          com = swrite(format="content = sum(print(*%s(%d).%s))",sname,ne,keys(i));
          include,[com],1;
          vals(i) = content;
        }
      } else {
        prefix = " ";
        com = swrite(format="dof = dimsof(%s(%d).%s)",sname,ne,keys(i));
        include,[com],1;
      }
      if (strlen(vals(i))>max_content_length) {
        vals(i) = strpart(vals(i),1:max_content_length)+"...";
      }
   
      write,fd,format=fmt,keys(i),prefix,to;
      if (numberof(dof)==0) dofs = "[]";
      if (numberof(dof)==1) {
        dofs = swrite(format="[%d]",dof(1));
        if (dof(1)==0)         dofs = "scalar";
      }
      if (numberof(dof)==2) dofs = swrite(format="[%d,%d]",dof(1),dof(2));
      if (numberof(dof)==3) dofs = swrite(format="[%d,%d,%d]",dof(1),dof(2),dof(3));
      if (numberof(dof)==4) dofs = swrite(format="[%d,%d,%d,%d]",dof(1),dof(2),dof(3),dof(4));
      write,fd,format="%-8s",dofs;
      write,fd,format=fmt3,vals(i);
    }
  }
}
