// append environment variables of the form $YORICK_LIB_[X]=/path/to
// as /path/to/lib:/path/to/contrib
tmp= save(tmp,t,m,w,i,s,ss,u,cmdl,verb);

cmdl= get_argv();
verb= noneof(cmdl == "-q");
 
t= rdfile(popen("env",0));
m= strgrepm("^YORICK_LIB_",t);
if (anyof(m)) {
  w= where(m);
  for (i=1;i<=numberof(w);i++) {
    s= strtok(t(w(i)),"=");
    if (strlen(s(2))) {
      u= s(2);
      do {
        ss= strtok(u,":");
        set_path, get_path()+":"+ss(1);
        if (!batch() && verb)
          write,ss(1),format="Adding "+s(1)+" to path: %s\n";
        u= ss(2);
      } while (strlen(u));
    }
  }
}
restore,tmp;

// remove blacklisted i-start (autoload) files
tmp= save(tmp,autoload_blacklist,i);
autoload_blacklist= ["ylib-start.i"];
autoload_blacklist= Y_HOME+"i-start/"+autoload_blacklist;
for (i=1;i<=numberof(autoload_blacklist);i++)
  rename,autoload_blacklist(i),autoload_blacklist(i)+".bkp";
restore, tmp;

pldefault, marks=0;
