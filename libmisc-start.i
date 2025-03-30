// append environment variables listed in YORICK_ENV_LIBS
// which might be in  YORICK_LIB_[X]=/path/to form ;p
// as /path/to/lib:/path/to/contrib to yorick load path
require,"pathfun.i";
require,"yut.i";
// require,"rrequire.i";   // log reqs: require,"q.i",verbose=1;

tmp= save(tmp,s,sl,m,nm,i,ss,u,cmdl,verb);

cmdl= get_argv();
verb= noneof(cmdl == "-q");

s= get_env("YORICK_ENV_LIBS");
if (s!=string(0)) {
  sl= strpart(s,strword(s,":",20));
  m= sl!=string(0);
  nm= m(sum);
  for (i=1;i<=nm;i++) {
    s= get_env(sl(i));
    if (strlen(s)) {
      u= s;
      do {
        ss= strtok(u,":");
        l= get_path()+":"+ss(1);
        l= discrete(pathsplit(l,delim=":"));
        l= pathform(l,delim=":");
        set_path,l;
        if (!batch() && verb)
          write,ss(1),format="Adding "+sl(i)+" path: %s\n";
        u= ss(2);
      } while (strlen(u));
    }
  }
}
close,fenv;

restore,tmp;

// remove blacklisted i-start (autoload) files
tmp= save(tmp,autoload_blacklist,i);
autoload_blacklist= [ \
    "ylib-start.i", \
    "regex-start.i", \
    "tiff-start.i", \
    "yeti-start.i", \
    "yhdf-start.i", \
    "fftw-start.i"];   // yeti and ylib

autoload_blacklist= Y_HOME+"i-start/"+autoload_blacklist;
for (i=1;i<=numberof(autoload_blacklist);i++)
  if (open(autoload_blacklist(i),"r",1))
    rename,autoload_blacklist(i),autoload_blacklist(i)+".bkp";
restore, tmp;

Y_ETC= Y_HOME+"etc/";

pldefault, marks=0;
