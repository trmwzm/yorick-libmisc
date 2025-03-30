if (is_void(orequire))
  orequire= require;

func rrequire (hist, source, verbose=)
/* DOCUMENT rrequire, source
  SOURCE should be a scalar string, to be interpreted as a filename like
  "yorick_source.i". This file will be sourced if it hasn't already been.

  Unlike the builtin require, rrequire is recursive-aware. It detects
  dependency loops and will refrain from re-requiring a file if it's currently
  already being required.

  Typically, the builtin require will be replaced by rrequire and the original
  require will be renamed to orequire.
*/
{
  if (noneof(hist(prev)==source)) {
    hist,np= hist(np)+1;
    if (hist(np)>numberof(hist(prev)))
        hist,prev=_(hist(prev),array(string(0),hist(n)));
    hist(prev,hist(np))= source;
    orequire, source;
    if (verbose && hist(np)>0)
      hist(prev,1:hist(np));  // see list
  }
}
rrequire= closure(rrequire, save(prev=array(string(0),100),np=0,n=100));  // same 100 ...

require= rrequire;
