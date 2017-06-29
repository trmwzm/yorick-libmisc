func moveop(op,f,n)
/* DOCUMENT
   minimum/max/median of the previous N samples
   for samples after N (N-1 first samples are replicated.)
   F has one dimension
   OP is MIN, or MAX, or MEDIAN
   SEE ALSO:
 */
{
  // dim f is N, dim f(i:-n+i) is N-(i-1)-n+i=N-n+1
  sop= info(op)(1);
  ff= array(structof(f),n,numberof(f)-n+1);
  for (i=1;i<=n;i++)
    ff(i,..)= f(i:-n+i);
  if (strmatch(sop,"min")) {
    return _(f(:n-1),ff(min,..));
  } else if (strmatch(sop,"max")) {
    return _(f(:n-1),ff(max,..));
  } else if (strmatch(sop,"median")) {
    return _(f(:n-1),median(ff,1));
  } else errror,sop+" operation not {min,max,median}";
}
