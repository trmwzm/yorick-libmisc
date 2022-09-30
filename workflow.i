require, "yut.i";

/* SIMPLISTIC workflow engine, generates a DAG where:
       1 - rules are nodes/vertices
       2 - types are edges, directions
   from three sets:
   1 - set of all types, this can be gathered from in-/output from {rules}
   2 - set of all rules, name_of_rule + {inputs} + {outputs}
   3 - desired start/end types/products/dependencies
   
   NOTE:!no leading, or ending spaces in member names!

   With notation {} for oxy object:
   
   type: either a group of *identical* types, or an elementary non-group type.
         Non-group types are objects with only named members.
         When a member name ends in "_dims" then the preceding pattern is also 
         am array member name whose value is an array *and its dimension are
         as specified in "_dims" member and free to vary*. All other array
         dimensions are *fixed* and are type checked. Type member values
         may be types.

         Group types are not checked for dimensions (how many of the sub-types)
         
         Practically is necessary to define a "wrkfl_types" function
         registering all acceptable types and their name in the form of an oxy
         object (type templates.) Input and outputs of rules are type-checked
         against the registry. For group types, the registry simply needs to
         contain a single sub-type element named group. 

   input: {} of named types, subset of all types

   output: {} of named types
   
   rule:  {name_of_rule/node, input, output}

   data start:

   data products:
*/

func is_wrkfl_t (a, tmpl, quiet=)
/* DOCUMENT is_wrkfl_t (a, [name,] [quiet=])
   return in {1,0} is quiet==1, else 1 or error
   without TMPL, runs high level checks of the workflow type:
   1 - if it is a group of elementary types, check all types are
       identical, see (2)
   2 - if it is not a group, but an elementary type 
       1 - all members are named
       2 - if array dimensions are present, checks array sizes
       3 - recursively 1 & 2, if members are objects
   NOTE: TMPL is also checked, if present 
   SEE ALSO:
 */
{
  err= is_void(quiet)? 0: quiet;
  na= a(*);
  anm= a(*,);
  if (is_group(a)) {  // without TMPL, check all id.; with, check
    i= 0;
    l= is_void(tmpl)? 1: is_group(tmpl); 
    while (l==1 && i++<na)
      if (is_void(tmpl))
        l&= is_wrkfl_t(a(noop(i)),a(1),quiet=quiet);  // i==1 ;(
      else
        l&= is_wrkfl_t(a(noop(i)),tmpl(1),quiet=quiet);
  } else {
    // no non-named members
    l= is_obj(a)>0 && !is_stream(a) && noneof(a(*,)==string(0));
    l&= allof(a(*,)==strtrim(a(*,)));  // no leading or ending spaces
    l&= wrkfl_check_dims(a);    // arrays and group values dimension checks
    if (l && !is_void(tmpl))
      l&= oxeq(a,tmpl,0);
    i= 0;
    while (l==1 && i++<na)
      if (is_obj(a(noop(i))))
        l&= is_wrkfl_t(a(noop(i)),tmpl(anm(i)),quiet=quiet);
  }
  return l;
}

func wrkfl_check_dims (a)
/* DOCUMENT
   checks that "[sth]_dims" members describes the dimensions
   of the STH array value, or number of goup elements
   
   !! not recursive, used only as a worked for is_wrkfl_t
   SEE ALSO:
 */
{
  n= a(*,);
  // object members
  mo= is_obj(a,n);  // object mask
  io= anyof(mo);
  sg= strgrepm("_dims$",strtrim(n));  // _dims s:e 
  m= sg(2,..) >= 0;  // _dims mask
  l= 1;
  if (anyof(m)) {
    w= where(m);     // where _dims
    nw= numberof(w);
    sw= strpart(n(w),transpose([0,sg(1,w)]));  // array membname
    i= 0;
    while (l==1 && i++<nw)
      if (io && mo(w(i)))
        l&= is_scalar(a(w(i))) && is_obj(a,sw(i)) && a(w(i))==a(sw(i),*);
      else
        l&= allof(a(w(i))==dimsof(a(sw(i))));          
  }
  return l;
}
 
