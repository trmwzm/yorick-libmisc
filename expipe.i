require, "yut.i";

/* TODO:
   1. func converting given object into "null" template for typecheck
   2. testing testing testing

 */
/* BASIC type checks for the data exchanged in pipelines,
   not sure what will actually execute!... maybe [snake]make?
       1 - rules are nodes/vertices
       2 - types are edges, directions
   Build pipeline from three sets:
   1 - set of all types, this can be gathered from in-/out-put from {rules}
   2 - set of all rules, name_of_rule + {inputs} + {outputs}
   3 - desired start/end types/products/dependencies

   Types:
   [hierachical] oxy objects and groups represent types whithin yorick...
   Oxy restrictions and rules for types that may be checked:
   1 - no leading, or ending, spaces in member names, no un-named member (*) below
   2 - no function, streams, structs, _lists, ... as values, only basic kinds
   3 - (*) a group of IDENITICAL "types" checks as a group of one element
   4 - values are only numerical, strings, character, and objects
   5 - void matches all types
   6 - values not type matched, but types are, *and* dimension for arrays, UNLESS:
   7 - it is an object of a type belonging to the  WRKFL_EXT_RA_TS type group
   8 - ...

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

scratch= save(o,oo);
o= save(); {

  oo= save(); {
    oo, ext_type= "pdb";
    oo, pdbname= "dir/name.pdb";
  } save, o, pdb=oo;

  oo= save(); {
    oo, ext_type= "yorick-cfg";
    oo, ycfgname= "dir/name.i";
    oo, objnm= ["obj1","obj2"]
  } save, o, ycfg=oo;

  oo= save(); {
    oo, ext_type= "bin";
    oo, type= double;
    oo, fcomplex= 0;
    oo, dims= "dimsof";
    oo, binname= "dir/name.dat";
  } save, o, bin=oo;
}
wrkfl_ext_ra_ts= o;
restore, scratch;

func is_wrkfl_t (a, tmpl)
/* DOCUMENT is_wrkfl_t (a, [tmpl])
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
  if (!is_obj(a) && !is_void(a))
    return 0;
  na= a(*);
  if (na==0)   // void is all
    return 1;
  if (is_group(a)) {  // without TMPL, check all id.; with, check
    i= 0;
    l= is_void(tmpl)? 1: is_group(tmpl); 
    while (l==1 && i++<na)
      if (is_void(tmpl))
        l&= is_wrkfl_t(a(noop(i)),a(1),quiet=quiet);  // i==1 ;(
      else
        l&= is_wrkfl_t(a(noop(i)),tmpl(1),quiet=quiet);
  } else if (is_obj(a)) {
    anm= a(*,);
    // no non-named members
    l= noneof(anm==string(0));
    // no pre-/post-spaces
    l&= allof(anm==strtrim(anm));  // no leading or ending spaces
    // find "_extern$" keys with obj value: not type checked
    mo= is_obj(a,noop(anm));
    me= strgrepm("_extern$",anm)(2,..) >= 0;
    if (allof(mo & me))
      return l;
    if (anyof(mo & !me)) {
      w= where(mo & !me);
      i= 0; while (l==1 && i++<numberof(w))
        l&= is_wrkfl_t(a(w(i)),tmpl(anm(w(i))));
    }
    if (nallof(mo)) {
      w= where(!mo);
      l&= oxeq(a(noop(w)),tmpl(anm(w)),1);  // check kind, rank, and dimensions
    }
  }
  return l;
}

func to_wkfl_t (a)
/* DOCUMENT t= to_wkfl_t (o)
   T==[]: VOID return if not valid type

   convert into template object. Testing > is_wrkfl_t(o,to_wkfl_t(o));
   SEE ALSO:
 */
{
  if (!is_wrkfl_t(a))
    return [];
  na= a(*);
  if (na==1)   // void is all
    return save();

  if (is_group(a)) // group of type
    return save(string(0),to_wkfl_t(a(1)));
  else {
    n= a(*,);
    mo= is_obj(a,noop(n));  // object mask
    sg= strgrepm("_dims$",strtrim(n));  // _dims s:e
    md= sg(2,..) >= 0;  // _dims mask

    b= save();
    for (i=1;i<=a(*);i++) {
      if (mo(i))
        save,b,n(i),to_wkfl_t(a(noop(i)));
      else
        save,b,n(i),to_wkfl_t(a(noop(i)));
    }
    l= 1;
    if (anyof(m)) {
      w= where(m);     // where _dims
      nw= numberof(w);
      sw= strpart(n(w),transpose([0,sg(1,w)]));  // array membname
      i= 0;

    }
    return out;
  }
}

// task ruuner processes a task set sequentially. IO to disk is used for all(?) task in-/out-put
// task set is a group of { task object | group of task object}
// a task object is an oxy object with members ["run","dir",in","out"[,??]] - no non-named member
// RUN is the yorick funcion, or closure, name (string) the task runner will call using "exec"
//    - RUN function interface:
//      func run (in) {
//        // do stuff using inputs from files whose *names* are members (positional and names) of IN object
//        // write stuff to files whit names are values in OUT object
//      }
//      in- and out- arguments which contain
// IN is the object consisting of all input file names to RUN function
// DIR directory name, must be unique, or void, in which case -> "run-name"+"task#"[+"_"+"subtask#"[+...]]
//    - un-named==positional(?), named==keyword/val(better)
//    - any IN member my be void
//    - file names have a directory pointing to its origin==other task
//    - directory names are: function-name+"_"+task number: for example "azcomp_03_02", id DIR is poir
// OUT sane as IN, but for input
