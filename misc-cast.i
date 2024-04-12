func obj2pdb (obj, pdb, prims=)
/* DOCUMENT obj2pdb, obj, pdb
  Converts a **FLAT** Yorick group object to a PDB file.
  Caveat: Only group members witch are struct or val arrays
  and have non-nil key names will get saved.

  SEE ALSO: pdb2obj pointers2obj oxy
*/
{
  default, prims, i86_primitives;
  if (is_string(pdb))
    pdb= createb(pdb, prims);
  for (i=1; i<=obj(*); i++) {
    key= obj(*,i);
    val= obj(noop(i));
    if (!strlen(key) || !is_array(val))
      continue;
    if (typeof(val)=="struct_instance")
      save, pdb, nameof(structof(val)), structof(val);
    save, pdb, noop(key), val;
  }
}

func pdb2obj (pdb)
/* DOCUMENT obj= pdb2obj(pdb)
  Converts a PDB file to a Yorick group object.

  SEE ALSO: obj2pdb pointers2obj oxy
*/
{
  if (is_string(pdb))
    pdb= openb(pdb);
  obj= save();
  vars= *(get_vars(pdb)(1));
  for (i=1; i<=numberof(vars); i++)
    // Wrap the get_member in parens to ensure we don't end up with a
    // reference to the file.
    save, obj, vars(i), (get_member(pdb, vars(i)));
  return obj;
}
func struct2obj (data)
/* DOCUMENT struct2obj(data)
  Converts data that is held in a struct to an equivalent oxy object.
*/
{
  fields= get_members(data);
  count= numberof(fields);
  obj= save();
  for (i=1; i<=count; i++)
    save, obj, fields(i), get_member(data, fields(i));
  return obj;
}

func obj2struct (data, name=, ary=)
/* DOCUMENT obj2struct(data, name=, ary=)
  Converts an oxy group object to a struct instance.

  Parameter:
    data: An oxy group object

  Options:
    name= The name to use for the temporary struct created to initialize the
      result. This defaults to "temp_struct". This must be a valid Yorick
      variable name.
    ary= By default, a scalar result is returned. Use ary=1 if all members are
      arrays with the same dimensionality to return an array of struct
      instances with the same dimensionality.

  SEE ALSO: struct2obj
*/
{
  default, name, "temp_struct";
  default, ary, 0;

  if (!data(*))
    return [];

  bkp= [];
  if (symbol_exists(name))
    bkp= symbol_def(name);

  keys= data(*,);
  if (noneof(keys))
    error, "no keys found";
  // have to eliminate anonymous members
  keys= keys(where(keys));

  count= numberof(keys);
  sdef= ["struct "+name+" {\n"];
  for (i=1; i<=count; i++) {
    key= keys(i);
    val= data(noop(key));
    tmp= typeof(val) + " " + key;
    if (!ary && !is_scalar(val)) {
      dims= dimsof(val);
      ndims= numberof(dims);
      tmp += "(";
      for (j= 2; j<=ndims; j++) {
        tmp += swrite(format="%d", dims(j));
        if (j < ndims)
          tmp += ",";
      }
      tmp += ")";
    }
    tmp += ";\n";
    grow, sdef, tmp;
  }
  grow, sdef, "};\n";
  include, sdef, 1;

  if (ary)
    result= array(symbol_def(name), dimsof(val));
  else
    result= symbol_def(name)();

  for (i=1; i<=count; i++) {
    get_member(result, keys(i))= data(keys(i));
  }

  symbol_set, name, bkp;
  return result;
}

func list2obj (data)
/* DOCUMENT list2obj(data)
  Converts a Yorick list DATA into a Yorick oxy group object.
*/
{
  count= _len(data);
  result= save();
  for (i=1; i<=count; i++) {
    save, result, string(0), _car(data, i);
  }
  return result;
}

func args2obj (args)
/* DOCUMENT args2obj(args)
  Converts an ARGS object (from wrap_args) into a Yorick oxy group object.
*/
{
  obj= save();
  for (i=1; i<=args(0); i++)
    save, obj, string(0), args(noop(i));
  keys= args(-);
  count= numberof(keys);
  for (i=1; i<=count; i++)
    save, obj, keys(i), args(keys(i));
  return obj;
}

func bool (val)
/* DOCUMENT result = bool(val)
  Coerces its result a boolean values. RESULT will be an array of type
  char, where 0x00 is false and 0x01 is true.
  VAL is virtually anything as input. It is logically equivalent to
  the following:
    result = (val ? 0x01 : 0x00)
  However, it also works for arrays and will maintain their dimensions.
*/
{
  return char(!(!val));
}

func pointers2obj (pary)
/* DOCUMENT obj = pointers2obj(pary)
  Given an array of pointers PARY, this returns a object OBJ that contains the
  dereferenced pointers' contents such that obj(i) == *pary(i).

  SEE ALSO: obj2pbd pbd2obj oxy
*/
{
  obj= save();
  count= numberof(pary);
  for (i=1; i<=count; i++)
    save, obj, string(0), *pary(i);
  return obj;
}

func obj2pointers (obj)
/* DOCUMENT pary = obj2pointers(obj)
  Given an object OBJ, returns an array of pointers PARY that contains pointers
  to the object's contents such that *pary(i) == obj(i). Note that only values
  are maintained; key names are lost. Note also that all object members must be
  suitable for conversion to a pointer.
*/
{
  ptr= array(pointer, obj(*));
  for (i=1; i<=obj(*); i++)
    ptr(i)= &obj(noop(i));
  return ptr;
}
