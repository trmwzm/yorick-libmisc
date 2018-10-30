require, "yut.i";
require, "json.i";

scratch = save(scratch, tmp);
tmp = save(add, key, get, record, write, read, todox, fromdox, load, dump);

func dbase(base, .., read=)
/* DOCUMENT > db= dbase(keyname1, keyname2, ...);
         or > database = dbase();
     create a database object DB with the given key names.  In the
     second form, all scalar arrays in the *first* record will
     become keys for the database.

     Each record of the database is itself an object.  A record
     must contain one member whose name matches every key name;
     those key name memebrs must all be arrays whose data type and
     dimensions are the same for every record.  Beyond the key
     name members, a record may contain any number of additional
     members.  The additional members need not be the same for
     different records, that is, just because one record contains
     a non-key member "foo" does not mean that any other record
     contains "foo", nor, if it does, that "foo" have any common
     type or shape across different records.

     You add record objects to a database with:
       > database, add, obj1, obj2, ...; // ADD method

       Records are numbered in the order they are added, and you can
     retrieve the i-th record with:
       > obj = database(record, i);      // RECORD method, 1st use
     Once you have retrieved a record object in this manner, you
     are free to modify any *non-key* members of obj -- the retrieved
     obj is *not* a copy of the object in the database, but the
     object itself.  If you want to replace record i with a new
     object (or modify the key members of an existing object),
     you can use a second form of the record method:
       > database, record, i, new_obj;   // RECORD method, 2nd use
     If i is an index range or an index list, database(record,i)
     returns an object containing all the specified record objects.
     The replacement form (setting a new_obj) only works when i is
     a scalar record number.

     The related method GET is like record, but it extracts the
     specified record or records *as a new database object*:
       > small_database = database(get, i); // GET method

     You can retrieve keys as arrays whose final index is the
     record number.  The add method builds these arrays from the
     key members as you add each record.  The syntax is:
       key = database(key, keyname);       // KEY method
     Again, if keyname is a *array* of names, the key method returns
     an object containing each of the named lists of key member
     values.  You can use these key lists to query your database
     and build index lists for the record or get methods:
       > list = where(database(key,"x") < 1.5);

    To save or retrieve a database object from file
       > db, write, filename;
       > db= dbase();
       > db, read, filename;
 */
{
  local keys, klist, records;
  while (more_args()) grow, keys, next_arg();
  if (!is_void(keys) && structof(keys)!=string)
    error, "keys= must be an array of key names";
  obj = base(:);  /* make copy of the dbase base class */
  if (!is_void(read)) {
      save, obj, keys, klist, records;
      obj,read,read;
      return obj;
  }
  klist = save();
  records = save();
  for (i=1 ; i<=numberof(keys) ; ++i) save, klist, keys(i), [];
  save, obj, keys, klist, records;
  return obj;
}

func add(..)
{
  use, keys, klist, records;
  while (more_args()) {
    rec = next_arg();
    if (!is_obj(rec)) error, "each dbase record must be an object";
    n = records(*);
    if (!n) {                        /* this is first record */
      if (is_void(keys)) {
        keys = rec(*,);
        for (i=1 ; i<=numberof(keys) ; ++i)
          if (!is_scalar(rec(keys(i)))) keys(i) = string(0);
        keys = keys(where(keys));
        if (!numberof(keys))
          error, "unable to guess at any dbase keys from first record";
      }
      for (i=1 ; i<=numberof(keys) ; ++i)
        save, klist, keys(i),
          array(structof(rec(keys(i))), dimsof(rec(keys(i))), 4);
    } else if ((n>3 && !(n&(n-1))) || numberof(klist(keys(1)))<=n) {  /* double all key lists */
      local list;
      for (i=1 ; i<=numberof(keys) ; ++i) {
        eq_nocopy, list, klist(keys(i));
        save, klist, keys(i), grow(list, array(structof(list),dimsof(list)));
      }
    }
    n += 1;
    for (i=1 ; i<=numberof(keys) ; ++i) {  /* add all key values to klists */
      eq_nocopy, list, klist(keys(i));
      list(..,n) = rec(keys(i));
    }
    save, records, string(0), rec;   /* add record itself to records member */
  }
}

func key(name)
{
  local keys;
  eq_nocopy, keys, use(keys);
  if (is_void(keys)) return [];
  if (numberof(name)) {
    if (nallof((name(-,..)==keys)(sum,..))) error, "unknown key name";
    keys = name(*);
  }
  n = use(records,*);
  if(is_scalar(name))
    return use(klist, noop(name), .., 1:n);
  /* extract cumulative key lists, then truncate to number of records */
  klist = save();
  for (i=1 ; i<=numberof(keys) ; ++i)
    save, klist, keys(i), use(klist,keys(i),..,1:n);
  return klist;
}

func record(i, value)
{
  if (is_void(value)) return use(records,noop(i));
  if (!is_scalar(i)) error, "set record needs scalar record number";
  if (!is_obj(value)) error, "each dbase record must be an object";
  local keys;
  eq_nocopy, keys, use(keys);
  for (j=1 ; j<=numberof(keys) ; ++j) {  /* add all key values to klists */
    eq_nocopy, list, use(klist, keys(j));
    list(..,i) = value(keys(j));
  }
  recs = use(records);
  save, recs, noop(i), value;
}

func get(i)
{
  if (is_scalar(i)) i= [i];
  obj = use(:);
  keys = obj(keys);
  klist = obj(klist, :);
  for (j=1; j<=numberof(keys) ; ++j)
    save, klist, keys(j), obj(klist, keys(j), .., i);
  save, obj, klist, records=obj(records, noop(i));
  return obj;
}

func write (fnm)
{
    f= createb(fnm);
    use, keys, klist, records;
    oxsave,f,keys, klist, records;
}

func read (fnm)
{
    f= openb(fnm);
    use, keys, klist, records;
    oxrestore,f, keys;
    oxrestore,f, klist;
    oxrestore,f, records;
}
func todox (void)
{
  return oxprune(use(),nofunc=1);
}
func fromdox (dox)
{
  return oxmerge(use(),dox);
}
func dump (fnmout, json=, szmx=)
{
  write,format="Write dbase: %s\n",fnmout;

  o= use_method(todox,);
  
  if (json==1) {
    s= oxjsn(oxjsb(o,rootdir=dirname(fnmout),szmx=szmx));
    write,open(fnmout,"w"),s,format="%s";
  } else 
    oxsave, (f=createb(fnmout)), o;
  return f;
}
func load (fnmin, json=)
{
  write,format="Reading dbase: %s\n",fnmin;
  if (json==1)
    oo= jsbox(jsnox(text_lines(fnmin)));
  else 
    oo= oxrestore((f=openb(fnmin)));
  save, use(), [], use_method(fromdox, oo);  // got that wrong, at first ...
  return f;
}
dbase = closure(dbase, restore(tmp));
restore, scratch;
