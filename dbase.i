require, "yut.i";
require, "json.i";

scratch = save(scratch, tmp);
tmp = save(add, key, get, record, todox, fromdox, load, dump); //  write, read,
save, tmp, match, totext, fromtext;

func dbase(base, .., read=)
/* DOCUMENT > db= dbase(keyname1, keyname2, ...);
         or > database = dbase();
         or > database = dbase(obj); // sameas > db= dbase(obj(*,));
                                               > db, add, obj;
     create a database object DB with the given key names.  In the
     second and third forms, all  arrays in the *first*
     record will become keys for the database.

     Each record of the database is itself an object.  A record
     must contain one member whose name matches every key name;
     those key name members must all be arrays whose data type and
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

     ADD can be used to fill an empty DBASE with and existing one, or to
     join compatible dbases.

     MATCH select matching records from an object whose key/vals are used to
     match the database record/keys: for string partial matches are accepted.
     Multiple possible values as search criteria are submitted my using ARRAY
     values to the selection object
       !!only works for SCALAR key values !!
     EXAMPLE: > d= dbase();
              > d, fromtext, "$YORICK_LIB_SPL/conf/sys.txt",colrec=1;
              > w= d(match,save(type=["uav","asar-l"]));  // partial, or extract

     LOAD/DUMP methods, both accepting "json=1" flag, otherwise PDB file/name.
     d= dbase("nm");
     for(i=1;i<=100;i++)
       d,add,save(nm=swrite(i,format="x%i"),val=random(long(max(1,random()*100))));
     d,dump,"~/tmp/dbase_dump_test.pdb";
     dd= dbase();
     dd,load,"~/tmp/dbase_dump_test.pdb";

     TOTEXT
        write a scalar record member database object to string array.
        COLREC=1:  store records as columns, default is lines.
        usage:
        d= dbase();
        d, fromtext, fnm, [colrec=1]; // $YORICK_LIB_SPL/../conf/sys.txt, colrec=1

     FROMTEXT
        returns a dbase object from a simple ascii rep. in file or str array FNM.
        COLREC=1: treat columns are records, default is lines as records
        TAB=1:    treat tabs as spaces, or delim
        comma=1:  treat comma as spaces, or delim
        Ascii representation is:
        ^# for comments,
        records as columns# 2,3,...
        first column contains keynames
        ... convenience is to use first line as "type" ... not necessary.
        EXAMPLE: (COLREC=1)
        sysdesc= [                                                      \
        "type       uav-p        uav-l       uav-ka      asar-l      asar-s", \
        "bw         20.0e6       80.0e6      80.0e6      75.0e6      75.0e6", \
        "bws        22.5e6       90.0e6      90.0e6      83.33e6     83.33e6", \
        ....
        "#--- legend ---",                    \
        "#bw        radar bandwidth [Hz]",        \
        "#bws       complex sampling bandwidth [Hz]", \
        ...      ]
        DEG=1: special *_deg keys -> add radians non-key member named [*] to record
        DB=1:  special *_db keys -> add radians non-key member named [*] to record

     To save or retrieve a database object from file
     > db, write, filename;
        > db= dbase();
     > db, read, filename;
*/
{
  local klist, records;
  obj = base(:);  /* make copy of the dbase base class */
  ar1= next_arg();
  if (is_obj(ar1)) {
    if (is_group(ar1))
      keys= ar1(1,*,);
    else
      keys= ar1(*,);
  } else {
    keys= ar1;
    while (more_args())
      grow, keys, next_arg();
  }
  if (!is_void(keys) && structof(keys)!=string)
    error, "keys= must be an array of key names";
  if (!is_void(read)) {
      save, obj, keys, klist, records;
      obj,read,read;
      return obj;
  }
  klist = save();
  records = save();
  for (i=1 ; i<=numberof(keys) ; ++i) save, klist, keys(i), [];
  save, obj, keys, klist, records;
  if (is_obj(ar1)) {
    if (is_group(ar1)) {
      do (i=1;i<=ar1(*);i++)
           obj, add, ar1(noop(i));
    } else
      obj, add, ar1;
  }
  return obj;
}

func add (..)
{
  use, keys, klist, records;
  while (more_args()) {
    rec = next_arg();
    if (!is_obj(rec))
      error, "each dbase record must be an object";
    // check if the arg is a dbase
    if ((rec(*,)(-,..)==["klist","key","get","record","keys","records"])(*)(sum)==6) {
      for (i=1;i<=rec(records,*);i++)
        use_method, add, rec(records,noop(i));
    } else {
      n = records(*);
      if (!n) {                        /* this is first record */
        if (is_void(keys)) {
          keys = rec(*,);
          // for (i=1 ; i<=numberof(keys) ; ++i)
          //   if (!is_scalar(rec(keys(i))))
          //     keys(i) = string(0);
          // keys = keys(where(keys));
          if (!numberof(keys))
            error, "unable to guess at any dbase keys from first record";
        }
        for (i=1 ; i<=numberof(keys) ; ++i)
          save, klist, keys(i), \
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
}

func key(name)
{
  use, keys, records, klist;
  //eq_nocopy, keys, use(keys);
  if (is_void(keys))
    return [];
  if (numberof(name)) {
    if (nallof((name(-,..)==keys)(sum,..)))
      error, "unknown key name";
    keys2= name(*);
  }
  n= records(*);
  if (is_scalar(name))
    return klist(noop(name), .., 1:n);
  /* extract cumulative key lists, then truncate to number of records */
  out= save();
  for (i=1 ; i<=numberof(keys2) ; ++i)
    save, out, keys2(i), klist(keys2(i),..,1:n);
  return out;
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

func match (ok, verbose=)
{
  use, records, keys, klist;
  nd= records(*);
  if (is_void(ok))
    w= indgen(nd);
  else if (is_integer(ok)) {
    if (anyof(ok>nd | ok<1))
      error,"out og bounds.";
    return ok;
  } else {
    if (!is_obj(ok))
      error,"expecting database key selection object.";
    kk= ok(*,);
    if (nallof((kk(-,..)==keys)(sum,)))
      error,"keyname not found.";
    m= array(char(1),nd);     // all record mask, start with all OFF/1
    mj= array(char(0),nd);
    for (j=1;j<=ok(*);j++) {
      mj= char(0);     //
      for (i=1;i<=numberof(ok(noop(j)));i++)
        if (is_string(ok(noop(j),i)))
          mj|= strgrepm(ok(noop(j),i),use_method(key,ok(*,j)));  // fuzzy match on string value
        else
          mj|= use_method(key,ok(*,j))==ok(noop(j),i);
      m&= mj;
    }
    w= where(m);
  }
  if (verbose==1)
    write,format="Number of dbase records selected: %i\n",numberof(w);
  return w;
}
// func write (fnm)
// {
//     f= createb(fnm);
//     use, keys, klist, records;
//     oxsave,f,keys, klist, records;
// }

// func read (fnm)
// {
//     f= openb(fnm);
//     use, keys, klist, records;
//     oxrestore,f, keys;
//     oxrestore,f, klist;
//     oxrestore,f, records;
// }
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

  if ((strlen(fnmout)>5 && strpart(fnmout,-4:)==".json") || json==1)
    jsbox,o,fnmout,szmx=szmx;
  else
    oxsave, (f=createb(fnmout)), o;
}
func load (fnmin, json=)
{
  write,format="Reading dbase: %s\n",fnmin;
  if ((strlen(fnmin)>5 && strpart(fnmin,-4:)==".json") || json==1)
    oo= oxjsb(fnmin);
  else
    oo= oxrestore(openb(fnmin));

  save, use(), [], use_method(fromdox, oo);  // got that wrong, at first ...
}
func totext (f, colrec=, fmt=)
/* DOCUMENT text_db (db)
   write a scalar record member database object to string array.
   ONLY key/names/values are written to text, ***not*** other record members.
   COLREC=1:  store records as columns, default is lines.
   SEE ALSO:
 */
{
  use, keys, records;
  kn= numberof(keys);
  rn= records(*);
  s= colrec==1? array(string(0),rn+1,kn): array(string(0),kn,rn+1);
  for (i=1;i<=kn;i++) {
    if (colrec==1)
      s(1,i)= keys(i);
    else
      s(i,1)= keys(i);
    for (j=1;j<=rn;j++) {
      x= records(noop(j),keys(i));
      if (numberof(x)>1)
        error,"only scalar record members allowed.";
      if (colrec==1)
        s(j+1,i)= is_string(x)? x: totxt(x,fmt);
      else
        s(i,j+1)= is_string(x)? x: totxt(x,fmt);
    }
  }
  wmx= max(strlen(s));

  ss= swrite(s,format=swrite(-(wmx+1),format="%%%ds"))(sum,);
  if (!is_void(f)) {
    if (is_string(f))
      write,(f=open(f,"w")),ss,format="%s\n";
    else if (typeof(f)=="text_stream")
      write,f,ss,format="%s\n";
    return f;
  } else {
    return ss;
  }
}
func fromtext (fnm, colrec=, deg=, db=, tab=, comma=)
{
  use, keys;

  if (is_scalar(fnm))
    fl= text_lines(fnm);
  else
    fl= fnm;

  if (tab==1 || comma==1) {
    w= where(strgrepm((tab==1? "\t": ","),fl));
    if (numberof(w)) {
      ss= strchar((tab==1? "\t ": ", "));
      fl(w)= strtranslate(fl(w),strtrtable(ss(1),ss(2)));
    }
  }

  fl= strtrim(fl);
  // remove comments
  fl= fl(where(!strgrepm("^#",fl) & fl!=string(0)));

  q= text_cells(strchar((fl+"\n")(sum))(:-1)," ");
  m= q!=string(0);
  n= dimsof(q)(0);
  if (anyof(m))
    q= reform(q((w=where(m))),[2,numberof(w)/n,n]);

  // dbase with row label key: q(1,..);    // bw, bws, hp, ht, re, f0, la, ....
  if (colrec==1)
    keys= q(1,..);
  else
    keys= q(..,1);

  // add one DB object per table column past col#1
  dq= dimsof(q);
  if (colrec==1) {
    nr= dq(2)-1;
    nk= dq(3);
  } else {
    nr= dq(3)-1;
    nk= dq(2);
  }
  radeg= pi/180;
  for (i=2;i<=nr+1;i++) {
    ro= save();
    x= colrec==1? tonum(q(i,..),m): tonum(q(..,i),m);
    for (j=1;j<=nk;j++) {
      knm= colrec==1? q(1,j): q(j,1);
      val= m(j)>0? merge2(long(x(j)),x(j),m(j)==3): \
                          (colrec==1? q(i,j): q(j,i));
      save,ro,noop(knm),val;
      if (deg==1 && m(j) && strgrepm("_deg$",knm)) // add radians non-dbkey member
        save,ro,strpart(knm,:-4),x(j)*radeg;
      if (db==1 && m(j) && strgrepm("_deg$",knm)) // add lin scale non-dbkey member
        save,ro,strpart(knm,:-3),10^(x(j)/10.0);
    }
    use_method, add, ro;
  }
}
dbase = closure(dbase, restore(tmp));
restore, scratch;
