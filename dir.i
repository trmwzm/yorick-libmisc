require, "util_str.i";
require, "pathfun.i";

func file_split(fn)
/* DOCUMENT file_split(fn)
  Splits a path into its component parts and returns them as an array.

  If the path is an absolute path, the first array element will be "/".

  Only works on scalar strings.
*/
{
  // David Nagle 2008-12-24
  parts = strsplit(fn, "/");
  if (strpart(fn, 1:1) == "/")
    parts(1) = "/";
  return parts;
}

func file_join(..)
/* DOCUMENT file_join(part1, part2, part3, ...)
        file_join([part1, part2, part3, ...])
  Joins a list of component parts into a valid path. Returns a single string.

  This properly handles arbitrarily complicated paths as noted:
    .. is recognized and discards a path component when appropriate
    . is recognized and is discarded
    / (and /foo) is recognized and will result in all prior parts being
      ignored

  If passed multiple arguments, each argument may be either a scalar or an
  array of strings. Arguments must have conformable dimensions with one
  another.
*/
{
  // David Nagle 2008-12-24
  parts = array(pointer, 4);
  arrays = array(short, 4);
  idx = 0;
  while(more_args()) {
    idx++;
    if (idx > numberof(parts)) {
      grow, parts, parts;
      grow, arrays, arrays;
    }
    parts(idx) = &next_arg();
    arrays(idx) = dimsof(*parts(idx))(0) > 0;
  }
  parts = parts(:idx);
  arrays = arrays(:idx);

  result = [];

  if (numberof(parts) == 1) {
    // Current format!
    expanded = [];
    parts = *parts(1);
    // Component paths might not be single components. We have to split them
    // up to handle special cases like .. or /foo
    for(i = 1; i <= numberof(parts); i++) {
      grow, expanded, file_split(parts(i));
    }
    parts = expanded;
    expanded = cleaned = [];
    for(i = 1; i <= numberof(parts); i++) {
      part = parts(i);
      if (part == "/") {
        // / indicates that the path is restarting; everything prior gets
        // thrown out
        cleaned = ["/"];
      } else if (part == ".") {
        // . doesn't change the path, so we can throw it out
      } else if (part == string(0)) {
        // treat a nil string as if it weren't there
      } else if (part == "") {
        // the empty string also can be discarded
      } else if (part == "..") {
        // .. is complicated, depending on what preceeds it
        // If the path ends with a .., then we need to add this ..
        // If the path does not end with a .., then we get rid of the last
        //   element
        // If the path is empty, then we start the path out with ..
        // If the path is ["/"], then we simply ignore ..
        if (numberof(cleaned)) {
          if (cleaned(0) == "..") {
            grow, cleaned, "..";
          } else if (cleaned(0) == "/") {
            // do nothing
          } else {
            if (numberof(cleaned) > 1)
              cleaned = cleaned(:-1);
            else
              cleaned = [];
          }
        } else {
          cleaned = [".."];
        }
      } else {
        // Anything else just gets added to the list
        grow, cleaned, part;
      }
    }
    parts = cleaned;
    cleaned = [];
    joined = strjoin(parts, "/");
    if (parts(1) == "/") {
      joined = strpart(joined, 2:);
    }
    result = joined;
  } else {
    // more than one argument: need to coalesce into arrays

    // See if there are any arrays among the arguments
    w = where(arrays);
    if (numberof(w)) {
      // Make sure all the arrays have equivalent dimensions
      dims = dimsof(*parts(w(1)));
      for(i = 1; i <= numberof(w); i++) {
        accum_dimsof, dims, *parts(w(i));
        if (is_void(dims))
          error, "Non-conformable arrays were passed.";
      }
      // Broadcast all
      for(i = 1; i <= numberof(parts); i++)
        parts(i) = &(*parts(i) + array(string, dims));
      // Now iterate through and join each one
      result = array(string, dims);
      for(i = 1; i <= numberof(*parts(1)); i++) {
        temp = [];
        for(j = 1; j <= numberof(parts); j++) {
          grow, temp, (*parts(j))(i);
        }
        result(i) = file_join(temp);
      }
    } else {
      // No arrays were found
      new_parts = [];
      for(i = 1; i <= numberof(parts); i++) {
        grow, new_parts, *parts(i);
      }
      result = file_join(new_parts);
    }
  }

  return result;
}

func file_pathtype(path)
/* DOCUMENT file_pathtype(path)
  Returns "relative" or "absolute" for each path, depending on whether the
  path is relative or absolute. This works on both scalars and arrays.

  Absolute paths are defined as those that begin with / or ~. All other paths
  are relative.
*/
{
  result = array("relative", dimsof(path));
  w = where(strpart(path, 1:1) == "/");
  if (numberof(w))
    result(w) = "absolute";
  w = where(strpart(path, 1:1) == "~");
  if (numberof(w))
    result(w) = "absolute";
  return result;
}

func file_commonpath(path, ..)
/* DOCUMENT file_commonpath(paths, paths, ...)
  Given a series of paths, this will return the common directory under which
  all given paths can be found. All given paths must be absolute paths.
  Arguments can be scalars or arrays; output is always a single scalar.
*/
{
  while(more_args())
    grow, path, next_arg();
  if (numberof(path) <= 1)
    return numberof(path) ? path(1) : [];
  if (anyof(file_pathtype(path)) == "relative")
    return [];
  len = strlen(path)(min);
  path = strpart(path, 1:len);
  while(nallof(path == path(1))) {
    len--;
    path = strpart(path, 1:len);
  }
  path = path(1);
  if (strpart(path, 0:0) == "/")
    return strpart(path, :-1);
  else
    return dirname(path);
}

func file_relative(base, dest)
/* DOCUMENT file_relative(base, dest)
  Returns a relative path for dest as referenced against base.

  Works with scalars and arrays. If base and dest are both arrays, they must
  have the same dimensions.
*/
{
  // adapted from fileutil::relative in Tcllib
  bdims = dimsof(base);
  ddims = dimsof(dest);
  result = [];
  if (numberof(base) > 1) {
    result = array(string, bdims);
    if (numberof(dest) > 1) {
      if (numberof(bdims) != numberof(ddims))
        error, "Non-conformable arrays were passed.";
      if (numberof(bdims) != numberof(where(bdims==ddims)))
        error, "Non-conformable arrays were passed.";
      for(i = 1; i <= numberof(base); i++) {
        result(i) = file_relative(base(i), dest(i));
      }
    } else {
      for(i = 1; i <= numberof(base); i++) {
        result(i) = file_relative(base(i), dest);
      }
    }
  } else if (numberof(dest) > 1) {
    result = array(string, ddims);
    for(i = 1; i <= numberof(dest); i++) {
      result(i) = file_relative(base, dest(i));
    }
  } else {
    dest = dest(1);
    if (file_pathtype(base) != file_pathtype(dest))
      error, "Unable to compute relation for paths of different path types.";

    base = file_split(file_join(pwd(), base));
    dest = file_split(file_join(pwd(), dest));

    while(base(1) == dest(1)) {
      base = numberof(base) > 1 ? base(2:) : [];
      dest = numberof(dest) > 1 ? dest(2:) : [];
      if (!numberof(dest) || !numberof(base)) break;
    }

    if (numberof(base) == 0 && numberof(dest) == 0) {
      // Case 1: base == dest
      result = ".";
    } else {
      // Case 2: base is base/sub = sub
      //         dest is base     = {}
      // Case 3: base is base     = {}
      //         dest is base/sub = sub

      if (numberof(base))
        dest = grow(array("..", numberof(base)), dest);
      result = file_join(dest);
    }
  }
  return result;
}

func find(path, searchstr=)
/* DOCUMENT find(path, searchstr=)

  Finds all files in PATH that match the pattern(s) in SEARCHSTR. SEARCHSTR
  defaults to "*" and can be an array of patterns, in which case files that
  match any pattern will be returned (it uses "or", not "and").

  Full path and filename will be returned for each file.
*/
{
  if (!is_scalar(path)) {
    results = array(pointer, numberof(path));
    for(i = 1; i <= numberof(path); i++)
      results(i) = &find(path(i), searchstr=searchstr);
    return set_remove_duplicates(merge_pointers(results));
  }

  fix_dir, path;
  default, searchstr, "*";
  if (numberof(searchstr) > 1)
    searchstr=searchstr(:); // Seems to improve performance for some reason
  results = subdirs = [];
  files = lsdir(path, subdirs);
  if (files == 0)
    return [];
  if (numberof(files)) {
    idx = array(0, numberof(files));
    for(i = 1; i <= numberof(searchstr); i++)
      idx |= strglob(searchstr(i), files);
    if (anyof(idx))
      results = path+files(where(idx));
  }
  if (numberof(subdirs))
    for(i = 1; i <= numberof(subdirs); i++)
      grow, results, find(path+subdirs(i), searchstr=searchstr);
  return results;
}

func remove_empty(path, dirs=, files=)
/* DOCUMENT remove_empty, path, type=
  Given a path, this will recursively search its subdirectories and will
  remove all directories and/or files that are empty (or that become empty
  because they had only empty directories and/or files).

  The default is equivalent to this linux command:
    cd path
    find -depth -type d -empty -exec rmdir \{} \;

  Options:
    dirs= Delete empty dirs?
        dirs=1    Yes, delete them (default)
        dirs=0    No, don't delete them
    files= Delete empty files?
        files=0   No, don't delete them (default)
        files=1   Yes, delete them
*/
{
  fix_dir, path;
  default, dirs, 1;
  default, files, 0;
  local subdirs;
  fns = lsdir(path, subdirs);
  // This signifies the path doesn't exist.
  if (structof(fns) == long) return;
  for(i = 1; i <= numberof(subdirs); i++)
    remove_empty, path+subdirs(i), dirs=dirs, files=files;
  if (files)
    for(i = 1; i <= numberof(fns); i++)
      if (!file_size(path+fns(i)))
        remove, path+fns(i);
  if (dirs && !numberof(lsdir(path, subdirs)) && !numberof(subdirs))
    rmdir, path;
}

func remove_recursive(path)
/* DOCUMENT remove_recursive, path
  Removes a path and everything under it, equivalent to rm -rf.
*/
{
  fix_dir, path;
  local subdirs;
  files = lsdir(path, subdirs);
  // This signifies the path doesn't exist.
  if (structof(files) == long) return;
  for(i = 1; i <= numberof(subdirs); i++)
    remove_recursive, path+subdirs(i);
  for(i = 1; i <= numberof(files); i++)
    remove, path+files(i);
  rmdir, path;
}

func lsfiles(dir, glob=, ext=, case=, regex=)
/* DOCUMENT lsfiles(directory_name, glob=, ext=, case=, regex=)

  List DIRECTORY_NAME. The return value FILES is an array of strings or [];
  the order of the filenames is unspecified; it does not contain "." or "..";
  it does not contain the names of subdirectories.

  Options:

    ext= The filename extension, such as ext=".c" or ext=".conf". Do not use
      wildcards with this, they won't work.  This is ignored if glob= is
      provided.  (This option is deprecated and is kept for backwards
      compatibility.)

    glob= A glob pattern to use, such as glob="*.c" or glob="foo*.conf".
      Wildcards will work with this (they are passed to strglob).

    case= When used with glob, indicates whether the glob should be case
      sensitive.  Default is 0, case insensitive.

    regex= A regular expression to use to search against. Specifying
      this will cause glob and ext to be ignored.

  SEE ALSO: cd, mkdir, rmdir, get_cwd, get_home, lsdir
*/
{
  fix_dir, dir;
  default, case, 0;
  files = lsdir(dir);
  if (numberof(files) && typeof(files) == "string") {
    if (!is_void(regex)) {
      w = where(regmatch(regex, files, icase=!case));
      if (numberof(w))
        return files(w);
    } else {
      if (is_void(glob) && !is_void(ext)) glob = "*"+ext;
      if (is_void(glob))
        return files;
      if (!is_void(glob)) {
        w = where(strglob(glob, files, case=case));
        if (numberof(w))
          return files(w);
      }
    }
  }
  return [];
}

func lsdirs(dir, glob=)
/* DOCUMENT lsdirs(dir, glob=)

  List the subdirectories of dir. The return value is an array of strings or
  []. The order of the directories is unspecified.

  Options:

    glob= A glob pattern to use, suitable for strglob.

  SEE ALSO: lsfiles, lsdir
*/
{
  lsdir, dir, subdirs;
  if (!is_void(glob)) {
    w = where(strglob(glob, subdirs));
    if (numberof(w))
      subdirs = subdirs(w);
    else
      subdirs = [];
  }
  return subdirs;
}

func fix_dir(&idir)
/* DOCUMENT fix_dir, dir
        new_dir = fix_dir(old_dir)

  Given a directory, this will ensure that it ends with a trailing slash.
  The first form will update the variable dir in-place. The second form
  will return the validated directory, but will not clobber the original.
*/
{
  if (is_void(idir)) return;
  dir = idir;
  if (numberof(dir) == 1) {
    if (strlen(dir) && "/" != strpart(dir, strlen(dir):strlen(dir)))
      dir = dir + "/";
  } else {
    w = where(strlen(dir) > 0 & !regmatch("/$", dir));
    if (numberof(w))
      dir(w) = dir(w) + "/";
  }
  if (am_subroutine())
    idir = dir;
  return dir;
}

func mktempdir(name)
/* DOCUMENT mktempdir(name)

  Creates a temporary directory. The directory will be:

    /tmp/(name).(datetime).(pid).(rand)

  Where
    (name) is either name (the parameter) or "yorick"
    (datetime) is the current date+time
    (pid) is a pid as retrieved from a new process (NOT your current
      process's ID)
    (rand) is a random number

  This is not guaranteed to create a unique or unpredictable temporary
  directory, but should be okay for normal purposes.

  The directory will be created, and its name will be returned. The
  directory must be manually removed by the user later.

  This uses alpsrc.temp_dir, so you can change where the temporary directories
  are created.
*/
{
  default, name, "yorick";
  ts1 = (parsedate(timestamp())*[1,10^2,10^4,0,0,0])(sum);
  ts2 = (parsedate(timestamp())*[0,0,0,10^4,10^2,1])(sum);
  pid = 0;
  read, popen("echo $$", 0), pid;
  rd = int(random()*1000);
  dir = file_join(alpsrc.temp_dir,
    swrite(format="%s.%06d%06d.%d.%d", name, ts1, ts2, pid, rd));
  mkdir, dir;
  return dir;
}

func file_isdir(filename)
/* DOCUMENT file_isdir(filename)

  Checks if the file 'filename' is a directory.

  Return 1 if it is, or 0 if it is not.
*/
{
  if (dimsof(filename)(1)) {
    result = array(0, dimsof(filename));
    for(i = 1; i <= numberof(filename); i++) {
      result(i) = 0 != lsdir(filename(i));
    }
    return result;
  } else {
    return 0 != lsdir(filename);
  }
}

func file_isfile(filename)
/* DOCUMENT file_isfile(filename)

  Checks if the file 'filename' is a file.

  Return 1 if it is, or 0 if it is not.
*/
{
  f_exists = file_exists(filename);
  f_isdir = file_isdir(filename);
  return f_exists & ! f_isdir;
}

func file_copy(src, dest, force=)
/* DOCUMENT file_copy, src, dest, force=

  Will copy file src to dest. dest must be a full path and filename, and the
  directory to contain the destination must already exist. If the file already
  exists as dest, it will be overwritten.

  If ytk is running, this will use Tcl to copy the file. Otherwise, it will
  use native Yorick commands, which are noticeably slower. Both methods are
  drastically faster than using 'system, "cp src dest"'.
*/
{
  extern _ytk;
  default, force, 1;
  if (_ytk) {
    forcestr = force ? "-force" : "";
    cmd = swrite(format="file copy %s -- {%s} {%s}", forcestr, src, dest);
    tkcmd, cmd, async=0;
  } else {
    if (!force && file_exists(dest))
      return;
    fs = open(src, "rb");
    c = array(char, sizeof(fs));
    _read, fs, 0, c;
    close, fs;
    fd = open(dest, "wb");
    _write, fd, 0, c;
    close, fd;
    remove, dest + "L";
  }
}

func dir_empty(dir)
/* DOCUMENT dir_empty(dir)
  Tests to see if a directory is empty. Returns 1 if yes, 0 if no.
*/
{
  files = lsfiles(dir, glob="*");
  subds = lsdirs(dir);
  found = numberof(files) + numberof(subds);
  found = found ? 0 : 1;
  return found;
}

func file_sanitize(&fn)
/* DOCUMENT sanitized = file_sanitize(fn)
  -or- file_sanitize, fn

  Sanitizes a filename so that it does not contain inappropriate characters.
  The sanitiziation process replaces all series of invalid characters with an
  underscore; invalid leading and trailing characters are simply removed. Only
  the following characters are permitted:
    alphabet: A-Za-z
    numbers: 0-9
    punctuation: -_/.
  Note that spaces are not allowed.

  As an edge case, if the filename begins or ends with a series of invalid
  characters adjacent to an underscore, that underscore will also be removed.
  However, if a filename begins or ends directly with an underscore, it will be
  left in place.
*/
{
  which = 0;
  if (strpart(fn, 1:1) != "_") which |= 1;
  if (strpart(fn, 0:0) != "_") which |= 2;
  ofn = regsub("[^-A-Za-z0-9_/.]+", fn, "_", all=1);
  if (which) ofn = strtrim(ofn, which, blank="_");
  if (am_subroutine())
    fn = ofn;
  return ofn;
}

func file_mtime(fn)
/* DOCUMENT file_mtime(fn)
  Returns the file modification unix timestamp of a file as an integer.
*/
{
  cmd = swrite(format="stat -c '%Y' '%s'", fn);
  return atoi(popen_rdfile(cmd)(1));
}
