/*
 *  inspired/adapted from ...
 *	02/20/96: release 1.1
 *	02/20/96 by Eric THIEBAUT: improve performances of asciiRead()
 *	    execution time almost divided by 2, "string.i" no more needed.
 *      02/21/96 D Munro improved performance by a factor of 250 or so.
 */

func asciiRead (file)
/* DOCUMENT data= asciiRead(name)
   data= asciiRead(file)
   read ascii numeric data in columns from file NAME, or the already
   open file FILE.
   The result is a NCOLUMNS-by-NLINES array of doubles.

   Data are read as double values arranged in columns separated
   by any number of spaces or tabs.  Comments starting with a "#"
   or any other character which is not part of a number are ignored
   up to the end-of-line.  Blank lines are ignored.
   The first non-blank/commented line gives the number of values per
   column, for subsequent lines.  Subsequent lines must have the
   same number of columns -- blanks in columns are not permitted,
   use 0.0 instead.  However, minimal error checking is performed,
   and if the data is not really in columns, asciiRead can silently
   fail to interpret your file as you would scanning it by eye.

   The read operation will be much faster if the number of commented
   lines is relatively small.  Blank lines cost nothing, while a line
   containing just a "#" is expensive.

   SEE ALSO: read
*/
{
  if (structof(file)==string)
    file= open(file);

  ncmx= 128;  // max# columns
  x= array(0.0, ncmx);

  ncols= 0;
  while ((line= rdline(file))) {
    if (!strgrepm("^#",line))
      ncols= sread(line, x);
    if (ncols)
      break;
  }
  if (!ncols)
    return [];

  nvmx= 10000;      // #values read in a chunk
  nrows= 1;
  list= save(string(0),[x(1:ncols)]);
  x= array(0.0, ncols, nvmx/ncols + 1);
  for(;;) { // read nvmx numbers, blank lines skipped, comment interrupts read
    n= read(file, x);
    if (!n) {
      while ((line= rdline(file))) {
        if (!strgrepm("^#",line))
          n= sread(line, x);
        if (n)
          break;
      }
      if (!line)  // rdline end-of-file, n==0 too
        break;
    }
    if (n%ncols)
      error, "data is not in columns";
    n/= ncols;

    // not sure to add @ head/tail
    list= save(,x(,1:n),[],list);
    nrows+= n;
  }

  /* pop chunks off list and reassemble result */
  x= array(0.0, ncols, nrows);
  m= list(*);
  for (i=m, j=1; i>0; i--) {
    n= numberof(list(noop(i)))/ncols;
    x(,j:j+n-1)= list(noop(i));
    j+= n;
  }

  return x;
}
