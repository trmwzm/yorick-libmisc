func intmjd (iyr,imo,idy) 
/* DOCUMENT 
   set the integer part of a modified julian date as epoch, mjd
   the modified julian day is derived from civil time as in civmjd()
   allows single number expression of time in seconds w.r.t. mjd0
   SEE ALSO:
 */ 
{
  if (anyof(iyr < 1900))
    error,"iyr < 1900";

  w= imo<=2;
  y= merge2(iyr-1,iyr,w);
  m= merge2(imo+12,imo,w);

  it1= 365.25*y;
  it2= 30.6001*(m+1);
  mjd= it1+it2+idy-679019;

  return mjd;
}
func civjts (iyr,imo,idy,ihr,imn,sec)
/* DOCUMENT civjts (iyr,imo,idy,ihr,imn,sec)
   convert civil date to time in seconds past mjd epoch, mjd0
 
   imo in range 1-12, idy in range 1-31
   only valid in range mar-1900 thru feb-2100     (leap year protocols)
   ref: hofmann-wellenhof, 2nd ed., pg 34-35
   adapted from civmjd()  
   SEE ALSO:
 */
{
  mjd0= intmjd(iyr,imo,idy);
  
  if (anyof(iyr < 1900))
    error,"iyr < 1900";

  w= imo<=2;
  y= merge2(iyr-1,iyr,w);
  m= merge2(imo+12,imo,w);

  it1= 365.25*y;
  it2= 30.6001*(m+1);
  mjd= it1+it2+idy-679019;

  return (mjd-mjd0)*86400.0+3600*ihr+60*imn+sec;
}
func jtsciv (tsec,iyr,imo,idy,ihr,imn)
/* DOCUMENT jtsciv (tsec,iyr,imo,idy,ihr,imn,sec)
   convert time in seconds past mjd0 epoch into civil date
   requires initialization of mjd0 by intmjd()

   imo in range 1-12, idy in range 1-31
   only valid in range mar-1900 thru feb-2100
   ref: hofmann-wellenhof, 2nd ed., pg 34-35
   adapted from mjdciv()
     
   SEE ALSO:
*/
{
  mjd0= intmjd(iyr,imo,idy);
  mjd= mjd0+tsec/86400.0;
  // the following equation preserves significant digits
  fmjd= (tsec % 86400.0)/86400.0;

  rjd= mjd+fmjd+2400000.5;
  ia= (rjd+0.5);
  ib= ia+1537;
  ic= (ib-122.1)/365.25;
  id= 365.25*ic;
  ie= (ib-id)/30.6001;

  // the fractional part of a julian day is (fractional mjd + 0.5)
  // therefore, fractional part of julian day + 0.5 is (fractional mjd)

  it1= ie*30.6001;
  idy= ib-id-it1+fmjd;
  it2= ie/14.0;
  imo= ie-1-12*it2;
  it3= (7+imo)/10.0;
  iyr= ic-4715-it3;

  tmp= fmjd*24.0;
  ihr= tmp;
  tmp= (tmp-ihr)*60.0;
  imn= tmp;
  sec= (tmp-imn)*60.0;
  return sec;
}
func civmjd (iyr,imo,idy,ihr,imn,sec,&mjd)
/* DOCUMENT fmjd= civmjd (iyr,imo,idy,ihr,imn,sec,&mjd)
   convert civil date to modified julian date

   imo in range 1-12, idy in range 1-31
   only valid in range mar-1900 thru feb-2100     (leap year protocols)
   ref: hofmann-wellenhof, 2nd ed., pg 34-35
   operation confirmed against table 3.3 values on pg.34
     
   SEE ALSO:
*/
{
  mjd= intmjd(iyr,imo,idy);

  return (3600*ihr+60*imn+sec)/86400.0;
}
func mjdciv (mjd,fmjd,iyr,imo,idy,ihr,imn)
/* DOCUMENT  mjdciv (mjd,fmjd,iyr,imo,idy,ihr,imn)
   imo in range 1-12, idy in range 1-31
   only valid in range mar-1900 thru feb-2100
   ref: hofmann-wellenhof, 2nd ed., pg 34-35
   operation confirmed for leap years (incl. year 2000)
   SEE ALSO:
 */
{
  rjd= mjd+fmjd+2400000.5;
  ia= (rjd+0.5);
  ib= ia+1537;
  ic= (ib-122.1)/365.25;
  id= 365.25*ic;
  ie= (ib-id)/30.6001;

  // the fractional part of a julian day is fractional mjd + 0.5
  // therefore, fractional part of julian day + 0.5 is fractional mjd

  it1= ie*30.6001;
  idy= ib-id-it1+fmjd;
  it2= ie/14.0;
  imo= ie-1-12*it2;
  it3= (7+imo)/10.0;
  iyr= ic-4715-it3;

  tmp= fmjd*24.0;
  ihr= tmp;
  tmp= (tmp-ihr)*60.0;
  imn= tmp;
  sec= (tmp-imn)*60.0;

  return sec;
}
func gps2tt (tsec)
/* DOCUMENT 
   convert tsec in GPS to tsec in TT  
   SEE ALSO:
 */
{
  return tsec+51.184;        // fixed offset
}
func gps2utc (mjd0,tsec)
/* DOCUMENT gps2utc (mjd0,tsec)
   convert tsec in GPS to tsec in UTC
   GPS is ahead of UTC  (c.f. USNO)
   UTC is behind GPS
   gpsleap() is (so far) positive (and increasing)
   so, must subtract gpsleap from GPS to get UTC     
   SEE ALSO:
*/
{
  return tsec-gpsleap(mjd0,tsec);
}
func gpsleap (mjd0, tsec)
/* DOCUMENT gpsleap (mjd0, tsec)
   return total leap seconds since GPS epoch 1980jan06
   note: does **NOT** return the full TAI-UTC delta
   input time is GPS seconds -- initialized by intmjd()
   Y2K -- only functional between 1980jan06-00:00:00  (GPS time start)
   and hard-coded date
   "Julian Date Converter"
   http://aa.usno.navy.mil/data/docs/JulianDate.php
   "Bulletin C"
   http://hpiers.obspm.fr/eoppc/bul/bulc/bulletinc.dat
   parameter(mjdhard= 55196)            !// cut-off date 2009dec31
   parameter(mjdhard= 55377)            !// cut-off date 2010jun30
   parameter(mjdhard= 55561)            !// cut-off date 2010dec31
   parameter(mjdhard= 55742)            !// cut-off date 2011jun30
   parameter(mjdhard= 55926)            !// cut-off date 2011dec31
   parameter(mjdhard= 56108)            !// cut-off date 2012jun30
   parameter(mjdhard= 56292)            !// cut-off date 2012dec31
   parameter(mjdhard= 56473)            !// cut-off date 2013jun30
   parameter(mjdhard= 56657)            !// cut-off date 2013dec31
   parameter(mjdhard= 56838)            !// cut-off date 2014jun30
   http://maia.usno.navy.mil/ser7/tai-utc.dat
   1980 JAN  1 = JD 2444239.5  TAI-UTC=   19.0s
   1981 JUL  1 = JD 2444786.5  TAI-UTC=   20.0s
   1982 JUL  1 = JD 2445151.5  TAI-UTC=   21.0s
   1983 JUL  1 = JD 2445516.5  TAI-UTC=   22.0s
   1985 JUL  1 = JD 2446247.5  TAI-UTC=   23.0s
   1988 JAN  1 = JD 2447161.5  TAI-UTC=   24.0s
   1990 JAN  1 = JD 2447892.5  TAI-UTC=   25.0s
   1991 JAN  1 = JD 2448257.5  TAI-UTC=   26.0s
   1992 JUL  1 = JD 2448804.5  TAI-UTC=   27.0s
   1993 JUL  1 = JD 2449169.5  TAI-UTC=   28.0s
   1994 JUL  1 = JD 2449534.5  TAI-UTC=   29.0s
   1996 JAN  1 = JD 2450083.5  TAI-UTC=   30.0s
   1997 JUL  1 = JD 2450630.5  TAI-UTC=   31.0s
   1999 JAN  1 = JD 2451179.5  TAI-UTC=   32.0s
   2006 JAN  1 = JD 2453736.5  TAI-UTC=   33.0s
   2009 JAN  1 = JD 2454832.5  TAI-UTC=   34.0s
   2012 JUL  1 = JD 2456109.5  TAI-UTC=   35.0s

   SEE ALSO:
*/
{
  ttsec= tsec;
  mjd0t= mjd0;

  while (anyof((m= ttsec>=86400.0))) {
    ttsec= merge2(ttsec-86400.0,ttsec,m);
    mjd0t= merge2(mjd0t+1,mjd0t,m);
  }
  while (anyof((m= ttsec < 0.0))) {
    ttsec= merge2(ttsec+86400.0,ttsec,m);
    mjd0t= merge2(mjd0t-1,mjd0t,m);
  }

  // test date limits
  if (anyof(mjd0t > mjdhard)) 
    error,"exceeded cut-off date in gpsleap()";

  if (anyof(mjd0t < 44244)) // 1980jan06
    error,"cut-off date underflow in gpsleap()";

  // test against newest leaps first
  co= [44239,44786,45151,45516,46247,47161,47892,48257,48804, \
       49169,49534,50083,50630,51179,53736,54832,56109];
  
  tm= indgen(18:35)*1.0;
  // convert TAI-UTC into leap seconds
  return tm(digitize(mjd0t,co))-19.0;
}
