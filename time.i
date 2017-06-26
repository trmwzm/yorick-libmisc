JDREF = 2451545.0;  
// 2000 January 1, 12h UT, Julian date 2451545.0: 
// day counting according to Julius Caesar: 
// 1 year, on average, is 365.25 days, (.25==6hrs for 1 leap year/4years)

// see /u/ptolemy0/ec/Util/timedmp.f

func julday(y,m,d,ut)
/* DOCUMENT d = julday(y,m,d,ut)
 julday  function  convert Georgian date to Julian Day. 
 The Julian date (JD) is a continuous count of days from 
 1 January 4713 BC (= -4712 January 1), Greenwich mean noon (= 12h UT).

  d = 367*Y - (7*(Y + ((M+9)/12)))/4 + (275*M)/9 + D + 1721013.5 + ut/24.0;

 NOTE: that ut==12.0 must be entered to get the integer JULIAN DAY
 example JDAY = long(julday(2005,2,24,12) +0.5)
 ANOTHER function j2date is provided for that (see below)
 Y is the year (all 4 digits!), M the month (1-12) and D the date. 
 D is an integer, which is valid at the start (at midnight),
 in UT or TDT, of that calendar date. If you want d for some other time, add
 F = UT/24.0 (here the division is a floating-point division!) to d to obtain
 a double
 */
{
  y = long(y);  m = long(m);  d = long(d);
  if (is_void(ut)) ut = 0.0;
  return 367*y - 7*(y+(m+9)/12)/4 + 275*m/9 + d + 1721013.5 + ut/24.0;
}

func strdate(year, month, day, sep=)
/* DOCUMENT getdate -- get date of the day in the form "YYYYMMDD"
      OR get date of the day in the form "YYYY'sep'MM'sep'DD" when sep=="_"
   SYNOPSIS: strdate(year, month, day, sep=)
 */
{ 
  year = long(year); month = long(month); day = long(day);
  if(is_void(sep)){
    return (date= swrite(format="%04d%02d%02d", year, month, day));
  }else if(structof(sep)==string){
    return (date= swrite(format="%04d"+sep+"%02d"+sep+"%02d", year, month, day));
  }else{
    error,"sep= must be string";
  } 
}

func calsec(year, month, day, hour, minute, sec, frac)
/* DOCUMENT
     Purpose:  This double precision function (CALendar date to secs) 
     takes the components of a calendar date and time
            Year / Month / Day , Hour : Minute : sec.Frac
     and returns the corresponding secs past the reference date (jdref)
     this library. 

     Input:--------
     year                    is the year.
     month                   is the month number.
     day                     is the day.
     hour                    is the hour.
     minute                  is the minute.
     sec                  is the sec.
     frac                    is the fractional secs. 
     Output:-------
     real (return value)     is the secs past J2000 (including fractions) 
 */
{
   if(is_void(frac))frac=0.0;
   if(is_void(sec))sec=0;
   if(is_void(minute))minute=0;
   if(is_void(hour))hour=0.0;
   if(is_void(day))day=1;
   if(is_void(month))month=1;
     
   /* Call DATE2J to compute the double precision Julian date at the start the current day. */
   jd = date2j( year, month, day ) - 0.5;

   return ((((jd - JDREF)*24.0 + hour)*60.0 + minute)*60.0 + sec + frac);

}

func date2j(year,  month, day, matlab=)
/* DOCUMENT date2j(year,  month, day)
   Purpose:
   This integer function (calendar DATE 2 Julian date) takes the input
   Gregorian calendar date and returns as its functional value the
   corresponding integer Julian date. Since the Julian date is an integer
   this correspondence is exact for noon of the output calendar date.

   The algorithm for this conversion is taken from the following article
   Tantzen,R.T., "Communications of the ACM", Volume 6, Number 8, August
   Algorithm 199, page 444.
 
   Input_Arguments:

   year     is the year number.
   month    is the month number.
   day      is the day number. 

   IF matlab==1 then emulated matlab's datenum(y,m,d) which is ???
   SEE ALSO: j2date
*/
{
   year = long(year); month = long(month); day = long(day);

   j = array(1, dimsof(year, month, day));
   
   y = year*j;
   m = month*j;
   d = day*j;

   wm = m>2;
   m = merge2(m - 3, m + 9, wm);
   y = merge2(y, y - 1, wm);
    
   c  = y/100;
   ya = y - 100*c;
   
   if(matlab==1){
     return ((146097*c)/4 + (1461*ya)/4 + (153*m+2)/5 + d + 60);
   }else{
     return ((146097*c)/4 + (1461*ya)/4 + (153*m+2)/5 + d + 1721119);
   }
}

func seccal(sec, &year, &month, &day, &hour, &minute, &secnds, &frac )
/* DOCUMENT:  seccal(sec, &year, &month, &day, &hour, &minute, &sec, &frac )
   This function takes secs(double) past the reference data (jdref) 
   and returns the components of a calendar date and time
     Year / Month / Day , Hour : Minute : sec.Frac
*/
{
   frac = sec - floor(sec);

   //call sec2jd to convert the integral secs to the Julian date.
   jd = sec2jd( sec - frac + 0.5);

   //call jd2cal to convert the Julian date to calandar date. */
   year = month = day = hour = minute = secnds = 0;
   temp = 0.0;
   jd2cal, jd, year, month, day, hour, minute, secnds, temp;

   return;
}

func sec2jd(sec)
{
   return sec/86400.0 + JDREF;
}

func jd2cal(jd, &year, &month, &day, &hour, &minute, &sec, &frac)
/* DOCUMENT 
   Compute jdint, the integer Julian date at noon of the current day  
   compute dsec, the number of secs elapsed since the start of the 
   current day.               
 */                                        
{
   secperday = 24.0*60*60;

   jdplus = jd + 0.5;
   jdint = long(jdplus);
   dsec = secperday * ( jdplus - jdint );
   wd = dsec > secperday;
   jdint = merge2(jdint + 1, jdint, wd);
   dsec = merge2(dsec - secperday, dsec, wd);
   isec = long(dsec);
 
   //call j2date with jdint to compute the year, month, and day of the calendar date.
   year = month = day = 0;
   j2date, jdint, year, month, day;

   //compute hour */
   hour = isec/3600;
   isec = long(isec - 3600 * hour);
 
   //compute minute */

   minute = isec/60;
   isec = long(isec - 60 * minute);
 
   //compute sec */
   sec = isec;
 
   //compute frac */
   frac = dsec - long(dsec);

}

func j2date(jd, &year, &month, &day ,matlab=)
/* DOCUMENT
   SEE date2j
 */
{
   jd = long(jd);
 
   j = jd;
   if(matlab==1){
     j = j - 60;
   }else{
     j = j - 1721119;
   }
   year = (4*j-1)/146097;
   j = 4*j - 1 - 146097*year;
   day = j/4;
   j = (4*day+3)/1461;
   day = 4*day + 3 -1461*j;
   day = (day+4)/4;
   month = (5*day-3)/153;
   day = 5*day - 3 - 153*month;
   day = (day+5)/5;
   year = 100*year + j;
   
   wm = month<10;
   month = merge2(month + 3, month - 9, wm);
   year = merge2(year, year + 1, wm);
   return;
}

func secdoy (sec)
{
  local year, month, day;
  seccal, sec, year, month,day;
  return long((calsec(year, month, day)-calsec(year))/86400.0+0.5)+1;
}
