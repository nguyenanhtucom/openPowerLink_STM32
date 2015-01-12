pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.x86_64_linux_gnu_bits_types_h;

package oplk.time_h is

  -- Copyright (C) 1991-2003,2006,2009 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, write to the Free
  --   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
  --   02111-1307 USA.   

  -- *	ISO C99 Standard: 7.23 Date and time	<time.h>
  --  

  -- Get size_t and NULL from <stddef.h>.   
  -- This defines CLOCKS_PER_SEC, which is the number of processor clock
  --   ticks per second.   

  -- This is the obsolete POSIX.1-1988 name for the same constant.   
  -- Returned by `clock'.   
   subtype clock_t is oplk.x86_64_linux_gnu_bits_types_h.uu_clock_t;  -- /usr/include/time.h:60

  -- Returned by `time'.   
   subtype time_t is oplk.x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:76

  -- Clock ID used in clock and timer functions.   
   subtype clockid_t is oplk.x86_64_linux_gnu_bits_types_h.uu_clockid_t;  -- /usr/include/time.h:92

  -- Timer ID returned by `timer_create'.   
   subtype timer_t is oplk.x86_64_linux_gnu_bits_types_h.uu_timer_t;  -- /usr/include/time.h:104

  -- This defines __time_t for us.   
  -- POSIX.1b structure for a time value.  This is like a `struct timeval' but
  --   has nanoseconds instead of microseconds.   

  -- Seconds.   
   type timespec is record
      tv_sec : aliased oplk.x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/time.h:122
      tv_nsec : aliased long;  -- /usr/include/time.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, timespec);  -- /usr/include/time.h:120

  -- Nanoseconds.   
  -- Used by other time functions.   
  -- Seconds.	[0-60] (1 leap second)  
  -- Minutes.	[0-59]  
  -- Hours.	[0-23]  
  -- Day.		[1-31]  
  -- Month.	[0-11]  
  -- Year	- 1900.   
  -- Day of week.	[0-6]  
  -- Days in year.[0-365]	 
  -- DST.		[-1/0/1] 
  -- Seconds east of UTC.   
  -- Timezone abbreviation.   
  -- Seconds east of UTC.   
  -- Timezone abbreviation.   
  -- POSIX.1b structure for timer start values and intervals.   
  -- We can use a simple forward declaration.   
  -- Time used by the program so far (user time + system time).
  --   The result / CLOCKS_PER_SECOND is program time in seconds.   

  -- Return the current time and put it in *TIMER if TIMER is not NULL.   
  -- Return the difference between TIME1 and TIME0.   
  -- Return the `time_t' representation of TP and normalize TP.   
  -- Format TP into S according to FORMAT.
  --   Write no more than MAXSIZE characters and return the number
  --   of characters written, or 0 if it would exceed MAXSIZE.   

  -- Parse S according to FORMAT and store binary time information in TP.
  --   The return value is a pointer to the first unparsed character in S.   

  -- Similar to the two functions above but take the information from
  --   the provided locale and not the global locale.   

  -- Return the `struct tm' representation of *TIMER
  --   in Universal Coordinated Time (aka Greenwich Mean Time).   

  -- Return the `struct tm' representation
  --   of *TIMER in the local timezone.   

  -- Return the `struct tm' representation of *TIMER in UTC,
  --   using *TP to store the result.   

  -- Return the `struct tm' representation of *TIMER in local time,
  --   using *TP to store the result.   

  -- Return a string of the form "Day Mon dd hh:mm:ss yyyy\n"
  --   that is the representation of TP in this format.   

  -- Equivalent to `asctime (localtime (timer))'.   
  -- Reentrant versions of the above functions.   
  -- Return in BUF a string of the form "Day Mon dd hh:mm:ss yyyy\n"
  --   that is the representation of TP in this format.   

  -- Equivalent to `asctime_r (localtime_r (timer, *TMP*), buf)'.   
  -- Defined in localtime.c.   
  -- Current timezone names.   
  -- If daylight-saving time is ever in use.   
  -- Seconds west of UTC.   
  -- Same as above.   
  -- Set time conversion information from the TZ environment variable.
  --   If TZ is not defined, a locale-dependent default is used.   

  -- Set the system time to *WHEN.
  --   This call is restricted to the superuser.   

  -- Nonzero if YEAR is a leap year (every 4 years,
  --   except every 100th isn't, and every 400th is).   

  -- Miscellaneous functions many Unices inherited from the public domain
  --   localtime package.  These are included only for compatibility.   

  -- Like `mktime', but for TP represents Universal Time, not local time.   
  -- Another name for `mktime'.   
  -- Return the number of days in YEAR.   
  -- Pause execution for a number of nanoseconds.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

  -- Get resolution of clock CLOCK_ID.   
  -- Get current value of clock CLOCK_ID and store it in TP.   
  -- Set clock CLOCK_ID to value TP.   
  -- High-resolution sleep with the specified clock.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

  -- Return clock ID for CPU-time clock.   
  -- Create new per-process timer using CLOCK_ID.   
  -- Delete timer TIMERID.   
  -- Set timer TIMERID to VALUE, returning old value in OVLAUE.   
  -- Get current value of timer TIMERID and store it in VLAUE.   
  -- Get expiration overrun for timer TIMERID.   
  -- Set to one of the following values to indicate an error.
  --     1  the DATEMSK environment variable is null or undefined,
  --     2  the template file cannot be opened for reading,
  --     3  failed to get file status information,
  --     4  the template file is not a regular file,
  --     5  an error is encountered while reading the template file,
  --     6  memory allication failed (not enough memory available),
  --     7  there is no line in the template that matches the input,
  --     8  invalid input specification Example: February 31 or a time is
  --        specified that can not be represented in a time_t (representing
  --	the time in seconds since 00:00:00 UTC, January 1, 1970)  

  -- Parse the given string as a date specification and return a value
  --   representing the value.  The templates from the file identified by
  --   the environment variable DATEMSK are used.  In case of an error
  --   `getdate_err' is set.
  --   This function is a possible cancellation points and therefore not
  --   marked with __THROW.   

  -- Since `getdate' is not reentrant because of the use of `getdate_err'
  --   and the static buffer to return the result in, we provide a thread-safe
  --   variant.  The functionality is the same.  The result is returned in
  --   the buffer pointed to by RESBUFP and in case of an error the return
  --   value is != 0 with the same values as given above for `getdate_err'.
  --   This function is not part of POSIX and therefore no official
  --   cancellation point.  But due to similarity with an POSIX interface
  --   or due to the implementation it is a cancellation point and
  --   therefore not marked with __THROW.   

end oplk.time_h;
