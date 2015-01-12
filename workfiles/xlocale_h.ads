pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;

package xlocale_h is

  -- Definition of locale datatype.
  --   Copyright (C) 1997,2000,2002,2009,2010 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.
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

  -- Structure for reentrant locale using functions.  This is an
  --   (almost) opaque type for the user level programs.  The file and
  --   this data structure is not standardized.  Don't rely on it.  It can
  --   go away without warning.   

  -- Note: LC_ALL is not a valid index into this array.   
  -- 13 = __LC_LAST.  
   type uu_locale_struct_uu_locales_array is array (0 .. 12) of System.Address;
   type uu_locale_struct_uu_names_array is array (0 .. 12) of Interfaces.C.Strings.chars_ptr;
   type uu_locale_struct is record
      uu_locales : aliased uu_locale_struct_uu_locales_array;  -- /usr/include/xlocale.h:31
      uu_ctype_b : access unsigned_short;  -- /usr/include/xlocale.h:34
      uu_ctype_tolower : access int;  -- /usr/include/xlocale.h:35
      uu_ctype_toupper : access int;  -- /usr/include/xlocale.h:36
      uu_names : aliased uu_locale_struct_uu_names_array;  -- /usr/include/xlocale.h:39
   end record;
   pragma Convention (C_Pass_By_Copy, uu_locale_struct);  -- /usr/include/xlocale.h:28

   --  skipped empty struct uu_locale_data

  -- To increase the speed of this solution we add some special members.   
  -- Note: LC_ALL is not a valid index into this array.   
   type uu_locale_t is access all uu_locale_struct;  -- /usr/include/xlocale.h:40

  -- POSIX 2008 makes locale_t official.   
   subtype locale_t is uu_locale_t;  -- /usr/include/xlocale.h:43

end xlocale_h;
