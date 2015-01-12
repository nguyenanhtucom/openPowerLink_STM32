pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with stddef_h;
with System;

package alloca_h is

   --  arg-macro: procedure alloca (size)
   --    __builtin_alloca (size)
  -- Copyright (C) 1992, 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
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

  -- Remove any previous definitions.   
  -- Allocate a block that will be freed when the calling function exits.   
   function alloca (uu_size : stddef_h.size_t) return System.Address;  -- /usr/include/alloca.h:33
   pragma Import (C, alloca, "alloca");

end alloca_h;
