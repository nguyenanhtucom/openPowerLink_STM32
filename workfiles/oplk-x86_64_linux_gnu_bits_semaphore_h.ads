pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package oplk.x86_64_linux_gnu_bits_semaphore_h is

   --  unsupported macro: SEM_FAILED ((sem_t *) 0)
  -- Copyright (C) 2002, 2004 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   Contributed by Ulrich Drepper <drepper@redhat.com>, 2002.
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

  -- Value returned if `sem_open' failed.   
   subtype sem_t_uu_size_array is Interfaces.C.char_array (0 .. 31);
   type sem_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased sem_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/semaphore.h:39
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/semaphore.h:40
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, sem_t);
   pragma Unchecked_Union (sem_t);  -- /usr/include/x86_64-linux-gnu/bits/semaphore.h:41

   --  skipped anonymous struct anon_24

end oplk.x86_64_linux_gnu_bits_semaphore_h;
