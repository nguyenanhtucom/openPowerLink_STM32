pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package oplk.x86_64_linux_gnu_bits_waitstatus_h is

   --  unsupported macro: w_termsig __wait_terminated.__w_termsig
   --  unsupported macro: w_coredump __wait_terminated.__w_coredump
   --  unsupported macro: w_retcode __wait_terminated.__w_retcode
   --  unsupported macro: w_stopsig __wait_stopped.__w_stopsig
   --  unsupported macro: w_stopval __wait_stopped.__w_stopval
  -- Definitions of status bits for `wait' et al.
  --   Copyright (C) 1992,1994,1996,1997,2000,2004 Free Software Foundation, Inc.
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

  -- Everything extant so far uses these same bits.   
  -- If WIFEXITED(STATUS), the low-order 8 bits of the status.   
  -- If WIFSIGNALED(STATUS), the terminating signal.   
  -- If WIFSTOPPED(STATUS), the signal that stopped the child.   
  -- Nonzero if STATUS indicates normal termination.   
  -- Nonzero if STATUS indicates termination by a signal.   
  -- Nonzero if STATUS indicates the child is stopped.   
  -- Nonzero if STATUS indicates the child continued after a stop.  We only
  --   define this if <bits/waitflags.h> provides the WCONTINUED flag bit.   

  -- Nonzero if STATUS indicates the child dumped core.   
  -- Macros for constructing status values.   
   type wait;
   type anon_0 is record
      uu_w_termsig : Extensions.Unsigned_7;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:73
      uu_w_coredump : Extensions.Unsigned_1;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:74
      uu_w_retcode : aliased unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:75
      field_4 : aliased unsigned_short;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_0);
   type anon_1 is record
      uu_w_stopval : aliased unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:88
      uu_w_stopsig : aliased unsigned_char;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:89
      field_3 : aliased unsigned_short;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_1);
   type wait (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            w_status : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:69
         when 1 =>
            uu_wait_terminated : aliased anon_0;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:84
         when others =>
            uu_wait_stopped : aliased anon_1;  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:97
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, wait);
   pragma Unchecked_Union (wait);  -- /usr/include/x86_64-linux-gnu/bits/waitstatus.h:67

  -- Terminating signal.   
  -- Set if dumped core.   
  -- Return code if exited normally.   
  -- W_STOPPED if stopped.   
  -- Stopping signal.   
  -- Stopping signal.   
  -- W_STOPPED if stopped.   
end oplk.x86_64_linux_gnu_bits_waitstatus_h;
