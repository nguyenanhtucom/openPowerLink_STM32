pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
limited with oplk.x86_64_linux_gnu_bits_semaphore_h;
with Interfaces.C.Strings;
limited with oplk.time_h;

package oplk.semaphore_h is

  -- Copyright (C) 2002, 2003 Free Software Foundation, Inc.
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

  -- Get the definition for sem_t.   
  -- Initialize semaphore object SEM to VALUE.  If PSHARED then share it
  --   with other processes.   

   function sem_init
     (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t;
      uu_pshared : int;
      uu_value : unsigned) return int;  -- /usr/include/semaphore.h:37
   pragma Import (C, sem_init, "sem_init");

  -- Free resources associated with semaphore object SEM.   
   function sem_destroy (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t) return int;  -- /usr/include/semaphore.h:40
   pragma Import (C, sem_destroy, "sem_destroy");

  -- Open a named semaphore NAME with open flags OFLAG.   
   function sem_open (uu_name : Interfaces.C.Strings.chars_ptr; uu_oflag : int  -- , ...
      ) return access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t;  -- /usr/include/semaphore.h:43
   pragma Import (C, sem_open, "sem_open");

  -- Close descriptor for named semaphore SEM.   
   function sem_close (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t) return int;  -- /usr/include/semaphore.h:46
   pragma Import (C, sem_close, "sem_close");

  -- Remove named semaphore NAME.   
   function sem_unlink (uu_name : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/semaphore.h:49
   pragma Import (C, sem_unlink, "sem_unlink");

  -- Wait for SEM being posted.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sem_wait (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t) return int;  -- /usr/include/semaphore.h:55
   pragma Import (C, sem_wait, "sem_wait");

  -- Similar to `sem_wait' but wait only until ABSTIME.
  --   This function is a cancellation point and therefore not marked with
  --   __THROW.   

   function sem_timedwait (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t; uu_abstime : access constant oplk.time_h.timespec) return int;  -- /usr/include/semaphore.h:62
   pragma Import (C, sem_timedwait, "sem_timedwait");

  -- Test whether SEM is posted.   
   function sem_trywait (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t) return int;  -- /usr/include/semaphore.h:67
   pragma Import (C, sem_trywait, "sem_trywait");

  -- Post SEM.   
   function sem_post (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t) return int;  -- /usr/include/semaphore.h:70
   pragma Import (C, sem_post, "sem_post");

  -- Get current value of SEM and store it in *SVAL.   
   function sem_getvalue (uu_sem : access oplk.x86_64_linux_gnu_bits_semaphore_h.sem_t; uu_sval : access int) return int;  -- /usr/include/semaphore.h:73
   pragma Import (C, sem_getvalue, "sem_getvalue");

end oplk.semaphore_h;
