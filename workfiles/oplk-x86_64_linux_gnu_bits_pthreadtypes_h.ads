pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with System;

package oplk.x86_64_linux_gnu_bits_pthreadtypes_h is

  -- Copyright (C) 2002,2003,2004,2005,2006,2007 Free Software Foundation, Inc.
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

  -- Thread identifiers.  The structure of the attribute type is not
  --   exposed on purpose.   

   subtype pthread_t is unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:50

   subtype pthread_attr_t_uu_size_array is Interfaces.C.char_array (0 .. 55);
   type pthread_attr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_attr_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:55
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:56
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_attr_t);
   pragma Unchecked_Union (pthread_attr_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:57

   --  skipped anonymous struct anon_8

   type uu_pthread_internal_list is record
      uu_prev : access uu_pthread_internal_list;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:63
      uu_next : access uu_pthread_internal_list;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:64
   end record;
   pragma Convention (C_Pass_By_Copy, uu_pthread_internal_list);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:61

   subtype uu_pthread_list_t is uu_pthread_internal_list;

  -- Data structures for mutex handling.  The structure of the attribute
  --   type is not exposed on purpose.   

  -- KIND must stay at this position in the structure to maintain
  --       binary compatibility.   

   subtype pthread_mutex_t_uu_size_array is Interfaces.C.char_array (0 .. 39);
   type pthread_mutex_t;
   type uu_pthread_mutex_s is record
      uu_lock : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:80
      uu_count : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:81
      uu_owner : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:82
      uu_nusers : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:84
      uu_kind : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:88
      uu_spins : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:90
      uu_list : aliased uu_pthread_list_t;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:91
   end record;
   pragma Convention (C_Pass_By_Copy, uu_pthread_mutex_s);
   type pthread_mutex_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_data : aliased uu_pthread_mutex_s;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:101
         when 1 =>
            uu_size : aliased pthread_mutex_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:102
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:103
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_mutex_t);
   pragma Unchecked_Union (pthread_mutex_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:104

   --  skipped anonymous struct anon_9

   subtype pthread_mutexattr_t_uu_size_array is Interfaces.C.char_array (0 .. 3);
   type pthread_mutexattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_mutexattr_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:108
         when others =>
            uu_align : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:109
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_mutexattr_t);
   pragma Unchecked_Union (pthread_mutexattr_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:110

   --  skipped anonymous struct anon_10

  -- Data structure for conditional variable handling.  The structure of
  --   the attribute type is not exposed on purpose.   

   subtype pthread_cond_t_uu_size_array is Interfaces.C.char_array (0 .. 47);
   type pthread_cond_t;
   type anon_12 is record
      uu_lock : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:119
      uu_futex : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:120
      uu_total_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:121
      uu_wakeup_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:122
      uu_woken_seq : aliased Extensions.unsigned_long_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:123
      uu_mutex : System.Address;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:124
      uu_nwaiters : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:125
      uu_broadcast_seq : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:126
   end record;
   pragma Convention (C_Pass_By_Copy, anon_12);
   type pthread_cond_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_data : aliased anon_12;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:127
         when 1 =>
            uu_size : aliased pthread_cond_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:128
         when others =>
            uu_align : aliased Long_Long_Integer;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:129
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_cond_t);
   pragma Unchecked_Union (pthread_cond_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:130

   --  skipped anonymous struct anon_11

   subtype pthread_condattr_t_uu_size_array is Interfaces.C.char_array (0 .. 3);
   type pthread_condattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_condattr_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:134
         when others =>
            uu_align : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:135
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_condattr_t);
   pragma Unchecked_Union (pthread_condattr_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:136

   --  skipped anonymous struct anon_13

  -- Keys for thread-specific data  
   subtype pthread_key_t is unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:140

  -- Once-only execution  
   subtype pthread_once_t is int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:144

  -- Data structure for read-write lock variable handling.  The
  --   structure of the attribute type is not exposed on purpose.   

  -- FLAGS must stay at this position in the structure to maintain
  --       binary compatibility.   

   subtype pthread_rwlock_t_uu_size_array is Interfaces.C.char_array (0 .. 55);
   type pthread_rwlock_t;
   type anon_15 is record
      uu_lock : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:155
      uu_nr_readers : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:156
      uu_readers_wakeup : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:157
      uu_writer_wakeup : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:158
      uu_nr_readers_queued : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:159
      uu_nr_writers_queued : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:160
      uu_writer : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:161
      uu_shared : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:162
      uu_pad1 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:163
      uu_pad2 : aliased unsigned_long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:164
      uu_flags : aliased unsigned;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:167
   end record;
   pragma Convention (C_Pass_By_Copy, anon_15);
   type pthread_rwlock_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_data : aliased anon_15;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:168
         when 1 =>
            uu_size : aliased pthread_rwlock_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:187
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:188
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_rwlock_t);
   pragma Unchecked_Union (pthread_rwlock_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:189

   --  skipped anonymous struct anon_14

  -- FLAGS must stay at this position in the structure to maintain
  --       binary compatibility.   

   subtype pthread_rwlockattr_t_uu_size_array is Interfaces.C.char_array (0 .. 7);
   type pthread_rwlockattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_rwlockattr_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:193
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:194
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_rwlockattr_t);
   pragma Unchecked_Union (pthread_rwlockattr_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:195

   --  skipped anonymous struct anon_16

  -- POSIX spinlock data type.   
   subtype pthread_spinlock_t is int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:201

  -- POSIX barriers data type.  The structure of the type is
  --   deliberately not exposed.   

   subtype pthread_barrier_t_uu_size_array is Interfaces.C.char_array (0 .. 31);
   type pthread_barrier_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_barrier_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:208
         when others =>
            uu_align : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:209
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_barrier_t);
   pragma Unchecked_Union (pthread_barrier_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:210

   --  skipped anonymous struct anon_17

   subtype pthread_barrierattr_t_uu_size_array is Interfaces.C.char_array (0 .. 3);
   type pthread_barrierattr_t (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            uu_size : aliased pthread_barrierattr_t_uu_size_array;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:214
         when others =>
            uu_align : aliased int;  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:215
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, pthread_barrierattr_t);
   pragma Unchecked_Union (pthread_barrierattr_t);  -- /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h:216

   --  skipped anonymous struct anon_18

  -- Extra attributes for the cleanup functions.   
end oplk.x86_64_linux_gnu_bits_pthreadtypes_h;
