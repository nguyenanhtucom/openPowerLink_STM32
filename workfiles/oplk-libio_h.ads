pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with oplk.x86_64_linux_gnu_bits_types_h;
with oplk.stddef_h;

package oplk.libio_h is


   EOF : constant := (-1);  --  /usr/include/libio.h:90

  -- Copyright (C) 1991-1995,1997-2006,2007,2009 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   Written by Per Bothner <bothner@cygnus.com>.
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
  --   As a special exception, if you link the code in this file with
  --   files compiled with a GNU compiler to produce an executable,
  --   that does not cause the resulting executable to be covered by
  --   the GNU Lesser General Public License.  This exception does not
  --   however invalidate any other reasons why the executable file
  --   might be covered by the GNU Lesser General Public License.
  --   This exception applies to code released by its copyright holders
  --   in files containing the exception.   

  -- ALL of these should be defined in _G_config.h  
  -- This define avoids name pollution if we're using GNU stdarg.h  
  -- For backward compatibility  
  -- Magic numbers and bits for the _flags field.
  --   The magic numbers use the high-order bits of _flags;
  --   the remaining bits are available for variable flags.
  --   Note: The magic numbers must all be negative if stdio
  --   emulation is desired.  

  -- These are "formatting flags" matching the iostream fmtflags enum values.  
   --  skipped empty struct u_IO_jump_t

  -- Handle lock.   
  --# include <comthread.h> 
   subtype u_IO_lock_t is System.Address;  -- /usr/include/libio.h:180

  -- A streammarker remembers a position in a buffer.  
   type u_IO_FILE;
   type u_IO_marker is record
      u_next : access u_IO_marker;  -- /usr/include/libio.h:187
      u_sbuf : access u_IO_FILE;  -- /usr/include/libio.h:188
      u_pos : aliased int;  -- /usr/include/libio.h:192
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_marker);  -- /usr/include/libio.h:186

  -- If _pos >= 0
  -- it points to _buf->Gbase()+_pos. FIXME comment  

  -- if _pos < 0, it points to _buf->eBptr()+_pos. FIXME comment  
  -- This is the structure from the libstdc++ codecvt class.   
   type uu_codecvt_result is 
     (uu_codecvt_ok,
      uu_codecvt_partial,
      uu_codecvt_error,
      uu_codecvt_noconv);
   pragma Convention (C, uu_codecvt_result);  -- /usr/include/libio.h:206

  -- The order of the elements in the following struct must match the order
  --   of the virtual functions in the libstdc++ codecvt class.   

  -- Extra data for wide character streams.   
  -- Current read pointer  
  -- End of get area.  
  -- Start of putback+get area.  
  -- Start of put area.  
  -- Current put pointer.  
  -- End of put area.  
  -- Start of reserve area.  
  -- End of reserve area.  
  -- The following fields are used to support backing up and undo.  
  -- Pointer to start of non-current get area.  
  -- Pointer to first valid character of
  --				   backup area  

  -- Pointer to end of non-current get area.  
  -- High-order word is _IO_MAGIC; rest is flags.  
   type u_IO_FILE is record
      u_flags : aliased int;  -- /usr/include/libio.h:272
      u_IO_read_ptr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:277
      u_IO_read_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:278
      u_IO_read_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:279
      u_IO_write_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:280
      u_IO_write_ptr : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:281
      u_IO_write_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:282
      u_IO_buf_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:283
      u_IO_buf_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:284
      u_IO_save_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:286
      u_IO_backup_base : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:287
      u_IO_save_end : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libio.h:288
      u_markers : access u_IO_marker;  -- /usr/include/libio.h:290
      u_chain : access u_IO_FILE;  -- /usr/include/libio.h:292
      u_fileno : aliased int;  -- /usr/include/libio.h:294
      u_flags2 : aliased int;  -- /usr/include/libio.h:298
      u_old_offset : aliased oplk.x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/libio.h:300
      u_cur_column : aliased unsigned_short;  -- /usr/include/libio.h:304
      u_vtable_offset : aliased signed_char;  -- /usr/include/libio.h:305
      u_shortbuf : aliased u_IO_FILE_u_shortbuf_array;  -- /usr/include/libio.h:306
      u_lock : System.Address;  -- /usr/include/libio.h:310
      u_offset : aliased oplk.x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/libio.h:319
      uu_pad1 : System.Address;  -- /usr/include/libio.h:328
      uu_pad2 : System.Address;  -- /usr/include/libio.h:329
      uu_pad3 : System.Address;  -- /usr/include/libio.h:330
      uu_pad4 : System.Address;  -- /usr/include/libio.h:331
      uu_pad5 : aliased oplk.stddef_h.size_t;  -- /usr/include/libio.h:332
      u_mode : aliased int;  -- /usr/include/libio.h:334
      u_unused2 : aliased u_IO_FILE_u_unused2_array;  -- /usr/include/libio.h:336
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_FILE);  -- /usr/include/libio.h:271

  -- The following pointers correspond to the C++ streambuf protocol.  
  -- Note:  Tk uses the _IO_read_ptr and _IO_read_end fields directly.  
  -- Current read pointer  
  -- End of get area.  
  -- Start of putback+get area.  
  -- Start of put area.  
  -- Current put pointer.  
  -- End of put area.  
  -- Start of reserve area.  
  -- End of reserve area.  
  -- The following fields are used to support backing up and undo.  
  -- Pointer to start of non-current get area.  
  -- Pointer to first valid character of backup area  
  -- Pointer to end of non-current get area.  
  -- This used to be _offset but it's too small.   
  -- 1+column number of pbase(); 0 is unknown.  
  --  char* _save_gptr;  char* _save_egptr;  
  -- Wide character stream stuff.   
  -- Make sure we don't get into trouble again.   
   --  skipped empty struct u_IO_FILE_plus

  -- Functions to do I/O and file management for a stream.   
  -- Read NBYTES bytes from COOKIE into a buffer pointed to by BUF.
  --   Return number of bytes read.   

   --  skipped function type uu_io_read_fn

  -- Write N bytes pointed to by BUF to COOKIE.  Write all N bytes
  --   unless there is an error.  Return number of bytes written, or -1 if
  --   there is an error without writing anything.  If the file has been
  --   opened for append (__mode.__append set), then set the file pointer
  --   to the end of the file and then do the write; if not, just write at
  --   the current file pointer.   

   --  skipped function type uu_io_write_fn

  -- Move COOKIE's file position to *POS bytes from the
  --   beginning of the file (if W is SEEK_SET),
  --   the current position (if W is SEEK_CUR),
  --   or the end of the file (if W is SEEK_END).
  --   Set *POS to the new file position.
  --   Returns zero if successful, nonzero if not.   

   --  skipped function type uu_io_seek_fn

  -- Close COOKIE.   
   --  skipped function type uu_io_close_fn

  -- User-visible names for the above.   
   --  skipped function type cookie_read_function_t

   --  skipped function type cookie_write_function_t

   --  skipped function type cookie_seek_function_t

   --  skipped function type cookie_close_function_t

  -- The structure with the cookie function pointers.   
  -- Read bytes.   
   type u_IO_cookie_io_functions_t is record
      read : access function
           (arg1 : System.Address;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : oplk.stddef_h.size_t) return oplk.x86_64_linux_gnu_bits_types_h.uu_ssize_t;  -- /usr/include/libio.h:397
      write : access function
           (arg1 : System.Address;
            arg2 : Interfaces.C.Strings.chars_ptr;
            arg3 : oplk.stddef_h.size_t) return oplk.x86_64_linux_gnu_bits_types_h.uu_ssize_t;  -- /usr/include/libio.h:398
      seek : access function
           (arg1 : System.Address;
            arg2 : access oplk.x86_64_linux_gnu_bits_types_h.uu_off64_t;
            arg3 : int) return int;  -- /usr/include/libio.h:399
      close : access function (arg1 : System.Address) return int;  -- /usr/include/libio.h:400
   end record;
   pragma Convention (C_Pass_By_Copy, u_IO_cookie_io_functions_t);  -- /usr/include/libio.h:401

   --  skipped anonymous struct anon_23

  -- Write bytes.   
  -- Seek/tell file position.   
  -- Close file.   
   subtype cookie_io_functions_t is u_IO_cookie_io_functions_t;

   --  skipped empty struct u_IO_cookie_file

  -- Initialize one of those.   
   --  skipped func _IO_cookie_init

   --  skipped func __underflow

   --  skipped func __uflow

   --  skipped func __overflow

   --  skipped func _IO_getc

   --  skipped func _IO_putc

   --  skipped func _IO_feof

   --  skipped func _IO_ferror

   --  skipped func _IO_peekc_locked

  -- This one is for Emacs.  
   --  skipped func _IO_flockfile

   --  skipped func _IO_funlockfile

   --  skipped func _IO_ftrylockfile

   --  skipped func _IO_vfscanf

   --  skipped func _IO_vfprintf

   --  skipped func _IO_padn

   --  skipped func _IO_sgetn

   --  skipped func _IO_seekoff

   --  skipped func _IO_seekpos

   --  skipped func _IO_free_backup_area

  -- While compiling glibc we have to handle compatibility with very old
  --   versions.   

  -- A special optimized version of the function above.  It optimizes the
  --   case of initializing an unoriented byte stream.   

end oplk.libio_h;
