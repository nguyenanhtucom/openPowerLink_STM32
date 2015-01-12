pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.x86_64_linux_gnu_bits_types_h;
with oplk.wchar_h;

package oplk.uG_config_h is

  -- This file is needed by libio to define various configuration parameters.
  --   These are always the same in the GNU C library.   

  -- Define types for libio in terms of the standard internal type names.   
   type u_G_fpos_t is record
      uu_pos : aliased oplk.x86_64_linux_gnu_bits_types_h.uu_off_t;  -- /usr/include/_G_config.h:24
      uu_state : aliased oplk.wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:25
   end record;
   pragma Convention (C_Pass_By_Copy, u_G_fpos_t);  -- /usr/include/_G_config.h:26

   --  skipped anonymous struct anon_21

   type u_G_fpos64_t is record
      uu_pos : aliased oplk.x86_64_linux_gnu_bits_types_h.uu_off64_t;  -- /usr/include/_G_config.h:29
      uu_state : aliased oplk.wchar_h.uu_mbstate_t;  -- /usr/include/_G_config.h:30
   end record;
   pragma Convention (C_Pass_By_Copy, u_G_fpos64_t);  -- /usr/include/_G_config.h:31

   --  skipped anonymous struct anon_22

   subtype u_G_int16_t is short;  -- /usr/include/_G_config.h:53

   subtype u_G_int32_t is int;  -- /usr/include/_G_config.h:54

   subtype u_G_uint16_t is unsigned_short;  -- /usr/include/_G_config.h:55

   subtype u_G_uint32_t is unsigned;  -- /usr/include/_G_config.h:56

  -- These library features are always available in the GNU C library.   
  -- This is defined by <bits/stat.h> if `st_blksize' exists.   
  -- These are the vtbl details for ELF.   
end oplk.uG_config_h;
