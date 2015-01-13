--\brief  Standard include file for public headers.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package Oplk.oplkinc is
   
   
------------------------------------------------------------------------------
--\brief  Timestamp structure                                               --
--                                                                          --
-- The following structure defines a timestamp value use to store target    --
-- specific timestamps.                                                     --
------------------------------------------------------------------------------
   type tTimestamp is record
      timeStamp : aliased unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, tTimestamp);  -- ./oplk/oplkinc.h:72
   
   
------------------------------------------------------------------------------
--\brief  IEEE 1588 conforming net time structure                           --
--                                                                          --
-- The structure defines an IEEE 1588 conforming net time.                  --
------------------------------------------------------------------------------
   type tNetTime is record
      sec  : aliased unsigned;
      -- Seconds
      nsec : aliased unsigned;
      -- Nanoseconds
   end record;
   pragma Convention (C_Pass_By_Copy, tNetTime);  -- ./oplk/oplkinc.h:69

------------------------------------------------------------------------------
--\brief Hardware parameter structure                                       --
--                                                                          --
-- The following structure specifies the hardware parameters of an          --
-- openPOWERLINK Ethernet controller.                                       --
------------------------------------------------------------------------------
   type tHwParam is record
      devNum   : aliased unsigned;
      -- Device number of the used Ethernet controller
      pDevName : Interfaces.C.Strings.chars_ptr;
      -- Device name of the Ethernet controller (valid if non-null)
   end record;
   pragma Convention (C_Pass_By_Copy, tHwParam);  -- ./oplk/oplkinc.h:81

------------------------------------------------------------------------------
--\brief Time of day structure                                              --
--                                                                          --
-- The following structure defines a CANopen time-of-day format.            --
------------------------------------------------------------------------------
   type tTimeOfDay is record
      msec : aliased unsigned_long;
      -- Milliseconds after midnight
      days : aliased unsigned_short;
      -- Days since January the 1st, 1984
   end record;
   pragma Convention (C_Pass_By_Copy, tTimeOfDay);  -- ./oplk/oplkinc.h:93

end Oplk.oplkinc;
