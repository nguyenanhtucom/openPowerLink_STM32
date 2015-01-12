
-- \brief  Definitions for user LED module
-- This file contains definitions for the user LED module.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package oplk.led is
   
   ------------------------------------------------------------------------------
   --\brief   Valid LED types                                                  --
   --                                                                          --
   -- The structure defines all valid LED types used by POWERLINK.             --
   ------------------------------------------------------------------------------
   type tLedType is 
     (kLedTypeStatus,
      -- POWERLINK Status LED
      KLedTypeError
	-- POWERLINK Error LED
     );
   pragma Convention (C, tLedType);  -- ./oplk/led.h:63
   
end oplk.Led;
