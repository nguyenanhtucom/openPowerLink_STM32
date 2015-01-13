
--\brief  Generic definitions for timer modules
--This file contains some generic definitions for timer modules.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package oplk.timer is
   
   -- type for timer handle
   subtype tTimerHdl is unsigned_long;  -- ./oplk/timer.h:58
   
------------------------------------------------------------------------------
--\brief  Structure for timer event arguments                               --
-- The structure defines a timer event argument. It provides information    --
-- about the timer to the sink the event is sent to.                        --
------------------------------------------------------------------------------
   type anon_50 (discr : unsigned := 0) is record
      -- The timer argument the timer was initialized with.
      -- check if this crap is needed at all !!!!!!!!!!!!!!!!!!!!!!!
      case discr is
         when 0 =>
            value : aliased unsigned;
	    -- Timer argument supplied as UINT32
         when others =>
            pValue : System.Address;
	    -- Timer argument supplied as void*
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_50);
   pragma Unchecked_Union (anon_50);
   type tTimerEventArg is record
      timerHdl : aliased tTimerHdl;
      -- Delivers the handle of the expired timer
      argument : anon_50;
   end record;
   pragma Convention (C_Pass_By_Copy, tTimerEventArg);  -- ./oplk/timer.h:75
   
end oplk.Timer;

