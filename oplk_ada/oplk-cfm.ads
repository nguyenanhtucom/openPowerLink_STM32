
  --\brief  General include file for configuration file manager (CFM)
  --This file contains global definitions for the CFM module.


pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with oplk.errordefs;

package oplk.cfm is
   
------------------------------------------------------------------------------
--\brief Structure for CFM CN progress event                                --
--                                                                          --
-- This structure contains all information of the CFMs CN progress event.   --
------------------------------------------------------------------------------
   type tCfmEventCnProgress is record
      nodeId             : aliased unsigned;
      -- Node ID of the CN
      objectIndex        : aliased unsigned;
      -- Index of object to be written
      ObjectSubIndex     : aliased unsigned;
      -- Subindex of object to be written
      sdoAbortCode       : aliased unsigned;
      -- SDO abort code
      error              : aliased oplk.errordefs.tOplkError;
      -- Error which occured
      totalNumberOfBytes : aliased unsigned;
      -- Total number of bytes to transfer
      BytesDownloaded    : aliased unsigned;
      -- Number of already downloaded bytes
   end record;
   pragma Convention (C_Pass_By_Copy, tCfmEventCnProgress);  -- ./oplk/cfm.h:68
   
   
end oplk.cfm;
