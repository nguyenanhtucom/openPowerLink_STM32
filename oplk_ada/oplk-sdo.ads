
-- \brief  Definitions for SDO module
--  This file contains definitions for the SDO module.

pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package Oplk.sdo is

------------------------------------------------------------------------------
-- Type definitions                                                         --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Data type for handle between SDO Command Layer and application           --
------------------------------------------------------------------------------
   subtype tSdoComConHdl is unsigned;  -- ./oplk/sdo.h:62
   

------------------------------------------------------------------------------
--\brief Enumeration for SDO types                                          --
--                                                                          --
-- This enumeration lists all valid SDO types.                              --
------------------------------------------------------------------------------
   type tSdoType is (kSdoTypeAuto, 
		     -- SDO connection type is automatically selected
		     kSdoTypeUdp, 
		     -- Use SDO via UDP
		     kSdoTypeAsnd, 
		     -- Use SDO via ASnd
		     KSdoTypePdo
		     -- Use SDO via PDO
		    );
   pragma Convention (C, tSdoType);  -- ./oplk/sdo.h:75
   
   
------------------------------------------------------------------------------
--\brief Enumeration lists valid SDO Command Layer connection states        --
--                                                                          --
-- This enumeration lists all valid SDO Command Layer connection states.    --
------------------------------------------------------------------------------
   type tSdoComConState is
     (kSdoComTransferNotActive,
      -- SDO transfer is not active
      kSdoComTransferRunning,
      -- SDO transfer is currently running
      kSdoComTransferTxAborted,
      -- SDO transfer is aborted (abort code is going to be sent)
      kSdoComTransferRxAborted,
      -- SDO transfer has been aborted by the remote side
      kSdoComTransferFinished,
      -- SDO transfer is finished
      KSdoComTransferLowerLayerAbort
      -- SDO transfer has been aborted by the SDO sequence layer
     );
   pragma Convention (C, tSdoComConState);  -- ./oplk/sdo.h:90
   
   
------------------------------------------------------------------------------
--\brief Enumeration for SDO access types                                   --
--                                                                          --
-- This enumeration lists all valid SDO access types.                       --
------------------------------------------------------------------------------
   type tSdoAccessType is 
     (kSdoAccessTypeRead, 
      -- SDO read access
      KSdoAccessTypeWrite
      -- SDO write access
     );
   pragma Convention (C, tSdoAccessType);  -- ./oplk/sdo.h:101
   
   
------------------------------------------------------------------------------
--\brief Structure for finished SDO transfer                                --
--                                                                          --
-- This structure is used to inform the application about a finished        --
-- SDO transfer.                                                            --
------------------------------------------------------------------------------
   type tSdoComFinished is record
      sdoComConHdl     : aliased tSdoComConHdl;
      -- Handle to SDO Command Layer connection
      sdoComConState   : aliased tSdoComConState;
      -- Status of SDO Command Layer connection
      abortCode        : aliased unsigned;
      -- SDO abort code
      sdoAccessType    : aliased tSdoAccessType;
      -- SDO access type
      nodeId           : aliased unsigned;
      -- The node ID of the target
      targetIndex      : aliased unsigned;
      -- Index which was accessed
      targetSubIndex   : aliased unsigned;
      -- Sub-index which was accessed
      transferredBytes : aliased unsigned;
      -- The number of bytes transferred
      pUserArg         : System.Address;
      -- The user defined argument pointer
   end record;
   pragma Convention (C_Pass_By_Copy, tSdoComFinished);  -- ./oplk/sdo.h:119

end Oplk.sdo;
