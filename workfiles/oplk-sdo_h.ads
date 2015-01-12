pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package oplk.sdo_h is

   --  unsupported macro: SDO_MAX_FRAME_SIZE C_IP_MIN_MTU
   --  unsupported macro: SDO_MAX_REC_FRAME_SIZE C_IP_MAX_MTU
  --*
  --********************************************************************************
  --\file   oplk/sdo.h
  --\brief  Definitions for SDO module
  --This file contains definitions for the SDO module.
  --****************************************************************************** 

  --------------------------------------------------------------------------------
  --Copyright (c) 2014, Bernecker+Rainer Industrie-Elektronik Ges.m.b.H. (B&R)
  --Copyright (c) 2013, SYSTEC electronic GmbH
  --All rights reserved.
  --Redistribution and use in source and binary forms, with or without
  --modification, are permitted provided that the following conditions are met:
  --    * Redistributions of source code must retain the above copyright
  --      notice, this list of conditions and the following disclaimer.
  --    * Redistributions in binary form must reproduce the above copyright
  --      notice, this list of conditions and the following disclaimer in the
  --      documentation and/or other materials provided with the distribution.
  --    * Neither the name of the copyright holders nor the
  --      names of its contributors may be used to endorse or promote products
  --      derived from this software without specific prior written permission.
  --THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  --ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  --WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  --DISCLAIMED. IN NO EVENT SHALL COPYRIGHT HOLDERS BE LIABLE FOR ANY
  --DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  --(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  --LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  --ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  --(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  --SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  -------------------------------------------------------------------------------- 

  --------------------------------------------------------------------------------
  -- includes
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  -- const defines
  --------------------------------------------------------------------------------
  -- size for send buffer and history
  -- size for receive frame
  -- -> needed because SND-Kit sends up to 1518 Byte
  --    without Sdo-Command: Maximum Segment Size
  --------------------------------------------------------------------------------
  -- Type definitions
  --------------------------------------------------------------------------------
  --/ Data type for handle between SDO Command Layer and application
   subtype tSdoComConHdl is unsigned;  -- ./oplk/sdo.h:62

  --*
  --\brief Enumeration for SDO types
  --This enumeration lists all valid SDO types.
  -- 

  --/< SDO connection type is automatically selected
  --/< Use SDO via UDP
  --/< Use SDO via ASnd
  --/< Use SDO via PDO
   type tSdoType is 
     (kSdoTypeAuto,
      kSdoTypeUdp,
      kSdoTypeAsnd,
      kSdoTypePdo);
   pragma Convention (C, tSdoType);  -- ./oplk/sdo.h:75

  --*
  --\brief Enumeration lists valid SDO Command Layer connection states
  --This enumeration lists all valid SDO Command Layer connection states.
  -- 

  --/< SDO transfer is not active
  --/< SDO transfer is currently running
  --/< SDO transfer is aborted (abort code is going to be sent)
  --/< SDO transfer has been aborted by the remote side
  --/< SDO transfer is finished
  --/< SDO transfer has been aborted by the SDO sequence layer
   type tSdoComConState is 
     (kSdoComTransferNotActive,
      kSdoComTransferRunning,
      kSdoComTransferTxAborted,
      kSdoComTransferRxAborted,
      kSdoComTransferFinished,
      kSdoComTransferLowerLayerAbort);
   pragma Convention (C, tSdoComConState);  -- ./oplk/sdo.h:90

  --*
  --\brief Enumeration for SDO access types
  --This enumeration lists all valid SDO access types.
  -- 

  --/< SDO read access
  --/< SDO write access
   type tSdoAccessType is 
     (kSdoAccessTypeRead,
      kSdoAccessTypeWrite);
   pragma Convention (C, tSdoAccessType);  -- ./oplk/sdo.h:101

  --*
  --\brief Structure for finished SDO transfer
  --This structure is used to inform the application about a finished SDO transfer.
  -- 

  --/< Handle to SDO Command Layer connection
   type tSdoComFinished is record
      sdoComConHdl : aliased tSdoComConHdl;  -- ./oplk/sdo.h:110
      sdoComConState : aliased tSdoComConState;  -- ./oplk/sdo.h:111
      abortCode : aliased unsigned;  -- ./oplk/sdo.h:112
      sdoAccessType : aliased tSdoAccessType;  -- ./oplk/sdo.h:113
      nodeId : aliased unsigned;  -- ./oplk/sdo.h:114
      targetIndex : aliased unsigned;  -- ./oplk/sdo.h:115
      targetSubIndex : aliased unsigned;  -- ./oplk/sdo.h:116
      transferredBytes : aliased unsigned;  -- ./oplk/sdo.h:117
      pUserArg : System.Address;  -- ./oplk/sdo.h:118
   end record;
   pragma Convention (C_Pass_By_Copy, tSdoComFinished);  -- ./oplk/sdo.h:119

   --  skipped anonymous struct anon_51

  --/< Status of SDO Command Layer connection
  --/< SDO abort code
  --/< SDO access type
  --/< The node ID of the target
  --/< Index which was accessed
  --/< Sub-index which was accessed
  --/< The number of bytes transferred
  --/< The user defined argument pointer
end oplk.sdo_h;
