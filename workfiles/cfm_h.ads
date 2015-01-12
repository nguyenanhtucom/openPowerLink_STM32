pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with errordefs_h;

package cfm_h is

  --*
  --********************************************************************************
  --\file   oplk/cfm.h
  --\brief  General include file for configuration file manager (CFM)
  --This file contains global definitions for the CFM module.
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
  --------------------------------------------------------------------------------
  -- typedef
  --------------------------------------------------------------------------------
  --*
  --* \brief Structure for CFM CN progress event
  --*
  --* This structure contains all information of the CFMs CN progress event.
  -- 

  --/< Node ID of the CN
   type tCfmEventCnProgress is record
      nodeId : aliased unsigned;  -- ./oplk/cfm.h:61
      objectIndex : aliased unsigned;  -- ./oplk/cfm.h:62
      objectSubIndex : aliased unsigned;  -- ./oplk/cfm.h:63
      sdoAbortCode : aliased unsigned;  -- ./oplk/cfm.h:64
      error : aliased errordefs_h.tOplkError;  -- ./oplk/cfm.h:65
      totalNumberOfBytes : aliased unsigned;  -- ./oplk/cfm.h:66
      bytesDownloaded : aliased unsigned;  -- ./oplk/cfm.h:67
   end record;
   pragma Convention (C_Pass_By_Copy, tCfmEventCnProgress);  -- ./oplk/cfm.h:68

   --  skipped anonymous struct anon_70

  --/< Index of object to be written
  --/< Subindex of object to be written
  --/< SDO abort code
  --/< Error which occured
  --/< Total number of bytes to transfer
  --/< Number of already downloaded bytes
end cfm_h;
