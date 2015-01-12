pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package oplkinc_h is

  --*
  --********************************************************************************
  --\file   oplk/oplkinc.h
  --\brief  Standard include file for public headers.
  --This is the standard include file for all public openPOWERLINK header files.
  --It includes all necessary files for setting up the basic types and definitions.
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
  --\brief  IEEE 1588 conforming net time structure
  --The structure defines an IEEE 1588 conforming net time.
  -- 

  --/< Seconds
   type tNetTime is record
      sec : aliased unsigned;  -- ./oplk/oplkinc.h:67
      nsec : aliased unsigned;  -- ./oplk/oplkinc.h:68
   end record;
   pragma Convention (C_Pass_By_Copy, tNetTime);  -- ./oplk/oplkinc.h:69

   --  skipped anonymous struct anon_26

  --/< Nanoseconds
  --*
  --\brief Hardware parameter structure
  --The following structure specifies the hardware parameters of an openPOWERLINK
  --Ethernet controller.
  -- 

  --/< Device number of the used Ethernet controller
   type tHwParam is record
      devNum : aliased unsigned;  -- ./oplk/oplkinc.h:79
      pDevName : Interfaces.C.Strings.chars_ptr;  -- ./oplk/oplkinc.h:80
   end record;
   pragma Convention (C_Pass_By_Copy, tHwParam);  -- ./oplk/oplkinc.h:81

   --  skipped anonymous struct anon_27

  --/< Device name of the Ethernet controller (valid if non-null)
  --*
  --\brief Time of day structure
  --The following structure defines a CANopen time-of-day format.
  -- 

  --/< Milliseconds after midnight
   type tTimeOfDay is record
      msec : aliased unsigned_long;  -- ./oplk/oplkinc.h:91
      days : aliased unsigned_short;  -- ./oplk/oplkinc.h:92
   end record;
   pragma Convention (C_Pass_By_Copy, tTimeOfDay);  -- ./oplk/oplkinc.h:93

   --  skipped anonymous struct anon_28

  --/< Days since January the 1st, 1984
  --------------------------------------------------------------------------------
  -- global macros
  --------------------------------------------------------------------------------
end oplkinc_h;
