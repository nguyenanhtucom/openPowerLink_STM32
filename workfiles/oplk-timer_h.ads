pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package oplk.timer_h is

  --*
  --********************************************************************************
  --\file   common/timer.h
  --\brief  Generic definitions for timer modules
  --This file contains some generic definitions for timer modules.
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
  -- type for timer handle
   subtype tTimerHdl is unsigned_long;  -- ./oplk/timer.h:58

  --*
  --\brief  Structure for timer event arguments
  --The structure defines a timer event argument. It provides information about
  --the timer to the sink the event is sent to.
  -- 

  --/< Delivers the handle of the expired timer
   type anon_50 (discr : unsigned := 0) is record
      case discr is
         when 0 =>
            value : aliased unsigned;  -- ./oplk/timer.h:72
         when others =>
            pValue : System.Address;  -- ./oplk/timer.h:73
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, anon_50);
   pragma Unchecked_Union (anon_50);
   type tTimerEventArg is record
      timerHdl : aliased tTimerHdl;  -- ./oplk/timer.h:69
      argument : anon_50;  -- ./oplk/timer.h:74
   end record;
   pragma Convention (C_Pass_By_Copy, tTimerEventArg);  -- ./oplk/timer.h:75

   --  skipped anonymous struct anon_49

  --/< Timer argument supplied as UINT32
  --/< Timer argument supplied as void*
  --/< The timer argument the timer was initialized with.
end oplk.timer_h;
