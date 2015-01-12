pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package debug_h is


   DEBUG_LVL_EDRV : constant := 16#00000001#;  --  ./oplk/debug.h:56
   DEBUG_LVL_DLL : constant := 16#00000002#;  --  ./oplk/debug.h:57
   DEBUG_LVL_OBD : constant := 16#00000004#;  --  ./oplk/debug.h:58
   DEBUG_LVL_NMTK : constant := 16#00000008#;  --  ./oplk/debug.h:59
   DEBUG_LVL_NMTCN : constant := 16#00000010#;  --  ./oplk/debug.h:60
   DEBUG_LVL_NMTU : constant := 16#00000020#;  --  ./oplk/debug.h:61
   DEBUG_LVL_NMTMN : constant := 16#00000040#;  --  ./oplk/debug.h:62
   DEBUG_LVL_CFM : constant := 16#00000080#;  --  ./oplk/debug.h:63
   DEBUG_LVL_TIMERU : constant := 16#00000100#;  --  ./oplk/debug.h:64
   DEBUG_LVL_TIMERH : constant := 16#00000200#;  --  ./oplk/debug.h:65
   DEBUG_LVL_CTRL : constant := 16#00000400#;  --  ./oplk/debug.h:66

   DEBUG_LVL_PDO : constant := 16#00800000#;  --  ./oplk/debug.h:68
   DEBUG_LVL_SDO : constant := 16#01000000#;  --  ./oplk/debug.h:69
   DEBUG_LVL_VETH : constant := 16#02000000#;  --  ./oplk/debug.h:70
   DEBUG_LVL_EVENTK : constant := 16#04000000#;  --  ./oplk/debug.h:71
   DEBUG_LVL_EVENTU : constant := 16#08000000#;  --  ./oplk/debug.h:72

   DEBUG_LVL_ASSERT : constant := 16#20000000#;  --  ./oplk/debug.h:74
   DEBUG_LVL_ERROR : constant := 16#40000000#;  --  ./oplk/debug.h:75
   DEBUG_LVL_ALWAYS : constant := 16#80000000#;  --  ./oplk/debug.h:76
   --  unsupported macro: DEF_DEBUG_LVL (DEBUG_LVL_ALWAYS | DEBUG_LVL_ERROR)
   --  unsupported macro: DEBUG_GLB_LVL (DEF_DEBUG_LVL)
   --  unsupported macro: DEBUG_LVL_ALWAYS_TRACE(...) TRACE(__VA_ARGS__)
   --  unsupported macro: DEBUG_LVL_ERROR_TRACE(...) TRACE(__VA_ARGS__)
   --  unsupported macro: TRACE(...) trace(__VA_ARGS__)
   --  arg-macro: procedure ASSERTMSG (expr, string)
   --    if (not(expr)) { PRINTF("Assertion failed: " string); for ( ; ; ); }

  --*
  --********************************************************************************
  --\file   oplk/debug.h
  --\brief  Definitions for debugging
  --The file contains definitions used the debugging.
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
  -- These definitions are important for level-debug traces.
  -- A macro DEBUG_GLB_LVL defines the current debug-level using following bis.
  -- If the corresponding bit is set then trace message will be printed out
  -- (only if NDEBUG is not defined). The upper debug-levels are reserved for
  -- the debug-levels ALWAYS, ERROR and ASSERT.
  -----------------------------------------------------------------------------
  -- The default debug-level is: ERROR and ALWAYS.
  -- You can define an other debug-level in project settings.
  -----------------------------------------------------------------------------
  -- At microcontrollers we do reduce the memory usage by deleting DEBUG_TRACE-lines
  -- (compiler does delete the lines).
  -- Here the parameter 'lvl' can only be used with one debug-level.
  -- Example: DEBUG_TRACE(DEBUG_LVL_ERROR, "error code %d", dwRet);
  --------------------------------------------------------------------------------
  --  definition of TRACE
  --------------------------------------------------------------------------------
   procedure trace (fmt : Interfaces.C.Strings.chars_ptr  -- , ...
      );  -- ./oplk/debug.h:222
   pragma Import (C, trace, "trace");

  --------------------------------------------------------------------------------
  --  definition of ASSERT
  --------------------------------------------------------------------------------
  --------------------------------------------------------------------------------
  -- This macro doesn't print out C-file and line number of the failed assertion
  -- but a string, which exactly names the mistake.
  --------------------------------------------------------------------------------
end debug_h;
