pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package basictypes_h is

   --  unsupported macro: SHORT short int
   --  unsupported macro: USHORT unsigned short int
   --  unsupported macro: INT int
   --  unsupported macro: UINT unsigned int
   --  unsupported macro: LONG long int
   --  unsupported macro: ULONG unsigned long int
   --  unsupported macro: ULONGLONG unsigned long long int
   --  unsupported macro: BYTE unsigned char
   --  unsupported macro: WORD unsigned short int
   --  unsupported macro: DWORD unsigned int
   --  unsupported macro: QWORD unsigned long long int
   --  unsupported macro: BOOL unsigned char
   --  unsupported macro: UINT8 unsigned char
   --  unsupported macro: UINT16 unsigned short int
   --  unsupported macro: UINT32 unsigned int
   --  unsupported macro: UINT64 unsigned long long int
   --  unsupported macro: INT8 char
   --  unsupported macro: INT16 short int
   --  unsupported macro: INT32 int
   --  unsupported macro: INT64 long long int

   TRUE : constant := 16#FF#;  --  ./oplk/basictypes.h:131

   FALSE : constant := 16#00#;  --  ./oplk/basictypes.h:135

  --*
  --********************************************************************************
  --\file   oplk/basictypes.h
  --\brief  Basic type definitions for openPOWERLINK
  --This file contains basic type definitions for openPOWERLINK.
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

  -----------------------------------------------------------------------------
  --  definitions of basic types
  -- --- arithmetic types ---
  -- --- logic types ---
  -- --- bit sized types ---
  -- --- alias types ---
  -- some standard function pointer types  
   type VOIDFUNCPTR is access procedure;
   pragma Convention (C, VOIDFUNCPTR);  -- ./oplk/basictypes.h:145

   type INTFUNCPTR is access function return int;
   pragma Convention (C, INTFUNCPTR);  -- ./oplk/basictypes.h:146

end basictypes_h;
