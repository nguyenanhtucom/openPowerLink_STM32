
--\brief  Body template for Ethernet driver module
--This file contains a body template for the Ethernet driver module.

package body Oplk.Edrv is
   
   ------------------------------------------------------------------------------
   -- public functions                                                         --
   ------------------------------------------------------------------------------
   
   
   ------------------------------------------------------------------------------
   -- pEdrvInitParam_p    Edrv initialization parameters
   -- The function returns a tOplkError error code.
   -- 
   -- This function initializes the Ethernet driver.
   ------------------------------------------------------------------------------
   function Edrv_Init 
     (pEdrvInitParam_p : access tEdrvInitParam) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_Init;
   
   
   ------------------------------------------------------------------------------
   -- The function returns a tOplkError error code.
   -- 
   -- This function shuts down the Ethernet driver.
   ------------------------------------------------------------------------------
   function Edrv_Shutdown 
     return errordefs.TOplkError
   is
   begin
      return Errordefs.KErrorOk;
   end Edrv_Shutdown;
   
   
   ------------------------------------------------------------------------------
   -- pMacAddr_p  Multicast address
   -- The function returns a tOplkError error code.
   -- 
   -- This function sets a multicast entry into the Ethernet controller.
   ------------------------------------------------------------------------------
   function edrv_setRxMulticastMacAddr 
     (pMacAddr_p : access unsigned_char) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_SetRxMulticastMacAddr;
   
   
   ------------------------------------------------------------------------------
   -- pMacAddr_p  Multicast address
   -- The function returns a tOplkError error code.
   -- 
   -- This function removes the multicast entry from the Ethernet controller.
   ------------------------------------------------------------------------------
   function edrv_clearRxMulticastMacAddr 
     (pMacAddr_p : access unsigned_char) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_ClearRxMulticastMacAddr;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p           Tx buffer descriptor
   -- The function returns a tOplkError error code.
   -- 
   -- This function allocates a Tx buffer.
   ------------------------------------------------------------------------------
   function edrv_allocTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_AllocTxBuffer;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p           Tx buffer descriptor
   -- The function returns a tOplkError error code.
   -- 
   -- This function releases the Tx buffer.
   ------------------------------------------------------------------------------
   function edrv_freeTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_FreeTxBuffer;
   
   
   function edrv_updateTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_UpdateTxBuffer;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p           Tx buffer descriptor
   -- The function returns a tOplkError error code.
   -- 
   -- This function sends the Tx buffer.
   ------------------------------------------------------------------------------
   function edrv_sendTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_SendTxBuffer;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p   Tx buffer buffer descriptor
   -- The function returns a tOplkError error code.
   -- 
   -- This function sets the Tx buffer buffer ready for transmission.
   ------------------------------------------------------------------------------
   function edrv_setTxBufferReady 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_SetTxBufferReady;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p   Tx buffer buffer descriptor
   -- The function returns a tOplkError error code.
   -- 
   -- This function sends the Tx buffer marked as ready.
   ------------------------------------------------------------------------------
   function edrv_startTxBuffer 
     (pBuffer_p : access tEdrvTxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_StartTxBuffer;
   
   
   ------------------------------------------------------------------------------
   -- pRxBuffer_p     Rx buffer to be released
   -- The function returns a tOplkError error code.
   -- 
   -- This function releases a late release Rx buffer.
   ------------------------------------------------------------------------------
   function edrv_releaseRxBuffer 
     (pBuffer_p : access tEdrvRxBuffer) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_ReleaseRxBuffer;
   
   
   ------------------------------------------------------------------------------
   -- pFilter_p           Base pointer of Rx filter array
   -- count_p             Number of Rx filter array entries
   -- entryChanged_p      Index of Rx filter entry that shall be changed
   -- changeFlags_p       Bit mask that selects the changing Rx filter property
   -- The function returns a tOplkError error code.
   -- 
   -- This function changes the Rx filter setup. The parameter entryChanged_p
   -- selects the Rx filter entry that shall be changed and changeFlags_p 
   -- determines the property.
   -- If entryChanged_p is equal or larger count_p all Rx filters shall 
   -- be changed.
   -- 
   -- \note Rx filters are not supported by this driver! (maybe)!!!!!!!!!!!!!!!!
   ------------------------------------------------------------------------------
   function Edrv_ChangeRxFilter
     (pFilter_p      : access tEdrvFilter;
      count_p        : unsigned;
      entryChanged_p : unsigned;
      changeFlags_p  : unsigned) 
     return errordefs.TOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_ChangeRxFilter;
   
   
   ------------------------------------------------------------------------------
   -- pBuffer_p   Pointer to buffer filled with diagnostics.
   -- size_p      Size of buffer
   -- The function returns a tOplkError error code.
   -- 
   -- This function returns the Edrv diagnostics to a provided buffer.
   ------------------------------------------------------------------------------
   function edrv_getDiagnostics 
     (pBuffer_p : Interfaces.C.Strings.chars_ptr; 
      size_p    : int) 
     return errordefs.tOplkError
   is
      
   begin
      return Errordefs.KErrorOk;
   end Edrv_GetDiagnostics;
   
   
   
   
end Oplk.Edrv;
