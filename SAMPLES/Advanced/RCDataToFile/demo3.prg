/*
 * Author: P.Chornyj <myorg63@mail.ru>
 */

ANNOUNCE RDDSYS

///////////////////////////////////////////////////////////////////////////////
procedure main()

   local cMemVar
   local cDiskFile := "image1.png"
   local nResult, aXY, cMsg

   delete file cDiskFile

   cMemVar := RCDataToMem( "IMAGE1", "PNG" )

   if Len( cMemVar ) > 0
      hb_memoWrit( cDiskFile, cMemVar ) 

      if hb_FileExists( cDiskFile )
         aXY  := hb_GetImageSize( cDiskFile )

         cMsg := ( "IMAGE1 saved successfully as" + Chr(13) + Chr(10) )
         cMsg += ( cDiskFile + ": " + hb_ntos( aXY[1] ) + " x " + hb_ntos( aXY[2] ) + " Pixels" )

         MsgInfo( cMsg, "Congratulations!" )
      endif
   else
      MsgInfo( "Code: " + hb_ntos( nResult ), "Error" )
   endif

return
