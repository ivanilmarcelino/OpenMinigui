
#pragma BEGINDUMP

#include <ShObjIdl.h>
#include <mgdefs.h>

HB_FUNC( GETICOINDEX )
{
   SHFILEINFO sfi;

   SHGetFileInfo( hb_parc( 1 ), 0, &sfi, sizeof( SHFILEINFO ), SHGFI_ICON | SHGFI_SMALLICON | SHGFI_SYSICONINDEX );

   hb_retni( sfi.iIcon );
}

//        ListView_SetImageList ( hWnd , hImageList , [iImageList] )
HB_FUNC( LISTVIEW_SETIMAGELIST )
{
   HWND       hWnd       = hmg_par_raw_HWND( 1 );
   HIMAGELIST hImageList = hmg_par_raw_HIMAGELIST( 2 );
   int        iImageList = HB_ISNIL( 3 ) ? LVSIL_SMALL : hb_parni( 3 );

   hmg_ret_raw_HANDLE( ListView_SetImageList( hWnd, hImageList, iImageList ) );
}

//        ListView_SetItemImageIndex ( hWnd , nRow , nCol, [ nImageIndex ] )
HB_FUNC( LISTVIEW_SETITEMIMAGEINDEX )
{
   HWND hWnd   = hmg_par_raw_HWND( 1 );
   int  nRow   = hb_parni( 2 );
   int  nCol   = hb_parni( 3 );
   int  iImage = HB_ISNIL( 4 ) ? -1 : hb_parni( 4 );

   LV_ITEM LV;

   LV.mask      = LVIF_IMAGE;
   LV.state     = 0;
   LV.stateMask = 0;
   LV.iImage    = iImage;
   LV.iSubItem  = nCol - 1;
   LV.iItem     = nRow - 1;

   ListView_SetItem( hWnd, &LV );
}

#pragma ENDDUMP

*
* EOF
*