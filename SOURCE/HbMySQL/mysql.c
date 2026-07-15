/*
 * MySQL DBMS low-level (client API) interface code.
 *
 * Copyright 2010 Viktor Szakats (GC support)
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net> (DATATOSQL(), FILETOSQLBINARY())
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapifs.h"

/* NOTE: To satisfy MySQL headers. */
#if defined( HB_OS_WIN )
#include <winsock2.h>
#endif

#if defined( HB_GCC_HAS_DIAG ) && defined( __clang__ )
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wignored-attributes" /* Windows */
#pragma GCC diagnostic ignored "-Wstrict-prototypes"  /* darwin */
#endif

#include "mysql.h"

#if defined( HB_GCC_HAS_DIAG ) && defined( __clang__ )
#pragma GCC diagnostic pop
#endif

#if !defined( MYSQL_VERSION_ID )
#if defined( MARIADB_VERSION_ID )

/* Required since MariaDB ~10.2.* */
#define MYSQL_VERSION_ID   MARIADB_VERSION_ID
#else
#define MYSQL_VERSION_ID   0
#endif
#endif

/* GC object handlers */
static HB_GARBAGE_FUNC( MYSQL_release )
{
   void  **ph = ( void ** ) Cargo;
   if( ph && *ph )
   {
      mysql_close( ( MYSQL * ) *ph );
      *ph = NULL;
   }
}

static const HB_GC_FUNCS   s_gcMYSQLFuncs = { MYSQL_release, hb_gcDummyMark };

static HB_GARBAGE_FUNC( MYSQL_RES_release )
{
   void  **ph = ( void ** ) Cargo;
   if( ph && *ph )
   {
      mysql_free_result( ( MYSQL_RES * ) *ph );
      *ph = NULL;
   }
}

static const HB_GC_FUNCS   s_gcMYSQL_RESFuncs = { MYSQL_RES_release, hb_gcDummyMark };

/* Helper functions for GC pointer handling */
static void hb_MYSQL_ret( MYSQL *p )
{
   if( p )
   {
      void  **ph = ( void ** ) hb_gcAllocate( sizeof( MYSQL * ), &s_gcMYSQLFuncs );
      *ph = p;
      hb_retptrGC( ph );
   }
   else
   {
      hb_retptr( NULL );
   }
}

static MYSQL *hb_MYSQL_par( int iParam )
{
   void  **ph = ( void ** ) hb_parptrGC( &s_gcMYSQLFuncs, iParam );
   return ph ? ( MYSQL * ) *ph : NULL;
}

static void hb_MYSQL_RES_ret( MYSQL_RES *p )
{
   if( p )
   {
      void  **ph = ( void ** ) hb_gcAllocate( sizeof( MYSQL_RES * ), &s_gcMYSQL_RESFuncs );
      *ph = p;
      hb_retptrGC( ph );
   }
   else
   {
      hb_retptr( NULL );
   }
}

static MYSQL_RES *hb_MYSQL_RES_par( int iParam )
{
   void  **ph = ( void ** ) hb_parptrGC( &s_gcMYSQL_RESFuncs, iParam );
   return ph ? ( MYSQL_RES * ) *ph : NULL;
}

/* Common error handler */
static void hb_mysqlError( void )
{
   hb_errRT_BASE( EG_ARG, 2020, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* API wrappers */
HB_FUNC( MYSQL_REAL_CONNECT )
{
   /* MYSQL * mysql_real_connect( MYSQL *, char * host, char * user, char * password, char * db, uint port, char *, uint flags ) */
   const char     *szHost = hb_parc( 1 );
   const char     *szUser = hb_parc( 2 );
   const char     *szPass = hb_parc( 3 );
   unsigned int   port = ( unsigned int ) hb_parni( 4 );
   unsigned int   flags = ( unsigned int ) hb_parni( 5 );

#if MYSQL_VERSION_ID > 32200
   MYSQL          *pMySQL = mysql_init( NULL );
   if( pMySQL )
   {
      PHB_ITEM pSSL = hb_param( 6, HB_IT_HASH );
      if( pSSL )
      {
         flags |= CLIENT_SSL;
         mysql_ssl_set
         (
            pMySQL,
            hb_itemGetCPtr( hb_hashGetCItemPtr( pSSL, "key" ) ),
            hb_itemGetCPtr( hb_hashGetCItemPtr( pSSL, "cert" ) ),
            hb_itemGetCPtr( hb_hashGetCItemPtr( pSSL, "ca" ) ),
            hb_itemGetCPtr( hb_hashGetCItemPtr( pSSL, "capath" ) ),
            hb_itemGetCPtr( hb_hashGetCItemPtr( pSSL, "cipher" ) )
         );
      }

      if( mysql_real_connect( pMySQL, szHost, szUser, szPass, NULL, port, NULL, flags ) )
      {
         hb_MYSQL_ret( pMySQL );
      }
      else
      {
         mysql_close( pMySQL );
         hb_retptr( NULL );
      }
   }
   else
   {
      hb_retptr( NULL );
   }

#else
   hb_MYSQL_ret( mysql_real_connect( NULL, szHost, szUser, szPass, port, NULL, flags ) );
#endif
}

HB_FUNC( MYSQL_GET_SERVER_VERSION )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( ( long ) mysql_get_server_version( pMySQL ) );
#else
      const char  *szVer = mysql_get_server_info( pMySQL );
      long        lVer = 0;
      while( *szVer )
      {
         if( *szVer >= '0' && *szVer <= '9' )
         {
            lVer = lVer * 10 + ( *szVer - '0' );
         }

         ++szVer;
      }

      hb_retnl( lVer );
#endif
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_GET_SSL_CIPHER )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retc( mysql_get_ssl_cipher( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_COMMIT )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( ( long ) mysql_commit( pMySQL ) );
#else
      hb_retnl( ( long ) mysql_query( pMySQL, "COMMIT" ) );
#endif
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_ROLLBACK )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
#if MYSQL_VERSION_ID >= 40100
      hb_retnl( ( long ) mysql_rollback( pMySQL ) );
#else
      hb_retnl( ( long ) mysql_query( pMySQL, "ROLLBACK" ) );
#endif
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_SELECT_DB )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnl( ( long ) mysql_select_db( pMySQL, hb_parc( 2 ) ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_QUERY )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnl( ( long ) mysql_query( pMySQL, hb_parc( 2 ) ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_STORE_RESULT )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_MYSQL_RES_ret( mysql_store_result( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_USE_RESULT )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_MYSQL_RES_ret( mysql_use_result( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_NEXT_RESULT )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retl( mysql_next_result( pMySQL ) != 0 );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_MORE_RESULTS )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retl( mysql_more_results( pMySQL ) != 0 );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_FETCH_ROW )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      unsigned int   num_fields = mysql_num_fields( pResult );
      PHB_ITEM       aRow = hb_itemArrayNew( num_fields );
      MYSQL_ROW      pRow = mysql_fetch_row( pResult );

      if( pRow )
      {
         unsigned long  *lengths = mysql_fetch_lengths( pResult );
         unsigned int   i;
         for( i = 0; i < num_fields; ++i )
         {
            hb_arraySetCL( aRow, i + 1, pRow[i], lengths[i] );
         }
      }

      hb_itemReturnRelease( aRow );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_DATA_SEEK )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      mysql_data_seek( pResult, ( unsigned int ) hb_parni( 2 ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_NUM_ROWS )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      hb_retnint( mysql_num_rows( pResult ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_FETCH_FIELD )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      /* NOTE: field structure of MySQL has 8 members as of MySQL 3.22.x */
      PHB_ITEM    aField = hb_itemArrayNew( 8 );
      MYSQL_FIELD *pField = mysql_fetch_field( pResult );

      if( pField )
      {
         hb_arraySetC( aField, 1, pField->name );
         hb_arraySetC( aField, 2, pField->table );
         hb_arraySetC( aField, 3, pField->def );
         hb_arraySetNL( aField, 4, ( long ) pField->type );
         hb_arraySetNL( aField, 5, ( long ) pField->length );
         hb_arraySetNL( aField, 6, ( long ) pField->max_length );
         hb_arraySetNL( aField, 7, ( long ) pField->flags );
         hb_arraySetNL( aField, 8, ( long ) pField->decimals );
      }

      hb_itemReturnRelease( aField );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_FIELD_SEEK )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      mysql_field_seek( pResult, ( MYSQL_FIELD_OFFSET ) hb_parni( 2 ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_NUM_FIELDS )
{
   MYSQL_RES   *pResult = hb_MYSQL_RES_par( 1 );
   if( pResult )
   {
      hb_retnl( ( long ) mysql_num_fields( pResult ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_FIELD_COUNT )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
#if MYSQL_VERSION_ID > 32200
      hb_retnl( ( long ) mysql_field_count( pMySQL ) );
#else
      hb_retnl( 0 );
#endif
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_LIST_FIELDS )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_MYSQL_RES_ret( mysql_list_fields( pMySQL, hb_parc( 2 ), NULL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_ERRNO )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnint( mysql_errno( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_ERROR )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retc( mysql_error( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_LIST_DBS )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      MYSQL_RES   *pResult = mysql_list_dbs( pMySQL, NULL );
      if( pResult )
      {
         HB_SIZE  nr = ( HB_SIZE ) mysql_num_rows( pResult );
         PHB_ITEM aDBs = hb_itemArrayNew( nr );
         HB_SIZE  i;

         for( i = 0; i < nr; ++i )
         {
            MYSQL_ROW   pRow = mysql_fetch_row( pResult );
            if( pRow )
            {
               hb_arraySetC( aDBs, i + 1, pRow[0] );
            }
         }

         mysql_free_result( pResult );
         hb_itemReturnRelease( aDBs );
      }
      else
      {
         hb_itemReturnRelease( hb_itemArrayNew( 0 ) );
      }
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_LIST_TABLES )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      const char  *szWild = hb_parc( 2 );
      MYSQL_RES   *pResult = mysql_list_tables( pMySQL, szWild );
      if( pResult )
      {
         long     nr = ( long ) mysql_num_rows( pResult );
         PHB_ITEM aTables = hb_itemArrayNew( nr );
         long     i;

         for( i = 0; i < nr; ++i )
         {
            MYSQL_ROW   pRow = mysql_fetch_row( pResult );
            if( pRow )
            {
               hb_arraySetC( aTables, i + 1, pRow[0] );
            }
         }

         mysql_free_result( pResult );
         hb_itemReturnRelease( aTables );
      }
      else
      {
         hb_itemReturnRelease( hb_itemArrayNew( 0 ) );
      }
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_AFFECTED_ROWS )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnl( ( long ) mysql_affected_rows( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_GET_HOST_INFO )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retc( mysql_get_host_info( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_GET_SERVER_INFO )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retc( mysql_get_server_info( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_INSERT_ID )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnint( mysql_insert_id( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_PING )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      hb_retnint( mysql_ping( pMySQL ) );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_REAL_ESCAPE_STRING )
{
   MYSQL *pMySQL = hb_MYSQL_par( 1 );
   if( pMySQL )
   {
      const char     *from = hb_parcx( 2 );
      unsigned long  nSize = ( unsigned long ) hb_parclen( 2 );
      char           *buffer = ( char * ) hb_xgrab( nSize * 2 + 1 );

      nSize = mysql_real_escape_string( pMySQL, buffer, from, nSize );
      hb_retclen_buffer( buffer, nSize );
   }
   else
   {
      hb_mysqlError();
   }
}

HB_FUNC( MYSQL_ESCAPE_STRING )
{
   const char     *from = hb_parcx( 1 );
   unsigned long  nSize = ( unsigned long ) hb_parclen( 1 );
   char           *buffer = ( char * ) hb_xgrab( nSize * 2 + 1 );

   nSize = mysql_escape_string( buffer, from, nSize );
   hb_retclen_buffer( buffer, nSize );
}

HB_FUNC( MYSQL_ESCAPE_STRING_FROM_FILE )
{
   HB_SIZE  nSize;
   char     *from = ( char * ) hb_fileLoad( hb_parcx( 1 ), ( ULONG_MAX / 2 ) - 1, &nSize );

   if( from )
   {
      char  *buffer = ( char * ) hb_xgrab( nSize * 2 + 1 );
      nSize = mysql_escape_string( buffer, from, ( unsigned long ) nSize );
      hb_retclen_buffer( buffer, nSize );
      hb_xfree( from );
   }
   else
   {
      hb_retc( "" );
   }
}

HB_FUNC( MYSQL_GET_CLIENT_INFO )
{
   hb_retc( mysql_get_client_info() );
}

HB_FUNC( MYSQL_GET_CLIENT_VERSION )
{
#if MYSQL_VERSION_ID >= 40101
   hb_retnl( mysql_get_client_version() );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( MYSQL_VERSION_ID )
{
   hb_retnl( MYSQL_VERSION_ID );
}
