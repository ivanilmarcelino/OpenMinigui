/*
   MiniPrint2 Localization module
*/

#include "hmg.ch"

*------------------------------------------------------------------------------*
PROCEDURE _hmg_printer_InitUserMessages
*------------------------------------------------------------------------------*
#ifdef _MULTILINGUAL_
   LOCAL cLang
#endif
   PUBLIC _HMG_MINIPRINT[ 27 ]

   _hmg_printer_name := ""
   _hmg_printer_JobId := 0
   _hmg_printer_usercopies := .F.
   _hmg_printer_usercollate := .F.

   _hmg_printer_usermessages := Array( 30 )

   _hmg_printer_usermessages [ 01 ] := 'Page'
   _hmg_printer_usermessages [ 02 ] := 'Print Preview'
   _hmg_printer_usermessages [ 03 ] := 'First Page [HOME]'
   _hmg_printer_usermessages [ 04 ] := 'Previous Page [PGUP]'
   _hmg_printer_usermessages [ 05 ] := 'Next Page [PGDN]'
   _hmg_printer_usermessages [ 06 ] := 'Last Page [END]'
   _hmg_printer_usermessages [ 07 ] := 'Go To Page'
   _hmg_printer_usermessages [ 08 ] := 'Zoom'
   _hmg_printer_usermessages [ 09 ] := 'Print:'
   _hmg_printer_usermessages [ 10 ] := 'Page Number'
   _hmg_printer_usermessages [ 11 ] := 'Ok'
   _hmg_printer_usermessages [ 12 ] := 'Cancel'
   _hmg_printer_usermessages [ 13 ] := 'Select Printer'
   _hmg_printer_usermessages [ 14 ] := 'Collate Copies'
   _hmg_printer_usermessages [ 15 ] := 'Print Range'
   _hmg_printer_usermessages [ 16 ] := 'All'
   _hmg_printer_usermessages [ 17 ] := 'Pages'
   _hmg_printer_usermessages [ 18 ] := 'From:'
   _hmg_printer_usermessages [ 19 ] := 'To:'
   _hmg_printer_usermessages [ 20 ] := 'Copies:'
   _hmg_printer_usermessages [ 21 ] := 'All Range'
   _hmg_printer_usermessages [ 22 ] := 'Odd Pages Only'
   _hmg_printer_usermessages [ 23 ] := 'Even Pages Only'
   _hmg_printer_usermessages [ 24 ] := 'Yes'
   _hmg_printer_usermessages [ 25 ] := 'No'
   _hmg_printer_usermessages [ 26 ] := 'Close'
   _hmg_printer_usermessages [ 27 ] := 'Save as PDF'
   _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
   _hmg_printer_usermessages [ 29 ] := 'Generating Thumbnails... Please Wait...'
   _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

#ifdef _MULTILINGUAL_

   cLang := Upper( Left( SET ( _SET_LANGUAGE ), 2 ) )

// LANGUAGE IS NOT SUPPORTED BY hb_langSelect() FUNCTION
   IF _HMG_LANG_ID == 'FI'      // FINNISH
      cLang := 'FI'
   ENDIF
   
   DO CASE

   CASE cLang == "CS"
/////////////////////////////////////////////////////////////
// CZECH
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Strana'
      _hmg_printer_usermessages [ 02 ] := 'N�hled'
      _hmg_printer_usermessages [ 03 ] := 'Prvn� strana [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'P�edchoz� strana [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Dal�� strana [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Posledn� strana [END]'
      _hmg_printer_usermessages [ 07 ] := 'Jdi na stranu'
      _hmg_printer_usermessages [ 08 ] := 'Lupa'
      _hmg_printer_usermessages [ 09 ] := 'Tisk'
      _hmg_printer_usermessages [ 10 ] := '��slo strany'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Storno'
      _hmg_printer_usermessages [ 13 ] := 'Vyber tisk�rnu'
      _hmg_printer_usermessages [ 14 ] := 'T��d�n�'
      _hmg_printer_usermessages [ 15 ] := 'Rozsah tisku'
      _hmg_printer_usermessages [ 16 ] := 'v�e'
      _hmg_printer_usermessages [ 17 ] := 'strany'
      _hmg_printer_usermessages [ 18 ] := 'od'
      _hmg_printer_usermessages [ 19 ] := 'do'
      _hmg_printer_usermessages [ 20 ] := 'kopi�'
      _hmg_printer_usermessages [ 21 ] := 'v�echny strany'
      _hmg_printer_usermessages [ 22 ] := 'lich� strany'
      _hmg_printer_usermessages [ 23 ] := 'sud� strany'
      _hmg_printer_usermessages [ 24 ] := 'Ano'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Zav�i'
      _hmg_printer_usermessages [ 27 ] := 'Ulo�'
      _hmg_printer_usermessages [ 28 ] := 'Miniatury'
      _hmg_printer_usermessages [ 29 ] := 'Generuji miniatury... �ekejte, pros�m...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "HR"   // Croatian
/////////////////////////////////////////////////////////////
// CROATIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Page'
      _hmg_printer_usermessages [ 02 ] := 'Print Preview'
      _hmg_printer_usermessages [ 03 ] := 'First Page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Previous Page [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Next Page [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Last Page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Go To Page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Print'
      _hmg_printer_usermessages [ 10 ] := 'Page Number'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancel'
      _hmg_printer_usermessages [ 13 ] := 'Select Printer'
      _hmg_printer_usermessages [ 14 ] := 'Collate Copies'
      _hmg_printer_usermessages [ 15 ] := 'Print Range'
      _hmg_printer_usermessages [ 16 ] := 'All'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'From'
      _hmg_printer_usermessages [ 19 ] := 'To'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'All Range'
      _hmg_printer_usermessages [ 22 ] := 'Odd Pages Only'
      _hmg_printer_usermessages [ 23 ] := 'Even Pages Only'
      _hmg_printer_usermessages [ 24 ] := 'Yes'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Close'
      _hmg_printer_usermessages [ 27 ] := 'Save'
      _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
      _hmg_printer_usermessages [ 29 ] := 'Generating Thumbnails... Please Wait...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "EU"   // Basque.
/////////////////////////////////////////////////////////////
// BASQUE
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Page'
      _hmg_printer_usermessages [ 02 ] := 'Print Preview'
      _hmg_printer_usermessages [ 03 ] := 'First Page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Previous Page [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Next Page [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Last Page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Go To Page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Print'
      _hmg_printer_usermessages [ 10 ] := 'Page Number'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancel'
      _hmg_printer_usermessages [ 13 ] := 'Select Printer'
      _hmg_printer_usermessages [ 14 ] := 'Collate Copies'
      _hmg_printer_usermessages [ 15 ] := 'Print Range'
      _hmg_printer_usermessages [ 16 ] := 'All'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'From'
      _hmg_printer_usermessages [ 19 ] := 'To'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'All Range'
      _hmg_printer_usermessages [ 22 ] := 'Odd Pages Only'
      _hmg_printer_usermessages [ 23 ] := 'Even Pages Only'
      _hmg_printer_usermessages [ 24 ] := 'Yes'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Close'
      _hmg_printer_usermessages [ 27 ] := 'Save'
      _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
      _hmg_printer_usermessages [ 29 ] := 'Generating Thumbnails... Please Wait...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "FR"   // French
/////////////////////////////////////////////////////////////
// FRENCH
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Page'
      _hmg_printer_usermessages [ 02 ] := "Aper�u avant impression"
      _hmg_printer_usermessages [ 03 ] := 'Premi�re page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Page pr�c�dente [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Page suivante [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Derni�re page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Allez page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Imprimer'
      _hmg_printer_usermessages [ 10 ] := 'Page'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Annulation'
      _hmg_printer_usermessages [ 13 ] := "S�lection de l'imprimante"
      _hmg_printer_usermessages [ 14 ] := "Assemblez"
      _hmg_printer_usermessages [ 15 ] := "Param�tres d'impression"
      _hmg_printer_usermessages [ 16 ] := 'Tous'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'De'
      _hmg_printer_usermessages [ 19 ] := '�'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'Toutes les pages'
      _hmg_printer_usermessages [ 22 ] := 'Pages Impaires'
      _hmg_printer_usermessages [ 23 ] := 'Pages Paires'
      _hmg_printer_usermessages [ 24 ] := 'Oui'
      _hmg_printer_usermessages [ 25 ] := 'Non'
      _hmg_printer_usermessages [ 26 ] := 'Fermer'
      _hmg_printer_usermessages [ 27 ] := 'Sauver'
      _hmg_printer_usermessages [ 28 ] := 'Affichettes'
      _hmg_printer_usermessages [ 29 ] := "Cr�ation des affichettes... Merci d'attendre..."
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "DE"   // German
/////////////////////////////////////////////////////////////
// GERMAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Seite'
      _hmg_printer_usermessages [ 02 ] := 'Druck Vorschau'
      _hmg_printer_usermessages [ 03 ] := 'Erste Seite [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Vorherige Seite [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'N�chste Seite [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Letzte Seite [END]'
      _hmg_printer_usermessages [ 07 ] := 'Gehe zur Seite '
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Drucken'
      _hmg_printer_usermessages [ 10 ] := 'Seite Nummer'
      _hmg_printer_usermessages [ 11 ] := 'Okay'
      _hmg_printer_usermessages [ 12 ] := 'Abbruch'
      _hmg_printer_usermessages [ 13 ] := 'Drucker w�hlen'
      _hmg_printer_usermessages [ 14 ] := 'Sortieren'
      _hmg_printer_usermessages [ 15 ] := 'Druckbereich Auswahl'
      _hmg_printer_usermessages [ 16 ] := 'Alle Seiten'
      _hmg_printer_usermessages [ 17 ] := 'Seiten'
      _hmg_printer_usermessages [ 18 ] := 'von'
      _hmg_printer_usermessages [ 19 ] := 'bis'
      _hmg_printer_usermessages [ 20 ] := 'Kopien'
      _hmg_printer_usermessages [ 21 ] := 'Alle Seiten'
      _hmg_printer_usermessages [ 22 ] := 'Nur ungerade Seiten'
      _hmg_printer_usermessages [ 23 ] := 'Nur gerade Seiten'
      _hmg_printer_usermessages [ 24 ] := 'Ja'
      _hmg_printer_usermessages [ 25 ] := 'Nein'
      _hmg_printer_usermessages [ 26 ] := 'Beenden'
      _hmg_printer_usermessages [ 27 ] := 'Speichern'
      _hmg_printer_usermessages [ 28 ] := 'Seitenvorschau'
      _hmg_printer_usermessages [ 29 ] := 'Erzeuge Vorschau...  Bitte warten...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "IT"   // Italian
/////////////////////////////////////////////////////////////
// ITALIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Pagina'
      _hmg_printer_usermessages [ 02 ] := 'Anteprima di stampa'
      _hmg_printer_usermessages [ 03 ] := 'Prima Pagina [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Pagina Precedente [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Pagina Seguente [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ultima Pagina [END]'
      _hmg_printer_usermessages [ 07 ] := 'Vai Alla Pagina'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Stampa'
      _hmg_printer_usermessages [ 10 ] := 'Pagina'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Annulla'
      _hmg_printer_usermessages [ 13 ] := 'Selezioni Lo Stampatore'
      _hmg_printer_usermessages [ 14 ] := 'Fascicoli'
      _hmg_printer_usermessages [ 15 ] := 'Intervallo di stampa'
      _hmg_printer_usermessages [ 16 ] := 'Tutti'
      _hmg_printer_usermessages [ 17 ] := 'Pagine'
      _hmg_printer_usermessages [ 18 ] := 'Da'
      _hmg_printer_usermessages [ 19 ] := 'A'
      _hmg_printer_usermessages [ 20 ] := 'Copie'
      _hmg_printer_usermessages [ 21 ] := 'Tutte le pagine'
      _hmg_printer_usermessages [ 22 ] := 'Le Pagine Pari'
      _hmg_printer_usermessages [ 23 ] := 'Le Pagine Dispari'
      _hmg_printer_usermessages [ 24 ] := 'Si'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Chiudi'
      _hmg_printer_usermessages [ 27 ] := 'Salva'
      _hmg_printer_usermessages [ 28 ] := 'Miniatura'
      _hmg_printer_usermessages [ 29 ] := 'Generando Miniatura...  Prego Attesa...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "PL"   // Polish
/////////////////////////////////////////////////////////////
// POLISH
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Strona'
      _hmg_printer_usermessages [ 02 ] := 'Podgl�d wydruku'
      _hmg_printer_usermessages [ 03 ] := 'Pierwsza strona [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Poprzednia strona [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Nast�pna strona [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ostatnia strona [END]'
      _hmg_printer_usermessages [ 07 ] := 'Skocz do strony'
      _hmg_printer_usermessages [ 08 ] := 'Powi�ksz'
      _hmg_printer_usermessages [ 09 ] := 'Drukuj'
      _hmg_printer_usermessages [ 10 ] := 'Numer strony'
      _hmg_printer_usermessages [ 11 ] := 'Tak'
      _hmg_printer_usermessages [ 12 ] := 'Przerwij'
      _hmg_printer_usermessages [ 13 ] := 'Wybierz drukark�'
      _hmg_printer_usermessages [ 14 ] := 'Sortuj kopie'
      _hmg_printer_usermessages [ 15 ] := 'Zakres wydruku'
      _hmg_printer_usermessages [ 16 ] := 'Wszystkie'
      _hmg_printer_usermessages [ 17 ] := 'Strony'
      _hmg_printer_usermessages [ 18 ] := 'Od'
      _hmg_printer_usermessages [ 19 ] := 'Do'
      _hmg_printer_usermessages [ 20 ] := 'Kopie'
      _hmg_printer_usermessages [ 21 ] := 'Wszystkie'
      _hmg_printer_usermessages [ 22 ] := 'Nieparzyste'
      _hmg_printer_usermessages [ 23 ] := 'Parzyste'
      _hmg_printer_usermessages [ 24 ] := 'Tak'
      _hmg_printer_usermessages [ 25 ] := 'Nie'
      _hmg_printer_usermessages [ 26 ] := 'Zamknij'
      _hmg_printer_usermessages [ 27 ] := 'Zapisz'
      _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
      _hmg_printer_usermessages [ 29 ] := 'Generuj� Thumbnails... Prosz� czeka�...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "PT"   // Portuguese
/////////////////////////////////////////////////////////////
// PORTUGUESE
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'P�gina'
      _hmg_printer_usermessages [ 02 ] := 'Visualiza��o da Impress�o'
      _hmg_printer_usermessages [ 03 ] := 'Primeira P�gina [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'P�gina Anterior [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'P�gina Seguinte [PGDN]'
      _hmg_printer_usermessages [ 06 ] := '�ltima P�gina [END]'
      _hmg_printer_usermessages [ 07 ] := 'Ir Para a P�gina N�'
      _hmg_printer_usermessages [ 08 ] := 'Ampliar'
      _hmg_printer_usermessages [ 09 ] := 'Imprimir'
      _hmg_printer_usermessages [ 10 ] := 'P�gina'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancelar'
      _hmg_printer_usermessages [ 13 ] := 'Selecione a Impressora'
      _hmg_printer_usermessages [ 14 ] := 'Ordenar C�pias'
      _hmg_printer_usermessages [ 15 ] := 'Intervalo de Impress�o'
      _hmg_printer_usermessages [ 16 ] := 'Tudo'
      _hmg_printer_usermessages [ 17 ] := 'P�ginas'
      _hmg_printer_usermessages [ 18 ] := 'De'
      _hmg_printer_usermessages [ 19 ] := 'At�'
      _hmg_printer_usermessages [ 20 ] := 'N� de C�pias'
      _hmg_printer_usermessages [ 21 ] := 'Todas as P�ginas'
      _hmg_printer_usermessages [ 22 ] := ' Somente P�ginas Impares'
      _hmg_printer_usermessages [ 23 ] := ' Somente P�ginas Pares'
      _hmg_printer_usermessages [ 24 ] := 'Sim'
      _hmg_printer_usermessages [ 25 ] := 'N�o'
      _hmg_printer_usermessages [ 26 ] := 'Fechar'
      _hmg_printer_usermessages [ 27 ] := 'Salvar'
      _hmg_printer_usermessages [ 28 ] := 'Miniaturas'
      _hmg_printer_usermessages [ 29 ] := 'Aguarde, Gerando Miniaturas...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "RU"   // Russian
/////////////////////////////////////////////////////////////
// RUSSIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := '��������'
      _hmg_printer_usermessages [ 02 ] := '��������������� ��������'
      _hmg_printer_usermessages [ 03 ] := '������ [HOME]'
      _hmg_printer_usermessages [ 04 ] := '���������� [PGUP]'
      _hmg_printer_usermessages [ 05 ] := '��������� [PGDN]'
      _hmg_printer_usermessages [ 06 ] := '��������� [END]'
      _hmg_printer_usermessages [ 07 ] := '������� �'
      _hmg_printer_usermessages [ 08 ] := '�������'
      _hmg_printer_usermessages [ 09 ] := '������'
      _hmg_printer_usermessages [ 10 ] := '�������� �'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := '������'
      _hmg_printer_usermessages [ 13 ] := '����� ��������'
      _hmg_printer_usermessages [ 14 ] := '���������� �����'
      _hmg_printer_usermessages [ 15 ] := '�������� ������'
      _hmg_printer_usermessages [ 16 ] := '���'
      _hmg_printer_usermessages [ 17 ] := '��������'
      _hmg_printer_usermessages [ 18 ] := '��'
      _hmg_printer_usermessages [ 19 ] := '��'
      _hmg_printer_usermessages [ 20 ] := '�����'
      _hmg_printer_usermessages [ 21 ] := '���� ��������'
      _hmg_printer_usermessages [ 22 ] := '�������� ������'
      _hmg_printer_usermessages [ 23 ] := '������ ������'
      _hmg_printer_usermessages [ 24 ] := '��'
      _hmg_printer_usermessages [ 25 ] := '���'
      _hmg_printer_usermessages [ 26 ] := '�������'
      _hmg_printer_usermessages [ 27 ] := '��������� ��� PDF'
      _hmg_printer_usermessages [ 28 ] := '���������'
      _hmg_printer_usermessages [ 29 ] := '�����, ��������� ���������...'
      _hmg_printer_usermessages [ 30 ] := '��������� �� ����� ��� PDF'

   CASE cLang == "UK" .OR. cLang == "UA"   // Ukrainian
/////////////////////////////////////////////////////////////
// UKRAINIAN
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := '�������'
      _hmg_printer_usermessages [ 02 ] := '��������� ��������'
      _hmg_printer_usermessages [ 03 ] := '����� [HOME]'
      _hmg_printer_usermessages [ 04 ] := '��������� [PGUP]'
      _hmg_printer_usermessages [ 05 ] := '�������� [PGDN]'
      _hmg_printer_usermessages [ 06 ] := '������� [END]'
      _hmg_printer_usermessages [ 07 ] := '������� ��'
      _hmg_printer_usermessages [ 08 ] := '�������'
      _hmg_printer_usermessages [ 09 ] := '����'
      _hmg_printer_usermessages [ 10 ] := '������� �'
      _hmg_printer_usermessages [ 11 ] := '������'
      _hmg_printer_usermessages [ 12 ] := '³�����'
      _hmg_printer_usermessages [ 13 ] := '���� ��������'
      _hmg_printer_usermessages [ 14 ] := '���������� ����'
      _hmg_printer_usermessages [ 15 ] := '�������� �����'
      _hmg_printer_usermessages [ 16 ] := '��'
      _hmg_printer_usermessages [ 17 ] := '�������'
      _hmg_printer_usermessages [ 18 ] := '�'
      _hmg_printer_usermessages [ 19 ] := '��'
      _hmg_printer_usermessages [ 20 ] := '����'
      _hmg_printer_usermessages [ 21 ] := '���� ��������'
      _hmg_printer_usermessages [ 22 ] := 'ҳ���� ������'
      _hmg_printer_usermessages [ 23 ] := 'ҳ���� ����'
      _hmg_printer_usermessages [ 24 ] := '���'
      _hmg_printer_usermessages [ 25 ] := 'ͳ'
      _hmg_printer_usermessages [ 26 ] := '�������'
      _hmg_printer_usermessages [ 27 ] := '��������'
      _hmg_printer_usermessages [ 28 ] := '̳������'
      _hmg_printer_usermessages [ 29 ] := '��������, ������� �������...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "ES"   // Spanish
/////////////////////////////////////////////////////////////
// SPANISH
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'P�gina'
      _hmg_printer_usermessages [ 02 ] := 'Vista Previa'
      _hmg_printer_usermessages [ 03 ] := 'Inicio [INICIO]'
      _hmg_printer_usermessages [ 04 ] := 'Anterior [REPAG]'
      _hmg_printer_usermessages [ 05 ] := 'Siguiente [AVPAG]'
      _hmg_printer_usermessages [ 06 ] := 'Fin [FIN]'
      _hmg_printer_usermessages [ 07 ] := 'Ir a'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Imprimir'
      _hmg_printer_usermessages [ 10 ] := 'P�gina Nro.'
      _hmg_printer_usermessages [ 11 ] := 'Aceptar'
      _hmg_printer_usermessages [ 12 ] := 'Cancelar'
      _hmg_printer_usermessages [ 13 ] := 'Seleccionar Impresora'
      _hmg_printer_usermessages [ 14 ] := 'Ordenar Copias'
      _hmg_printer_usermessages [ 15 ] := 'Rango de Impresi�n'
      _hmg_printer_usermessages [ 16 ] := 'Todo'
      _hmg_printer_usermessages [ 17 ] := 'P�ginas'
      _hmg_printer_usermessages [ 18 ] := 'Desde'
      _hmg_printer_usermessages [ 19 ] := 'Hasta'
      _hmg_printer_usermessages [ 20 ] := 'Copias'
      _hmg_printer_usermessages [ 21 ] := 'Todo El Rango'
      _hmg_printer_usermessages [ 22 ] := 'Solo P�ginas Impares'
      _hmg_printer_usermessages [ 23 ] := 'Solo P�ginas Pares'
      _hmg_printer_usermessages [ 24 ] := 'Si'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Cerrar'
      _hmg_printer_usermessages [ 27 ] := 'Guardar'
      _hmg_printer_usermessages [ 28 ] := 'Miniaturas'
      _hmg_printer_usermessages [ 29 ] := 'Generando Miniaturas... Espere Por Favor...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "FI"   // Finnish
///////////////////////////////////////////////////////////////////////
// FINNISH
///////////////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Page'
      _hmg_printer_usermessages [ 02 ] := 'Print Preview'
      _hmg_printer_usermessages [ 03 ] := 'First Page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Previous Page [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Next Page [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Last Page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Go To Page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Print'
      _hmg_printer_usermessages [ 10 ] := 'Page Number'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancel'
      _hmg_printer_usermessages [ 13 ] := 'Select Printer'
      _hmg_printer_usermessages [ 14 ] := 'Collate Copies'
      _hmg_printer_usermessages [ 15 ] := 'Print Range'
      _hmg_printer_usermessages [ 16 ] := 'All'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'From'
      _hmg_printer_usermessages [ 19 ] := 'To'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'All Range'
      _hmg_printer_usermessages [ 22 ] := 'Odd Pages Only'
      _hmg_printer_usermessages [ 23 ] := 'Even Pages Only'
      _hmg_printer_usermessages [ 24 ] := 'Yes'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Close'
      _hmg_printer_usermessages [ 27 ] := 'Save'
      _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
      _hmg_printer_usermessages [ 29 ] := 'Generating Thumbnails... Please Wait...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "NL"   // Dutch
/////////////////////////////////////////////////////////////
// DUTCH
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Page'
      _hmg_printer_usermessages [ 02 ] := 'Print Preview'
      _hmg_printer_usermessages [ 03 ] := 'First Page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Previous Page [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Next Page [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Last Page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Go To Page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Print'
      _hmg_printer_usermessages [ 10 ] := 'Page Number'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancel'
      _hmg_printer_usermessages [ 13 ] := 'Select Printer'
      _hmg_printer_usermessages [ 14 ] := 'Collate Copies'
      _hmg_printer_usermessages [ 15 ] := 'Print Range'
      _hmg_printer_usermessages [ 16 ] := 'All'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'From'
      _hmg_printer_usermessages [ 19 ] := 'To'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'All Range'
      _hmg_printer_usermessages [ 22 ] := 'Odd Pages Only'
      _hmg_printer_usermessages [ 23 ] := 'Even Pages Only'
      _hmg_printer_usermessages [ 24 ] := 'Yes'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Close'
      _hmg_printer_usermessages [ 27 ] := 'Save'
      _hmg_printer_usermessages [ 28 ] := 'Thumbnails'
      _hmg_printer_usermessages [ 29 ] := 'Generating Thumbnails... Please Wait...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "SL"   // Slovenian
/////////////////////////////////////////////////////////////
// SLOVENIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Stran'
      _hmg_printer_usermessages [ 02 ] := 'Predgled tiskanja'
      _hmg_printer_usermessages [ 03 ] := 'Prva stran [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Prej�nja stran [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Naslednja stran [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Zadnja stran [END]'
      _hmg_printer_usermessages [ 07 ] := 'Pojdi na stran'
      _hmg_printer_usermessages [ 08 ] := 'Pove�ava'
      _hmg_printer_usermessages [ 09 ] := 'Natisni'
      _hmg_printer_usermessages [ 10 ] := '�tevilka strani'
      _hmg_printer_usermessages [ 11 ] := 'V redu'
      _hmg_printer_usermessages [ 12 ] := 'Prekini'
      _hmg_printer_usermessages [ 13 ] := 'Izberi tiskalnik'
      _hmg_printer_usermessages [ 14 ] := 'Zbiranje kopij'
      _hmg_printer_usermessages [ 15 ] := 'Obseg tiskanja'
      _hmg_printer_usermessages [ 16 ] := 'Vse'
      _hmg_printer_usermessages [ 17 ] := 'Strani'
      _hmg_printer_usermessages [ 18 ] := 'od'
      _hmg_printer_usermessages [ 19 ] := 'do'
      _hmg_printer_usermessages [ 20 ] := 'Kopij'
      _hmg_printer_usermessages [ 21 ] := 'Ves obseg'
      _hmg_printer_usermessages [ 22 ] := 'Neparne strani'
      _hmg_printer_usermessages [ 23 ] := 'Parne strani'
      _hmg_printer_usermessages [ 24 ] := 'Ja'
      _hmg_printer_usermessages [ 25 ] := 'Ne'
      _hmg_printer_usermessages [ 26 ] := 'Zapri'
      _hmg_printer_usermessages [ 27 ] := 'Shrani'
      _hmg_printer_usermessages [ 28 ] := 'Sli�ice'
      _hmg_printer_usermessages [ 29 ] := 'Pripravljam sli�ice... prosim, po�akajte...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "SK"   // Slovak
/////////////////////////////////////////////////////////////
// SLOVAK
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Strana'
      _hmg_printer_usermessages [ 02 ] := 'N�h�ad'
      _hmg_printer_usermessages [ 03 ] := 'Prv� strana [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Predch�zaj�ca strana [PGUP]'
      _hmg_printer_usermessages [ 05 ] := '�al�ia strana [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Posledn� strana [END]'
      _hmg_printer_usermessages [ 07 ] := 'Uka� stranu'
      _hmg_printer_usermessages [ 08 ] := 'Lupa'
      _hmg_printer_usermessages [ 09 ] := 'Tla�'
      _hmg_printer_usermessages [ 10 ] := '��slo strany'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Storno'
      _hmg_printer_usermessages [ 13 ] := 'Vyberte tla�i�re�'
      _hmg_printer_usermessages [ 14 ] := 'Zoradenie'
      _hmg_printer_usermessages [ 15 ] := 'Rozsah tla�e'
      _hmg_printer_usermessages [ 16 ] := 'v�etko'
      _hmg_printer_usermessages [ 17 ] := 'strany'
      _hmg_printer_usermessages [ 18 ] := 'od'
      _hmg_printer_usermessages [ 19 ] := 'po'
      _hmg_printer_usermessages [ 20 ] := 'k�pi�'
      _hmg_printer_usermessages [ 21 ] := 'v�etky strany'
      _hmg_printer_usermessages [ 22 ] := 'nep�rn� strany'
      _hmg_printer_usermessages [ 23 ] := 'p�rn� strany'
      _hmg_printer_usermessages [ 24 ] := '�no'
      _hmg_printer_usermessages [ 25 ] := 'Nie'
      _hmg_printer_usermessages [ 26 ] := 'Zatvor'
      _hmg_printer_usermessages [ 27 ] := 'Ulo�'
      _hmg_printer_usermessages [ 28 ] := 'Miniatury'
      _hmg_printer_usermessages [ 29 ] := 'Generujem miniatury... �akajte, pros�m...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "HU"   // Hungarian
/////////////////////////////////////////////////////////////
// HUNGARIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Oldal'
      _hmg_printer_usermessages [ 02 ] := 'El�n�zet'
      _hmg_printer_usermessages [ 03 ] := 'Els� oldal [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'El�z� oldal [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'K�vetkez� oldal [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Utols� oldal [END]'
      _hmg_printer_usermessages [ 07 ] := 'Oldalt mutasd'
      _hmg_printer_usermessages [ 08 ] := 'Nagy�t�'
      _hmg_printer_usermessages [ 09 ] := 'Nyomtasd'
      _hmg_printer_usermessages [ 10 ] := 'Oldal sz�ma'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Elvet�s'
      _hmg_printer_usermessages [ 13 ] := 'V�lasszon nyomtat�t'
      _hmg_printer_usermessages [ 14 ] := 'Rendezve'
      _hmg_printer_usermessages [ 15 ] := 'Oldalak nyomtat�sa'
      _hmg_printer_usermessages [ 16 ] := 'mindent'
      _hmg_printer_usermessages [ 17 ] := 'Oldalakat'
      _hmg_printer_usermessages [ 18 ] := 'ett�l'
      _hmg_printer_usermessages [ 19 ] := 'eddig'
      _hmg_printer_usermessages [ 20 ] := 'm�solat'
      _hmg_printer_usermessages [ 21 ] := 'Minden oldalt'
      _hmg_printer_usermessages [ 22 ] := 'p�ros oldalakat'
      _hmg_printer_usermessages [ 23 ] := 'p�ratlan oldalakat'
      _hmg_printer_usermessages [ 24 ] := 'Igen'
      _hmg_printer_usermessages [ 25 ] := 'Nem'
      _hmg_printer_usermessages [ 26 ] := 'Z�rd be'
      _hmg_printer_usermessages [ 27 ] := 'Mentsd'
      _hmg_printer_usermessages [ 28 ] := 'Miniat�r�k'
      _hmg_printer_usermessages [ 29 ] := 'Miniat�r�k l�trehoz�sa... K�rem, v�rjon...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "BG"   // Bulgarian
/////////////////////////////////////////////////////////////
// BULGARIAN
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := '��������'
      _hmg_printer_usermessages [ 02 ] := '������������� �������'
      _hmg_printer_usermessages [ 03 ] := '����� [HOME]'
      _hmg_printer_usermessages [ 04 ] := '��������� [PGUP]'
      _hmg_printer_usermessages [ 05 ] := '������� [PGDN]'
      _hmg_printer_usermessages [ 06 ] := '�������� [END]'
      _hmg_printer_usermessages [ 07 ] := '��� ��'
      _hmg_printer_usermessages [ 08 ] := '�����'
      _hmg_printer_usermessages [ 09 ] := '�����'
      _hmg_printer_usermessages [ 10 ] := '�������� �'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := '������'
      _hmg_printer_usermessages [ 13 ] := '����� �� �������'
      _hmg_printer_usermessages [ 14 ] := '���������� �� �������'
      _hmg_printer_usermessages [ 15 ] := '�������� �� �����'
      _hmg_printer_usermessages [ 16 ] := '������'
      _hmg_printer_usermessages [ 17 ] := '��������'
      _hmg_printer_usermessages [ 18 ] := '��'
      _hmg_printer_usermessages [ 19 ] := '��'
      _hmg_printer_usermessages [ 20 ] := '�����'
      _hmg_printer_usermessages [ 21 ] := '����� ��������'
      _hmg_printer_usermessages [ 22 ] := '���� ���������'
      _hmg_printer_usermessages [ 23 ] := '���� �������'
      _hmg_printer_usermessages [ 24 ] := '��'
      _hmg_printer_usermessages [ 25 ] := '��'
      _hmg_printer_usermessages [ 26 ] := '�������'
      _hmg_printer_usermessages [ 27 ] := '�������'
      _hmg_printer_usermessages [ 28 ] := '���������'
      _hmg_printer_usermessages [ 29 ] := '���������, ��������� ���������...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "EL"   // Greek - Ellinika
/////////////////////////////////////////////////////////////
// GREEK - ��������
/////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := '������'
      _hmg_printer_usermessages [ 02 ] := '�������������'
      _hmg_printer_usermessages [ 03 ] := '1� ������ [HOME]'
      _hmg_printer_usermessages [ 04 ] := '����������� ������ [PGUP]'
      _hmg_printer_usermessages [ 05 ] := '������� ������ [PGDN]'
      _hmg_printer_usermessages [ 06 ] := '��������� ������ [END]'
      _hmg_printer_usermessages [ 07 ] := '������'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := '��������'
      _hmg_printer_usermessages [ 10 ] := '���� �������'
      _hmg_printer_usermessages [ 11 ] := '�������'
      _hmg_printer_usermessages [ 12 ] := '������'
      _hmg_printer_usermessages [ 13 ] := '������� ��������'
      _hmg_printer_usermessages [ 14 ] := '������� ����������'
      _hmg_printer_usermessages [ 15 ] := '������� ���������'
      _hmg_printer_usermessages [ 16 ] := '���'
      _hmg_printer_usermessages [ 17 ] := '�������'
      _hmg_printer_usermessages [ 18 ] := '���'
      _hmg_printer_usermessages [ 19 ] := '���'
      _hmg_printer_usermessages [ 20 ] := '���������'
      _hmg_printer_usermessages [ 21 ] := '���� �� �������'
      _hmg_printer_usermessages [ 22 ] := '����� �������'
      _hmg_printer_usermessages [ 23 ] := '����� �������'
      _hmg_printer_usermessages [ 24 ] := '���'
      _hmg_printer_usermessages [ 25 ] := '���'
      _hmg_printer_usermessages [ 26 ] := '������'
      _hmg_printer_usermessages [ 27 ] := '����������'
      _hmg_printer_usermessages [ 28 ] := '������������'
      _hmg_printer_usermessages [ 29 ] := '���������� ������������... ����������...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   ENDCASE

#endif

   RETURN