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
      _hmg_printer_usermessages [ 02 ] := 'Náhled'
      _hmg_printer_usermessages [ 03 ] := 'První strana [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Pøedchozí strana [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Další strana [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Poslední strana [END]'
      _hmg_printer_usermessages [ 07 ] := 'Jdi na stranu'
      _hmg_printer_usermessages [ 08 ] := 'Lupa'
      _hmg_printer_usermessages [ 09 ] := 'Tisk'
      _hmg_printer_usermessages [ 10 ] := 'Èíslo strany'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Storno'
      _hmg_printer_usermessages [ 13 ] := 'Vyber tiskárnu'
      _hmg_printer_usermessages [ 14 ] := 'Tøídìní'
      _hmg_printer_usermessages [ 15 ] := 'Rozsah tisku'
      _hmg_printer_usermessages [ 16 ] := 'vše'
      _hmg_printer_usermessages [ 17 ] := 'strany'
      _hmg_printer_usermessages [ 18 ] := 'od'
      _hmg_printer_usermessages [ 19 ] := 'do'
      _hmg_printer_usermessages [ 20 ] := 'kopií'
      _hmg_printer_usermessages [ 21 ] := 'všechny strany'
      _hmg_printer_usermessages [ 22 ] := 'liché strany'
      _hmg_printer_usermessages [ 23 ] := 'sudé strany'
      _hmg_printer_usermessages [ 24 ] := 'Ano'
      _hmg_printer_usermessages [ 25 ] := 'No'
      _hmg_printer_usermessages [ 26 ] := 'Zavøi'
      _hmg_printer_usermessages [ 27 ] := 'Ulo'
      _hmg_printer_usermessages [ 28 ] := 'Miniatury'
      _hmg_printer_usermessages [ 29 ] := 'Generuji miniatury... Èekejte, prosím...'
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
      _hmg_printer_usermessages [ 02 ] := "Aperçu avant impression"
      _hmg_printer_usermessages [ 03 ] := 'Première page [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Page précédente [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Page suivante [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Dernière page [END]'
      _hmg_printer_usermessages [ 07 ] := 'Allez page'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Imprimer'
      _hmg_printer_usermessages [ 10 ] := 'Page'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Annulation'
      _hmg_printer_usermessages [ 13 ] := "Sélection de l'imprimante"
      _hmg_printer_usermessages [ 14 ] := "Assemblez"
      _hmg_printer_usermessages [ 15 ] := "Paramètres d'impression"
      _hmg_printer_usermessages [ 16 ] := 'Tous'
      _hmg_printer_usermessages [ 17 ] := 'Pages'
      _hmg_printer_usermessages [ 18 ] := 'De'
      _hmg_printer_usermessages [ 19 ] := 'À'
      _hmg_printer_usermessages [ 20 ] := 'Copies'
      _hmg_printer_usermessages [ 21 ] := 'Toutes les pages'
      _hmg_printer_usermessages [ 22 ] := 'Pages Impaires'
      _hmg_printer_usermessages [ 23 ] := 'Pages Paires'
      _hmg_printer_usermessages [ 24 ] := 'Oui'
      _hmg_printer_usermessages [ 25 ] := 'Non'
      _hmg_printer_usermessages [ 26 ] := 'Fermer'
      _hmg_printer_usermessages [ 27 ] := 'Sauver'
      _hmg_printer_usermessages [ 28 ] := 'Affichettes'
      _hmg_printer_usermessages [ 29 ] := "Création des affichettes... Merci d'attendre..."
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "DE"   // German
/////////////////////////////////////////////////////////////
// GERMAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Seite'
      _hmg_printer_usermessages [ 02 ] := 'Druck Vorschau'
      _hmg_printer_usermessages [ 03 ] := 'Erste Seite [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Vorherige Seite [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Nächste Seite [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Letzte Seite [END]'
      _hmg_printer_usermessages [ 07 ] := 'Gehe zur Seite '
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Drucken'
      _hmg_printer_usermessages [ 10 ] := 'Seite Nummer'
      _hmg_printer_usermessages [ 11 ] := 'Okay'
      _hmg_printer_usermessages [ 12 ] := 'Abbruch'
      _hmg_printer_usermessages [ 13 ] := 'Drucker wählen'
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
      _hmg_printer_usermessages [ 02 ] := 'Podgl¹d wydruku'
      _hmg_printer_usermessages [ 03 ] := 'Pierwsza strona [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Poprzednia strona [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Nastêpna strona [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ostatnia strona [END]'
      _hmg_printer_usermessages [ 07 ] := 'Skocz do strony'
      _hmg_printer_usermessages [ 08 ] := 'Powiêksz'
      _hmg_printer_usermessages [ 09 ] := 'Drukuj'
      _hmg_printer_usermessages [ 10 ] := 'Numer strony'
      _hmg_printer_usermessages [ 11 ] := 'Tak'
      _hmg_printer_usermessages [ 12 ] := 'Przerwij'
      _hmg_printer_usermessages [ 13 ] := 'Wybierz drukarkê'
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
      _hmg_printer_usermessages [ 29 ] := 'Generujê Thumbnails... Proszê czekaæ...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "PT"   // Portuguese
/////////////////////////////////////////////////////////////
// PORTUGUESE
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Página'
      _hmg_printer_usermessages [ 02 ] := 'Visualização da Impressão'
      _hmg_printer_usermessages [ 03 ] := 'Primeira Página [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Página Anterior [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Página Seguinte [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Última Página [END]'
      _hmg_printer_usermessages [ 07 ] := 'Ir Para a Página Nº'
      _hmg_printer_usermessages [ 08 ] := 'Ampliar'
      _hmg_printer_usermessages [ 09 ] := 'Imprimir'
      _hmg_printer_usermessages [ 10 ] := 'Página'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Cancelar'
      _hmg_printer_usermessages [ 13 ] := 'Selecione a Impressora'
      _hmg_printer_usermessages [ 14 ] := 'Ordenar Cópias'
      _hmg_printer_usermessages [ 15 ] := 'Intervalo de Impressão'
      _hmg_printer_usermessages [ 16 ] := 'Tudo'
      _hmg_printer_usermessages [ 17 ] := 'Páginas'
      _hmg_printer_usermessages [ 18 ] := 'De'
      _hmg_printer_usermessages [ 19 ] := 'Até'
      _hmg_printer_usermessages [ 20 ] := 'Nº de Cópias'
      _hmg_printer_usermessages [ 21 ] := 'Todas as Páginas'
      _hmg_printer_usermessages [ 22 ] := ' Somente Páginas Impares'
      _hmg_printer_usermessages [ 23 ] := ' Somente Páginas Pares'
      _hmg_printer_usermessages [ 24 ] := 'Sim'
      _hmg_printer_usermessages [ 25 ] := 'Não'
      _hmg_printer_usermessages [ 26 ] := 'Fechar'
      _hmg_printer_usermessages [ 27 ] := 'Salvar'
      _hmg_printer_usermessages [ 28 ] := 'Miniaturas'
      _hmg_printer_usermessages [ 29 ] := 'Aguarde, Gerando Miniaturas...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'
      
   CASE cLang == "RU"   // Russian
/////////////////////////////////////////////////////////////
// RUSSIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Ñòğàíèöà'
      _hmg_printer_usermessages [ 02 ] := 'Ïğåäâàğèòåëüíûé ïğîñìîòğ'
      _hmg_printer_usermessages [ 03 ] := 'Ïåğâàÿ [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Ïğåäûäóùàÿ [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Ñëåäóşùàÿ [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ïîñëåäíÿÿ [END]'
      _hmg_printer_usermessages [ 07 ] := 'Ïåğåéòè ê'
      _hmg_printer_usermessages [ 08 ] := 'Ìàñøòàá'
      _hmg_printer_usermessages [ 09 ] := 'Ïå÷àòü'
      _hmg_printer_usermessages [ 10 ] := 'Ñòğàíèöà ¹'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Îòìåíà'
      _hmg_printer_usermessages [ 13 ] := 'Âûáîğ ïğèíòåğà'
      _hmg_printer_usermessages [ 14 ] := 'Ñîğòèğîâêà êîïèé'
      _hmg_printer_usermessages [ 15 ] := 'Èíòåğâàë ïå÷àòè'
      _hmg_printer_usermessages [ 16 ] := 'Âñå'
      _hmg_printer_usermessages [ 17 ] := 'Ñòğàíèöû'
      _hmg_printer_usermessages [ 18 ] := 'Îò'
      _hmg_printer_usermessages [ 19 ] := 'Äî'
      _hmg_printer_usermessages [ 20 ] := 'Êîïèé'
      _hmg_printer_usermessages [ 21 ] := 'Âåñü èíòåğâàë'
      _hmg_printer_usermessages [ 22 ] := 'Íå÷åòíûå òîëüêî'
      _hmg_printer_usermessages [ 23 ] := '×åòíûå òîëüêî'
      _hmg_printer_usermessages [ 24 ] := 'Äà'
      _hmg_printer_usermessages [ 25 ] := 'Íåò'
      _hmg_printer_usermessages [ 26 ] := 'Çàêğûòü'
      _hmg_printer_usermessages [ 27 ] := 'Ñîõğàíèòü êàê PDF'
      _hmg_printer_usermessages [ 28 ] := 'Ìèíèàòşğû'
      _hmg_printer_usermessages [ 29 ] := 'Æäèòå, ãåíåğèğóş ìèíèàòşğû...'
      _hmg_printer_usermessages [ 30 ] := 'Îòïğàâèòü ïî ïî÷òå êàê PDF'

   CASE cLang == "UK" .OR. cLang == "UA"   // Ukrainian
/////////////////////////////////////////////////////////////
// UKRAINIAN
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Ñòîğ³íêà'
      _hmg_printer_usermessages [ 02 ] := 'Ïîïåğåäí³é ïåğåãëÿä'
      _hmg_printer_usermessages [ 03 ] := 'Ïåğøà [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Ïîïåğåäíÿ [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Íàñòóïíà [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Îñòàííÿ [END]'
      _hmg_printer_usermessages [ 07 ] := 'Ïåğåéòè äî'
      _hmg_printer_usermessages [ 08 ] := 'Ìàñøòàá'
      _hmg_printer_usermessages [ 09 ] := 'Äğóê'
      _hmg_printer_usermessages [ 10 ] := 'Ñòîğ³íêà ¹'
      _hmg_printer_usermessages [ 11 ] := 'Ãàğàçä'
      _hmg_printer_usermessages [ 12 ] := 'Â³äìîâà'
      _hmg_printer_usermessages [ 13 ] := 'Âèá³ğ ïğèíòåğà'
      _hmg_printer_usermessages [ 14 ] := 'Ñîğòóâàííÿ êîï³é'
      _hmg_printer_usermessages [ 15 ] := '²íòåğâàë äğóêó'
      _hmg_printer_usermessages [ 16 ] := 'Óñ³'
      _hmg_printer_usermessages [ 17 ] := 'Ñòîğ³íêè'
      _hmg_printer_usermessages [ 18 ] := 'Ç'
      _hmg_printer_usermessages [ 19 ] := 'Ïî'
      _hmg_printer_usermessages [ 20 ] := 'Êîï³é'
      _hmg_printer_usermessages [ 21 ] := 'Âåñü ³íòåğâàë'
      _hmg_printer_usermessages [ 22 ] := 'Ò³ëüêè íåïàğí³'
      _hmg_printer_usermessages [ 23 ] := 'Ò³ëüêè ïàğí³'
      _hmg_printer_usermessages [ 24 ] := 'Òàê'
      _hmg_printer_usermessages [ 25 ] := 'Í³'
      _hmg_printer_usermessages [ 26 ] := 'Çàêğèòè'
      _hmg_printer_usermessages [ 27 ] := 'Çáåğåãòè'
      _hmg_printer_usermessages [ 28 ] := 'Ì³í³àòşğè'
      _hmg_printer_usermessages [ 29 ] := 'Î÷³êóéòå, ãåíåğóş ì³í³àòşğè...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "ES"   // Spanish
/////////////////////////////////////////////////////////////
// SPANISH
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Página'
      _hmg_printer_usermessages [ 02 ] := 'Vista Previa'
      _hmg_printer_usermessages [ 03 ] := 'Inicio [INICIO]'
      _hmg_printer_usermessages [ 04 ] := 'Anterior [REPAG]'
      _hmg_printer_usermessages [ 05 ] := 'Siguiente [AVPAG]'
      _hmg_printer_usermessages [ 06 ] := 'Fin [FIN]'
      _hmg_printer_usermessages [ 07 ] := 'Ir a'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Imprimir'
      _hmg_printer_usermessages [ 10 ] := 'Página Nro.'
      _hmg_printer_usermessages [ 11 ] := 'Aceptar'
      _hmg_printer_usermessages [ 12 ] := 'Cancelar'
      _hmg_printer_usermessages [ 13 ] := 'Seleccionar Impresora'
      _hmg_printer_usermessages [ 14 ] := 'Ordenar Copias'
      _hmg_printer_usermessages [ 15 ] := 'Rango de Impresión'
      _hmg_printer_usermessages [ 16 ] := 'Todo'
      _hmg_printer_usermessages [ 17 ] := 'Páginas'
      _hmg_printer_usermessages [ 18 ] := 'Desde'
      _hmg_printer_usermessages [ 19 ] := 'Hasta'
      _hmg_printer_usermessages [ 20 ] := 'Copias'
      _hmg_printer_usermessages [ 21 ] := 'Todo El Rango'
      _hmg_printer_usermessages [ 22 ] := 'Solo Páginas Impares'
      _hmg_printer_usermessages [ 23 ] := 'Solo Páginas Pares'
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
      _hmg_printer_usermessages [ 04 ] := 'Prej¹nja stran [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Naslednja stran [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Zadnja stran [END]'
      _hmg_printer_usermessages [ 07 ] := 'Pojdi na stran'
      _hmg_printer_usermessages [ 08 ] := 'Poveèava'
      _hmg_printer_usermessages [ 09 ] := 'Natisni'
      _hmg_printer_usermessages [ 10 ] := '©tevilka strani'
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
      _hmg_printer_usermessages [ 28 ] := 'Slièice'
      _hmg_printer_usermessages [ 29 ] := 'Pripravljam slièice... prosim, poèakajte...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "SK"   // Slovak
/////////////////////////////////////////////////////////////
// SLOVAK
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Strana'
      _hmg_printer_usermessages [ 02 ] := 'Náh¾ad'
      _hmg_printer_usermessages [ 03 ] := 'Prvá strana [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Predcházajúca strana [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Ïalšia strana [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Posledná strana [END]'
      _hmg_printer_usermessages [ 07 ] := 'Uka stranu'
      _hmg_printer_usermessages [ 08 ] := 'Lupa'
      _hmg_printer_usermessages [ 09 ] := 'Tlaè'
      _hmg_printer_usermessages [ 10 ] := 'Èíslo strany'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Storno'
      _hmg_printer_usermessages [ 13 ] := 'Vyberte tlaèiáreò'
      _hmg_printer_usermessages [ 14 ] := 'Zoradenie'
      _hmg_printer_usermessages [ 15 ] := 'Rozsah tlaèe'
      _hmg_printer_usermessages [ 16 ] := 'všetko'
      _hmg_printer_usermessages [ 17 ] := 'strany'
      _hmg_printer_usermessages [ 18 ] := 'od'
      _hmg_printer_usermessages [ 19 ] := 'po'
      _hmg_printer_usermessages [ 20 ] := 'kópií'
      _hmg_printer_usermessages [ 21 ] := 'všetky strany'
      _hmg_printer_usermessages [ 22 ] := 'nepárné strany'
      _hmg_printer_usermessages [ 23 ] := 'párné strany'
      _hmg_printer_usermessages [ 24 ] := 'Áno'
      _hmg_printer_usermessages [ 25 ] := 'Nie'
      _hmg_printer_usermessages [ 26 ] := 'Zatvor'
      _hmg_printer_usermessages [ 27 ] := 'Ulo'
      _hmg_printer_usermessages [ 28 ] := 'Miniatury'
      _hmg_printer_usermessages [ 29 ] := 'Generujem miniatury... Èakajte, prosím...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "HU"   // Hungarian
/////////////////////////////////////////////////////////////
// HUNGARIAN
////////////////////////////////////////////////////////////
      
      _hmg_printer_usermessages [ 01 ] := 'Oldal'
      _hmg_printer_usermessages [ 02 ] := 'Elõnézet'
      _hmg_printer_usermessages [ 03 ] := 'Elsõ oldal [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Elõzõ oldal [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Következõ oldal [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Utolsó oldal [END]'
      _hmg_printer_usermessages [ 07 ] := 'Oldalt mutasd'
      _hmg_printer_usermessages [ 08 ] := 'Nagyító'
      _hmg_printer_usermessages [ 09 ] := 'Nyomtasd'
      _hmg_printer_usermessages [ 10 ] := 'Oldal száma'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Elvetés'
      _hmg_printer_usermessages [ 13 ] := 'Válasszon nyomtatót'
      _hmg_printer_usermessages [ 14 ] := 'Rendezve'
      _hmg_printer_usermessages [ 15 ] := 'Oldalak nyomtatása'
      _hmg_printer_usermessages [ 16 ] := 'mindent'
      _hmg_printer_usermessages [ 17 ] := 'Oldalakat'
      _hmg_printer_usermessages [ 18 ] := 'ettõl'
      _hmg_printer_usermessages [ 19 ] := 'eddig'
      _hmg_printer_usermessages [ 20 ] := 'másolat'
      _hmg_printer_usermessages [ 21 ] := 'Minden oldalt'
      _hmg_printer_usermessages [ 22 ] := 'páros oldalakat'
      _hmg_printer_usermessages [ 23 ] := 'páratlan oldalakat'
      _hmg_printer_usermessages [ 24 ] := 'Igen'
      _hmg_printer_usermessages [ 25 ] := 'Nem'
      _hmg_printer_usermessages [ 26 ] := 'Zárd be'
      _hmg_printer_usermessages [ 27 ] := 'Mentsd'
      _hmg_printer_usermessages [ 28 ] := 'Miniatúrák'
      _hmg_printer_usermessages [ 29 ] := 'Miniatúrák létrehozása... Kérem, várjon...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "BG"   // Bulgarian
/////////////////////////////////////////////////////////////
// BULGARIAN
////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Ñòğàíèöà'
      _hmg_printer_usermessages [ 02 ] := 'Ïğåäâàğèòåëåí ïğåãëåä'
      _hmg_printer_usermessages [ 03 ] := 'Ïúğâà [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Ïğåäèäóùà [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Ñëåäâùà [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ïîñëåäíà [END]'
      _hmg_printer_usermessages [ 07 ] := 'Èäè íà'
      _hmg_printer_usermessages [ 08 ] := 'Ìàùàá'
      _hmg_printer_usermessages [ 09 ] := 'Ïå÷àò'
      _hmg_printer_usermessages [ 10 ] := 'Ñòğàíèöà ¹'
      _hmg_printer_usermessages [ 11 ] := 'Ok'
      _hmg_printer_usermessages [ 12 ] := 'Îòìÿíà'
      _hmg_printer_usermessages [ 13 ] := 'Èçáîğ íà ïğèíòåğ'
      _hmg_printer_usermessages [ 14 ] := 'Ñîğòèğîâêà íà êîïèÿòà'
      _hmg_printer_usermessages [ 15 ] := 'Èíòåğâàë çà ïå÷àò'
      _hmg_printer_usermessages [ 16 ] := 'Âñè÷êè'
      _hmg_printer_usermessages [ 17 ] := 'Ñòğàíèöè'
      _hmg_printer_usermessages [ 18 ] := 'Îò'
      _hmg_printer_usermessages [ 19 ] := 'Äî'
      _hmg_printer_usermessages [ 20 ] := 'Êîïèÿ'
      _hmg_printer_usermessages [ 21 ] := 'Öåëèÿ èíòåğâàë'
      _hmg_printer_usermessages [ 22 ] := 'Ñàìî íå÷åòíèòå'
      _hmg_printer_usermessages [ 23 ] := 'Ñàìî ÷åòíèòå'
      _hmg_printer_usermessages [ 24 ] := 'Äà'
      _hmg_printer_usermessages [ 25 ] := 'Íå'
      _hmg_printer_usermessages [ 26 ] := 'Çàòâîğè'
      _hmg_printer_usermessages [ 27 ] := 'Ñúõğàíè'
      _hmg_printer_usermessages [ 28 ] := 'Ìèíèàòşğè'
      _hmg_printer_usermessages [ 29 ] := 'Èç÷àêàéòå, ãåíåğèğàì ìèíèàòşğè...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   CASE cLang == "EL"   // Greek - Ellinika
/////////////////////////////////////////////////////////////
// GREEK - ÅËËÇÍÉÊÁ
/////////////////////////////////////////////////////////////

      _hmg_printer_usermessages [ 01 ] := 'Óåëßäá'
      _hmg_printer_usermessages [ 02 ] := 'Ğñïåğéóêüğçóç'
      _hmg_printer_usermessages [ 03 ] := '1ç óåëßäá [HOME]'
      _hmg_printer_usermessages [ 04 ] := 'Ğñïçãïıìåíç óåëßäá [PGUP]'
      _hmg_printer_usermessages [ 05 ] := 'Åğüìåíç óåëßäá [PGDN]'
      _hmg_printer_usermessages [ 06 ] := 'Ôåëåõôáßá Óåëßäá [END]'
      _hmg_printer_usermessages [ 07 ] := 'Óåëßäá'
      _hmg_printer_usermessages [ 08 ] := 'Zoom'
      _hmg_printer_usermessages [ 09 ] := 'Åêôığùóç'
      _hmg_printer_usermessages [ 10 ] := 'Áñéè óåëßäáò'
      _hmg_printer_usermessages [ 11 ] := 'ÅíôÜîåé'
      _hmg_printer_usermessages [ 12 ] := '¢êõñïí'
      _hmg_printer_usermessages [ 13 ] := 'ÅğéëïãŞ åêôõğùôŞ'
      _hmg_printer_usermessages [ 14 ] := 'ÓõññáöŞ áíôéãñÜöùí'
      _hmg_printer_usermessages [ 15 ] := 'Ğåñéï÷Ş åêôığùóçò'
      _hmg_printer_usermessages [ 16 ] := 'Ïëá'
      _hmg_printer_usermessages [ 17 ] := 'Óåëßäåò'
      _hmg_printer_usermessages [ 18 ] := 'Áğü'
      _hmg_printer_usermessages [ 19 ] := 'İùò'
      _hmg_printer_usermessages [ 20 ] := 'Áíôßãñáöá'
      _hmg_printer_usermessages [ 21 ] := 'Ïëåò ïé óåëßäåò'
      _hmg_printer_usermessages [ 22 ] := 'Æõãİò óåëßäåò'
      _hmg_printer_usermessages [ 23 ] := 'Ìïíåò óåëßäåò'
      _hmg_printer_usermessages [ 24 ] := 'Íáß'
      _hmg_printer_usermessages [ 25 ] := 'Ï÷é'
      _hmg_printer_usermessages [ 26 ] := 'Åîïäïò'
      _hmg_printer_usermessages [ 27 ] := 'ÁğïèŞêåõóç'
      _hmg_printer_usermessages [ 28 ] := 'Ìéêñïãñáößåò'
      _hmg_printer_usermessages [ 29 ] := 'Äçìéïõñãßá ìéêñïãñáöéşí... Ğåñéìİíåôå...'
      _hmg_printer_usermessages [ 30 ] := 'Send by email as PDF'

   ENDCASE

#endif

   RETURN