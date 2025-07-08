// ---------------------------------------------------------------
// ---------------------------------------------------------------
// ESTA PARTE DEL CODIGO LA PUEDES INCORPORAR EN TUS ".ch" Y ASI
// SERA MAS FACIL LLAMAR A LA FUNCION.
// -----------------------------------------------------------------
// -----------------------------------------------------------------

#command @ <row>,<col> CALENDARI ;
      [ PARENT <parent> ] ;
      [ VALUE  <value> ] ;
      [ ONCHANGE <action> ] ;
      [ <verdia : VERDIA > ] ;
      [ COLORNOMES <cColorNomes> ] ;
      [ COLORMES   <cColorMes> ] ;
      [ COLORDIA   <cColorDia> ] ;
      [ COLORDOM   <cColorDom> ] ;
      [ COLORINV   <cColorInv> ] ;
      [ COLORFONDO <cColorFondo> ] ;
   => ;
      Calendari( <"parent">, <row>, <col>, <value>, <.verdia.>, ;
         <cColorNomes>, <cColorMes>, <cColorDia>, <cColorDom>, <cColorInv>, <cColorFondo>, ;
         <{action}> )

