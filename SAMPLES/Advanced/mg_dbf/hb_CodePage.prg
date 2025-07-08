/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Список кодовых страниц всех языков в Харборе
 * List of code pages for all languages in Harbor
*/
#include "minigui.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION myCodePage()
   LOCAL aRet, aDim, nI

   aDim := {}
   AADD( aDim , { "RU1251" , "русский Windows-1251 (ANSI)"                      , "russian Windows-1251 (ANSI)"                      })
   AADD( aDim , { "RU866"  , "русский OEM"                                      , "russian OEM"                                      })
   AADD( aDim , { "RUKOI8" , "русский KOI8"                                     , "russian KOI8"                                     })
   AADD( aDim , { "RUISO"  , "русский ISO"                                      , "russian ISO"                                      })
   AADD( aDim , { "UA866"  , "украинский UA866"                                 , "ukrainian UA866"                                  })
   AADD( aDim , { "UA1251" , "украинский Windows-1251"                          , "ukrainian Windows-1251"                           })
   AADD( aDim , { "UAKOI8" , "украинский KOI8"                                  , "ukrainian KOI8"                                   })
   AADD( aDim , { "UA1125" , "украинский 1125"                                  , "ukrainian 1125"                                   })
   AADD( aDim , { "UTF8"   , "формат Юникода, 8-битный"                         , "unicode Format, 8-bit"                            })
   AADD( aDim , { "UTF8EX" , "расширенный формат Юникода"                       , "Unicode Extended Format"                          })
   AADD( aDim , { "UTF16LE", "кодировка Little Endian UTF-16"                   , "Little Endian UTF-16 encoding"                    })
   AADD( aDim , { "EN"     , "английский язык"                                  , "English language"                                 })
   AADD( aDim , { "BG866"  , "болгарский CP-866"                                , "Bulgarian CP-866"                                 })
   AADD( aDim , { "BGISO"  , "болгарский ISO-8859-5"                            , "Bulgarian ISO-8859-5"                             })
   AADD( aDim , { "BGMIK"  , "болгарский MIK"                                   , "Bulgarian MIK"                                    })
   AADD( aDim , { "BGWIN"  , "болгарский Windows-1251"                          , "Bulgarian Windows-1251"                           })
   AADD( aDim , { "CS852"  , "чешский CP-852"                                   , "Czech CP-852"                                     })
   AADD( aDim , { "CS852C" , "чешский CP-852 (совместимый с ntxcz852.obj)"      , "Czech CP-852 (compatible with ntxcz852.obj)"      })
   AADD( aDim , { "CSISO"  , "чешский ISO-8859-2"                               , "Czech ISO-8859-2"                                 })
   AADD( aDim , { "CSKAMC" , "чешский Каменицкий (совместимый с ntxcz895.obj)"  , "Czech Kamenicky (compatible with ntxcz895.obj)"   })
   AADD( aDim , { "CSWIN"  , "чешский Windows-1250"                             , "Czech Windows-1250"                               })
   AADD( aDim , { "DE850"  , "немецкий CP-850 (совместимый с ntxger.obj)"       , "German CP-850 (compatible with ntxger.obj)"       })
   AADD( aDim , { "DE850M" , "немецкий CP-850 (совместимый с mdxger.obj)"       , "German CP-850 (compatible with mdxger.obj)"       })
   AADD( aDim , { "DE858"  , "немецкий CP-858"                                  , "German CP-858"                                    })
   AADD( aDim , { "DEISO"  , "немецкий ISO-8859-1"                              , "German ISO-8859-1"                                })
   AADD( aDim , { "DEWIN"  , "немецкий Windows-1252"                            , "German Windows-1252"                              })
   AADD( aDim , { "DK865"  , "датский CP-865 (совместимый с ntxdan.obj)"        , "Danish CP-865 (compatible with ntxdan.obj)"       })
   AADD( aDim , { "EE775"  , "эстонский CP-775"                                 , "Estonian CP-775"                                  })
   AADD( aDim , { "EEWIN"  , "эстонский Windows-1257"                           , "Estonian Windows-1257"                            })
   AADD( aDim , { "EL437"  , "греческий CP-437 (совместимый с ntxgr437.obj)"    , "Greek CP-437 (compatible with ntxgr437.obj)"      })
   AADD( aDim , { "EL737"  , "греческий CP-737"                                 , "Greek CP-737"                                     })
   AADD( aDim , { "ELISO"  , "греческий ISO-8859-7"                             , "Greek ISO-8859-7"                                 })
   AADD( aDim , { "ELWIN"  , "греческий ANSI CP-1253"                           , "Greek ANSI CP-1253"                               })
   AADD( aDim , { "ES850"  , "испанский (современный) CP-850"                   , "Spanish (modern) CP-850"                          })
   AADD( aDim , { "ES850C" , "испанский CP-850 (совместимый с ntxspa.obj)"      , "Spanish CP-850 (compatible with ntxspa.obj)"      })
   AADD( aDim , { "ES850M" , "испанский CP-850 (совместимый с mdxspa.obj)"      , "Spanish CP-850 (compatible with mdxspa.obj)"      })
   AADD( aDim , { "ESISO"  , "испанский (современный) ISO-8859-1"               , "Spanish (modern) ISO-8859-1"                      })
   AADD( aDim , { "ESMWIN" , "испанский (современный) ISO-8859-1"               , "Spanish (modern) ISO-8859-1"                      })
   AADD( aDim , { "ESWIN"  , "испанский (современный) Windows-1252"             , "Spanish (Modern) Windows-1252"                    })
   AADD( aDim , { "FI850"  , "финский CP-850 (совместимый с ntxfin.obj)"        , "Finnish CP-850 (compatible with ntxfin.obj)"      })
   AADD( aDim , { "FR850"  , "французский CP-850"                               , "French CP-850"                                    })
   AADD( aDim , { "FR850C" , "французский CP-850 (совместимый с ntxfre.obj)"    , "French CP-850 (compatible with ntxfre.obj)"       })
   AADD( aDim , { "FR850M" , "французский CP-850 (совместимый с mdxfre.obj)"    , "French CP-850 (compatible with mdxfre.obj)"       })
   AADD( aDim , { "FRISO"  , "французский ISO-8859-1"                           , "French ISO-8859-1"                                })
   AADD( aDim , { "FRWIN"  , "французский Windows-1252"                         , "French Windows-1252"                              })
   AADD( aDim , { "HE862"  , "иврит CP-862"                                     , "Hebrew CP-862"                                    })
   AADD( aDim , { "HEWIN"  , "иврит Windows-1255"                               , "Hebrew Windows-1255"                              })
   AADD( aDim , { "HR646"  , "хорватский ISO-646 (CROSCII)"                     , "Croatian ISO-646 (CROSCII)"                       })
   AADD( aDim , { "HR852"  , "хорватский CP-852"                                , "Croatian CP-852"                                  })
   AADD( aDim , { "HRISO"  , "хорватский ISO-8859-2"                            , "Croatian ISO-8859-2"                              })
   AADD( aDim , { "HRWIN"  , "хорватские Windows-1250"                          , "Croatian Windows-1250"                            })
   AADD( aDim , { "HU852"  , "венгерский CP-852 (совместимый с sixhu852.obj)"   , "Hungarian CP-852 (compatible with sixhu852.obj)"  })
   AADD( aDim , { "HU852C" , "венгерский CP-852 (совместимый с ntxhu852.obj)"   , "Hungarian CP-852 (compatible with ntxhu852.obj)"  })
   AADD( aDim , { "HUISO"  , "венгерский ISO-8859-2"                            , "Hungarian ISO-8859-2"                             })
   AADD( aDim , { "HUWIN"  , "венгерский Windows-1250"                          , "Hungarian Windows-1250"                           })
   AADD( aDim , { "IS850"  , "исландский CP-850 (совместимый с ntxic850.obj)"   , "Icelandic CP-850 (compatible with ntxic850.obj)"  })
   AADD( aDim , { "IS861"  , "исландский CP-861 (совместимый с ntxic861.obj)"   , "Icelandic CP-861 (compatible with ntxic861.obj)"  })
   AADD( aDim , { "IT437"  , "итальянский CP-437"                               , "Italian CP-437"                                   })
   AADD( aDim , { "IT850"  , "итальянский CP-850 (совместимый с ntxita.obj)"    , "Italian CP-850 (compatible with ntxita.obj)"      })
   AADD( aDim , { "IT850M" , "итальянский CP-850 (совместимый с mdxita.obj)"    , "Italian CP-850 (compatible with mdxita.obj)"      })
   AADD( aDim , { "ITISB"  , "итальянский ISO-8859-1b (с символами BOX)"        , "Italian ISO-8859-1b (with BOX characters)"        })
   AADD( aDim , { "ITISO"  , "итальянский ISO-8859-1"                           , "Italian ISO-8859-1"                               })
   AADD( aDim , { "ITWIN"  , "итальянский Windows-1252"                         , "Italian Windows-1252"                             })
   AADD( aDim , { "LT775"  , "литовский CP-775"                                 , "Lithuanian CP-775"                                })
   AADD( aDim , { "LTWIN"  , "литовский Windows-1257"                           , "Lithuanian Windows-1257"                          })
   AADD( aDim , { "LV775"  , "латвийский CP-775"                                , "Latvian CP-775"                                   })
   AADD( aDim , { "LVWIN"  , "латвийский Windows-1257"                          , "Latvian Windows-1257"                             })
   AADD( aDim , { "NL850"  , "голландский CP-850 (совместимый с ntxdut.obj)"    , "Dutch CP-850 (compatible with ntxdut.obj)"        })
   AADD( aDim , { "NL850M" , "голландский CP-850 (совместимый с mdxdut.obj)"    , "Dutch CP-850 (compatible with mdxdut.obj)"        })
   AADD( aDim , { "NO865"  , "норвежский CP-865 (совместимый с ntxnor.obj)"     , "Norwegian CP-865 (compatible with ntxnor.obj)"    })
   AADD( aDim , { "PL852"  , "польский CP-852"                                  , "Polish CP-852"                                    })
   AADD( aDim , { "PLISO"  , "польский ISO-8859-2"                              , "Polish ISO-8859-2"                                })
   AADD( aDim , { "PLMAZ"  , "польский Мазовия"                                 , "Polish Mazovia"                                   })
   AADD( aDim , { "PLWIN"  , "польский Windows-1250"                            , "Polish Windows-1250"                              })
   AADD( aDim , { "PT850"  , "португальский CP-850"                             , "Portuguese CP-850"                                })
   AADD( aDim , { "PT860"  , "португальский CP-860 (совместимый с ntxpor.obj)"  , "Portuguese CP-860 (compatible with ntxpor.obj)"   })
   AADD( aDim , { "PTISO"  , "португальский ISO-8859-1"                         , "Portuguese ISO-8859-1"                            })
   AADD( aDim , { "RO852"  , "румынский CP-852"                                 , "Romanian CP-852"                                  })
   AADD( aDim , { "ROISO"  , "румынский ISO-8859-2"                             , "Romanian ISO-8859-2"                              })
   AADD( aDim , { "ROWIN"  , "румынский Windows-1250"                           , "Romanian Windows-1250"                            })
   AADD( aDim , { "SK852"  , "словацкий CP-852"                                 , "Slovak CP-852"                                    })
   AADD( aDim , { "SK852C" , "словацкий CP-852 (совместимый с ntxsl852.obj)"    , "Slovak CP-852 (compatible with ntxsl852.obj)"     })
   AADD( aDim , { "SKISO"  , "словацкий ISO-8859-2"                             , "Slovak ISO-8859-2"                                })
   AADD( aDim , { "SKKAMC" , "словацкий Каменицкий (совместимый с ntxsl895.obj)", "Slovak Kamenicky (compatible with ntxsl895.obj)"  })
   AADD( aDim , { "SKWIN"  , "словацкий Windows-1250"                           , "Slovak Windows-1250"                              })
   AADD( aDim , { "SL646"  , "словенский ISO-646 (SLOSCII)"                     , "Slovenian ISO-646 (SLOSCII)"                      })
   AADD( aDim , { "SL852"  , "словенский CP-852"                                , "Slovenian CP-852"                                 })
   AADD( aDim , { "SLISO"  , "словенский ISO-8859-2"                            , "Slovenian ISO-8859-2"                             })
   AADD( aDim , { "SLWIN"  , "cловенский CP-1250"                               , "Slovenian CP-1250"                                })
   AADD( aDim , { "SR646"  , "cербский ISO-646 (YUSCII)"                        , "Serbian ISO-646 (YUSCII)"                         })
   AADD( aDim , { "SR646C" , "cербский ISO-646C (кириллица YUSCII)"             , "Serbian ISO-646C (Cyrillic YUSCII)"               })
   AADD( aDim , { "SRWIN"  , "cербский Windows-1251"                            , "Serbian Windows-1251"                             })
   AADD( aDim , { "SV437C" , "шведский CP-437 (совместимый с ntxswe.obj)"       , "Swedish CP-437 (compatible with ntxswe.obj)"      })
   AADD( aDim , { "SV850"  , "шведский CP-850 (совместимый с ntxswe.obj)"       , "Swedish CP-850 (ntxswe.obj compatible)"           })
   AADD( aDim , { "SV850M" , "шведский CP-850 (совместимый с mdxswe.obj)"       , "Swedish CP-850 (mdxswe.obj compatible)"           })
   AADD( aDim , { "SVISO"  , "шведский ISO-8859-15"                             , "Swedish ISO-8859-15"                              })
   AADD( aDim , { "SVWIN"  , "шведская Windows-1252"                            , "Swedish Windows-1252"                             })
   AADD( aDim , { "TR857"  , "турецкий CP-857"                                  , "Turkish CP-857"                                   })
   AADD( aDim , { "TRISO"  , "турецкий ISO-8859-9"                              , "Turkish ISO-8859-9"                               })
   AADD( aDim , { "TRWIN"  , "турецкий Windows-1254"                            , "Turkish Windows-1254"                             })

   aRet := {}
   IF App.Cargo:cLang == "RU"
      FOR nI := 1 TO LEN(aDim)
         AADD( aRet, { aDim[nI,1] , aDim[nI,2] } )
      NEXT
   ELSE
      FOR nI := 1 TO LEN(aDim)
         AADD( aRet, { aDim[nI,1] , aDim[nI,3] } )
      NEXT
   ENDIF

RETURN aRet

/////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myCodePagePart(aYX)
   LOCAL aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, aFntExt, cTypeRes, cMsg
   DEFAULT aYX := 0

   cForm := ThisWindow.Name
   IF App.Cargo:cLang == "RU"
      cMsg := "Кодовая страница: "
   ELSE
      cMsg := "Code page: "
   ENDIF
   aDim := {}
   AADD( aDim, { "iFlag_Ru32"    , cMsg + "RU1251" , .F. , "", "RU1251"  , 1 } )
   AADD( aDim, { "iFlag_Ru32"    , cMsg + "RU866"  , .F. , "", "RU866"   , 2 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA866"  , .F. , "", "UA866"   , 3 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA1251" , .F. , "", "UA1251"  , 4 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA1125" , .F. , "", "UA1125"  , 5 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF8"   , .F. , "", "UTF8"    , 6 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF8EX" , .F. , "", "UTF8EX"  , 7 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF16LE", .F. , "", "UTF16LE" , 8 } )
   AADD( aDim, {                                                             } )
   AADD( aDim, { "iLang32"       , cMsg + "Резерв" , .F. , "", "NONE"    ,99 } )

   aRet     := {}
   nPos     := IIF( IsArray(aYX), aYX , 3  )  // 3-координаты меню где мышка нажата
   cTypeRes := "ICO"  // "BMP"
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFntExt, cTypeRes )
   IF nChoice > 0
      aRet := { aDim[nChoice,6], aDim[nChoice,5], aDim[nChoice,2], aDim[nChoice,1] }
   ENDIF
   DO EVENTS

RETURN aRet

