/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������ ������� ������� ���� ������ � �������
 * List of code pages for all languages in Harbor
*/
#include "minigui.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION myCodePage()
   LOCAL aRet, aDim, nI

   aDim := {}
   AADD( aDim , { "RU1251" , "������� Windows-1251 (ANSI)"                      , "russian Windows-1251 (ANSI)"                      })
   AADD( aDim , { "RU866"  , "������� OEM"                                      , "russian OEM"                                      })
   AADD( aDim , { "RUKOI8" , "������� KOI8"                                     , "russian KOI8"                                     })
   AADD( aDim , { "RUISO"  , "������� ISO"                                      , "russian ISO"                                      })
   AADD( aDim , { "UA866"  , "���������� UA866"                                 , "ukrainian UA866"                                  })
   AADD( aDim , { "UA1251" , "���������� Windows-1251"                          , "ukrainian Windows-1251"                           })
   AADD( aDim , { "UAKOI8" , "���������� KOI8"                                  , "ukrainian KOI8"                                   })
   AADD( aDim , { "UA1125" , "���������� 1125"                                  , "ukrainian 1125"                                   })
   AADD( aDim , { "UTF8"   , "������ �������, 8-������"                         , "unicode Format, 8-bit"                            })
   AADD( aDim , { "UTF8EX" , "����������� ������ �������"                       , "Unicode Extended Format"                          })
   AADD( aDim , { "UTF16LE", "��������� Little Endian UTF-16"                   , "Little Endian UTF-16 encoding"                    })
   AADD( aDim , { "EN"     , "���������� ����"                                  , "English language"                                 })
   AADD( aDim , { "BG866"  , "���������� CP-866"                                , "Bulgarian CP-866"                                 })
   AADD( aDim , { "BGISO"  , "���������� ISO-8859-5"                            , "Bulgarian ISO-8859-5"                             })
   AADD( aDim , { "BGMIK"  , "���������� MIK"                                   , "Bulgarian MIK"                                    })
   AADD( aDim , { "BGWIN"  , "���������� Windows-1251"                          , "Bulgarian Windows-1251"                           })
   AADD( aDim , { "CS852"  , "������� CP-852"                                   , "Czech CP-852"                                     })
   AADD( aDim , { "CS852C" , "������� CP-852 (����������� � ntxcz852.obj)"      , "Czech CP-852 (compatible with ntxcz852.obj)"      })
   AADD( aDim , { "CSISO"  , "������� ISO-8859-2"                               , "Czech ISO-8859-2"                                 })
   AADD( aDim , { "CSKAMC" , "������� ���������� (����������� � ntxcz895.obj)"  , "Czech Kamenicky (compatible with ntxcz895.obj)"   })
   AADD( aDim , { "CSWIN"  , "������� Windows-1250"                             , "Czech Windows-1250"                               })
   AADD( aDim , { "DE850"  , "�������� CP-850 (����������� � ntxger.obj)"       , "German CP-850 (compatible with ntxger.obj)"       })
   AADD( aDim , { "DE850M" , "�������� CP-850 (����������� � mdxger.obj)"       , "German CP-850 (compatible with mdxger.obj)"       })
   AADD( aDim , { "DE858"  , "�������� CP-858"                                  , "German CP-858"                                    })
   AADD( aDim , { "DEISO"  , "�������� ISO-8859-1"                              , "German ISO-8859-1"                                })
   AADD( aDim , { "DEWIN"  , "�������� Windows-1252"                            , "German Windows-1252"                              })
   AADD( aDim , { "DK865"  , "������� CP-865 (����������� � ntxdan.obj)"        , "Danish CP-865 (compatible with ntxdan.obj)"       })
   AADD( aDim , { "EE775"  , "��������� CP-775"                                 , "Estonian CP-775"                                  })
   AADD( aDim , { "EEWIN"  , "��������� Windows-1257"                           , "Estonian Windows-1257"                            })
   AADD( aDim , { "EL437"  , "��������� CP-437 (����������� � ntxgr437.obj)"    , "Greek CP-437 (compatible with ntxgr437.obj)"      })
   AADD( aDim , { "EL737"  , "��������� CP-737"                                 , "Greek CP-737"                                     })
   AADD( aDim , { "ELISO"  , "��������� ISO-8859-7"                             , "Greek ISO-8859-7"                                 })
   AADD( aDim , { "ELWIN"  , "��������� ANSI CP-1253"                           , "Greek ANSI CP-1253"                               })
   AADD( aDim , { "ES850"  , "��������� (�����������) CP-850"                   , "Spanish (modern) CP-850"                          })
   AADD( aDim , { "ES850C" , "��������� CP-850 (����������� � ntxspa.obj)"      , "Spanish CP-850 (compatible with ntxspa.obj)"      })
   AADD( aDim , { "ES850M" , "��������� CP-850 (����������� � mdxspa.obj)"      , "Spanish CP-850 (compatible with mdxspa.obj)"      })
   AADD( aDim , { "ESISO"  , "��������� (�����������) ISO-8859-1"               , "Spanish (modern) ISO-8859-1"                      })
   AADD( aDim , { "ESMWIN" , "��������� (�����������) ISO-8859-1"               , "Spanish (modern) ISO-8859-1"                      })
   AADD( aDim , { "ESWIN"  , "��������� (�����������) Windows-1252"             , "Spanish (Modern) Windows-1252"                    })
   AADD( aDim , { "FI850"  , "������� CP-850 (����������� � ntxfin.obj)"        , "Finnish CP-850 (compatible with ntxfin.obj)"      })
   AADD( aDim , { "FR850"  , "����������� CP-850"                               , "French CP-850"                                    })
   AADD( aDim , { "FR850C" , "����������� CP-850 (����������� � ntxfre.obj)"    , "French CP-850 (compatible with ntxfre.obj)"       })
   AADD( aDim , { "FR850M" , "����������� CP-850 (����������� � mdxfre.obj)"    , "French CP-850 (compatible with mdxfre.obj)"       })
   AADD( aDim , { "FRISO"  , "����������� ISO-8859-1"                           , "French ISO-8859-1"                                })
   AADD( aDim , { "FRWIN"  , "����������� Windows-1252"                         , "French Windows-1252"                              })
   AADD( aDim , { "HE862"  , "����� CP-862"                                     , "Hebrew CP-862"                                    })
   AADD( aDim , { "HEWIN"  , "����� Windows-1255"                               , "Hebrew Windows-1255"                              })
   AADD( aDim , { "HR646"  , "���������� ISO-646 (CROSCII)"                     , "Croatian ISO-646 (CROSCII)"                       })
   AADD( aDim , { "HR852"  , "���������� CP-852"                                , "Croatian CP-852"                                  })
   AADD( aDim , { "HRISO"  , "���������� ISO-8859-2"                            , "Croatian ISO-8859-2"                              })
   AADD( aDim , { "HRWIN"  , "���������� Windows-1250"                          , "Croatian Windows-1250"                            })
   AADD( aDim , { "HU852"  , "���������� CP-852 (����������� � sixhu852.obj)"   , "Hungarian CP-852 (compatible with sixhu852.obj)"  })
   AADD( aDim , { "HU852C" , "���������� CP-852 (����������� � ntxhu852.obj)"   , "Hungarian CP-852 (compatible with ntxhu852.obj)"  })
   AADD( aDim , { "HUISO"  , "���������� ISO-8859-2"                            , "Hungarian ISO-8859-2"                             })
   AADD( aDim , { "HUWIN"  , "���������� Windows-1250"                          , "Hungarian Windows-1250"                           })
   AADD( aDim , { "IS850"  , "���������� CP-850 (����������� � ntxic850.obj)"   , "Icelandic CP-850 (compatible with ntxic850.obj)"  })
   AADD( aDim , { "IS861"  , "���������� CP-861 (����������� � ntxic861.obj)"   , "Icelandic CP-861 (compatible with ntxic861.obj)"  })
   AADD( aDim , { "IT437"  , "����������� CP-437"                               , "Italian CP-437"                                   })
   AADD( aDim , { "IT850"  , "����������� CP-850 (����������� � ntxita.obj)"    , "Italian CP-850 (compatible with ntxita.obj)"      })
   AADD( aDim , { "IT850M" , "����������� CP-850 (����������� � mdxita.obj)"    , "Italian CP-850 (compatible with mdxita.obj)"      })
   AADD( aDim , { "ITISB"  , "����������� ISO-8859-1b (� ��������� BOX)"        , "Italian ISO-8859-1b (with BOX characters)"        })
   AADD( aDim , { "ITISO"  , "����������� ISO-8859-1"                           , "Italian ISO-8859-1"                               })
   AADD( aDim , { "ITWIN"  , "����������� Windows-1252"                         , "Italian Windows-1252"                             })
   AADD( aDim , { "LT775"  , "��������� CP-775"                                 , "Lithuanian CP-775"                                })
   AADD( aDim , { "LTWIN"  , "��������� Windows-1257"                           , "Lithuanian Windows-1257"                          })
   AADD( aDim , { "LV775"  , "���������� CP-775"                                , "Latvian CP-775"                                   })
   AADD( aDim , { "LVWIN"  , "���������� Windows-1257"                          , "Latvian Windows-1257"                             })
   AADD( aDim , { "NL850"  , "����������� CP-850 (����������� � ntxdut.obj)"    , "Dutch CP-850 (compatible with ntxdut.obj)"        })
   AADD( aDim , { "NL850M" , "����������� CP-850 (����������� � mdxdut.obj)"    , "Dutch CP-850 (compatible with mdxdut.obj)"        })
   AADD( aDim , { "NO865"  , "���������� CP-865 (����������� � ntxnor.obj)"     , "Norwegian CP-865 (compatible with ntxnor.obj)"    })
   AADD( aDim , { "PL852"  , "�������� CP-852"                                  , "Polish CP-852"                                    })
   AADD( aDim , { "PLISO"  , "�������� ISO-8859-2"                              , "Polish ISO-8859-2"                                })
   AADD( aDim , { "PLMAZ"  , "�������� �������"                                 , "Polish Mazovia"                                   })
   AADD( aDim , { "PLWIN"  , "�������� Windows-1250"                            , "Polish Windows-1250"                              })
   AADD( aDim , { "PT850"  , "������������� CP-850"                             , "Portuguese CP-850"                                })
   AADD( aDim , { "PT860"  , "������������� CP-860 (����������� � ntxpor.obj)"  , "Portuguese CP-860 (compatible with ntxpor.obj)"   })
   AADD( aDim , { "PTISO"  , "������������� ISO-8859-1"                         , "Portuguese ISO-8859-1"                            })
   AADD( aDim , { "RO852"  , "��������� CP-852"                                 , "Romanian CP-852"                                  })
   AADD( aDim , { "ROISO"  , "��������� ISO-8859-2"                             , "Romanian ISO-8859-2"                              })
   AADD( aDim , { "ROWIN"  , "��������� Windows-1250"                           , "Romanian Windows-1250"                            })
   AADD( aDim , { "SK852"  , "��������� CP-852"                                 , "Slovak CP-852"                                    })
   AADD( aDim , { "SK852C" , "��������� CP-852 (����������� � ntxsl852.obj)"    , "Slovak CP-852 (compatible with ntxsl852.obj)"     })
   AADD( aDim , { "SKISO"  , "��������� ISO-8859-2"                             , "Slovak ISO-8859-2"                                })
   AADD( aDim , { "SKKAMC" , "��������� ���������� (����������� � ntxsl895.obj)", "Slovak Kamenicky (compatible with ntxsl895.obj)"  })
   AADD( aDim , { "SKWIN"  , "��������� Windows-1250"                           , "Slovak Windows-1250"                              })
   AADD( aDim , { "SL646"  , "���������� ISO-646 (SLOSCII)"                     , "Slovenian ISO-646 (SLOSCII)"                      })
   AADD( aDim , { "SL852"  , "���������� CP-852"                                , "Slovenian CP-852"                                 })
   AADD( aDim , { "SLISO"  , "���������� ISO-8859-2"                            , "Slovenian ISO-8859-2"                             })
   AADD( aDim , { "SLWIN"  , "c��������� CP-1250"                               , "Slovenian CP-1250"                                })
   AADD( aDim , { "SR646"  , "c������� ISO-646 (YUSCII)"                        , "Serbian ISO-646 (YUSCII)"                         })
   AADD( aDim , { "SR646C" , "c������� ISO-646C (��������� YUSCII)"             , "Serbian ISO-646C (Cyrillic YUSCII)"               })
   AADD( aDim , { "SRWIN"  , "c������� Windows-1251"                            , "Serbian Windows-1251"                             })
   AADD( aDim , { "SV437C" , "�������� CP-437 (����������� � ntxswe.obj)"       , "Swedish CP-437 (compatible with ntxswe.obj)"      })
   AADD( aDim , { "SV850"  , "�������� CP-850 (����������� � ntxswe.obj)"       , "Swedish CP-850 (ntxswe.obj compatible)"           })
   AADD( aDim , { "SV850M" , "�������� CP-850 (����������� � mdxswe.obj)"       , "Swedish CP-850 (mdxswe.obj compatible)"           })
   AADD( aDim , { "SVISO"  , "�������� ISO-8859-15"                             , "Swedish ISO-8859-15"                              })
   AADD( aDim , { "SVWIN"  , "�������� Windows-1252"                            , "Swedish Windows-1252"                             })
   AADD( aDim , { "TR857"  , "�������� CP-857"                                  , "Turkish CP-857"                                   })
   AADD( aDim , { "TRISO"  , "�������� ISO-8859-9"                              , "Turkish ISO-8859-9"                               })
   AADD( aDim , { "TRWIN"  , "�������� Windows-1254"                            , "Turkish Windows-1254"                             })

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
      cMsg := "������� ��������: "
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
   AADD( aDim, { "iLang32"       , cMsg + "������" , .F. , "", "NONE"    ,99 } )

   aRet     := {}
   nPos     := IIF( IsArray(aYX), aYX , 3  )  // 3-���������� ���� ��� ����� ������
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

