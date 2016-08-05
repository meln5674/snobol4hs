module ASTs where

import Language.Snobol4.Syntax.AST

ast_pg1_1 = assignStmt Nothing (IdExpr "V") (LitExpr (Int 5)) Nothing

ast_pg1_2 = assignStmt Nothing (IdExpr "W")
         (BinaryExpr 
            (LitExpr (Int 14)) 
            Plus 
            (ParenExpr 
                (BinaryExpr 
                    (LitExpr (Int 16)) 
                    Minus 
                    (LitExpr (Int 10))
                )
            )
         )
         Nothing

ast_pg1_3 = assignStmt Nothing (IdExpr "V") (LitExpr (String "DOG")) Nothing

ast_pg2_1 = assignStmt Nothing (IdExpr "RESULT") (IdExpr "ANS_1") Nothing

ast_pg2_2 = Program
        [ assignStmt Nothing (IdExpr "M") (LitExpr (Int 4)) Nothing
        , assignStmt Nothing (IdExpr "N") (LitExpr (Int 5)) Nothing
        , assignStmt Nothing (IdExpr "P")
            (BinaryExpr
                (BinaryExpr
                    (IdExpr "N")
                    Star
                    (IdExpr "M")
                )
                Slash
                (ParenExpr
                    (BinaryExpr
                        (IdExpr "N")
                        Minus
                        (LitExpr (Int 1))
                    )
                )
            )
            Nothing
        ]

ast_pg2_3 = assignStmt Nothing (IdExpr "Q2")
        (BinaryExpr
            (PrefixExpr Minus (IdExpr "P"))
            Slash
            (PrefixExpr Minus (IdExpr "N"))
        )
        Nothing

ast_pg2_4 = assignStmt Nothing (IdExpr "X")
        (BinaryExpr
            (LitExpr (Int 2))
            DoubleStar
            (BinaryExpr
                (LitExpr (Int 3))
                DoubleStar
                (LitExpr (Int 2))
            )
        )
        Nothing

ast_pg2_5 = assignStmt Nothing (IdExpr "X")
        (BinaryExpr
            (LitExpr (Int 2))
            DoubleStar
            (ParenExpr
                (BinaryExpr
                    (LitExpr (Int 3))
                    DoubleStar
                    (LitExpr (Int 2))
                )
            )
        )
        Nothing

ast_pg3_1 = Program
        [ assignStmt Nothing (IdExpr "PI") (LitExpr (Real 3.14159)) Nothing
        , assignStmt Nothing (IdExpr "CIRUM")
            (BinaryExpr
                (BinaryExpr
                    (LitExpr (Real 2.0))
                    Star
                    (IdExpr "PI")
                )
                Star
                (LitExpr (Real 5.0))
            )
            Nothing
        ]

ast_pg3_2 = assignStmt Nothing (IdExpr "SUM") 
        (BinaryExpr
            (LitExpr (Real 16.4))
            Plus
            (LitExpr (Int 2))
        )
        Nothing

ast_pg3_3 = assignStmt Nothing (IdExpr "SCREAM") (LitExpr (String "HELP")) Nothing

ast_pg3_4 = Program
        [ assignStmt Nothing (IdExpr "PLEA") (LitExpr (String "HE SHOUTED, \"HELP.\"")) Nothing
        , assignStmt Nothing (IdExpr "QUOTE") (LitExpr (String "\"")) Nothing
        , assignStmt Nothing (IdExpr "APOSTROPHE") (LitExpr (String "\'")) Nothing
        ]

ast_pg4_1 = assignStmt Nothing (IdExpr "NULL") NullExpr Nothing

ast_pg4_2 = Program
        [ assignStmt Nothing (IdExpr "Z") (LitExpr (String "10")) Nothing
        , assignStmt Nothing (IdExpr "X")
            (BinaryExpr
                (BinaryExpr
                    (LitExpr (Int 5))
                    Star
                    (PrefixExpr Minus (IdExpr "Z"))
                )
                Plus
                (LitExpr (String "10.6"))
            )
            Nothing
        ]

ast_pg4_5 = Program
        [ assignStmt Nothing (IdExpr "TYPE") (LitExpr (String "SEMI")) Nothing
        , assignStmt Nothing (IdExpr "OBJECT")
            (BinaryExpr 
                (IdExpr "TYPE") 
                Blank 
                (LitExpr (String "GROUP"))
            )
            Nothing
        ]

ast_pg5_1 = Program
        [ assignStmt Nothing (IdExpr "FIRST") (LitExpr (String "WINTER")) Nothing
        , assignStmt Nothing (IdExpr "SECOND") (LitExpr (String "SPRING")) Nothing
        , assignStmt Nothing (IdExpr "TWO_SEASONS")
            (BinaryExpr
                (BinaryExpr
                    (IdExpr "FIRST")
                    Blank
                    (LitExpr (String ","))
                )
                Blank
                (IdExpr ("SECOND"))
            )
            Nothing
        ]


ast_pg5_2 = assignStmt Nothing (IdExpr "TWO_SEASONS") (LitExpr (String "WINTER,SPRING")) Nothing

ast_pg5_3 = Program
        [ assignStmt Nothing (IdExpr "ROW") (LitExpr (String "K")) Nothing
        , assignStmt Nothing (IdExpr "NO_") (LitExpr (Int 22)) Nothing
        , assignStmt Nothing (IdExpr "SEAT") (BinaryExpr (IdExpr "ROW") Blank (IdExpr "NO_")) Nothing
        ]

ast_pg5_4 = assignStmt Nothing (IdExpr "SEAT") 
            (BinaryExpr
                (IdExpr "ROW")
                Blank
                (BinaryExpr
                    (IdExpr "NO_")
                    Plus
                    (BinaryExpr
                        (LitExpr (Int 4))
                        Slash
                        (LitExpr (Int 2))
                    )
                )
            )
            Nothing
            
ast_pg5_5 = assignStmt Nothing (IdExpr "SEAT") 
            (BinaryExpr
                (IdExpr "ROW")
                Blank
                (ParenExpr
                    (BinaryExpr
                        (IdExpr "NO_")
                        Plus
                        (ParenExpr
                            (BinaryExpr
                                (LitExpr (Int 4))
                                Slash
                                (LitExpr (Int 2))
                            )
                        )
                    )
                )
            )
            Nothing

ast_pg5_6 = assignStmt Nothing (IdExpr "SEAT") (LitExpr (String "K24")) Nothing

ast_pg5_7 = assignStmt Nothing (IdExpr "OUTPUT") (LitExpr (String "THE RESULTS ARE:")) Nothing

ast_pg5_8 = assignStmt Nothing (IdExpr "PUNCH") (IdExpr "OUTPUT") Nothing

ast_pg5_9 = Program
    [ assignStmt Nothing (IdExpr "OUTPUT") NullExpr Nothing
    , assignStmt Nothing (IdExpr "PUNCH") NullExpr Nothing
    ]

ast_pg5_10 = assignStmt Nothing (IdExpr "PUNCH") (IdExpr ("INPUT")) Nothing

ast_pg6_1 = assignStmt Nothing (IdExpr "TRADE") (LitExpr (String "PROGRAMMER")) Nothing

ast_pg6_2 = matchStmt Nothing (IdExpr "TRADE") (LitExpr (String "GRAM")) Nothing

ast_pg6_3 = assignStmt Nothing (IdExpr "PART") (LitExpr (String "GRAM")) Nothing

ast_pg6_4 = matchStmt Nothing (IdExpr "TRADE") (IdExpr "PART") Nothing

ast_pg6_5 = Program
    [ assignStmt Nothing (IdExpr "ROW") (LitExpr (String "K")) Nothing
    , assignStmt Nothing (IdExpr "NO_") (LitExpr (Int 20)) Nothing
    , matchStmt Nothing (LitExpr (String "K24"))
        (BinaryExpr
            (IdExpr "ROW")
            Blank
            (BinaryExpr
                (IdExpr "NO_")
                Plus
                (LitExpr (Int 4))
            )
        )
        Nothing
    ]

ast_pg7_1 = Program
    [ assignStmt Nothing (IdExpr "TENS") (LitExpr (Int 2)) Nothing
    , assignStmt Nothing (IdExpr "UNITS") (LitExpr (Int 5)) Nothing
    , matchStmt Nothing
        (ParenExpr (BinaryExpr (IdExpr "TENS") Blank (IdExpr "UNITS")))
        (LitExpr (Int 30))
        Nothing
    ]

ast_pg7_2 = matchStmt Nothing (IdExpr "TENS")
    (BinaryExpr
        (IdExpr "UNITS")
        Blank
        (LitExpr (Int 30))
     )
     Nothing

ast_pg7_3 = matchStmt Nothing (IdExpr "TENS")
    (ParenExpr
        (BinaryExpr
            (IdExpr "UNITS")
            Blank
            (LitExpr (Int 30))
        )
     )
     Nothing

ast_pg7_4 = assignStmt Nothing (IdExpr "WORD") (LitExpr (String "GRID")) Nothing

ast_pg7_5 = replStmt Nothing (IdExpr "WORD") (LitExpr (String "I")) (LitExpr (String "OU")) Nothing

ast_pg7_6 = replStmt Nothing (IdExpr "WORD") (LitExpr (String "AB")) (LitExpr (String "OU")) Nothing

ast_pg7_7 = Program
    [ assignStmt Nothing (IdExpr "HAND") (LitExpr (String "AC4DAHKDKS")) Nothing
    , assignStmt Nothing (IdExpr "RANK") (LitExpr (Int 4)) Nothing
    , assignStmt Nothing (IdExpr "SUIT") (LitExpr (String "D")) Nothing
    , replStmt Nothing (IdExpr "HAND") 
        (BinaryExpr (IdExpr "RANK") Blank (IdExpr "SUIT")) (LitExpr (String "AS"))
        Nothing
    ]

ast_pg7_8 = replStmt Nothing (IdExpr "HAND") (BinaryExpr (IdExpr "RANK") Blank (IdExpr "SUIT")) NullExpr Nothing

ast_pg8_1 = BinaryExpr (IdExpr "P1") Pipe (IdExpr "P2")

ast_pg8_2 = assignStmt Nothing (IdExpr "KEYWORD")
    (BinaryExpr
        (LitExpr (String "COMPUTER"))
        Pipe
        (LitExpr (String "PROGRAM"))
    )
    Nothing

ast_pg8_3 = assignStmt Nothing (IdExpr "KEYWORD")
    (BinaryExpr
        (IdExpr "KEYWORD")
        Pipe
        (LitExpr (String "ALGORITHM"))
    )
    Nothing


ast_pg8_4 = assignStmt Nothing (IdExpr "KEYWORD")
    (BinaryExpr
        (BinaryExpr
            (LitExpr (String "COMPUTER"))
            Pipe
            (LitExpr (String "PROGRAM"))
        )
        Pipe
        (LitExpr (String "ALGORITHM"))
    )
    Nothing

ast_pg8_5 = replStmt Nothing (IdExpr "TEXT") (IdExpr "KEYWORD") NullExpr Nothing

ast_pg8_6 = assignStmt Nothing (IdExpr "TEXT") (LitExpr (String "PROGRAMMING ALGORITHMS FOR COMPUTERS")) Nothing

ast_pg8_7 = assignStmt Nothing (IdExpr "TEXT") (LitExpr (String "MING ALGORITHMS FOR COMPUTERS")) Nothing

ast_pg8_8 = degenStmt Nothing (BinaryExpr (IdExpr "P1") Blank (IdExpr "P2")) Nothing

ast_pg9_1 = Program
    [ assignStmt Nothing (IdExpr "BASE") 
        (BinaryExpr
            (BinaryExpr
                (LitExpr (String "BINARY"))
                Pipe
                (LitExpr (String "DECIMAL"))
            )
            Pipe
            (LitExpr (String "HEX"))
        )
        Nothing
    , assignStmt Nothing (IdExpr "SCALE")
        (BinaryExpr
            (LitExpr (String "FIXED"))
            Pipe
            (LitExpr (String "FLOAT"))
        )
        Nothing
    , assignStmt Nothing (IdExpr "ATTRIBUTE") 
        (BinaryExpr
            (IdExpr "SCALE")
            Blank
            (IdExpr "BASE")
        )
        Nothing
    ]

ast_pg9_2 = assignStmt Nothing (IdExpr "DCL") (LitExpr (String "AREAFIXEDDECIMAL")) Nothing

ast_pg9_3 = matchStmt Nothing (IdExpr "DCL") (IdExpr "ATTRIBUTE") Nothing

ast_pg9_4 = assignStmt Nothing (IdExpr "ATTRIBUTE") 
    (BinaryExpr
        (LitExpr (String "FIXED"))
        Pipe
        (BinaryExpr
            (LitExpr (String "FLOAT"))
            Blank
            (LitExpr (String "DECIMAL"))
        )
    )
    Nothing
 
ast_pg9_5 = assignStmt Nothing (IdExpr "ATTRIBUTE") 
    (BinaryExpr
        (ParenExpr
            (BinaryExpr
                (LitExpr (String "FIXED"))
                Pipe
                (LitExpr (String "FLOAT"))
            )
        )
        Blank
        (LitExpr (String "DECIMAL"))
    )
    Nothing

ast_pg9_6 = assignStmt Nothing (IdExpr "BASE")
    (BinaryExpr
        (ParenExpr
            (BinaryExpr
                (LitExpr (String "HEX"))
                Pipe
                (LitExpr (String "DEC"))
            )
        )
        Dot
        (IdExpr "B1")
    )
    Nothing

ast_pg9_7 = assignStmt Nothing (IdExpr "A_OR_B")
    (BinaryExpr
        (IdExpr "A")
        Pipe
        (BinaryExpr
            (IdExpr "B")
            Dot
            (IdExpr "OUTPUT")
        )
    )
    Nothing

ast_pg9_8 = assignStmt Nothing (IdExpr "A_OR_B")
    (BinaryExpr
        (IdExpr "A")
        Pipe
        (ParenExpr
            (BinaryExpr
                (IdExpr "B")
                Dot
                (IdExpr "OUTPUT")
            )
        )
    )
    Nothing

ast_pg10_1 = assignStmt (Just "START") (IdExpr "TEXT") (IdExpr "INPUT") Nothing

ast_pg10_2 = assignStmt Nothing (IdExpr "TEXT") (IdExpr "INPUT") (Just (FailGoto (GotoPart (IdExpr "DONE"))))

ast_pg10_3 = Program
    [ assignStmt (Just "LOOP") (IdExpr "PUNCH") (IdExpr "INPUT") (Just (SuccessGoto (GotoPart (IdExpr ("LOOP")))))
    , EndStmt Nothing
    ]

ast_pg10_4 = Program
    [ assignStmt Nothing (IdExpr "COLOR") 
        (BinaryExpr
            (BinaryExpr
                (LitExpr (String "RED")) 
                Pipe
                (LitExpr (String "GREEN"))
            )
            Pipe
            (LitExpr (String "BLUE"))
        )
        Nothing
    , replStmt (Just "BRIGHT") (IdExpr "TEST") (IdExpr "COLOR") NullExpr
        (Just
            (BothGoto
                (GotoPart (IdExpr "BRIGHT"))
                (GotoPart (IdExpr "BLAND"))
            )
        )
    , Stmt (Just "BLAND") Nothing Nothing Nothing Nothing
    ]

ast_pg11_1 = Program
    [ assignStmt (Just "LOOP") (IdExpr "PUNCH") (IdExpr "INPUT") (Just (FailGoto (GotoPart (IdExpr "END"))))
    , assignStmt Nothing (IdExpr "OUTPUT") (IdExpr "PUNCH") (Just (Goto (GotoPart (IdExpr "LOOP"))))
    , EndStmt Nothing
    ]

ast_pg11_2 = assignStmt Nothing (IdExpr "MONTH") (LitExpr (String "APRIL")) Nothing

ast_pg11_3 = assignStmt Nothing (PrefixExpr Dollar (IdExpr "MONTH")) (LitExpr (String "CRUEL")) Nothing

ast_pg11_4 = assignStmt Nothing (IdExpr "APRIL") (LitExpr (String "CRUEL")) Nothing

ast_pg11_5 = Program
    [ assignStmt Nothing (IdExpr "WORD") (LitExpr (String "RUN")) Nothing
    , assignStmt Nothing
        (PrefixExpr Dollar
            (ParenExpr
                (BinaryExpr
                    (IdExpr "WORD")
                    Blank
                    (LitExpr (String ":"))
                )
            )
        )
        (BinaryExpr
             (PrefixExpr Dollar
                (ParenExpr
                    (BinaryExpr
                        (IdExpr "WORD")
                        Blank
                        (LitExpr (String ":"))
                    )
                )
            )
            Plus
            (LitExpr (Int 1))
        )
        Nothing
    ]

ast_pg11_6 = PrefixExpr Dollar
    (ParenExpr
        (BinaryExpr
            (LitExpr (String "A"))
            Pipe
            (LitExpr (String "B"))
        )
    )

ast_pg11_7 = assignStmt Nothing (IdExpr "N")
    (BinaryExpr
        (IdExpr "N")
        Plus
        (LitExpr (Int 1))
    )
    (Just
        (Goto
            (GotoPart
                (PrefixExpr Dollar
                    (ParenExpr
                        (BinaryExpr
                            (LitExpr (String "PHASE"))
                            Blank
                            (IdExpr "N")
                        )
                     )
                 )
             )
         )
     )

ast_pg12_1 = Program
    [ assignStmt Nothing (IdExpr "APE") (LitExpr (String "SIMIAN")) Nothing
    , assignStmt Nothing (IdExpr "OUTPUT") (CallExpr "SIZE" [IdExpr "APE"]) Nothing
    ]

ast_pg12_2 = Program
    [ assignStmt Nothing (IdExpr "N") (LitExpr (Int 100)) Nothing
    , assignStmt Nothing (IdExpr "OUTPUT") (CallExpr "SIZE"
        [BinaryExpr
            (LitExpr (String "PART"))
            Blank
            (BinaryExpr
                (IdExpr "N")
                Plus
                (LitExpr (Int 4))
            )
        ])
        Nothing
    ]

ast_pg12_3 = degenStmt Nothing (CallExpr "DUPL"
    [ LitExpr (String "/*")
    , LitExpr (Int 5)
    ])
    Nothing


ast_pg20_5 = Program
    [ assignStmt Nothing (PrefixExpr And (IdExpr "TRIM")) (LitExpr (Int 1)) Nothing
    , assignStmt Nothing (IdExpr "CHAR")
        (BinaryExpr
            (CallExpr "LEN" [LitExpr (Int 1)])
            Dot
            (IdExpr "CH")
        )
        Nothing
    , assignStmt Nothing (IdExpr "LETTERS") (LitExpr (String "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) Nothing
    , assignStmt Nothing (IdExpr "COUNT") (CallExpr "TABLE" [LitExpr (Int 30)]) Nothing
    , assignStmt (Just "READ") (IdExpr "OUTPUT") (IdExpr "INPUT") (Just (FailGoto (GotoPart (IdExpr "DISPLAY"))))
    , assignStmt Nothing (IdExpr "TEXT") (IdExpr "OUTPUT") Nothing
    , replStmt (Just "NEXT") (IdExpr "TEXT") (IdExpr "CHAR") NullExpr (Just (FailGoto (GotoPart (IdExpr "READ"))))
    , assignStmt Nothing (RefExpr "COUNT" [IdExpr "CH"])
        (BinaryExpr
            (RefExpr "COUNT" [IdExpr "CH"])
            Plus
            (LitExpr (Int 1))
        )
        (Just (Goto (GotoPart (IdExpr "NEXT"))))
    , assignStmt (Just "DISPLAY") (IdExpr "OUTPUT") NullExpr Nothing
    , replStmt (Just "LOOP") (IdExpr "LETTERS") (IdExpr "CHAR") NullExpr (Just (FailGoto (GotoPart (IdExpr "END"))))
    , assignStmt Nothing (IdExpr "OUTPUT") 
        (BinaryExpr
            (BinaryExpr
                (BinaryExpr
                    (BinaryExpr
                        (CallExpr "NE" [RefExpr "COUNT" [IdExpr "CH"]])
                        Blank
                        (IdExpr "CH")
                    )
                    Blank
                    (LitExpr (String " OCCURS "))
                )
                Blank
                (RefExpr "COUNT" [IdExpr "CH"])
            )
            Blank
            (LitExpr (String " TIMES"))
        )
        (Just (Goto (GotoPart (IdExpr "LOOP"))))
    , EndStmt Nothing
    ]
