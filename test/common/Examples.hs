module Examples where

ex_pg1_1 = " V = 5\n"

ex_pg1_2 = " W = 14 + (16 - 10)\n"

ex_pg1_3 = " V = \'DOG\'\n"

ex_pg2_1 = " RESULT = ANS_1\n"

ex_pg2_2 = " M = 4\n N = 5\n P = N * M / (N - 1)\n"

ex_pg2_3 = " Q2 = -P / -N\n"

ex_pg2_4 = " X = 2 ** 3 ** 2\n"

ex_pg2_5 = " X = 2 ** (3 ** 2)\n"

ex_pg3_1 = " PI = 3.14159\n CIRUM = 2. * PI * 5.\n"

ex_pg3_2 = " SUM = 16.4 + 2\n"

ex_pg3_3 = " SCREAM = \'HELP\'\n"

ex_pg3_4 = " PLEA = \'HE SHOUTED, \"HELP.\"\'\n QUOTE = \'\"\'\n APOSTROPHE = \"\'\"\n"

ex_pg4_1 = " NULL =\n"

ex_pg4_2 = " Z = \'10\'\n X = 5 * -Z + \'10.6\'\n"

ex_pg4_3 = "1,253,465"

ex_pg4_4 = ".364 E-03"

ex_pg4_5 = " TYPE = \'SEMI\'\n OBJECT = TYPE \'GROUP\'\n"

ex_pg5_1 = " FIRST = \'WINTER\'\n SECOND = \'SPRING\'\n TWO_SEASONS = FIRST \',\' SECOND\n"

ex_pg5_2 = " TWO_SEASONS = \'WINTER,SPRING\'\n"

ex_pg5_3 = " ROW = \'K\'\n NO_ = 22\n SEAT = ROW NO_\n"

ex_pg5_4 = " SEAT = ROW NO_ + 4 / 2\n"

ex_pg5_5 = " SEAT = ROW (NO_ + (4 / 2))\n"

ex_pg5_6 = " SEAT = \'K24\'\n"

ex_pg5_7 = " OUTPUT = \'THE RESULTS ARE:\'\n"

ex_pg5_8 = " PUNCH = OUTPUT\n"

ex_pg5_9 = " OUTPUT =\n PUNCH =\n"

ex_pg5_10 = " PUNCH = INPUT\n"

ex_pg6_1 = " TRADE = \'PROGRAMMER\'\n"

ex_pg6_2 = " TRADE \'GRAM\'\n"

ex_pg6_3 = " PART = \'GRAM\'\n"

ex_pg6_4 = " TRADE PART\n"

ex_pg6_5 = " ROW = \'K\'\n NO_ = 20\n \'K24\' ROW NO_ + 4\n"

ex_pg7_1 = " TENS = 2\n UNITS = 5\n (TENS UNITS) 30\n"

ex_pg7_2 = " TENS UNITS 30\n"

ex_pg7_3 = " TENS (UNITS 30)\n"

ex_pg7_4 = " WORD = \'GRID\'\n"

ex_pg7_5 = " WORD \'I\' = \'OU\'\n"

ex_pg7_6 = " WORD \'AB\' = \'OU\'\n"

ex_pg7_7 = " HAND = \'AC4DAHKDKS\'\n RANK = 4\n SUIT = \'D\'\n HAND RANK SUIT = \'AS\'\n"

ex_pg7_8 = " HAND RANK SUIT = \n"

ex_pg8_1 = "P1 | P2"

ex_pg8_2 = " KEYWORD = \'COMPUTER\' | \'PROGRAM\'\n"

ex_pg8_3 = " KEYWORD = KEYWORD | \'ALGORITHM\'\n"
    
ex_pg8_4 = " KEYWORD = \'COMPUTER\' | \'PROGRAM\' | \'ALGORITHM\'\n"

ex_pg8_5 = " TEXT KEYWORD =\n"

ex_pg8_6 = " TEXT = \'PROGRAMMING ALGORITHMS FOR COMPUTERS'\n"

ex_pg8_7 = " TEXT = \'MING ALGORITHMS FOR COMPUTERS\'\n"

ex_pg8_8 = " P1 P2\n"

ex_pg9_1 = " BASE = \'BINARY\' | \'DECIMAL\' | \'HEX\'\n SCALE = \'FIXED\' | \'FLOAT\'\n ATTRIBUTE = SCALE BASE\n"

ex_pg9_2 = " DCL = \'AREAFIXEDDECIMAL'\n"

ex_pg9_3 = " DCL ATTRIBUTE\n"

ex_pg9_4 = " ATTRIBUTE = 'FIXED' | 'FLOAT' 'DECIMAL'\n"

ex_pg9_5 = " ATTRIBUTE = ('FIXED' | 'FLOAT') 'DECIMAL'\n"

ex_pg9_6 = " BASE = ('HEX' | 'DEC') . B1\n"

ex_pg9_7 = " A_OR_B = A | B . OUTPUT\n"

ex_pg9_8 = " A_BOR_B = A | (B . OUTPUT)\n"

ex_pg10_1 = "START TEXT = INPUT\n"

ex_pg10_2 = " TEXT = INPUT : F(DONE)\n"

ex_pg10_3 = "LOOP PUNCH = INPUT :S(LOOP)\nEND\n"

ex_pg10_4 = " COLOR = \'RED\' | \'GREEN\' | \'BLUE\'\nBRIGHT TEST COLOR = :S(BRIGHT)F(BLAND)\nBLAND\n"

ex_pg11_1 = "LOOP PUNCH = INPUT :F(END)\n OUTPUT = PUNCH :(LOOP)\nEND\n"

ex_pg11_2 = " MONTH = \'APRIL\'\n"

ex_pg11_3 = " $MONTH = \'CRUEL\'\n"

ex_pg11_4 = " APRIL = \'CRUEL\'\n"

ex_pg11_5 = " WORD = \'RUN\'\n $(WORD \':\') = $(WORD \':\') + 1\n"

ex_pg11_6 = "$(\'A\' | \'B\')"

ex_pg11_7 = " N = N + 1 :($(\'PHASE\' N))\n"

ex_pg12_1 = " APE = \'SIMIAN\'\n OUTPUT = SIZE(APE)\n"

ex_pg12_2 = " N = 100\n OUTPUT = SIZE(\'PART\' N + 4)\n"

ex_pg12_3 = " DUPL(\'/*\',5)\n"

ex_pg12_4 = " OUTPUT = DUPL(\' \',40 - SIZE(S)) S\n"

ex_pg13_1 = " STATEMENT = \'A(I,J) = A(I,J) + 3\'\n OUTPUT = REPLACE(STATEMENT,'()','<>')\n"

ex_pg13_2 = " INPUT LEN(40) . PUNCH\n"

ex_pg13_4 = " PUNCH = LE(SIZE(TEXT),80) TEXT\n"

ex_pg13_5 = " SUM = 0\n N = 0\nADD N = LT(N,50) N + 1 :F(DONE)\n SUM = SUM + N\nDONE OUTPUT = SUM\n"

ex_pg14_1 = " OUTPUT = DIFFER(FIRST,SECOND) FIRST SECOND\n"

ex_pg14_2 = " PUNCH = DIFFER(TEXT) TEXT\n"

ex_pg14_3 = " OUTPUT = LGT(TEXT1,TEXT2) TEXT2 :S(SKIP)\n OUTPUT = TEXT1\n OUTPUT = TEXT2 :(JUMP)\nSKIP OUTPUT = TEXT1\nJUMP\n"

ex_pg14_4 = " DEFINE(\'DELETE(STRING,CHAR)\',\'D1\')\n"

ex_pg15_1 = "D1 STRING CHAR = :S(D1)\n DELETE = STRING :(RETURN)\n"

ex_pg15_2 = " DEFINE(\'DELETE(STRING,CHAR)\')\n"

ex_pg15_3 = "DELETE STRING CHAR = :S(DELETE)\n DELETE = STRING :(RETURN)\n"

ex_pg15_4 = " MAGIC = 'ABRACADABRA'\n OUTPUT = DELETE(MAGIC,\'A')\n"

ex_pg15_5 = " TEXT = DELETE(DELETE(INPUT,\'.\'),\' \')\n"

ex_pg15_6 = "DELETE STRING CHAR = :F(RETURN)\nD2 STRING CHAR = :S(D2)\n DELETE = STRING :(RETURN)\n"

ex_pg15_7 = " PUNCH = DELETE(INPUT,\'*\')\n"

ex_pg16_1 = " PAT = MAXNO(\'A\' | \'B\' | \'C\' ,2)\n"

ex_pg16_2 = " \'EBCDIC\' PAT \'D'\n"

ex_pg16_3 = " DEFINE(\'MAXNO(P,N)\')\n"

ex_pg16_4 = "MAXNO N = GT(N,0) N - 1 :F(RETURN)\n MAXNO = NULL | P MAXNO :(MAXNO)\n"

ex_pg16_5 = " DEFINE(\'REVERSE(STRING)\',\'R1\')\n"

ex_pg16_6 = "R1 ONECH = LEN(1) . CH\nR2 STRING ONECH = :F(RETURN)\n REVERSE = CH REVERSE :(R2)\n"

ex_pg16_7 = " DEFINE(\'REVERSE(STRING)ONECH,CH\',\'R1\')\n"

ex_pg16_8 = " DEFINE(\'C(N,M)\')\n"

ex_pg17_1 = "C M = LT(N - M,M) N - M\n C = EQ(M,0) 1 :S(RETURN)\n C = N * C(N - 1,M - 1) / M :(RETURN)\n"

ex_pg17_2 = " DEFINE(\'COMB(STR,N,HEAD)CH\')\n"

ex_pg17_3 = "COMB OUTPUT = EQ(N,0) HEAD :S(RETURN)\nC2 STR LE(NE,SIZE(STR)) LEN(1) . CH - :F(RETURN)\n COMB(STR,N - 1,HEAD CH) :(C2)\n"

ex_pg17_4 = " COMB(\'ABCD',3)\n"

ex_pg17_5 = " &DUMP = 1\n"

ex_pg17_6 = " &TRIM = 1\n"

ex_pg18_1 = " V = ARRAY(10,1.0)\n"

ex_pg18_2 = " N = ARRAY(\'3,5\')\n"

ex_pg18_3 = " &TRIM = 1\n A = ARRAY(INPUT)\n"

ex_pg18_4 = " &TRIM = 1\n I = 1\n ST = ARRAY(INPUT)\nMORE ST<I> = INPUT :F(GO)\n I = I + 1 :(MORE)\nGO\n"

ex_pg19_1 = " T = TABLE()\n"

ex_pg19_2 = " T<\'A\'> = 5\n"

ex_pg19_3 = " T<WORD> = T<WORD> + 1\n"

ex_pg19_4 = " DATA(\'NODE(VALUE,LINK)\')\n"

ex_pg19_5 = " P = NODE(\'S\',)\n"

ex_pg20_1 = " P = NODE(\'T\',P)\n"

ex_pg20_2 = " P = LINK(P)\n"

ex_pg20_3 = " OUTPUT = \'THE TOTAL NUMBER OF OCCURENCES IF \'\n+ SUM<N>\n"

ex_pg20_4 = " X = 2; Y = 3; Z = 10\n"

ex_pg20_5 = unlines
    [ "********************************************************************************"
    , "*                                                                              *"
    , "*         THIS PROGRAM COUNTS THE NUMBER OF TIMES EACH                         *"
    , "*         LETTER IS USED IN INPUT TEXT.                                        *"
    , "*                                                                              *"
    , "********************************************************************************"
    , "          &TRIM     = 1"
    , "          CHAR      = LEN(1) . CH"
    , "          LETTERS   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'    "
    , "          COUNT     = TABLE(30)"
    , "READ      OUTPUT    = INPUT                       :F(DISPLAY)"
    , "          TEXT      = OUTPUT"
    , "NEXT      TEXT      CHAR  =                       :F(READ)"
    , "          COUNT<CH> =  COUNT<CH>  +  1            :(NEXT)"
    , "DISPLAY   OUTPUT    ="
    , "LOOP      LETTERS   CHAR  =                       :F(END)"
    , "          OUTPUT    = NE(COUNT<CD>)  CH  ' OCCURS ' COUNT<CD> 'TIMES '"
    , "+                                                 :(LOOP)"
    , "END"
    ]



