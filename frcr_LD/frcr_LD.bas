$LIBRARY <shell32.lib>

GLOBAL Fsize AS UINT
global offset2 as uint
global offset3 as uint
GLOBAL RAW FP2 AS FILE
GLOBAL RAW FP3 AS FILE
GLOBAL File_Chiave$ AS STRING
GLOBAL File_Orig$ AS STRING
GLOBAL File_Dest$ AS STRING
'valore presente in cima dei files crittografati
GLOBAL Signature * 4 + 1
Signature = "FRCR"

IF COMMAND$ = ""  OR COMMAND$(1) = "?" THEN
    PRINT "FrCr_LD: Fractal Crypter by Stefano Trevisani (c) 2007 Ver 0.1"
    PRINT " Usage: FrCr_LD infile outfile keyfile [option]"
    PRINT " [-c] Crypt"
    PRINT " [-d] DeCrypt"
    PRINT BEL$
    END
END IF

'pathname file da cifrare o
'pathname file cifrato da decifrare
File_Orig$ = COMMAND$(1)
IF NOT EXIST (File_Orig$) THEN
    PRINT "Error: ", MCASE$(File_Orig$), " was not found."
    PRINT BEL$
    END
END IF

'pathname file cifrato o
'pathname file decifrato
IF COMMAND$(2) <> "" THEN
    File_Dest$ = COMMAND$(2)
ELSE
    PRINT "Error: outfile not specified"
    PRINT BEL$
    END
END IF

'verifico che il file sorgente sia diverso dal file destinazione
IF LCASE$(File_Orig$) = LCASE$(File_Dest$) THEN
    PRINT "Error: infile is equal outfile process not possible"
    PRINT BEL$
    END
END IF

'pathname file chiave
IF COMMAND$(3) <> "" THEN
    File_Chiave$ = COMMAND$(3)
    IF NOT EXIST (File_Chiave$) THEN
        PRINT "Error: ", MCASE$(File_Chiave$), " was not found."
        PRINT BEL$
        END
    END IF
ELSE
    PRINT "Error: keyfile not specified"
    PRINT BEL$
    END
END IF

IF COMMAND$(4) <> "" THEN
    IF COMMAND$(4) = "-c" THEN
        CALL Cripta
    ELSEIF COMMAND$(4) = "-d" THEN
        CALL DeCripta
    ELSE
        PRINT "Error: option not valid"
        PRINT BEL$
        END
    END IF
ELSE
    PRINT "Error: option not specified"
    PRINT BEL$
    END
END IF

END

'---------------------------------------------

SUB Cripta
'leggi la chiave
    CALL LeggiChiave

'verifico i valori della chiave per prevenire un errore di divisione per zero
    IF (cx2 = cx1) OR (cy2 = cy1) THEN
        PRINT "Error: key ", MCASE$(File_Chiave$), " not valid."
        PRINT BEL$
        END
    END IF

'determino le dimensioni del file da cifrare
    Fsize = LOF(File_Orig$)

'trasformo la dimensione del file in dimensioni window
    CALL RangeWindow

'apro il file dati da cifrare
    offset2 = 0
    OPEN File_Orig$ FOR BINARY AS FP2
    IF LeggiSignature() = True THEN
        PRINT "Error: ", MCASE$(File_Orig$), " is crypted."
        PRINT BEL$
'chiudo il file dati da cifrare
        CLOSE FP2
        END
    ELSE
'ripristino offset
        offset2 = 0
        SEEK FP2, offset2
    END IF

'creo il file dati cifrato
    offset3 = 0
    OPEN File_Dest$ FOR BINARY NEW AS FP3
    CALL ScriviSignature
    CALL ScriviSize

    CALL CodificaMandelbrot

'chiudo il file dati da cifrare
    CLOSE FP2

'chiudo il file dati cifrato
    CLOSE FP3
END SUB

SUB DeCripta
'leggi la chiave
    CALL LeggiChiave

'verifico i valori della chiave per prevenire un errore di divisione per zero
    IF (cx2 = cx1) OR (cy2 = cy1) THEN
        PRINT "Error: key ", MCASE$(File_Chiave$), " not valid."
        PRINT BEL$
        END
    END IF

'apro il file dati da decifrare
    offset2 = 0
    OPEN File_Orig$ FOR BINARY AS FP2
    IF LeggiSignature() = False THEN
        PRINT "Error: ", MCASE$(File_Orig$), " is not crypted."
        PRINT BEL$
'chiudo il file dati da decifrare
        CLOSE FP2
        END
    END IF

'determino le dimensioni del file da decifrare
    CALL DimFileCript

'trasformo la dimensione del file in dimensioni window
    CALL RangeWindow

'creo il file dati decifrato
    offset3 = 0
    OPEN File_Dest$ FOR BINARY NEW AS FP3

    CALL DeCodificaMandelbrot

'chiudo il file dati da decifrare
    CLOSE FP2

'chiudo il file dati decifrato
    CLOSE FP3

END SUB

'algoritmo di calcolo
SUB CodificaMandelbrot
    local c as uint
    local r as uint
    local i as ushort
    local x as LDOUBLE
    local y as LDOUBLE
    local a as LDOUBLE
    local b as LDOUBLE
    local d as LDOUBLE
    local e as LDOUBLE
    local dx as LDOUBLE
    local dy as LDOUBLE

    c=0
    r=0
    i=0
    dx=(cx2-cx1)/(o)
    dy=(cy2-cy1)/(v)

    aggiorna:
    x=cx1+dx*c
    a=x
    y=cy1+dy*r
    b=y

    if (a*a+b*b) >= 4.0L then
        Cifra(0)
        goto esterno
    end if

    cicla:
    e=a*a-b*b+x

    b=a*b
    b=b+b+y

    a=e
    d=a*a+b*b
    if d >= 4.0L then goto colora

    if i>t then
        Cifra(65535)
        goto interno
    end if

    i++
    goto cicla

    colora:
    Cifra(i)

    esterno:
    interno:

    if r=v+1 then goto wexit
'passo il controllo al windows per gestione eventi
    DOEVENTS

    if c=o then
        c=0
        r++
        i=0
    else
        c++
        i=0
    end if

    goto aggiorna

    wexit:
END SUB

'algoritmo di calcolo
SUB DeCodificaMandelbrot
    local c as uint
    local r as uint
    local i as ushort
    local x as LDOUBLE
    local y as LDOUBLE
    local a as LDOUBLE
    local b as LDOUBLE
    local d as LDOUBLE
    local e as LDOUBLE
    local dx as LDOUBLE
    local dy as LDOUBLE
    
    c=0
    r=0
    i=0
    dx=(cx2-cx1)/(o)
    dy=(cy2-cy1)/(v)

    aggiorna:
    x=cx1+dx*c
    a=x
    y=cy1+dy*r
    b=y

    if (a*a+b*b) >= 4.0L then
        DeCifra(0)
        goto esterno
    end if

    cicla:
    e=a*a-b*b+x

    b=a*b
    b=b+b+y

    a=e
    d=a*a+b*b
    if d >= 4.0L then goto colora

    if i>t then
        DeCifra(65535)
        goto interno
    end if

    i++
    goto cicla

    colora:
    DeCifra(i)

    esterno:
    interno:

    if r=v+1 then goto wexit
'passo il controllo al windows per gestione eventi
    DOEVENTS

    if c=o then
        c=0
        r++
        i=0
    else
        c++
        i=0
    end if

    goto aggiorna

    wexit:
END SUB

'Cifratura della word
SUB Cifra(Iterazioni AS USHORT)
'valore cifrato 16bit unsigned 0-65535
    GLOBAL RetValCifrato AS USHORT
'valore da cifrare 16bit unsigned 0-65535
    GLOBAL Valore AS USHORT
'legge il prossimo valore da cifrare
    CALL LeggiValore
'Cifratura tramite XOR
    RetValCifrato = Valore XOR Iterazioni
'scrivo valore cifrato
    CALL ScriviValore(RetValCifrato)
END SUB

'DeCifratura della word
SUB DeCifra(Iterazioni AS USHORT)
'valore decifrato 16bit unsigned 0-65535
    GLOBAL RetValDeCifrato AS USHORT
'valore da decifrare 16bit unsigned 0-65535
    GLOBAL ValoreD AS USHORT
'legge il prossimo valore da decifrare
    CALL LeggiValoreD
'DeCifratura tramite XOR
    RetValDeCifrato = ValoreD XOR Iterazioni
'scrivo valore decifrato
    CALL ScriviValoreCS(RetValDeCifrato)
END SUB

SUB ScriviValore(RetValCifrato AS USHORT)
'valore cifrato 16bit unsigned 0-65535
    LOCAL A$ * 1 + 1
    memset(A$, (65280 BAND RetValCifrato) >> 8 , 1)
    SEEK FP3, offset3
    PUT$  FP3, A$ , 1
    offset3++
    memset(A$, (255 BAND RetValCifrato), 1)
    SEEK FP3, offset3
    PUT$  FP3, A$ , 1
    offset3++
END SUB

SUB LeggiValore
'valore da cifrare 16bit unsigned 0-65535
    LOCAL A$ * 1 + 1
    LOCAL yog AS UCHAR

    IF offset2 < Fsize THEN
        SEEK FP2, offset2
        GET$  FP2, A$ , 1
        yog = A[0]
        Valore = yog
        offset2++
    ELSE
        Valore = 0
    END IF

    IF offset2 < Fsize THEN
        SEEK FP2, offset2
        GET$  FP2, A$ , 1
        yog = A[0]
        Valore = (Valore << 8) + yog
        offset2++
    ELSE
        Valore = (Valore << 8)
    END IF
END SUB

SUB ScriviSignature
'scrivo signature
    SEEK FP3, offset3
    PUT$  FP3, Signature , 4
    offset3 = offset3 + 4
END SUB

SUB ScriviSize
'scrivo file size in bytes in 32bit
    LOCAL B$ * 1 + 1
    memset(B$, (4278190080 BAND Fsize) >> 24 , 1)
    SEEK FP3, offset3
    PUT$  FP3, B$ , 1
    offset3++
    memset(B$, (16711680 BAND Fsize) >> 16 , 1)
    SEEK FP3, offset3
    PUT$  FP3, B$ , 1
    offset3++
    memset(B$, (65280 BAND Fsize) >> 8 , 1)
    SEEK FP3, offset3
    PUT$  FP3, B$ , 1
    offset3++
    memset(B$, (255 BAND Fsize), 1)
    SEEK FP3, offset3
    PUT$  FP3, B$ , 1
    offset3++
END SUB

SUB ScriviValoreCS(RetValCifrato AS USHORT)
'valore decifrato 16bit unsigned 0-65535
    LOCAL A$ * 1 + 1
    IF offset3 < Fsize THEN
        memset(A$, (65280 BAND RetValDeCifrato) >> 8 , 1)
        SEEK FP3, offset3
        PUT$  FP3, A$ , 1
        offset3++
    END IF
    IF offset3 < Fsize THEN
        memset(A$, (255 BAND RetValDeCifrato), 1)
        SEEK FP3, offset3
        PUT$  FP3, A$ , 1
        offset3++
    END IF
END SUB

SUB LeggiValoreD
'valore da decifrare 16bit unsigned 0-65535
    LOCAL A$ * 1 + 1
    LOCAL yog AS UCHAR

    SEEK FP2, offset2
    GET$  FP2, A$ , 1
    yog = A[0]
    ValoreD = yog
    offset2++

    SEEK FP2, offset2
    GET$  FP2, A$ , 1
    yog = A[0]
    ValoreD = (ValoreD << 8) + yog
    offset2++
END SUB

FUNCTION LeggiSignature() AS integer
'salto signature
    LOCAL A$ * 4 + 1
    SEEK FP2, offset2
    GET$  FP2, A$ , 4
    offset2 = offset2 + 4
    IF A$ = Signature THEN
        FUNCTION = True
    ELSE
        FUNCTION = False
    END IF
END FUNCTION

'determino le dimensioni del file da decifrare
SUB DimFileCript
'leggo dimensione file di origine in bytes in 32bit
    LOCAL B$ * 1 + 1
    LOCAL bubu AS UCHAR

    SEEK FP2, offset2
    GET$  FP2, B$ , 1
    bubu = B[0]
    Fsize = bubu
    offset2++

    SEEK FP2, offset2
    GET$  FP2, B$ , 1
    bubu = B[0]
    Fsize = (Fsize << 8) + bubu
    offset2++

    SEEK FP2, offset2
    GET$  FP2, B$ , 1
    bubu = B[0]
    Fsize = (Fsize << 8) + bubu
    offset2++

    SEEK FP2, offset2
    GET$  FP2, B$ , 1
    bubu = B[0]
    Fsize = (Fsize << 8) + bubu
    offset2++
END SUB

'Determina la misura della window in funzione del
'coefficiente di forma ed la dimensione del file
SUB RangeWindow
'coefficiente di forma
    LOCAL cforma AS LDOUBLE
    cforma = (cx2-cx1)/(cy2-cy1)

'trasformo in valore assoluto il coefficiente di forma
    IF cforma < 0.0L THEN cforma = -cforma

'ricavo le dimensioni in word del file
    LOCAL FWord AS UINT
    FWord = Round(Fsize/2.0L,0)

'calcolo dimensioni window
'ymassimo window
    v = Round((FWord / cforma)^0.5L,0)
'xmassimo window
    o = Round((FWord * cforma)^0.5L,0)
'tratto caso particolare
    IF (o * v) < FWord THEN
        DIM o1 AS UINT
        DIM v1 AS UINT
        o1 = o + 1
        v1 = v + 1
        IF (o1 * v) > (o * v1) THEN
            IF (o * v1) >= FWord THEN
                v = v1
            ELSE
                o = o1
            END IF
        ELSE
            IF (o1 * v) >= FWord THEN
                o = o1
            ELSE
                v = v1
            END IF
        END IF
        IF (o * v) < FWord THEN
            o = o1
            v = v1
        END IF
    END IF
'aggiusto errore di fuori di uno
'il valore massimo da 0 a o da 0 a v
    o--
    v--
END SUB

'leggi la chiave
SUB LeggiChiave
    LOCAL RAW FP1 AS FILE
    LOCAL linea$ AS string

    global cx1 as LDOUBLE
    global cy1 as LDOUBLE
    global cx2 as LDOUBLE
    global cy2 as LDOUBLE

    global o as UINT
    global v as UINT
    global t as USHORT

'apro il file della chiave e carico i parametri
'32000
'-3.9142157862540031985339297087507860355827593250082712954915982158885863568966009731070476718173267727E-1
'-6.6114693189629083627164338008922440554399104567421810504009627209667837101255945600475349976932193385E-1
'-3.9142157862540031985339297087390597342525248935949253379611681636939763067576677048678424064576120145E-1
'-6.6114693189629083627164338008828630143757229116115042843766186792110956700144479454561707854054475319E-1

    OPEN File_Chiave$ FOR INPUT AS FP1
'numero di iterazioni
    IF NOT EOF(FP1) THEN LINE INPUT FP1, linea$
    t = val(linea$)
'reale minimo
    IF NOT EOF(FP1) THEN LINE INPUT FP1, linea$
    cx1 = vall(linea$)
'reale massimo
    IF NOT EOF(FP1) THEN LINE INPUT FP1, linea$
    cy1 = vall(linea$)
'immaginario minimo
    IF NOT EOF(FP1) THEN LINE INPUT FP1, linea$
    cx2 = vall(linea$)
'immaginario massimo
    IF NOT EOF(FP1) THEN LINE INPUT FP1, linea$
    cy2 = vall(linea$)

    CLOSE FP1
END SUB

