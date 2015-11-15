'***********************************************************
'primi test dell'algoritmo descritto in un articolo della
'rivista le scienze "scientific of american" per mezzo di
'una calcolatrice sharp programmabile e foglio millimetrato
'con matite colorate intorno al 1985
'scritto intorno al 1986 per ZXSpectrum in basic ed in
'assembler per cpu Z80 a 3 MHZ con una risoluzione video di
'176x256 monocromatica (non avevo la TV a colori...sig!)
'portato su PC cpu 486 a 50 MHZ con qbasic intorno al 1990
'con risoluzione video VGA 640x480 a 16 colori
'Mandelbrot su windows con 16777216 colori
'Mandelbrot v.1.01 (c) Stefano Trevisani 17-08-2006 porting in iBasic
'Mandelbrot v.1.01 (c) Stefano Trevisani 29-10-2006 porting in BCX
'***********************************************************

$ONENTRY  "echo Compiling and Linking $FILE$"
$ONEXIT   "Lrc mandelbrot.Rc"                       ' create the resource file
$ONEXIT   "lw $FILE$ mandelbrot.Res"    ' after translation, compile and link
$ONEXIT   "$FILE$"

GUI "Mandelbrot_Q", PIXELS, ICON, 100

$LIBRARY <shell32.lib>
$LIBRARY <qfloat.lib>
#include <qfloat.h>

dim cx1 as qfloat
dim cy1 as qfloat
dim cx2 as qfloat
dim cy2 as qfloat
dim dx as qfloat
dim dy as qfloat

dim qcx1$[255] as char
dim qcy1$[255] as char
dim qcx2$[255] as char
dim qcy2$[255] as char

dim x1a as qfloat
dim y1a as qfloat
dim x2a as qfloat
dim y2a as qfloat

global xa as integer
global ya as integer
global xb as integer
global yb as integer

GLOBAL Form1 AS HWND
global nero as integer
global bianco as integer
global nc as integer
global o as integer
global v as integer
global t as integer

GLOBAL Bmp1  AS CONTROL
global ExePath$ as string
global Start$ as string
global Temp$ as string
global TempStamp$ as string
global ris_vid$ as string

SUB FORMLOAD

'percorso file eseguibile
ExePath$ = APPEXEPATH$

'file con immagine di partenza
'percorso file "Start.bmp"
Start$ = ExePath$ + "Start.bmp"

'file temporaneo con bitmap visualizzata correntemente
'percorso file "Temp.bmp"
Temp$ = ExePath$ + "temp.bmp"

'file di stampa temporaneo
'percorso file "TempStamp.bmp"
TempStamp$ = ExePath$ + "TempStamp.bmp"

'numero massimo di colori utilizzati
nc=rgb(255,255,255)

'nero
nero=rgb(0,0,0)

'bianco
bianco=rgb(255,255,255)

'inserimento dei dati di partenza
'xmassimo window
o = 319
'ymassimo window
v = 239
'numero di iterazioni
t = 125
'reale minimo
cx1 = atoq("-2.0")
'reale massimo
cx2 = atoq("2.0")
'immaginario minimo
cy1 = atoq("-1.5")
'determino immaginario massimo in cosiderazione degli altri dati immessi
'salvaguardo fattore di scalatura su ambo gli assi cartesiani
'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
cy2 = atoq("1.5")

   Form1 = BCX_FORM("Mandelbrot v1.01 (c)2006 by Stefano Trevisani", 0, 0, 332, 286)
   'ModStyle (Form1,WS_MAXIMIZEBOX | WS_MINIMIZEBOX, 0, FALSE)

   'trucco per evitare ricalcolo
   COPYFILE Start$, Temp$, FALSE
   dx=(cx2-cx1)/(o+1)
   dy=(cy2-cy1)/(v+1)
  
   Bmp1  = BCX_BITMAP(Temp$ , Form1, 0, 0, 0, 332, 286)
   BCX_SET_FORM_COLOR(Form1,nero)
   AddMenu(Form1)
   CENTER(Form1)
   SHOW(Form1)
   'CALL DrawMandelbrot
END SUB

FUNCTION AddMenu(hwndOwner AS HWND) AS BOOL
CONST  ID_MENU0 = 9000
CONST  ID_MENU1 = 9001
CONST  ID_MENU2 = 9002
CONST  ID_MENU3 = 9003
CONST  ID_MENU4 = 9004
CONST  ID_MENU5 = 9005
CONST  ID_MENU6 = 9006
CONST  ID_MENU7 = 9007
CONST  ID_MENU8 = 9008
CONST  ID_MENU9 = 9009
CONST  ID_MENU10 = 9010
CONST  ID_MENU11 = 9011
CONST  ID_MENU12 = 9012
CONST  ID_MENU13 = 9013
CONST  ID_MENU14 = 9014
CONST  ID_MENU15 = 9015
CONST  ID_MENU16 = 9016
CONST  ID_MENU17 = 9017
CONST  ID_MENU18 = 9018
CONST  ID_MENU19 = 9019
CONST  ID_MENU20 = 9020
CONST  ID_MENU21 = 9021
CONST  ID_MENU22 = 9022
CONST  ID_MENU23 = 9023
CONST  ID_MENU24 = 9024
CONST  ID_MENU25 = 9025
CONST  ID_MENU26 = 9026
CONST  ID_MENU27 = 9027
CONST  ID_MENU28 = 9028
CONST  ID_MENU29 = 9029
CONST  ID_MENU30 = 9030
CONST  ID_MENU31 = 9031
CONST  ID_MENU32 = 9032
CONST  ID_MENU33 = 9033
CONST  ID_MENU34 = 9034
CONST  ID_MENU35 = 9035
'--------------------------------------------------------------------------
GLOBAL MainMenu AS HMENU
  MainMenu = CreateMenu()
'--------------------------------------------------------------------------
GLOBAL FileMenu AS HMENU
  FileMenu = CreateMenu()
  InsertMenu(MainMenu,  0, MF_BYPOSITION OR MF_POPUP, FileMenu, "&File")
  AppendMenu(FileMenu, MF_STRING, ID_MENU1, "&LoadMnd")
  AppendMenu(FileMenu, MF_STRING, ID_MENU2, "&SaveMndAs")
  AppendMenu(FileMenu, MF_STRING, ID_MENU3, "L&oadBmp")
  AppendMenu(FileMenu, MF_STRING, ID_MENU4, "S&aveBmpAs")
  AppendMenu(FileMenu, MF_STRING, ID_MENU5, "&Print")
  AppendMenu(FileMenu, MF_STRING, ID_MENU6, "&Quit")
'--------------------------------------------------------------------------
GLOBAL IterazioniMenu AS HMENU
  IterazioniMenu = CreateMenu()
  InsertMenu(MainMenu,  1, MF_BYPOSITION OR MF_POPUP, IterazioniMenu, "&Iterazioni")
  AppendMenu(IterazioniMenu, MF_CHECKED OR MF_STRING, ID_MENU8, "125")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU9, "177")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU10, "250")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU11, "353")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU12, "500")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU13, "707")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU14, "1000")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU15, "1414")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU16, "2000")
  AppendMenu(IterazioniMenu, MF_STRING, ID_MENU17, "&Custom")
  CheckMenuRadioItem (IterazioniMenu, 0, 9, 0, MF_BYPOSITION)
'--------------------------------------------------------------------------
GLOBAL RisoluzioneMenu AS HMENU
  RisoluzioneMenu = CreateMenu()
  InsertMenu(MainMenu,  2, MF_BYPOSITION OR MF_POPUP, RisoluzioneMenu, "&Risoluzione")
  AppendMenu(RisoluzioneMenu, MF_CHECKED OR MF_STRING, ID_MENU19, "320x240")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU20, "640x480")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU21, "800x600")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU22, "1024x768")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU23, "1280x1024")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU24, "1600x1200")
  AppendMenu(RisoluzioneMenu, MF_STRING, ID_MENU25, "&Custom")
  CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 0, MF_BYPOSITION)
'--------------------------------------------------------------------------
GLOBAL StopGoMenu AS HMENU
  StopGoMenu = CreateMenu()
  InsertMenu(MainMenu,  3, MF_BYPOSITION OR MF_POPUP, StopGoMenu, "&GoReInit")
  AppendMenu(StopGoMenu, MF_STRING, ID_MENU28, "G&o")
  AppendMenu(StopGoMenu, MF_STRING, ID_MENU32, "&Clear")
  AppendMenu(StopGoMenu, MF_STRING, ID_MENU29, "&ReInit")
'--------------------------------------------------------------------------
GLOBAL ToolsMenu AS HMENU
  ToolsMenu = CreateMenu()
  InsertMenu(MainMenu,  4, MF_BYPOSITION OR MF_POPUP, ToolsMenu, "&Tools")
  AppendMenu(ToolsMenu, MF_STRING, ID_MENU31, "&Resize")
  AppendMenu(ToolsMenu, MF_STRING, ID_MENU35, "&SetCoords")
'--------------------------------------------------------------------------
GLOBAL HelpMenu AS HMENU
  HelpMenu = CreateMenu()
  InsertMenu(MainMenu,  5, MF_BYPOSITION OR MF_POPUP, HelpMenu, "&Help")
  AppendMenu(HelpMenu, MF_STRING, ID_MENU34, "&About")
  AppendMenu(HelpMenu, MF_STRING, ID_MENU27, "&Info")
'--------------------------------------------------------------------------
  ' activate menu
  IF NOT SetMenu(hwndOwner, MainMenu) THEN
    FUNCTION = FALSE
  END IF
  FUNCTION = TRUE
END FUNCTION

BEGIN EVENTS
SELECT CASE CBMSG
local rc as RECT
local coord_man$ as string
local tmp$ as string

  CASE WM_LBUTTONDOWN
  xa=LOWORD(lParam)
  ya=HIWORD(lParam)
  x1a=(cx1+xa*dx)
  y1a=(cy1+ya*dy)

  CASE WM_LBUTTONUP
  xb=LOWORD(lParam)
  yb=HIWORD(lParam)
  x2a=(cx1+xb*dx)
  y2a=(cy1+yb*dy)
  'riordino coordinate rettangolo di selezione per evitare rovesciamenti e specchiature
  if x1a > x2a then swap x1a, x2a
  if y1a > y2a then swap y1a, y2a
  BCX_RECTANGLE(Form1,xa,ya,xb,yb,RGB(RND*255,RND*255,RND*255),FALSE)
  'reale minimo
  cx1 = x1a
  'reale massimo
  cx2 = x2a
  'immaginario minimo
  cy1 = y1a
  'determino immaginario massimo in cosiderazione degli altri dati immessi
  'salvaguardo fattore di scalatura su ambo gli assi cartesiani
  'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
  CALL DrawMandelbrot
    
    CASE WM_PAINT
      dim RAW ps  AS PAINTSTRUCT
      dim RAW hdc AS HDC
      hdc = BeginPaint (hWnd, &ps)
      CALL Load_Bmp_tmp
      'CALL DrawMandelbrot
      DeleteDC (hdc)
      EndPaint (hWnd, &ps)

'fine programma
    CASE WM_CLOSE
      DestroyWindow (Form1)

'fine programma
    CASE WM_DESTROY
    PostQuitMessage(0)
    EXIT FUNCTION

    CASE WM_COMMAND
'Info
      IF CBCTL = ID_MENU27 THEN
      	CALL Info
      end if

'go
      IF CBCTL = ID_MENU28 THEN
      	CALL DrawMandelbrot
      end if

'reinit
      IF CBCTL = ID_MENU29 THEN
      'inserimento dei dati di partenza escluso risoluzione ed iterazioni
	  'reale minimo
		cx1 = atoq("-2.0")
	  'reale massimo
		cx2 = atoq("2.0")
	  'immaginario minimo
		cy1 = atoq("-1.5")
      'determino immaginario massimo in cosiderazione degli altri dati immessi
      'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
        'cy2 = 1.5L
      	CALL DrawMandelbrot
      end if

'menu iterazioni
      IF CBCTL = ID_MENU8 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 0, MF_BYPOSITION) '125
        t=125
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU9 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 1, MF_BYPOSITION) '177
        t=177
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU10 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 2, MF_BYPOSITION) '250
        t=250
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU11 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 3, MF_BYPOSITION) '353
        t=353
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU12 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 4, MF_BYPOSITION) '500
        t=500
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU13 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 5, MF_BYPOSITION) '707
        t=707
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU14 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 6, MF_BYPOSITION) '1000
        t=1000
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU15 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 7, MF_BYPOSITION) '1414
        t=1414
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU16 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 8, MF_BYPOSITION) '2000
        t=2000
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU17 THEN
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 9, MF_BYPOSITION) 'Custom
        'Inserisce valori Custom
		t = val(INPUTBOX$("Iterazioni", "Inserire Numero di Iterazioni (valore >= 1)",trim$(str$(t))))
        CALL FlagIterazioni
      	CALL DrawMandelbrot
      end if

'menu risoluzione  
      IF CBCTL = ID_MENU19 THEN
        CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 0, MF_BYPOSITION) '320x240
	  'xmassimo window
		o = 319
	  'ymassimo window
		v = 239
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU20 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 1, MF_BYPOSITION) '640x480
	  'xmassimo window
		o = 639
	  'ymassimo window
		v = 479
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU21 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 2, MF_BYPOSITION) '800x600
	  'xmassimo window
		o = 799
	  'ymassimo window
		v = 599
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU22 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 3, MF_BYPOSITION) '1024x768
	  'xmassimo window
		o = 1023
	  'ymassimo window
		v = 767
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU23 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 4, MF_BYPOSITION) '1280x1024
	  'xmassimo window
		o = 1279
	  'ymassimo window
		v = 1023
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU24 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 5, MF_BYPOSITION) '1600x1280
	  'xmassimo window
		o = 1599
	  'ymassimo window
		v = 1279
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

      IF CBCTL = ID_MENU25 THEN
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 6, MF_BYPOSITION) 'Custom
        'Inserisce valori Custom
		ris_vid$ = INPUTBOX$("Dimensioni video", "Inserire Dimensioni in pixel (OxV)", _
        remove$(str$(o+1)+"x"+str$(v+1),SPC$))
		CALL FlagRisoluzione (ris_vid$)
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
      	CALL DrawMandelbrot
      end if

'About  
      IF CBCTL = ID_MENU34 THEN
        MSGBOX "Mandelbrot v1.01"+CRLF$+ _
        "(c)2006 by Stefano Trevisani"+CRLF$+ _
        "All rights reserved","About",MB_ICONINFORMATION
      END IF

'SetCoords
      IF CBCTL = ID_MENU35 THEN
        'Inserisce valori coordinate manualmente
        qtoasc(&cx1,qcx1$,100)
        qtoasc(&cy1,qcy1$,100)
        qtoasc(&cx2,qcx2$,100)
        qtoasc(&cy2,qcy2$,100)
        
		coord_man$ = INPUTBOX$("Coordinate", "Inserire Coordinate (Xmin,Ymin,Xmax,Ymax)", _
        remove$(qcx1$+","+qcy1$+","+qcx2$+","+qcy2$,SPC$))
        if tally(coord_man$,",") = 3 then
            REMOVE SPC$ FROM UCASE$(coord_man$)
            REMOVE TAB$ FROM UCASE$(coord_man$)
        	cx1 = atoq(extract$(coord_man$,","))
            tmp$ = remain$(coord_man$,",")
        	cy1 = atoq(extract$(tmp$,","))
            tmp$ = remain$(tmp$,",")
        	cx2 = atoq(extract$(tmp$,","))
            tmp$ = remain$(tmp$,",")
        	cy2 = atoq(tmp$)
            'riordino coordinate nel caso di input non corretto
            if cx1 > cx2 then swap cx1, cx2
            if cy1 > cy2 then swap cy1, cy2
            'determino immaginario massimo in cosiderazione degli altri dati immessi
            'salvaguardo fattore di scalatura su ambo gli assi cartesiani
            'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
            CALL DrawMandelbrot
        end if
      END IF

'Resize ridimensiona disegno in funzione della dimensione della finestra
      IF CBCTL = ID_MENU31 THEN
	    GetClientRect(Form1,&rc)
    	'GetWindowRect (Form1, &rc)
		o = rc.right - rc.left
		v = rc.bottom - rc.top
		ris_vid$ = remove$(str$(o)+"x"+str$(v),SPC$)
		CALL FlagRisoluzione (ris_vid$)
        'determino immaginario massimo in cosiderazione degli altri dati immessi
        'salvaguardo fattore di scalatura su ambo gli assi cartesiani
        'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
        CALL DrawMandelbrot
      END IF

'Clear pulisce window
      IF CBCTL = ID_MENU32 THEN
	    GetClientRect(Form1,&rc)
        'GetWindowRect (Form1, &rc)
        'BCX_RECTANGLE(Form1,rc.left,rc.top,rc.right,rc.bottom,nero,TRUE)
        BCX_RECTANGLE(Form1,0,0,rc.right,rc.bottom,nero,TRUE)
      END IF
 
'LoadMnd
      IF CBCTL = ID_MENU1 THEN
      	CALL LoadMnd
      	CALL DrawMandelbrot
      END IF

'SaveMndAs
      IF CBCTL = ID_MENU2 THEN
      	CALL SaveMnd
      END IF

'LoadBmp
      IF CBCTL = ID_MENU3 THEN
      	CALL Load_Bmp
      END IF

'SaveBmpAs
      IF CBCTL = ID_MENU4 THEN
      	CALL Save_Bmp
      END IF

'Print
      IF CBCTL = ID_MENU5 THEN
        CALL Print_Bmp
      END IF

'Quit
      IF CBCTL = ID_MENU6 THEN
        SendMessage(Form1, WM_CLOSE, 0, 0)
        EXIT FUNCTION
      END IF
    
END SELECT
END EVENTS

'algoritmo di calcolo e tinteggiatura finestra
SUB DrawMandelbrot
local c as integer
local r as integer
local i as integer
local x as qfloat
local y as qfloat
local a as qfloat
local b as qfloat
local d as qfloat
local e as qfloat
local quattro as qfloat
local coeff as int

quattro = atoq("4.0")
coeff = int(nc/t)

'determino immaginario massimo in cosiderazione degli altri dati immessi
'salvaguardo fattore di scalatura su ambo gli assi cartesiani
cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)

c=0
r=0
i=0
dx=(cx2-cx1)/(o+1)
dy=(cy2-cy1)/(v+1)

aggiorna:
x=cx1+dx*c
a=x
y=cy1+dy*r
b=y

if (a*a+b*b) >= quattro then
BCX_PSET (Form1,c,r,bianco)
goto esterno
end if

cicla:
'passo il controllo al windows per gestione eventi
'DOEVENTS
e=a*a-b*b+x

'piccolo hack che dovrebbe migliorare la precisione
'b=2.0L*a*b+y
b=a*b
b=b+b+y

a=e
d=a*a+b*b
if d >= quattro then goto colora

if i>t then
BCX_PSET (Form1,c,r,nero)
goto interno
end if

i++
goto cicla

colora:
BCX_PSET (Form1,c,r,coeff*i)

esterno:
interno:

if r=v then goto wexit
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
'salvo la bitmap
CALL Save_Bmp_tmp
UpdateWindow(Form1)
END SUB

'Carica file con i dati di una determinata zona (coordinate ed iterazioni)
SUB LoadMnd
Dim FileName$
Dim Mask$
DIM RAW fp1 AS FILE
DIM linea$ as string

Mask$ = "Mandelbrot Files(*.man)|*.MAN"
FileName$ = GETFILENAME$("OPEN", Mask$, 0) ' set flag to use OPEN Dialog
IF LEN(FileName$) THEN
'  MSGBOX FileName$
'carico i dati del file
   open (FileName$) for input as fp1
   while not eof ( fp1)
      line input fp1,linea$
        REMOVE SPC$ FROM UCASE$(linea$)
        REMOVE TAB$ FROM UCASE$(linea$)
      'salto le linee di commento
      IF LEFT$(linea$,1) <> ";" THEN

      	IF INSTR(linea$,"NUMERO_ITERAZIONI=") > 0 THEN
      	t = val(REMAIN$(linea$,"="))
			CALL FlagIterazioni
        END IF

      	IF INSTR(linea$,"X_MINIMO=") > 0 THEN
      	cx1 = atoq(REMAIN$(linea$,"="))
        END IF

      	IF INSTR(linea$,"Y_MINIMO=") > 0 THEN
      	cy1 = atoq(REMAIN$(linea$,"="))
        END IF

      	IF INSTR(linea$,"X_MASSIMO=") > 0 THEN
      	cx2 = atoq(REMAIN$(linea$,"="))
        END IF

      	IF INSTR(linea$,"Y_MASSIMO=") > 0 THEN
      	cy2 = atoq(REMAIN$(linea$,"="))
        END IF

      	IF INSTR(linea$,"LARGHEZZA_PER_ALTEZZA_VIDEO=") > 0 THEN
        ris_vid$=remain$(linea$,"=")
			CALL FlagRisoluzione (ris_vid$)
        END IF
         
      END IF
   wend
   'riordino coordinate nel caso di input non corretto
   if cx1 > cx2 then swap cx1, cx2
   if cy1 > cy2 then swap cy1, cy2
  'determino immaginario massimo in cosiderazione degli altri dati immessi
  'salvaguardo fattore di scalatura su ambo gli assi cartesiani
  'cy2 = cy1+(cx2-cx1)*(v+1)/(o+1)
   close fp1

ELSE
  MSGBOX "Operazione Annullata","Abort!", MB_OK + MB_ICONWARNING
END IF

END SUB

'Salva file con i dati di una determinata zona (coordinate ed iterazioni)
SUB SaveMnd
Dim FileName$
Dim Mask$
DIM RAW fp1 AS FILE
DIM linea$ as string

Mask$ = "Mandelbrot Files(*.man)|*.MAN"
FileName$ = GETFILENAME$("SAVE", Mask$, 1) ' set flag to use SAVE Dialog
IF LEN(FileName$) THEN
'  MSGBOX FileName$
'salvo i dati del file
   
   'se non presente aggiungo la estensione ".man"
   if instr(UCASE$(FileName$),".MAN") = 0 then FileName$ = FileName$ + ".man"
   
   open (FileName$) for output as fp1

   linea$ = ";File dati del programma Mandelbrot v1.01 (c)2006 by Stefano Trevisani"
   fprint fp1,linea$

   linea$ = ";Numero di iterazioni es: 1200"
   fprint fp1,linea$
   linea$ = "NUMERO_ITERAZIONI=" + str$(t)
   fprint fp1,linea$

   linea$ = ";X minimo es: -2.0"
   fprint fp1,linea$
   qtoasc(&cx1,qcx1$,100)
   linea$ = "X_MINIMO=" + qcx1$
   fprint fp1,linea$

   linea$ = ";Y minimo es: -1.5"
   fprint fp1,linea$
   qtoasc(&cy1,qcy1$,100)
   linea$ = "Y_MINIMO=" + qcy1$
   fprint fp1,linea$

   linea$ = ";X massimo es: 2.0"
   fprint fp1,linea$
   qtoasc(&cx2,qcx2$,100)
   linea$ = "X_MASSIMO=" + qcx2$
   fprint fp1,linea$

   linea$ = ";Y massimo es: 2.0"
   fprint fp1,linea$
   qtoasc(&cy2,qcy2$,100)
   linea$ = "Y_MASSIMO=" + qcy2$
   fprint fp1,linea$

   linea$ = ";Larghezza x altezza video in pixel es: 800x800"
   fprint fp1,linea$
   linea$ = "LARGHEZZA_PER_ALTEZZA_VIDEO=" + str$(o+1) + "x" + str$(v+1)
   fprint fp1,linea$

   close fp1   
ELSE
  MSGBOX "Operazione Annullata","Abort!", MB_OK + MB_ICONWARNING
END IF

END SUB


'Carica il disegno bitmap del frattale nella window
SUB Load_Bmp
Dim FileName$
Dim Mask$

Mask$ = "Mandelbrot Bmp(*.bmp)|*.BMP"
FileName$ = GETFILENAME$("OPEN", Mask$, 0) ' set flag to use OPEN Dialog
IF LEN(FileName$) THEN
'  MSGBOX FileName$
'carico immagine bitmap
'   SET_BCX_BITMAP(Bmp1,FileName$)   ' Load A File Based Bitmap
'copio il file selezionato nel file temporaneo "temp.bmp"
   COPYFILE FileName$, Temp$, FALSE  
   SET_BCX_BITMAP(Bmp1,Temp$)   ' Load A File Based Bitmap
ELSE
  MSGBOX "Operazione Annullata","Abort!", MB_OK + MB_ICONWARNING
END IF

END SUB

'Salva il disegno bitmap del frattale
SUB Save_Bmp
Dim FileName$
Dim Mask$
DIM  Rect     AS  RECT
DIM  HDCBmp   AS  HDC
DIM  retval

Mask$ = "Mandelbrot Bmp(*.bmp)|*.BMP"
FileName$ = GETFILENAME$("SAVE", Mask$, 1) ' set flag to use SAVE Dialog
IF LEN(FileName$) THEN
'  MSGBOX FileName$
'salvo la bitmap del client
   'se non presente aggiungo la estensione ".bmp"
   if instr(UCASE$(FileName$),".BMP") = 0 then FileName$ = FileName$ + ".bmp"
   
' Capture Client area of a window
' evito di copiare anche bordi non renderizzati n.b. salva solo cio che si vede
      GetClientRect(Form1,&Rect)
      if o > (Rect.right - Rect.left) OR v > (Rect.bottom - Rect.top) then
      	'HDCBmp = GetBmp(Rect.left, Rect.top, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      	HDCBmp = GetBmp(0, 0, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      else
      	HDCBmp = GetBmp(0, 0, o+1, v+1, Form1)
      end if
      SaveBmp(HDCBmp,FileName$)
' IMPORTANT ! Release device conStefano Trevisanit resources back to system.
      retval = DeleteDC(HDCBmp)
ELSE
  MSGBOX "Operazione Annullata","Abort!", MB_OK + MB_ICONWARNING
END IF

END SUB

'Salva il disegno bitmap temporaneo
SUB Save_Bmp_tmp
DIM  Rect     AS  RECT
DIM  HDCBmp   AS  HDC
DIM  retval

'salvo la bitmap del client
' Capture Client area of a window
' evito di copiare anche bordi non renderizzati n.b. salva solo cio che si vede
      GetClientRect(Form1,&Rect)
      if o > (Rect.right - Rect.left) OR v > (Rect.bottom - Rect.top) then
      	'HDCBmp = GetBmp(Rect.left, Rect.top, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      	HDCBmp = GetBmp(0, 0, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      else
      	HDCBmp = GetBmp(0, 0, o+1, v+1, Form1)
      end if
      SaveBmp(HDCBmp,Temp$)
' IMPORTANT ! Release device conStefano Trevisanit resources back to system.
      retval = DeleteDC(HDCBmp)
END SUB

'Stampa il disegno tramite il software di fotoritocco predefinito
SUB Print_Bmp
DIM  Rect     AS  RECT
DIM  HDCBmp   AS  HDC
DIM  retval
CONST RunEx(lpFile, lpParameters, nShowCmd) = ShellExecute(0, "open", lpFile, lpParameters, 0, nShowCmd)
' Capture Client area of a window
' evito di copiare anche bordi non renderizzati n.b. salva solo cio che si vede
      GetClientRect(Form1,&Rect)
      if o > (Rect.right - Rect.left) OR v > (Rect.bottom - Rect.top) then
      	'HDCBmp = GetBmp(Rect.left, Rect.top, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      	HDCBmp = GetBmp(0, 0, Rect.right - Rect.left, Rect.bottom - Rect.top, Form1)
      else
      	HDCBmp = GetBmp(0, 0, o+1, v+1, Form1)
      end if
      SaveBmp(HDCBmp,TempStamp$)
' IMPORTANT ! Release device conStefano Trevisanit resources back to system.
      retval = DeleteDC(HDCBmp)
      RunEx(TempStamp$,"" , 1) ' Start default paint app
END SUB

'Carica il disegno bitmap del frattale nella window
SUB Load_Bmp_tmp
'carico immagine bitmap
   SET_BCX_BITMAP(Bmp1,Temp$)   ' Load A File Based Bitmap
END SUB

'imposto i flag del menu in base ai valori immessi
SUB FlagIterazioni
        if t < 1 then t = 1200      
      	CheckMenuRadioItem (IterazioniMenu, 0, 9, 9, MF_BYPOSITION) 'Custom
		'flag del menu con ceckbox
			if t=125  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 0, MF_BYPOSITION) '125
			if t=177  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 1, MF_BYPOSITION) '177
			if t=250  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 2, MF_BYPOSITION) '250
			if t=353  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 3, MF_BYPOSITION) '353
			if t=500  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 4, MF_BYPOSITION) '500
			if t=707  then CheckMenuRadioItem (IterazioniMenu, 0, 9, 5, MF_BYPOSITION) '707
			if t=1000 then CheckMenuRadioItem (IterazioniMenu, 0, 9, 6, MF_BYPOSITION) '1000
			if t=1414 then CheckMenuRadioItem (IterazioniMenu, 0, 9, 7, MF_BYPOSITION) '1414
			if t=2000 then CheckMenuRadioItem (IterazioniMenu, 0, 9, 8, MF_BYPOSITION) '2000
END SUB

'imposto i flag del menu in base ai valori immessi
SUB FlagRisoluzione (ris_vid$)
        REMOVE SPC$ FROM UCASE$(ris_vid$)
        REMOVE TAB$ FROM UCASE$(ris_vid$)
          IF INSTR (ris_vid$,"X") = 0 THEN
        	o = 800
            v = 800
          ELSE
            o = val(extract$(ris_vid$,"X"))
        	if o < 1 then o = 800
            v = val(remain$(ris_vid$,"X"))
        	if v < 1 then v = 800      
          END IF
		'flag del menu con ceckbox
      	CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 6, MF_BYPOSITION) 'Custom
        if (o=320 AND v=240) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 0, MF_BYPOSITION) '320x240
        if (o=640 AND v=480) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 1, MF_BYPOSITION) '640x480
        if (o=800 AND v=600) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 2, MF_BYPOSITION) '800x600
        if (o=1024 AND v=768) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 3, MF_BYPOSITION) '1024x768
        if (o=1280 AND v=1024) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 4, MF_BYPOSITION) '1280x1024
        if (o=1600 AND v=1200) then CheckMenuRadioItem (RisoluzioneMenu, 0, 6, 5, MF_BYPOSITION) '1600x1200
        o = o - 1
        v = v - 1
END SUB

SUB Info

    qtoasc(&cx1,qcx1$,100)
    qtoasc(&cy1,qcy1$,100)
    qtoasc(&cx2,qcx2$,100)
    qtoasc(&cy2,qcy2$,100)

    ris_vid$ = str$(o+1) & "x" & str$(v+1)
    REMOVE SPC$ FROM UCASE$(ris_vid$)
    REMOVE TAB$ FROM UCASE$(ris_vid$)
    MsgBox  "Numero Iterazioni -> " + str$(t) + CRLF$ + _
             "X minimo -> " + qcx1$ + CRLF$ + _
             "Y minimo -> " + qcy1$ + CRLF$ + _
             "X massimo -> " + qcx2$ + CRLF$ + _
             "Y massimo -> " + qcy2$ + CRLF$ + _
             "Risoluzione (OxV) -> " + ris_vid$, "Parametri Inseriti", MB_OK + MB_ICONINFORMATION
END SUB
