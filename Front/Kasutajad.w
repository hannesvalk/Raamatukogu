&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          raamat           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER admin AS LOGICAL.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tkasutajad LIKE kasutajad.

DEFINE VARIABLE clKasutaja AS back.kasutaja.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrmKasutajad
&Scoped-define BROWSE-NAME bKasutajad

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tkasutajad kasutajad

/* Definitions for BROWSE bKasutajad                                    */
&Scoped-define FIELDS-IN-QUERY-bKasutajad //tkasutajad.id tkasutajad.Nimi //tkasutajad.password tkasutajad.staatus tkasutajad.username   
&Scoped-define ENABLED-FIELDS-IN-QUERY-bKasutajad   
&Scoped-define SELF-NAME bKasutajad
&Scoped-define OPEN-QUERY-bKasutajad IF admin THEN     (NEW back.kasutaja()):GetKasutajad(fnimi:INPUT-VALUE, ~
      fkasutajatunnus:INPUT-VALUE, ~
      fStaatus:INPUT-VALUE, ~
      INPUT-OUTPUT TABLE tkasutajad). ELSE     (NEW back.kasutaja()):GetKasutajad(fnimi:INPUT-VALUE, ~
      fkasutajatunnus:INPUT-VALUE, ~
      'K', ~
      INPUT-OUTPUT TABLE tkasutajad). OPEN QUERY {&SELF-NAME} FOR EACH tkasutajad NO-LOCK BY tkasutajad.nimi INDEXED-REPOSITION .
&Scoped-define TABLES-IN-QUERY-bKasutajad tkasutajad
&Scoped-define FIRST-TABLE-IN-QUERY-bKasutajad tkasutajad


/* Definitions for FRAME FrmKasutajad                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FrmKasutajad ~
    ~{&OPEN-QUERY-bKasutajad}
&Scoped-define QUERY-STRING-FrmKasutajad FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-FrmKasutajad OPEN QUERY FrmKasutajad FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-FrmKasutajad kasutajad
&Scoped-define FIRST-TABLE-IN-QUERY-FrmKasutajad kasutajad


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS bKasutajad BtnExit BtnOtsi 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Vali ja Välju" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnExit DEFAULT 
     LABEL "&Välju" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnKustuta AUTO-GO DEFAULT 
     LABEL "Kustuta" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnLisa AUTO-GO DEFAULT 
     LABEL "Lisa" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOtsi AUTO-GO DEFAULT 
     LABEL "Otsi" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fkasutajatunnus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasutajatunnus" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fnimi AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ees- ja perekonnanimi" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fStaatus AS CHARACTER INITIAL "K" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Administraator", "A",
"Raamatukogu teenindaja", "T",
"Anonüümne kasutaja", "K"
     SIZE 27 BY 3 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY bKasutajad FOR 
      tkasutajad SCROLLING.

DEFINE QUERY FrmKasutajad FOR 
      kasutajad SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE bKasutajad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS bKasutajad C-Win _FREEFORM
  QUERY bKasutajad DISPLAY
      //tkasutajad.id FORMAT "->,>>>,>>9":U
tkasutajad.Nimi FORMAT "x(40)":U
//tkasutajad.password FORMAT "x(8)":U
tkasutajad.staatus FORMAT "x(1)":U
tkasutajad.username FORMAT "x(8)":U WIDTH 48.83
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82 BY 24.05 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmKasutajad
     bKasutajad AT ROW 1.71 COL 3.5 WIDGET-ID 200
     fStaatus AT ROW 26.24 COL 42.33 NO-LABEL WIDGET-ID 34
     fnimi AT ROW 26.48 COL 22 COLON-ALIGNED WIDGET-ID 4 AUTO-RETURN 
     fkasutajatunnus AT ROW 28.14 COL 22 COLON-ALIGNED WIDGET-ID 32
     BtnLisa AT ROW 30.05 COL 4.67 WIDGET-ID 30
     BtnKustuta AT ROW 30.05 COL 21.33 WIDGET-ID 18
     BtnDone AT ROW 30.05 COL 37.83 WIDGET-ID 2
     BtnExit AT ROW 30.05 COL 54 WIDGET-ID 38
     BtnOtsi AT ROW 30.05 COL 70.5 WIDGET-ID 40
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.83 BY 30.86
         DEFAULT-BUTTON BtnExit WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kasutajate nimestik"
         HEIGHT             = 30.86
         WIDTH              = 87.83
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 48.43
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FrmKasutajad
   FRAME-NAME                                                           */
/* BROWSE-TAB bKasutajad 1 FrmKasutajad */
/* SETTINGS FOR BUTTON BtnDone IN FRAME FrmKasutajad
   NO-ENABLE                                                            */
ASSIGN 
       BtnDone:HIDDEN IN FRAME FrmKasutajad           = TRUE.

/* SETTINGS FOR BUTTON BtnKustuta IN FRAME FrmKasutajad
   NO-ENABLE                                                            */
ASSIGN 
       BtnKustuta:HIDDEN IN FRAME FrmKasutajad           = TRUE.

/* SETTINGS FOR BUTTON BtnLisa IN FRAME FrmKasutajad
   NO-ENABLE                                                            */
ASSIGN 
       BtnLisa:HIDDEN IN FRAME FrmKasutajad           = TRUE.

/* SETTINGS FOR FILL-IN fkasutajatunnus IN FRAME FrmKasutajad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fkasutajatunnus:HIDDEN IN FRAME FrmKasutajad           = TRUE.

/* SETTINGS FOR FILL-IN fnimi IN FRAME FrmKasutajad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fnimi:HIDDEN IN FRAME FrmKasutajad           = TRUE.

/* SETTINGS FOR RADIO-SET fStaatus IN FRAME FrmKasutajad
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fStaatus:HIDDEN IN FRAME FrmKasutajad           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE bKasutajad
/* Query rebuild information for BROWSE bKasutajad
     _START_FREEFORM
IF admin THEN
    (NEW back.kasutaja()):GetKasutajad(fnimi:INPUT-VALUE,fkasutajatunnus:INPUT-VALUE,fStaatus:INPUT-VALUE,INPUT-OUTPUT TABLE tkasutajad).
ELSE
    (NEW back.kasutaja()):GetKasutajad(fnimi:INPUT-VALUE,fkasutajatunnus:INPUT-VALUE,'K',INPUT-OUTPUT TABLE tkasutajad).
OPEN QUERY {&SELF-NAME} FOR EACH tkasutajad NO-LOCK BY tkasutajad.nimi INDEXED-REPOSITION .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE bKasutajad */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FrmKasutajad
/* Query rebuild information for FRAME FrmKasutajad
     _TblList          = "raamat.kasutajad"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* FRAME FrmKasutajad */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kasutajate nimestik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kasutajate nimestik */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME FrmKasutajad /* Vali ja Välju */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      DEFINE VARIABLE vastus AS CHARACTER.
      IF bKasutajad:NUM-ITERATIONS>0 THEN
        vastus=STRING(tkasutajad.id).
      ELSE
        vastus="-1".
      
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN vastus.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit C-Win
ON CHOOSE OF BtnExit IN FRAME FrmKasutajad /* Välju */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN '-1'.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnKustuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnKustuta C-Win
ON CHOOSE OF BtnKustuta IN FRAME FrmKasutajad /* Kustuta */
DO:
  DEFINE VARIABLE FocusedRow AS INTEGER.
  FocusedRow =  bkasutajad:FOCUSED-ROW .
  IF bkasutajad:NUM-ITERATIONS>0 THEN
  DO:
      IF (NEW back.kasutaja()):DelKasutaja(tkasutajad.id) THEN DO:
        {&OPEN-QUERY-Bkasutajad}
        IF MIN(bkasutajad:NUM-ITERATIONS,FocusedRow)>0 THEN
           bkasutajad:SELECT-ROW (MIN(bkasutajad:NUM-ITERATIONS,FocusedRow)).  
      END.
      ELSE
         MESSAGE "Kasutajar '" + tkasutajad.nimi + "' ei saa kustutada." + CHR(10) + "Tal on väljalaenutatud eksemplare." VIEW-AS ALERT-BOX.    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLisa C-Win
ON CHOOSE OF BtnLisa IN FRAME FrmKasutajad /* Lisa */
DO:
  (NEW back.kasutaja()):AddKAsutaja(fnimi:INPUT-VALUE, fkasutajatunnus:INPUT-VALUE, '', fstaatus:INPUT-VALUE).
  {&OPEN-QUERY-Bkasutajad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOtsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOtsi C-Win
ON CHOOSE OF BtnOtsi IN FRAME FrmKasutajad /* Otsi */
DO:
   {&OPEN-QUERY-bKasutajad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fkasutajatunnus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fkasutajatunnus C-Win
ON RETURN OF fkasutajatunnus IN FRAME FrmKasutajad /* Kasutajatunnus */
DO:
  {&OPEN-QUERY-bKasutajad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fnimi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fnimi C-Win
ON RETURN OF fnimi IN FRAME FrmKasutajad /* Ees- ja perekonnanimi */
DO:
  {&OPEN-QUERY-bKasutajad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fStaatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fStaatus C-Win
ON RETURN OF fStaatus IN FRAME FrmKasutajad
DO:
  {&OPEN-QUERY-bKasutajad}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME bKasutajad
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  fStaatus='K'. 
  IF admin THEN DO:
     ENABLE BtnLisa         WITH FRAME FrmKasutajad.
     ENABLE BtnKustuta      WITH FRAME FrmKasutajad.
     ENABLE fnimi           WITH FRAME FrmKasutajad.
     ENABLE fkasutajatunnus WITH FRAME FrmKasutajad.
     ENABLE fStaatus        WITH FRAME FrmKasutajad.
     DISPLAY fStaatus       WITH FRAME FrmKasutajad.
  END.
  ELSE
     ENABLE BtnDone        WITH FRAME FrmKasutajad.
   
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/

  {&OPEN-QUERY-FrmKasutajad}
  GET FIRST FrmKasutajad.
  ENABLE bKasutajad BtnExit BtnOtsi 
      WITH FRAME FrmKasutajad IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrmKasutajad}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

