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

/* Local Variable Definitions ---                                       */

{back/laenutusedStru.i}

DEFINE VARIABLE clLaenutus AS back.laenutus.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrmLaenutused
&Scoped-define BROWSE-NAME Blaenutused

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tlaenutused kasutajad

/* Definitions for BROWSE Blaenutused                                   */
&Scoped-define FIELDS-IN-QUERY-Blaenutused tlaenutused.laenutatud tlaenutused.tahtaeg tlaenutused.kasutaja tlaenutused.pealkiri tlaenutused.autor tlaenutused.ISBN   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Blaenutused   
&Scoped-define SELF-NAME Blaenutused
&Scoped-define OPEN-QUERY-Blaenutused (NEW back.laenutus()):GetLaenutused(fpealkiri:INPUT-VALUE, ~
       fautor:INPUT-VALUE, ~
       fISBN:INPUT-VALUE, ~
       INPUT-OUTPUT TABLE tlaenutused). OPEN QUERY {&SELF-NAME} FOR EACH tlaenutused NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Blaenutused tlaenutused
&Scoped-define FIRST-TABLE-IN-QUERY-Blaenutused tlaenutused


/* Definitions for FRAME FrmLaenutused                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FrmLaenutused ~
    ~{&OPEN-QUERY-Blaenutused}
&Scoped-define QUERY-STRING-FrmLaenutused FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-FrmLaenutused OPEN QUERY FrmLaenutused FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-FrmLaenutused kasutajad
&Scoped-define FIRST-TABLE-IN-QUERY-FrmLaenutused kasutajad


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Blaenutused fpealkiri fautor fISBN BtnOtsi ~
BtnLisa BtnKustuta BtnDone 
&Scoped-Define DISPLAYED-OBJECTS fpealkiri fautor fISBN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Välju" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnKustuta AUTO-GO DEFAULT 
     LABEL "Tagastus" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnLisa AUTO-GO DEFAULT 
     LABEL "Laenuta" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOtsi AUTO-GO DEFAULT 
     LABEL "Otsi" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fautor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Autor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fISBN AS CHARACTER FORMAT "X(256)":U 
     LABEL "ISBN" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fpealkiri AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pealkiri" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Blaenutused FOR 
      tlaenutused SCROLLING.

DEFINE QUERY FrmLaenutused FOR 
      kasutajad SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Blaenutused
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Blaenutused C-Win _FREEFORM
  QUERY Blaenutused DISPLAY
      tlaenutused.laenutatud   FORMAT "99/99/9999":U 
tlaenutused.tahtaeg      FORMAT "99/99/9999":U 
tlaenutused.kasutaja     FORMAT "x(20)":U
tlaenutused.pealkiri     FORMAT "x(20)":U
tlaenutused.autor        FORMAT "x(20)":U
tlaenutused.ISBN         FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112.5 BY 30.48 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmLaenutused
     Blaenutused AT ROW 1.71 COL 3.5 WIDGET-ID 200
     fpealkiri AT ROW 33.38 COL 20.5 COLON-ALIGNED WIDGET-ID 28
     fautor AT ROW 33.38 COL 41.17 COLON-ALIGNED WIDGET-ID 24
     fISBN AT ROW 33.38 COL 61.83 COLON-ALIGNED WIDGET-ID 26
     BtnOtsi AT ROW 33.38 COL 81.33 WIDGET-ID 30
     BtnLisa AT ROW 35.52 COL 29 WIDGET-ID 4
     BtnKustuta AT ROW 35.52 COL 46.83 WIDGET-ID 18
     BtnDone AT ROW 35.52 COL 74.5 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.83 BY 36.71 WIDGET-ID 100.


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
         TITLE              = "Raamatueksemplaride ja laenutuste loend"
         HEIGHT             = 36.71
         WIDTH              = 118.83
         MAX-HEIGHT         = 36.71
         MAX-WIDTH          = 137.33
         VIRTUAL-HEIGHT     = 36.71
         VIRTUAL-WIDTH      = 137.33
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
/* SETTINGS FOR FRAME FrmLaenutused
   FRAME-NAME                                                           */
/* BROWSE-TAB Blaenutused 1 FrmLaenutused */
ASSIGN 
       BtnLisa:HIDDEN IN FRAME FrmLaenutused           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Blaenutused
/* Query rebuild information for BROWSE Blaenutused
     _START_FREEFORM
(NEW back.laenutus()):GetLaenutused(fpealkiri:INPUT-VALUE, fautor:INPUT-VALUE, fISBN:INPUT-VALUE, INPUT-OUTPUT TABLE tlaenutused).
OPEN QUERY {&SELF-NAME} FOR EACH tlaenutused NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Blaenutused */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FrmLaenutused
/* Query rebuild information for FRAME FrmLaenutused
     _TblList          = "raamat.kasutajad"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* FRAME FrmLaenutused */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raamatueksemplaride ja laenutuste loend */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raamatueksemplaride ja laenutuste loend */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME FrmLaenutused /* Välju */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnKustuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnKustuta C-Win
ON CHOOSE OF BtnKustuta IN FRAME FrmLaenutused /* Tagastus */
DO:
  DEFINE VARIABLE laenutaja_id AS INTEGER NO-UNDO.
  DEFINE VARIABLE FocusedRow AS INTEGER.
  
  FocusedRow =  blaenutused:FOCUSED-ROW .
  (NEW back.laenutus()):endLaenutus(tlaenutused.id).
  {&OPEN-QUERY-Blaenutused}
  blaenutused:SELECT-ROW (MIN(blaenutused:NUM-ITERATIONS,FocusedRow)).  

/*
  DEFINE VARIABLE FocusedRow AS INTEGER.
  FocusedRow =  braamatud:FOCUSED-ROW .
  IF (NEW back.raamat()):DelRaamat(traamatud.id) THEN DO:
    {&OPEN-QUERY-Braamatud}  
     braamatud:SELECT-ROW (MIN(braamatud:NUM-ITERATIONS,FocusedRow)).  
  END.
  ELSE
     MESSAGE "Raamatut '" + traamatud.pealkiri + "' ei saa kustutada." + CHR(10) + "Sellel on väljalaenutatud eksemplare." VIEW-AS ALERT-BOX.
*/    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLisa C-Win
ON CHOOSE OF BtnLisa IN FRAME FrmLaenutused /* Laenuta */
DO:
  DEFINE VARIABLE laenutaja_id AS INTEGER NO-UNDO.
  DEFINE VARIABLE FocusedRow AS INTEGER.
  
  IF tlaenutused.laenutaja_id <> -1 THEN
  DO:
    MESSAGE "Raamatu '" + tlaenutused.pealkiri + "' see eksemplar on juba välja laenutatud." VIEW-AS ALERT-BOX.
    RETURN.
  END.
  FocusedRow =  blaenutused:FOCUSED-ROW .
  RUN Front/Kasutajad(FALSE).
  laenutaja_id = INTEGER(RETURN-VALUE).
  IF laenutaja_id>=0 THEN
  DO:
    (NEW back.laenutus()):StartLaenutus(tlaenutused.id, laenutaja_id).
    {&OPEN-QUERY-Blaenutused}
    blaenutused:SELECT-ROW (MIN(blaenutused:NUM-ITERATIONS,FocusedRow)).                
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOtsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOtsi C-Win
ON CHOOSE OF BtnOtsi IN FRAME FrmLaenutused /* Otsi */
DO:
  {&OPEN-QUERY-Blaenutused}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fautor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fautor C-Win
ON RETURN OF fautor IN FRAME FrmLaenutused /* Autor */
DO:
  {&OPEN-QUERY-Blaenutused}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fISBN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fISBN C-Win
ON RETURN OF fISBN IN FRAME FrmLaenutused /* ISBN */
DO:
  {&OPEN-QUERY-Blaenutused}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fpealkiri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fpealkiri C-Win
ON RETURN OF fpealkiri IN FRAME FrmLaenutused /* Pealkiri */
DO:
  {&OPEN-QUERY-Blaenutused}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Blaenutused
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

  {&OPEN-QUERY-FrmLaenutused}
  GET FIRST FrmLaenutused.
  DISPLAY fpealkiri fautor fISBN 
      WITH FRAME FrmLaenutused IN WINDOW C-Win.
  ENABLE Blaenutused fpealkiri fautor fISBN BtnOtsi BtnLisa BtnKustuta BtnDone 
      WITH FRAME FrmLaenutused IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrmLaenutused}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

