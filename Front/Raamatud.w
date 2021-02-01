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

DEFINE TEMP-TABLE traamatud LIKE raamatud.

DEFINE VARIABLE clRaamat AS back.raamat.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrmRaamatud
&Scoped-define BROWSE-NAME Braamatud

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES traamatud kasutajad

/* Definitions for BROWSE Braamatud                                     */
&Scoped-define FIELDS-IN-QUERY-Braamatud //traamatud.id traamatud.pealkiri traamatud.autor traamatud.asukoht traamatud.ISBN traamatud.kuupaev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Braamatud   
&Scoped-define SELF-NAME Braamatud
&Scoped-define OPEN-QUERY-Braamatud (NEW back.raamat()):GetRaamatud(fpealkiri:INPUT-VALUE, ~
      fautor:INPUT-VALUE, ~
      fISBN:INPUT-VALUE, ~
      fasukoht:INPUT-VALUE, ~
      INPUT-OUTPUT TABLE traamatud). OPEN QUERY {&SELF-NAME} FOR EACH traamatud NO-LOCK BY traamatud.pealkiri INDEXED-REPOSITION .
&Scoped-define TABLES-IN-QUERY-Braamatud traamatud
&Scoped-define FIRST-TABLE-IN-QUERY-Braamatud traamatud


/* Definitions for FRAME FrmRaamatud                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FrmRaamatud ~
    ~{&OPEN-QUERY-Braamatud}
&Scoped-define QUERY-STRING-FrmRaamatud FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-FrmRaamatud OPEN QUERY FrmRaamatud FOR EACH kasutajad SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-FrmRaamatud kasutajad
&Scoped-define FIRST-TABLE-IN-QUERY-FrmRaamatud kasutajad


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Braamatud BtnOtsi BtnExit 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
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

DEFINE VARIABLE farv AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Raamatute arv" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fasukoht AS CHARACTER FORMAT "X(256)":U 
     LABEL "Asukoht" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE fautor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Autor" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fISBN AS CHARACTER FORMAT "X(256)":U 
     LABEL "ISBN" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fkuupaev AS DATE FORMAT "99/99/9999":U 
     LABEL "Kuupäev" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fpealkiri AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pealkiri" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Braamatud FOR 
      traamatud SCROLLING.

DEFINE QUERY FrmRaamatud FOR 
      kasutajad SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Braamatud
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Braamatud C-Win _FREEFORM
  QUERY Braamatud DISPLAY
      //traamatud.id FORMAT "->,>>>,>>9":U        
traamatud.pealkiri FORMAT "x(20)":U
traamatud.autor FORMAT "x(20)":U
traamatud.asukoht FORMAT "x(20)":U
traamatud.ISBN FORMAT "x(20)":U
traamatud.kuupaev FORMAT "99/99/9999":U WIDTH 48.83
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112.5 BY 25.71 ROW-HEIGHT-CHARS .71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmRaamatud
     Braamatud AT ROW 1.71 COL 3.5 WIDGET-ID 200
     fpealkiri AT ROW 28.14 COL 8 COLON-ALIGNED WIDGET-ID 8
     fautor AT ROW 28.14 COL 28.33 COLON-ALIGNED WIDGET-ID 10
     fasukoht AT ROW 28.14 COL 51.33 COLON-ALIGNED WIDGET-ID 12
     fISBN AT ROW 28.14 COL 72 COLON-ALIGNED WIDGET-ID 16
     fkuupaev AT ROW 28.14 COL 96.83 COLON-ALIGNED WIDGET-ID 14
     farv AT ROW 30.29 COL 14.33 COLON-ALIGNED WIDGET-ID 20
     BtnLisa AT ROW 30.29 COL 36 WIDGET-ID 4
     BtnKustuta AT ROW 30.29 COL 56.83 WIDGET-ID 18
     BtnOtsi AT ROW 30.29 COL 78 WIDGET-ID 40
     BtnExit AT ROW 30.29 COL 96.5 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.83 BY 31.76 WIDGET-ID 100.


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
         TITLE              = "Raamatute nimestik"
         HEIGHT             = 31.76
         WIDTH              = 118.83
         MAX-HEIGHT         = 31.76
         MAX-WIDTH          = 137.33
         VIRTUAL-HEIGHT     = 31.76
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
/* SETTINGS FOR FRAME FrmRaamatud
   FRAME-NAME                                                           */
/* BROWSE-TAB Braamatud 1 FrmRaamatud */
/* SETTINGS FOR BUTTON BtnKustuta IN FRAME FrmRaamatud
   NO-ENABLE                                                            */
ASSIGN 
       BtnKustuta:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR BUTTON BtnLisa IN FRAME FrmRaamatud
   NO-ENABLE                                                            */
ASSIGN 
       BtnLisa:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN farv IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       farv:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN fasukoht IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fasukoht:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN fautor IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fautor:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN fISBN IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fISBN:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN fkuupaev IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fkuupaev:HIDDEN IN FRAME FrmRaamatud           = TRUE.

/* SETTINGS FOR FILL-IN fpealkiri IN FRAME FrmRaamatud
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fpealkiri:HIDDEN IN FRAME FrmRaamatud           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Braamatud
/* Query rebuild information for BROWSE Braamatud
     _START_FREEFORM
(NEW back.raamat()):GetRaamatud(fpealkiri:INPUT-VALUE,fautor:INPUT-VALUE,fISBN:INPUT-VALUE,fasukoht:INPUT-VALUE,INPUT-OUTPUT TABLE traamatud).
OPEN QUERY {&SELF-NAME} FOR EACH traamatud NO-LOCK BY traamatud.pealkiri INDEXED-REPOSITION .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE Braamatud */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FrmRaamatud
/* Query rebuild information for FRAME FrmRaamatud
     _TblList          = "raamat.kasutajad"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* FRAME FrmRaamatud */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raamatute nimestik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raamatute nimestik */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit C-Win
ON CHOOSE OF BtnExit IN FRAME FrmRaamatud /* Välju */
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
ON CHOOSE OF BtnKustuta IN FRAME FrmRaamatud /* Kustuta */
DO:
  DEFINE VARIABLE FocusedRow AS INTEGER.
  FocusedRow =  braamatud:FOCUSED-ROW .
  IF braamatud:NUM-ITERATIONS>0 THEN
  DO:
      IF (NEW back.raamat()):DelRaamat(traamatud.id) THEN DO:
        {&OPEN-QUERY-Braamatud}
        IF MIN(braamatud:NUM-ITERATIONS,FocusedRow)>0 THEN
           braamatud:SELECT-ROW (MIN(braamatud:NUM-ITERATIONS,FocusedRow)).  
      END.
      ELSE
         MESSAGE "Raamatut '" + traamatud.pealkiri + "' ei saa kustutada." + CHR(10) + "Sellel on väljalaenutatud eksemplare." VIEW-AS ALERT-BOX.      
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLisa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLisa C-Win
ON CHOOSE OF BtnLisa IN FRAME FrmRaamatud /* Lisa */
DO:
  IF farv:INPUT-VALUE<1 THEN
    MESSAGE 'Raamatute arv peab olema suurem kui 0'. 
  ELSE DO:
      (NEW back.raamat()):AddRaamat(fpealkiri:INPUT-VALUE, fautor:INPUT-VALUE, fasukoht:INPUT-VALUE, fkuupaev:INPUT-VALUE, fISBN:INPUT-VALUE, farv:INPUT-VALUE).
    {&OPEN-QUERY-Braamatud}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOtsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOtsi C-Win
ON CHOOSE OF BtnOtsi IN FRAME FrmRaamatud /* Otsi */
DO:
   {&OPEN-QUERY-Braamatud}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME farv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL farv C-Win
ON LEAVE OF farv IN FRAME FrmRaamatud /* Raamatute arv */
DO:
  IF farv:INPUT-VALUE<1 THEN
     farv=1.
  RUN enable_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fasukoht
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fasukoht C-Win
ON RETURN OF fasukoht IN FRAME FrmRaamatud /* Asukoht */
DO:
  {&OPEN-QUERY-Braamatud}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fautor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fautor C-Win
ON RETURN OF fautor IN FRAME FrmRaamatud /* Autor */
DO:
  {&OPEN-QUERY-Braamatud}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fISBN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fISBN C-Win
ON RETURN OF fISBN IN FRAME FrmRaamatud /* ISBN */
DO:
  {&OPEN-QUERY-Braamatud}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fpealkiri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fpealkiri C-Win
ON RETURN OF fpealkiri IN FRAME FrmRaamatud /* Pealkiri */
DO:
  {&OPEN-QUERY-Braamatud}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Braamatud
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

  fkuupaev=TODAY.
  farv=1.
  IF admin THEN DO:
     ENABLE BtnLisa     WITH FRAME FrmRaamatud.
     ENABLE BtnKustuta  WITH FRAME FrmRaamatud.
     ENABLE fpealkiri   WITH FRAME FrmRaamatud.
     ENABLE fautor      WITH FRAME FrmRaamatud.
     ENABLE fasukoht    WITH FRAME FrmRaamatud.
     ENABLE fkuupaev    WITH FRAME FrmRaamatud.
     ENABLE fISBN       WITH FRAME FrmRaamatud.
     ENABLE farv        WITH FRAME FrmRaamatud.
     DISPLAY fkuupaev   WITH FRAME FrmRaamatud.
     DISPLAY farv       WITH FRAME FrmRaamatud.
  END.
  
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

  {&OPEN-QUERY-FrmRaamatud}
  GET FIRST FrmRaamatud.
  ENABLE Braamatud BtnOtsi BtnExit 
      WITH FRAME FrmRaamatud IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrmRaamatud}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

