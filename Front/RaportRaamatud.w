&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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

{back/raamaturapStru.i}  

DEFINE BUFFER braamaturaport FOR traamaturaport.

DEFINE VARIABLE clLaenutus AS back.laenutus.


/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FrmRaamatuRaport
&Scoped-define BROWSE-NAME BraamatuRaport

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES traamaturaport

/* Definitions for BROWSE BraamatuRaport                                */
&Scoped-define FIELDS-IN-QUERY-BraamatuRaport traamaturaport.pealkiri traamaturaport.autor traamaturaport.asukoht traamaturaport.ISBN traamaturaport.arv traamaturaport.tagasi traamaturaport.vaba traamaturaport.tahtaeg //traamaturaport.kuupaev   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BraamatuRaport   
&Scoped-define SELF-NAME BraamatuRaport
&Scoped-define OPEN-QUERY-BraamatuRaport clLaenutus:GetRaportRaamatud(fpealkiri:INPUT-VALUE, ~
       fautor:INPUT-VALUE, ~
       fISBN:INPUT-VALUE, ~
       INPUT-OUTPUT TABLE traamaturaport). OPEN QUERY {&SELF-NAME} FOR EACH traamaturaport NO-LOCK BY traamaturaport.pealkiri INDEXED-REPOSITION .
&Scoped-define TABLES-IN-QUERY-BraamatuRaport traamaturaport
&Scoped-define FIRST-TABLE-IN-QUERY-BraamatuRaport traamaturaport


/* Definitions for FRAME FrmRaamatuRaport                               */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FrmRaamatuRaport ~
    ~{&OPEN-QUERY-BraamatuRaport}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BraamatuRaport fpealkiri fautor fISBN ~
BtnOtsi BtnExit 
&Scoped-Define DISPLAYED-OBJECTS fpealkiri fautor fISBN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnExit DEFAULT 
     LABEL "&Välja" 
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
DEFINE QUERY BraamatuRaport FOR 
      traamaturaport SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BraamatuRaport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BraamatuRaport C-Win _FREEFORM
  QUERY BraamatuRaport DISPLAY
      traamaturaport.pealkiri FORMAT "x(30)":U
traamaturaport.autor    FORMAT "x(20)":U
traamaturaport.asukoht  FORMAT "x(20)":U
traamaturaport.ISBN     FORMAT "x(12)":U
traamaturaport.arv      FORMAT ">>9":U             LABEL 'Raamatute üldarv'
traamaturaport.tagasi   FORMAT "99/99/9999":U      LABEL 'Lähim tagastus'
traamaturaport.vaba     FORMAT ">>9":U             LABEL 'Saadaval'
traamaturaport.tahtaeg  FORMAT "99/99/9999":U      LABEL 'Tähtaeg'
//traamaturaport.kuupaev  FORMAT "99/99/9999":U      LABEL 'Raamatukokku saabunud'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 139 BY 25.24 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FrmRaamatuRaport
     BraamatuRaport AT ROW 1.71 COL 3 WIDGET-ID 200
     fpealkiri AT ROW 28.14 COL 8 COLON-ALIGNED WIDGET-ID 8
     fautor AT ROW 28.14 COL 28.33 COLON-ALIGNED WIDGET-ID 10
     fISBN AT ROW 28.14 COL 49.5 COLON-ALIGNED WIDGET-ID 16
     BtnOtsi AT ROW 28.14 COL 87.5 WIDGET-ID 4
     BtnExit AT ROW 28.14 COL 112 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 143.33 BY 29.48
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
         TITLE              = "Raamatute seisu aruanne"
         HEIGHT             = 29.48
         WIDTH              = 143.33
         MAX-HEIGHT         = 41.38
         MAX-WIDTH          = 175.5
         VIRTUAL-HEIGHT     = 41.38
         VIRTUAL-WIDTH      = 175.5
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
/* SETTINGS FOR FRAME FrmRaamatuRaport
   FRAME-NAME                                                           */
/* BROWSE-TAB BraamatuRaport 1 FrmRaamatuRaport */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BraamatuRaport
/* Query rebuild information for BROWSE BraamatuRaport
     _START_FREEFORM
clLaenutus:GetRaportRaamatud(fpealkiri:INPUT-VALUE, fautor:INPUT-VALUE, fISBN:INPUT-VALUE, INPUT-OUTPUT TABLE traamaturaport).
OPEN QUERY {&SELF-NAME} FOR EACH traamaturaport NO-LOCK BY traamaturaport.pealkiri INDEXED-REPOSITION .
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BraamatuRaport */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Raamatute seisu aruanne */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Raamatute seisu aruanne */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit C-Win
ON CHOOSE OF BtnExit IN FRAME FrmRaamatuRaport /* Välja */
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


&Scoped-define SELF-NAME BtnOtsi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOtsi C-Win
ON CHOOSE OF BtnOtsi IN FRAME FrmRaamatuRaport /* Otsi */
DO:
   {&OPEN-QUERY-Braamaturaport}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOtsi C-Win
ON RETURN OF BtnOtsi IN FRAME FrmRaamatuRaport /* Otsi */
DO:
   {&OPEN-QUERY-Braamaturaport}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fautor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fautor C-Win
ON RETURN OF fautor IN FRAME FrmRaamatuRaport /* Autor */
DO:
   {&OPEN-QUERY-Braamaturaport}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fISBN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fISBN C-Win
ON RETURN OF fISBN IN FRAME FrmRaamatuRaport /* ISBN */
DO:
   {&OPEN-QUERY-Braamaturaport}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fpealkiri
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fpealkiri C-Win
ON RETURN OF fpealkiri IN FRAME FrmRaamatuRaport /* Pealkiri */
DO:
   {&OPEN-QUERY-Braamaturaport}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BraamatuRaport
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
   clLaenutus = NEW back.laenutus().
//   clLaenutus:GetRaportRaamatud(INPUT -1, INPUT-OUTPUT TABLE traamaturaport).

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
  DISPLAY fpealkiri fautor fISBN 
      WITH FRAME FrmRaamatuRaport IN WINDOW C-Win.
  ENABLE BraamatuRaport fpealkiri fautor fISBN BtnOtsi BtnExit 
      WITH FRAME FrmRaamatuRaport IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FrmRaamatuRaport}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

