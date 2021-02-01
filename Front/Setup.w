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

DEFINE TEMP-TABLE setup LIKE setup.

DEFINE VARIABLE clSetup AS back.setup.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES setup

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH setup SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH setup SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME setup
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME setup


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS setup.raamat_tahtaeg setup.uus_raamat ~
setup.uusraamat_tahtaeg setup.vahe_raamat setup.vaheraamat_tahtaeg 
&Scoped-define ENABLED-TABLES setup
&Scoped-define FIRST-ENABLED-TABLE setup
&Scoped-Define ENABLED-OBJECTS BtnDone BtnExit 
&Scoped-Define DISPLAYED-FIELDS setup.raamat_tahtaeg setup.uus_raamat ~
setup.uusraamat_tahtaeg setup.vahe_raamat setup.vaheraamat_tahtaeg 
&Scoped-define DISPLAYED-TABLES setup
&Scoped-define FIRST-DISPLAYED-TABLE setup


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salvesta ja V�lju" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnExit DEFAULT 
     LABEL "&V�lju" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      setup SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     setup.raamat_tahtaeg AT ROW 1.95 COL 54.17 COLON-ALIGNED WIDGET-ID 44
          LABEL "Raamatu laenutamise perioodi pikkus p�evades"
          VIEW-AS FILL-IN 
          SIZE 8.67 BY 1
     setup.uus_raamat AT ROW 3.86 COL 54 COLON-ALIGNED WIDGET-ID 46
          LABEL "'Uue raamatu' vanus p�evades"
          VIEW-AS FILL-IN 
          SIZE 8.83 BY 1
     setup.uusraamat_tahtaeg AT ROW 5.76 COL 54 COLON-ALIGNED WIDGET-ID 48
          LABEL "'Uue raamatu' laenutuse pikkus p�evades"
          VIEW-AS FILL-IN 
          SIZE 8.83 BY 1
     setup.vahe_raamat AT ROW 7.67 COL 54.17 COLON-ALIGNED WIDGET-ID 50
          LABEL "Eksemplaride arv, millest alates on laenutus l�hem"
          VIEW-AS FILL-IN 
          SIZE 8.67 BY 1
     setup.vaheraamat_tahtaeg AT ROW 9.62 COL 54.17 COLON-ALIGNED WIDGET-ID 52
          LABEL "'V�hese eksemplaride arvuga' raamatu laenutuse pikkus"
          VIEW-AS FILL-IN 
          SIZE 8.67 BY 1
     BtnDone AT ROW 12.43 COL 21.5 WIDGET-ID 40
     BtnExit AT ROW 12.43 COL 42.67 WIDGET-ID 38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.57 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 13.57
         WIDTH              = 80
         MAX-HEIGHT         = 26.29
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 26.29
         VIRTUAL-WIDTH      = 80
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN setup.raamat_tahtaeg IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN setup.uusraamat_tahtaeg IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN setup.uus_raamat IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN setup.vaheraamat_tahtaeg IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN setup.vahe_raamat IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "setup"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Salvesta ja V�lju */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      IF NOT (NEW back.setup()):PutSetup(INPUT-OUTPUT TABLE setup) THEN
      MESSAGE 'Muudatuste salvestamine eba�nnestus'.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnExit C-Win
ON CHOOSE OF BtnExit IN FRAME DEFAULT-FRAME /* V�lju */
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


&Scoped-define SELF-NAME setup.raamat_tahtaeg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setup.raamat_tahtaeg C-Win
ON LEAVE OF setup.raamat_tahtaeg IN FRAME DEFAULT-FRAME /* Raamatu laenutamise perioodi pikkus p�evades */
DO:
  setup.raamat_tahtaeg=raamat_tahtaeg:INPUT-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setup.uusraamat_tahtaeg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setup.uusraamat_tahtaeg C-Win
ON LEAVE OF setup.uusraamat_tahtaeg IN FRAME DEFAULT-FRAME /* 'Uue raamatu' laenutuse pikkus p�evades */
DO:
  setup.uusraamat_tahtaeg=uusraamat_tahtaeg:INPUT-VALUE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setup.uus_raamat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setup.uus_raamat C-Win
ON LEAVE OF setup.uus_raamat IN FRAME DEFAULT-FRAME /* 'Uue raamatu' vanus p�evades */
DO:
  setup.uus_raamat=uus_raamat:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setup.vaheraamat_tahtaeg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setup.vaheraamat_tahtaeg C-Win
ON LEAVE OF setup.vaheraamat_tahtaeg IN FRAME DEFAULT-FRAME /* 'V�hese eksemplaride arvuga' raamatu laenutuse pikkus */
DO:
  setup.vaheraamat_tahtaeg=vaheraamat_tahtaeg:INPUT-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setup.vahe_raamat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setup.vahe_raamat C-Win
ON LEAVE OF setup.vahe_raamat IN FRAME DEFAULT-FRAME /* Eksemplaride arv, millest alates on laenutus l�hem */
DO:
  setup.vahe_raamat=vahe_raamat:INPUT-VALUE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

  (NEW back.setup()):Getsetup(INPUT-OUTPUT TABLE setup).
   
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  IF AVAILABLE setup THEN 
    DISPLAY setup.raamat_tahtaeg setup.uus_raamat setup.uusraamat_tahtaeg 
          setup.vahe_raamat setup.vaheraamat_tahtaeg 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE setup.raamat_tahtaeg setup.uus_raamat setup.uusraamat_tahtaeg 
         setup.vahe_raamat setup.vaheraamat_tahtaeg BtnDone BtnExit 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

