unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRaoulDisplay, ExtCtrls, ActnList, ClipBrd,
  {dos utilities}
  uDosUtils,

  {Parameters manager}
  uRaoulParameters,

  {Vocal}
  uRaoulRecorder, uNewRecorder, SpeechLib_TLB,

  {Racccourcis clavier}
  SendKey32,

  {Accès SQL distant}
  uSQLTransaction, uRaoulDB,

  {Elite bindings}
  EliteBindingsTools,

  {Help}
  uEliteHelp, uHelpDlg;


type
  TMainForm = class(TForm)
    RaoulKeys: TButton;
    DelayedStart: TTimer;
    DistantData: TDistantData;
    ActionList1: TActionList;
    BootDelayed: TTimer;
    BootFinalize: TTimer;
    procedure BootFinalizeTimer(Sender: TObject);
    procedure BootDelayedTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DistantDataPrepare(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DelayedStartTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure KeyBack(Sender: TObject);
    procedure KeyReturn(Sender: TObject);
    procedure KeyCtrlReturn(Sender: TObject);
    function GetCanCloseIdx: Integer;
    procedure SetCanCloseIdx(const Value: Integer);

  private
    FStarter: Boolean;
    procedure StarterProc;

  public
    procedure AssignKeyFun;
    procedure ForegroundDisplay;

    property CanCloseIdx: Integer read GetCanCloseIdx write SetCanCloseIdx;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
 uRegistry, DisplayTextForm, ScreenDlg, uEmbendedAncestor, uEmbendedDlg,
 Math, uRecorderTools, KeysDef, uRaoulUpdater, uEliteManager, WebAPIs,
 uSplashWaitForm, uGaussDisplay;

procedure TMainForm.AssignKeyFun;
begin
  KeyBackFunc       := KeyBack;
  KeyReturnFunc     := KeyReturn;
  KeyCtrlReturnFunc := KeyCtrlReturn;
end;

procedure TMainForm.ForegroundDisplay;
begin
  SetForegroundWindow(Handle);
  SetFocus
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  { --- INITIAISATTION EXTERNE }
  TAppParameters.Initialize;                     { --- Instantiation des paramètres de l'applicatif }
  TAppUpdater.TryReplaceUpdater;                 { --- Remplace l'updater si nécessaire }
  TAppUpdater.TryToUpdate;                       { --- Normalement se referme ici si l'update se lance }

  { --- If Internet connexion failed }
  if not InternetCheck then TSplashWaitForm.Splash('LOCAL START CHECKED');
  { --- Define main thread priority }
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS );
  
  { --- INITTIALISATTION INTERNE }
  FStarter := True;
  TKeyMessageSender.Initialize;                  { --- Instantiation des classes  gérant le bindings d'Elite }
  IpMacInitialize;                               { --- Initialise Elite bindings }
  with DistantData do Prepare;                   { --- Initialize les accès aux données distantes }

  with ToolsDisplay do KeyInitialize( RaoulKeys );
  TFunctionment.Initialize;                      { --- Initialisattion de la factory }
  TFunTalk.Initialize;
  CanCloseIdx := 0;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  { --- Reload Help files if not boot process}
  with THelpDlg do if not RetrieveBootProcess then HelpReload;
  { --- EliteBindings Finalize initialization }
  try
    TKeyInventory.Initialize;
    TEliteManager.KeyInventoryAssign(KeyInventory);
    KeyWrite(AppKey, 'EliteBindingsError', 0)
  except
    { --- Indiquer que pour la suite la fonction Elite bindings sera indiponible }
    on E:Exception do begin
      KeyWrite(AppKey, 'EliteBindingsError', 1);
      MsgDlg(E.Message, mtWarning, [mbOK], 0)
    end
  end;
  { --- RaoulFumier finalize initialization }
  TToolsDisplay.Initialize;                      { --- Only with Starter}
  AssignKeyFun;                                  { --- Pour les événements gérés pour les tests pré-production }
  with TalkativeFacade do
    TalkativeView := KeyReadBoolean(BufferKey, 'TalkativeDialogBoxView', False);
  if FStarter then StarterProc;
end;

procedure TMainForm.KeyBack(Sender: TObject);
begin
  {for tests pré-peoduction }

  //if Assigned(AppCloseFunc) then AppCloseFunc(nil)
end;

procedure TMainForm.KeyCtrlReturn(Sender: TObject);
begin
  {for tests pré-peoduction }
//  TAppUpdater.TryToRelauch;                      
  ForegroundDisplay
end;

procedure TMainForm.KeyReturn(Sender: TObject);
begin
  {For tests pré-peoduction}
  ForegroundDisplay
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  with GaussDisplayForm do if Visible then Close;
  {Recorder}
  InternRecorder.DoOnClose;
  TSQLRaoul.UserDataExport;
end;

procedure TMainForm.DelayedStartTimer(Sender: TObject);
{ --- delayed of MainStartDelayed value --> let Grammar check process running }
begin
  { --- Lancement retardé après affichage }
  if KeyReadBoolean(BufferKey, 'RecorderStarted') then begin
    with DelayedStart do Enabled := False;
    with TalkativeFacade do begin
      PostInitialize;
      CommandText := 'En sommeil'
    end;
    { --- Help prepare if not Boot process }
    with THelpDlg do if not IsBootProcess then HelpDefine
      else BootDelayed.Enabled := True
  end;
  { --- Si éventuellement l'avertissement est ouvert }
  TSplashWaitForm.SplashHide
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TFunctionment.Finalize;
end;

procedure TMainForm.StarterProc;
{ --- Finalization du FormShow }
begin
  FStarter := False;
  TInternRecorder.Initialize;                    { --- Recorder instanciation et initialisaton}
  TRecordLoader.Create(Recorder);                { --- Instantiation et démarrage du thread chargé d'activer le recorder }
  with DelayedStart do begin
    Interval := MainStartDelayed + 1500; 
    Enabled  := True
  end
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  ForegroundDisplay                              { --- Forcer l'application au premier plan en toutes circonstances }
end;

procedure TMainForm.DistantDataPrepare(Sender: TObject);
{ --- Evénement réalisé lors de l'instantiation de la classe TDistantData }
begin
  { --- Ne se connecte pas au serveur SQL
          si mot de passe serveur est absent ou
          si Internet est inacessible
     Les mises à jour automatiques ne sont pas gérées, durant toute la session }
  if (DistantData.Password = EmptyStr) or not InternetCheck then Exit;

  { --- Vérification de la version : effectue la mise à jour si nécessaire }
  if not Assigned(SQLRaoul) then SQLRaoul := TSQLRaoul.Create;

  with SQLRaoul do SetDistantData( DistantData );
  TSQLRaoul.Inscription;
  TSQLRaoul.CheckRegistration;

  { --- Si les grammaires ont été mise à jour alors redémarrer Raoul }
  if TAppUpdater.TryGrammarUpdate then TAppUpdater.TryToRelauch;
  { --- Charger la mise à jour }
  if KeyReadInt(Appkey, 'UpdateAction') = 1 then TAppUpdater.TryToLoad;
  { --- Confirm user Pseudo }
  TSQLRaoul.CheckPseudo;
end;

function TMainForm.GetCanCloseIdx: Integer;
begin
  Result := KeyReadInt(ParamKey, 'CanCloseIdx')
end;

procedure TMainForm.SetCanCloseIdx(const Value: Integer);
begin
  KeyWrite(ParamKey, 'CanCloseIdx', Value)
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  { --- Alt+F4 sur application interdit }
  case CanCloseIdx of
    0 : CanClose := False else CanClose := True;
  end
end;

procedure TMainForm.BootDelayedTimer(Sender: TObject);
begin
  BootDelayed.Enabled := False;
  THelpDlg.HelpReload;
  BootFinalize.Enabled := True
end;

procedure TMainForm.BootFinalizeTimer(Sender: TObject);
begin
  BootFinalize.Enabled := False;
  THelpDlg.HelpDefine;
  KeyWrite(AppKey, 'BootState', False)
end;

end.
