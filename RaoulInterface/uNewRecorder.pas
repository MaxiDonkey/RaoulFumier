{*******************************************************}
{                                                       }
{             04/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit uNewRecorder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpeechLib_TLB, OleServer, ActiveX, StdCtrls, ComCtrls, StrCopyUtils,
  uRecorderTools, Math, StrUtils, uEliteManager, Clipbrd, uWinKeyShortCut,

  {devExpress}
  cxProgressBar, cxTrackBar;

const
  FRENCH_ID   = $40c;
  MAX_GRAMMAR = 99;

type
  TArrayOfGrammars  = array[0..MAX_GRAMMAR] of ISpeechRecoGrammar;

  TRecordModes      = (rm_sleep, rm_listen);
  TSRState          = (srs_inactive, srs_active, srs_activealways, srs_inactivewithpurge);
  TSensibility      = (s_highperformance, s_headphonemic, s_studiomic, s_speakermic );
  TAudioInput       = (ai_none, ai_interface);
  TWavEmulate       = (we_disabled, we_enabled);
  TInterferenceKind = (
    ik_SINone,     ik_SINoise,   ik_SINoSignal,  ik_SITooLoud,
    ik_SITooQuiet, ik_SITooFast, ik_SITooSlow                       );
  TGramType         = (gt_switch, gt_fumier, gt_gauss, gt_spell, gt_elite, gt_pause);        //A-G
  TMetiers          = (m_none, m_fumier, m_gauss, m_spell, m_elite, m_pause);                //A-G

  TArrayOfJobFiles  = array[TGramType] of string;
  TArrayOfNoises    = array[TInterferenceKind] of Integer;

  TAudioNotifyEvent = procedure(Sender: TObject; AudioLvl: Integer) of object;
  TIntNotifyEvent   = procedure(Sender: TObject; Interfer: TInterferenceKind; Noises: TArrayOfNoises) of object;
  TRecoNotifyEvent  = procedure(Sender: TObject; Text: string) of object;
  TSMLNotifyEvent   = procedure(Sender: TObject; Text, SML: string) of object;
  TAudioChanged     = procedure(Sender: TObject; NewMode: Boolean) of object;
  TMetierChanged    = procedure(Sender: TObject; NewMetier: TMetiers) of object;
  TTauxChange       = procedure(Sender: TObject; ATaux: Double) of object;
  TModeChange       = procedure(Sender: TObject; OldMode: TRecordModes;
    var NewMode: TRecordModes; var Handled: Boolean) of object;

type
  TRaoulRecorder = class(TComponent)
  private
    FForm: TForm;
    Fmaprop:Integer;
    function  FormRetrieve: TForm;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property maprop:Integer read Fmaprop write Fmaprop;
  end;

  TModeFonctionnement = (mf_none, mf_sensibility, mf_closeapp);

  TCustomDefRecorder = class
  private
    function  GetState: TSRState;
    function  GetListen: Boolean;
    procedure SetMode(const Value: TRecordModes);
    procedure SetModeFonctionnement(const Value: TModeFonctionnement);
    function  GetModeFonctionnement: TModeFonctionnement;
    function  GetOkExpected: Boolean;
    procedure SetOkExpected(const Value: Boolean);
    function  GetPrevMetier: TMetiers;
    procedure SetPrevMetier(const Value: TMetiers);

  private
    { --- Constantes d'environnement }
    HKMLCategory         : string;
    AGNES_DEFAULT_VOICE  : string;
    { --- Variables d'environnement }
    FCategory            : TSpObjectTokenCategory;
    FToken               : TSpObjectToken;
    { --- DATA functionment }
    FLock                : Boolean;
    FGrammars            : TArrayOfGrammars;
    FGramCount           : Integer;
    FMicroDescription    : string;
    FMode                : TRecordModes;
    FCurrentMetier       : TMetiers;
    FTaux                : Double;
    FVuMetre             : TcxProgressBar;
    FNoises              : TArrayOfNoises;
    FInterference        : TInterferenceKind;

    { --- Local Events }
    FOnAudioChanged      : TAudioChanged;
    FOnAudioLevelChange  : TAudioNotifyEvent;
    FOnMetierChanged     : TMetierChanged;
    FOnTauxChange        : TTauxChange;
    FOnRecognizeAccepted : TSMLNotifyEvent;
    FOnRecognizeReject   : TNotifyEvent;
    FOnBuildHypothesis   : TRecoNotifyEvent;
    FOnSleepMode         : TNotifyEvent;
    FOnAwakeMode         : TNotifyEvent;
    FOnEndStream         : TNotifyEvent;
    FOnSoundEnd          : TNotifyEvent;
    FOnPhraseStart       : TNotifyEvent;
    FOnStartStream       : TNotifyEvent;
    FOnStartSound        : TNotifyEvent;
    FOnGramReload        : TRecoNotifyEvent;
    FOnGramStartReload   : TRecoNotifyEvent;
    FOnInterference      : TIntNotifyEvent;
    { --- Metiers events }
    FOnEliteFocus        : TNotifyEvent;

  private
    function  GrammarCreate(AFileName: string; var Index: Integer): ISpeechRecoGrammar;
    procedure GrammarActivate(grType: TGramType; grState: Boolean=True);
    procedure GrammarReload(grType: TGramType);
    function  GrammarFileName(grType: TGramType):string;
    procedure CreateContext;

  private
    procedure VuMetreReset;
    procedure VuMetreSetter(const Apos: Integer);
    procedure NoisesReset;
    procedure NoisesInc(const Item: TInterferenceKind);

  protected
    procedure SetState(const Value: TSRState); virtual;
    procedure SetListen(const Value: Boolean); virtual;
    procedure SetCurrentMetier(const Value: TMetiers); virtual;
    procedure SetTaux(const Value: Double); virtual;

  protected
    procedure FoldersInitialize;
    procedure ComomnInitialization; virtual; abstract;
    procedure DefineEvents; virtual; abstract;
    procedure GrammarLoading; virtual; abstract;
    procedure SetSleepMode; virtual;
    procedure SetAwakeMode; virtual;
    procedure GrammarsDisable; virtual;

  public
    { --- Variables d'environnement publiques }
    Context              : TSpInProcRecoContext;
    Recognizer           : ISpeechRecognizer;

    procedure Initialize;
    procedure Disable;
    procedure Enable;

    property Lock: Boolean read FLock write FLock;
    property Mode: TRecordModes read FMode write SetMode;
    property State: TSRState read GetState write SetState;
    property Listen: Boolean read GetListen write SetListen;
    property CurrentMetier: TMetiers read FCurrentMetier write SetCurrentMetier;
    property MicroDescription: string read FMicroDescription;
    property Token: TSpObjectToken read FToken;
    property Taux: Double read FTaux write SetTaux;
    property Noises : TArrayOfNoises read FNoises;
    property ModeFonctionnement: TModeFonctionnement read GetModeFonctionnement write SetModeFonctionnement;
    property OkExpected: Boolean read GetOkExpected write SetOkExpected;
    property PrevMetier: TMetiers read GetPrevMetier write SetPrevMetier;

    constructor Create(AVuMetre: TcxProgressBar = nil); virtual;
  public
    { --- for testing }
    procedure NoneActivate;
    procedure FumierActivate;
    procedure GaussActivate;
    procedure SpellActivate;
    procedure EliteActivate;
    procedure PauseActivate;
    procedure FumierReload;

    procedure NavExitProc(Sender: TObject);
    procedure ExitEliteProc(Sender: TObject);

  public
    { --- Speak }
    procedure TalkFmt(const Rate, Pitch: Integer; ASt: string); overload;
    procedure TalkFmt(const Rate, Pitch: Integer; ASt: string;
      const Rate1, Pitch1: Integer; ASt1: string); overload;

  public
    { --- fonctionnement courant }
    procedure AppCloseActivate;
    procedure SensibilityActivate;
    procedure OkActivate;
    procedure CancelActivate;
    procedure RetourActivate;
    procedure YesActivate;
    procedure NoActivate;
    procedure EchapActivate;
    procedure MouseLeftActivate;
    procedure MouseRightActivate;
    procedure MouseDoubleActivate;
    procedure MouseMiddleActivate;

    procedure AppVersionShowActivate;

    procedure HighPerfActivate;
    procedure MicroCasqueActivate;
    procedure MicroStudioActivate;
    procedure MicroEnceinteActivate;
    procedure PickerGridShowActivate;
    procedure PickerGridCloseActivate;
    procedure PickerGridMonitorPrevActivate;
    procedure PickerGridMonitorNextActivate;
    procedure PickerGridPointsShowActivate;
    procedure PickerGridPointsHideActivate;
    procedure PickerGridPointPrevActivate;
    procedure PickerGridPointNextActivate;
    procedure PickerGridPointSelectActivate;
    procedure PickerGridPointAddActivate;
    procedure PickerGridPointClearActivate;

    procedure PickerGridWhiteActivate;
    procedure PickerGridGrayActivate;
    procedure PickerGridBlackActivate;
    procedure PickerGridRedActivate;
    procedure PickerGridGreenActivate;
    procedure PickerGridBlueActivate;
    procedure PickerGridYellowActivate;
    procedure PickerGridOrangeActivate;
    procedure PickerGridPinkActivate;

    procedure RaoulIntRightActivate;
    procedure RaoulIntLeftActivate;
    procedure RaoulIntTopActivate;
    procedure RaoulIntBottomActivate;
    procedure RaoulIntCmdShowActivate;
    procedure RaoulIntCmdHideActivate;
    procedure RaoulIntTextShowActivate;
    procedure RaoulIntTextHideActivate;
    procedure RaoulIntVumetreShowActivate;
    procedure RaoulIntVumetreHideActivate;

    procedure RaoulRelaunching;

  published
    property OnAudioChanged: TAudioChanged read FOnAudioChanged write FOnAudioChanged;
    property OnAudioLevelChange: TAudioNotifyEvent read FOnAudioLevelChange write FOnAudioLevelChange;
    property OnMetierChanged: TMetierChanged read FOnMetierChanged write FOnMetierChanged;
    property OnTauxChange: TTauxChange read FOnTauxChange write FOnTauxChange;
    property OnRecognizeAccepted: TSMLNotifyEvent read FOnRecognizeAccepted write FOnRecognizeAccepted;
    property OnRecognizeReject: TNotifyEvent read FOnRecognizeReject write FOnRecognizeReject;
    property OnBuildHypothesis: TRecoNotifyEvent read FOnBuildHypothesis write FOnBuildHypothesis;
    property OnSleepMode: TNotifyEvent read FOnSleepMode write FOnSleepMode;
    property OnAwakeMode: TNotifyEvent read FOnAwakeMode write FOnAwakeMode;
    property OnEndStream: TNotifyEvent read FOnEndStream write FOnEndStream;
    property OnEndSound: TNotifyEvent read FOnSoundEnd write FOnSoundEnd;
    property OnPhraseStart: TNotifyEvent read FOnPhraseStart write FOnPhraseStart;
    property OnStartStream: TNotifyEvent read FOnStartStream write FOnStartStream;
    property OnStartSound: TNotifyEvent read FOnStartSound write FOnStartSound;
    property OnGramReloaded: TRecoNotifyEvent read FOnGramReload write FOnGramReload;
    property OnGramStartReload: TRecoNotifyEvent read FOnGramStartReload write FOnGramStartReload;
    property OnInterference: TIntNotifyEvent read FOnInterference write FOnInterference;
    { --- Metier }
    property OnEliteFocus: TNotifyEvent read FOnEliteFocus write FOnEliteFocus;
  end;

  TCustomNewRecorder = class(TCustomDefRecorder)
  private
    { --- Events }
    procedure Hypothesis(ASender: TObject; StreamNumber: Integer;
      StreamPosition: OleVariant; const Result: ISpeechRecoResult);
    procedure Recognition(ASender: TObject; StreamNumber: Integer;
      StreamPosition: OleVariant; RecognitionType: TOleEnum;
      const Result: ISpeechRecoResult);
    procedure SoundEnd(ASender: TObject;
       StreamNumber: Integer; StreamPosition: OleVariant);
    procedure SoundStart(ASender: TObject;
       StreamNumber: Integer; StreamPosition: OleVariant);
    procedure FalseRecognition(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant; const Result: ISpeechRecoResult);
    procedure Interference(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant; Interference: TOleEnum);
    procedure AudioLevel(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant; AudioLevel: Integer);
    procedure PhraseStart(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant);
    procedure RecognizerStateChange(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant; NewState: SpeechRecognizerState);
    procedure StartStreamEx(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant);
    procedure EndStream(ASender: TObject; StreamNumber: Integer;
       StreamPosition: OleVariant; StreamReleased: WordBool);

  protected
    procedure ComomnInitialization; override;
    procedure DefineEvents; override;
    procedure GrammarLoading; override;

  public
  published
  end;

  TNewRecorder = class(TCustomNewRecorder)
  protected
  public
  published
    property OnAudioChanged;
    property OnAudioLevelChange;
    property OnMetierChanged;
    property OnTauxChange;
    property OnRecognizeAccepted;
    property OnRecognizeReject;
    property OnBuildHypothesis;
    property OnSleepMode;
    property OnAwakeMode;
    property OnEndStream;
    property OnEndSound;
    property OnPhraseStart;
    property OnStartStream;
    property OnStartSound;
    property OnInterference;
  end;

  TSMLCalcul = class
  private
    FSML: string;
    procedure SetSML(const Value: string);
    function  Operation_: string;
    function  Value_: string;
    function  ExtractOperation(const Value: string): string;
    function  ToDot(const Value: string): string;
  public
    class function Operation(const ASML: string): string;
  end;

  TSMLConfiance = class
  private
    FSML: string;
    procedure SetSML(const Value: string);
    function  ExtractConfidence(const Value: string): Double;
    function  ExtractTag(const Value: string):Cardinal;
    function  ExtractTagStr(const Value: string):string;
    function  ExtractSender(const Value: string):TGramType;
    function  ExtractText(const Value: string):string;
  public
    class function Confidence(const ASML: string): Double;
    class function Tag(const ASML: string): Cardinal;
    class function TagStr(const ASML: string): string;
    class function Sender(const ASML: string): TGramType;
    class function GaussBuffer(const ASML: string): string;
    class function Text(const ASML: string): string;
  end;

  TGramFactories = class
  private
    procedure SwitchProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure FumierProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure GaussProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure SpellProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure EliteProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure PauseProcess_(const SML: string; const ARecorder: TNewRecorder);
  public
    class procedure SwitchProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure FumierProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure GaussProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure SpellProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure EliteProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure PauseProcess(const SML: string; const ARecorder: TNewRecorder);
  end;

  //TODO: ajouter un mutex
  TStrFifoStack = class(TStringList)
  private
    FLock : Boolean;
    procedure WaitFor;
  public
    function Poke(const ASt: string):Integer;
    function Peek:string;

    constructor Create;
  end;

  TLocalStack = class
  public
    class function AstAdd(const ASt: string; const grType: TGramType):Integer;
    class function MacrosAdd(const ASt: string):Integer;
    class function FumierAdd(const ASt: string):Integer;
    class function GaussAdd(const ASt: string):Integer;
    class function SpellAdd(const ASt: string):Integer;
    class function EliteAdd(const ASt: string):Integer;
    class function PauseAdd(const ASt: string):Integer;
  end;

  TStrFifoThread = class(TThread)
  private
    ThPile         : TStrFifoStack;
    ThRecorder     : TNewRecorder;
    ThGramType     : TGramType;
    ThEliteManager : TEliteManager;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const APile: TStrFifoStack; const ARecorder: TNewRecorder;
      AGramType: TGramType);
  end;

  TThreadManager = class
  private
    procedure MacrosInitialize;
    procedure FumierInitialize;
    procedure GaussInitialize;
    procedure SpellInitialize;
    procedure EliteInitialize;
    procedure PauseInitialize;

    procedure MacrosFinalize;
    procedure FumierFinalize;
    procedure GaussFinalize;
    procedure SpellFinalize;
    procedure EliteFinalize;
    procedure PauseFinalize;
  public
    class procedure Initialize;
    class procedure Finalize;
  end;

  TSpeaker = class
  private
    procedure Talk_(const ASt: string);
    procedure TalkNP_(const ASt: string); //non prot�g�
  public
    class procedure Talk(const ASt: string);
    class procedure TalkNP(const ASt: string);
  end;

  TRecorderParams = class
  private
    procedure SetCeilFumier(const Value: Double);
    function  GetCeilFumier: Double;
    function  GetCeilGauss: Double;
    procedure SetCeilGauss(const Value: Double);
    function  GetCeilSwitch: Double;
    procedure SetCeilSwitch(const Value: Double);
    function  GetSensibility: Integer;
    procedure SetSensibility(const Value: Integer);
  public
    property CeilFumier:Double read GetCeilFumier write SetCeilFumier;
    property CeilGauss:Double read GetCeilGauss write SetCeilGauss;
    property CeilSwitch:Double read GetCeilSwitch write SetCeilSwitch;
    property Sensibility:Integer read GetSensibility write SetSensibility;

    class procedure Initialize;
    class procedure Finalize;
  end;

  TInterferenceDisp = class
  private
    FNoises: TArrayOfNoises;
    procedure SetNoises(const Value: TArrayOfNoises);
    function  NoisesToText(ANoises: TArrayOfNoises):string;
    procedure DispGauges;
  public
    class function Text(const Value: TArrayOfNoises):string;
    class procedure GaugesUpdate(const Value: TArrayOfNoises);
  end;

  TSensibilitySetting = class
  private
    procedure Adjust_(const Value: TSensibility);
    function  Position_:TSensibility;
    function  Sensibility_:Integer;
    procedure Floors_(var High, Middle: Double);
  public
    class procedure Adjust(const Value: TSensibility);
    class procedure AdjustInt(const Value: Integer);
    class function  Position:TSensibility;
    class function  Sensibility: Integer;
    class procedure Floors(var High, Middle: Double);
  end;

  TFunTalk = class
  private
    function  GetAleaMeteo: Integer;
    procedure SetAleaMeteo(const Value: Integer);
    function  GetAleaNightMare: Integer;
    procedure SetAleaNightMare(const Value: Integer);
    function  GetMeteaoAsked: Boolean;
    procedure SetMeteaoAsked(const Value: Boolean);
  private
    procedure NightMare_;
    procedure ThankYou_;
    procedure Meteo_;
    procedure Meteo_we_;
    procedure OkGoogle_;
    procedure ThankMission_;

    property AleaMeteo: Integer read GetAleaMeteo write SetAleaMeteo;
    property AleaNightMare: Integer read GetAleaNightMare write SetAleaNightMare;
    property MeteaoAsked: Boolean read GetMeteaoAsked write SetMeteaoAsked;
  public
    class procedure Initialize;
    class procedure NightMare;
    class procedure ThankYou;
    class procedure Meteo;
    class procedure Meteo_we;
    class procedure OkGoogle;
    class procedure ThankMission;
  end;

procedure Register;

var
  Recorder      : TNewRecorder;
  Params        : TRecorderParams;
  LocalTrackBar : TcxTrackBar;
  LocalBDF      : TcxProgressBar;
  LocalBV       : TcxProgressBar;
  LocalNF       : TcxProgressBar;
  LocalVTL      : TcxProgressBar;
  LocalVTR      : TcxProgressBar;

var
  {App managment}
  AppCloseFunc               : TNotifyEvent = nil;
  SensibilityFunc            : TNotifyEvent = nil;
  OkFunc                     : TNotifyEvent = nil;
  CancelFunc                 : TNotifyEvent = nil;
  YesFunc                    : TNotifyEvent = nil;
  NoFunc                     : TNotifyEvent = nil;
  RetourFunc                 : TNotifyEvent = nil;
  EchapFunc                  : TNotifyEvent = nil;
  {Mouse managment indep du module de navigation}
  MouseLeftFunc              : TNotifyEvent = nil;
  MouseRightFunc             : TNotifyEvent = nil;
  MouseDoubleFunc            : TNotifyEvent = nil;
  MouseMiddleFunc            : TNotifyEvent = nil;
  {Versionning}
  AppVersionShowFunc         : TNotifyEvent = nil;
  {Sensibility managment}
  HautesperformancesFunc     : TNotifyEvent = nil;
  MicroCasqueFunc            : TNotifyEvent = nil;
  MicroStudioFunc            : TNotifyEvent = nil;
  MicroEnceinteFunc          : TNotifyEvent = nil;
  {Picker managment}
  PickerGridShowFunc         : TNotifyEvent = nil;
  PickerGridCloseFunc        : TNotifyEvent = nil;
  PickerGridMonitorPrevFunc  : TNotifyEvent = nil;
  PickerGridMonitorNextFunc  : TNotifyEvent = nil;
  PickerGridPointsShowFunc   : TNotifyEvent = nil;
  PickerGridPointsHideFunc   : TNotifyEvent = nil;
  PickerGridPointPrevFunc    : TNotifyEvent = nil;
  PickerGridPointNextFunc    : TNotifyEvent = nil;
  PickerGridPointSelectFunc  : TNotifyEvent = nil;
  PickerGridPointAddFunc     : TNotifyEvent = nil;
  PickerGridPointClearFunc   : TNotifyEvent = nil;
  {Picker Zoom}
  PickerGridWhiteFunc        : TNotifyEvent = nil;
  PickerGridGrayFunc         : TNotifyEvent = nil;
  PickerGridBlackFunc        : TNotifyEvent = nil;
  PickerGridRedFunc          : TNotifyEvent = nil;
  PickerGridGreenFunc        : TNotifyEvent = nil;
  PickerGridBlueFunc         : TNotifyEvent = nil;
  PickerGridYellowFunc       : TNotifyEvent = nil;
  PickerGridOrangeFunc       : TNotifyEvent = nil;
  PickerGridPinkFunc         : TNotifyEvent = nil;
  {Soft view managment}
  RaoulIntRightFunc          : TNotifyEvent = nil;
  RaoulIntLeftFunc           : TNotifyEvent = nil;
  RaoulIntTopFunc            : TNotifyEvent = nil;
  RaoulIntBottomFunc         : TNotifyEvent = nil;
  RaoulIntCmdShowFunc        : TNotifyEvent = nil;
  RaoulIntCmdHideFunc        : TNotifyEvent = nil;
  RaoulIntTextShowFunc       : TNotifyEvent = nil;
  RaoulIntTextHideFunc       : TNotifyEvent = nil;
  RaoulIntVumetreShowFunc    : TNotifyEvent = nil;
  RaoulIntVumetreHideFunc    : TNotifyEvent = nil;

var
  ArrayOfJobFiles : TArrayOfJobFiles = ('Switch', 'gr_main', 'Numeration', 'Spelling', 'Elite', 'Pause');    //A-G

// IMPORTANT Catalog ---> Raoul.trf=

//  GrammarPath     : string = 'H:\2020 Developpement\Vocalis\bin\Grammar';     //ver dev
  GrammarPath     : string = 'Grammar';                                     //ver prod

  GrammarStrs     : array[0..Integer(High(TGramType))] of string =
   ('switch', 'fumier', 'gauss', 'spelling', 'elite', 'pause');     //A-G
var
  {Scruptor Managers}
  PileMacros      : TStrFifoStack;
  ThMacros        : TStrFifoThread;

  PileFumier      : TStrFifoStack;
  ThFumier        : TStrFifoThread;

  PileGauss       : TStrFifoStack;
  ThGauss         : TStrFifoThread;

  PileSpell       : TStrFifoStack;
  ThSpell         : TStrFifoThread;

  PileElite       : TStrFifoStack;
  ThElite         : TStrFifoThread;

  PilePause       : TStrFifoStack;
  ThPause         : TStrFifoThread;

implementation

uses
  uRegistry, uRaoulUpdater, uRaoulDisplay;

var
  InterferenceStr : array[TInterferenceKind] of string =
   ( '',
     'Bruit de fond',
     'Micro inactif',
     'Bruit violent',
     'Niveau sonore faible',
     'D�bit vocal trop lent',
     'D�bit vocal trop rapide'
   );

{ TCustomDefRecorder }

procedure TCustomDefRecorder.CreateContext;
begin
  FoldersInitialize;
  FMicroDescription := 'aucun micro actif';
  HKMLCategory :=
    'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Speech\AudioInput\TokenEnums\MMAudioIn\';
  AGNES_DEFAULT_VOICE :=
    'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Speech\Voices\Tokens\ScanSoftVirginie_Dri40_16kHz';

  { - Instanciations de classe SP - }
  Context    := TSpInProcRecoContext.Create(nil);
  FCategory  := TSpObjectTokenCategory.Create(nil);
  FToken     := TSpObjectToken.Create(nil);

  { - Initialisation du contexte de reconnaissance vocale - }
  Recognizer := Context.Recognizer;

  { - Le context pour la reconnaissance est inactif par d�faut - }
  Recognizer.State := SRSInactive;

  { - Permettre au contexte de reconna�tre et d'interpr�ter les entr�es audios - }
  FCategory.SetId(HKMLCategory, True);
  FToken.SetId(HKMLCategory, HKMLCategory, True);

  { - Connexion des �v�nements - }
  DefineEvents;

  { - Ajout des grammaires - }
  GrammarLoading;

  { S�lectionner le format ie la qualit� de l'enregistrement audio
    Influe �galement sur la qualit� de l'�coute � priori --> test avec enceinte  }
  Context.RetainedAudioFormat.type_ := SAFTCCITT_uLaw_44kHzStereo; //SAFTGSM610_44kHzMono; //SAFT8kHz8BitMono;

  {Pour l'utilisation de l'event AudioLevel car non d�fini par d�faut}
  Context.EventInterests := SREAllEvents;

  ComomnInitialization;
end;

procedure TCustomDefRecorder.FoldersInitialize;
begin
  try MkDir('Grammar') except end;
end;


function TCustomDefRecorder.GetState: TSRState;
begin
  with Recognizer do Result := TSRState( State )
end;

procedure TCustomDefRecorder.SetState(const Value: TSRState);
begin
  with Recognizer do State := Integer( Value );
end;

function TCustomDefRecorder.GrammarCreate(AFileName: string;
  var Index: Integer): ISpeechRecoGrammar;
begin
  Result := Context.CreateGrammar( Index );
  FGrammars[Index] := Result;
  with Result do CmdLoadFromFile(AFileName, SLODynamic);
  Inc( Index );
end;

procedure TCustomDefRecorder.Initialize;
begin
  FLock := True;
  try
    CreateContext;
  finally
    FLock := False;
  end
end;


function TCustomDefRecorder.GetListen: Boolean;
begin
  with Recognizer do
  try
    Result := AudioInput.GetDescription(0) <> EmptyStr
  except
    Result := False
  end
end;

procedure TCustomDefRecorder.SetListen(const Value: Boolean);
begin
  with Recognizer do if Listen <> Value then begin
    case Value of
      True : begin
               AudioInput := FToken.DefaultInterface;
               FMicroDescription := AudioInput.GetDescription(0);
             end;
      else   AudioInput := nil;
    end;
    if Assigned(FOnAudioChanged) then FOnAudioChanged(Self, Value)
  end
end;

procedure TCustomDefRecorder.SetMode(const Value: TRecordModes);
begin
  if FMode <> Value then begin
    FMode := Value;
    case Value of
      rm_sleep : SetSleepMode else SetAwakeMode
    end;
  end;
end;

procedure TCustomDefRecorder.SetAwakeMode;
begin
//  if Assigned(FVuMetre) then with FVuMetre do Visible := True;
  if Assigned(FOnAwakeMode) then FOnAwakeMode(Self)
end;

procedure TCustomDefRecorder.SetSleepMode;
begin
  GrammarsDisable;
  if Assigned(FVuMetre) then with FVuMetre do Visible := False;
  if Assigned(FOnSleepMode) then FOnSleepMode(Self);
end;

procedure TCustomDefRecorder.GrammarsDisable;
var
  i : Integer;
begin
  CurrentMetier := m_none;
  FGrammars[0].CmdSetRuleIdState(0, SGDSactive);
  for i := 1 to FGramCount do
  try
    FGrammars[i].CmdSetRuleIdState(0, SGDSInactive);
  except
  end
end;

procedure TCustomDefRecorder.GrammarActivate(grType: TGramType;
  grState: Boolean);
begin
  case grState of
    True : FGrammars[Integer(grType)].CmdSetRuleIdState(0, SGDSActive);
    else   FGrammars[Integer(grType)].CmdSetRuleIdState(0, SGDSInactive);
  end
end;

procedure TCustomDefRecorder.GrammarReload(grType: TGramType);
begin
  FGrammars[Integer(grType)].CmdLoadFromFile(GrammarFileName(grType), SLODynamic);
end;

function TCustomDefRecorder.GrammarFileName(grType: TGramType): string;
begin
  Result := Format('%s\%s.cfg', [ GrammarPath, ArrayOfJobFiles[grType] ])
end;

procedure TCustomDefRecorder.SetCurrentMetier(const Value: TMetiers);
begin
  if Value <> FCurrentMetier then begin
    FCurrentMetier := Value;
    if Assigned(FOnMetierChanged) then FOnMetierChanged(Self, Value)
  end
end;

procedure TCustomDefRecorder.Disable;
begin
  GrammarsDisable;
  with Context do EventInterests := SINone
end;

procedure TCustomDefRecorder.Enable;
begin
  with Context do EventInterests := SREAllEvents;
end;

procedure TCustomDefRecorder.SetTaux(const Value: Double);
begin
  FTaux := Value;
  if Assigned(FOnTauxChange) then FOnTauxChange(Self, FTaux)
end;

constructor TCustomDefRecorder.Create(AVuMetre: TcxProgressBar);
begin
  inherited Create;
  FVuMetre            := AVuMetre;
  ModeFonctionnement  := mf_none;
  OkExpected          := False;
  KeyWrite(ParamKey, 'ModFunctionment', Integer(mf_none));
  ProcExitNav         := NavExitProc;
  ProcExitElite       := ExitEliteProc;
end;

procedure TCustomDefRecorder.VuMetreReset;
begin
  if Assigned(FVuMetre) then with FVuMetre do begin
    Position := 0;
    with Properties do PeakValue := 0;
  end
end;

procedure TCustomDefRecorder.VuMetreSetter(const Apos: Integer);
begin
  if Assigned(FVuMetre) then with FVuMetre do Position := APos;
end;

procedure TCustomDefRecorder.FumierActivate;
begin //A-G
  try
    GrammarActivate(gt_gauss, False);
    GrammarActivate(gt_spell, False);
    GrammarActivate(gt_elite, False);
    GrammarActivate(gt_pause, False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_fumier);

    CurrentMetier := m_fumier;
  except
  end
end;

procedure TCustomDefRecorder.GaussActivate;
begin //A-G
  try
    GrammarActivate(gt_fumier, False);
    GrammarActivate(gt_spell,  False);
    GrammarActivate(gt_elite,  False);
    GrammarActivate(gt_pause,  False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_gauss);

    CurrentMetier := m_gauss;
  except
  end
end;

procedure TCustomDefRecorder.SpellActivate;
begin //A-G
  TSCRepeater.Initialize;
  try
    GrammarActivate(gt_fumier, False);
    GrammarActivate(gt_gauss,  False);
    GrammarActivate(gt_elite,  False);
    GrammarActivate(gt_pause,  False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_spell);

    CurrentMetier := m_spell;
  except
  end
end;

procedure TCustomDefRecorder.EliteActivate;
begin //A-G
  try
    GrammarActivate(gt_fumier, False);
    GrammarActivate(gt_gauss,  False);
    GrammarActivate(gt_spell,  False);
    GrammarActivate(gt_pause,  False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_elite);

    CurrentMetier := m_elite;
    if Assigned(FOnEliteFocus) then FOnEliteFocus(Self)
  except
  end
end;

procedure TCustomDefRecorder.NoneActivate;
begin //A-G
  try
    GrammarActivate(gt_fumier, False);
    GrammarActivate(gt_gauss,  False);
    GrammarActivate(gt_spell,  False);
    GrammarActivate(gt_elite,  False);
    GrammarActivate(gt_pause,  False);
    GrammarActivate(gt_switch);

    CurrentMetier := m_none;
  except
  end
end;



procedure TCustomDefRecorder.FumierReload;
begin
  State  := srs_inactive;
  Listen := False;
  Disable;
  VuMetreReset;
  if Assigned(FOnGramStartReload) then FOnGramStartReload(Self, 'Wait while grammar reloading');
  Application.ProcessMessages;
  Sleep(250);
  try
    GrammarActivate(gt_fumier, False);
    GrammarReload(gt_fumier);
    if Assigned(FOnGramReload) then FOnGramReload(Self, 'Fumier(Grammar) reloaded')
  finally
    FumierActivate;
    Enable;
    Listen := True;
    State  := srs_active;
  end;
end;

procedure TCustomDefRecorder.NoisesReset;
var
  n : Integer;
begin
  n := Integer( High(TInterferenceKind) ) + 1;
  FillChar(FNoises, n*SizeOf(Integer), 0);
end;

procedure TCustomDefRecorder.NoisesInc(const Item: TInterferenceKind);
begin
  FNoises[Item] := FNoises[Item] + 1;
end;

procedure TCustomDefRecorder.SensibilityActivate;
begin
  if (ModeFonctionnement in [mf_none, mf_sensibility]) and Assigned(SensibilityFunc) then
    SensibilityFunc(nil)
end;

procedure TCustomDefRecorder.OkActivate;
begin
  if Assigned(OkFunc) then OkFunc(nil) else ModeFonctionnement := mf_none
end;

procedure TCustomDefRecorder.SetModeFonctionnement(
  const Value: TModeFonctionnement);
begin
  KeyWrite(ParamKey, 'ModFunctionment', Integer(Value))
end;

function TCustomDefRecorder.GetModeFonctionnement: TModeFonctionnement;
begin
  Result := TModeFonctionnement( KeyReadInt(ParamKey, 'ModFunctionment', 0) );
end;

procedure TCustomDefRecorder.HighPerfActivate;
begin
  if Assigned(HautesperformancesFunc) and (ModeFonctionnement = mf_sensibility)
    then HautesperformancesFunc(nil)
end;

procedure TCustomDefRecorder.MicroCasqueActivate;
begin
  if Assigned(MicroCasqueFunc) and (ModeFonctionnement = mf_sensibility)
    then MicroCasqueFunc(nil)
end;

procedure TCustomDefRecorder.MicroStudioActivate;
begin
  if Assigned(MicroStudioFunc) and (ModeFonctionnement = mf_sensibility)
    then MicroStudioFunc(nil)
end;

procedure TCustomDefRecorder.MicroEnceinteActivate;
begin
  if Assigned(MicroEnceinteFunc) and (ModeFonctionnement = mf_sensibility)
    then MicroEnceinteFunc(nil)
end;

function TCustomDefRecorder.GetOkExpected: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'OkExpected')
end;

procedure TCustomDefRecorder.SetOkExpected(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'OkExpected', Value)
end;

procedure TCustomDefRecorder.CancelActivate;
begin
  if Assigned(CancelFunc) then CancelFunc(nil) else ModeFonctionnement := mf_none
end;

procedure TCustomDefRecorder.YesActivate;
begin
  if Assigned(YesFunc) then YesFunc(nil) else ModeFonctionnement := mf_none
end;

procedure TCustomDefRecorder.NoActivate;
begin
  if Assigned(NoFunc) then NoFunc(nil) else ModeFonctionnement := mf_none
end;

procedure TCustomDefRecorder.AppCloseActivate;
begin
  if (ModeFonctionnement in [mf_none, mf_closeapp]) and Assigned(AppCloseFunc) then AppCloseFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridShowActivate;
begin
  if Assigned(PickerGridShowFunc) then PickerGridShowFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridCloseActivate;
begin
  if Assigned(PickerGridCloseFunc) then PickerGridCloseFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridMonitorPrevActivate;
begin
  if Assigned(PickerGridMonitorPrevFunc) then PickerGridMonitorPrevFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridMonitorNextActivate;
begin
  if Assigned(PickerGridMonitorNextFunc) then PickerGridMonitorNextFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointsShowActivate;
begin
  if Assigned(PickerGridPointsShowFunc) then PickerGridPointsShowFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointsHideActivate;
begin
  if Assigned(PickerGridPointsHideFunc) then PickerGridPointsHideFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointPrevActivate;
begin
  if Assigned(PickerGridPointPrevFunc) then PickerGridPointPrevFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointNextActivate;
begin
  if Assigned(PickerGridPointNextFunc) then PickerGridPointNextFunc(nil)
end;

procedure TCustomDefRecorder.RetourActivate;
begin
  if Assigned(RetourFunc) then RetourFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointSelectActivate;
begin
  if Assigned(PickerGridPointSelectFunc) then PickerGridPointSelectFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointAddActivate;
begin
  if Assigned(PickerGridPointAddFunc) then PickerGridPointAddFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridYellowActivate;
begin
  if Assigned(PickerGridYellowFunc) then PickerGridYellowFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridWhiteActivate;
begin
  if Assigned(PickerGridWhiteFunc) then PickerGridWhiteFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridRedActivate;
begin
  if Assigned(PickerGridRedFunc) then PickerGridRedFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridBlueActivate;
begin
  if Assigned(PickerGridBlueFunc) then PickerGridBlueFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPinkActivate;
begin
  if Assigned(PickerGridPinkFunc) then PickerGridPinkFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridBlackActivate;
begin
  if Assigned(PickerGridBlackFunc) then PickerGridBlackFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridGrayActivate;
begin
  if Assigned(PickerGridGrayFunc) then PickerGridGrayFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridOrangeActivate;
begin
  if Assigned(PickerGridOrangeFunc) then PickerGridOrangeFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridGreenActivate;
begin
  if Assigned(PickerGridGreenFunc) then PickerGridGreenFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntRightActivate;
begin
  if Assigned(RaoulIntRightFunc) then RaoulIntRightFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntLeftActivate;
begin
  if Assigned(RaoulIntLeftFunc) then RaoulIntLeftFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntTopActivate;
begin
  if Assigned(RaoulIntTopFunc) then RaoulIntTopFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntBottomActivate;
begin
  if Assigned(RaoulIntBottomFunc) then RaoulIntBottomFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntCmdHideActivate;
begin
  if Assigned(RaoulIntCmdHideFunc) then RaoulIntCmdHideFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntCmdShowActivate;
begin
  if Assigned(RaoulIntCmdShowFunc) then RaoulIntCmdShowFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntTextShowActivate;
begin
  if Assigned(RaoulIntTextShowFunc) then RaoulIntTextShowFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntTextHideActivate;
begin
  if Assigned(RaoulIntTextHideFunc) then RaoulIntTextHideFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntVumetreShowActivate;
begin
  if Assigned(RaoulIntVumetreShowFunc) then RaoulIntVumetreShowFunc(nil)
end;

procedure TCustomDefRecorder.RaoulIntVumetreHideActivate;
begin
  if Assigned(RaoulIntVumetreHideFunc) then RaoulIntVumetreHideFunc(nil)
end;

procedure TCustomDefRecorder.MouseLeftActivate;
begin
  if Assigned(MouseLeftFunc) then MouseLeftFunc(nil)
end;

procedure TCustomDefRecorder.MouseRightActivate;
begin
  if Assigned(MouseRightFunc) then MouseRightFunc(nil)
end;

procedure TCustomDefRecorder.EchapActivate;
begin
  if Assigned(EchapFunc) then EchapFunc(nil)
end;

procedure TCustomDefRecorder.PickerGridPointClearActivate;
begin
  if Assigned(PickerGridPointClearFunc) then PickerGridPointClearFunc(nil)
end;

procedure TCustomDefRecorder.AppVersionShowActivate;
begin
  if Assigned(AppVersionShowFunc) then AppVersionShowFunc(nil)
end;

procedure TCustomDefRecorder.MouseDoubleActivate;
begin
  if Assigned(MouseDoubleFunc) then MouseDoubleFunc(nil)
end;

procedure TCustomDefRecorder.MouseMiddleActivate;
begin
  if Assigned(MouseMiddleFunc) then MouseMiddleFunc(nil) 
end;

procedure TCustomDefRecorder.RaoulRelaunching;
begin
  if KeyReadInt(RaoulKey, 'DownloadState') = 1 then begin
    with TalkativeFacade do Text := 'Mise � jour en attente, red�marrage automatique impossible';
    Exit;
  end;
  TAppUpdater.TryToRelauch;
end;

procedure TCustomDefRecorder.PauseActivate;
begin
  try
    PrevMetier := CurrentMetier;
    GrammarActivate(gt_fumier, False);
    GrammarActivate(gt_gauss,  False);
    GrammarActivate(gt_spell,  False);
    GrammarActivate(gt_switch, False);
    GrammarActivate(gt_elite,  False);
    GrammarActivate(gt_pause);

    CurrentMetier := m_pause;
  except
  end
end;


function TCustomDefRecorder.GetPrevMetier: TMetiers;
begin
  Result := TMetiers(KeyReadInt(ParamKey, 'PrevMetier'))
end;

procedure TCustomDefRecorder.SetPrevMetier(const Value: TMetiers);
begin
  KeyWrite(ParamKey, 'PrevMetier', Integer(Value))
end;

const
  TTS_PATERN =
   '<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" xml:lang="fr-FR">'#10#13 +
     '<voice name="fr-FR-DeniseNeural">'#10#13 +
       '<prosody rate="%s" pitch="%s">'#10#13 +
         '%s' + #10#13 +
       '</prosody>'#10#13 +
     '</voice>'#10#13 +
   '</speak>';

  TTS_PATERN2 =
   '<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" xml:lang="fr-FR">'#10#13 +
     '<voice name="fr-FR-DeniseNeural">'#10#13 +
       '<prosody rate="%s" pitch="%s">'#10#13 +
         '%s' + #10#13 +
       '</prosody>'#10#13 +
       '<prosody rate="%s" pitch="%s">'#10#13 +
         '%s' + #10#13 +
       '</prosody>'#10#13 +
     '</voice>'#10#13 +
   '</speak>';

function DblFmtTTS(const Value: Integer):string;
begin
  if Value > 0 then Result := Format('+%d%s', [Value,  '.00%'])
    else Result := Format('-%d%s', [Abs(Value),  '.00%'])
end;

procedure TCustomDefRecorder.TalkFmt(const Rate, Pitch: Integer; ASt: string);
begin
  TSpeaker.TalkNP( Format(TTS_PATERN, [DblFmtTTS(Rate), DblFmtTTS(Pitch), ASt]) )
end;

procedure TCustomDefRecorder.TalkFmt(const Rate, Pitch: Integer; ASt: string;
  const Rate1, Pitch1: Integer; ASt1: string);
begin
  TSpeaker.TalkNP( Format(TTS_PATERN2,
    [ DblFmtTTS(Rate),  DblFmtTTS(Pitch),  ASt,
      DblFmtTTS(Rate1), DblFmtTTS(Pitch1), ASt1]) )
end;

procedure TCustomDefRecorder.NavExitProc(Sender: TObject);
begin
  if (Mode = rm_listen) and (CurrentMetier = m_spell) then GrammarsDisable
end;

procedure TCustomDefRecorder.ExitEliteProc(Sender: TObject);
begin
  if Mode = rm_listen then NoneActivate
end;

{ TCustomNewRecorder }

procedure TCustomNewRecorder.Recognition(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant; RecognitionType: TOleEnum;
  const Result: ISpeechRecoResult);
var
  SML     : string;
  GSender : TGramType;
  Speech  : ISpeechRecoResult;

  function DisplayStr:string; begin
    case GSender of //A-G
      gt_switch,
      gt_fumier,
      gt_spell,
      gt_pause,
      gt_elite  : Result := QuoteFix(Speech.PhraseInfo.GetText(0,-1, True));
      gt_gauss  : Result := TSMLCalcul.Operation(SML);
    end;
  end;

  procedure Accept; begin
    TLocalStack.AstAdd(SML, GSender);
    if Mode = rm_listen then if Assigned(FOnRecognizeAccepted) then
      FOnRecognizeAccepted(Self, DisplayStr, SML);
  end;

  procedure Reject; begin
    if Assigned(FOnRecognizeReject) then FOnRecognizeReject(Self);
  end;

  procedure Initialize; begin
    Speech  := Result;
    SML     := (Result as ISpeechXMLRecoResult).GetXMLResult( SPXRO_SML );
    Taux    := TSMLConfiance.Confidence(SML);
    GSender := TSMLConfiance.Sender(SML);
  end;

  function CanProcess:Boolean; begin
    Result := False;
    Initialize;
    if GSender = gt_switch then if TSMLConfiance.Tag(SML) = 0 then begin
      Result := False;
      Exit;
    end;
    case GSender of  //A-G
      gt_switch,
      gt_spell,
      gt_pause,
      gt_elite  : Result := FTaux > Params.CeilSwitch;
      gt_fumier : Result := FTaux > Params.CeilFumier;
      gt_gauss  : Result := FTaux > Params.CeilGauss;
    end
  end;

  procedure Finalize; begin
    {r�initialise le suivi des interf�rences}
    FInterference := ik_SINone;
    NoisesReset;
  end;

begin
  if CanProcess then Accept else Reject;
  Finalize;
end; {Recognition}

procedure TCustomNewRecorder.Hypothesis(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant; const Result: ISpeechRecoResult);
begin
  if Mode = rm_listen then if Assigned(FOnBuildHypothesis) then
    FOnBuildHypothesis(Self, QuoteFix(Result.PhraseInfo.GetText(0,-1, True)) )
end;

procedure TCustomNewRecorder.ComomnInitialization;
begin
  if KeyReadBoolean(AppKey, 'GrammarUpdated')
    then TalkFmt(18,0, 'Maintnant c''est � jour ! Raoul va s''relancer')
    else TalkFmt(10,0, 'je suis pr�t');

  Listen         := True;
  State          := srs_active;
  Mode           := rm_sleep;
  CurrentMetier  := m_none;
  FInterference  := ik_SINone;
end;

procedure TCustomNewRecorder.DefineEvents;
begin
  with Context do begin
    OnHypothesis            := Hypothesis;
    OnRecognition           := Recognition;
    OnSoundEnd              := SoundEnd;
    OnSoundStart            := SoundStart;
    OnFalseRecognition      := FalseRecognition;
    OnInterference          := Interference;
    OnAudioLevel            := AudioLevel;
    OnPhraseStart           := PhraseStart;
    OnRecognizerStateChange := RecognizerStateChange;
    OnStartStream           := StartStreamEx;
    OnEndStream             := EndStream;
  end
end;

procedure TCustomNewRecorder.GrammarLoading;
var
  i : Integer;
begin
  FGramCount := 0;
  for i := Integer(Low(TGramType)) to Integer(High(TGramType)) do begin
    GrammarCreate( GrammarFileName( TGramType(i) ), FGramCount);
    Application.ProcessMessages;
  end;
  {Activer la grammaire index 0 qui doit le rester pendant le run-time}
  GrammarActivate(gt_switch);
end;

procedure TCustomNewRecorder.EndStream(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant; StreamReleased: WordBool);
begin
  if Assigned(FOnEndStream) then FOnEndStream(Self)
end;

procedure TCustomNewRecorder.FalseRecognition(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant;
  const Result: ISpeechRecoResult);
begin
  {Depend du type de grammaire je crois... � v�rifier}
end;

procedure TCustomNewRecorder.SoundEnd(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant);
begin
  if Assigned(FOnSoundEnd) then FOnSoundEnd(Self);
  VuMetreReset;
  if Assigned(FOnAudioLevelChange) then FOnAudioLevelChange(Self, 0);
end;

procedure TCustomNewRecorder.RecognizerStateChange(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant;
  NewState: SpeechRecognizerState);
begin

end;

procedure TCustomNewRecorder.PhraseStart(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant);
begin
  TInterferenceDisp.GaugesUpdate(FNoises);
  if Assigned(FOnPhraseStart) then FOnPhraseStart(Self)
end;

procedure TCustomNewRecorder.StartStreamEx(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant);
begin
  if Assigned(FOnStartStream) then FOnStartStream(Self)
end;

procedure TCustomNewRecorder.AudioLevel(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant; AudioLevel: Integer);
begin
  VuMetreSetter(AudioLevel);
  if Assigned(FOnAudioLevelChange) then OnAudioLevelChange(Self, AudioLevel)
end;

procedure TCustomNewRecorder.SoundStart(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant);
begin
  if Assigned(FOnStartSound) then FOnStartSound(Self)
end;

procedure TCustomNewRecorder.Interference(ASender: TObject;
  StreamNumber: Integer; StreamPosition: OleVariant; Interference: TOleEnum);
begin
  FInterference := TInterferenceKind( Interference );
  NoisesInc( FInterference );
  if Assigned(FOnInterference) then FOnInterference(Self, FInterference, Noises)
end;

{ TSMLConfiance }

class function TSMLConfiance.Confidence(const ASML: string): Double;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractConfidence(ASML);
  finally
    Free
  end
end;

function TSMLConfiance.ExtractConfidence(const Value: string): Double;
var
  Buffer : string;
begin
  SetSML(Value);
  Buffer := GetAfterStr(FSML,   'confidence="');
  Buffer := GetBeforStr(Buffer, '">' );
  DecimalSeparator := '.';
  try Result := StrToFloat(Buffer) except Result := 0 end
end;

function TSMLConfiance.ExtractSender(const Value: string): TGramType;
var
  Buffer: string;
begin
  SetSML(Value);
  Buffer := GetBeforStr(FSML,     '</Sender>');
  Buffer := GetAfterStr(Buffer,   '<Sender');
  Buffer := GetAfterStr(Buffer,   '>');
  case IndexStr(AnsiLowerCase(Buffer), GrammarStrs) of   //A-G
    0 : Result := gt_switch;
    1 : Result := gt_fumier;
    2 : Result := gt_gauss;
    3 : Result := gt_spell;
    4 : Result := gt_elite;
    5 : Result := gt_pause;
    else raise Exception.Create('nom de grammaire inconnue');
  end
end;

function TSMLConfiance.ExtractTag(const Value: string): Cardinal;
var
  Buffer : string;
begin
  Buffer := ExtractTagStr(Value);
  while Pos('undefined', Buffer) > 0 do Buffer := DelSubStr(Buffer, 'undefined');
  try Result := StrToInt(Buffer) except Result := 0 end
end;

function TSMLConfiance.ExtractTagStr(const Value: string): string;
begin
  SetSML(Value);
  Result := GetBeforStr(FSML,     '</Tag>');
  Result := GetAfterStr(Result,   '<Tag');
  Result := GetAfterStr(Result,   '>');
end;

function TSMLConfiance.ExtractText(const Value: string): string;
begin
  SetSML(Value);
  Result := GetAfterStr(FSML,   '<SML text="');
  Result := GetBeforStr(Result, '"');
end;

class function TSMLConfiance.GaussBuffer(const ASML: string): string;
begin
  Result := TSMLCalcul.Operation(ASML)
end;

class function TSMLConfiance.Sender(const ASML: string): TGramType;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractSender(ASML)
  finally
    Free
  end
end;

procedure TSMLConfiance.SetSML(const Value: string);
begin
  FSML := Value
end;

class function TSMLConfiance.Tag(const ASML: string): Cardinal;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractTag(ASML);
  finally
    Free
  end
end;

class function TSMLConfiance.TagStr(const ASML: string): string;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractTagStr(ASML);
  finally
    Free
  end
end;

class function TSMLConfiance.Text(const ASML: string): string;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractText(ASML);
  finally
    Free
  end
end;

{ TSMLCalcul }

function TSMLCalcul.ExtractOperation(const Value: string): string;
var
  x,y : string;
begin
  SetSML(Value);
  x := Operation_;
  y := ToDot(Value_);
  if x = EmptyStr then Result := y
    else
  if x = y then Result := x else begin
    if Pos('=', x) > 0 then x := Format('{%s}', [x]);
    Result := Format('%s = %s', [x, y])
  end
end;

function TSMLCalcul.Operation_: string;
begin
  Result := GetBeforStr(FSML, '</Operation>');
  Result := GetAfterStr(Result, '<Operation');
  Result := GetAfterStr(Result, '>');
end;

class function TSMLCalcul.Operation(const ASML: string): string;
begin
  with TSMLCalcul.Create do
  try
    Result := ExtractOperation(ASML)
  finally
    Free
  end
end;

procedure TSMLCalcul.SetSML(const Value: string);
begin
  FSML := Value
end;

function TSMLCalcul.Value_: string;
begin
  Result := GetBeforStr(FSML, '</Value>');
  Result := GetAfterStr(Result, '<Value');
  Result := GetAfterStr(Result, '>');
end;

function TSMLCalcul.ToDot(const Value: string): string;
begin
  if Pos(',', Value) = 0 then Result := Value
    else Result := Trim(SubstrSeplaceInText(Value, ',', '.'))
end;

{ TStrFifoStack }

constructor TStrFifoStack.Create;
begin
  inherited Create;
  FLock := False;
end;

function TStrFifoStack.Peek: string;
begin
  WaitFor;
  FLock := True;
  try
    if Self.Count > 0 then begin
      Result := Self.Strings[0];
      Delete(0);
    end else Result := EmptyStr
  finally
    FLock := False;
  end
end;

function TStrFifoStack.Poke(const ASt: string):Integer;
begin
  WaitFor;
  FLock := True;
  try
    Result := Add(ASt);
  finally
    FLock := False;
  end
end;

procedure TStrFifoStack.WaitFor;
begin
  if FLock then while not Application.Terminated and FLock do
    Application.ProcessMessages
end;

{ TStrFifoThread }

constructor TStrFifoThread.Create(const APile: TStrFifoStack;
  const ARecorder: TNewRecorder; AGramType: TGramType);
begin
  inherited Create( False );
  {Launch on create}
  ThPile          := APile;
  ThRecorder      := ARecorder;
  ThGramType      := AGramType;
  ThEliteManager  := EliteManager;
  FreeOnTerminate := True;
  Priority        := tpLowest;
end;

procedure TStrFifoThread.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 10 );
  end;
end;

procedure TStrFifoThread.Process;
var
  SML: string;
begin //A-G
  SML := ThPile.Peek;
  if Trim(SML) <> EmptyStr then case ThGramType of
    gt_switch : TGramFactories.SwitchProcess (SML, ThRecorder);
    gt_fumier : TGramFactories.FumierProcess (SML, ThRecorder);
    gt_gauss  : TGramFactories.GaussProcess  (SML, ThRecorder);
    gt_spell  : TGramFactories.SpellProcess  (SML, ThRecorder);
    gt_elite  : TGramFactories.EliteProcess  (SML, ThRecorder);
    gt_pause  : TGramFactories.PauseProcess  (SML, ThRecorder); 
  end;
end;

procedure TStrFifoThread.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TThreadManager }

procedure TThreadManager.EliteFinalize;
begin
  ThElite.Terminate;
end;

procedure TThreadManager.EliteInitialize;
begin
  PileElite  := TStrFifoStack.Create;
  ThElite    := TStrFifoThread.Create( PileElite, Recorder, gt_elite );
end;

class procedure TThreadManager.Finalize;
begin
  with TThreadManager.Create do
  try
    MacrosFinalize;
    FumierFinalize;
    GaussFinalize;
    SpellFinalize;
    EliteFinalize;
    PauseFinalize
  finally
    Free
  end
end;

procedure TThreadManager.FumierFinalize;
begin
  ThFumier.Terminate;
end;

procedure TThreadManager.FumierInitialize;
begin
  PileFumier := TStrFifoStack.Create;
  ThFumier   := TStrFifoThread.Create( PileFumier, Recorder, gt_fumier );
end;

procedure TThreadManager.GaussFinalize;
begin
  ThGauss.Terminate;
end;

procedure TThreadManager.GaussInitialize;
begin
  PileGauss  := TStrFifoStack.Create;
  ThGauss    := TStrFifoThread.Create( PileGauss, Recorder, gt_gauss );
end;

class procedure TThreadManager.Initialize;
begin
  with TThreadManager.Create do
  try
    MacrosInitialize;
    FumierInitialize;
    GaussInitialize;
    SpellInitialize;
    EliteInitialize;
    PauseInitialize
  finally
    Free
  end
end;

procedure TThreadManager.MacrosFinalize;
begin
  ThMacros.Terminate;
end;

procedure TThreadManager.MacrosInitialize;
begin
  PileMacros := TStrFifoStack.Create;
  ThMacros   := TStrFifoThread.Create( PileMacros, Recorder, gt_switch )
end;

procedure TThreadManager.PauseFinalize;
begin
  ThPause.Terminate;
end;

procedure TThreadManager.PauseInitialize;
begin
  PilePause  := TStrFifoStack.Create;
  ThPause    := TStrFifoThread.Create( PilePause, Recorder, gt_pause )
end;

procedure TThreadManager.SpellFinalize;
begin
  ThSpell.Terminate;
end;

procedure TThreadManager.SpellInitialize;
begin
  PileSpell  := TStrFifoStack.Create;
  ThSpell    := TStrFifoThread.Create( PileSpell, Recorder, gt_spell )
end;

{ TLocalStack }

class function TLocalStack.AstAdd(const ASt: string; const grType: TGramType): Integer;
begin //A-D
  case grType of
    gt_switch : Result := TLocalStack.MacrosAdd(ASt);
    gt_fumier : Result := TLocalStack.FumierAdd(ASt);
    gt_gauss  : Result := TLocalStack.GaussAdd(ASt);
    gt_spell  : Result := TLocalStack.SpellAdd(ASt);
    gt_elite  : Result := TLocalStack.EliteAdd(ASt);
    gt_pause  : Result := TLocalStack.PauseAdd(ASt);
    else Result := -1  
  end
end;

class function TLocalStack.EliteAdd(const ASt: string): Integer;
begin
  with PileElite do Result := Poke(ASt)
end;

class function TLocalStack.FumierAdd(const ASt: string): Integer;
begin
  with PileFumier do Result := Poke(ASt)
end;

class function TLocalStack.GaussAdd(const ASt: string): Integer;
begin
  with PileGauss do Result := Poke(ASt)
end;

class function TLocalStack.MacrosAdd(const ASt: string): Integer;
begin
  with PileMacros do Result := Poke(ASt)
end;

class function TLocalStack.PauseAdd(const ASt: string): Integer;
begin
  with PilePause do Result := Poke(ASt)
end;

class function TLocalStack.SpellAdd(const ASt: string): Integer;
begin
  with PileSpell do Result := Poke(ASt);
end;

{ TGramFactories }

class procedure TGramFactories.EliteProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try EliteProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.EliteProcess_(const SML: string;
  const ARecorder: TNewRecorder);
var
  LTag    : string;
  Current : TMetiers;
begin
  Current := TMetiers( KeyReadInt(BufferKey, 'IndexMetier') );
  if Current = m_elite then begin
    LTag := TSMLConfiance.TagStr( SML );
    ThElite.ThEliteManager.SetTags( LTag )
  end
end;

class procedure TGramFactories.FumierProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try FumierProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.FumierProcess_(const SML: string; const ARecorder: TNewRecorder);
var
  LTag : string;
begin
  LTag := TSMLConfiance.TagStr( SML );
  if Pos('110', LTag) > 0 then begin
    TSpeaker.TalkNP(WhatTime);
    Exit;
  end;
  if (Pos('112', LTag) > 0) or (Pos('111', LTag) > 0) then begin
    TSpeaker.TalkNP(GetAfterStr(TSMLConfiance.Text(SML), 'dis-moi'));
    Exit;
  end;
  case IndexStr(AnsiLowerCase(TSMLConfiance.Text(SML)), ['merci', 'encore']) of
    0 : TSpeaker.TalkNP('de rien');
    1 : TSpeaker.TalkNP('vraiment');
  end;

//  TODO

end;

class procedure TGramFactories.GaussProcess(const SML: string; const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try GaussProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.GaussProcess_(const SML: string; const ARecorder: TNewRecorder);
begin
  
end;

class procedure TGramFactories.PauseProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try PauseProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.PauseProcess_(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with ARecorder do
  try
    case TSMLConfiance.Tag( SML ) of
      901 : if (Mode = rm_listen) and (TSMLConfiance.Confidence(SML) > 0.92) then
              case PrevMetier of
                m_fumier  : FumierActivate;
                m_gauss   : GaussActivate;
                m_spell   : SpellActivate;
                m_elite   : EliteActivate;
                else NoneActivate
              end;
      { --- fun comments }
      1001 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.NightMare;
      1002 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.ThankYou;
      1003 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.Meteo;
      1004 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.Meteo_we;
      1005 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.OkGoogle;
      1006 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.ThankMission;
    end
  finally
  end
end;

class procedure TGramFactories.SpellProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try SpellProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.SpellProcess_(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with ARecorder do
  try
//    if TSMLConfiance.Confidence(SML) > 0.89 then
    with TSMLConfiance do TShortCutStacked.Execute( TagStr( SML ), Confidence(SML) )
  except
  end
end;

class procedure TGramFactories.SwitchProcess(const SML: string; const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try SwitchProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.SwitchProcess_(const SML: string; const ARecorder: TNewRecorder);
begin
  with ARecorder do
  try
    case TSMLConfiance.Tag( SML ) of
      1   : Mode := rm_sleep;
      2   : Mode := rm_listen;

      3   : OkActivate;
      4   : CancelActivate;
      5   : YesActivate;
      6   : NoActivate;
      7   : EchapActivate;
      8   : RetourActivate;
      9   : AppCloseActivate;

      10  : if Mode = rm_listen then TSpeaker.TalkNP(WhatTime);
      11  : if Mode = rm_listen then TSpeaker.TalkNP('leur');
      12  : if Mode = rm_listen then TSpeaker.TalkNP('leur qu''il est');

      20  : if Mode = rm_listen then MouseLeftActivate;
      21  : if Mode = rm_listen then MouseRightActivate;
      22  : if Mode = rm_listen then MouseDoubleActivate;
      23  : if Mode = rm_listen then MouseMiddleActivate;

      30  : AppVersionShowActivate;

      100 : if Mode = rm_listen then FumierActivate;
      101 : if Mode = rm_listen then if CurrentMetier = m_fumier then GrammarsDisable;

      200 : if Mode = rm_listen then GaussActivate;
      201 : if Mode = rm_listen then if CurrentMetier = m_gauss then GrammarsDisable;

      300 : if Mode = rm_listen then SpellActivate;

      400 : if Mode = rm_listen then SensibilityActivate;
      401 : if Mode = rm_listen then HighPerfActivate;
      402 : if Mode = rm_listen then MicroCasqueActivate;
      403 : if Mode = rm_listen then MicroStudioActivate;
      404 : if Mode = rm_listen then MicroEnceinteActivate;

      500 : if Mode = rm_listen then PickerGridShowActivate;
      501 : if Mode = rm_listen then PickerGridCloseActivate;
      502 : if Mode = rm_listen then PickerGridMonitorPrevActivate;
      503 : if Mode = rm_listen then PickerGridMonitorNextActivate;
      504 : if Mode = rm_listen then PickerGridPointsShowActivate;
      505 : if Mode = rm_listen then PickerGridPointsHideActivate;
      506 : if Mode = rm_listen then PickerGridPointPrevActivate;
      507 : if Mode = rm_listen then PickerGridPointNextActivate;
      508 : if Mode = rm_listen then PickerGridPointSelectActivate;
      509 : if Mode = rm_listen then PickerGridPointAddActivate;

      510 : if Mode = rm_listen then PickerGridWhiteActivate;
      511 : if Mode = rm_listen then PickerGridGrayActivate;
      512 : if Mode = rm_listen then PickerGridBlackActivate;
      513 : if Mode = rm_listen then PickerGridRedActivate;
      514 : if Mode = rm_listen then PickerGridGreenActivate;
      515 : if Mode = rm_listen then PickerGridBlueActivate;
      516 : if Mode = rm_listen then PickerGridYellowActivate;
      517 : if Mode = rm_listen then PickerGridOrangeActivate;
      518 : if Mode = rm_listen then PickerGridPinkActivate;
      519 : if Mode = rm_listen then PickerGridPointClearActivate;

      600 : if Mode = rm_listen then RaoulIntRightActivate;
      601 : if Mode = rm_listen then RaoulIntLeftActivate;
      602 : if Mode = rm_listen then RaoulIntTopActivate;
      603 : if Mode = rm_listen then RaoulIntBottomActivate;
      604 : if Mode = rm_listen then RaoulIntCmdShowActivate;
      605 : if Mode = rm_listen then RaoulIntCmdHideActivate;
      606 : if Mode = rm_listen then RaoulIntTextShowActivate;
      607 : if Mode = rm_listen then RaoulIntTextHideActivate;
      608 : if Mode = rm_listen then RaoulIntVumetreShowActivate;
      609 : if Mode = rm_listen then RaoulIntVumetreHideActivate;

      700 : if Mode = rm_listen then EliteActivate;

      800 : RaoulRelaunching;

      900 : if Mode = rm_listen then PauseActivate;
      901 : if Mode = rm_listen then NoneActivate;

      { --- fun comments }
      1001 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.NightMare;
      1002 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.ThankYou;
      1003 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.Meteo;
      1004 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.Meteo_we;
      1005 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.OkGoogle;
      1006 : if (Mode = rm_listen) and (CurrentMetier <> m_elite) then TFunTalk.ThankMission;
    end
  except
  end
end;

{ TSpeaker }

class procedure TSpeaker.Talk(const ASt: string);
begin
  with TSpeaker.Create do try Talk_(ASt) finally Free end;
end;

class procedure TSpeaker.TalkNP(const ASt: string);
begin
  with TSpeaker.Create do try TalkNP_(ASt) finally Free end;
end;

procedure TSpeaker.TalkNP_(const ASt: string);
begin
  with TSpVoice.Create(nil) do
  try
    try
      Speak(ASt, SVSFDefault); //SVSFPersistXML); //SVSFDefault);  //SVSFIsXML);
    except
    end
  finally
    Free;
  end;
end;

procedure TSpeaker.Talk_(const ASt: string);
begin
  Recorder.Disable;
  try
    TalkNP_(ASt);
    Sleep(10);
  finally
    Recorder.Enable;
  end
end;

{ TRecorderParams }

class procedure TRecorderParams.Finalize;
begin
  Params.Free;
end;

function TRecorderParams.GetCeilFumier: Double;
begin
  Result := KeyReadFloat(IniKey, 'CeilFumier', 0.50)
end;

function TRecorderParams.GetCeilGauss: Double;
begin
  Result := KeyReadFloat(IniKey, 'CeilGauss', 0.65)
end;

function TRecorderParams.GetCeilSwitch: Double;
begin
  Result := KeyReadFloat(IniKey, 'CeilSwitch', 0.50)
end;

function TRecorderParams.GetSensibility: Integer;
begin
  Result := KeyReadInt(IniKey, 'Sensibility', 1)
end;

class procedure TRecorderParams.Initialize;
begin
  Params := TRecorderParams.Create;
end;

procedure TRecorderParams.SetCeilFumier(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilFumier', Value);
end;

procedure TRecorderParams.SetCeilGauss(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilGauss', Value);
end;

procedure TRecorderParams.SetCeilSwitch(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilSwitch', Value);
end;

procedure TRecorderParams.SetSensibility(const Value: Integer);
begin
  KeyWrite(IniKey, 'Sensibility', Value);
end;

{ TSensibilitySetting }

class procedure TSensibilitySetting.Adjust(const Value: TSensibility);
begin
  with TSensibilitySetting.Create do try Adjust_(Value) finally Free end
end;

class procedure TSensibilitySetting.AdjustInt(const Value: Integer);
begin
   with TSensibilitySetting.Create do try Adjust_(TSensibility(Value)) finally Free end
end;

procedure TSensibilitySetting.Adjust_(const Value: TSensibility);

  procedure High; begin
    with Params do begin
      CeilSwitch := 0.94;
      CeilFumier := 0.91;
      CeilGauss  := 0.91;
    end
  end;

  procedure HeadPhone; begin
    with Params do begin
      CeilSwitch := 0.89;
      CeilFumier := 0.87;
      CeilGauss  := 0.85;
    end
  end;

  procedure Studio; begin
    with Params do begin
      CeilSwitch := 0.80;
      CeilFumier := 0.78;
      CeilGauss  := 0.72;
    end
  end;

  procedure Speaker; begin
    with Params do begin
      CeilSwitch := 0.50;
      CeilFumier := 0.50;
      CeilGauss  := 0.50;
    end
  end;

begin
  if Integer(Value) <> Params.Sensibility then begin
    case Value of
      s_highperformance : High;
      s_headphonemic    : HeadPhone;
      s_studiomic       : Studio;
      s_speakermic      : Speaker;
    end;
    Params.Sensibility := Integer(Value)
  end
end; {Adjust}

class procedure TSensibilitySetting.Floors(var High, Middle: Double);
begin
  with TSensibilitySetting.Create do
  try
    Floors_(High, Middle);
  finally
    Free
  end
end;

procedure TSensibilitySetting.Floors_(var High, Middle: Double);
begin
  case TSensibility(Sensibility_) of
    s_highperformance : begin High := 95.0; Middle := 91.0 end;
    s_headphonemic    : begin High := 91.0; Middle := 86.0 end;
    s_studiomic       : begin High := 86.0; Middle := 81.0 end;
    s_speakermic      : begin High := 81.0; Middle := 76.0 end;
  end
end;

class function TSensibilitySetting.Position: TSensibility;
begin
  with TSensibilitySetting.Create do try Result := Position_ finally Free end
end;

function TSensibilitySetting.Position_: TSensibility;
begin
  Result := TSensibility( LocalTrackBar.Position );
end;

class function TSensibilitySetting.Sensibility: Integer;
begin
  with TSensibilitySetting.Create do
  try
    Result := Sensibility_
  finally
    Free
  end
end;

function TSensibilitySetting.Sensibility_: Integer;
begin
  Result := KeyReadInt(IniKey, 'Sensibility', 3)
end;

{ TInterferenceDisp }

procedure TInterferenceDisp.DispGauges;
var
  I : TInterferenceKind;
begin
  for I := Low(TInterferenceKind) to High(TInterferenceKind) do
    case I of
      ik_SINone     : ;
      ik_SINoise    : if Assigned(LocalBDF) then LocalBDF.Position := FNoises[I];
      ik_SINoSignal : ;
      ik_SITooLoud  : if Assigned(LocalBV)  then LocalBV.Position  := FNoises[I];
      ik_SITooQuiet : if Assigned(LocalNF)  then LocalNF.Position  := FNoises[I];
      ik_SITooFast  : if Assigned(LocalVTL) then LocalVTL.Position := FNoises[I];
      ik_SITooSlow  : if Assigned(LocalVTR) then LocalVTR.Position := FNoises[I];
    end
end;

class procedure TInterferenceDisp.GaugesUpdate(const Value: TArrayOfNoises);
begin
  with TInterferenceDisp.Create do
  try
    SetNoises(Value);
    DispGauges
  finally
    Free
  end
end;

function TInterferenceDisp.NoisesToText(ANoises: TArrayOfNoises): string;
var
  I : TInterferenceKind;
begin
  Result := EmptyStr;
  for I := Low(TInterferenceKind) to High(TInterferenceKind) do
    if Result = EmptyStr then Result := Format('%d', [ANoises[I]])
      else Result := Format('%s-%d', [Result, ANoises[I]]);
end;

procedure TInterferenceDisp.SetNoises(const Value: TArrayOfNoises);
begin
  FNoises := Value
end;

class function TInterferenceDisp.Text(const Value: TArrayOfNoises): string;
begin
  with TInterferenceDisp.Create do
  try
    SetNoises(Value);
    Result := NoisesToText(FNoises)
  finally
    Free
  end
end;

procedure Register;
begin
  RegisterComponents('Raoul',  [ TRaoulRecorder ]);
end;

{ TRaoulRecorder }

constructor TRaoulRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FForm := FormRetrieve;
end;

function TRaoulRecorder.FormRetrieve: TForm;
var
  X : TComponent;
begin
  Result := nil;
  X      := Owner;
  while Assigned(X) and not (X is TForm) do X := X.Owner;
  if Assigned(X) and (X is TForm) then Result := TForm(X);
end;
{ TFunTalk }

function TFunTalk.GetAleaMeteo: Integer;
begin
  Result := KeyReadInt(ParamKey, 'AleaMeteo')
end;

function TFunTalk.GetAleaNightMare: Integer;
begin
  Result := KeyReadInt(ParamKey, 'AleaNightMare')
end;

function TFunTalk.GetMeteaoAsked: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'MeteaoAsked')
end;

class procedure TFunTalk.Initialize;
begin
  with TFunTalk.Create do
  try
    MeteaoAsked := False
  finally
    Free
  end
end;

class procedure TFunTalk.Meteo;
begin
  with TFunTalk.Create do try Meteo_ finally Free end
end;

procedure TFunTalk.Meteo_;
var
  Ran : Integer;
begin
  Randomize;
  with Recorder do begin
    MeteaoAsked := True;
    Ran := Trunc( Random(3) );
    if Ran = AleaMeteo then Ran := (Ran + 1) mod (3);
    AleaMeteo := Ran;
    case Ran of
      0 : TalkFmt(20, 15, 'demande � google', -8, -10, 'lui il n''a qu''�a � fouttre!!!');
      1 : TalkFmt(25, 10, 'la m�t�o aujourd''hui!!!', -5, -15, 'facile, ouvr ta fn�tre!!!');
      2 : TalkFmt(25,  5, 'passe ta t�te dehors', -5, -3, 'et tu verras!!!');
    end
  end;
end;

class procedure TFunTalk.Meteo_we;
begin
  with TFunTalk.Create do try Meteo_we_ finally Free end
end;

procedure TFunTalk.Meteo_we_;
var                                                                
  Ran : Integer;
begin
  Randomize;
  with Recorder do begin
    MeteaoAsked := True;
    Ran := Trunc( Random(3) );
    if Ran = AleaMeteo then Ran := (Ran + 1) mod (3);
    AleaMeteo := Ran;
    case Ran of
      0 : TalkFmt(20, 15, 'j''en ai rien � battre!!!', -10, -6, 'de toute fa�on j''bouge jamais!!!');
      1 : TalkFmt(25, 10, 'la m�t�o du week-end!!!', -5, -15, 'certainement pourri, comme ma pr�vision!!!');
      2 : TalkFmt(10,  5, 't''as pa une question plus intelligente ?');
    end
  end
end;

class procedure TFunTalk.NightMare;
begin
  with TFunTalk.Create do try NightMare_ finally Free end
end;

procedure TFunTalk.NightMare_;
var
  Ran : Integer;
begin
  Randomize;
  with Recorder do begin
    Ran := Trunc( Random(5) );
    if Ran = AleaNightMare then Ran := (Ran + 1) mod (5);
    AleaNightMare := Ran;
    case Ran of
      0 : TalkFmt(38, -8, 'petite nature ?');
      2 : TalkFmt(15, -5, 'et bin mon p�p�re ?', 30, 30, 'il a peur ?');
      4 : TalkFmt(68, 85, 'il a les chocottes, la chochotte ?');
    end
  end
end;

class procedure TFunTalk.OkGoogle;
begin
  with TFunTalk.Create do try OkGoogle_ finally Free end
end;

procedure TFunTalk.OkGoogle_;
var
  Ran : Integer;
begin
  Randomize;
  with Recorder do begin
    Ran := Trunc( Random(3) );
    if Ran = AleaMeteo then Ran := (Ran + 1) mod (3);
    AleaMeteo := Ran;
    case Ran of
      0 : TalkFmt(12, 10, 'tu sais pas quoi ?', -9, -8, 'Google sait pas jouer � �lite !!!');
      1 : TalkFmt(25, 35, 'Ok Google ? Ok gougou gueule ?', -35, -20, 'OK gou gougoug gueule !');
      2 : TalkFmt(15, 10, 'Il est pas l� ! ', -25, -7, 'pa Ok Google !');
    end
  end
end;

procedure TFunTalk.SetAleaMeteo(const Value: Integer);
begin
  KeyWrite(ParamKey, 'AleaMeteo', Value)
end;

procedure TFunTalk.SetAleaNightMare(const Value: Integer);
begin
  KeyWrite(ParamKey, 'AleaNightMare', Value)
end;

procedure TFunTalk.SetMeteaoAsked(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'MeteaoAsked', Value)
end;

class procedure TFunTalk.ThankMission;
begin
  with TFunTalk.Create do try ThankMission_ finally Free end
end;

procedure TFunTalk.ThankMission_;
var
  Ran : Integer;
begin
  Randomize;
  with Recorder do begin
    Ran := Trunc( Random(5) );
    if Ran = AleaNightMare then Ran := (Ran + 1) mod (5);
    AleaNightMare := Ran;
    case Ran of
      0 : TalkFmt(12, 10, 'dis plut�t', -9, -8, 'j''me suis bien gav� sur vot''dos !!!');
      2 : TalkFmt(25, 35, 'tu l''penses vraiment ?', -20, -20, 'moi j''chui pas s�r !');
      4 : TalkFmt(15,  5, 'tu deviens poli ? ', -5, 10, 'alors l�, je dis chapeau ba !');
    end
  end
end;

class procedure TFunTalk.ThankYou;
begin
  with TFunTalk.Create do try ThankYou_ finally Free end
end;

procedure TFunTalk.ThankYou_;
var
  Ran : Integer;
begin
  Randomize;
  with Recorder do if MeteaoAsked then begin
      Ran := Trunc( Random(3) );
      if Ran = AleaMeteo then Ran := (Ran + 1) mod (3);
      AleaMeteo := Ran;
      case Ran of
        0 : TalkFmt(20, 5,  'toujours un plaisir de t''aider');
        1 : TalkFmt(10, 5,  'de rien');
        2 : TalkFmt(15, 10, 'ioure ouelle comme');
      end;
      MeteaoAsked := False;
    end else begin
      Ran := Trunc( Random(5) );
      if Ran = AleaNightMare then Ran := (Ran + 1) mod (5);
      AleaNightMare := Ran;
      case Ran of
        0 : TalkFmt(10, 5,  'de rien');
        2 : TalkFmt(25, 15, 'la politesse, �a fait plaisir!!!');
        4 : TalkFmt(15, 10, 'ioure ouelle comme');
      end
    end
end;

end.
