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

  {Help}
  uEliteHelp, uHelpDlg,

  {devExpress}
  cxLabel, cxProgressBar, cxTrackBar;

const
  FRENCH_ID   = $40c;
  MAX_GRAMMAR = 99;

const
  MainStartDelayed = 3000;

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
  TGramType         = (gt_switch, gt_fumier, gt_gauss, gt_spell, gt_elite, gt_pause, gt_gridmode, gt_help);   //A-G
  TMetiers          = (m_none, m_fumier, m_gauss, m_spell, m_elite, m_pause, m_grid, m_help);                 //A-G
  { --- REM :  1) m_grid & m_help can't be useed as a metier }

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
    FForm   : TForm;
    Fmaprop : Integer;
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
    function  GetIsGridOpened: Boolean;
    procedure SetIsGridOpened(const Value: Boolean);
    function  GetIsEliteMode: Boolean;
    function  GetCalcFormVisible: Boolean;
    function  GetMetierBeforeGauss: TMetiers;
    procedure SetMetierBeforeGauss(const Value: TMetiers);
    function  GetTalkVoiceDisabled: Boolean;
    procedure SetTalkVoiceDisabled(const Value: Boolean);

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

  private
    function CurrentModeInArray(const Values: array of TMetiers): Boolean;

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
    Context    : TSpInProcRecoContext;
    Recognizer : ISpeechRecognizer;

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

    { --- "As a metier" states}
    property IsGridOpened: Boolean read GetIsGridOpened write SetIsGridOpened;
    property IsEliteMode: Boolean read GetIsEliteMode;
    property CalcFormVisible: Boolean read GetCalcFormVisible;
    property MetierBeforeGauss: TMetiers read GetMetierBeforeGauss write SetMetierBeforeGauss;
    property TalkVoiceDisabled: Boolean read GetTalkVoiceDisabled write SetTalkVoiceDisabled;

    constructor Create(AVuMetre: TcxProgressBar = nil); virtual;
  public
    procedure CheckGrammar;
    { --- for testing }
    procedure NoneActivate;
    procedure FumierActivate;
    procedure GaussActivate;
    procedure SpellActivate;
    procedure EliteActivate;
    procedure PauseActivate;
    { --- not as a metier }
    procedure GridActivate;
    procedure GridDeactivate;
    procedure HelpActivate;
    procedure HelpDeactivate;
    procedure FumierReload;
    { --- unactivate metier }
    procedure GaussDisable;

    procedure NavExitProc(Sender: TObject);
    procedure ExitEliteProc(Sender: TObject);

  public
    { --- Speak }
    procedure TalkFmt(const Rate, Pitch: Integer; ASt: string); overload;
    procedure TalkFmt(const Rate, Pitch: Integer; ASt: string;
      const Rate1, Pitch1: Integer; ASt1: string); overload;

  public
    { --- Advanced trigger }
    function IsListen:Boolean;
    function IsListenCommandCheck(const ASML: string;
      const Confiance: Double):Boolean; overload;
    function IsListenCommandCheck(const ASML: string;
      const Confiance: Double; ModeIgnore : array of TMetiers):Boolean; overload;
    { --- fonctionnement courant implements in uRaoulDisplay }
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
    procedure StartWithWindowsActivate;
    procedure NoStartWithWindowsActivate;
    procedure AskStartWithWindowsActivate;

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
    procedure HelpShowActivate;
    procedure HelpHideActivate;

    procedure PauseBackActivate;
    procedure CalcFormShowActivate;
    procedure CalcFormHideActivate;
    procedure CalcFormDeleteActivate;
    procedure CalcFormRestoreActivate;
    procedure CalcResultVoiceReadActivate;
    procedure CalcCopyActivate;

    procedure VoiceReadActivate;

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
    function  DoOnEliteActivate:Boolean;
    procedure ActionsOnGrid(const ASt: string);
    procedure ActionsOnHelp(const ASt: string);
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
    FTag: string;
    procedure SetSML(const Value: string);
    function  ExtractOperation(const Value: string): string;
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
    procedure GridProcess_(const SML: string; const ARecorder: TNewRecorder);
    procedure HelpProcess_(const SML: string; const ARecorder: TNewRecorder);
  public
    class procedure SwitchProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure FumierProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure GaussProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure SpellProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure EliteProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure PauseProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure GridProcess(const SML: string; const ARecorder: TNewRecorder);
    class procedure HelpProcess(const SML: string; const ARecorder: TNewRecorder);
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
    class function GridAdd(const ASt: string):Integer;
    class function HelpAdd(const ASt: string):Integer;
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
    procedure GridInitialize;
    procedure HelpInitialize;

    procedure MacrosFinalize;
    procedure FumierFinalize;
    procedure GaussFinalize;
    procedure SpellFinalize;
    procedure EliteFinalize;
    procedure PauseFinalize;
    procedure GridFinalize;
    procedure HelpFinalize;
  public
    class procedure Initialize;
    class procedure Finalize;
  end;

  TSpeaker = class
  private
    procedure Talk_(const ASt: string);
    procedure TalkNP_(const ASt: string); //non protégé
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

  { --- Multi Action execution on Grid }
  TGridMAStacked = class
  private
    FTagStack  : TStringList;
    FMethod    : TGetStrProc;
    procedure AddToStack(const Tags: string);
    procedure ProcessOnStack;
  public
    procedure SetTags(const Values: string);

    class procedure Execute(const Values: string; WithMethod: TGetStrProc);

    constructor Create;
    destructor Destroy; override;
  end;

  { --- Censure Part }
  TCensureThread = class(TThread)
  private
    ThSML           : string;
    ThTalkativeText : TcxLabel;

    procedure ThDelay(ms: Cardinal);
    procedure Blinking(const Message: string);
    function  CanProcess:Boolean;
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const ASML: string; const ATalkativeText: TcxLabel);
  end;

  { --- Talking thread process }
  TTalkThread = class(TThread)
  private
    ThRecorder      : TNewRecorder;
    ThVoiceValue    : string;

    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const ARecorder: TNewRecorder; const AVoiceValue: string);
  end;

  { --- Write calc résult from register datas }
  TCalcWriteThread = class(TThread)
  private
    ThRichEdit      : TRichEdit;

    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const ARichEdit: TRichEdit);
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
  {Start with Windows}
  StartWithWindowsFunc       : TNotifyEvent = nil;
  NoStartWithWindowsFunc     : TNotifyEvent = nil;
  AskStartWithWindowsFunc    : TNotifyEvent = nil;
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
  {Help}
  HelpShowFunc               : TNotifyEvent = nil;
  HelpHideFunc               : TNotifyEvent = nil;
  {Gauss}
  CalcFormShowFunc           : TNotifyEvent = nil;
  CalcFormHideFunc           : TNotifyEvent = nil;
  CalcFormDeleteFunc         : TNotifyEvent = nil;
  CalcFormRestoreFunc        : TNotifyEvent = nil;
  CalcResultVoiceReadFunc    : TNotifyEvent = nil;
  VoiceReadFunc              : TNotifyEvent = nil;
  CalcCopyFunc               : TNotifyEvent = nil;

var
  ArrayOfJobFiles : TArrayOfJobFiles = ('Switch', 'gr_main', 'Arinum', 'Spelling', 'Elite', 'Pause',  'Gridmode', 'help');    //A-G   Numeration

// IMPORTANT Catalog ---> Raoul.trf=

//  GrammarPath     : string = 'H:\2020 Developpement\Vocalis\bin\Grammar';     //ver dev
  GrammarPath     : string = 'Grammar';                                     //ver prod

  GrammarStrs     : array[0..Integer(High(TGramType))] of string =
   ('switch', 'fumier', 'arinum', 'spelling', 'elite', 'pause', 'gridmode', 'help');   //A-G     gauss
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

  PileGrid        : TStrFifoStack;
  ThGrid          : TStrFifoThread;

  PileHelp        : TStrFifoStack;
  ThHelp          : TStrFifoThread;

implementation

uses
  uRegistry, uRaoulUpdater, uRaoulDisplay, uEliteUtils, uDosUtils, uGauss, uGaussDisplay;

var
  InterferenceStr : array[TInterferenceKind] of string =
   ( '',
     'Bruit de fond',
     'Micro inactif',
     'Bruit violent',
     'Niveau sonore faible',
     'Débit vocal trop lent',
     'Débit vocal trop rapide'
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

  { - Le context pour la reconnaissance est inactif par défaut - }
  Recognizer.State := SRSInactive;

  { - Permettre au contexte de reconnaître et d'interpréter les entrées audios - }
  FCategory.SetId(HKMLCategory, True);
  FToken.SetId(HKMLCategory, HKMLCategory, True);

  { - Connexion des événements - }
  DefineEvents;

  { - Ajout des grammaires - }
  GrammarLoading;

  { Sélectionner le format ie la qualité de l'enregistrement audio
    Influe également sur la qualité de l'écoute à priori --> test avec enceinte  }
  Context.RetainedAudioFormat.type_ := SAFTCCITT_uLaw_44kHzStereo; //SAFTGSM610_44kHzMono; //SAFT8kHz8BitMono;

  {Pour l'utilisation de l'event AudioLevel car non défini par défaut}
  Context.EventInterests := SREAllEvents;

  ComomnInitialization
end;

procedure TCustomDefRecorder.FoldersInitialize;
begin
  try MkDir('Grammar') except end
end;


function TCustomDefRecorder.GetState: TSRState;
begin
  with Recognizer do Result := TSRState( State )
end;

procedure TCustomDefRecorder.SetState(const Value: TSRState);
begin
  with Recognizer do State := Integer( Value )
end;

function TCustomDefRecorder.GrammarCreate(AFileName: string;
  var Index: Integer): ISpeechRecoGrammar;
begin
  Result := Context.CreateGrammar( Index );
  FGrammars[Index] := Result;
  with Result do CmdLoadFromFile(AFileName, SLODynamic);
  Inc( Index )
end;

procedure TCustomDefRecorder.Initialize;
begin
  FLock := True;
  try
    CreateContext
  finally
    FLock := False
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
      else AudioInput := nil
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
    end
  end
end;

procedure TCustomDefRecorder.SetAwakeMode;
begin
  if Assigned(FOnAwakeMode) then FOnAwakeMode(Self)
end;

procedure TCustomDefRecorder.SetSleepMode;
begin
  GrammarsDisable;
  if IsGridOpened then PickerGridCloseActivate;
  if Assigned(FVuMetre) then with FVuMetre do Visible := False;
  if HelpView.Visible then HelpView.Close;
  if Assigned(FOnSleepMode) then FOnSleepMode(Self);
  if GaussDisplayForm.Visible then GaussDisplayForm.Close
end;

procedure TCustomDefRecorder.GrammarsDisable;
var
  i : Integer;
begin
  CurrentMetier := m_none;
  FGrammars[0].CmdSetRuleIdState(0, SGDSactive);
  for i := 1 to FGramCount do 
  try
    FGrammars[i].CmdSetRuleIdState(0, SGDSInactive)
  except
  end
end;

procedure TCustomDefRecorder.GrammarActivate(grType: TGramType;
  grState: Boolean);
begin
  case grState of
    True : FGrammars[Integer(grType)].CmdSetRuleIdState(0, SGDSActive);
    else   FGrammars[Integer(grType)].CmdSetRuleIdState(0, SGDSInactive)
  end
end;

procedure TCustomDefRecorder.GrammarReload(grType: TGramType);
begin
  FGrammars[Integer(grType)].CmdLoadFromFile(GrammarFileName(grType), SLODynamic)
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
  with Context do EventInterests := SREAllEvents
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
  IsGridOpened        := False
end;

procedure TCustomDefRecorder.VuMetreReset;
begin
  if Assigned(FVuMetre) then with FVuMetre do begin
    Position := 0;
    with Properties do PeakValue := 0
  end
end;

procedure TCustomDefRecorder.VuMetreSetter(const Apos: Integer);
begin
  if Assigned(FVuMetre) then with FVuMetre do Position := APos
end;

procedure TCustomDefRecorder.FumierActivate;
begin //A-G
  try
    GrammarActivate(gt_gauss,    False);
    GrammarActivate(gt_spell,    False);
    GrammarActivate(gt_elite,    False);
    GrammarActivate(gt_pause,    False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_fumier);

    CurrentMetier := m_fumier;
  except
  end
end;

procedure TCustomDefRecorder.GaussActivate;
begin //A-G
  try
    if CurrentMetier <> m_gauss then MetierBeforeGauss := CurrentMetier;
    GrammarActivate(gt_fumier,   False);
    GrammarActivate(gt_spell,    False);
    GrammarActivate(gt_elite,    False);
    GrammarActivate(gt_pause,    False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_gauss);

    CurrentMetier := m_gauss;
    GaussDisplayForm.Show
  except
  end
end;

procedure TCustomDefRecorder.SpellActivate;
begin //A-G
  TSCRepeater.Initialize;
  try
    GrammarActivate(gt_fumier,   False);
    GrammarActivate(gt_gauss,    False);
    GrammarActivate(gt_elite,    False);
    GrammarActivate(gt_pause,    False);
    GrammarActivate(gt_switch);
    GrammarActivate(gt_spell);

    CurrentMetier := m_spell;
  except
  end
end;

procedure TCustomDefRecorder.EliteActivate;
begin //A-G
  try
    GrammarActivate(gt_fumier,   False);
    GrammarActivate(gt_gauss,    False);
    GrammarActivate(gt_spell,    False);
    GrammarActivate(gt_pause,    False);
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
    GrammarActivate(gt_fumier,   False);
//    GrammarActivate(gt_gauss,    False);
    GrammarActivate(gt_spell,    False);
    GrammarActivate(gt_elite,    False);
    GrammarActivate(gt_pause,    False);
    GrammarActivate(gt_gridmode, IsGridOpened);
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
    State  := srs_active
  end
end;

procedure TCustomDefRecorder.NoisesReset;
var
  n : Integer;
begin
  n := Integer( High(TInterferenceKind) ) + 1;
  FillChar(FNoises, n*SizeOf(Integer), 0)
end;

procedure TCustomDefRecorder.NoisesInc(const Item: TInterferenceKind);
begin
  FNoises[Item] := FNoises[Item] + 1
end;

procedure TCustomDefRecorder.SensibilityActivate;
begin
  if (ModeFonctionnement in [mf_none, mf_sensibility])
     and Assigned(SensibilityFunc) then SensibilityFunc(nil)
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
  Result := TModeFonctionnement( KeyReadInt(ParamKey, 'ModFunctionment', 0) )
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
  if (ModeFonctionnement in [mf_none, mf_closeapp]) and Assigned(AppCloseFunc)
    then AppCloseFunc(nil)
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
    with TalkativeFacade do Text := 'Mise à jour en attente, redémarrage automatique impossible';
    Exit
  end;
  TAppUpdater.TryToRelauch
end;

procedure TCustomDefRecorder.PauseActivate;
begin
  try
    PrevMetier := CurrentMetier;
    GrammarActivate(gt_fumier,   False);
    GrammarActivate(gt_gauss,    False);
    GrammarActivate(gt_spell,    False);
    GrammarActivate(gt_switch,   False);
    GrammarActivate(gt_elite,    False);
    if IsGridOpened then GrammarActivate(gt_gridmode, False);
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
  if IsListen and (CurrentMetier = m_spell) then NoneActivate
end;

procedure TCustomDefRecorder.ExitEliteProc(Sender: TObject);
begin
  if IsListen then NoneActivate
end;

procedure TCustomDefRecorder.GridActivate;
begin //A-G
  try
    IsGridOpened := True;
    GrammarActivate(gt_gridmode);
  except
  end
end;

procedure TCustomDefRecorder.GridDeactivate;
begin //A-G
  try
    IsGridOpened := False;
    GrammarActivate(gt_gridmode, False)
  except
  end
end;

function TCustomDefRecorder.GetIsGridOpened: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'IsGridOpened')
end;

procedure TCustomDefRecorder.SetIsGridOpened(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'IsGridOpened', Value)
end;

procedure TCustomDefRecorder.CheckGrammar;
{ --- main delayed starter of MainStartDelayed value --> let Grammar check process running }
var
  i : Integer;
begin
  try
    for i := Integer(Low(TGramType)) to Integer(High(TGramType)) do begin
      GrammarActivate(TGramType(i));
      Sleep( 60 );
      GrammarActivate(TGramType(i), False)
    end;

    GrammarActivate(gt_switch);
    GrammarActivate(gt_gauss);
    GrammarActivate(gt_help);
  except
  end;
end;

procedure TCustomDefRecorder.HelpActivate;
begin //A-G
  try
    GrammarActivate(gt_help);
  except
  end
end;

procedure TCustomDefRecorder.HelpDeactivate;
begin //A-G
  try
    GrammarActivate(gt_help, False)
  except
  end
end;

function TCustomDefRecorder.GetIsEliteMode: Boolean;
begin
  Result := CurrentMetier = m_elite
end;

procedure TCustomDefRecorder.StartWithWindowsActivate;
begin
  if Assigned(StartWithWindowsFunc) then StartWithWindowsFunc(nil)
end;

procedure TCustomDefRecorder.AskStartWithWindowsActivate;
begin
  if Assigned(AskStartWithWindowsFunc) then AskStartWithWindowsFunc(nil)
end;

procedure TCustomDefRecorder.NoStartWithWindowsActivate;
begin
  if Assigned(NoStartWithWindowsFunc) then NoStartWithWindowsFunc(nil)
end;

function TCustomDefRecorder.IsListenCommandCheck(const ASML: string;
  const Confiance: Double): Boolean;
begin
  Result := IsListen and (TSMLConfiance.Confidence( ASML ) > Confiance)
end;

function TCustomDefRecorder.IsListenCommandCheck(const ASML: string;
  const Confiance: Double; ModeIgnore: array of TMetiers): Boolean;
begin
  Result := False;
  if not CurrentModeInArray([m_elite])
    then Result := IsListenCommandCheck( ASML, Confiance )
end;

function TCustomDefRecorder.CurrentModeInArray(
  const Values: array of TMetiers): Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := Low(Values) to High(Values) do
    if CurrentMetier = Values[i] then begin
      Result := True;
      Break
    end
end;

procedure TCustomDefRecorder.HelpShowActivate;
begin
  if Assigned(HelpShowFunc) then HelpShowFunc(nil)
end;

procedure TCustomDefRecorder.HelpHideActivate;
begin
  if Assigned(HelpHideFunc) then HelpHideFunc(nil)
end;

procedure TCustomDefRecorder.PauseBackActivate;
var
  PassByNone: Boolean;
begin
  PassByNone := False;
  case PrevMetier of
    m_fumier  : FumierActivate;
    m_gauss   : GaussActivate;
    m_spell   : SpellActivate;
    m_elite   : EliteActivate;
    else begin
      NoneActivate;
      PassByNone := True
    end
  end;
  if not PassByNone and IsGridOpened then GridActivate
end;

function TCustomDefRecorder.IsListen: Boolean;
begin
  Result := Mode = rm_listen
end;

procedure TCustomDefRecorder.CalcFormShowActivate;
begin
  if Assigned(CalcFormShowFunc) then CalcFormShowFunc(nil)
end;

procedure TCustomDefRecorder.CalcFormHideActivate;
begin
  if Assigned(CalcFormHideFunc) then CalcFormHideFunc(nil)
end;

procedure TCustomDefRecorder.CalcFormDeleteActivate;
begin
  if Assigned(CalcFormDeleteFunc) then CalcFormDeleteFunc(nil)
end;

procedure TCustomDefRecorder.CalcFormRestoreActivate;
begin
  if Assigned(CalcFormRestoreFunc) then CalcFormRestoreFunc(nil)
end;

function TCustomDefRecorder.GetCalcFormVisible: Boolean;
begin
  Result := KeyReadBoolean(BufferKey, 'CalcFormVisible')
end;

function TCustomDefRecorder.GetMetierBeforeGauss: TMetiers;
begin
  Result := TMetiers( KeyReadInt(BufferKey, 'MetierBeforeGauss') )
end;

procedure TCustomDefRecorder.SetMetierBeforeGauss(const Value: TMetiers);
begin
  KeyWrite(BufferKey, 'MetierBeforeGauss', Integer(Value) )
end;

procedure TCustomDefRecorder.GaussDisable;
begin
  { --- Désactiver la grammaire pour le calcul }
//  GrammarActivate(gt_gauss, False);
  { --- métiers principaux précédemment actifs }
  case MetierBeforeGauss of  //A-D
    m_none   : NoneActivate;
    m_fumier : FumierActivate;
    m_spell  : SpellActivate;
    m_elite  : EliteActivate;
  end;
  { --- métiers superposés précédemment actifs }
  if IsGridOpened then GridActivate;
  if HelpDlg.Visible then HelpActivate ;
  Application.ProcessMessages
end;

function TCustomDefRecorder.GetTalkVoiceDisabled: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'VoiceDisabled')
end;

procedure TCustomDefRecorder.SetTalkVoiceDisabled(const Value: Boolean);
begin
  KeyWrite(AppKey, 'VoiceDisabled', Value)
end;

procedure TCustomDefRecorder.CalcResultVoiceReadActivate;
begin
  if Assigned(CalcResultVoiceReadFunc) then CalcResultVoiceReadFunc(nil)
end;

procedure TCustomDefRecorder.VoiceReadActivate;
begin
  if Assigned(VoiceReadFunc) then VoiceReadFunc(nil)
end;

procedure TCustomDefRecorder.CalcCopyActivate;
begin
  if Assigned(CalcCopyFunc) then CalcCopyFunc(nil)
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
      gt_elite,
      gt_gridmode,
      gt_help       : Result := QuoteFix(Speech.PhraseInfo.GetText(0,-1, True));
      gt_gauss      : if CurrentMetier = m_gauss then Result := TSMLCalcul.Operation(SML);
    end
  end;

  procedure Accept; begin
    TLocalStack.AstAdd(SML, GSender);
    if IsListen then if Assigned(FOnRecognizeAccepted) then
      FOnRecognizeAccepted(Self, DisplayStr, SML)
  end;

  procedure Reject; begin
    if Assigned(FOnRecognizeReject) then FOnRecognizeReject(Self)
  end;

  procedure Initialize; begin
    Speech  := Result;
    SML     := (Result as ISpeechXMLRecoResult).GetXMLResult( SPXRO_SML );
    Taux    := TSMLConfiance.Confidence(SML);
    GSender := TSMLConfiance.Sender(SML)
  end;

  function CanProcess:Boolean; begin
    Result := False;
    Initialize;
    if GSender = gt_switch then if TSMLConfiance.Tag(SML) = 0 then begin
      Result := False;
      Exit
    end;
    case GSender of  //A-G
      gt_switch,
      gt_spell,
      gt_pause,
      gt_elite,
      gt_gridmode,
      gt_help      : Result := FTaux > Params.CeilSwitch;
      gt_fumier    : Result := FTaux > Params.CeilFumier;
      gt_gauss     : Result := FTaux > Params.CeilGauss;  
    end
  end;

  procedure Finalize; begin
    {réinitialise le suivi des interférences}
    FInterference := ik_SINone;
    NoisesReset
  end;

begin
  if CanProcess then Accept else Reject;
  Finalize
end; {Recognition}

procedure TCustomNewRecorder.Hypothesis(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant; const Result: ISpeechRecoResult);
begin
  if IsListen then if Assigned(FOnBuildHypothesis) then
    FOnBuildHypothesis(Self, QuoteFix(Result.PhraseInfo.GetText(0,-1, True)) )
end;

procedure TCustomNewRecorder.ComomnInitialization;
begin
  if KeyReadBoolean(AppKey, 'GrammarUpdated')
    then TalkFmt(18,0, 'Maintnant c''est à jour ! Raoul va s''relancer')
    else TalkFmt(10,0, 'je suis prêt');

  CheckGrammar;

  Listen         := True;
  State          := srs_active;
  Mode           := rm_sleep;
  CurrentMetier  := m_none;
  FInterference  := ik_SINone
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
    OnEndStream             := EndStream
  end
end;

procedure TCustomNewRecorder.GrammarLoading;
var
  i : Integer;
begin
  FGramCount := 0;
  for i := Integer(Low(TGramType)) to Integer(High(TGramType)) do begin
    GrammarCreate( GrammarFileName( TGramType(i) ), FGramCount);
    Application.ProcessMessages
  end;
  {Activer la grammaire index 0 qui doit le rester pendant le run-time}
  GrammarActivate(gt_switch)
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
  {Depend du type de grammaire je crois... à vérifier}
end;

procedure TCustomNewRecorder.SoundEnd(ASender: TObject; StreamNumber: Integer;
  StreamPosition: OleVariant);
begin
  if Assigned(FOnSoundEnd) then FOnSoundEnd(Self);
  VuMetreReset;
  if Assigned(FOnAudioLevelChange) then FOnAudioLevelChange(Self, 0)
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
    Result := ExtractConfidence(ASML)
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
  Buffer : string;
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
    6 : Result := gt_gridmode;
    7 : Result := gt_help;
    else raise Exception.Create('nom de grammaire inconnue')
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
  Result := GetAfterStr(Result,   '>')
end;

function TSMLConfiance.ExtractText(const Value: string): string;
begin
  SetSML(Value);
  Result := GetAfterStr(FSML,   '<SML text="');
  Result := GetBeforStr(Result, '"')
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
    Result := ExtractTag(ASML)
  finally
    Free
  end
end;

class function TSMLConfiance.TagStr(const ASML: string): string;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractTagStr(ASML)
  finally
    Free
  end
end;

class function TSMLConfiance.Text(const ASML: string): string;
begin
  with TSMLConfiance.Create do
  try
    Result := ExtractText(ASML)
  finally
    Free
  end
end;

{ TSMLCalcul }

function TSMLCalcul.ExtractOperation(const Value: string): string;
var
  DataResult : string;
begin
  SetSML(Value);
  Result := TStackConvertor.Evalue(FTag, DataResult);
//  if Recorder.CurrentMetier = m_gauss then
//    THistoDisplay.AddTo(GaussDisplayForm.RichEdit, Result, DataResult)
end;

class function TSMLCalcul.Operation(const ASML: string): string;
begin
  with TSMLCalcul.Create do
  try
    if TSMLConfiance.Confidence(ASML) > Params.CeilGauss
      then Result := ExtractOperation(ASML)
      else Result := 'poor'
  finally
    Free
  end
end;

procedure TSMLCalcul.SetSML(const Value: string);
begin
  FSML := Value;
  FTag := TSMLConfiance.TagStr(FSML)
end;

{ TStrFifoStack }

constructor TStrFifoStack.Create;
begin
  inherited Create;
  FLock := False
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
    FLock := False
  end
end;

function TStrFifoStack.Poke(const ASt: string):Integer;
begin
  WaitFor;
  FLock := True;
  try
    Result := Add(ASt)
  finally
    FLock := False
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
  Priority        := tpLowest
end;

procedure TStrFifoThread.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 10 )
  end
end;

procedure TStrFifoThread.Process;
var
  SML: string;
begin //A-G
  SML := ThPile.Peek;
  if Trim(SML) <> EmptyStr then case ThGramType of
    gt_switch    : TGramFactories.SwitchProcess (SML, ThRecorder);
    gt_fumier    : TGramFactories.FumierProcess (SML, ThRecorder);
    gt_gauss     : TGramFactories.GaussProcess  (SML, ThRecorder);
    gt_spell     : TGramFactories.SpellProcess  (SML, ThRecorder);
    gt_elite     : TGramFactories.EliteProcess  (SML, ThRecorder);
    gt_pause     : TGramFactories.PauseProcess  (SML, ThRecorder);
    gt_gridmode  : TGramFactories.GridProcess   (SML, ThRecorder);
    gt_help      : TGramFactories.HelpProcess   (SML, ThRecorder);
  end
end;

procedure TStrFifoThread.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
      Application.ProcessMessages
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TThreadManager }

procedure TThreadManager.EliteFinalize;
begin
  ThElite.Terminate
end;

procedure TThreadManager.EliteInitialize;
begin
  PileElite  := TStrFifoStack.Create;
  ThElite    := TStrFifoThread.Create( PileElite, Recorder, gt_elite )
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
    PauseFinalize;
    GridFinalize;
    HelpFinalize
  finally
    Free
  end
end;

procedure TThreadManager.FumierFinalize;
begin
  ThFumier.Terminate
end;

procedure TThreadManager.FumierInitialize;
begin
  PileFumier := TStrFifoStack.Create;
  ThFumier   := TStrFifoThread.Create( PileFumier, Recorder, gt_fumier )
end;

procedure TThreadManager.GaussFinalize;
begin
  ThGauss.Terminate
end;

procedure TThreadManager.GaussInitialize;
begin
  PileGauss  := TStrFifoStack.Create;
  ThGauss    := TStrFifoThread.Create( PileGauss, Recorder, gt_gauss )
end;

procedure TThreadManager.GridFinalize;
begin
  ThGrid.Terminate
end;

procedure TThreadManager.GridInitialize;
begin
  PileGrid   := TStrFifoStack.Create;
  ThGrid     := TStrFifoThread.Create( PileGrid, Recorder, gt_gridmode )
end;

procedure TThreadManager.HelpFinalize;
begin
  ThHelp.Terminate
end;

procedure TThreadManager.HelpInitialize;
begin
  PileHelp   := TStrFifoStack.Create;
  ThHelp     := TStrFifoThread.Create( PileHelp, Recorder, gt_help )
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
    PauseInitialize;
    GridInitialize;
    HelpInitialize
  finally
    Free
  end
end;

procedure TThreadManager.MacrosFinalize;
begin
  ThMacros.Terminate
end;

procedure TThreadManager.MacrosInitialize;
begin
  PileMacros := TStrFifoStack.Create;
  ThMacros   := TStrFifoThread.Create( PileMacros, Recorder, gt_switch )
end;

procedure TThreadManager.PauseFinalize;
begin
  ThPause.Terminate
end;

procedure TThreadManager.PauseInitialize;
begin
  PilePause  := TStrFifoStack.Create;
  ThPause    := TStrFifoThread.Create( PilePause, Recorder, gt_pause )
end;

procedure TThreadManager.SpellFinalize;
begin
  ThSpell.Terminate
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
    gt_switch   : Result := TLocalStack.MacrosAdd(ASt);
    gt_fumier   : Result := TLocalStack.FumierAdd(ASt);
    gt_gauss    : Result := TLocalStack.GaussAdd(ASt);
    gt_spell    : Result := TLocalStack.SpellAdd(ASt);
    gt_elite    : Result := TLocalStack.EliteAdd(ASt);
    gt_pause    : Result := TLocalStack.PauseAdd(ASt);
    gt_gridmode : Result := TLocalStack.GridAdd(ASt);
    gt_help     : Result := TLocalStack.HelpAdd(ASt);
    else Result := -1
  end;

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

class function TLocalStack.GridAdd(const ASt: string): Integer;
begin
  with PileGrid do Result := Poke(ASt)
end;

class function TLocalStack.HelpAdd(const ASt: string): Integer;
begin
  with PileHelp do Result := Poke(ASt)
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
  with PileSpell do Result := Poke(ASt)
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
    Exit
  end;
  if (Pos('112', LTag) > 0) or (Pos('111', LTag) > 0) then begin
    TSpeaker.TalkNP(GetAfterStr(TSMLConfiance.Text(SML), 'dis-moi'));
    Exit
  end;
  case IndexStr(AnsiLowerCase(TSMLConfiance.Text(SML)), ['merci', 'encore']) of
    0 : TSpeaker.TalkNP('de rien');
    1 : TSpeaker.TalkNP('vraiment');
  end
end;

class procedure TGramFactories.GaussProcess(const SML: string; const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try GaussProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.GaussProcess_(const SML: string; const ARecorder: TNewRecorder);
begin
  with ARecorder do 
    if CurrentMetier = m_gauss then begin
      GaussDisable;
      if KeyReadBoolean(BufferKey, 'CalcFormVisible') then begin
        TCalcWriteThread.Create( GaussDisplayForm.RichEdit );
        CalcResultVoiceReadActivate;
      end
    end else GaussDisable;
end;

class procedure TGramFactories.GridProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try GridProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.GridProcess_(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with Recorder, TSMLConfiance do if Confidence(SML) > 0.84 then
    TGridMAStacked.Execute( TagStr(SML), ActionsOnGrid )
end;

class procedure TGramFactories.HelpProcess(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with TGramFactories.Create do try HelpProcess_(SML, ARecorder) finally Free end
end;

procedure TGramFactories.HelpProcess_(const SML: string;
  const ARecorder: TNewRecorder);
begin
  with Recorder, TSMLConfiance do if Confidence(SML) > 0.84 then
    TGridMAStacked.Execute( TagStr(SML), ActionsOnHelp )
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
      901 : if IsListenCommandCheck(SML, 0.90) then PauseBackActivate;
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
      { --- Censured }
      99999999 : if IsListen then TCensureThread.Create(SML, TalkativeText);
      { --- Main states }
      1   : Mode := rm_sleep;
      2   : Mode := rm_listen;
      { --- Func Raoul }
      3   : OkActivate;
      4   : CancelActivate;
      5   : YesActivate;
      6   : NoActivate;
      7   : if IsListen or HelpDlg.Visible then EchapActivate;
      9   : AppCloseActivate;
      { --- Time function }
      10  : if IsListenCommandCheck(SML, 0.75) then TSpeaker.TalkNP(WhatTime);
      { --- fun comments }
      11  : if IsListen then TSpeaker.TalkNP('leur');
      12  : if IsListen then TSpeaker.TalkNP('leur qu''il est');
      { --- Mouse actions }
      20  : if IsListen then MouseLeftActivate;
      21  : if IsListen then MouseRightActivate;
      22  : if IsListen then MouseDoubleActivate;
      23  : if IsListen then MouseMiddleActivate;
      { --- Date function }
      25  : if IsListenCommandCheck(SML, 0.75) then TSpeaker.TalkNP(WitchDate);
      26  : if IsListenCommandCheck(SML, 0.75) then TSpeaker.TalkNP(WitchFullDate);
      27  : if IsListenCommandCheck(SML, 0.75) then TSpeaker.TalkNP(WitchDayOfWeek);
      { --- Dispay app version }
      30  : AppVersionShowActivate;
      { --- Start with Windows }
      40  : if IsListenCommandCheck(SML, 0.87) then StartWithWindowsActivate;
      41  : if IsListenCommandCheck(SML, 0.87) then NoStartWithWindowsActivate;
      42  : if IsListenCommandCheck(SML, 0.87) then AskStartWithWindowsActivate;
      { --- Voice control }
      50  : if IsListen then TalkVoiceDisabled := False; //Voice enabled
      51  : if IsListen then TalkVoiceDisabled := True;  //Voice disabled
      52  : if IsListen then CalcResultVoiceReadActivate;
      53  : if IsListen then VoiceReadActivate;
      { --- Conversation mode }
      100 : if IsListen then FumierActivate;
      101 : if IsListen then if CurrentMetier = m_fumier then NoneActivate;
      { --- Calculation mode }
      200 : if IsListen then GaussActivate;
      201 : if IsListen then if CurrentMetier = m_gauss then NoneActivate;
      202 : if IsListen then CalcFormShowActivate;
      203 : if IsListen then CalcFormHideActivate;
      204 : if IsListen then CalcFormDeleteActivate;
      205 : if IsListen then CalcFormRestoreActivate;
      206 : if IsListen then CalcCopyActivate;
      207 : if IsListen then TSpeaker.TalkNP('le résultat');
      208 : if IsListen then TSpeaker.TalkNP('la réponse');
      { --- Navigation mode }
      300 : if IsListen then SpellActivate;
      { --- Func Raoul }
      400 : if IsListen then SensibilityActivate;
      401 : if IsListen then HighPerfActivate;
      402 : if IsListen then MicroCasqueActivate;
      403 : if IsListen then MicroStudioActivate;
      404 : if IsListen then MicroEnceinteActivate;
      { --- Grid color selection }
      500 : if IsListen then PickerGridShowActivate;
      502 : if IsListen then PickerGridMonitorPrevActivate;
      503 : if IsListen then PickerGridMonitorNextActivate;
      504 : if IsListen then PickerGridPointsShowActivate;
      { --- Help }
      550 : if IsListen then HelpShowActivate;
      551 : HelpHideActivate;
      { --- Facade manipulaation }
      600 : if IsListen then RaoulIntRightActivate;
      601 : if IsListen then RaoulIntLeftActivate;
      602 : if IsListen then RaoulIntTopActivate;
      603 : if IsListen then RaoulIntBottomActivate;
      604 : if IsListen then RaoulIntCmdShowActivate;
      605 : if IsListen then RaoulIntCmdHideActivate;
      606 : if IsListen then RaoulIntTextShowActivate;
      607 : if IsListen then RaoulIntTextHideActivate;
      608 : if IsListen then RaoulIntVumetreShowActivate;
      609 : if IsListen then RaoulIntVumetreHideActivate;
      { --- Play to Elite Dangerous }
      700 : if IsListen then DoOnEliteActivate;
      { --- Func Raoul }
      800 : RaoulRelaunching;
      900 : if IsListen then PauseActivate;
      901 : if IsListen then NoneActivate;
      { --- fun comments - not available in elite mode }
     1001 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.NightMare;
     1002 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.ThankYou;
     1003 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.Meteo;
     1004 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.Meteo_we;
     1005 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.OkGoogle;
     1006 : if IsListenCommandCheck(SML, 0.75, [m_elite]) then TFunTalk.ThankMission;
    end
  except
  end
end;

{ TSpeaker }

class procedure TSpeaker.Talk(const ASt: string);
begin
  with TSpeaker.Create do try Talk_(ASt) finally Free end
end;

class procedure TSpeaker.TalkNP(const ASt: string);
begin
  with TSpeaker.Create do try TalkNP_(ASt) finally Free end
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
    Free
  end
end;

procedure TSpeaker.Talk_(const ASt: string);
begin
  Recorder.Disable;
  try
    TalkNP_(ASt);
    Sleep(10)
  finally
    Recorder.Enable
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
  Params := TRecorderParams.Create
end;

procedure TRecorderParams.SetCeilFumier(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilFumier', Value)
end;

procedure TRecorderParams.SetCeilGauss(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilGauss', Value)
end;

procedure TRecorderParams.SetCeilSwitch(const Value: Double);
begin
  KeyWrite(IniKey, 'CeilSwitch', Value)
end;

procedure TRecorderParams.SetSensibility(const Value: Integer);
begin
  KeyWrite(IniKey, 'Sensibility', Value)
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
      CeilGauss  := 0.91
    end
  end;

  procedure HeadPhone; begin
    with Params do begin
      CeilSwitch := 0.89;
      CeilFumier := 0.87;
      CeilGauss  := 0.85
    end
  end;

  procedure Studio; begin
    with Params do begin
      CeilSwitch := 0.80;
      CeilFumier := 0.78;
      CeilGauss  := 0.72
    end
  end;

  procedure Speaker; begin
    with Params do begin
      CeilSwitch := 0.50;
      CeilFumier := 0.50;
      CeilGauss  := 0.50
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
    Floors_(High, Middle)
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
  Result := TSensibility( LocalTrackBar.Position )
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
      else Result := Format('%s-%d', [Result, ANoises[I]])
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
  RegisterComponents('Raoul',  [ TRaoulRecorder ])
end;

{ TRaoulRecorder }

constructor TRaoulRecorder.Create(AOwner: TComponent);
begin
  inherited;
  FForm := FormRetrieve
end;

function TRaoulRecorder.FormRetrieve: TForm;
var
  X : TComponent;
begin
  Result := nil;
  X      := Owner;
  while Assigned(X) and not (X is TForm) do X := X.Owner;
  if Assigned(X) and (X is TForm) then Result := TForm(X)
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
      0 : TalkFmt(20, 15, 'demande à google', -8, -10, 'lui il n''a qu''ça à fouttre!!!');
      1 : TalkFmt(25, 10, 'la météo aujourd''hui!!!', -5, -15, 'facile, ouvr ta fnêtre!!!');
      2 : TalkFmt(25,  5, 'passe ta tête dehors', -5, -3, 'et tu verras!!!');
    end
  end
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
      0 : TalkFmt(20, 15, 'j''en ai rien à battre!!!', -10, -6, 'de toute façon j''bouge jamais!!!');
      1 : TalkFmt(25, 10, 'la météo du week-end!!!', -5, -15, 'certainement pourri, comme ma prévision!!!');
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
    Ran := Trunc( Random(20) );
    if Ran = AleaNightMare then Ran := (Ran + 1) mod (20);
    AleaNightMare := Ran;
    case Ran of
      1  : TalkFmt(38, -8, 'petite nature ?');
      9  : TalkFmt(15, -5, 'et bin mon pépère ?', 30, 30, 'il a peur ?');
      18 : TalkFmt(68, 85, 'il a les chocottes, la chochotte ?');
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
      0 : TalkFmt(12, 10, 'tu sais pas quoi ?', -5, -2, 'Google se tape alexa la ninfo !');
      1 : TalkFmt(25, 35, 'Ok Google ?', -5, -17, 'tu n''as qu''ça à dire ?');
      2 : TalkFmt(15, 10, 'Il est pas là ?', -25, -7, 'pa Ok Google !');
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
    Ran := Trunc( Random(20) );
    if Ran = AleaNightMare then Ran := (Ran + 1) mod (20);
    AleaNightMare := Ran;
    case Ran of
      1  : TalkFmt(12, 10, 'dis plutôt', -7, -6, 'j''me suis bien gavé sur vot''dos !!!');
      9  : TalkFmt(12, 10, 'il t''arrive quoi là ?', 14, 10, 'un début d''anévrisme ?');
      18 : TalkFmt(15,  5, 'tu deviens poli ? ', -5, 10, 'alors là, je dis chapeau ba !');
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
        2 : TalkFmt(25, 15, 'la politesse, ça se perd !!!');
        4 : TalkFmt(15, 10, 'ioure ouelle comme');
      end
    end
end;

{ TGridMAStacked }

procedure TGridMAStacked.AddToStack(const Tags: string);
var
  ASt : string;

  procedure Stack(const Value: string); begin
    if Pos('undefined', Value) = 0 then FTagStack.Insert(0, Value )
  end;

  function Current: string; begin
    Result := GetBeforStr(ASt, '.');
    ASt    := GetAfterStr(ASt, '.')
  end;

  function MoveNext: Boolean; begin
    Result := Pos('.', ASt) > 0
  end;

  procedure Initialize; begin
    FTagStack.Clear;
    ASt := Tags
  end;

begin
  Initialize;
  while MoveNext do Stack( Current );
  if Ast <> EmptyStr then Stack( Ast )
end; {AddToStack}

constructor TGridMAStacked.Create;
begin
  inherited Create;
  FTagStack := TStringList.Create;
  FMethod   := nil
end;

destructor TGridMAStacked.Destroy;
begin
  FTagStack.Free;
  inherited
end;

class procedure TGridMAStacked.Execute(const Values: string;
  WithMethod: TGetStrProc);
begin
  with TGridMAStacked.Create do
  try
    FMethod := WithMethod;
    SetTags( Values )
  finally
    Free
  end
end;

procedure TGridMAStacked.ProcessOnStack;
begin
  with FTagStack do
  try
    while Count > 0 do begin
      if Assigned(FMethod) then FMethod( Strings[ Pred(Count) ] );
      Delete( Pred(Count) )
    end
  finally
  end
end;

procedure TGridMAStacked.SetTags(const Values: string);
begin
  AddToStack( Values );
  ProcessOnStack
end;

{ TNewRecorder }

procedure TNewRecorder.ActionsOnGrid(const ASt: string);
var
  Cmd: Integer;
begin
  try
    Cmd := StrToInt( ASt );
    if IsListen then
      case Cmd of
        7   : EchapActivate;
        8   : RetourActivate; //Grid level back
        { --- Mouse actions }
        20  : MouseLeftActivate;
        21  : MouseRightActivate;
        22  : MouseDoubleActivate;
        23  : MouseMiddleActivate;
        { --- Grid functions }
        501 : PickerGridCloseActivate;
        505 : PickerGridPointsHideActivate;
        506 : PickerGridPointPrevActivate;
        507 : PickerGridPointNextActivate;
        508 : PickerGridPointSelectActivate;
        509 : PickerGridPointAddActivate;

        510 : PickerGridWhiteActivate;
        511 : PickerGridGrayActivate;
        512 : PickerGridBlackActivate;
        513 : PickerGridRedActivate;
        514 : PickerGridGreenActivate;
        515 : PickerGridBlueActivate;
        516 : PickerGridYellowActivate;
        517 : PickerGridOrangeActivate;
        518 : PickerGridPinkActivate;
        519 : PickerGridPointClearActivate;
      end
    else
      { REM : ???? why }
      case Cmd of
        7   : EchapActivate;
        8   : RetourActivate; //Grid level back
      end
  except
  end
end;

procedure TNewRecorder.ActionsOnHelp(const ASt: string);
var
  Cmd: Integer;

  procedure HeplChange(const Value: THelpKind); begin
    Helps.Current := Value;
    HelpView.FirstDisplay
  end;

begin
  if KeyReadBoolean(AppKey, 'HelpVisible') then 
  try
    Cmd := StrToInt( ASt );
    if Cmd < 600 then HelpView.TitleSelect(Cmd - 551)
      else
    if Cmd < 621 then HelpView.SubTitleSelect(Cmd - 601)
      else
    case Cmd of
      650 : HeplChange(hk_main);
      651 : HeplChange(hk_elite);
      652 : HeplChange(hk_navigation);
      660 : HelpView.HideAutoOpen := True;
      661 : HelpView.HideAutoOpen := False;
    end
  except
  end
end; {ActionsOnHelp}

var
  EliteMode_FAILED : string =
     'Echec car Elite Dangerous n''est pas en cours de fonctionnement';

function TNewRecorder.DoOnEliteActivate: Boolean;
begin
  Result := IsEliteRunningUTLS;
  case Result of
    False : with TalkativeFacade do Text := EliteMode_FAILED
    else EliteActivate
  end
end;

{ TCensureThread }

procedure TCensureThread.Blinking(const Message: string);
var
  i   : Integer;
  ASt : string;
begin
  if CanProcess then
  for i := 1 to 5 do begin
    case i mod 2 of
      0 : ASt := EmptyStr;
      1 : ASt := Message;
    end;
    ThTalkativeText.Caption := ASt;
    ThDelay(200);
  end;
end;

function TCensureThread.CanProcess: Boolean;
begin
  Result := AnsiPos(TSMLConfiance.Text(ThSML), ThTalkativeText.Caption) > 0;
  Result := Result and (ThTalkativeText.Caption <> 'CENSURED')
end;

constructor TCensureThread.Create(const ASML: string;
  const ATalkativeText: TcxLabel);
begin
  inherited Create( False );
  {Launch on create}
  ThSML           := ASML;
  ThTalkativeText := ATalkativeText;
  FreeOnTerminate := True;
  Priority        := tpLowest
end;

procedure TCensureThread.Execute;
begin
  inherited;
  ThDelay(2800);
  Synchronize( Process )
end;

procedure TCensureThread.Process;
begin
  Blinking('CENSURED');
end;

procedure TCensureThread.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
      Application.ProcessMessages
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TTalkThread }

constructor TTalkThread.Create(const ARecorder: TNewRecorder;
  const AVoiceValue: string);
begin
  inherited Create( False );
  {Launch on create}
  ThRecorder      := ARecorder;
  ThVoiceValue    := AVoiceValue;
  FreeOnTerminate := True;
  Priority        := tpLowest
end;

procedure TTalkThread.Execute;
begin
  inherited;
  ThDelay(350);
  Synchronize( Process )
end;

procedure TTalkThread.Process;
begin
  with ThRecorder do TSpeaker.TalkNP( ThVoiceValue )
end;

procedure TTalkThread.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
      Application.ProcessMessages
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TCalcWriteThread }

constructor TCalcWriteThread.Create(const ARichEdit: TRichEdit);
begin
  inherited Create( False );
  {Launch on create}
  ThRichEdit      := ARichEdit;
  FreeOnTerminate := True;
  Priority        := tpLowest
end;

procedure TCalcWriteThread.Execute;
begin
  inherited;
  ThDelay(90);
  Synchronize( Process )
end;

procedure TCalcWriteThread.Process;
begin
  THistoDisplay.AddTo(
    ThRichEdit,
    KeyReadString(BufferKey, 'ExprStr'),
    KeyReadString(BufferKey, 'ExprValue')
  )
end;

procedure TCalcWriteThread.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
      Application.ProcessMessages
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

initialization
  KeyWrite(BufferKey, 'IndexMetier', 0)
finalization
end.
