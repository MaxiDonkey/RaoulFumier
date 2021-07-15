{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{             rev-2 06/2021 for Odyssey                 }
{                                                       }
{*******************************************************}

unit uEliteManager;

{ --- Use Status.json Flag2 only with Custom.4.0.binds. IsOnFoot not with Horizon }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrCopyUtils, uRegistry, StrUtils,

  KeysDef, uDosUtils,
  {Elite bindings and Tobii}
  EliteBindingsTools, uStatusReader, uGazeSettings;

type
  TEliteRunningObserver = class(TThread)
  private
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create;
  end;

  TCockPitModeType = (cmt_none, cmt_combat, cmt_exploration);
  TFADOType        = (ft_none, ft_on, ft_off);
  TLandingGearType = (lgt_none, lgt_open, lgt_close);

  TOdysseyPressedKey  = class;
  TOdysseyKeySurveyor = class;
  TCustomEliteManager = class
  private
    FTags              : string;
    FTagStack          : TStringList;
    FKeyInventory      : TKeyInventory;
    LastCmd            : Integer;
    OdysseyKeySurveyor : TOdysseyKeySurveyor;
    FOdysseyPressedKey : TOdysseyPressedKey;
    function  GetMaxRepeat: Integer;
    procedure SetMaxRepeat(const Value: Integer);
  private
    { --- Mutex }
    EliteMutex : Cardinal;
    procedure InitializeMutex;
    procedure FinalizeMutex;
    procedure ProtectedCode(Method: TNotifyEvent);
    procedure SendChar(const Car: Char); overload;
    procedure SendChar(VKCode: SmallInt); overload;
  private
    { --- Gestion des tags }
    procedure DoWithTag(Sender: TObject);
    procedure DoOnStack(Sender: TObject);
    procedure AssignTagsToStack;
    procedure CallCommande(const Ast: string; Again: Boolean = False);
    procedure IterateCallCommande(Count: Integer);
    procedure ProcessOnStack;
  private
    { --- Divers local et API }
    function  IsAgainCommand(const Cmd: Integer):Boolean;
    function  CanLongRepeat:Boolean;
    procedure PipeReset;
    procedure PipeSystem;
    procedure PipeMoteur;
    procedure PipeArmes;
    procedure PousseeHaut(const sTime: Cardinal);
    procedure TrainAtterrissage;
    procedure MainZoom;
    procedure MainDezoom;
  private
    { --- Macros }
    procedure CibleOfAilier(index: Byte);
    procedure NavigationOnAilier(index: Byte);
    procedure FSD;
    procedure SuperNavigation;
    procedure PRL;
    procedure FullSystem;
    procedure FullMoteur;
    procedure FullArme;
    procedure CombatOffensif;
    procedure CombatDefensif;
    procedure ModeFuite;
    procedure ModeDefensif;
    {CMD : Vol rotation}
    procedure Nord; overload;
    procedure Nord(index: Byte; tms: Integer = 0); overload;
    procedure Sud; overload;
    procedure Sud(index: Byte; tms: Integer = 0); overload;
    procedure Est; overload;
    procedure Est(index: Byte; tms: Integer = 0); overload;
    procedure Ouest; overload;
    procedure Ouest(index: Byte; tms: Integer = 0); overload;
    procedure Pere; overload;
    procedure Pere(index: Byte; tms: Integer = 0); overload;
    procedure Fils; overload;
    procedure Fils(index: Byte; tms: Integer = 0); overload;
    {CMD : Vol poussée}
    procedure PousseGauche(index: Byte; tms: Integer = 0);
    procedure PousseDroite(index: Byte; tms: Integer = 0);
    procedure PousseHaut(index: Byte; tms: Integer = 0);
    procedure PousseBas(index: Byte; tms: Integer = 0);
    procedure PousseAvant(index: Byte; tms: Integer = 0);
    procedure PousseArriere(index: Byte; tms: Integer = 0);
    procedure Decollage;
    {CMD : Vol propulsion}
    procedure InverserPropulsion;
    procedure Acceleration;
    procedure Deceleration;
    procedure SpeedM100;
    procedure SpeedM75;
    procedure SpeedM50;
    procedure SpeedM25;
    procedure SpeedNull;
    procedure Speed25;
    procedure Speed50;
    procedure Speed75;
    procedure Speed100;
    {CMD : Vol divers}
    procedure AssistanceDeVol(const FAO: TFADOType);
    procedure Boost;
    procedure FixRotation;
    procedure OrbitDisplay;
    {CMD : Visée}
    procedure Target12h;
    procedure NextTarget;
    procedure PriorTarget;
    procedure MenacePrincipale;
    procedure MenaceSuivante;
    procedure MenacePrecedente;
    procedure Ailier1;
    procedure Ailier2;
    procedure Ailier3;
    procedure AilierTarget;
    procedure AilierNavLock;
    procedure NextSubSystem;
    procedure PriorSubSystem;
    procedure NextRoute;
    {CMD : Armes}
    procedure PrimaryFire(tms: Integer = 0);
    procedure SecondaryFire(tms: Integer = 0);
    procedure NextArmGroup;
    procedure PriorArmGroup;
    procedure HardPoint;
    procedure StealthyMode;
    procedure HeatSink;
    {CMD : Divers}
    procedure LandingGear(const Mode: TLandingGearType);
    procedure CargoEject;
    procedure CargoScoop;
    procedure RadarRangeDec;
    procedure RadarRangeInc;
    procedure Searchlight;
    procedure ShieldCell;
    procedure ChaffLauncher;
    procedure ChargeECM;
    procedure WeaponColour;
    procedure EngineColour;
    procedure NightVision;
    {CMD : Changement de mode}
    procedure LeftPanel;
    procedure CommsPanel;
    procedure QuickCommsPanel;
    procedure RadarPanel;
    procedure RightPanel;
    procedure MapGalaxy;
    procedure MapSystem;
    procedure Pause;
    procedure FriendBoard;
    procedure Codex;
    procedure CockpitMode(const Mode: TCockpitModeType);
    procedure ModeACS;
    {CMD : Mode tableau de bord}
    procedure Up(index: Byte; tms: Integer = 0);
    procedure Down(index: Byte; tms: Integer = 0);
    procedure Left(index: Byte; tms: Integer = 0);
    procedure Right(index: Byte; tms: Integer = 0);
    procedure UISelect;
  public
    procedure UIBack;
  private
    procedure CloseCarte;
    procedure NextSheet(index: Byte);
    procedure PriorSheet(index: Byte);
    procedure NextPage;
    procedure PriorPage;
    {CMD : Conduite}
    procedure DriveAssist;
    procedure TurnLeft(tms: Integer = 0);
    procedure TurnRight(tms: Integer = 0);
    procedure VerticalThruster(tms: Integer = 0);
    procedure VRSPrimaryFire(tms: Integer = 0);
    procedure VRSSecondaryFire(tms: Integer = 0);
    procedure VRSAutoBreak;
    procedure VRSSearchlight;
    procedure VRSTurret;
    procedure VRSNextArmGroup;
    procedure VRSPriorArmGroup;
    {CMD : Conduite visée}
    procedure VRSTarget12h;
    {CMD : Conduite tourelle}
    procedure TurretYawLeft(tms: Integer = 0);
    procedure TurretYawRight(tms: Integer = 0);
    procedure TurretPitchUp(tms: Integer = 0);
    procedure TurretPitchDown(tms: Integer = 0);
    {CMD : Conduite propulsion}
    procedure PropulsionReverse;
    procedure PropulsionAcceleration(tms: Integer = 0);
    procedure PropulsionDeceleration(tms: Integer = 0);
    {CMD : Conduite divers }
    procedure VRSCargoScoop;
    procedure VRSCargoEject;
    procedure ShipDismissRecall;
    {CMD : Ordres au chasseur}
    procedure RequestDock;
    procedure DefensiveBehaviour;
    procedure AggressiveBehaviour;
    procedure FocusTarget;
    procedure HoldFire;
    procedure HoldPosition;
    procedure Follow;
    procedure OpenOrders;
    {CMD : ACS}
    procedure ACSCameraPitchInc(tms: Integer = 0);
    procedure ACSCameraPitchDec(tms: Integer = 0);
    procedure ACSCameraYawInc(tms: Integer = 0);
    procedure ACSCameraYawDec(tms: Integer = 0);
    procedure ACSZoomIn(tms: Integer = 0);
    procedure ACSZoomOut(tms: Integer = 0);
    procedure ACSZoomInMini(tms: Integer = 0);
    procedure ACSZoomOutMini(tms: Integer = 0);
    procedure ACSRadioInc(tms: Integer = 0);
    procedure ACSRadioDec(tms: Integer = 0);
    procedure ACSAnalyse(tms: Integer = 0);
    procedure ACSClose;
    procedure ACSGetTarget;
    procedure ACSHelp;
    {CMD : DSD}
    procedure DSDViewChange;
    procedure DSDClose;
    procedure DSDYawLeft(tms: Integer = 0);
    procedure DSDYawRight(tms: Integer = 0);
    procedure DSDPitchUp(tms: Integer = 0);
    procedure DSDPitchDown(tms: Integer = 0);
    procedure DSDZoomOut(tms: Integer = 0);
    procedure DSDZoomIn(tms: Integer = 0);
    {CMD : Carte de la galaxie}
    procedure CamPitchUp(tms: Integer = 0);
    procedure CamPitchDown(tms: Integer = 0);
    procedure CamYawLeft(tms: Integer = 0);
    procedure CamYawRight(tms: Integer = 0);
    procedure CamZoomIn(tms: Integer = 0);
    procedure CamZoomOut(tms: Integer = 0);
    procedure GalaxyMapHome;
    {CMD : Système de cameras}
    procedure Camera_ShipShow;
    procedure Camera_VRSShow;
    procedure Camera_Next;
    procedure Camera_Prior;
    procedure Camera_One;
    procedure Camera_Two;
    procedure Camera_Three;
    procedure Camera_Four;
    procedure Camera_Five;
    procedure Camera_Six;
    procedure Camera_Seven;
    procedure Camera_Eight;
    procedure Camera_Nine;
    {CMD : Free cameras}
    procedure FreeCamera_Close;
    {CMD : Atterrissage manuel}
    procedure LandingYawLeft(tms: Integer = 0);
    procedure LandingYawRight(tms: Integer = 0);
    procedure LandingPitchUp(tms: Integer = 0);
    procedure LandingPitchDown(tms: Integer = 0);
    procedure LandingRollLeft(tms: Integer = 0);
    procedure LandingRollRight(tms: Integer = 0);
    procedure LandingLeftThrust(tms: Integer = 0);
    procedure LandingRightThrust(tms: Integer = 0);
    procedure LandingUpThrust(tms: Integer = 0);
    procedure LandingDownThrust(tms: Integer = 0);
    procedure LandingForwardThrust(tms: Integer = 0);
    procedure LandingBackwardThrust(tms: Integer = 0);
    {CMD : Equipage multiple}
    procedure Crew_SwitchMode;
    procedure Crew_PrimaryFire(tms: Integer = 0);
    procedure Crew_SecondaryFire(tms: Integer = 0);
    procedure Crew_ToolsPrimaryFire(tms: Integer = 0);
    procedure Crew_ToolsSecondaryFire(tms: Integer = 0);
    procedure Crew_YawLeft(tms: Integer = 0);
    procedure Crew_YawRight(tms: Integer = 0);
    procedure Crew_PitchUp(tms: Integer = 0);
    procedure Crew_PitchDown(tms: Integer = 0);
    procedure Crew_ZoomIn(tms: Integer = 0);
    procedure Crew_ZoomOut(tms: Integer = 0);
    procedure Crew_CokpitNext;
    procedure Crew_CokpitPrior;
    {CMD : NATO Alphabet}
    procedure AlphaKeyBoard(const Car: Char); overload;
    procedure AlphaKeyBoard(const Car: Char; Specials: TSpecials); overload;
    procedure AlphaKeyBoard(VKCode: SmallInt); overload;
    procedure Coller;

    {CMD : au sol *** rev-2 06/2021 Odyssey *** }
    procedure KeyStop;                                          //Stop l'enfoncement d'une touche
    procedure KeyFireStop;
    procedure HumAvance; overload;
    procedure HumRecule; overload;
    procedure HumAvance(index: Byte; tms: Integer = 0); overload;        //déplacement vers l'avant
    procedure HumRecule(index: Byte; tms: Integer = 0); overload;        //déplacement vers l'arrière
    procedure HumLateralLeft(index: Byte; tms: Integer = 0);    //déplacement latéral à gauche
    procedure HumLateralRight(index: Byte; tms: Integer = 0);   //déplacement latéral à droite
    procedure HumRotateLeft(index: Byte; tms: Integer = 0);     //rotation à gauche
    procedure HumRotateRight(index: Byte; tms: Integer = 0);    //rotation à droite
    procedure HumPitchUp(index: Byte; tms: Integer = 0);        //yeux lever (tangage)
    procedure HumPitchDown(index: Byte; tms: Integer = 0);      //yeux baisser (tangage)

    procedure HumSprint;                                        //course activation
    procedure HumWalk;                                          //marche activation
    procedure HumAccroupir;                                     //à genoux
    procedure HumJump;                                          //saut jetpack
    procedure HumPrimaryInteract;                               //première interaction
    procedure HumSecondaryInteract;                             //seonde interaction

    procedure HumItemWheel;                                     //roue d'inventaire show/hide     TODO
    procedure HumItemWheelXAxis;                                //roue d'inventaire axe C         TODO
    procedure HumItemWheelXLeft;                                //roue d'inventaire à gauche      TODO
    procedure HumItemWheelXRight;                               //roue d'inventaire à droite      TODO
    procedure HumItemWheelYAxis;                                //roue d'inventaire axe Y         TODO
    procedure HumItemWheelYUp;                                  //roue d'inventaire montée        TODO
    procedure HumItemWheelYDown;                                //roue d'inventaire descente      TODO

    procedure HumPrimaryFire; overload;
    procedure HumPrimaryFire(index: Byte; tms: Integer = 0); overload;   //arme Shoot
    procedure HumPrimaryFire_1;
    procedure HumPrimaryFire_Rafale;
    procedure HumAimZoom;                                       //Visée switch
    procedure HumThrowGrenade;                                  //grenade lancer
    procedure HumMelee;                                         //corps à corps
    procedure HumReload;                                        //arme recharger
    procedure HumSwitchWeapon;                                  //arme switch
    procedure HumSelectPrimaryWeapon;                           //arme primaire sélection
    procedure HumSelectSecondaryWeapon;                         //arme secondaire sélection
    procedure HumSelectUtilityWeapon;                           //outil sélection
    procedure HumSelectNextWeapon;                              //arme suivante   voir .43
    procedure HumSelectPreviousWeapon;                          //arme précédente voir .44
    procedure HumHideWeapon;                                    //arme/outil rengainer
    procedure HumSelectNextGrenadeType;                         //grenade type suivant
    procedure HumSelectPreviousGrenadeType;                     //grenade type précédent
    procedure HumToggleFlashlight;                              //lampe enable/disable
    procedure HumToggleNightVision;                             //vision nocturne enable/disable  voir .16
    procedure HumToggleShields;                                 //bouclier enable/disable         TODO {3	8	0000 0008	Shields Up si actif}
    procedure HumClearAuthorityLevel;                           //supprimer niveau d'accréditation
    procedure HumHealthPack;                                    //utiliser le medi kit
    procedure HumBattery;                                       //cellule d'énergie
    procedure HumSelectFragGrenade;                             //grenade fragmentation sélection
    procedure HumSelectEMPGrenade;                              //grenade EMPG sélection
    procedure HumSelectShieldGrenade;                           //grenade bouclier sélection
    procedure HumSwitchToRechargeTool;                          //outil (re)chargeeur
    procedure HumSwitchToCompAnalyser;                          //outil analyseur
    procedure HumSwitchToSuitTool;                              //sélectionner l'outil de la combinaison
    procedure HumToggleToolMode;                                //changer de mode d'outil
    procedure HumToggleMissionHelpPanel;                        //panneau d'aide

    {CMD : au sol mode *** rev-2 06/2021 Odyssey *** }
    procedure HumGalaxyMapOpen;                                 //carte de la galaxie
    procedure HumSystemMapOpen;                                 //carte système
    procedure HumFocusCommsPanel;                               //panneau de communication
    procedure HumQuickCommsPanel;                               //panneau de communication rapide
    procedure HumOpenAccessPanel;                               //roue d'inventaire
    procedure HumConflictContextualUI;                          //panneau de statisques de combat

    {MACRO : au sol *** rev-2 06/2021 Odyssey *** }
    procedure ThrowGrenadeFragmentation;
    procedure ThrowGrenadeEMP;
    procedure ThrowGrenadeBouclier;
    procedure AimZoomRafale;
    procedure ForwardWithCaution;
    procedure BackwardWithCaution;
    procedure StandInFront;
    procedure StandInBack;
    procedure OpenTheDoor;
    procedure ShipBoarding;
    procedure SniperMode;
    procedure Engager;
    procedure EngagerEx;
    procedure DoAfterInteract;
    procedure CashPrimes;
    procedure EnrollmentManage;
    procedure ShowStock;
    procedure ShowConsumable;
    {*** Manager : emulated joystick by Tobii Eye tracker }
    procedure TobiiLoad;
    procedure TobiiStart;
    procedure TobiiPause;
    procedure TobiiStop;
    procedure TobiiMenuMode;
    procedure TobiiConfigCombat;
    procedure TobiiConfigStation;
    procedure TobiiConfigExplo;
    procedure TobiiConfigCutter;
    procedure TobiiConfigDivers1;
    procedure TobiiCongigDivers2;
    procedure TobiiMoreSensibility;
    procedure TobiiLessSensibility;
    procedure TobiiMorePrecision;
    procedure TobiiLessPrecision;
    procedure TobiiSaveDiver1;
    procedure TobiiSaveDiver2;
    {*** Vue subjective}
    procedure HeadLockReset;
    procedure HeadLockUp;
    procedure HeadLockDown;
    procedure HeadLockLeft;
    procedure HeadLockRight;
    procedure HeadLockTools;
    procedure HeadLockExit;
    procedure HeadLockTarget;
    procedure HeadLockShield;
  public
    procedure TobiiConfig;      //Switch 1100
    procedure TobiiHideConfig;  //Switch 1101
    procedure TobiiPauseSwitch; //Switch 1102
    procedure TobiiMouseSwitch; //Switch 1103
    procedure StopMovAndFire;

  private
    procedure OdysseyInteractionValidate;
    procedure HumInteractMenuItem(const Value: Integer);
    procedure StopMovAndTobii;
  private
    procedure UIRetour;
  public
    procedure SetTags(const Value: string);
    procedure SetKeyInventory(const Value: TKeyInventory);
    procedure SetOdysseyKeyPressed(const Value: TOdysseyPressedKey);

    property MaxRepeat: Integer read GetMaxRepeat write SetMaxRepeat;

    constructor Create;
    destructor Destroy; override;
  published
  end;

  TEliteManager = class(TCustomEliteManager)
  public
    class procedure Initialize;
    class procedure TagAssign(const Value: string);
    class procedure KeyInventoryAssign(const Value: TKeyInventory);
    class procedure Finalize;
  published
  end;

  TOdysseyPressedKeyType = (opknone, opkforward, opkbackward, opkfire);
  TOdysseyPressedKeySet = set of TOdysseyPressedKeyType;

  TOdysseyPressedKey = class
  private
    FManager   : TEliteManager;
    FMov       : TOdysseyPressedKeySet;
    FClockMov  : Cardinal;
    FClockFire : Cardinal;
    procedure DoOnCaseAdd(const Value: TOdysseyPressedKeyType);
    procedure DoOnCaseSub(const Value: TOdysseyPressedKeyType);
    function  IsCaseOn(const Value: TOdysseyPressedKeyType): Boolean;
    procedure AddCase(const Value: TOdysseyPressedKeyType);
    procedure SubCase(const Value: TOdysseyPressedKeyType);
  public
    property ClockMov: Cardinal read FClockMov write FClockMov;
    property ClockFire: Cardinal read FClockFire write FClockFire;

    procedure Avancer;
    procedure Reculer;
    procedure Tirer;
    procedure StopMov;
    procedure StopFire;
    procedure Clear;
    function  MvtExists: Boolean;
    function  FireExists: Boolean;

    constructor Create(const Value: TEliteManager);
    destructor Destroy; override;
  end;

  TOdysseyKeySurveyor = class(TThread)
  private
    ThOdysseyPressedKey : TOdysseyPressedKey;
    ThWaitingFor        : Cardinal;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const Value: TOdysseyPressedKey);
  end;


var
  EliteManager         : TEliteManager = nil;
  EliteRunningObserver : TEliteRunningObserver;
  ProcExitElite        : TNotifyEvent = nil;
  OdysseyPressedKey    : TOdysseyPressedKey;

implementation

uses
  uEliteUtils,
  uNewRecorder,
  { --- Pour gérer la saisie des textes
        SendKey(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []); }
  SendKey32,
  uRaoulUpdater;

{ TCustomEliteManager }

procedure TCustomEliteManager.AssignTagsToStack;
begin
  ProtectedCode( DoWithTag )
end;

procedure TCustomEliteManager.CallCommande(const Ast: string; Again: Boolean);
var
  indexCmd: Integer;
begin
  if not Assigned(FKeyInventory) or (ASt = '0') then Exit;
  try indexCmd := StrToInt( ASt ) except indexCmd := 0 end;
  EliteForeGround;
  case indexCmd of
    {*** VOL - ROTATION;  17-63}
    85    : Ouest;   // without_keyup
    10850 : Ouest   (1, 90);
    10851 : Ouest   (1, 250);
    10852 : Ouest   (1, 400);
    10853 : Ouest   (1, 600);
    10854 : Ouest   (1, 150); //rev-2 06/2021
    86    : Est;     // without_keyup
    10860 : Est     (1, 90);
    10861 : Est     (1, 250);
    10862 : Est     (1, 400);
    10863 : Est     (1, 600);
    10864 : Est     (1, 150); //rev-2 06/2021
    87    : Pere;    // without_keyup
    10870 : Pere    (1, 90);
    10871 : Pere    (1, 250);
    10872 : Pere    (1, 400);
    10873 : Pere    (1, 600);
    10874 : Pere    (1, 150); //rev-2 06/2021
    10875 : Pere    (1, 800);
    10876 : Pere    (1, 1000);
    88    : Fils;    // without_keyup
    10880 : Fils    (1, 90);
    10881 : Fils    (1, 250);
    10882 : Fils    (1, 400);
    10883 : Fils    (1, 600);
    10884 : Fils    (1, 150); //rev-2 06/2021
    10885 : Fils    (1, 800);
    10886 : Fils    (1, 1000);
    89    : Nord;    // without_keyup
    10890 : Nord    (1, 90);
    10891 : Nord    (1, 250);
    10892 : Nord    (1, 400);
    10893 : Nord    (1, 600);
    10894 : Nord    (1, 150); //rev-2 06/2021
    90    : Sud;     // without_keyup
    10900 : Sud     (1, 90);
    10901 : Sud     (1, 250);
    10902 : Sud     (1, 400);
    10903 : Sud     (1, 600);
    10904 : Sud     (1, 150); //rev-2 06/2021
    {*** VOL - POUSSEE; 64-115}
    79    : PousseGauche    (1, 90);
    80    : PousseDroite    (1, 90);
    81    : PousseHaut      (1, 90);
    82    : PousseBas       (1, 90);
    83    : PousseAvant     (1, 90);
    84    : PousseArriere   (1, 90);
    {*** VOL - PROPULSION; 146-225}
    67    : InverserPropulsion;
    68    : Acceleration;
    69    : Deceleration;
    70    : SpeedM100;
    71    : SpeedM75;
    72    : SpeedM50;
    73    : SpeedM25;
    74    : SpeedNull;
    75    : Speed25;
    76    : Speed50;
    77    : Speed75;
    78    : Speed100;
    {*** VOL - DIVERS; 305-348}
    60    : AssistanceDeVol(ft_none);
    10601 : AssistanceDeVol(ft_on);
    10602 : AssistanceDeVol(ft_off);
    61    : Boost;
    62    : FSD;
    63    : SuperNavigation;
    64    : PRL;
    65    : FixRotation;
    66    : OrbitDisplay;
    {*** VISEE; 349-432}
    46    : Target12h;
    47    : NextTarget;
    48    : PriorTarget;
    49    : MenacePrincipale;
    50    : MenaceSuivante;
    51    : MenacePrecedente;
    52    : Ailier1;
    53    : Ailier2;
    54    : Ailier3;
    55    : AilierTarget;
    10551 : CibleOfAilier(1);
    10552 : CibleOfAilier(2);
    10553 : CibleOfAilier(3);
    56    : AilierNavLock;
    10561 : NavigationOnAilier(1);
    10562 : NavigationOnAilier(2);
    10563 : NavigationOnAilier(3);
    57    : NextSubSystem;
    58    : PriorSubSystem;
    59    : NextRoute;
    {*** ARMES; 433-459}
    41    : PrimaryFire;
    10411 : PrimaryFire(1000);
    10412 : PrimaryFire(2000);
    10413 : PrimaryFire(3000);
    10414 : PrimaryFire(4000); 
    42    : SecondaryFire;
    10421 : SecondaryFire(1000);
    10422 : SecondaryFire(2000);
    10423 : SecondaryFire(3000);
    10424 : SecondaryFire(4000);
    43    : NextArmGroup;
    44    : PriorArmGroup;
    45    : HardPoint;
    {*** REFROIDISSEMENT; 460-472}
    39    : StealthyMode;
    40    : HeatSink;
    {*** DIVERS; 473-590}
    1     : LandingGear(lgt_none);
    99001 : LandingGear(lgt_open);
    99002 : LandingGear(lgt_close);
    2     : CargoEject;
    3     : CargoScoop;
    4     : PipeReset;
    5     : PipeSystem;
    6     : PipeArmes;
    7     : PipeMoteur;
    8     : RadarRangeDec;
    9     : RadarRangeInc;
    10    : Searchlight;
    11    : ShieldCell;
    12    : ChaffLauncher;
    13    : ChargeECM;
    14    : WeaponColour;
    15    : EngineColour;
    16    : NightVision;
    {*** CHANGEMENT DE MODE; 591-688}
    17    : LeftPanel;
    18    : CommsPanel;
    19    : QuickCommsPanel;
    20    : RadarPanel;
    21    : RightPanel;
    22    : MapGalaxy;
    23    : MapSystem;
    24    : Pause;
    25    : FriendBoard;
    26    : Codex;
    27    : CockpitMode( cmt_none        );
    10271 : CockpitMode( cmt_combat      );
    10272 : CockpitMode( cmt_exploration );
    28    : ModeACS;
    {*** MODE TABLEAU DE BORD; 689-732}
    29    : Down(1);
    30    : Up(1);
    31    : Left(1);
    32    : Right(1);
    33    : UISelect;
    { --- Retour : if grid opened do back on grid else uiback }
    34    : UIRetour;
    10341 : CloseCarte;
    35    : NextSheet(1);
    10352 : NextSheet(2);
    10353 : NextSheet(3);
    10354 : NextSheet(4);
    10355 : NextSheet(5);
    36    : PriorSheet(1);
    10362 : PriorSheet(2);
    10363 : PriorSheet(3);
    10364 : PriorSheet(4);
    10365 : PriorSheet(5);
    37    : NextPage;
    38    : PriorPage;
    {*** CONDUITE; 862-968}
    113   : DriveAssist;
    114   : TurnLeft(500);
    115   : TurnRight(500);
    116   : FKeyInventory.KeyTrigger_( 'BuggyRollLeftButton',  WITH_KEYUP);     //NA
    117   : FKeyInventory.KeyTrigger_( 'BuggyRollRightButton', WITH_KEYUP);     //NA
    118   : FKeyInventory.KeyTrigger_( 'BuggyPitchUpButton',   WITH_KEYUP);     //NA
    119   : FKeyInventory.KeyTrigger_( 'BuggyPitchDownButton', WITH_KEYUP);     //NA
    120   : VerticalThruster(500);
    121   : VRSPrimaryFire;
    122   : VRSSecondaryFire;
    123   : VRSAutoBreak;
    124   : VRSSearchlight;
    125   : VRSTurret;
    126   : VRSNextArmGroup;
    127   : VRSPriorArmGroup;
    {*** CONDUITE VISEEE; 969-974}
    128   : VRSTarget12h;
    {*** CONDUITE TOURELLE; 975-1019}
    129   : TurretYawLeft(40);
    130   : TurretYawRight(40);
    131   : TurretPitchUp(40);
    132   : TurretPitchDown(40);
    {*** CONDUITE PROPULSION; 1020-1055}
    133   : PropulsionReverse;
    134   : PropulsionAcceleration(200);
    135   : PropulsionDeceleration(200);
    {*** CONDUITE DIVERS; 1056-1099}
    136   : FKeyInventory.KeyTrigger_( 'IncreaseEnginesPower_Buggy',   WITH_KEYUP);  //NA
    137   : FKeyInventory.KeyTrigger_( 'IncreaseWeaponsPower_Buggy',   WITH_KEYUP);  //NA
    138   : FKeyInventory.KeyTrigger_( 'IncreaseSystemsPower_Buggy',   WITH_KEYUP);  //NA
    139   : FKeyInventory.KeyTrigger_( 'ResetPowerDistribution_Buggy', WITH_KEYUP);  //NA
    140   : VRSCargoScoop;
    141   : VRSCargoEject;
    142   : ShipDismissRecall;
    {*** CONDUITE MODES; 1100-1165}
          // NA
    {*** ORDRES AU CHASSEUR; 1238-1290}
    105   : RequestDock;
    106   : DefensiveBehaviour;
    107   : AggressiveBehaviour;
    108   : FocusTarget;
    109   : HoldFire;
    110   : HoldPosition;
    111   : Follow;
    112   : OpenOrders;
    {*** DETECTEUR D'ANALYSE COMPLETE DU SYSTEME; 1561-1659}
    143   : ACSCameraPitchInc(50);
    11431 : ACSCameraPitchInc(500); 
    144   : ACSCameraPitchDec(50);
    11441 : ACSCameraPitchDec(500); 
    145   : ACSCameraYawInc(50);
    11451 : ACSCameraYawInc(500);
    146   : ACSCameraYawDec(50);
    11461 : ACSCameraYawDec(500);
    147   : ACSZoomIn(100);
    148   : ACSZoomOut(100);
    149   : ACSZoomInMini(50);
    150   : ACSZoomOutMini(50);
    151   : ACSRadioInc(50);
    152   : ACSRadioDec(50);
    153   : ACSAnalyse(5000);
    154   : ACSClose;
    155   : ACSGetTarget;
    156   : ACSHelp;
    {*** DETECTEUR DE SURFACE DETAILLEE; 1660-1714}
    164   : DSDViewChange;
    165   : DSDClose;
    166   : DSDYawLeft(150);
    167   : DSDYawRight(150);
    168   : DSDPitchUp(150);
    169   : DSDPitchDown(150);
    170   : DSDZoomOut(150);
    171   : DSDZoomIn(150);
    {*** VOL ATTERRISSAGE MANUEL 226-305}
    172   : LandingYawLeft;
    173   : LandingYawRight;
    174   : LandingPitchUp;
    175   : LandingPitchDown;
    176   : LandingRollLeft;
    177   : LandingRollRight;
    178   : LandingLeftThrust;
    179   : LandingRightThrust;
    180   : LandingUpThrust;
    181   : LandingDownThrust;
    182   : LandingForwardThrust;
    183   : LandingBackwardThrust;
    {*** EQUIPAGE MULTIPLE 1186-1257}
    184   : Crew_SwitchMode;
    185   : Crew_PrimaryFire(1500);
    186   : Crew_SecondaryFire(1500);
    187   : Crew_ToolsPrimaryFire(1500);
    188   : Crew_ToolsSecondaryFire(1500);
    189   : Crew_YawLeft(150);
    190   : Crew_YawRight(150);
    191   : Crew_PitchUp(150);
    192   : Crew_PitchDown(150);
    193   : Crew_ZoomOut(150);
    194   : Crew_ZoomIn(150);
    195   : Crew_CokpitNext;
    196   : Crew_CokpitPrior;
    {*** CARTE DE LA GALAXIE 781-867}
    157   : CamPitchUp(200);
    158   : CamPitchDown(200);
    159   : CamYawLeft(200);
    160   : CamYawRight(200);
    161   : CamZoomIn(200);
    162   : CamZoomOut(200);
    163   : GalaxyMapHome;
    {*** SYSTÈME DE CAMERAS 1311-1370}
    91    : Camera_ShipShow;
    92    : Camera_VRSShow;
    93    : Camera_Next;
    94    : Camera_Prior;
    95    : Camera_One;
    96    : Camera_Two;
    97    : Camera_Three;
    98    : Camera_Four;
    99    : Camera_Five;
    100   : Camera_Six;
    101   : Camera_Seven;
    102   : Camera_Eight;
    103   : Camera_Nine;
    {*** LISTE DE LECTURE 1564-1580}
    {*** CAMERA LIBRE 1401-…}
    104   : FreeCamera_Close;

    { --- Vol }
    9950  : Decollage;
    { --- Pipe }
    9990  : FullSystem;
    9991  : FullArme;
    9992  : FullMoteur;
    9993  : CombatOffensif;
    9994  : CombatDefensif;
    9995  : ModeFuite;
    9996  : ModeDefensif; 
    { --- Key Entrée }
    9998  : TKeyMessageSender.Signal(VK_RETURN, 30, WITH_KEYUP);
    { --- Again methodes }
    9999  : IterateCallCommande(    1 );
    10000 : IterateCallCommande(    2 );
    10001 : IterateCallCommande(    3 );
    10002 : IterateCallCommande(    4 );
    10003 : IterateCallCommande(    5 );
    10004 : IterateCallCommande(   10 );
    10005 : IterateCallCommande(   20 );
    10006 : IterateCallCommande(   50 );
    10007 : IterateCallCommande(  100 );
    10008 : IterateCallCommande(  200 );
    10009 : IterateCallCommande(  500 );
    {descente remontée}
    10011 : Up(1);
    10012 : Up(2);
    10013 : Up(3);
    10014 : Up(4);
    10015 : Up(5);
    10021 : Down(1);
    10022 : Down(2);
    10023 : Down(3);
    10024 : Down(4);
    10025 : Down(5);
    10031 : Left(1);
    10032 : Left(2);
    10033 : Left(3);
    10034 : Left(4);
    10035 : Left(5);
    10041 : Right(1);
    10042 : Right(2);
    10043 : Right(3);
    10044 : Right(4);
    10045 : Right(5);
    {Zoom de-zoom}
    10046 : MainZoom;
    10047 : MainDezoom;
    {Nord/sud/est/ouest}
    20011 : Ouest(1, 150);
    20012 : Ouest(2, 150);
    20013 : Ouest(3, 150);
    20014 : Ouest(4, 150);
    20015 : Ouest(5, 150);
    20021 : Est(1, 150);
    20022 : Est(2, 150);
    20023 : Est(3, 150);
    20024 : Est(4, 150);
    20025 : Est(5, 150);
    20031 : Pere(1, 150);
    20032 : Pere(2, 150);
    20033 : Pere(3, 150);
    20034 : Pere(4, 150);
    20035 : Pere(5, 150);
    20041 : Fils(1, 150);
    20042 : Fils(2, 150);
    20043 : Fils(3, 150);
    20044 : Fils(4, 150);
    20045 : Fils(5, 150);
    20051 : Nord(1, 150);
    20052 : Nord(2, 150);
    20053 : Nord(3, 150);
    20054 : Nord(4, 150);
    20055 : Nord(5, 150);
    20061 : Sud(1, 150);
    20062 : Sud(2, 150);
    20063 : Sud(3, 150);
    20064 : Sud(4, 150);
    20065 : Sud(5, 150);
    {Pousses multiples}
    20112 : PousseGauche(2, 90);
    20113 : PousseGauche(3, 90);
    20114 : PousseGauche(4, 90);
    20115 : PousseGauche(5, 90);
    20122 : PousseDroite(2, 90);
    20123 : PousseDroite(3, 90);
    20124 : PousseDroite(4, 90);
    20125 : PousseDroite(5, 90);
    20132 : PousseHaut(2, 90);
    20133 : PousseHaut(3, 90);
    20134 : PousseHaut(4, 90);
    20135 : PousseHaut(5, 90);
    20142 : PousseBas(2, 90);
    20143 : PousseBas(3, 90);
    20144 : PousseBas(4, 90);
    20145 : PousseBas(5, 90);
    20152 : PousseAvant(2, 90);
    20153 : PousseAvant(3, 90);
    20154 : PousseAvant(4, 90);
    20155 : PousseAvant(5, 90);
    20162 : PousseArriere(2, 90);
    20163 : PousseArriere(3, 90);
    20164 : PousseArriere(4, 90);
    20165 : PousseArriere(5, 90);
    {NATO Alphabet}
    30001 : AlphaKeyBoard('A');
    30002 : AlphaKeyBoard('B');
    30003 : AlphaKeyBoard('C');
    30004 : AlphaKeyBoard('D');
    30005 : AlphaKeyBoard('E');
    30006 : AlphaKeyBoard('F');
    30007 : AlphaKeyBoard('G');
    30008 : AlphaKeyBoard('H');
    30009 : AlphaKeyBoard('I');
    30010 : AlphaKeyBoard('J');
    30011 : AlphaKeyBoard('K');
    30012 : AlphaKeyBoard('L');
    30013 : AlphaKeyBoard('M');
    30014 : AlphaKeyBoard('N');
    30015 : AlphaKeyBoard('O');
    30016 : AlphaKeyBoard('P');
    30017 : AlphaKeyBoard('Q');
    30018 : AlphaKeyBoard('R');
    30019 : AlphaKeyBoard('S');
    30020 : AlphaKeyBoard('T');
    30021 : AlphaKeyBoard('U');
    30022 : AlphaKeyBoard('V');
    30023 : AlphaKeyBoard('W');
    30024 : AlphaKeyBoard('X');
    30025 : AlphaKeyBoard('Y');
    30026 : AlphaKeyBoard('Z');
    30037 : AlphaKeyBoard(VK_NUMPAD0);
    30027 : AlphaKeyBoard(VK_NUMPAD1);
    30028 : AlphaKeyBoard(VK_NUMPAD2);
    30029 : AlphaKeyBoard(VK_NUMPAD3);
    30030 : AlphaKeyBoard(VK_NUMPAD4);
    30031 : AlphaKeyBoard(VK_NUMPAD5);
    30032 : AlphaKeyBoard(VK_NUMPAD6);
    30033 : AlphaKeyBoard(VK_NUMPAD7);
    30034 : AlphaKeyBoard(VK_NUMPAD8);
    30035 : AlphaKeyBoard(VK_NUMPAD9);
    30036 : AlphaKeyBoard(VK_SPACE);
    30038 : AlphaKeyBoard(VK_BACK);
    30039 : AlphaKeyBoard(VK_LEFT);
    30040 : AlphaKeyBoard(VK_RIGHT);
    30041 : Coller;
    30042 : AlphaKeyBoard(VK_ADD);
    30043 : AlphaKeyBoard(VK_SUBTRACT);
    31000 : if Assigned(ProcExitElite) then ProcExitElite(nil);
    { *** Au sol rev-2 06/2021 for Odyssey}
    200   : KeyStop;
    20000 : KeyFireStop;
    201   : FOdysseyPressedKey.Avancer;  //HumAvance;
    20190 : HumAvance(1,150);
    20191 : HumAvance(1,300);
    20192 : HumAvance(1,600);
    20193 : HumAvance(1,900);
    20194 : HumAvance(1,1500);
    202   : FOdysseyPressedKey.Reculer;  //HumRecule;
    20290 : HumRecule(1,150);
    20291 : HumRecule(1,300);
    20292 : HumRecule(1,600);
    20293 : HumRecule(1,900);
    20294 : HumRecule(1,1500);
    203   : HumSprint;
    204   : HumWalk;
    205   : HumAccroupir;
    206   : HumJump;
    207   : HumPrimaryInteract;
    208   : HumSecondaryInteract;
    209   : HumAimZoom;
    210   : HumThrowGrenade;
    211   : HumMelee;
    212   : HumReload;
    213   : HumSwitchWeapon;
    214   : HumSelectPrimaryWeapon;
    215   : HumSelectSecondaryWeapon;
    216   : HumSelectUtilityWeapon;
    217   : HumSelectNextWeapon;
    218   : HumSelectPreviousWeapon;
    219   : HumHideWeapon;
    220   : HumSelectNextGrenadeType;
    221   : HumSelectPreviousGrenadeType;
    222   : HumToggleFlashlight;
    223   : HumToggleNightVision;
    224   : HumToggleShields;
    225   : HumClearAuthorityLevel;
    226   : HumHealthPack;
    227   : HumBattery;
    228   : HumSelectFragGrenade;
    229   : HumSelectEMPGrenade;
    230   : HumSelectShieldGrenade;
    231   : HumSwitchToRechargeTool;
    232   : HumSwitchToCompAnalyser;
    233   : HumSwitchToSuitTool;
    234   : HumToggleToolMode;
    235   : ;
    236   : HumToggleMissionHelpPanel;  //TODO si nécessaire
    237   : FOdysseyPressedKey.Tirer;   //HumPrimaryFire;
    238   : HumPrimaryFire_1;           //Tirer une cartouche
    239   : HumPrimaryFire_Rafale;      //Tirer une rafale de 3 cartouches
    24000 : HumLateralLeft(1,90);
    24001 : HumLateralLeft(1,150);
    24002 : HumLateralLeft(1,250);
    24003 : HumLateralLeft(1,400);
    24004 : HumLateralLeft(1,600);
    24100 : HumLateralRight(1,90);
    24101 : HumLateralRight(1,150);
    24102 : HumLateralRight(1,250);
    24103 : HumLateralRight(1,400);
    24104 : HumLateralRight(1,600);
    {*** Rotation de yeux plus/moins [1..5] }
    24500 : HumRotateLeft(1,35);
    24501 : HumRotateLeft(1,45);
    24502 : HumRotateLeft(1,60);
    24503 : HumRotateLeft(1,75);
    24504 : HumRotateLeft(1,90);
    24600 : HumRotateRight(1,35);
    24601 : HumRotateRight(1,45);
    24602 : HumRotateRight(1,60);
    24603 : HumRotateRight(1,75);
    24604 : HumRotateRight(1,90);
    {*** Regard haut bas hausse/baisse [1..5] }
    25000 : HumPitchUp(1,35);
    25001 : HumPitchUp(1,45);
    25002 : HumPitchUp(1,60);
    25003 : HumPitchUp(1,75);
    25004 : HumPitchUp(1,90);
    25100 : HumPitchDown(1,35);
    25101 : HumPitchDown(1,45);
    25102 : HumPitchDown(1,60);
    25103 : HumPitchDown(1,75);
    25104 : HumPitchDown(1,90);
    {Au sol mode rev-2 06/2021 for Odyssey}
    290   : HumGalaxyMapOpen;
    291   : HumSystemMapOpen;
    292   : HumFocusCommsPanel;
    293   : HumQuickCommsPanel;
    294   : HumOpenAccessPanel;
    295   : HumConflictContextualUI;
    {MACRO au sol rev-2 06/2021 for Odyssey }
    300   : ThrowGrenadeFragmentation;
    301   : ThrowGrenadeEMP;
    302   : ThrowGrenadeBouclier;
    303   : DoAfterInteract;       //Stop Tobii + Interact
    304   : AimZoomRafale;
    305   : ForwardWithCaution;
    306   : BackwardWithCaution;
    307   : StandInFront;
    308   : OpenTheDoor;
    309   : ShipBoarding;          //Stop Tobii + Interact
    310   : SniperMode;
    311   : Engager;
    312   : StandInBack;
    313   : CashPrimes;            //Stop Tobii + Interact
    314   : EnrollmentManage;      //Stop Tobii + Interact
    315   : ShowStock;             //Stop Tobii + Interact
    316   : ShowConsumable;        //Stop Tobii + Interact
    317   : EngagerEx;
    {*** In game, Tobii manager }
    400   : TobiiStart;            
    401   : TobiiPause;
    402   : TobiiConfigCombat;
    403   : TobiiConfigStation;
    404   : TobiiConfigExplo;
    405   : TobiiConfigCutter;
    406   : TobiiConfigDivers1;
    407   : TobiiCongigDivers2;
    408   : TobiiMoreSensibility;
    409   : TobiiLessSensibility;
    410   : TobiiMorePrecision;
    411   : TobiiLessPrecision;
    412   : TobiiSaveDiver1;
    413   : TobiiSaveDiver2;
    414   : TobiiLoad;
    415   : TobiiStop;
    416   : TobiiMenuMode;
//    417   : TobiiConfig;           //Switch 1100
//    418   : TobiiHideConfig;       //Switch 1101
    {*** Vue subjective}
    500   : HeadLockReset;
    501   : HeadLockUp;
    502   : HeadLockDown;
    503   : HeadLockLeft;
    504   : HeadLockRight;
    505   : HeadLockTools;           //vue sur les instruments
    506   : HeadLockExit;
    507   : HeadLockTarget;
    508   : HeadLockShield;
  end;
  if not Again and not IsAgainCommand(indexCmd) then LastCmd := indexCmd
end; {CallCommande}

function TCustomEliteManager.CanLongRepeat: Boolean;
begin
  Result := IndexStr( IntToStr(LastCmd),
              ['29', '30', '31', '32', '35', '36', '37', '38'] ) > -1
end;

procedure TCustomEliteManager.CibleOfAilier(index: Byte);
begin
  case index of
    1  : FKeyInventory.KeyTrigger_( 'TargetWingman0' , WITH_KEYUP);
    2  : FKeyInventory.KeyTrigger_( 'TargetWingman1' , WITH_KEYUP);
    else FKeyInventory.KeyTrigger_( 'TargetWingman2' , WITH_KEYUP);
  end;
  FKeyInventory.KeyTrigger_( 'SelectTargetsTarget' ,   WITH_KEYUP)
end;

constructor TCustomEliteManager.Create;
begin
  inherited Create;
  FKeyInventory := nil;
  FTagStack     := TStringList.Create;
  InitializeMutex;
  LastCmd       := 0;
end;

destructor TCustomEliteManager.Destroy;
begin
  FinalizeMutex;
  FTagStack.Free;
  inherited
end;

procedure TCustomEliteManager.DoOnStack(Sender: TObject);
begin
  with FTagStack do
  try
    while Count > 0 do begin
      CallCommande( Strings[ Pred(Count) ] );
      Delete( Pred(Count) )
    end
  finally
  end
end;

procedure TCustomEliteManager.DoWithTag(Sender: TObject);
var
  Buffer : string;
  ASt    : string;

  procedure Stack(const Value: string); begin
    if Pos('undefined', Value) = 0 then FTagStack.Insert(0, Value )
  end;

begin
  FTagStack.Clear;
  ASt := FTags;
  while Pos('.', ASt) > 0 do begin
    Buffer := GetBeforStr(ASt, '.');
    ASt    := GetAfterStr(ASt, '.');
    Stack( Buffer )
  end;
  if Ast <> EmptyStr then Stack( Ast )
end; {DoWithTag}

procedure TCustomEliteManager.FinalizeMutex;
begin
  CloseHandle( EliteMutex )
end;

procedure TCustomEliteManager.FSD;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'HyperSuperCombination', WITH_KEYUP)
end;

procedure TCustomEliteManager.FullSystem;
begin
  PipeReset;
  PipeSystem;
  PipeSystem
end;

procedure TCustomEliteManager.FullArme;
begin
  PipeReset;
  PipeArmes;
  PipeArmes
end;

procedure TCustomEliteManager.FullMoteur;
begin
  PipeReset;
  PipeMoteur;
  PipeMoteur
end;

procedure TCustomEliteManager.InitializeMutex;
begin
  EliteMutex := CreateMutex(nil, False, 'StackElite');
  if EliteMutex = 0 then RaiseLastOSError
end;

function TCustomEliteManager.IsAgainCommand(const Cmd: Integer): Boolean;
begin
  Result := IndexStr( IntToStr(Cmd), ['9999', '10000', '10001', '10002', '10003',
     '10004', '10005', '10006', '10007', '10008', '10009',
     {TODO traiter à part}
     '9990', '9991', '9992'] ) > -1
end;

procedure TCustomEliteManager.IterateCallCommande(Count: Integer);
var
  i : Integer;
  M : Integer;
begin
  M := MaxRepeat;
  for i := 1 to Count do begin
    Application.ProcessMessages;
    CallCommande(IntToStr( LastCmd ), True);
    if (i >= M) and not CanLongRepeat then Break
  end
end;

procedure TCustomEliteManager.NavigationOnAilier(index: Byte);
begin
  case index of
    1  : FKeyInventory.KeyTrigger_( 'TargetWingman0' , WITH_KEYUP);
    2  : FKeyInventory.KeyTrigger_( 'TargetWingman1' , WITH_KEYUP);
    else FKeyInventory.KeyTrigger_( 'TargetWingman2' , WITH_KEYUP);
  end;
  FKeyInventory.KeyTrigger_( 'WingNavLock',           WITH_KEYUP)
end;

procedure TCustomEliteManager.PRL;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'Hyperspace',  WITH_KEYUP)
end;

procedure TCustomEliteManager.ProcessOnStack;
begin
  ProtectedCode( DoOnStack )
end;

procedure TCustomEliteManager.ProtectedCode(Method: TNotifyEvent);
begin
  if Assigned(Method) then
    if WaitForSingleObject(EliteMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Method(nil);
    finally
      ReleaseMutex( EliteMutex )
    end
end;

procedure TCustomEliteManager.SetKeyInventory(const Value: TKeyInventory);
begin
  FKeyInventory := Value
end;

procedure TCustomEliteManager.SetTags(const Value: string);  {WARNING PROD}
begin
  { --- Elite dangerous processus doit être en cours de fonctionnement }
  if KeyReadBoolean(AppKey, 'EliteLaunched', False) then begin
    FTags := Value;
    AssignTagsToStack;
    ProcessOnStack
  end else
  if (Pos('31000', Value) > 0) and Assigned(ProcExitElite) then ProcExitElite(nil)
end;

procedure TCustomEliteManager.SuperNavigation;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'Supercruise', WITH_KEYUP)
end;

function TCustomEliteManager.GetMaxRepeat: Integer;
begin
  Result := KeyReadInt(ParamKey, 'MaxRepeat', 20)
end;

procedure TCustomEliteManager.SetMaxRepeat(const Value: Integer);
begin
  KeyWrite(ParamKey, 'MaxRepeat', Value)
end;

procedure TCustomEliteManager.Camera_ShipShow;
begin
  FKeyInventory.KeyTrigger_( 'PhotoCameraToggle', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD',  WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_VRSShow;
begin
  FKeyInventory.KeyTrigger_( 'PhotoCameraToggle_Buggy', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD',        WITH_KEYUP)
end;

procedure TCustomEliteManager.Up(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Down', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Down', tms)
end;

procedure TCustomEliteManager.Down(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Up', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Up', tms)
end;

procedure TCustomEliteManager.Left(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Left', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Left', tms)
end;

procedure TCustomEliteManager.Right(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Right', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Right', tms)
end;

procedure TCustomEliteManager.Nord(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchUpButton';
    gt_systemmap : ASt := 'RollLeftButton'; { --> Père value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchUpButton_Landing'
        else ASt := 'PitchUpButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Sud(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchDownButton';
    gt_systemmap : ASt := 'RollRightButton'; { --> Fils value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchDownButton_Landing'
        else ASt := 'PitchDownButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Est(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawRightButton_Landing'
        else ASt := 'YawRightButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Ouest(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawLeftButton_Landing'
        else ASt := 'YawLeftButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Pere(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollLeftButton_Landing'
        else ASt := 'RollLeftButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Fils(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollRightButton_Landing'
        else ASt := 'RollRightButton'
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
       else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseGauche(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'LeftThrustButton_Landing'
    else ASt := 'LeftThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseDroite(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'RightThrustButton_Landing'
    else ASt := 'RightThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseHaut(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'UpThrustButton_Landing'
    else ASt := 'UpThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseBas(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'DownThrustButton_Landing'
    else ASt := 'DownThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseAvant(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'ForwardThrustButton_Landing'
    else ASt := 'ForwardThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseArriere(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'BackwardThrustButton_Landing'
    else ASt := 'BackwardThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PipeMoteur;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseEnginesPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeSystem;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseSystemsPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeArmes;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseWeaponsPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeReset;
begin
  FKeyInventory.KeyTrigger_( 'ResetPowerDistribution' , WITH_KEYUP)
end;

procedure TCustomEliteManager.CombatOffensif;
begin
  PipeReset;
  PipeArmes; PipeArmes; PipeArmes;
  PipeMoteur;
  PipeArmes;
  PipeSystem
end;

procedure TCustomEliteManager.CombatDefensif;
begin
  PipeReset;
  PipeSystem;
  PipeArmes
end;

procedure TCustomEliteManager.ModeFuite;
begin
  PipeReset;
  PipeSystem;
  PipeMoteur;
  PipeSystem;
  PipeMoteur
end;

procedure TCustomEliteManager.ModeDefensif;
begin
  PipeReset;
  PipeSystem; PipeSystem;
  PipeMoteur;
  PipeSystem;
  PipeArmes; PipeArmes;
  PipeSystem
end;

procedure TCustomEliteManager.PousseeHaut(const sTime: Cardinal);
begin
  FKeyInventory.KeyTrigger_( 'UpThrustButton' , sTime)
end;

procedure TCustomEliteManager.TrainAtterrissage;
begin
  FKeyInventory.KeyTrigger_( 'LandingGearToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.Decollage;
begin
  PousseeHaut(1000);
  TrainAtterrissage;
  PousseeHaut(2000);
  PousseeHaut(1000);
  PousseeHaut(500)
end;

procedure TCustomEliteManager.InverserPropulsion;
begin
  FKeyInventory.KeyTrigger_( 'ToggleReverseThrottleInput', WITH_KEYUP)
end;

procedure TCustomEliteManager.Acceleration;
begin
  case EliteStatus.InSrv of
    True : PropulsionAcceleration(200);
    else FKeyInventory.KeyTrigger_( 'ForwardKey', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.Deceleration;
begin
  case EliteStatus.InSrv of
    True : PropulsionDeceleration(200);
    else FKeyInventory.KeyTrigger_( 'BackwardKey', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.SpeedM100;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus100', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM75;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus75', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM50;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus50', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM25;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus25', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedNull;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedZero', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed25;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed25', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed50;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed50', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed75;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed75', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed100;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP)
end;

procedure TCustomEliteManager.AssistanceDeVol(const FAO: TFADOType);
begin
  case FAO of
     ft_on  : if not EliteStatus.FlightAssistOff then Exit;
     ft_off : if EliteStatus.FlightAssistOff then Exit;
  end;
  FKeyInventory.KeyTrigger_( 'ToggleFlightAssist', WITH_KEYUP)
end;

procedure TCustomEliteManager.Boost;
begin
  FKeyInventory.KeyTrigger_( 'UseBoostJuice', WITH_KEYUP)
end;

procedure TCustomEliteManager.FixRotation;
begin
  FKeyInventory.KeyTrigger_( 'DisableRotationCorrectToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.OrbitDisplay;
begin
  FKeyInventory.KeyTrigger_( 'OrbitLinesToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.Target12h;
begin
  FKeyInventory.KeyTrigger_( 'SelectTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextTarget;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorTarget;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenacePrincipale;
begin
  FKeyInventory.KeyTrigger_( 'SelectHighestThreat', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenaceSuivante;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextHostileTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenacePrecedente;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousHostileTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier1;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman0', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier2;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman1', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier3;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman2', WITH_KEYUP)
end;

procedure TCustomEliteManager.AilierTarget;
begin
  FKeyInventory.KeyTrigger_( 'SelectTargetsTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextSubSystem;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextSubsystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorSubSystem;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousSubsystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextRoute;
begin
  FKeyInventory.KeyTrigger_( 'TargetNextRouteSystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.AilierNavLock;
begin
  FKeyInventory.KeyTrigger_( 'WingNavLock', WITH_KEYUP)
end;

procedure TCustomEliteManager.PrimaryFire(tms: Integer);

  procedure ShipFire; begin
    if tms < 1 then FKeyInventory.KeyTrigger_( 'PrimaryFire', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'PrimaryFire', tms)
  end;

begin
  if EliteStatus.IsOnOdyssey then HumPrimaryFire
   else case EliteStatus.InSrv of
    True : VRSPrimaryFire else ShipFire    //TODO without_keyup
   end
end; {PrimaryFire}

procedure TCustomEliteManager.SecondaryFire(tms: Integer);

  procedure ShipFire; begin
    if tms < 1 then FKeyInventory.KeyTrigger_( 'SecondaryFire', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'SecondaryFire', tms)
  end;

begin
  case EliteStatus.InSrv of
    True : VRSSecondaryFire else ShipFire
  end
end; {SecondaryFire}

procedure TCustomEliteManager.NextArmGroup;
begin
  if EliteStatus.IsOnOdyssey then HumSelectNextWeapon
    else FKeyInventory.KeyTrigger_( 'CycleFireGroupNext', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorArmGroup;
begin
  if EliteStatus.IsOnOdyssey then HumSelectPreviousWeapon
    else FKeyInventory.KeyTrigger_( 'CycleFireGroupPrevious', WITH_KEYUP)
end;

procedure TCustomEliteManager.HardPoint;
begin
  FKeyInventory.KeyTrigger_( 'DeployHardpointToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.StealthyMode;
begin
  FKeyInventory.KeyTrigger_( 'ToggleButtonUpInput', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeatSink;
begin
  FKeyInventory.KeyTrigger_( 'DeployHeatSink', WITH_KEYUP)
end;

procedure TCustomEliteManager.LandingGear(const Mode: TLandingGearType);
begin
  case Mode of
    lgt_open  : if EliteStatus.LandinGearDown then Exit;
    lgt_close : if not EliteStatus.LandinGearDown then Exit;
  end;
  FKeyInventory.KeyTrigger_( 'LandingGearToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.CargoEject;
begin
  FKeyInventory.KeyTrigger_( 'EjectAllCargo', WITH_KEYUP)
end;

procedure TCustomEliteManager.CargoScoop;
begin
  FKeyInventory.KeyTrigger_( 'ToggleCargoScoop', WITH_KEYUP)
end;

procedure TCustomEliteManager.RadarRangeDec;
begin
  FKeyInventory.KeyTrigger_( 'RadarDecreaseRange', WITH_KEYUP)
end;

procedure TCustomEliteManager.RadarRangeInc;
begin
  FKeyInventory.KeyTrigger_( 'RadarIncreaseRange', WITH_KEYUP)
end;

procedure TCustomEliteManager.Searchlight;
begin
  FKeyInventory.KeyTrigger_( 'ShipSpotLightToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.ShieldCell;
begin
//  if not EliteStatus.ShieldsUp then
  if EliteStatus.IsOnOdyssey then HumBattery
    else FKeyInventory.KeyTrigger_( 'UseShieldCell', WITH_KEYUP)
end;

procedure TCustomEliteManager.ChaffLauncher;
begin
  FKeyInventory.KeyTrigger_( 'FireChaffLauncher', WITH_KEYUP)
end;

procedure TCustomEliteManager.ChargeECM;
begin
  FKeyInventory.KeyTrigger_( 'ChargeECM', 5000)
end;

procedure TCustomEliteManager.WeaponColour;
begin
  FKeyInventory.KeyTrigger_( 'WeaponColourToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.EngineColour;
begin
  FKeyInventory.KeyTrigger_( 'EngineColourToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.NightVision;
begin
  if EliteStatus.IsOnOdyssey then HumToggleNightVision
    else FKeyInventory.KeyTrigger_( 'NightVisionToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.LeftPanel;
begin
  TobiiPause;
  FKeyInventory.KeyTrigger_( 'FocusLeftPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.CommsPanel;
begin
  if EliteStatus.IsOnOdyssey then HumFocusCommsPanel
    else begin
      TobiiPause;
      FKeyInventory.KeyTrigger_( 'FocusCommsPanel', WITH_KEYUP)
    end;
end;

procedure TCustomEliteManager.QuickCommsPanel;
begin
  if EliteStatus.IsOnOdyssey then HumQuickCommsPanel
    else begin
      TobiiPause;
      FKeyInventory.KeyTrigger_( 'QuickCommsPanel', WITH_KEYUP)
    end
end;

procedure TCustomEliteManager.RadarPanel;
begin
  TobiiPause;
  FKeyInventory.KeyTrigger_( 'FocusRadarPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.RightPanel;
begin
  TobiiPause;
  FKeyInventory.KeyTrigger_( 'FocusRightPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.MapGalaxy;
begin
  TobiiPause;
  if EliteStatus.IsOnOdyssey then HumGalaxyMapOpen
    else FKeyInventory.KeyTrigger_( 'GalaxyMapOpen', WITH_KEYUP)
end;

procedure TCustomEliteManager.MapSystem;
begin
  TobiiPause;
  if EliteStatus.IsOnOdyssey then HumSystemMapOpen
    else FKeyInventory.KeyTrigger_( 'SystemMapOpen', WITH_KEYUP)
end;

procedure TCustomEliteManager.Pause;
begin
  FKeyInventory.KeyTrigger_( 'Pause', WITH_KEYUP);
  { *** Pause the tobii to avoid deleterious effects }
  Delay(100);
  StopMovAndFire;
  TobiiPause;
end;

procedure TCustomEliteManager.FriendBoard;
begin
  FKeyInventory.KeyTrigger_( 'FriendsMenu', WITH_KEYUP)
end;

procedure TCustomEliteManager.Codex;
begin
  FKeyInventory.KeyTrigger_( 'OpenCodexGoToDiscovery', WITH_KEYUP)
end;

procedure TCustomEliteManager.CockpitMode(const Mode: TCockpitModeType);
begin
  case Mode of
    cmt_combat      : if not EliteStatus.HudInanalysisMode then Exit;
    cmt_exploration : if EliteStatus.HudInanalysisMode then Exit;
  end;
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'PlayerHUDModeToggle_Buggy', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'PlayerHUDModeToggle', WITH_KEYUP)
  end
end; {CockpitMode}

procedure TCustomEliteManager.ModeACS;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSEnter', WITH_KEYUP)
end;

procedure TCustomEliteManager.UISelect;
begin
  FKeyInventory.KeyTrigger_( 'UI_Select', WITH_KEYUP)
end;

procedure TCustomEliteManager.UIBack;
begin
  FKeyInventory.KeyTrigger_( 'UI_Back', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextSheet(index: Byte);
var
  i : Integer;
begin
  for i := 1 to index do FKeyInventory.KeyTrigger_('CycleNextPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorSheet(index: Byte);
var
  i : Integer;
begin
  for i := 1 to index do FKeyInventory.KeyTrigger_('CyclePreviousPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextPage;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextPage', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorPage;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousPage', WITH_KEYUP)
end;

procedure TCustomEliteManager.DriveAssist;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleDriveAssist', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.TurnLeft(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SteerLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SteerLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurnRight(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SteerRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SteerRightButton', tms);
    end
  end;
end;

procedure TCustomEliteManager.VerticalThruster(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'VerticalThrustersButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'VerticalThrustersButton', tms);
    end
  end
end;

procedure TCustomEliteManager.VRSPrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyPrimaryFireButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BuggyPrimaryFireButton', tms)
end;

procedure TCustomEliteManager.VRSSecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggySecondaryFireButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BuggySecondaryFireButton', tms)
end;

procedure TCustomEliteManager.VRSAutoBreak;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'AutoBreakBuggyButton', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.VRSSearchlight;
begin
  FKeyInventory.KeyTrigger_( 'HeadlightsBuggyButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.VRSTurret;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleBuggyTurretButton', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.VRSNextArmGroup;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyCycleFireGroupNext', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSPriorArmGroup;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyCycleFireGroupPrevious', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSTarget12h;
begin
  FKeyInventory.KeyTrigger_( 'SelectTarget_Buggy', WITH_KEYUP)
end;

procedure TCustomEliteManager.TurretYawLeft(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretYawLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretYawLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretYawRight(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretYawRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretYawRightButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretPitchUp(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretPitchUpButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretPitchUpButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretPitchDown(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretPitchDownButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretPitchDownButton', tms)
    end
  end
end;

procedure TCustomEliteManager.PropulsionReverse;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyToggleReverseThrottleInput', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.PropulsionAcceleration(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'IncreaseSpeedButtonMax', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'IncreaseSpeedButtonMax', tms)
end;

procedure TCustomEliteManager.PropulsionDeceleration(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'DecreaseSpeedButtonMax', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'DecreaseSpeedButtonMax', tms)
end;

procedure TCustomEliteManager.VRSCargoScoop;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleCargoScoop_Buggy', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSCargoEject;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'EjectAllCargo_Buggy', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ShipDismissRecall;
begin
  if EliteStatus.InSrv or EliteStatus.IsOnOdyssey then
    FKeyInventory.KeyTrigger_( 'RecallDismissShip', WITH_KEYUP)
end;

procedure TCustomEliteManager.RequestDock;
begin
  FKeyInventory.KeyTrigger_( 'OrderRequestDock', WITH_KEYUP)
end;

procedure TCustomEliteManager.DefensiveBehaviour;
begin
  FKeyInventory.KeyTrigger_( 'OrderDefensiveBehaviour', WITH_KEYUP)
end;

procedure TCustomEliteManager.AggressiveBehaviour;
begin
  FKeyInventory.KeyTrigger_( 'OrderAggressiveBehaviour', WITH_KEYUP)
end;

procedure TCustomEliteManager.FocusTarget;
begin
  FKeyInventory.KeyTrigger_( 'OrderFocusTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.HoldFire;
begin
  FKeyInventory.KeyTrigger_( 'OrderHoldFire', WITH_KEYUP)
end;

procedure TCustomEliteManager.HoldPosition;
begin
  FKeyInventory.KeyTrigger_( 'OrderHoldPosition', WITH_KEYUP)
end;

procedure TCustomEliteManager.Follow;
begin
  FKeyInventory.KeyTrigger_( 'OrderFollow', WITH_KEYUP)
end;

procedure TCustomEliteManager.OpenOrders;
begin
  FKeyInventory.KeyTrigger_( 'OpenOrders', WITH_KEYUP)
end;

procedure TCustomEliteManager.ACSCameraPitchInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchIncreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchIncreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraPitchDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchDecreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchDecreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraYawInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawIncreaseButton', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawIncreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraYawDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawDecreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawDecreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomIn(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomIn', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomIn', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomOut(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomOut', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomOut', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomInMini(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomIn', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomIn', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomOutMini(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomOut', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomOut', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSRadioInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Increase', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Increase', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSRadioDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Decrease', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Decrease', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSAnalyse(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSDiscoveryScan', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSDiscoveryScan', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSClose;
begin
  case EliteStatus.GuiValue of
   gt_fssmode : FKeyInventory.KeyTrigger_( 'ExplorationFSSQuit', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ACSGetTarget;
begin
  case EliteStatus.GuiValue of
   gt_fssmode : FKeyInventory.KeyTrigger_( 'ExplorationFSSTarget', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ACSHelp;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSShowHelp', WITH_KEYUP)
end;

procedure TCustomEliteManager.DSDViewChange;
begin
  case EliteStatus.GuiValue of
    gt_saamode : FKeyInventory.KeyTrigger_( 'ExplorationSAAChangeScannedAreaViewToggle', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.DSDClose;
begin
  case EliteStatus.GuiValue of
    gt_saamode : FKeyInventory.KeyTrigger_( 'ExplorationSAAExitThirdPerson', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.DSDYawLeft(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDYawRight(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawRightButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDPitchUp(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchUpButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchUpButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDPitchDown(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchDownButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchDownButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDZoomOut(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovOutButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovOutButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDZoomIn(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovInButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovInButton', tms)
    end
  end
end;

procedure TCustomEliteManager.CamPitchUp(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamPitchUp', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamPitchUp', tms)
end;

procedure TCustomEliteManager.CamPitchDown(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamPitchDown', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamPitchDown', tms)
end;

procedure TCustomEliteManager.CamYawLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamYawLeft', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamYawLeft', tms)
end;

procedure TCustomEliteManager.CamYawRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamYawRight', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamYawRight', tms)
end;

procedure TCustomEliteManager.CamZoomIn(tms: Integer);
begin
  //rev-2 : Note TEST AVEC ODYSSEY Puisque la fonction échoue maintenant
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamZoomIn', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamZoomIn', tms);
//  case EliteStatus.GuiValue of
//    gt_galaxymap, gt_systemmap : begin
//      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamZoomIn', WITH_KEYUP)
//        else FKeyInventory.KeyTrigger_( 'CamZoomIn', tms)
//    end
//  end
end;

procedure TCustomEliteManager.CamZoomOut(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'CamZoomOut', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'CamZoomOut', tms);
end;

procedure TCustomEliteManager.GalaxyMapHome;
begin
  FKeyInventory.KeyTrigger_( 'GalaxyMapHome', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Next;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraScrollLeft', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Prior;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraScrollRight', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_One;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraOne', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Two;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraTwo', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Three;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraThree', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Four;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraFour', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Five;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraFive', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Six;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraSix', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Seven;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraSeven', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Eight;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraEight', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Nine;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraNine', WITH_KEYUP)
end;

procedure TCustomEliteManager.FreeCamera_Close;
begin
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD', WITH_KEYUP)
end;

procedure TCustomEliteManager.LandingYawLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'YawLeftButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'YawLeftButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingYawRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'YawRightButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'YawRightButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingPitchUp(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'PitchUpButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'PitchUpButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingPitchDown(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'PitchDownButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'PitchDownButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRollLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RollLeftButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RollLeftButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRollRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RollRightButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RollRightButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingLeftThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'LeftThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'LeftThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRightThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RightThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RightThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingUpThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'UpThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'UpThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingDownThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'DownThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'DownThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingForwardThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'ForwardThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'ForwardThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingBackwardThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BackwardThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BackwardThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.Crew_CokpitNext;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewCockpitUICycleForward', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_SecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryFire', tms)
end;

procedure TCustomEliteManager.Crew_ToolsSecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryUtilityFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryUtilityFire', tms)
end;

procedure TCustomEliteManager.Crew_CokpitPrior;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewCockpitUICycleBackward', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_PitchDown(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchDownButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchDownButton', tms)
end;

procedure TCustomEliteManager.Crew_YawLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawLeftButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawLeftButton', tms)
end;

procedure TCustomEliteManager.Crew_PitchUp(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchUpButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchUpButton', tms)
end;

procedure TCustomEliteManager.Crew_YawRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawRightButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawRightButton', tms)
end;

procedure TCustomEliteManager.Crew_ZoomOut(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovOutButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovOutButton', tms)
end;

procedure TCustomEliteManager.Crew_PrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryFire', tms)
end;

procedure TCustomEliteManager.Crew_ToolsPrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryUtilityFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryUtilityFire', tms)
end;

procedure TCustomEliteManager.Crew_SwitchMode;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewToggleMode', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_ZoomIn(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovInButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovInButton', tms)
end;

procedure TCustomEliteManager.MainDezoom;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap     : CamZoomOut(200);
    gt_fssmode       : ACSZoomOut(100);
    gt_saamode       : DSDZoomOut(150);
  end
end;

procedure TCustomEliteManager.MainZoom;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap     : CamZoomIn(200);
    gt_fssmode       : ACSZoomIn(100);
    gt_saamode       : DSDZoomIn(150);
  end
end;

procedure TCustomEliteManager.SendChar(const Car: Char);
begin
  SendKey(Ord(Car), 150, [])
end;

procedure TCustomEliteManager.SendChar(VKCode: SmallInt);
begin
  keybd_event(VKCode, MapVirtualKey(Ord(VKCode), 0), 0, 0);
  WaitForKey( 150 );
  keybd_event(Ord(VKCode), MapVirtualKey(Ord(VKCode), 0), KEYEVENTF_KEYUP, 0)
end;

procedure TCustomEliteManager.AlphaKeyBoard(const Car: Char);
begin
  { --- Ne peut être simulé que si l'on se trouve dans ::
         * La carte de la galaxie
         * le panneaau des amis (comment le retrouver ?)
         * le pannneai de communication}
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendChar(Car);
  end
end;

procedure TCustomEliteManager.AlphaKeyBoard(VKCode: SmallInt);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendChar(VKCode);
  end
end;

procedure TCustomEliteManager.AlphaKeyBoard(const Car: Char;
  Specials: TSpecials);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendKey(Ord(Car), 150, Specials);
  end
end;

procedure TCustomEliteManager.Coller;
begin
  AlphaKeyBoard('V', [ss_lctrl])
end;

procedure TCustomEliteManager.CloseCarte;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : UIBack
  end
end;

procedure TCustomEliteManager.HumAvance(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidForwardButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidForwardButton', tms)
  end;
end;

procedure TCustomEliteManager.HumRecule(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidBackwardButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidBackwardButton', tms)
  end;
end;

procedure TCustomEliteManager.HumLateralLeft(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidStrafeLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidStrafeLeftButton', tms)
  end;
end;

procedure TCustomEliteManager.HumLateralRight(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidStrafeRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidStrafeRightButton', tms)
  end
end;

procedure TCustomEliteManager.HumRotateLeft(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidRotateLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidRotateLeftButton', tms)
  end
end;

procedure TCustomEliteManager.HumRotateRight(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidRotateRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidRotateRightButton', tms)
  end
end;

procedure TCustomEliteManager.HumPitchUp(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidPitchUpButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidPitchUpButton', tms)
  end;
end;

procedure TCustomEliteManager.HumPitchDown(index: Byte; tms: Integer);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidPitchDownButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidPitchDownButton', tms)
  end
end;

procedure TCustomEliteManager.HumSprint;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSprintButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumWalk;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidWalkButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumAccroupir;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidCrouchButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumJump;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidJumpButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumPrimaryInteract;
begin
  if EliteStatus.IsOnOdyssey then begin
    FOdysseyPressedKey.Clear;
    FKeyInventory.KeyTrigger_( 'HumanoidPrimaryInteractButton', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumSecondaryInteract;
begin
  if EliteStatus.IsOnOdyssey then begin
    fOdysseyPressedKey.Clear;
    FKeyInventory.KeyTrigger_( 'HumanoidSecondaryInteractButton', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumItemWheel;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelXAxis;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_XAxis', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelXLeft;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_XLeft', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelXRight;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_XRight', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelYAxis;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_YAxis', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelYUp;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_YUp', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumItemWheelYDown;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidItemWheelButton_YDown', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumPrimaryFire(index: Byte; tms: Integer = 0);
var
  i : Integer;
begin
  if EliteStatus.IsOnOdyssey then begin
    for i := 1 to index do
      if tms < 1 then FKeyInventory.KeyTrigger_( 'HumanoidPrimaryFireButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'HumanoidPrimaryFireButton', tms)
  end;
end;

procedure TCustomEliteManager.HumAimZoom;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidZoomButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumThrowGrenade;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidThrowGrenadeButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumMelee;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidMeleeButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumReload;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidReloadButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSwitchWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSwitchWeapon', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectPrimaryWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectPrimaryWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectSecondaryWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectSecondaryWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectUtilityWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectUtilityWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectNextWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectNextWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectPreviousWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectPreviousWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumHideWeapon;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidHideWeaponButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectNextGrenadeType;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectNextGrenadeTypeButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectPreviousGrenadeType;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectPreviousGrenadeTypeButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumToggleFlashlight;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidToggleFlashlightButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumToggleNightVision;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidToggleNightVisionButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumToggleShields;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidToggleShieldsButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumClearAuthorityLevel;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidClearAuthorityLevel', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumHealthPack;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidHealthPack', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumBattery;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidBattery', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectFragGrenade;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectFragGrenade', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectEMPGrenade;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectEMPGrenade', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSelectShieldGrenade;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSelectShieldGrenade', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSwitchToRechargeTool;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSwitchToRechargeTool', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSwitchToCompAnalyser;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSwitchToCompAnalyser', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumSwitchToSuitTool;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidSwitchToSuitTool', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumToggleToolMode;
begin
  FKeyInventory.KeyTrigger_( 'HumanoidToggleToolModeButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumToggleMissionHelpPanel;
begin
  if EliteStatus.IsOnOdyssey then
    FKeyInventory.KeyTrigger_( 'HumanoidToggleMissionHelpPanelButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.HumGalaxyMapOpen;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'GalaxyMapOpen_Humanoid', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumSystemMapOpen;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'SystemMapOpen_Humanoid', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumFocusCommsPanel;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'FocusCommsPanel_Humanoid', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumQuickCommsPanel;
begin
  Exit;
  { *** Ne plus l'ouvrir car déclenchement intempestif }
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'QuickCommsPanel_Humanoid', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumOpenAccessPanel;
begin
  //presser 1000ms pour ouvrir
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'HumanoidOpenAccessPanelButton', 1000);
  end;
end;

procedure TCustomEliteManager.HumConflictContextualUI;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    FKeyInventory.KeyTrigger_( 'HumanoidConflictContextualUIButton', WITH_KEYUP);
  end;
end;

procedure TCustomEliteManager.KeyStop;
begin
  FOdysseyPressedKey.StopMov;
  //Relacher "physiquement" la combinaison de touches courante
  KeyMessageSender.DoKeyUp(True);
end;

procedure TCustomEliteManager.HumAvance;
begin
  if EliteStatus.IsOnFoot then begin
    //Ne relache pas la combinaison de touches
    KeyWrite(AppKey, 'OdysseyMvt', 'avance');
    FKeyInventory.KeyTrigger_( 'HumanoidForwardButton', WITHOUT_KEYUP);
  end;
end;

procedure TCustomEliteManager.HumRecule;
begin
  if EliteStatus.IsOnFoot then begin
    //Ne relache pas la combinaison de touches
    KeyWrite(AppKey, 'OdysseyMvt', 'recule');
    FKeyInventory.KeyTrigger_( 'HumanoidBackwardButton', WITHOUT_KEYUP);
  end;
end;

procedure TCustomEliteManager.Nord;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchUpButton';
    gt_systemmap : ASt := 'RollLeftButton'; { --> Père value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchUpButton_Landing'
        else ASt := 'PitchUpButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.Sud;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchDownButton';
    gt_systemmap : ASt := 'RollRightButton'; { --> Fils value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchDownButton_Landing'
        else ASt := 'PitchDownButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.Est;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawRightButton_Landing'
        else ASt := 'YawRightButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt, WITHOUT_KEYUP);
  FOdysseyPressedKey.FClockMov := GetTickCount;
end;

procedure TCustomEliteManager.Ouest;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawLeftButton_Landing'
        else ASt := 'YawLeftButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt, WITHOUT_KEYUP);
  FOdysseyPressedKey.FClockMov := GetTickCount;
end;

procedure TCustomEliteManager.Pere;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollLeftButton_Landing'
        else ASt := 'RollLeftButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt, WITHOUT_KEYUP);
  FOdysseyPressedKey.FClockMov := GetTickCount;
end;

procedure TCustomEliteManager.Fils;
var
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollRightButton_Landing'
        else ASt := 'RollRightButton'
  end;
  //Ne relache pas la combinaison de touches
  FKeyInventory.KeyTrigger_(ASt , WITHOUT_KEYUP);
  FOdysseyPressedKey.FClockMov := GetTickCount;
end;

procedure TCustomEliteManager.HumPrimaryFire;
begin
  if EliteStatus.IsOnOdyssey then begin
    //Ne relache pas la combinaison de touches - tir continu
    KeyWrite(AppKey, 'OdysseyFire', 'Ok');
    FKeyInventory.KeyTrigger_( 'HumanoidPrimaryFireButton', WITHOUT_KEYUP);
  end;
end;


procedure TCustomEliteManager.HumPrimaryFire_1;
begin
  FOdysseyPressedKey.StopFire;
  //Tirer une cartouche
  if EliteStatus.IsOnOdyssey then HumPrimaryFire(1, 150);
end;

procedure TCustomEliteManager.HumPrimaryFire_Rafale;
begin
  FOdysseyPressedKey.StopFire;
  //Tirer trois cartouches
  if EliteStatus.IsOnOdyssey then HumPrimaryFire(1, 250);
end;

procedure TCustomEliteManager.ThrowGrenadeFragmentation;
begin
  if EliteStatus.IsOnOdyssey then begin
    HumSelectFragGrenade;
    Delay(250);
    HumThrowGrenade;
  end;
end;

procedure TCustomEliteManager.ThrowGrenadeBouclier;
begin
  if EliteStatus.IsOnOdyssey then begin
    HumSelectShieldGrenade;
    Delay(250);
    HumThrowGrenade;
  end;
end;

procedure TCustomEliteManager.ThrowGrenadeEMP;
begin
  if EliteStatus.IsOnOdyssey then begin
    HumSelectEMPGrenade;
    Delay(250);
    HumThrowGrenade;
  end;
end;

procedure TCustomEliteManager.AimZoomRafale;
begin
  if EliteStatus.IsOnOdyssey then begin
    FOdysseyPressedKey.StopFire;
    if not EliteStatus.IsAimDownSight then HumAimZoom;
    Delay(250);
    HumPrimaryFire_Rafale;
  end;
end;

procedure TCustomEliteManager.ForwardWithCaution;
begin
  if EliteStatus.IsOnOdyssey then begin
    FOdysseyPressedKey.Clear;
    if not EliteStatus.IsAimDownSight then HumAimZoom;
    Delay(250);
    FOdysseyPressedKey.Avancer;
    Delay(250);
    HumWalk;
  end;
end;

procedure TCustomEliteManager.BackwardWithCaution;
begin
  if EliteStatus.IsOnOdyssey then begin
    FOdysseyPressedKey.Clear;
    if not EliteStatus.IsAimDownSight then HumAimZoom;
    Delay(250);
    FOdysseyPressedKey.Reculer;
    Delay(250);
    HumWalk;
  end;
end;

procedure TCustomEliteManager.StandInFront;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    if EliteStatus.IsAimDownSight then HumAimZoom;
    Delay(250);
    FOdysseyPressedKey.Avancer;
    Delay(250);
    HumSprint;
  end;
end;

procedure TCustomEliteManager.OpenTheDoor;
begin
  OdysseyInteractionValidate;
end;

procedure TCustomEliteManager.ShipBoarding;
begin
  if EliteStatus.IsOnOdyssey then begin
    TobiiPause;
    Delay(300);
    OdysseyInteractionValidate;
  end;
end;

procedure TCustomEliteManager.OdysseyInteractionValidate;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    Delay(90);
    HumPrimaryInteract;
    delay(450);
    UISelect;
  end;
end;

procedure TCustomEliteManager.SetOdysseyKeyPressed(
  const Value: TOdysseyPressedKey);
begin
  FOdysseyPressedKey := Value;
end;

procedure TCustomEliteManager.TobiiSaveDiver1;
begin
  EyesGazeMouseSettings.SaveConfigToSet1;
end;

procedure TCustomEliteManager.TobiiMorePrecision;
{*** plus précis pour les petites valeurs}
begin
  with EyesGazeMouseSettings do
    if WalkDeadZone > 0 then WalkDeadZone := WalkDeadZone - 1;
end;

procedure TCustomEliteManager.TobiiCongigDivers2;
begin
  EyesGazeMouseSettings.ConfigDivers2;
end;

procedure TCustomEliteManager.TobiiPause;
begin
  EyesGazeMouseSettings.GazePause;
end;

procedure TCustomEliteManager.TobiiConfigCombat;
begin
  EyesGazeMouseSettings.ConfigCombat;
end;

procedure TCustomEliteManager.TobiiConfigDivers1;
begin
  EyesGazeMouseSettings.ConfigDivers1;
end;

procedure TCustomEliteManager.TobiiConfigExplo;
begin
  EyesGazeMouseSettings.ConfigExplo;
end;

procedure TCustomEliteManager.TobiiLessPrecision;
{*** moins précis pour les grandes valeurs}
begin
  with EyesGazeMouseSettings do
    if WalkDeadZone < 12 then WalkDeadZone := WalkDeadZone + 1;
end;

procedure TCustomEliteManager.TobiiConfigCutter;
begin
  EyesGazeMouseSettings.ConfigACutter;
end;

procedure TCustomEliteManager.TobiiConfigStation;
begin
  EyesGazeMouseSettings.ConfigStation;
end;

procedure TCustomEliteManager.TobiiSaveDiver2;
begin
  EyesGazeMouseSettings.SaveConfigToSet2;
end;

procedure TCustomEliteManager.TobiiMoreSensibility;
{*** plus sensible pour les petites valeurs }
begin
  with EyesGazeMouseSettings do
    if Sensibility > 10 then Sensibility := Sensibility - TrackPositionToSensibility(1);
end;

procedure TCustomEliteManager.TobiiLessSensibility;
{*** moins sensible pour les grandes valeurs}
begin
  with EyesGazeMouseSettings do
    if Sensibility < 40 then Sensibility := Sensibility + TrackPositionToSensibility(1);
end;

procedure TCustomEliteManager.TobiiStart;
begin
  EyesGazeMouseSettings.Start;
end;

procedure TCustomEliteManager.TobiiLoad;
begin
  EyesGazeMouseSettings.EyeXMouseRun;
end;

procedure TCustomEliteManager.TobiiStop;
begin
  EyesGazeMouseSettings.GazeDisable;
end;

procedure TCustomEliteManager.TobiiMenuMode;
begin
  EyesGazeMouseSettings.GazeMenu
end;

procedure TCustomEliteManager.SniperMode;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    Delay(250);
    HumAccroupir;
    if not EliteStatus.IsAimDownSight then HumAimZoom;
  end;
end;

procedure TCustomEliteManager.Engager;
begin
  if EliteStatus.IsOnOdyssey then begin
    TobiiStart;
    Delay(250);
    HumSelectPrimaryWeapon;
    Delay(250);
    FOdysseyPressedKey.Avancer;
  end;
end;

procedure TCustomEliteManager.StandInBack;
begin
  if EliteStatus.IsOnOdyssey then begin
    StopMovAndFire;
    if EliteStatus.IsAimDownSight then HumAimZoom;
    Delay(250);
    FOdysseyPressedKey.Reculer;
    Delay(250);
    HumSprint;
  end;
end;

procedure TCustomEliteManager.KeyFireStop;
begin
  FOdysseyPressedKey.StopFire;
  //Relacher la combinaison de touches courante
  KeyMessageSender.DoKeyUp(True);
  HoldFire; //voir .109 
end;

procedure TCustomEliteManager.DoAfterInteract;
begin
  if EliteStatus.IsOnOdyssey then begin
    TobiiPause;
    StopMovAndFire;
    Delay(350);
    HumPrimaryInteract;
  end;
end;

procedure TCustomEliteManager.HumInteractMenuItem(const Value: Integer);
begin
  DoAfterInteract;
  if EliteStatus.IsOnOdyssey then begin
    Delay(250);
    Down(5);
    if Value > 0 then begin
      Delay(250);
      Up(Value);
    end;
    Delay(250);
    UISelect;
  end;
end;

procedure TCustomEliteManager.CashPrimes;
begin
  HumInteractMenuItem(1);
end;

procedure TCustomEliteManager.EnrollmentManage;
begin
  HumInteractMenuItem(0);
end;

procedure TCustomEliteManager.ShowStock;
begin
  HumInteractMenuItem(0);
end;

procedure TCustomEliteManager.ShowConsumable;
begin
  HumInteractMenuItem(1);
end;

procedure TCustomEliteManager.EngagerEx;
begin
  if EliteStatus.IsOnOdyssey then begin
    TobiiStart;
    Delay(250);
    FOdysseyPressedKey.Avancer;
  end;
end;

procedure TCustomEliteManager.TobiiConfig;
begin
  if not uRaoulUpdater.IsDirectorOpened then
    uDosUtils.OpenExecute('GazeMode.exe', '' );
end;

procedure TCustomEliteManager.TobiiHideConfig;
begin
  if uRaoulUpdater.IsDirectorOpened then uRaoulUpdater.CloseDirector;
end;

procedure TCustomEliteManager.TobiiPauseSwitch;
begin
  //NOTE Il faut que EyeMouse géère le non fonctionnement d'Elite
  TobiiPause;
end;

procedure TCustomEliteManager.TobiiMouseSwitch;
begin
  //NOTE Il faut que EyeMouse géère le non fonctionnement d'Elite
  EyesGazeMouseSettings.StartMouse;
end;

procedure TCustomEliteManager.StopMovAndTobii;
begin
  StopMovAndFire;
  TobiiStop;
end;

procedure TCustomEliteManager.StopMovAndFire;
begin
  with FOdysseyPressedKey do Clear;
end;

procedure TCustomEliteManager.HeadLockReset;
begin
  FKeyInventory.KeyTrigger_( 'HeadLookReset', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeadLockUp;
begin
  FKeyInventory.KeyTrigger_( 'HeadLookPitchUp', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeadLockDown;
begin
  FKeyInventory.KeyTrigger_( 'HeadLookPitchDown', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeadLockLeft;
begin
  FKeyInventory.KeyTrigger_( 'HeadLookYawLeft', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeadLockRight;
begin
  FKeyInventory.KeyTrigger_( 'HeadLookYawRight', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeadLockTools;
begin
  if not EliteStatus.IsOnElite then Exit;
  TobiiPause;
//  Delay(150);
  HeadLockReset;
  Delay(150);
  HeadLockDown;
  HeadLockDown;
end;

procedure TCustomEliteManager.HeadLockExit;
begin
  UIRetour;
  HeadLockReset;
  Delay(150);
  TobiiStart;
end;

procedure TCustomEliteManager.HeadLockTarget;
begin
  if EliteStatus.IsOnElite then begin
    HeadLockTools;
    HeadLockLeft;
  end;
end;

procedure TCustomEliteManager.HeadLockShield;
begin
  if EliteStatus.IsOnElite then begin
    HeadLockTools;
    HeadLockRight;
  end;
end;

procedure TCustomEliteManager.UIRetour;
begin
  with Recorder do if IsGridOpened then RetourActivate else UIBack;
end;

{ TEliteManager }

class procedure TEliteManager.Finalize;
begin
  EyesGazeMouseSettings.Terminate;
  FreeAndNil(EyesGazeMouseSettings);
  EliteManager.OdysseyKeySurveyor.Terminate;
  if Assigned(OdysseyPressedKey) then FreeAndNil( OdysseyPressedKey );
  if Assigned(EliteManager) then FreeAndNil( EliteManager );
end;

class procedure TEliteManager.Initialize;
begin
  if not Assigned(EliteManager) then EliteManager := TEliteManager.Create;
  if not Assigned(OdysseyPressedKey) then OdysseyPressedKey := TOdysseyPressedKey.Create(EliteManager);
  EliteManager.SetOdysseyKeyPressed(OdysseyPressedKey);
  EliteManager.OdysseyKeySurveyor := TOdysseyKeySurveyor.Create(OdysseyPressedKey );
  TEyesGazeMouseSettings.Initialize;
end;

class procedure TEliteManager.KeyInventoryAssign(const Value: TKeyInventory);
begin
  if Assigned(EliteManager) then EliteManager.SetKeyInventory( Value )
end;

class procedure TEliteManager.TagAssign(const Value: string);
begin
  if Assigned(EliteManager) then EliteManager.SetTags( Value )
end;

{ TEliteRunningObserver }

constructor TEliteRunningObserver.Create;
begin
  inherited Create( False );
  {Launch on create}
  FreeOnTerminate := True;
  Priority        := tpLower
end;

procedure TEliteRunningObserver.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 500 )
  end
end;

procedure TEliteRunningObserver.Process;
begin
  KeyWrite(AppKey, 'EliteLaunched', IsEliteRunningUTLS);
end;

procedure TEliteRunningObserver.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 )
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TOdysseyPressedKey }

procedure TOdysseyPressedKey.AddCase(const Value: TOdysseyPressedKeyType);
begin
  FMov := FMov + [Value];
  DoOnCaseAdd(Value);
end;

procedure TOdysseyPressedKey.Avancer;
begin
  SubCase(opkbackward);
  AddCase(opkforward);
end;

procedure TOdysseyPressedKey.Clear;
begin
  StopMov;
  StopFire;
end;

constructor TOdysseyPressedKey.Create(const Value: TEliteManager);
begin
  inherited Create;
  FManager := Value;
  FMov := [];
end;

destructor TOdysseyPressedKey.Destroy;
begin
  Clear;
  inherited;
end;

procedure TOdysseyPressedKey.DoOnCaseAdd(const Value: TOdysseyPressedKeyType);
begin
  case Value of
    opkfire : FClockFire := GetTickCount;
    else FClockMov := GetTickCount;
  end;
  case Value of
    opkforward  : FManager.HumAvance;
    opkbackward : FManager.HumRecule;
    opkfire     : FManager.HumPrimaryFire;
  end;
end;

procedure TOdysseyPressedKey.DoOnCaseSub(const Value: TOdysseyPressedKeyType);
begin
  case Value of
    opkforward  : FManager.HumAvance(1, 90);
    opkbackward : FManager.HumRecule(1, 90);
    opkfire     : FManager.HumPrimaryFire(1, 90);
  end;
end;

function TOdysseyPressedKey.FireExists: Boolean;
begin
  Result := opkfire in FMov;
end;

function TOdysseyPressedKey.IsCaseOn(
  const Value: TOdysseyPressedKeyType): Boolean;
begin
  Result := Value in FMov
end;

function TOdysseyPressedKey.MvtExists: Boolean;
begin
  Result := (opkforward in FMov) or (opkbackward in FMov);
end;

procedure TOdysseyPressedKey.Reculer;
begin
  SubCase(opkforward);
  AddCase(opkbackward);
end;

procedure TOdysseyPressedKey.StopFire;
begin
  if FireExists then begin
    FClockFire := 0;
    SubCase(opkfire);
    KeyWrite(AppKey, 'OdysseyFire', 'none');
  end;
end;

procedure TOdysseyPressedKey.StopMov;
begin
  if MvtExists then begin
    FClockMov := 0; 
    SubCase(opkforward);
    SubCase(opkbackward);
    KeyWrite(AppKey, 'OdysseyMvt', 'none');
    {*** pour les mvts du vaisseau "va père ...."}
    KeyMessageSender.DoKeyUp(True);
  end;
end;

procedure TOdysseyPressedKey.SubCase(const Value: TOdysseyPressedKeyType);
begin
  if Value in FMov then begin
    FMov := FMov - [Value];
    DoOnCaseSub(Value);
  end;
end;

procedure TOdysseyPressedKey.Tirer;
begin
  AddCase(opkfire);
end;

{ TOdysseyKeySurveyor }

constructor TOdysseyKeySurveyor.Create(const Value: TOdysseyPressedKey);
begin
  inherited Create( False );
  {Launch on create}
  ThWaitingFor        := KeyReadCard(AppKey, 'KeyPressedDelay', 60000);
  { --- 90 et 30 seconds for key down before automatic key up }
  ThOdysseyPressedKey := Value;
  FreeOnTerminate     := True;
  Priority            := tpLower;

end;

procedure TOdysseyKeySurveyor.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 150  );
  end;
end;

procedure TOdysseyKeySurveyor.Process;
begin
  with ThOdysseyPressedKey do begin
    if (ClockMov  > 0) and (GetTickCount - ClockMov  > ThWaitingFor) then StopMov;
    if (ClockFire > 0) and (GetTickCount - ClockFire > ThWaitingFor div 2) then StopFire;
  end;
end;

procedure TOdysseyKeySurveyor.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 )
    until Self.Terminated or Terminated or (GetTickCount > S)
end;


initialization
  { --- Initialize le traitement pour Elite}
  TEliteManager.Initialize;
  EliteRunningObserver := TEliteRunningObserver.Create
finalization
  EliteRunningObserver.Terminate;
end.
