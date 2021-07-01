{****************************************************}
{                                                    }
{               06/2021 MaxiDonkey                   }
{            Gaze settings for Tobii                 }
{               Need EyeXMouse.exe                   }
{                                                    }
{****************************************************}


unit uGazeSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  {Accès registre}
  uRegistry, ExtCtrls, ComCtrls, ShellAPI;

type
  TConfigArray = array[0..3] of Integer;

var
  ConfigCombat_  : TConfigArray = (0, 10, 150, 1);
  ConfigStation_ : TConfigArray = (1, 20, 250, 0);
  ConfigExplo_   : TConfigArray = (0, 20, 150, 0);
  ConfigACutter_ : TConfigArray = (4, 40, 150, 0);
  ConfigDivers1_ : TConfigArray = (1, 40, 650, 0);
  ConfigDivers2_ : TConfigArray = (2, 30, 550, 0);


type
  TGazeMode = (gmDisabled, gmJoystick, gmMenu, gmPause);
  TEliteState = (esStop, EsRun);

  TGazeSurveyor = class;
  TEyesGazeMouseSettings = class
  private
    FMode: TGazeMode;
    FEliteState: TEliteState;
    FGazeSurveyor: TGazeSurveyor;
    procedure SetMode(const Value: TGazeMode);
    procedure SetEliteState(const Value: TEliteState);
    function  GetWalkDeadZone: Integer;
    procedure SetWalkDeadZone(const Value: Integer);
    function  GetSensibility: Integer;
    procedure SetSensibility(const Value: Integer);
    function  GetNoise: Integer;
    procedure SetNoise(const Value: Integer);
    function  GetCombat: Boolean;
    procedure SetCombat(const Value: Boolean);
    procedure Configure(W, S, N, C: Integer); overload;
    procedure Configure(V : TConfigArray); overload;
    function  GetGazeEnabled: Boolean;
    procedure SetGazeEnabled(const Value: Boolean);
    function  CurrentConfig: TConfigArray;
    function  CurrentConfigToText: string;
  public
    procedure EyeXMouseRun;
    {*** State }
    procedure Update;
    procedure GazeReset;
    procedure GazeJoystick;
    procedure GazeDisable;
    procedure GazeMenu;
    procedure GazePause;
    {*** Configurations }
    procedure ConfigCombat;
    procedure ConfigStation;
    procedure ConfigExplo;
    procedure ConfigACutter;
    procedure ConfigDivers1;
    procedure ConfigDivers2;
    procedure SaveConfigToSet1;
    procedure SaveConfigToSet2;
    {*** Manager }
    procedure Start;
    procedure Stop;
    procedure Terminate;
    procedure EliteSurveyor; //for Timer
    {*** Parameters }
    property Mode: TGazeMode read FMode write SetMode;
    property EliteState: TEliteState read FEliteState write SetEliteState;
    property WalkDeadZone: Integer read GetWalkDeadZone write SetWalkDeadZone;
    property Sensibility: Integer read GetSensibility write SetSensibility;
    property Noise: Integer read GetNoise write SetNoise;
    property Combat: Boolean read GetCombat write SetCombat;
    property GazeEnabled: Boolean read GetGazeEnabled write SetGazeEnabled;

    constructor Create;
    destructor Destroy; override;
    class procedure Initialize;
  end;

  TGazeSurveyor = class(TThread)
  private
    ThEyesGazeMouseSettings : TEyesGazeMouseSettings;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const Value: TEyesGazeMouseSettings);
  end;

{*** APIs }
function TrackPositionToSensibility(const Value: Integer): Integer;
function SensibilityToTrackPosition(const Value: Integer): Integer;
function TrackPositionToNoise(const Value: Integer): Integer;
function NoiseToTrackPosition(const Value: Integer): Integer;

var
  StrModes : array[tGazeMode] of string =
    ('Disabled', 'Joystick', 'Menu', 'Pause');
  EyesGazeMouseSettings : TEyesGazeMouseSettings = nil;

implementation

var
  HElite      : THandle = 0;
  ELITE_CLASS : string = 'FrontierDevelopmentsAppWinClass';

function ConfigToText(const Value: TConfigArray): string;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    for i := 0 to 3 do Add( Format('%d', [Value[i]]) );
    Result := Text;
  finally
    Free
  end;
end;

function TextToConfig(const ASt: string): TConfigArray;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    Text := ASt;
    try
      for i := 0 to 3 do Result[i] := StrToInt(Strings[i]);
    except
      {*** si erreur alors rendre la config 1 }
      Result := ConfigDivers1_
    end
  finally
    Free
  end;
end;

procedure OpenExecute(AppName: string; AppParams: string; Displayed: Integer);
begin
  try
    if FileExists(AppName) then
      ShellExecute(0, 'Open', PChar(AppName), PChar(AppParams), nil, Displayed);
  except
  end;
end;

function IsEliteRunning: Boolean;
begin
  HElite := FindWindow( PChar(ELITE_CLASS), nil );
  Result :=  HElite <> 0;
end;

procedure EliteForeGround;
begin
  if HElite = 0 then HElite := FindWindow( PChar(ELITE_CLASS), nil );
  SetForegroundWindow( HElite );
end;

{ TEyesGazeMouseSettings }

procedure TEyesGazeMouseSettings.Configure(W, S, N, C: Integer);
begin
  WalkDeadZone := W;
  Sensibility  := S;
  Noise        := N;
  Combat       := Boolean(C);
end;

procedure TEyesGazeMouseSettings.ConfigDivers2;
begin
  Configure( TextToConfig(KeyReadString(AppKey, 'GazeConfig2')) );
end;

procedure TEyesGazeMouseSettings.ConfigCombat;
begin
  Configure(ConfigCombat_);
end;

procedure TEyesGazeMouseSettings.ConfigDivers1;
begin
  Configure( TextToConfig(KeyReadString(AppKey, 'GazeConfig1')) );
end;

procedure TEyesGazeMouseSettings.ConfigExplo;
begin
  Configure(ConfigExplo_);
end;

procedure TEyesGazeMouseSettings.ConfigStation;
begin
  Configure(ConfigStation_);
end;

procedure TEyesGazeMouseSettings.ConfigACutter;
begin
  Configure(ConfigACutter_);
end;

procedure TEyesGazeMouseSettings.Configure(V: TConfigArray);
begin
  Configure(V[0], V[1], V[2], V[3]);
end;

constructor TEyesGazeMouseSettings.Create;
var
  ASt : string;
begin
  inherited Create;
  FEliteState := esStop;
  FGazeSurveyor := TGazeSurveyor.Create(Self);
  {*** EyeXMouse.exe closure forced, if opened }
  GazeReset;
  GazeEnabled := False;
  {*** params read forced  }
  WalkDeadZone;
  Sensibility;
  Noise;
  Combat;
  {*** SavConfig 1 et 2}
  if KeyReadString(AppKey, 'GazeConfig1') = EmptyStr
    then KeyWrite(AppKey, 'GazeConfig1', ConfigToText( ConfigDivers1_ ));
  if KeyReadString(AppKey, 'GazeConfig2') = EmptyStr
    then KeyWrite(AppKey, 'GazeConfig2', ConfigToText( ConfigDivers2_ ));
end;

procedure TEyesGazeMouseSettings.GazeDisable;
begin
  if Mode <> gmDisabled then GazeReset else GazeJoystick;
end;

procedure TEyesGazeMouseSettings.GazeJoystick;
begin
  KeyWriteDWord32(AppKey, 'GazeMode', 1);
  Mode := gmJoystick;
end;

procedure TEyesGazeMouseSettings.GazeMenu;
begin
  KeyWriteDWord32(AppKey, 'GazeMode', 2);
  Mode := gmMenu;
end;

procedure TEyesGazeMouseSettings.GazePause;
begin
  KeyWriteDWord32(AppKey, 'GazeMode', 3);
  Mode := gmPause;
end;

procedure TEyesGazeMouseSettings.GazeReset;
begin
  KeyWriteDWord32(AppKey, 'GazeMode', 0);
  Mode := gmDisabled;
end;

function TEyesGazeMouseSettings.GetCombat: Boolean;
begin
  Result := Boolean(KeyReadDWord32(AppKey, 'Combat', 0));
end;

function TEyesGazeMouseSettings.GetNoise: Integer;
begin
  Result := KeyReadDWord32(AppKey, 'Noise', 250)
end;

function TEyesGazeMouseSettings.GetSensibility: Integer;
begin
  Result := KeyReadDWord32(AppKey, 'Sensibility', 20)
end;

function TEyesGazeMouseSettings.GetWalkDeadZone: Integer;
begin
  Result := KeyReadDWord32(AppKey, 'WalkDeadZone')
end;

procedure TEyesGazeMouseSettings.SetCombat(const Value: Boolean);
begin
  KeyWriteDWord32(AppKey, 'Combat', Integer(Value));
  Update;
end;

procedure TEyesGazeMouseSettings.SetEliteState(const Value: TEliteState);
begin
  FEliteState := Value;
  if FEliteState = esStop then GazeReset;
end;

procedure TEyesGazeMouseSettings.SetMode(const Value: TGazeMode);
begin
  FMode := Value;
end;

procedure TEyesGazeMouseSettings.SetNoise(const Value: Integer);
begin
  KeyWriteDWord32(AppKey, 'Noise', Value);
  Update;
end;

procedure TEyesGazeMouseSettings.SetSensibility(const Value: Integer);
begin
  KeyWriteDWord32(AppKey, 'Sensibility', Value);
  Update;
end;

procedure TEyesGazeMouseSettings.SetWalkDeadZone(const Value: Integer);
begin
  KeyWriteDWord32(AppKey, 'WalkDeadZone', Value);
  Update;
end;

procedure TEyesGazeMouseSettings.Update;
begin
  KeyWriteDWord32(AppKey, 'Update', 1);
end;

function TrackPositionToSensibility(const Value: Integer): Integer;
begin
  Result := Value * 10
end;

function SensibilityToTrackPosition(const Value: Integer): Integer;
begin
  Result := Value div 10
end;

function TrackPositionToNoise(const Value: Integer): Integer;
begin
  Result := 50 + 100 * Value
end;

function NoiseToTrackPosition(const Value: Integer): Integer;
begin
  Result := (Value - 50) div 100
end;

class procedure TEyesGazeMouseSettings.Initialize;
begin
  if not Assigned(EyesGazeMouseSettings) then
    EyesGazeMouseSettings := TEyesGazeMouseSettings.Create;
  EyesGazeMouseSettings.GazeDisable;
end;

procedure TEyesGazeMouseSettings.Terminate;
begin
  GazeDisable;
end;

procedure TEyesGazeMouseSettings.Stop;
begin
  GazePause;
end;

procedure TEyesGazeMouseSettings.Start;
begin
  GazeJoystick;
  Sleep(90);
  EyeXMouseRun;
end;

procedure TEyesGazeMouseSettings.EliteSurveyor;
begin
  if IsEliteRunning then EliteState := esRun else EliteState := esStop;
end;

destructor TEyesGazeMouseSettings.Destroy;
begin
  FGazeSurveyor.Terminate;
  inherited;
end;

function TEyesGazeMouseSettings.GetGazeEnabled: Boolean;
begin
  Result := Boolean(KeyReadDWord32(AppKey, 'GazeEnabled', 0));
end;

procedure TEyesGazeMouseSettings.SetGazeEnabled(const Value: Boolean);
begin
  KeyWriteDWord32(AppKey, 'GazeEnabled', Integer(Value));
end;

procedure TEyesGazeMouseSettings.EyeXMouseRun;
begin
  if IsEliteRunning and not GazeEnabled then OpenExecute('EyeXMouse.exe', '', SW_SHOW)
end;

procedure TEyesGazeMouseSettings.SaveConfigToSet1;
begin
  KeyWrite(AppKey, 'GazeConfig1', CurrentConfigToText );
end;

function TEyesGazeMouseSettings.CurrentConfigToText: string;
begin
  Result := ConfigToText( CurrentConfig );
end;

function TEyesGazeMouseSettings.CurrentConfig: TConfigArray;
begin
  Result[0] := WalkDeadZone;
  Result[1] := Sensibility;
  Result[2] := Noise;
  Result[3] := Integer(Combat);
end;

procedure TEyesGazeMouseSettings.SaveConfigToSet2;
begin
  KeyWrite(AppKey, 'GazeConfig2', CurrentConfigToText );
end;

{ TGazeSurveyor }

constructor TGazeSurveyor.Create(const Value: TEyesGazeMouseSettings);
begin
  ThEyesGazeMouseSettings := Value;
  inherited Create( False );
  {Launch on create}
  FreeOnTerminate := True;
  Priority        := tpLower
end;

procedure TGazeSurveyor.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 300 )
  end
end;

procedure TGazeSurveyor.Process;
begin
  ThEyesGazeMouseSettings.EliteSurveyor;
end;

procedure TGazeSurveyor.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 )
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

end.

