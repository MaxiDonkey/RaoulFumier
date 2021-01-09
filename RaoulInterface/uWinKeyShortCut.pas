unit uWinKeyShortCut;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Math, StrUtils, EliteBindingsTools, StrCopyUtils,
  clipbrd, SendKey32, uRegistry, KeysDef;

type
  TWShortCut = class
  private
    function  GrCodeToCardinal(const grCode: Integer): Cardinal;
    procedure WriteChar(const Index: Integer);
    procedure Media(const grCode: Integer);
    procedure KeyWin(const grCode: Integer);
    procedure DoWithMethod(const Method: TNotifyEvent);
    procedure AppOnNextScreen(Sender: TObject);
    procedure AppOnPrevScreen(Sender: TObject);
  private
    procedure SendSignal(const KeySignal: Cardinal);
    procedure SendWinSignal(const KeySignal: Integer);
    procedure SCWinExecute(const grCodeInt: Integer);
    procedure SCExecute(const grCodeInt: Integer); overload;
    function  SCExecute(const grCode: string; AConfiance: Double): Integer; overload;
  public
    class procedure Execute(const grCode: string; AConfiance: Double);
    class procedure ExecuteCmd(const gsCodeInt: Integer);
    class procedure ExecuteMedia(const grCode: Integer);
    class procedure ExecuteKeyWin(const grCode: Integer);
  end;

  { --- Multi shortcut execution }
  TShortCutStacked = class
  private
    FTagStack  : TStringList;
    FConfiance : Double;
    procedure AddToStack(const Tags: string);
    procedure ProcessOnStack;
  public
    procedure SetTags(const Values: string; const AConfiance: Double);

    class procedure Execute(const Values: string; const AConfiance: Double);

    constructor Create;
    destructor Destroy; override;
  end;

  TSCRepeater = class
  private
    function  GetLastCommande: Integer;
    procedure SetLastCommande(const Value: Integer);
    function  ValInSet(const Value: Integer; SetOfInt: array of Integer):Boolean;
    procedure CmdCheck(const Value: Integer);
    procedure AgainCheck(const Value: Integer);
    procedure ProcessWithCmd(const Value: Integer);
    function  GetLastNavCmd: Integer;
    procedure SetLastNavCmd(const Value: Integer);
  public
    procedure SetCurrentCommand(const Value: Integer);

    property LastCommande: Integer read GetLastCommande write SetLastCommande;
    property LastNavCmd: Integer read GetLastNavCmd write SetLastNavCmd;

    class procedure Initialize;
    class procedure CurrentCmdNav(const Value: Integer);
    class procedure AssignLastCommand(const Value: Integer);
  end;

var
  ProcExitNav: TNotifyEvent = nil;

implementation

uses
  uDosUtils;

{ TWShortCut }

const
  CODEBIAS = 100000;
  SCCOUNT  = 107;

class procedure TWShortCut.Execute(const grCode: string; AConfiance: Double);
begin
  with TWShortCut.Create do try SCExecute(grCode, AConfiance) finally Free end
end;

class procedure TWShortCut.ExecuteCmd(const gsCodeInt: Integer);
begin
  with TWShortCut.Create do try SCExecute(gsCodeInt) finally Free end
end;

function TWShortCut.GrCodeToCardinal(const grCode: Integer): Cardinal;
var
  ASt : string;
begin
  Result := grCode - CODEBIAS;
  ASt    := KeyScanToEliteString(
              EliteKeyToScanValue( TEliteKeyType( Result mod SCCOUNT - 1 ) ));
  case Result div SCCOUNT of
    0 : Result := EncodeKey( ASt, '',              '  ' );
    1 : Result := EncodeKey( ASt, 'Key_LeftShift',   '' );
    2 : Result := EncodeKey( ASt, 'Key_LeftAlt',     '' );
    3 : Result := EncodeKey( ASt, 'Key_LeftControl', '' );
    4 : Result := EncodeKey( ASt, 'Key_LeftShift',   'Key_LeftAlt' );
    5 : Result := EncodeKey( ASt, 'Key_LeftShift',   'Key_LeftControl' );
    6 : Result := EncodeKey( ASt, 'Key_LeftAlt',     'Key_LeftControl' );
    else Result := 0
  end
end;

procedure TWShortCut.SCExecute(const grCodeInt: Integer);
begin
  SendSignal( GrCodeToCardinal( grCodeInt ))
end;

var
  ALLOAWED_ALT_TAB : array[0..39] of string = (
     '100041', (*RightArrow*)      '100042', (*DownArrow*)
     '100043', (*LeftArrow*)       '100044', (*UpArrow*)
     '100047', (*Home*)            '100048', (*End*)
     '140002', (*Alt_Tab*)         '100076', (*Tab*)
     '100183', (*Shift_Tab*)
     (*Again*)
     '120001', '120002', '120003', '120004',
     '120005', '120006', '120007', '120008',
     '120009', '120010', '120021', '120022',
     '120023', '120024', '120025', '120026',
     '120027', '120028', '120029', '120030',
     '120000',
     '130001', '130002', '130003', '130004',
     '130005', '130006', '130007', '130008',
     '130009', '130010'
  );

procedure TWShortCut.Media(const grCode: Integer);
begin
  with TShortcuts do case grCode of
    140100 : Execute('mediaplay');
    140101 : Execute('mediastop');
    140102 : Execute('medianext');
    140103 : Execute('mediaprior');
    140104 : Execute('volume_mute');
    140105 : Execute('volume_up');
    140106 : Execute('volume_down');
    140110 : Execute('navigator_back');
    140111 : Execute('navigator_forward');
    140112 : Execute('navigator_refresh');
    140113 : Execute('navigator_stop');
    140114 : Execute('navigator_search');
    140115 : Execute('navigator_favorite');
    140116 : Execute('navigator_home');
    140117 : Execute('navigator_goliank');
    140118 : Execute('navigator_linknext');
    140119 : Execute('navigator_linkprior');
    140120 : Execute('navigator_fullscreen');
    140121 : Execute('navigator_sheetclose');
    140122 : Execute('navigator_sheetnew');
    140123 : Execute('navigator_sheetnext');
    140124 : Execute('navigator_sheetprior');
    140125 : Execute('navigator_new');
    140126 : Execute('navigator_zoomin');
    140127 : Execute('navigator_zoomout');
    140128 : Execute('navigator_nozoom');
  end
end;

function TWShortCut.SCExecute(const grCode: string; AConfiance: Double): Integer;

  function SetRepeat(const Value: Integer):Boolean; begin
    Result := True;
    TSCRepeater.AssignLastCommand( Value - 10000 )
  end;

  function CheckTab:Boolean; begin
    Result := KeyReadBoolean(ParamKey, 'Alt_Tab');
    case Result of
      True : Result := IndexStr(grCode,  ALLOAWED_ALT_TAB) = -1
    end;
    if Result then AltRelease
  end;

  function IsChar(const ACode: Integer): Boolean; begin
    case ACode of
      100001..100027,
      100077..100098,
      100100,
      100108..100133,
      100046,
      100053..100062,
      100064..100069,
      100201,
      100205,
      110001..110025 : Result := True;
      else Result := False
    end
  end;

  function CheckCharOk(const ACode: Integer): Boolean; begin
    Result := IsChar( ACode ) and (AConfiance > 0.82) //0.87
  end;

  function CheckCodeOk(const ACode: Integer): Boolean; begin
    Result := not IsChar( ACode ) and (AConfiance > 0.80)
  end;

  function CanContinue(const ACode: Integer): Boolean; begin
    Result := CheckCharOk( ACode ) or CheckCodeOk( ACode )
  end;

  procedure Process(const ACode: Integer); begin
    { --- Alt key up if necessary }
    if CheckTab then Exit;
    { --- Case code process }
    case ACode of
      100750..100856 : SCWinExecute( ACode );
      110001..110025 : WriteChar( ACode );
      130002..130010 : if SetRepeat( ACode ) then Exit;
      140000         : if Assigned(ProcExitNav) then ProcExitNav(nil);
      140001         : SendKeyWin;
      140002         : AltTab;
      140003         : MouseDoubleClic;
      140004         : MouseMiddleClic;
      140100..140128 : Media( ACode );
      140220..140250 : ExecuteKeyWin( ACode );
      else SCExecute( ACode )
    end;
    TSCRepeater.CurrentCmdNav( ACode )
  end;

begin
  try
    Result := StrToInt( grCode );
    { --- process under confidence }
    if CanContinue( Result ) then Process( Result )
  except
    Result := 0
  end
end; {SCExecute}

procedure TWShortCut.SCWinExecute(const grCodeInt: Integer);
var
  Value: TEliteKeyType;
begin
  Value := TEliteKeyType(grCodeInt - 100750);
  SendWinSignal( EliteKeyToScanValue( Value ) )
end;

procedure TWShortCut.SendSignal(const KeySignal: Cardinal);
begin
  if KeySignal > 0 then TKeyMessageSender.Signal(KeySignal, 30, WITH_KEYUP)
end;

procedure TWShortCut.SendWinSignal(const KeySignal: Integer);
begin
  if KeySignal > 0 then SendKeyWin(KeySignal)
end;

procedure TWShortCut.WriteChar(const Index: Integer);
begin
  case index of
    110001 : SendKeys( PChar('@'#0), False);
    110002 : SendKeys( PChar('°'#0), False);
    110003 : SendKeys( PChar('~'#0), False);
    110004 : SendKeys( PChar('\'#0), False);
    110005 : SendKeys( PChar('§'#0), False);
    110006 : SendKeys( PChar('?'#0), False);
    110007 : SendKeys( PChar('µ'#0), False);
    110008 : SendKeys( PChar('£'#0), False);
    110009 : SendKeys( PChar('%'#0), False);
    110010 : SendKeys( PChar('¨'#0), False);
    110011 : SendKeys( PChar('ï'#0), False);
    110012 : SendKeys( PChar('ô'#0), False);
    110013 : SendKeys( PChar('ê'#0), False);
    110014 : SendKeys( PChar('â'#0), False);
    110015 : SendKeys( PChar('#'#0), False);
    110016 : SendKeys( PChar('{'#0), False);
    110017 : SendKeys( PChar('}'#0), False);
    110018 : SendKeys( PChar('['#0), False);
    110019 : SendKeys( PChar(']'#0), False);
    110020 : SendKeys( PChar('|'#0), False);
    110021 : SendKeys( PChar('È'#0), False);
    110022 : SendKeys( PChar('É'#0), False);
    110023 : SendKeys( PChar('Ê'#0), False);
    110024 : SendKeys( PChar('ø'#0), False);
    110025 : SendKeys( PChar('Ø'#0), False);
  end
end;

class procedure TWShortCut.ExecuteMedia(const grCode: Integer);
begin
  with TWShortCut.Create do try Media(grCode) finally Free end
end;

procedure TWShortCut.KeyWin(const grCode: Integer);
begin
  with TShortcuts do case grCode of
    140220 : Execute('win_1');
    140221 : Execute('win_2');
    140222 : Execute('win_3');
    140223 : Execute('win_4');
    140224 : Execute('win_5');
    140225 : Execute('win_6');
    140226 : Execute('win_7');
    140227 : Execute('win_8');
    140228 : Execute('win_9');
    { --- https://fr.wikipedia.org/wiki/Windows_(touche) }
    140229 : SendKeyWin( VkKeyScan('M'), kw_shift);
    140230 : DoWithMethod( AppOnPrevScreen );
    140231 : DoWithMethod( AppOnNextScreen );
    140232 : SendKeyWin( VK_LEFT  );
    140233 : SendKeyWin( VK_RIGHT );
    140234 : SendKeyWin( VkKeyScan('D'), kw_alt);
    140235 : SendKeyWin( VkKeyScan(';') );
    140236 : SendKeyWin( VK_TAB );
    140237 : SendKeyWin( VK_HOME );
    140238 : SendKeyWin( VK_SPACE );
    140239 : SendKeyWin( VK_SUBTRACT );
    140240 : SendKeyWin( VK_ADD );
    140241 : SendKeyWin( VkKeyScan('D'), kw_control );
    140242 : SendKeyWin( VK_F4, kw_control );
    140243 : SendKeyWin( VK_ESCAPE );
    140244 : SendKeyWin( VK_UP );
  end
end;

class procedure TWShortCut.ExecuteKeyWin(const grCode: Integer);
begin
  with TWShortCut.Create do try KeyWin(grCode) finally Free end
end;

procedure TWShortCut.DoWithMethod(const Method: TNotifyEvent);
begin
  if Assigned(Method) then Method(Self)
end;

procedure TWShortCut.AppOnPrevScreen(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 1 do SendKeyWin( VK_LEFT, kw_shift)
end;

procedure TWShortCut.AppOnNextScreen(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 1 do SendKeyWin( VK_RIGHT, kw_shift)
end;

{ TShortCutStacked }

procedure TShortCutStacked.AddToStack(const Tags: string);
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

constructor TShortCutStacked.Create;
begin
  inherited Create;
  FTagStack := TStringList.Create
end;

destructor TShortCutStacked.Destroy;
begin
  FTagStack.Free;
  inherited
end;

class procedure TShortCutStacked.Execute(const Values: string; const AConfiance: Double);
begin
  with TShortCutStacked.Create do try SetTags( Values, AConfiance ) finally Free end
end;

procedure TShortCutStacked.ProcessOnStack;
begin
  with FTagStack do
  try
    while Count > 0 do begin
      TWShortCut.Execute( Strings[ Pred(Count) ], FConfiance );
      Delete( Pred(Count) )
    end
  finally
  end
end;

procedure TShortCutStacked.SetTags(const Values: string; const AConfiance: Double);
begin
  FConfiance := AConfiance;
  AddToStack( Values );
  ProcessOnStack
end;

{ TSCRepeater }

const
  NAVREPEATBIAS = 120000;
  ENCORE        = 120000;
  NAVREPEAXBIAS = 120020;

var
  CmdMemorizingSet : array[0..37] of Integer =
    ( 100041, (*Key_RightArrow*)                  100042, (*Key_DownArrow*)
      100043, (*Key_LeftArrow*)                   100044, (*Key_UpArrow*)
      100049, (*Key_PageUp*)                      100050, (*Key_PageDown*)
      100027, (*Key_Space*)                       100046, (*Key_Delete*)
      100100, (*Key_Backspace*)                   100076, (*Key_Tab*)
      100183, (*Shift + Key_Tab*)                 100362, (*Mot suivant   Ctrl+Droite*)
      100364, (*Mot précédent Ctrl+Gauche*)       100576, (*Sél. à dte Shift+Ctrl+Droite*)
      100578, (*Sél. à gche Shift+Ctrl+Gauche*)   140102, (*Media next*)
      140103, (*Media prev*)                      140105, (*Volume up*)
      140106, (*Volume down*)
      140110, (**)
      140111, (**)
      140112, (**)
      140113, (**)
      140114, (**)
      140115, (**)
      140116, (**)
      140117, (**)
      140118, (**)
      140119, (**)
      140120, (**)
      140121 ,(**)
      140122, (**)
      140123, (**)
      140124, (**)
      140125, (**)
      140126, (**)
      140127, (**)
      140128  (**)
    );

  CmdRepeater : array[0..9] of Integer =
    ( 120001, (*Une fois*)      120002, (*2 fois*)
      120003, (*3 fois*)        120004, (*4 fois*)
      120005, (*5 fois*)        120006, (*6 fois*)
      120007, (*7 fois*)        120008, (*8 fois*)
      120009, (*9 fois*)        120010  (*10 fois*)
    );

  { --- pour une itération de moins }
  CmdRepeaterEx : array[0..9] of Integer =
    ( 120021, 120022, 120023, 120024, 120025,
      120026, 120027, 120028, 120029, 120030
    );

procedure TSCRepeater.CmdCheck(const Value: Integer);

  function IsCommandRepeatOrRepeat: Boolean; begin
    Result := ValInSet(Value, CmdMemorizingSet) or  { --- commande can be memorized }
              ValInSet(Value, CmdRepeater)      or  { --- commande is a repeater }
              ValInSet(Value, CmdRepeaterEx)        { --- commande is a repeater without "Encore" }
  end;

begin
  if not IsCommandRepeatOrRepeat then raise Exception.Create('Silent')
end; {CmdCheck}

class procedure TSCRepeater.CurrentCmdNav(const Value: Integer);
begin
  with TSCRepeater.Create do try SetCurrentCommand( Value ) finally Free end
end;

function TSCRepeater.GetLastCommande: Integer;
begin
  Result := KeyReadInt(ParamKey, 'LastCommande')
end;

class procedure TSCRepeater.Initialize;
begin
  with TSCRepeater.Create do
  try
    LastCommande := 0;
    LastNavCmd   := 0
  finally
    Free
  end
end;

function TSCRepeater.ValInSet(const Value: Integer;
  SetOfInt: array of Integer): Boolean;
{ --- is Value in an array of integer }
var
  i : Integer;
begin
  Result := False;
  for i := Low(SetOfInt) to High(SetOfInt) do
    if Value = SetOfInt[i] then begin
      Result := True;
      Break
    end
end;

procedure TSCRepeater.ProcessWithCmd(const Value: Integer);
var
  i, d : Integer;
begin
  d := -1;
  { --- répéter l'action n fois car le terme "Encore" est dans le déclencheur }
  if ValInSet(Value, CmdRepeater)   then d := 0;
  { --- répéter l'action n-1 fois }
  if ValInSet(Value, CmdRepeaterEx) then d := 1;
  { --- Itération de la commande précédente }
  if (d <> -1) and ValInSet(LastCommande, CmdMemorizingSet) then
    for i := 1 to Value - Integer(d = 0) * NAVREPEATBIAS - Integer(d = 1) * NAVREPEAXBIAS - d
      do with TWShortCut do case lastCommande of
        140102..140103,
        140110..140111,
        140118..140119,
        140122..140124,
        140126..140127,
        140105..140106 : ExecuteMedia( LastCommande );
        else ExecuteCmd( LastCommande )
      end
end;

procedure TSCRepeater.SetCurrentCommand(const Value: Integer);
begin
  try
    { --- If cmd is "Encore" then repeat the LastNavCmd and raise or only save LastNavCmd }
    AgainCheck( Value );
    { --- Verify if cmd is valid or raise }
    CmdCheck( Value );
    { --- Repeat las cmd }
    ProcessWithCmd( Value );
    { --- Save last cmd }
    if ValInSet(Value, CmdMemorizingSet) then LastCommande := Value
  except
  end
end;

procedure TSCRepeater.SetLastCommande(const Value: Integer);
begin
  KeyWrite(ParamKey, 'LastCommande', Value)
end;


function TSCRepeater.GetLastNavCmd: Integer;
begin
  Result := KeyReadInt(ParamKey, 'LastNavCmd')
end;

procedure TSCRepeater.SetLastNavCmd(const Value: Integer);
begin
  KeyWrite(ParamKey, 'LastNavCmd', Value)
end;

procedure TSCRepeater.AgainCheck(const Value: Integer);

  function ProcessOnLastNavCmd: Integer; begin
    Result := LastNavCmd;
    if Result > NAVREPEAXBIAS then Result := Result - NAVREPEAXBIAS + NAVREPEATBIAS;
    ProcessWithCmd( Result )
  end;

  function DoneWithLastCmd: Boolean; begin
    { --- check last cmd like a repeater }
    Result := ValInSet(LastNavCmd, CmdRepeater) or ValInSet(LastNavCmd, CmdRepeaterEx);
    if Result then ProcessOnLastNavCmd
  end;

  procedure EncoreInCmdExists; begin
    if Value <> ENCORE then Exit;
    { --- if cmd process is "Encore" }
    if not DoneWithLastCmd then with TWShortCut do
      case LastNavCmd of
        140102..140103,
        140110..140111,
        140118..140119,
        140122..140124,
        140126..140127,
        140105..140106 : ExecuteMedia  ( LastNavCmd );
        140220..140250 : ExecuteKeyWin ( LastNavCmd );
        else ExecuteCmd( LastNavCmd )
      end;
    raise Exception.Create('Silent')
  end;

begin
  EncoreInCmdExists;
  LastNavCmd := Value
end; {AgainCheck}

class procedure TSCRepeater.AssignLastCommand(const Value: Integer);
begin
  with TSCRepeater.Create do try LastNavCmd := Value finally Free end
end;

end.
