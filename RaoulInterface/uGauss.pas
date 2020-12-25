unit uGauss;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Math, StrCopyUtils, StrUtils, StdCtrls, ComCtrls, uRegistry,
  uSymba, ClipBrd;

type
  TExprType = (et_numeric, et_symbolic, et_affectation);

  TDotStacked = class
  private
    FTagStack    : TStringList;
    FMethod      : TGetStrProc;
    FOnAfterExec : TNotifyEvent;
    procedure AddToStack(const Tags: string);
    procedure ProcessOnStack;
  public
    procedure SetTags(const Values: string);
    class procedure Execute(const Values: string; WithMethod: TGetStrProc;
      DoBeforeExec, DoAfterExec: TNotifyEvent);
    constructor Create;
    destructor Destroy; override;
  end;

  TRegConvertor = class
  private
    function  GetKindOfExpr: TExprType;
    procedure SetKindOfExpr(const Value: TExprType);
    function  GetExprStr: string;
    procedure SetExprStr(const Value: string);
    function  GetExprValue: Double;
    procedure SetExprValue(const Value: Double);
    function  GetExprError: Boolean;
    procedure SetExprError(const Value: Boolean);
    function  GetExprErrMsg: string;
    procedure SetExprErrMsg(const Value: string);
    function  GetXData: string;
    procedure SetXData(const Value: string);
    function  GetYData: string;
    procedure SetYData(const Value: string);
    function  GetZData: string;
    procedure SetZData(const Value: string);
    function  GetExprValueStr: string;
    function  GetExprAffect: Boolean;
    procedure SetExprAffect(const Value: Boolean);
  public
    property  KindOfExpr: TExprType read GetKindOfExpr write SetKindOfExpr;
    property  ExprStr: string read GetExprStr write SetExprStr;
    property  ExprValue: Double read GetExprValue write SetExprValue;
    property  ExprValueStr: string read GetExprValueStr;
    property  ExprError: Boolean read GetExprError write SetExprError;
    property  ExprErrMsg: string read GetExprErrMsg write SetExprErrMsg;
    property  ExprAffect: Boolean read GetExprAffect write SetExprAffect;
    property  XData: string read GetXData write SetXData;
    property  YData: string read GetYData write SetYData;
    property  ZData: string read GetZData write SetZData;
  end;

  TStackConvertor = class
  private
    FTag       : string;
    FX         : Double;
    FDeci      : Boolean;
    FZero      : Boolean;
    FZSt       : string;
    FBuffer    : string;
    FValue     : string;
    FAns       : string;
    FAnsError  : Boolean;
    EL,DL      : TStringList;
    FVarArray  : array[0..2] of Variant;
    FVarLst    : TStringList;
    FRegVal    : TRegConvertor;
    { --- Symbolic calcul engine }
    Symbolic : TSymbExpression;
    function  GetKindOfExpr: TExprType;
    procedure SetKindOfExpr(const Value: TExprType);
    function  GetExprStr: string;
    procedure SetExprStr(const Value: string);
    function  GetExprValue: Double;
    procedure SetExprValue(const Value: Double);
    function  GetExprError: Boolean;
    procedure SetExprError(const Value: Boolean);
    function  GetExprErrMsg: string;
    procedure SetExprErrMsg(const Value: string);
    function  GetXData: string;
    procedure SetXData(const Value: string);
    function  GetYData: string;
    procedure SetYData(const Value: string);
    function  GetZData: string;
    procedure SetZData(const Value: string);
    function  GetExprAffect: Boolean;
    procedure SetExprAffect(const Value: Boolean);
  private
    { --- Method during execution }
    procedure DoBeforeExecute(Sender: TObject);
    procedure DoAfterExecute(Sender: TObject);
  private
    function KindOfExprRetrieve: TExprType;
    function IsVarExists(const VarName: string): Boolean;
  private
    { --- Variables managment }
    procedure VarPrepare;
    procedure VarValuesToSymbolicEngine;
    procedure DoAffectation;
  private
    { --- Decode process }
    function  IsPuissance(const ASt: string):Boolean;
    function  Puissance(const ASt: string):Double;
    function  Exponant(const ASt: string):Double;
    procedure AddToL(const Value: Double; const Expo: Boolean = False);
    function  PileDecode(const Pile: TStringList): Double;
  private
    { --- Interpreter methods }
    procedure Initialize;
    procedure Finalize;
    function  N(const ASt: string): Integer;
    function  IsMilGroup(const ASt: string): Boolean;
    function  IsNumDot(const ASt: string): Boolean;
    function  IsExposant(const ASt: string): Boolean;
    function  IsNumeric(const ASt: string): Boolean;
    function  IsOperand(const ASt: string): Boolean;
    function  IsAns(const ASt: string): Boolean;
    function  IsVariable(const ASt: string): Boolean;
    function  IsAffectation(const ASt: string): Boolean;
    procedure Reset;
    procedure Validate;
    procedure NumericProcess(const ASt: string);
    procedure OperandProcess(const ASt: string);
    function  Evalue_:Double;
    procedure Interpreter(const S: string);
  public
    procedure SetTag(const Value: string);
    property  KindOfExpr: TExprType read GetKindOfExpr write SetKindOfExpr;
    property  ExprStr: string read GetExprStr write SetExprStr;
    property  ExprValue: Double read GetExprValue write SetExprValue;
    property  ExprError: Boolean read GetExprError write SetExprError;
    property  ExprErrMsg: string read GetExprErrMsg write SetExprErrMsg;
    property  ExprAffect: Boolean read GetExprAffect write SetExprAffect;
    property  XData: string read GetXData write SetXData;
    property  YData: string read GetYData write SetYData;
    property  ZData: string read GetZData write SetZData;
    class function Evalue(const Tags: string; var DataResult: string): string;
    constructor Create;
    destructor Destroy; override;
  end;

  THistoDisplay = class
  private
    FRichEdit : TRichEdit;
    FRegVal   : TRegConvertor;
    function  GetExprError: Boolean;
    procedure SetExprError(const Value: Boolean);
    function  GetExprErrMsg: string;
    procedure SetExprErrMsg(const Value: string);
    function  GetVarValues: string;
  public
    procedure SetRichEdit(const Value: TRichEdit);
    procedure AddLine(const Operation, Value: string);
    property  ExprError: Boolean read GetExprError write SetExprError;
    property  ExprErrMsg: string read GetExprErrMsg write SetExprErrMsg;
    class procedure AddTo(const ARitchEdit: TRichEdit; const Operation, Value: string);
    constructor Create;
    destructor Destroy; override;
  end;

  { --- Gauss process additional }
  TGaussAdditional = class
  private
    FRegVal : TRegConvertor;
    function VoiceValueValidate(var Approximate: Boolean): string;
    function VoiceValue_: string;
  public
    class function VoiceValue: string;
    constructor Create;
    destructor Destroy; override;
  end;

function DecimaleFmt(const ASt: string): string;

var
  PileStr      : string;
  Temp         : string;
  XVarCont     : TLabel = nil;
  YVarCont     : TLabel = nil;
  ZVarCont     : TLabel = nil;

implementation

const
  CENT      = 100;
  MILLE     = 1000;
  MILLION   = 1000000;
  MILLIARD  = 1000000000;
  VIRGULE   = '999';
  EXPOSANT  = '998';
  PLUS      = '990';
  MOINS     = '980';
  MULTIPLIE = '970';
  DIVISE    = '960';
  ANS       = '950';
  VARX      = '940';
  VARY      = '930';
  VARZ      = '920';
  AFFECT    = '910';

  MILGROUP  : array[0..2] of string = ('1000', '1000000', '1000000000');
  OPERAND   : array[0..3] of string = ('990', '980',  '970',     '960');
  OPVALUE   : array[0..3] of string = ('+',   '-',    '*',       '/');
  VARIABLES : array[0..2] of string = ('940', '930',  '920');
  VARNAMES  : array[0..2] of string = ('x',   'y',    'z');

function NoSpace(const Src: string):string;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(Src) do
    if Src[i] <> ' ' then Result := Result + Src[i]
end;

function DecimaleFmt(const ASt: string): string;
var
  p       : Integer;
  d       : Integer;
  Pattern : string;
begin
  DecimalSeparator := '.';
  p := AnsiPos('.', ASt);
  if p > 0 then begin
    d := Length(ASt) - p;
    if d < 2 then d := 2;
    Pattern := Format('%s.%d%s', ['%', d, 'f']);
    Result := Format(Pattern, [StrToFloat(ASt)]);
  end else
    Result := Format('%f', [StrToFloat(ASt)]);
end;

function FixDiv(const ASt: string):string;
{ --- FIX le problème du moteur formel avec le quotien pour les valeurs numériques
      Erreur ne s'avérant pas avec une expression symbolique }
var
  buffer  : string;
  LST     : TStringList;
  i       : Integer;
  indexes : array of Integer;
  cpt     : Integer;


  function Initialize: string; begin
    Buffer := NoSpace(ASt);
    LST    := TStringList.Create;
    Result := EmptyStr
  end;

  procedure Finalize; begin
    SetLength(indexes, 0);
    LST.Free
  end;

  procedure ExtractToLST; begin
    { --- Convert string value to list }
    if Buffer[1] = '/' then Buffer := Format('1%s', [Buffer]);
    i := 1;
    LST.Add(EmptyStr);
    with LST do while i <= Length(Buffer) do begin
      if not ( Buffer[i] in ['0'..'9','.','x','y','z'] ) then begin
        Add(Buffer[i]);
        Inc(i);
        Add(EmptyStr)
      end else begin
        if not ( Buffer[i] in ['0'..'9','.','x','y','z'] ) then
          raise Exception.Create('Erreur');
        Strings[Pred(Count)] := Strings[Pred(Count)] + Buffer[i];
        Inc(i);
      end
    end;
    LST.Text := Trim(LST.Text)
  end;

  procedure DivCount; var i : Integer; begin
    { --- Retrieve div place in the list }
    cpt := 0;
    SetLength(indexes, cpt);
    for i := 0 to Pred(LST.Count) do
      if LST[i] = '/' then begin
        Inc(cpt);
        SetLength(indexes, cpt);
        indexes[cpt - 1] := i;
      end;
  end;

  function BuildResult: string; var i,k : Integer; begin
    ExtractToLST;
    DivCount;
    { --- Fix values }
    with LST do for i := High(indexes) downto Low(indexes) do begin
      k := Indexes[i];
      if (i > 0) and (k - Indexes[i-1] = 2)
        then begin
          Strings[k] := Format('*(1/%s)', [Strings[k+1]]);
          Delete(k+1);
        end else begin
          Strings[k] := Format('(%s/%s)', [Strings[k-1], Strings[k+1]]);
          Delete(k+1); Delete(k-1)
        end
    end;
    Result := EmptyStr;
    { --- Build final string }
    for i := 0 to Pred(LST.Count) do if Result = EmptyStr
      then Result := LST[i]
      else Result := Format('%s%s', [Result, LST[i]])
  end;

  function Process: string; begin
    if (AnsiPos('/', Buffer) = 0) or (Trim(Buffer) = EmptyStr) then Result := Buffer
      else Result := BuildResult
  end;

begin
  Result := Initialize;
  try
    Result := Process
  finally
    Finalize
  end
end; {FixDiv}

{ TStackConvertor }

procedure TStackConvertor.AddToL(const Value: Double; const Expo: Boolean);
var
  u : TStringList;
begin
  if FDeci then u := DL else u := EL;
  with u do if Expo
    then Add( Format('E%.0f', [Value]) )
    else Add( Format('%.0f',  [Value]) )
end;

constructor TStackConvertor.Create;
begin
  inherited Create;
  FRegVal    := TRegConvertor.Create;
  Symbolic   := TSymbExpression.Create(nil);
  EL         := TStringList.Create;
  DL         := TStringList.Create;
  FVarLst    := TStringList.Create;
  ExprAffect := False;
end;

destructor TStackConvertor.Destroy;
begin
  FVarLst.Free;
  EL.Free;
  DL.Free;
  Symbolic.Free;
  FRegVal.Free;
  inherited;
end;

procedure TStackConvertor.DoAffectation;
var
  i     : Integer;
  Src   : string;
  After : string;
  Value : string;
  Found : Boolean;

  procedure RaiseError; begin
    ExprError  := True;
    ExprErrMsg := 'Affecter que des valeurs numériques';
    raise Exception.Create(ExprErrMsg)
  end;

  function FixData(const ASt: string):string; var i : Integer; begin
    Result := Trim( SubstrSeplaceInText(ASt, '=', #0) );
    Found  := False;
    for i := 1 to Length(Result) do
      if not (Result[i] in ['0'..'9','.']) then begin
          { --- autoriser les valeurs négatives }
          if Result[i] in ['-','+'] then if i <> 1 then RaiseError
        end else
      if Result[i] in ['0'..'9'] then Found := True;
    if not Found then RaiseError else Result := NoSpace(Result)
  end;

  function AddVarFromStr(const VName, ASt: string): string; begin
    Value := FixData( GetAfterStr(ASt, '=') );
    case IndexStr(VName, VARNAMES) of
      0 : if Value <> EmptyStr then XData := Value;
      1 : if Value <> EmptyStr then YData := Value;
      2 : if Value <> EmptyStr then ZData := Value;
    end
  end;

begin
  Src := FBuffer;
  with FVarLst do
    if Count = 1 then AddVarFromStr(Strings[0], Src)
     else for i := Pred(Count) downto 0 do begin
       After := GetAfterStr(Src, Strings[i]);
       Src   := GetBeforStr(Src, Strings[i]);
       AddVarFromStr(Strings[i], After)
     end
end; {DoAffectation}

procedure TStackConvertor.DoAfterExecute(Sender: TObject);
begin
  Finalize
end;

procedure TStackConvertor.DoBeforeExecute(Sender: TObject);
begin
  Initialize
end;

class function TStackConvertor.Evalue(const Tags: string;
  var DataResult: string): string;
begin
  with TStackConvertor.Create do
  try
    SetTag( Tags );
    if ExprAffect then DataResult := 'SET' else DataResult := FValue;
    if NoSpace(FBuffer) <> FValue
      then Result := FBuffer
      else Result := FBuffer;
    Application.ProcessMessages
  finally
    Free
  end
end;

function TStackConvertor.Evalue_: Double;
begin
  with Symbolic do begin
    InText := FixDiv( FBuffer );
    Result := toNumeric;
  end
end;

function TStackConvertor.Exponant(const ASt: string): Double;
begin
  DecimalSeparator := '.';
  try
    Result := StrToFloat(ASt)
  except
    raise
  end
end;

procedure TStackConvertor.Finalize;

  function CanEvaluate: Boolean; begin
    Result := not ExprError and not ExprAffect
  end;

  procedure Fix; begin
    FBuffer := Trim( FBuffer );
    if ExprError and (AnsiPos('. 0', FBuffer) > 0) then
      FBuffer := GetBeforStr(FBuffer, '. 0') + '.'
  end;

begin
  Validate;
  Fix;
  KindOfExpr  := KindOfExprRetrieve;
  ExprStr     := FBuffer;
  if CanEvaluate then
  try
    VarValuesToSymbolicEngine;
    ExprValue := Evalue_;
    FValue    := FloatToStr( ExprValue );
  except
    ExprErrMsg := '? expression erronée';
    ExprError  := True
  end else
  try
    DoAffectation
  except
  end
end; {Finalize}

function TStackConvertor.GetExprAffect: Boolean;
begin
  Result := FRegVal.ExprAffect
end;

function TStackConvertor.GetExprErrMsg: string;
begin
  Result := FRegVal.ExprErrMsg
end;

function TStackConvertor.GetExprError: Boolean;
begin
  Result := FRegVal.ExprError
end;

function TStackConvertor.GetExprStr: string;
begin
  Result := FRegVal.ExprStr
end;

function TStackConvertor.GetExprValue: Double;
begin
  Result := FRegVal.ExprValue
end;

function TStackConvertor.GetKindOfExpr: TExprType;
begin
  Result := FRegVal.KindOfExpr
end;

function TStackConvertor.GetXData: string;
begin
  Result := FRegVal.XData
end;

function TStackConvertor.GetYData: string;
begin
  Result := FRegVal.YData
end;

function TStackConvertor.GetZData: string;
begin
  Result := FRegVal.ZData
end;

procedure TStackConvertor.Initialize;
begin
  FBuffer    := EmptyStr;
  { --- Retrieve ANS values and informations }
  FAnsError  := ExprError;
  FAns       := KeyReadString(BufferKey, 'ExprValue');
  { --- Initiazlize datas in registry }
  ExprError  := False;
  ExprErrMsg := EmptyStr;
  Reset
end;

procedure TStackConvertor.Interpreter(const S: string);
begin
  NumericProcess(S);
  OperandProcess(S)
end;

function TStackConvertor.IsAffectation(const ASt: string): Boolean;
begin
  Result := ASt = AFFECT
end;

function TStackConvertor.IsAns(const ASt: string): Boolean;
begin
  Result := ASt = ANS
end;

function TStackConvertor.IsExposant(const ASt: string): Boolean;
begin
  Result := ASt = EXPOSANT
end;

function TStackConvertor.IsMilGroup(const ASt: string): Boolean;
begin
  Result := IndexStr(ASt, MILGROUP) > -1
end;

function TStackConvertor.IsNumDot(const ASt: string): Boolean;
begin
  Result := ASt = VIRGULE
end;

function TStackConvertor.IsNumeric(const ASt: string): Boolean;
begin
  { --- Compléter ici les autres fonctions A-D } 
  Result := not IsOperand(ASt)
end;

function TStackConvertor.IsOperand(const ASt: string): Boolean;
begin
  Result := IndexStr(ASt, OPERAND) > -1
end;

function TStackConvertor.IsPuissance(const ASt: string): Boolean;
begin
  Result := ASt[1] = 'E'
end;

function TStackConvertor.IsVarExists(const VarName: string): Boolean;
begin
  Result := AnsiPos(AnsiLowerCase(VarName), AnsiLowerCase(FBuffer)) > 0
end;

function TStackConvertor.IsVariable(const ASt: string): Boolean;
begin
  Result := IndexStr(ASt, VARIABLES) > -1
end;

function TStackConvertor.KindOfExprRetrieve: TExprType;
begin
  Result := TExprType(
    Integer( IsVarExists('x') or IsVarExists('y') or IsVarExists('z') )
    )
end;

function TStackConvertor.N(const ASt: string): Integer;
begin
  try Result := StrToInt( ASt ) except Result := 0 end
end;

procedure TStackConvertor.NumericProcess(const ASt: string);
var
  VName: string;

  procedure AddGroup; begin
    AddToL(FX);
    AddToL(N(ASt), True);
  end;

  procedure LocalReset; begin
    { --- Reset datas }
    FX     := 0;
    FZero  := True;
    FZSt   := EmptyStr
  end;

  procedure ErrorCheck; begin
    { --- raise exception when two decimal separators }
    if not FDeci then Exit;
    Validate;
    FBuffer    := FBuffer + '.';
    ExprError  := True;
    ExprErrMsg := 'erreur de virgule numérique';
    raise Exception.Create(ExprErrMsg);
  end;

  procedure Process; begin
    { --- Manage free zero on decimal part }
    if FZero then begin
      case ASt = '0' of
        True : FZSt  := Format('%s0', [FZSt]);
        else   FZero := False;
      end
    end;
    { --- number building }
    case N(ASt) = 100 of
      True : if FX = 0 then FX := 100 else FX := FX * 100;
      else   FX := FX + N(ASt)
    end;
  end;

  procedure AnsReplace; begin
    Validate;
    FBuffer := Format('%s %s', [FBuffer, FAns]);
  end;

  procedure VarReplace; begin
    Validate;
    KindOfExpr := et_symbolic;
    VName      := VARNAMES[IndexStr(AST, VARIABLES)];
    FBuffer := Format('%s %s', [ FBuffer, VName ]);
    FVarLst.Add( VName )
  end;

  procedure EqualIndicator; begin
    Validate;
    ExprAffect := True;
    FBuffer    := Format('%s =', [FBuffer]);
  end;

begin
  case IsNumeric(ASt) of
    True : begin
      { --- 1000, 1000000, 1000000000 }
      if IsMilGroup(ASt) then begin
        if FX = 0 then AddToL(N(ASt), True) else AddGroup;
        FX := 0;
      end else
      { --- Decimal separator }
      if IsNumDot(ASt) then begin
        ErrorCheck;
        { --- Stack last number }
        if Fx > 0 then AddToL(Fx);
        FDeci := True;
        LocalReset;
      end else
      if IsAns(ASt) then AnsReplace
      else
      if IsVariable(ASt) then VarReplace
      else
      if IsAffectation(ASt) then EqualIndicator
      else
      { --- main managment }
        Process
    end
  end
end; {NumericProcess}

procedure TStackConvertor.OperandProcess(const ASt: string);
var
  Oper : string;
begin
  if not IsOperand(ASt) then Exit;
  Validate;
  try
    Oper := OPVALUE[ IndexStr(ASt, OPERAND)];
    FBuffer := Format('%s %s', [FBuffer, Oper])
  except
    ExprErrMsg := 'Opération incorrecte';
    ExprError  := True;
    Raise
  end
end;

function TStackConvertor.PileDecode(const Pile: TStringList): Double;
var
  i    : Integer;
  V    : array[0..255] of Extended;
  cnt  : Integer;
  Max  : Double;

  procedure RaiseError; begin
    ExprError  := True;
    ExprErrMsg := 'énoncé incorrect';
    raise Exception.Create(ExprErrMsg);
  end;

begin
  Result := 0;
  Max    := 0;
  if Assigned(Pile) then with Pile do
  try
    FillChar(V, Length(V), #0);
    { --- Empty stack do nothing }
    if Count = 0 then Exit;
    { --- Read first value }
    if IsPuissance(Strings[0]) then V[0] := Puissance(Strings[0])
      else V[0] := Exponant(Strings[0]);
    cnt := 1;
    { --- Compose the current value with the previous one if it is a power }
    for i := 1 to Pred(Count) do
      if IsPuissance(Strings[i])
        then begin
          if Max = 0 then Max := Puissance(Strings[i]);
          if Max < Puissance(Strings[i]) then RaiseError;
          V[cnt-1] := V[cnt-1] * Puissance(Strings[i])
        end else begin
          V[cnt] := Exponant(Strings[i]);
          cnt    := cnt + 1
        end;
    { --- Sum over the entire stack }
    Result := V[0];
    for i := 1 to cnt do Result := Result + V[i]
  except
  end
end; {PileDecode}

function TStackConvertor.Puissance(const ASt: string): Double;
var
  S : string;
begin
  DecimalSeparator := '.';
  try
    S      := GetAfterStr(ASt, 'E');
    Result := StrToFloat( S );
  except
    raise
  end
end;

procedure TStackConvertor.Reset;
begin
  FX         := 0;
  FDeci      := False;
  FZero      := False;
  FZSt       := EmptyStr;
  DL.Clear;
  EL.Clear;
end;

procedure TStackConvertor.SetExprAffect(const Value: Boolean);
begin
  FRegVal.ExprAffect := Value
end;

procedure TStackConvertor.SetExprErrMsg(const Value: string);
begin
  FRegVal.ExprErrMsg := Value
end;

procedure TStackConvertor.SetExprError(const Value: Boolean);
begin
  FRegVal.ExprError := Value
end;

procedure TStackConvertor.SetExprStr(const Value: string);
begin
  FRegVal.ExprStr := Value
end;

procedure TStackConvertor.SetExprValue(const Value: Double);
begin
  FRegVal.ExprValue := Value
end;

procedure TStackConvertor.SetKindOfExpr(const Value: TExprType);
begin
  FRegVal.KindOfExpr := Value
end;

procedure TStackConvertor.SetTag(const Value: string);
begin
  FTag := Value;
  { --- Stack tags in FStack }
  TDotStacked.Execute(Value, Interpreter, DoBeforeExecute, DoAfterExecute);
end;

procedure TStackConvertor.SetXData(const Value: string);
begin
  FRegVal.XData := Value;
  if Assigned(XVarCont) then with XVarCont do Caption := Value
end;

procedure TStackConvertor.SetYData(const Value: string);
begin
  FRegVal.YData := Value;
  if Assigned(YVarCont) then with YVarCont do Caption := Value
end;

procedure TStackConvertor.SetZData(const Value: string);
begin
  FRegVal.ZData := Value;
  if Assigned(ZVarCont) then with ZVarCont do Caption := Value
end;

procedure TStackConvertor.Validate;
begin
  DecimalSeparator := '.';
  if FX > 0 then AddToL(FX);
  PileStr := EL.Text;
  if EL.Count > 0 then FBuffer := Format('%s %.0f', [FBuffer, PileDecode(EL)]);
  if FDeci and (DL.Count > 0) then begin
    if EL.Count = 0
      then FBuffer := Format('%s 0.%s%.0f', [FBuffer, FZSt, PileDecode(DL)])
      else FBuffer := Format('%s.%s%.0f',   [FBuffer, FZSt, PileDecode(DL)]);
  end;
  Reset
end;

procedure TStackConvertor.VarPrepare;
begin
  FVarArray[0] := XData;
  FVarArray[1] := YData;
  FVarArray[2] := ZData;
end;

procedure TStackConvertor.VarValuesToSymbolicEngine;
begin
  VarPrepare;
  with Symbolic do AssignVariables(['x','y','z'], FVarArray)
end;

{ TDotStacked }

procedure TDotStacked.AddToStack(const Tags: string);
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

constructor TDotStacked.Create;
begin
  inherited Create;
  FTagStack := TStringList.Create;
  FMethod   := nil
end;

destructor TDotStacked.Destroy;
begin
  FTagStack.Free;
  inherited
end;

class procedure TDotStacked.Execute(const Values: string;
  WithMethod: TGetStrProc; DoBeforeExec, DoAfterExec: TNotifyEvent);
begin
  with TDotStacked.Create do
  try
    FMethod      := WithMethod;
    FOnAfterExec := DoAfterExec;
    if Assigned(DoBeforeExec) then DoBeforeExec(nil);
    SetTags( Values );
    ProcessOnStack
  finally
    Free
  end
end;

procedure TDotStacked.ProcessOnStack;
begin
  with FTagStack do
  try
    try
      while Count > 0 do begin
        if Assigned(FMethod) then FMethod( Strings[ Pred(Count) ] );
        Delete( Pred(Count) )
      end;
    except
    end;
    if Assigned(FOnAfterExec) then FOnAfterExec(nil)
  finally
  end
end;

procedure TDotStacked.SetTags(const Values: string);
begin
  AddToStack( Values );
  PileStr := FTagStack.Text;
end;

{ THistoDisplay }

procedure THistoDisplay.AddLine(const Operation, Value: string);
begin
  with FRichEdit do begin
    SelAttributes.Size  := 24;
    Lines.Insert(0,'');
    SelStart := 0;
    Paragraph.Alignment := taRightJustify;
    SelAttributes.Color := clWhite;
    SelAttributes.Style :=  [fsBold];
    if ExprError
      then SelText := 'ERREUR' + #10
      else
    try SelText := DecimaleFmt(Value)+#10 except SelText := Value+#10 end;

    Paragraph.Alignment := taRightJustify;
    SelAttributes.Size  := 13;
    SelAttributes.Color := $000080FF;
    SelAttributes.Style := [fsBold];
    if ExprError
      then SelText := Format('%s %s'#10, [Operation, ExprErrMsg])
      else SelText := Format('%s'#10,    [Operation]);

    Paragraph.Alignment := taRightJustify;
    SelAttributes.Size  := 11;
    SelAttributes.Color := $000080FF;
    SelAttributes.Style := [fsBold];
    if not ExprError
      then SelText := Format('%s'#10,    [GetVarValues])
      else SelText := Format('mémo = %s'#10,  [Value]);

    Perform(WM_VSCROLL, SB_TOP, 0);
    Windows.SetFocus(0);
    Application.ProcessMessages;
    with Lines do SaveToFile('Calc.txt')
  end;
end;

class procedure THistoDisplay.AddTo(const ARitchEdit: TRichEdit;
  const Operation, Value: string);
begin
  DecimalSeparator := '.';
  with THistoDisplay.Create do
  try
    SetRichEdit ( ARitchEdit       );
    if Value = 'SET' then AddLine( Operation, Value )
     else try
       StrToFloat( Value );
       AddLine     ( Operation, Value )
     except
       AddLine     ( Operation, 'ERROR' )
     end
  finally
    Free
  end
end;

constructor THistoDisplay.Create;
begin
  inherited Create;
  FRegVal := TRegConvertor.Create;
end;

destructor THistoDisplay.Destroy;
begin
  FRegVal.Free;
  inherited
end;

function THistoDisplay.GetExprErrMsg: string;
begin
  Result := FRegVal.ExprErrMsg
end;

function THistoDisplay.GetExprError: Boolean;
begin
  Result := FRegVal.ExprError
end;

function THistoDisplay.GetVarValues: string;
begin
  with FRegVal do
    Result := Format('x = %s; y = %s; z = %s', [XData, YData, ZData])
end;

procedure THistoDisplay.SetExprErrMsg(const Value: string);
begin
  FRegVal.ExprErrMsg := Value
end;

procedure THistoDisplay.SetExprError(const Value: Boolean);
begin
  FRegVal.ExprError := Value
end;

procedure THistoDisplay.SetRichEdit(const Value: TRichEdit);
begin
  FRichEdit := Value
end;

{ TRegConvertor }

function TRegConvertor.GetExprAffect: Boolean;
begin
  Result := KeyReadBoolean(BufferKey, 'ExprAffect')
end;

function TRegConvertor.GetExprErrMsg: string;
begin
  Result := KeyReadString(BufferKey, 'ExprErrMsg')
end;

function TRegConvertor.GetExprError: Boolean;
begin
  Result := KeyReadBoolean(BufferKey, 'ExprError')
end;

function TRegConvertor.GetExprStr: string;
begin
  Result := KeyReadString(BufferKey, 'ExprStr')
end;

function TRegConvertor.GetExprValue: Double;
begin
  Result := KeyReadFloat(BufferKey, 'ExprValue')
end;

function TRegConvertor.GetExprValueStr: string;
begin
  Result := KeyReadString(BufferKey, 'ExprValue')
end;

function TRegConvertor.GetKindOfExpr: TExprType;
begin
  Result := TExprType(KeyReadInt(BufferKey, 'KindOfExpr'))
end;

function TRegConvertor.GetXData: string;
begin
  Result := KeyReadString(BufferKey, 'XData', '0')
end;

function TRegConvertor.GetYData: string;
begin
  Result := KeyReadString(BufferKey, 'YData', '0')
end;

function TRegConvertor.GetZData: string;
begin
  Result := KeyReadString(BufferKey, 'ZData', '0')
end;

procedure TRegConvertor.SetExprAffect(const Value: Boolean);
begin
  KeyWrite(BufferKey, 'ExprAffect', Value)
end;

procedure TRegConvertor.SetExprErrMsg(const Value: string);
begin
  KeyWrite(BufferKey, 'ExprErrMsg', Value)
end;

procedure TRegConvertor.SetExprError(const Value: Boolean);
begin
  KeyWrite(BufferKey, 'ExprError', Value)
end;

procedure TRegConvertor.SetExprStr(const Value: string);
begin
  KeyWrite(BufferKey, 'ExprStr', Value)
end;

procedure TRegConvertor.SetExprValue(const Value: Double);
begin
  KeyWrite(BufferKey, 'ExprValue', Value)
end;

procedure TRegConvertor.SetKindOfExpr(const Value: TExprType);
begin
  KeyWrite(BufferKey, 'KindOfExpr', Integer(Value))
end;

procedure TRegConvertor.SetXData(const Value: string);
begin
  KeyWrite(BufferKey, 'XData', Value)
end;

procedure TRegConvertor.SetYData(const Value: string);
begin
  KeyWrite(BufferKey, 'YData', Value)
end;

procedure TRegConvertor.SetZData(const Value: string);
begin
  KeyWrite(BufferKey, 'ZData', Value)
end;

{ TGaussAdditional }

constructor TGaussAdditional.Create;
begin
  inherited Create;
  FRegVal := TRegConvertor.Create
end;

destructor TGaussAdditional.Destroy;
begin
  FRegVal.Free;
  inherited
end;

class function TGaussAdditional.VoiceValue: string;
begin
  with TGaussAdditional.Create do try Result := VoiceValue_ finally Free end
end;

function TGaussAdditional.VoiceValueValidate(var Approximate: Boolean): string;
var
  Buffer : string;
  E,F    : string;
  X      : Double;
  P      : Integer;
  Expo   : string;
  IsExpo : Boolean;

  function DecimalExists: Boolean; begin
    Result := AnsiPos('.', Buffer) > 0
  end;

  function DodecimalProcess: string; begin
    DecimalSeparator := '.';
    E := GetBeforStr(Buffer, '.');
    F := GetAfterStr(Buffer, '.');
    if Length(F) >= 4 then begin
      Approximate := True;
      if F[4] = '5' then P := 4 else P := 3;

      X := SimpleRoundTo( StrToFloat( Format('0.%s', [F] )), -P );
      F := GetAfterStr(FloatToStr(X), '.');
    end;
    Result := Format('%s virgule %s', [E,F]);
  end;

  function ExposantExists:Boolean; begin
    Result := AnsiPos('E', AnsiUpperCase(Buffer)) > 0;
    if Result then begin
      Expo   := GetAfterStr(Buffer, 'E');
      Buffer := GetBeforStr(Buffer, 'E' )
    end
  end;

  function Process: string; begin
    case DecimalExists of
      False : Result := Buffer;
      True  : Result := DodecimalProcess;
    end
  end;

begin
  try
    Approximate := False;
    Buffer := FRegVal.ExprValueStr;
    IsExpo := ExposantExists;
    Result := Process;
    if IsExpo then
      Result := Format('%s exposant %s', [Result, Expo])
  except
  end
end; {VoiceValueValidate}

function TGaussAdditional.VoiceValue_: string;
var
  Approximate: Boolean;
begin
  with FRegVal do if not ExprAffect then begin
    { --- Valeur calculée }
    if ExprError then Result := 'erreur de calcul'
      else begin
        Result := VoiceValueValidate(Approximate);
        if Approximate then Result := Format('à peu près %s', [Result])
      end
  end else begin
    { --- Valeur affectée }
    if ExprError then Result := 'affectation incorrecte'
      else Result := 'ok'
  end
end;

end.



