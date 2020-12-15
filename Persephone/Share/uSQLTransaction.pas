unit uSQLTransaction;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ClipBrd, WebAPIs,
  {string utilities}
  StrUtils, StrCopyUtilsA,
  {Database access}
  DB, ADODB,
  {GUID Create}
  ActiveX,
  {dev express}
  cxProgressBar;

const
  SHORT_SLEEP = 10;
  SHORT_WAIT  = 90;
  WAIT_GAUGE  = 30;

type
  {Signature des méthodes pour acter sur les champs d'une table}
  TSQLWorkOnFields   = procedure (Sender: TObject; AFields: TFields) of object;
  TSQLGetStrOnFields = procedure (Sender: TObject; AFields: TFields; var ASt: string) of object;

  TFieldReader = class;

  TDistConnect = class
  private
    function GetConnectionString: string;

  private
    FADOConnection : TADOConnection;
    FServer        : string;
    FCatalog       : string;
    FPassWord      : string;
    FStarted       : Boolean;

  public
    procedure Close;
    procedure Login(Source, Catalog, PassWord : string);
    procedure LogOff;
    procedure Refresh;

    property ADOConnection: TADOConnection read FADOConnection;
    property ConnectionString: string read GetConnectionString;

    constructor Create;
    destructor Destroy; override;
    class function New(AServer, ACatalog, APassword: string):TDistConnect;
  end; {TDistConnect}



  TDistManagerCustom = class
  private
    FConnector: TADOConnection;
    procedure AssignConnnection(const DistantDB: TDistConnect);
    procedure IterateOnFields(const QueryStr: string; Method: TSQLWorkOnFields; const AProgress: TcxProgressBar = nil);
    function  GetStrOnFields(const QueryStr: string; Method: TSQLGetStrOnFields; const AProgress: TcxProgressBar = nil): string;

  public
    function  Count_(const QueryStr: string): Integer;
    function  IsActive: Boolean;
    procedure Open;
    procedure Close;
    function  RetrieveValues_(const QueryStr: string): string;
    function  RetrieveSetOfValues_(const QueryStr: string): string;
    procedure SqlExecute(const QueryStr: string);

    constructor Create(const DistantDB: TDistConnect);
    destructor Destroy; override;

    class function  Count(const DistantDB: TDistConnect; const QueryStr: string):Integer;
    class procedure Execute(const DistantDB: TDistConnect; const QueryStr: string);
    class function  RetrieveValues(const DistantDB: TDistConnect; const QueryStr: string): string;
    class function  RetrieveSetOfValues(const DistantDB: TDistConnect; const QueryStr: string): string;

    {Job d'une méthode sur toutes les lignes de l'ensemble de résultats}
    class procedure DoOnFields(const DistantDB: TDistConnect; const QueryStr: string;
      Method: TSQLWorkOnFields; const AProgress: TcxProgressBar = nil);

    {Job d'une méthode sur toutes les lignes de l'ensemble de résultats - en sortie une chaîne}
    class function  DoGetOnFields(const DistantDB: TDistConnect; const QueryStr: string;
      Method: TSQLGetStrOnFields; const AProgress: TcxProgressBar = nil): string;

  end; {TDistManagerCustom}

  
  TDistManager = class(TDistManagerCustom)
  end; {TDistManager}




  TDistantData = class(TComponent)
  private
    FForm            : TForm;
    FDistConnect     : TDistConnect;
    FServer          : string;
    FCatalog         : string;
    FPassWord        : string;
    FOnBeforeExecute : TNotifyEvent;
    FOnAfterExecute  : TNotifyEvent;
    FOnPrepare       : TNotifyEvent;
    procedure SetCatalog(const Value: string);
    procedure SetPassWord(const Value: string);
    procedure SetServer(const Value: string);

  private
    procedure Initialize;
    function  FormRetrieve: TForm;
    procedure CheckLogin;

  public
    procedure Login;                             { --- Ouvrir en début de run-time pour construire la chaîne de connexion }
    procedure Logoff;                            { --- Il peut être fermé juste après le premier login}
    procedure Prepare;
    function  IsActive: Boolean;

  public
    function  Count(const QueryStr: string):Integer;
    function  DoGetOnFields(const QueryStr: string;
      const Method: TSQLGetStrOnFields; const AProgress: TcxProgressBar = nil):string;
    procedure DoOnFields(const QueryStr: string;
      const Method: TSQLWorkOnFields; const AProgress: TcxProgressBar = nil);
    procedure Execute(const QueryStr: string); overload;
    function  Execute(const TableName: string; const FieldStr: array of string;
      const Values: array of string; IdFieldName: string; IdValue: string = ''): string; overload;
    function  RetrieveValues(const QueryStr: string): string;
    function  RetrieveSetOfValues(const QueryStr: string): string;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Server: string read FServer write SetServer;
    property Catalog: string read FCatalog write SetCatalog;
    property Password: string read FPassWord write SetPassWord;
    property OnBeforeExecute: TNotifyEvent read FOnBeforeExecute write FOnBeforeExecute;
    property OnAfterExecute: TNotifyEvent read FOnAfterExecute write FOnAfterExecute;
    property OnPrepare: TNotifyEvent read FOnPrepare write FOnPrepare;
  end; {TDistantData}



{...............................................................................
                       DATA BASE FIELDS MANAGMENT
...............................................................................}

  TFieldReader = class
  private
    FFields      : TFields;
    FTypes       : string;
    FValues      : string;

    function  GetFieldCount: Integer;
    procedure ReadTypes;
    procedure ReadValues;
    function  GetData(AFields: TFields): string;
    function  GetFieldNamesSep: string;
    function  GetFieldValuesSep: string;
    function  GetFieldNames: string;
    function  GetFieldValues: string;
  public
    procedure SetFields(const Value: TFields);

    property FieldCount: Integer read GetFieldCount;
    property FieldNamesSep: string read GetFieldNamesSep;         //fieldName1, fieldName2, ...
    property FieldNames: string read GetFieldNames; 
    property FieldValuesSep: string read GetFieldValuesSep;       //fieldValue1, fieldValue2, ...
    property FieldValues: string read GetFieldValues;
    property ToData[AFields: TFields]:string read GetData;
    property Types: string read FTypes;                           //fieldName=DataType...
    property Values: string read FValues;                         //fieldName=fieldValue...

    constructor Create;
    class function Datas(const AFields: TFields):string;
    class function ReadFieldNamesSep(const AFields: TFields; var AValues: string):string;
    class function ReadFieldNames(const AFields: TFields; var AValues: string):string;
    class function ReadFieldValuesSep(const AFields: TFields): string;
    class function ReadFieldValues(const AFields: TFields): string;
    class function ReadFieldsTypes(const AFields: TFields): string; //ne le faire que pour un enregistrement - liste en sortie
  end; {TFieldReader}

  TGauge = class
  private
    FProgress    : TcxProgressBar;
    FCursor      : TCursor;
    FModulo      : Integer;
    FHideOnFree  : Boolean;
    procedure Initialise_(const AMax: Integer);
    function  GetPosition: Integer;
  public
    procedure Next;
    procedure Show(const AMax: Integer = 100);
    procedure Hide;

    property Position: Integer read GetPosition;
    property HideOnFree: Boolean read FHideOnFree write FHideOnFree;

    constructor Create(const AProgress: TcxProgressBar = nil);
    destructor Destroy; override;

    class function Initialize(const AProgress: TcxProgressBar; AMax: Integer;
      AHideOnFree: Boolean = True): TGauge;
  end;

  TQueryMaker = class
  { --- Penser au QuotedStr pour les champ de type string dans les "array of string" }
  private
    function InsertString_(const TableName: string; const FieldStr: array of string;
      const Values: array of string; IdFieldName: string): string;

    function UpdateString_(const TableName: string; const FieldStr: array of string;
      const Values: array of string; IdFieldName: string; IdValue: string): string;

  public
    class function InsertString(const TableName: string; const FieldStr: array of string;
      const Values: array of string; IdFieldName: string): string;
    class function UpdateString(const TableName: string; const FieldStr: array of string;
      const Values: array of string; IdFieldName: string; IdValue: string): string;
  end;




























procedure Register;


{...............................................................................
                            GLOBAL VARIABLES
...............................................................................}










implementation

procedure Register;
begin
  RegisterComponents('RaoulDB',  [ TDistantData ]);
end;



const
  C_DISTSQLSTR =
    'Provider=SQLOLEDB.1;Password=%s;Persist Security Info=True;' +
    'User ID=sa;Initial Catalog=%s;Data Source=%s;' +
    'Use Procedure for Prepare=1;Auto Translate=True;' +
    'Packet Size=4096;Workstation ID=DEVSERV;Use Encryption for Data=False;' +
    'Tag with column collation when possible=False';

procedure Delay(ms: Cardinal);
var
  S : Cardinal;
  C : Cardinal;
begin
  C := 0;
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 1 );
      C := C + 5;
      if C mod 90 = 0 then Application.ProcessMessages;
    until Application.Terminated or (GetTickCount > S)
end;

function  NullString(const Value: string):Boolean;
begin
  Result := Trim( Value ) = EmptyStr
end;

function CreateGuid: string;
{ - Les accolades sont retirées - }
var GUID: TGUID;
begin
  Result := EmptyStr;
  if CoCreateGuid(GUID) = S_OK then Result := GUIDToString(GUID);
  Result := Copy(Result, 2, Length(Result)-2 );
end;


function FieldTypeToStr(const Value: TFieldType):string;
begin
  Result := Format('%d', [Integer(Value)])
end;

function ArrayToSepStr(const ASt: array of string): string; overload;
var
  i : Integer;
begin
  Result := EmptyStr;
  for i := 0 to High(ASt) do
    if Result = EmptyStr then Result := ASt[i]
      else Result := Format('%s, %s', [Result, ASt[i]])
end;

function ArrayToSepStr(const AStA, AStB: array of string): string; overload;
var
  i : integer;
begin
  Result := EmptyStr;
  if High(AStA) <> High(AStB) then raise Exception.Create(
     'ArrayRoSepStr : dimensions incompatibles');

  for i := 0 to High(AStA) do
    if Result = EmptyStr then Result := Format('%s = %s', [AStA[i], AStB[i]])
      else Result := Format('%s, %s = %s', [Result, AStA[i], AStB[i]])
end;

{ TDistConnect }

procedure TDistConnect.Close;
begin
  with FADOConnection do Close
end;

constructor TDistConnect.Create;
begin
  inherited Create;
  FStarted       := False;
  FADOConnection := TADOConnection.Create( nil );
  with FADOConnection do LoginPrompt := False
end;

destructor TDistConnect.Destroy;
begin
  if Assigned( FADOConnection ) then FADOConnection.Free;
  inherited
end;

function TDistConnect.GetConnectionString: string;
begin
  with FADOConnection do Result := ConnectionString
end;

procedure TDistConnect.Login(Source, Catalog, PassWord: string);

  procedure DoOnError(const Msg: string); begin
    FStarted := False;
//    Raise Exception.Create(Msg);
  end;

begin
  with FADOConnection do begin
    { --- Pas de connection internet alors TimeOut de 1 seconde sinon par défaut }
    if InternetCheck then ConnectionTimeout := 15 else ConnectionTimeout := 1;
    FServer   := Source;
    FCatalog  := Catalog;
    FPassWord := PassWord;
    ConnectionString := Format(C_DISTSQLSTR, [FPassWord, FCatalog, FServer]);
    try
      Open;
      FStarted := True;
      Delay( SHORT_WAIT )
    except
      on E:Exception do DoOnError(E.Message)
    end;
  end;
end; {Login}

procedure TDistConnect.LogOff;
begin
  if Assigned(FADOConnection) then with FADOConnection do Close
end;

class function TDistConnect.New(AServer, ACatalog,
  APassword: string): TDistConnect;
begin
  Result := TDistConnect.Create;
  try
    Result.Login(AServer, ACatalog, APassword);
  except
  end
end;

procedure TDistConnect.Refresh;
begin
  with FADOConnection do
  try
    Close; Delay(SHORT_SLEEP); Open
  except
  end
end;

{ TDistManagerCustom }

procedure TDistManagerCustom.AssignConnnection(const DistantDB: TDistConnect);

  procedure DoOnError(const Msg: string); begin
//    Raise Exception.Create(Msg);
  end;

begin
  with FConnector do
  try
    LoginPrompt      := False;
    ConnectionString := DistantDB.ConnectionString;
    Open
  except
    on E: Exception do DoOnError( E.Message )
  end
end; {AssignConnnection}

class function TDistManagerCustom.Count(const DistantDB: TDistConnect;
  const QueryStr: string): Integer;
begin
  with TDistManagerCustom.Create( DistantDB ) do
  try
    Result := Count_( QueryStr )
  finally
    Free
  end
end;

constructor TDistManagerCustom.Create(const DistantDB: TDistConnect);
begin
  inherited Create;
  FConnector := TADOConnection.Create(nil);
  AssignConnnection( DistantDB )
end;

function TDistManagerCustom.Count_(const QueryStr: string): Integer;
{ --- select count(*) ... comme requête }
begin
  with TADOQuery.Create(nil) do
  try
    Connection := FConnector;
    with SQL do Text := QueryStr;
    try
      Open;
      if not EOF then Result := Fields[0].AsInteger else Result := 0
    except
      Result := 0;
    end
  finally
    Free
  end
end;

destructor TDistManagerCustom.Destroy;
begin
  FConnector.Free;
  inherited;
end;

class procedure TDistManagerCustom.Execute(const DistantDB: TDistConnect;
  const QueryStr: string);
begin
  with TDistManagerCustom.Create( DistantDB ) do
  try
    if not IsActive then Open;
    SqlExecute( QueryStr )
  finally
    Free
  end
end;

function TDistManagerCustom.RetrieveValues_(const QueryStr: string): string;
begin
  Result := EmptyStr;
  with TADOQuery.Create(nil) do
  try
    Connection := FConnector;
    with SQL do Text := QueryStr;
    try
      Open;
      with TStringList.Create do
      try
        while not EOF do begin
          Add( Fields[0].AsString );
          Next
        end;
        Result := Trim(Text);
      finally
        Free
      end;
    except                                       { --- Exception silencieuse }
    end
  finally
    Free
  end
end;

procedure TDistManagerCustom.SqlExecute(const QueryStr: string);
begin
  with TADOCommand.Create(nil) do
  try
    Connection  := FConnector;
    CommandText := QueryStr;
    try Execute except end                       { --- Exception silencieuse }
  finally
    Free
  end;
end;

class function TDistManagerCustom.RetrieveValues(const DistantDB: TDistConnect;
  const QueryStr: string): string;
begin
  with TDistManagerCustom.Create( DistantDB ) do
  try
    Result := RetrieveValues_( QueryStr )
  finally
    Free
  end
end;

procedure TDistManagerCustom.IterateOnFields(const QueryStr: string;
  Method: TSQLWorkOnFields; const AProgress: TcxProgressBar);
var
  Gauge : TGauge;
begin
  if Assigned(Method) then with TADOQuery.Create(nil) do
  try
    Connection := FConnector;
    with SQL do Text := QueryStr;
    try
      Open;
      Gauge := TGauge.Initialize(AProgress, RecordCount);
      while not EOF do begin
        Method(Self, Fields);
        Next;
        with Gauge do Next
      end;
      with Gauge do Hide
    except
    end
  finally
    with Gauge do Free;
    Free
  end
end; {IterateOnFields}

class procedure TDistManagerCustom.DoOnFields(const DistantDB: TDistConnect;
  const QueryStr: string; Method: TSQLWorkOnFields; const AProgress: TcxProgressBar);
begin
   with TDistManagerCustom.Create( DistantDB ) do
  try
    IterateOnFields(QueryStr, Method, AProgress)
  finally
    Free
  end
end;

function TDistManagerCustom.GetStrOnFields(const QueryStr: string;
  Method: TSQLGetStrOnFields; const AProgress: TcxProgressBar): string;
var
  Gauge : TGauge;
  ASt   : string;
begin
  Result := EmptyStr;
  if Assigned(Method) then with TADOQuery.Create(nil) do
  try
    Connection := FConnector;
    with SQL do Text := QueryStr;
    try
      Open;
      Gauge := TGauge.Initialize(AProgress, RecordCount);
      with TStringList.Create do
      try
        while not EOF do begin
          Method(Self, Fields, ASt);
          if Ast <> EmptyStr then Add(Ast);
          Next;
          with Gauge do Next
        end;
        Result := Text;
        with Gauge do Hide
      finally
        Free
      end
    except
    end
  finally
    Gauge.Free;
    Free
  end
end; {GetStrOnFields}

class function TDistManagerCustom.DoGetOnFields(const DistantDB: TDistConnect;
  const QueryStr: string; Method: TSQLGetStrOnFields;
  const AProgress: TcxProgressBar): string;
begin
  with TDistManagerCustom.Create(DistantDB) do
  try
    Result := GetStrOnFields(QueryStr, Method, AProgress)
  finally
    Free
  end
end;

function TDistManagerCustom.IsActive: Boolean;
begin
  Result := False;
  if Assigned(FConnector) then with FConnector do Result := Connected
end;

procedure TDistManagerCustom.Open;
begin
  if Assigned(FConnector) then with FConnector do Open
end;

procedure TDistManagerCustom.Close;
begin
  if Assigned(FConnector) then with FConnector do Close;
end;

function TDistManagerCustom.RetrieveSetOfValues_(
  const QueryStr: string): string;
var
  i : Integer;
{ --- L'ensemble de résultats ne doit contenir qu'une ligne sinon que la première ligne}
begin
  Result := EmptyStr;
  with TADOQuery.Create(nil) do
  try
    Connection := FConnector;
    with SQL do Text := QueryStr;
    try
      Open;
      with TStringList.Create do
      try
        while not EOF do begin
          for i := 0 to Pred(Fields.Count) do
            Add( Format('%s=%s', [Fields[i].FieldName, Fields[i].AsString ]) );
          Break                                  { --- On se limite qu'à la première ligne }
        end;
        Result := Trim(Text);
      finally
        Free
      end;
    except                                       { --- Exception silencieuse }
    end
  finally
    Free
  end
end;

class function TDistManagerCustom.RetrieveSetOfValues(
  const DistantDB: TDistConnect; const QueryStr: string): string;
begin
  with TDistManagerCustom.Create( DistantDB ) do
  try
    Result := RetrieveSetOfValues_( QueryStr )
  finally
    Free
  end
end;

{ TFieldReader }

constructor TFieldReader.Create;
begin
  inherited Create;
  FFields := nil;
  FTypes  := EmptyStr;
end;

class function TFieldReader.Datas(const AFields: TFields): string;
begin
  with TFieldReader.Create do
  try
    Result := ToData[AFields]
  finally
    Free
  end
end;

function TFieldReader.GetData(AFields: TFields): string;
begin
  try
    FFields := AFields;
    ReadValues;
    Result  := Values;
  except
    Result := EmptyStr
  end
end;

function TFieldReader.GetFieldCount: Integer;
begin
  try
    Result := FFields.Count
  except
    Result := 0
  end
end;

function TFieldReader.GetFieldNames: string;
var
  I: Integer;
begin
  Result := EmptyStr;
  if FieldCount = 0 then Exit;

  with TStringList.Create do
  try
    for I := 0 to Pred(FieldCount) do Add( FFields[I].FieldName );
    Result := Text;
  finally
    Free
  end
end;

function TFieldReader.GetFieldNamesSep: string;
var
  I : Integer;
begin
  Result := EmptyStr;
  if FieldCount = 0 then Exit;

  for I := 0 to Pred(FieldCount) do
    if Result = EmptyStr then Result := FFields[I].FieldName
      else Result := Format('%s, %s', [Result, FFields[I].FieldName])
end;

function TFieldReader.GetFieldValues: string;
var
  I : Integer;

  function GetValue(const index: Integer):string; begin
    Result := FFields[index].AsString;
    if FFields[index].DataType in [ftString, ftWideString]
      then Result := QuotedStr(Result)
  end;

begin
  Result := EmptyStr;
  if FieldCount = 0 then Exit;

  with TstringList.Create do
  try
    for I := 0 to Pred(FieldCount) do Add( GetValue(I) );
    Result := Text
  finally
    Free
  end
end; {GetFieldValues}

function TFieldReader.GetFieldValuesSep: string;
var
  I : Integer;

  function GetValue(const index: Integer):string; begin
    Result := FFields[index].AsString;
    if FFields[index].DataType in [ftString, ftWideString]
      then Result := QuotedStr(Result)
  end;

begin
  Result := EmptyStr;
  if FieldCount = 0 then Exit;

  for I := 0 to Pred(FieldCount) do
    if Result = EmptyStr then Result := GetValue(I)
      else Result := Format('%s, %s', [Result, GetValue(I)])
end; {GetFieldValues}

class function TFieldReader.ReadFieldNames(const AFields: TFields;
  var AValues: string): string;
begin
  with TFieldReader.Create do
  try
    SetFields(AFields);
    Result  := FieldNames;
    AValues := FieldValues
  finally
    Free
  end
end;

class function TFieldReader.ReadFieldNamesSep(const AFields: TFields;
  var AValues: string): string;
begin
  with TFieldReader.Create do
  try
    SetFields(AFields);
    Result  := FieldNamesSep;
    AValues := FieldValuesSep
  finally
    Free
  end
end;

class function TFieldReader.ReadFieldsTypes(const AFields: TFields): string;
begin
  with TFieldReader.Create do
  try
    SetFields(AFields);
    ReadTypes;
    Result := FTypes
  finally
    Free
  end
end;

class function TFieldReader.ReadFieldValues(const AFields: TFields): string;
begin
  with TFieldReader.Create do
  try
    SetFields(AFields);
    Result := FieldValues
  finally
    Free
  end
end;

class function TFieldReader.ReadFieldValuesSep(const AFields: TFields): string;
begin
  with TFieldReader.Create do
  try
    SetFields(AFields);
    Result := FieldValuesSep
  finally
    Free
  end
end;

procedure TFieldReader.ReadTypes;
var
  I : Integer;
begin
  FTypes := EmptyStr;
  if Assigned(FFields) then with TStringList.Create do
  try
    for I := 0 to Pred(FieldCount) do
      Add( Format('%s=%s', [ FFields[I].FieldName,
                             FieldTypeToStr(FFields[I].DataType) ]));
    FTypes := Text
  finally
    Free
  end
end;

procedure TFieldReader.ReadValues;
{ --- Lit tous les champs indiqués dans la clause select }
var
  I : Integer;
begin
  FValues := EmptyStr;
  if Assigned(FFields) then with TStringList.Create do
  try
    for I := 0 to Pred(FieldCount) do
      Add( Format('%s=%s', [ FFields[I].FieldName,
                             FFields[I].AsString ]));
    FValues := Text
  finally
    Free
  end
end;

procedure TFieldReader.SetFields(const Value: TFields);
begin
  FFields := Value;
  if FTypes = EmptyStr then ReadTypes;
  ReadValues;
end;

{ TDistantData }

procedure TDistantData.CheckLogin;
begin
  if NullString( FServer ) or NullString( FCatalog ) then
    raise Exception.Create('Veuillez indiquer un nom de serveur et un nom de catalog');
end;
                                                    
function TDistantData.Count(const QueryStr: string): Integer;
begin
   Result := TDistManagerCustom.Count(FDistConnect, QueryStr)
end;

constructor TDistantData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := FormRetrieve;
  Initialize;
end;

destructor TDistantData.Destroy;
begin
  if Assigned( FDistConnect ) then FDistConnect.Free;
  inherited;
end;

function TDistantData.DoGetOnFields(const QueryStr: string;
  const Method: TSQLGetStrOnFields; const AProgress: TcxProgressBar): string;
begin
  if Assigned(FOnBeforeExecute) then FOnBeforeExecute(Self);
  Result := TDistManagerCustom.DoGetOnFields(FDistConnect, QueryStr, Method, AProgress);
  if Assigned(FOnAfterExecute) then FOnAfterExecute(Self)
end;

procedure TDistantData.DoOnFields(const QueryStr: string;
  const Method: TSQLWorkOnFields; const AProgress: TcxProgressBar);
begin
  if Assigned(FOnBeforeExecute) then FOnBeforeExecute(Self);
  TDistManagerCustom.DoOnFields(FDistConnect, QueryStr, Method, AProgress);
  if Assigned(FOnAfterExecute) then FOnAfterExecute(Self)
end;

procedure TDistantData.Execute(const QueryStr: string);
begin
  if Assigned(FOnBeforeExecute) then FOnBeforeExecute(Self);
  TDistManagerCustom.Execute(FDistConnect, QueryStr);
  if Assigned(FOnAfterExecute) then FOnAfterExecute(Self)
end;

function TDistantData.Execute(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName, IdValue: string): string;
begin
  if IdValue = EmptyStr
    then Result := TQueryMaker.InsertString(TableName, FieldStr, Values, IdFieldName)
    else Result := TQueryMaker.UpdateString(TableName, FieldStr, Values, IdFieldName, IdValue);
  Execute( Result );
end;

function TDistantData.FormRetrieve: TForm;
var
  X : TComponent;
begin
  Result := nil;
  X      := Owner;
  while Assigned(X) and not (X is TForm) do X := X.Owner;
  if Assigned(X) and (X is TForm) then Result := TForm(X);
end;

procedure TDistantData.Initialize;
begin
  FDistConnect := TDistConnect.Create;
  FServer      := EmptyStr;
  FCatalog     := EmptyStr;
  FPassWord    := EmptyStr
end;

function TDistantData.IsActive: Boolean;
begin
  Result := False;
  if Assigned(FDistConnect) then with FDistConnect, FADOConnection do
    Result := Connected
end;

procedure TDistantData.Login;
begin
  CheckLogin;
  with FDistConnect do Login(Server, Catalog, PassWord)
end;

procedure TDistantData.Logoff;
begin
  with FDistConnect do LogOff
end;

procedure TDistantData.Prepare;
begin
  try
    Login;
    if Assigned(FOnPrepare) then FOnPrepare(Self)
  finally
    Logoff
  end
end;

function TDistantData.RetrieveSetOfValues(const QueryStr: string): string;
begin
  Result := TDistManagerCustom.RetrieveSetOfValues(FDistConnect, QueryStr)
end;

function TDistantData.RetrieveValues(const QueryStr: string): string;
begin
  Result := TDistManagerCustom.RetrieveValues(FDistConnect, QueryStr)
end;

procedure TDistantData.SetCatalog(const Value: string);
begin
  FCatalog := Value
end;

procedure TDistantData.SetPassWord(const Value: string);
begin
  FPassWord := Value
end;

procedure TDistantData.SetServer(const Value: string);
begin
  FServer := Value
end;

{ TGauge }

constructor TGauge.Create(const AProgress: TcxProgressBar);
begin
  inherited Create;
  FProgress   := AProgress;
  FHideOnFree := True;
end;

destructor TGauge.Destroy;
begin
  if FHideOnFree then if Assigned(FProgress) then with FProgress do Visible := False;
  inherited;
end;

function TGauge.GetPosition: Integer;
begin
  with FProgress do Result := Trunc( Position )
end;

procedure TGauge.Hide;
begin
  if Assigned(FProgress) then begin
    with FProgress do Position := Properties.Max;
    Application.ProcessMessages;
    Delay( WAIT_GAUGE );
  end;
  with Screen do if Cursor = crHourGlass then Cursor := FCursor;
end;

procedure TGauge.Initialise_(const AMax: Integer);
begin
  with FProgress do begin
    Position := 0;
    FModulo  := AMax div 100;
    if FModulo = 0 then FModulo := 1;
    with Properties do Max := AMax;
  end
end;

class function TGauge.Initialize(const AProgress: TcxProgressBar;
  AMax: Integer; AHideOnFree: Boolean): TGauge;
begin
  Result := TGauge.Create(AProgress);
  with Result do begin
    HideOnFree := AHideOnFree;
    Show(AMax)
  end
end;

procedure TGauge.Next;
begin
  if Assigned(FProgress) then with FProgress do begin
    Position := Position + 1;
    if Trunc(Position) mod FModulo = 0 then Application.ProcessMessages;
  end
end;

procedure TGauge.Show(const AMax: Integer);
begin
  with Screen do if Cursor <> crSQLWait then begin
    FCursor := Cursor;
    Cursor  := crHourGlass;
  end;
  if Assigned(FProgress) then with FProgress do begin
    Position := 0;
    Visible  := True;
    Application.ProcessMessages;
    Initialise_(AMax);
  end;
end;

{ TQueryMaker }

const
  PROTO_INSERT = 'insert into %s (%s) values (%s)';

class function TQueryMaker.InsertString(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName: string): string;
begin
  with TQueryMaker.Create do
  try
    Result := InsertString_(TableName, FieldStr, Values, IdFieldName)
  finally
    Free
  end
end;

function TQueryMaker.InsertString_(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName: string): string;
{ --- chaque champ sauf id doit avoir une valeur par défaut dans la base (définition) }
var
  indexId: Integer;

  procedure Check; begin
    if High(FieldStr) <> High(Values) then raise Exception.CreateFmt(
         'Insert : dimensions incompatibles champs(%d), valeurs(%s) ',
         [High(FieldStr), High(Values)]);

    indexId := IndexStr(IdFieldName, FieldStr);
    if indexId <> -1 then raise Exception.Create(
         'le Champ Id ne peut faire partie du tableau FieldStr')
  end;

  function FullFields:string; begin
    Result := ArrayToSepStr([IdFieldName, ArrayToSepStr(FieldStr)])
  end;

  function FullValues:string; begin
    Result := ArrayToSepStr([QuotedStr(CreateGuid), ArrayToSepStr(Values)])
  end;

begin
  Check;
  Result := Format(PROTO_INSERT, [ TableName, FullFields, FullValues ])
end; {InsertString}




const
  PROTO_UPDATE = 'update %s set %s where %s';

class function TQueryMaker.UpdateString(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName, IdValue: string): string;
begin
  with TQueryMaker.Create do
  try
    Result := UpdateString_(TableName, FieldStr, Values, IdFieldName, IdValue)
  finally
    Free
  end
end;

function TQueryMaker.UpdateString_(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName, IdValue: string): string;

var
  indexId: Integer;

  procedure Check; begin
    if High(FieldStr) <> High(Values) then raise Exception.CreateFmt(
         'Insert : dimensions incompatibles champs(%d), valeurs(%s) ',
         [High(FieldStr), High(Values)]);

    indexId := IndexStr(IdFieldName, FieldStr);
    if indexId <> -1 then raise Exception.Create(
         'le Champ Id ne peut faire partie du tableau FieldStr')
  end;

  function WhereStr: string; begin
    Result := Format('%s = %s', [IdFieldName, QuotedStr(IdValue)])
  end;

begin
  Check;
  Result := Format(PROTO_UPDATE ,
    [TableName, ArrayToSepStr(FieldStr, Values), WhereStr])
end; {UpdateString}

end.
