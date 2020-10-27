unit uRaoulDB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRaoulDisplay, ExtCtrls, StrUtils, ClipBrd,
  {Register access}
  uRegistry,
  {dos functions, et versioning}
  uDosUtils, uVersion,
  {Accès SQL distant}
  uSQLTransaction;

type
  TRegDefToSql =
    (rdt_sensibility,        rdt_alignmenthoriz,          rdt_alignmentvertic,
     rdt_commandtext,        rdt_commandview,             rdt_textview,
     rdt_soundview,          rdt_indexmonitor,            rdt_gridmonitor,
     rdt_pen);

  TRegSqlStr      = array[TRegDefToSql] of string;


type
  TCustomSQLRaoul = class                                                  
  private
    FDD     : TDistantData;
    FRegDef : TRegSqlStr;
    FRegVal : TRegSqlStr;
  private
    procedure UpdateRegVal;

  protected
    {Divers}
    procedure SetInterfaceSQLDataToReg;virtual;

    {Users}
    function  IdFromUser(const APseudo: string):string; virtual;
    function  PseudoFromId(const AId: string):string; virtual;
    function  IdFromId(const AId: string):string; virtual;
    procedure ChangeId(const OldId, NewId: string);
    function  GetDataForReg(const AId: string):string; virtual;

    {Register}
    function IsRegistered(const AIp, AMac: string; var AId: string):Boolean; virtual;

    procedure Execute(const QueryStr: string); overload; virtual;
    procedure Execute(const TableName: string; const FieldStr,
      Values: array of string; IdFieldName: string; IdValue: string = ''); overload; virtual;
  public
    procedure SetDistantData(const Value: TDistantData);

    constructor Create;
  end;

  TSQLRaoul = class(TCustomSQLRaoul)
  private
    {Table Users}
    function  AddNewUser(const APseudo: string):string;
    function  TryRetrievePseudo:string;
    function  RetrievePseudo: string;
    function  CheckUserId: string;

    {Table Register}
    function  CheckRegisterId: string;

  public
    {Table Users}
    procedure AddIntoUsers(const Fields, Values: array of string);
    procedure VersionToSql;
    procedure UpdateVersionSql;
    procedure UpdateLastConnexion;
    procedure RegToSql;
    procedure IpToSql;
    procedure MacToSql;
    procedure IpMacToSql;

    class procedure Inscription;
    class procedure UserDataExport;
    class function  ImportDataForReg:string;

    {Table Register}
    procedure AddIntoRegister(const Fields, Values: array of string);
    function  RetrieveIdUser: string;
    function  GetAuthorized: Integer;

    class procedure CheckRegistration;

    {Table Application}
    function  RetrieveVersion: string;
    function  RetrieveBuild: string;
    function  RetrieveCatalog: string;
    {Grammar update}
    function  RetrievegrUpdateCount: Integer;
    function  RetrievegrCatalog: string;
    function  RetrievegrTempCatalog: string;
    procedure RetrievegrData;

  end;

function  Get911Pseudo:string;
function  GetUserId: string;
procedure IpMacInitialize;                       { --- Invoquer dans le crate de la mainForm }


var
  SQLRaoul : TSQLRaoul = nil;

implementation

function RaoulKey: string;
begin
  Result := Format('Software\%s', [ 'Raoul' ])
end;


function Get911Pseudo:string;
var
  Key: string;
begin
  Key := Format('Software\%s', [ '911WoJ' ]);
  Result := KeyReadString(Key, 'Nick');
end;

procedure IpMacInitialize;
begin
  KeyWrite(ParamKey, 'IP',  GetIpPublic);
  KeyWrite(ParamKey, 'MAC', GetMACAddress);
end;

function GetIp: string;
begin
  Result := KeyReadString(ParamKey, 'IP')
end;

function GetMac: string;
begin
  Result := KeyReadString(ParamKey, 'MAC')
end;

function GetUserId: string;
begin
  Result := KeyReadString(AppKey, 'Id')
end;

{ TCustomSQLRaoul }

procedure TCustomSQLRaoul.Execute(const QueryStr: string);
begin
  with FDD do Execute( QueryStr )
end;

var
  RegNameDef : TRegSqlStr = (
     'Sensibility',          'AlignmentHoriz',       'AlignmentVertic',
     'TalkativeCommandText', 'TalkativeCommandView', 'TalkativeView',
     'TalkativeSoundView',   'IndexMonitor' ,        'GridMonitor',
     'Pen'
    );

constructor TCustomSQLRaoul.Create;
begin
  inherited Create;
  FRegDef := RegNameDef;
end;

procedure TCustomSQLRaoul.Execute(const TableName: string; const FieldStr,
  Values: array of string; IdFieldName, IdValue: string);
begin
  with FDD do Execute(TableName, FieldStr, Values, IdFieldName, IdValue)
end;

function TCustomSQLRaoul.IdFromUser(const APseudo: string): string;
begin
  with FDD do Result := RetrieveValues(
    Format('select Id from Users where Pseudo = %s', [ QuotedStr(APseudo) ])
  )
end;

procedure TCustomSQLRaoul.SetDistantData(const Value: TDistantData);
begin
  FDD := Value
end;

procedure TCustomSQLRaoul.UpdateRegVal;

  function ReadIntValue(const Key, AName: string):string; begin
    Result := IntToStr( KeyReadInt(Key, AName, 0) )
  end;

  function ReadStrValue(const Key, AName: string):string; begin
    Result := QuotedStr( KeyReadString(Key, AName) )
  end;

begin
  FRegVal[rdt_sensibility]     := ReadIntValue(IniKey,    FRegDef[rdt_sensibility]);
  FRegVal[rdt_alignmenthoriz]  := ReadIntValue(BufferKey, FRegDef[rdt_alignmenthoriz]);
  FRegVal[rdt_alignmentvertic] := ReadIntValue(BufferKey, FRegDef[rdt_alignmentvertic]);
  FRegVal[rdt_commandtext]     := ReadStrValue(BufferKey, FRegDef[rdt_commandtext]);
  FRegVal[rdt_commandview]     := ReadIntValue(BufferKey, FRegDef[rdt_commandview]);
  FRegVal[rdt_textview]        := ReadIntValue(BufferKey, FRegDef[rdt_textview]);
  FRegVal[rdt_soundview]       := ReadIntValue(BufferKey, FRegDef[rdt_soundview]);
  FRegVal[rdt_indexmonitor]    := ReadIntValue(BufferKey, FRegDef[rdt_indexmonitor]);
  FRegVal[rdt_gridmonitor]     := ReadIntValue(ParamKey,  FRegDef[rdt_gridmonitor]);
  FRegVal[rdt_pen]             := ReadIntValue(AppKey,    FRegDef[rdt_pen]);
end; {UpdateRegVal}

function TCustomSQLRaoul.IdFromId(const AId: string): string;
begin
  with FDD do Result := RetrieveValues(
    Format('select Id from Users where Id = %s', [ QuotedStr(AId) ])
  )
end;

procedure TCustomSQLRaoul.ChangeId(const OldId, NewId: string);
begin
  Execute( Format('update Users set Id = %s where Id = %s', [QuotedStr(NewId), QuotedStr(OldId)]) )
end;

function TCustomSQLRaoul.IsRegistered(const AIp, AMac: string;
  var AId: string): Boolean;
begin
  with FDD do AId := RetrieveValues(
    Format('select Id from Register where IP = %s and MAC = %s', [ QuotedStr(AIp), QuotedStr(AMac) ])
  );
  Result := AId <> EmptyStr 
end;


function TCustomSQLRaoul.PseudoFromId(const AId: string): string;
begin
  with FDD do Result := RetrieveValues(
    Format('select Pseudo from Users where Id = %s', [ QuotedStr(AId) ])
  )
end;

function TCustomSQLRaoul.GetDataForReg(const AId: string): string;
begin
  with FDD do Result := RetrieveSetOfValues(
    Format(
     'select ' +
       'Sensibility, AlignmentHoriz, AlignmentVertic, TalkativeCommandView, '+
       'TalkativeView, TalkativeSoundView, IndexMonitor, GridMonitor '+
     'from Users where Id = %s', [ QuotedStr(AId) ])
  )
end;


var
  REG_FIELDS : array[0..7] of string =
   ('Sensibility',   'AlignmentHoriz',     'AlignmentVertic', 'TalkativeCommandView',
    'TalkativeView', 'TalkativeSoundView', 'IndexMonitor',    'GridMonitor'
   );

procedure TCustomSQLRaoul.SetInterfaceSQLDataToReg;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    Text := TSQLRaoul.ImportDataForReg;
    for i := 0 to Pred(Count) do
      case IndexStr(Names[i], REG_FIELDS) of
        0    : KeyWrite(IniKey,    Names[i], StrToInt(ValueFromIndex[i]));
        1..6 : KeyWrite(BufferKey, Names[i], StrToInt(ValueFromIndex[i]));
        7    : KeyWrite(ParamKey,  Names[i], StrToInt(ValueFromIndex[i]));
      end
  finally
    Free
  end
end;

{ TSQLRaoul }

procedure TSQLRaoul.AddIntoRegister(const Fields, Values: array of string);
var
  AId: string;
begin
  AId := CheckRegisterId;
  Execute('Register', Fields, Values, 'Id', AId)
end;

procedure TSQLRaoul.AddIntoUsers(const Fields, Values: array of string);
var
  AId: string;
begin
  AId := CheckUserId;
  Execute('Users', Fields, Values, 'Id', AId)
end;

function TSQLRaoul.AddNewUser(const APseudo: string): string;
var
  Pseudo  : string;
  QPseudo : string;
  Tstr    : string;
  Version : string;
begin
  { --- Définir le pseudo}
  Pseudo  := KeyReadString(AppKey, 'Pseudo');
  QPseudo := QuotedStr( PSeudo );

  { --- Mettre à jour la table distante Users }
  Tstr    := QuotedStr( DateTimeToStr(Now) );
  Version := QuotedStr( ApplicationVersion(False) );
  Execute('Users', ['Pseudo', 'InstalDate', 'UpdateDate', 'Version', 'LastConnexion' ],
                   [ QPseudo,  Tstr,         Tstr,         Version,   Tstr           ],
          'Id' );
  { --- Retrouver l'id et l'inscrire en local dans le registre}
  Result := IdFromUser( Pseudo );
  KeyWrite(AppKey, 'Id', Result);
end;

function TSQLRaoul.CheckRegisterId: string;
begin
  IsRegistered(GetIp, GetMac, Result);
end;

class procedure TSQLRaoul.CheckRegistration;
begin
  if Assigned(SQLRaoul) then with SQLRaoul do begin
    if CheckRegisterId = EmptyStr then AddIntoRegister(
       ['IP',             'MAC',             'InstalDate',              'UserId'],
       [ QuotedStr(GetIp), QuotedStr(GetMac), QuotedStr(DateToStr(Now)), QuotedStr(GetUserId)])
  end
end;

function TSQLRaoul.CheckUserId: string;
var
  Pseudo : string;
  DistId : string;
  Found  : Boolean;

  function DataServerNewAgain(const Value: string):string; begin
    Result := Value;
    DistId := IdFromId( Value );
    if DistId = EmptyStr then begin
      {on reconstruit la table en renomant l'id avec Id déjà réservé}
      Result := AddNewUser(Pseudo);
      ChangeId(Result, Value);
      Result := Value;
      KeyWrite(AppKey, 'Id', Result);
    end
  end;

  function DataServerCreate(const Value: string):string; begin
    Result := Value;
    {Existe-t-il un Pseudo Dans la base de registre de l'utilisateur}
    Result := IdFromUser( Pseudo );
    if Result = EmptyStr then Result := AddNewUser( Pseudo )
  end;

begin
  Result := RetrieveIdUser;
  Found  := Result <> EmptyStr;
  Pseudo := RetrievePseudo;

  if Found then DataServerNewAgain( Result ) else Result := DataServerCreate( Result );
  KeyWrite(AppKey, 'Id', Result);
end; {CheckId}

function TSQLRaoul.GetAuthorized: Integer;
begin
  with FDD do Result := StrToIntDef( RetrieveValues(
    Format('select Authorized from Register where IP = %s and MAC = %s', [ QuotedStr(GetIp), QuotedStr(GetMac) ])
  ), 0)
end;

class function TSQLRaoul.ImportDataForReg:string;
begin
  if Assigned(SQLRaoul) then with SQLRaoul do Result := GetDataForReg( GetUserId )
end;

class procedure TSQLRaoul.Inscription;
begin
  if Assigned(SQLRaoul) then with SQLRaoul do begin
    CheckUserId;
    IpMacToSql;
    UpdateLastConnexion;
    SetInterfaceSQLDataToReg;
    RetrieveVersion;            { --- Load version numero from dbo.Applicationn }
    RetrieveBuild;              { --- Load new build numero from dbo.Applicationn }
    RetrieveCatalog;            { --- Load modified files for Application update }
    RetrievegrData;             { --- Load datas for grammar update}
  end
end;

procedure TSQLRaoul.IpMacToSql;
var
  MonIp: string;
  MonAdresseMAC : string;
begin
  MonAdresseMAC := QuotedStr( GetMACAddress );
  MonIp         := QuotedStr( GetIpPublic );
  Execute('Users', ['IP', 'MAC'], [MonIp, MonAdresseMAC], 'Id', CheckUserId)
end;

procedure TSQLRaoul.IpToSql;
var
  MonIp: string;
begin
  MonIp := QuotedStr( GetIpPublic );
  Execute('Users', ['IP'], [MonIp], 'Id', CheckUserId)
end;

procedure TSQLRaoul.MacToSql;
var
  MonAdresseMAC : string;
begin
  MonAdresseMAC := QuotedStr( GetMACAddress );
  Execute('Users', ['MAC'], [MonAdresseMAC], 'Id', CheckUserId)
end;

procedure TSQLRaoul.RegToSql;
begin
  UpdateRegVal;
  Execute('Users', FRegDef, FRegVal, 'Id', CheckUserId)
end;

function TSQLRaoul.RetrieveBuild: string;
var
  LocalBuild: Integer;
  Action    : Integer;
begin
  with FDD do Result := RetrieveValues(
    'select Build from Application'
  );
  KeyWrite(AppKey, 'LastBuild', Result);
  LocalBuild := GetBuildVersion;
  UpdateVersionSql;     { --- Update dbo.Users avec la version actuelle de Raoul }
  if LocalBuild < StrToInt(Result) then Action := 1  { --- Mise à jour nécéssaire}
    else
  if LocalBuild > StrToInt(Result) then Action := 2  { --- En version béta ne rien faire}
    else Action := 0;                                { --- A jour ne rien faire}
  KeyWrite(Appkey, 'UpdateAction', Action);
end;

function TSQLRaoul.RetrieveCatalog: string;
begin
  with FDD do Result := RetrieveValues(
    'select Catalog from Application'
  );
  KeyWrite(RaoulKey, 'Catalog', Result)
end;

function TSQLRaoul.RetrievegrCatalog: string;
begin
  with FDD do Result := RetrieveValues(
    'select grCatalog from Application'
  );
  KeyWrite(Appkey, 'DBgrCatalog', Result)
end;

procedure TSQLRaoul.RetrievegrData;
begin
  RetrievegrUpdateCount;
  RetrievegrCatalog;
  RetrievegrTempCatalog;
end;

function TSQLRaoul.RetrievegrTempCatalog: string;
begin
  with FDD do Result := RetrieveValues(
    'select grTempCatalog from Application'
  );
  KeyWrite(Appkey, 'DBgrTempCatalog', Result)
end;

function TSQLRaoul.RetrievegrUpdateCount: Integer;
var
  ASt: string;
begin
  with FDD do ASt := RetrieveValues(
    'select grUpdateCout from Application' );
  try Result := StrToInt(ASt) except Result := 0 end;
  KeyWrite(Appkey, 'DBgrUpdateCount', Result)
end;

function TSQLRaoul.RetrieveIdUser: string;
begin
  with FDD do Result := RetrieveValues(
    Format('select UserId from Register where IP = %s and MAC = %s', [ QuotedStr(GetIp), QuotedStr(GetMac) ])
  );
end;

function TSQLRaoul.RetrievePseudo: string;
begin
  Result := KeyReadString(AppKey, 'Pseudo');
  if Result = EmptyStr then Result := TryRetrievePseudo
end;

function TSQLRaoul.RetrieveVersion: string;
begin
  with FDD do Result := RetrieveSetOfValues(
    'select Version, Build from Application'
  );
  with TStringList.Create do
  try
    Text := Result;
    if Count = 2 then Result := Format('%s.%s', [ ValueFromIndex[0], ValueFromIndex[1] ])
      else Result := 'unknown';
    KeyWrite(AppKey, 'LastVersion', Result)
  finally
    Free
  end
end;

function TSQLRaoul.TryRetrievePseudo: string;
begin
  if Result = EmptyStr then Result := Get911Pseudo;
  if Result = EmptyStr
    then begin
      Result := CreateGuid;
      KeyWrite(AppKey, 'PseudoUnknown', True);
      KeyWrite(AppKey, 'Pseudo', Result);
    end else begin
      KeyWrite(AppKey, 'PseudoUnknown', False);
      KeyWrite(AppKey, 'Pseudo', Result);
    end
end;

procedure TSQLRaoul.UpdateLastConnexion;
begin
  AddIntoUsers(['LastConnexion'], [QuotedStr( DateTimeToStr(Now) )] )
end;

procedure TSQLRaoul.UpdateVersionSql;
begin
  AddIntoUsers(
    ['Version',                            'UpdateDate'],
    [ QuotedStr(ApplicationVersion(False)), QuotedStr(TimeToStr(Now))] )
end;

class procedure TSQLRaoul.UserDataExport;
begin
  if Assigned(SQLRaoul) then with SQLRaoul do RegToSql;
end;

procedure TSQLRaoul.VersionToSql;
begin
  AddIntoUsers( ['Version'], [QuotedStr(ApplicationVersion(False))] )
end;

end.
