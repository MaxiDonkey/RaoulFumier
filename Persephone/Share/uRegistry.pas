unit uRegistry;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Registry, Dialogs;

procedure KeyCreate(AKey: string);
procedure KeyRead(AKey: string; KeyName: string; var Value: string); overload;
procedure KeyRead(AKey: string; KeyName: string; Default: string; var Value: string); overload;
procedure KeyWrite(AKey: string; KeyName: string; Value: string); overload;
procedure KeyWrite(AKey: string; KeyName: string; Value: Double); overload;
procedure KeyWrite(AKey: string; KeyName: string; Value: Integer); overload;
procedure KeyWrite(AKey: string; KeyName: string; Value: Boolean); overload;
procedure KeyWrite(AKey: string; KeyName: string; Value: Cardinal); overload;

function KeyReadInt(AKey: string; KeyName: string; Default: Integer = 0):Integer;
function KeyReadFloat(AKey: string; KeyName: string; Default: Double = 0.0):Double;
function KeyReadBoolean(AKey: string; KeyName: string; Default: Boolean = False):Boolean;
function KeyReadString(AKey: string; KeyName: string; Default: string = ''):string;
function KeyReadCard(AKey: string; KeyName: string; Default: Cardinal = 0):Cardinal;

procedure KeyDeleteValue(AKey: string; KeyName: string);

var
  AppKey    : string;
  ParamKey  : string;
  IniKey    : string;
  BufferKey : string;

implementation

uses
  StrCopyUtils;

function GetExeName:string;
{in uDos}
begin
  with Application do Result := GetBeforStr( ExtractFileName( ExeName ), '.' )
end;

function GetExeDir:string;
{in uDos}
begin
  with Application do Result := ExtractFileDir( ExeName )
end;

procedure Initialize;
{Registry keys initialize}
begin
  AppKey    := Format('Software\%s', [ GetExeName ]);
  ParamKey  := Format('%s\%s', [ AppKey, 'Params' ]);
  IniKey    := Format('%s\%s', [ AppKey, 'Ini' ]);
  BufferKey := Format('%s\%s', [ AppKey, 'Buffer' ]);

  KeyCreate( AppKey );
  KeyCreate( ParamKey );
  KeyCreate( IniKey );
  KeyCreate( BufferKey );
end;

{REGISTRY}

function RKeyExists(AKey: string):Boolean;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    Result  := KeyExists( AKey );
  finally
    Free;
  end;
end;

procedure KeyCreate(AKey: string);
begin
  if not RKeyExists(AKey) then
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    CreateKey( AKey );
  finally
    Free;
  end;
end;

procedure KeyDeleteValue(AKey: string; KeyName: string);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    try
      OpenKey( AKey, False);
      DeleteValue(KeyName);
    except
    end;
  finally
    Free;
  end;
end;

procedure KeyRead(AKey: string; KeyName: string; var Value: string);
begin
  Value := EmptyStr;
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    try
      OpenKey( AKey, False);
      Value := ReadString( KeyName );
    except
    end;
  finally
    Free;
  end;
end;

function KeyNameValueExists(AKey: string; KeyName: string):Boolean;
var
  L : TStringList;
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    OpenKey( AKey, False);
    try
      L := TStringList.Create;
      with L do
      try
        GetValueNames( L );
        Result := IndexOf(KeyName) > -1
      finally
        Free
      end
    except
    end
  finally
    Free
  end;
end;

procedure KeyRead(AKey: string; KeyName: string; Default: string; var Value: string);
begin
  if not KeyNameValueExists(AKey, KeyName)
    then Value := Default
    else KeyRead(AKey, KeyName, Value);
end;

procedure KeyWrite(AKey: string; KeyName: string; Value: string);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_CURRENT_USER;
    try
      OpenKey( AKey, False);
      WriteString(KeyName, Value );
    except
    end;
  finally
    Free
  end;
end;

procedure KeyWrite(AKey: string; KeyName: string; Value: Double);
begin
  KeyWrite(AKey, KeyName, FloatToStr(Value) )
end;

procedure KeyWrite(AKey: string; KeyName: string; Value: Integer);
begin
  KeyWrite(AKey, KeyName, IntToStr(Value) )
end;

procedure KeyWrite(AKey: string; KeyName: string; Value: Cardinal);
begin
  KeyWrite(AKey, KeyName, IntToStr(Value) )
end;

procedure KeyWrite(AKey: string; KeyName: string; Value: Boolean);
begin
  KeyWrite(AKey, KeyName, Integer(Value) )
end;

function KeyReadInt(AKey: string; KeyName: string; Default: Integer):Integer;
var
  Value : string;
begin
  KeyRead(AKey, KeyName, Value);
  try
    if Value = EmptyStr then Result := Default
      else Result := StrToInt( Value );
  except
    Result := Default;
  end;
end;

function KeyReadCard(AKey: string; KeyName: string; Default: Cardinal = 0):Cardinal;
var
  Value : string;
begin
  KeyRead(AKey, KeyName, Value);
  try
    if Value = EmptyStr then Result := Default
      else Result := StrToInt64( Value );
  except
    Result := Default;
  end;
end;

function KeyReadFloat(AKey: string; KeyName: string; Default: Double):Double;
var
  Value : string;
begin
  KeyRead(AKey, KeyName, Value);
  try
    if Value = EmptyStr then Result := Default
      else Result := StrToFloat( Value );
  except
    Result := Default;
  end;
end;

function KeyReadBoolean(AKey: string; KeyName: string; Default: Boolean = False):Boolean;
var
  Value : string;
begin
  KeyRead(AKey, KeyName, Value);
  try
    if Value = EmptyStr then Result := Default
      else Result := Boolean( StrToInt( Value ) );
  except
    Result := Default;
  end;
end;

function KeyReadString(AKey: string; KeyName: string; Default: string = ''):string;
begin
  KeyRead(AKey, KeyName, Default, Result);
end;

initialization
  Initialize;
end.
