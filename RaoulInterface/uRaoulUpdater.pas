unit uRaoulUpdater;

interface

uses
  Windows, Messages, SysUtils, Variants,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, uRegistry, StrUtils,
  {Dos & Web uttilities}
  uDosUtils, uWebTools,
  {App Parameter}
  uRaoulParameters;

const
  UPDATER_PROCESS = 'Raoul.exe';
  TRF_RAOUL       = 'Raoul.trf';

var
  UrlForUpdate : string = 'http://www.maxidonkey.com';

type
  TAppUpdater = class
  private
  public
    class procedure TryToLoad;
    class procedure TryToUpdate;
    class procedure TryToRelauch;
    class procedure TryReplaceUpdater;
    class function TryGrammarUpdate:Boolean;
  end;

  TAppComplementUpdater = class
  private
    function  LocalFilenameExists(const AFileName: string; const InFolder: string): Boolean;
    function  LoadEyeXFile(const AFileName: string): Boolean;
    procedure UpdateEyeXMouse;
  public
    class procedure Process;
  end;

function RaoulKey: string;

implementation

uses
  uSplashWaitForm, uGaussDisplay;

function RaoulKey: string;
begin
  Result := Format('Software\%s', [ 'Raoul' ])
end;

function DownloadStateEnable:Boolean;
begin
  Result := KeyReadBoolean(RaoulKey, 'DownloadState')
end;

procedure SetRestart;
begin
  KeyWrite(RaoulKey, 'Restart', True)
end;

function GetLocalgrUpdateCount:Integer;
begin
  Result := KeyReadInt(AppKey, 'LocalgrUpdateCount')
end;

procedure SetLocalgrUpdateCount(const Value: Integer);
begin
  KeyWrite(AppKey, 'LocalgrUpdateCount', Value)
end;

function GetLocalplCatalog:string;
begin
  Result := KeyReadString(AppKey, 'LocalplCatalog')
end;

procedure SetLocalplCatalog(const Value: string);
begin
  KeyWrite(AppKey, 'LocalplCatalog', Value)
end;

function GetDBgrUpdateCount: Integer;
begin
  Result := KeyReadInt(AppKey, 'DBgrUpdateCount')
end;

function GetDBgrCatalog: string;
begin
  Result := KeyReadString(AppKey, 'DBgrCatalog')
end;

function GetDBgrTempCatalog: string;
begin
  Result := KeyReadString(AppKey, 'DBgrTempCatalog')
end;

procedure FileReplace(const OldName, NewName: string);
begin
  if FileExists( NewName ) then DeleteFile( PChar(NewName) );
  RenameFile(PChar(OldName), PChar(NewName));
  Delay(50);
end;

function LocalAppFolder: string;
begin
  Result := ExtractFileDir(Application.ExeName)
end;

function GrammarFolder:string;
begin
  Result := Format('%s\Grammar', [ExtractFileDir(Application.ExeName)])
end;

function GrammarWebFolder:string;
begin
  Result := Format('%s/raoul/grammar', [UrlForUpdate])
end;

function EyeXWebFolder:string;
begin
  Result := Format('%s/raoul/eyexmouse', [UrlForUpdate])
end;

type
  TGrUpdateAction = (gua_none, gua_partial, gua_full);

procedure UpdateLocalGrammar(const ACatalog: string; Action: TGrUpdateAction = gua_full);
var
  Trace    : TStringList;
  updCount : Integer;

  procedure FileReplaceByName(const CurrentFileName: string); begin
    TWebTools.DownloadFromHttp(
      Format('%s/%s', [GrammarWebFolder, CurrentFileName]) ,
      Format('%s\%s', [GrammarFolder,    CurrentFileName]) );
    { --- Tracer le fichier courant }
    with Trace do Add( CurrentFileName );
    Application.ProcessMessages;
  end;

  procedure TraceCreateAndSigning; begin
    updCount := GetDBgrUpdateCount;
    Trace    := TStringList.Create;
    with Trace do case Action of
      gua_full    : Add( Format('Grammaire last modification (%d) : complète le %s',  [updCount, DateTimeToStr(Now)]) );
      gua_partial : Add( Format('Grammaire last modification (%d) : partielle le %s', [updCount, DateTimeToStr(Now)]) );
    end
  end;

begin
  TSplashWaitForm.Splash('Grammars Update'); { --- Pas besoin d'être refermée }
  TraceCreateAndSigning;
  try
    if Trim(ACatalog) <> EmptyStr then with TStringList.Create do
      try
        Text := ACatalog;
        with GetEnumerator do
        try
          while MoveNext do if Trim(Current) <> EmptyStr then FileReplaceByName(Current);
          { --- Mettre à jour le compteur de références de mise à jour }
          SetLocalgrUpdateCount( GetDBgrUpdateCount );
          { --- Garder la trace de la dernnière sauvegarde }
          SetLocalplCatalog( ACatalog );
          { --- Indiquer que la mmise à jour a effectivement été réalisée }
          AppParameters.GrammarUpdated := True
        finally
          Free
        end
      finally
        Free
      end;
    { --- Sauvegarder la trace }
    with Trace do SaveToFile('GrammarUpdateTrace.txt');
  finally
    Trace.Free;
  end
end; {UpdateLocalGrammar}

function grUpdateAction:TGrUpdateAction;
begin
  case Abs(GetDBgrUpdateCount - GetLocalgrUpdateCount) of
    0  : Result := gua_none;
    1  : Result := gua_partial;
    else Result := gua_full;
  end
end;

{ TAppUpdater }

class function TAppUpdater.TryGrammarUpdate:Boolean;
begin
  Result := True;
  case grUpdateAction of
    gua_partial : UpdateLocalGrammar( GetDBgrTempCatalog, gua_partial );
    gua_full    : UpdateLocalGrammar( GetDBgrCatalog );
    else Result := False;
  end
end;

class procedure TAppUpdater.TryReplaceUpdater;
begin
  if KeyReadInt(RaoulKey, 'RenewLauncher') = 1 then
  try
    FileReplace(TRF_RAOUL, UPDATER_PROCESS);
    Delay(50);
    KeyWrite(RaoulKey, 'RenewLauncher', 0);
  finally
  end
end;

class procedure TAppUpdater.TryToLoad;
begin
  if KeyReadInt(RaoulKey, 'DownloadState') = 0 then
    OpenExecute(UPDATER_PROCESS, '-d', SW_HIDE)
end;

class procedure TAppUpdater.TryToRelauch;
begin
//  try with GaussDisplayForm do if Visible then Close except end;
  SetRestart;
  OpenExecute(UPDATER_PROCESS, '-r', SW_HIDE);
  with Application do Terminate
end;

class procedure TAppUpdater.TryToUpdate;
begin
  if KeyReadInt(RaoulKey, 'DownloadState') = 1 then begin
    OpenExecute(UPDATER_PROCESS, '-u', SW_HIDE);
    Delay(90);
    with Application do Terminate
  end
end;

{ TAppComplementUpdater }

function TAppComplementUpdater.LoadEyeXFile(const AFileName: string): Boolean;
begin
  Result := TWebTools.DownloadFromHttp(
    Format('%s/%s', [EyeXWebFolder,  AFileName]) ,
    Format('%s\%s', [LocalAppFolder, AFileName]) );
end;

function TAppComplementUpdater.LocalFilenameExists(const AFileName: string; const InFolder: string): Boolean;
var
  FileName: string;
begin
  if InFolder = EmptyStr then FileName := Format('%s\%s', [ExtractFileDir(Application.ExeName), AFileName])
    else FileName := Format('%s\%s\%s', [ExtractFileDir(Application.ExeName), InFolder, AFileName]);
  Result :=  FileExists(FileName)
end;

class procedure TAppComplementUpdater.Process;
begin
  with TAppComplementUpdater.Create do
  try
    UpdateEyeXMouse;
  finally
    Free
  end
end;

procedure TAppComplementUpdater.UpdateEyeXMouse;
begin
  if not LocalFilenameExists('Tobii.EyeX.Client.dll', '') then
    LoadEyeXFile('Tobii.EyeX.Client.dll');
  if not LocalFilenameExists('EyeXMouse.exe', '') then
    LoadEyeXFile('EyeXMouse.exe');
end;

end.
