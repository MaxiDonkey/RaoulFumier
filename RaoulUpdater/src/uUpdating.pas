{****************************************************
*         Update installation from website          *
*                                                   *
*               06/2020 by MaxiDonkey               *
*                                                   *
*****************************************************}

unit uUpdating;

interface

uses
  SysUtils, Classes, Windows, Messages, StrUtils, Forms, uRegistry,
  uWebTools, uDosUtils, uProcessusManagment,
  Dialogs;

const
  DEFAULT_URL     = 'http://maxidonkey.com/raoul';
  DEFAULT_EXENAME = 'RaoulFumier';
  DEFAULT_CATALOG = '';

type
  TParams = class
  private
    function GetTRF_FileName: string;
    function GetEXE_FileName: string;
  private
    {Folder}
    procedure MakeSubFolder(const Folder: string);
    function  ExeFolder:string;
    procedure GotoExeFolder;

    function  RetrieveURL: string;
    function  RetrieveExeName: string;
    function  RetrieveCatalog: string;

    {Download}
    function  DownLoad(const FileName, Folder: string):Boolean;
    function  LoadCatalog:Boolean;
    function  LoadExe:Boolean;
    function  TryDownload:Boolean;
    procedure UpdateDownloadStateReg(const Value: Boolean);

    procedure DeleteCatalog;
    procedure DeleteExe;
    procedure ClearOnError;

    {Replace}
    procedure Rename(const OldName, NewName: string);
    procedure RenameCatalog;
    procedure RenameExe;
    procedure TryRename;
    function  CanRename:Boolean;

    {Exe Launcher}
    procedure LaunchExe;
    function  CanReload:Boolean;

    {Scrupteur}
    function  IsRaoulFumierLive:Boolean;
    function  WaitUntilClose:Boolean;

    property TRF_FileName: string read GetTRF_FileName;
    property EXE_FileName: string read GetEXE_FileName;

  public
    class function  Load:Boolean;
    class procedure Rerun;
    class procedure ReStart;
  end;


implementation

{ TParams }

function TParams.CanReload: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'Restart')
end;

function TParams.CanRename: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'DownloadState')
end;

procedure TParams.ClearOnError;
begin
  DeleteCatalog;
  DeleteExe
end;

procedure TParams.DeleteCatalog;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    Text := RetrieveCatalog;
    if Count > 0 then for i := 0 to Pred(Count) do
      if FileExists(Names[i]) then DeleteFile( PChar(Names[i]) )
  finally
    Free
  end
end;

procedure TParams.DeleteExe;
begin  
  if FileExists( TRF_FileName ) then DeleteFile( PChar(TRF_FileName) )
end;

function TParams.DownLoad(const FileName, Folder: string):Boolean;
begin
  if Folder <> EmptyStr then MakeSubFolder(Folder);
  try
    Result :=  TWebTools.DownloadFromHttp(
                  Format('%s/%s', [RetrieveURL, FileName]), FileName);
    if not Result then DeleteFile( PChar(FileName) );
  finally
    GotoExeFolder
  end
end;

function TParams.GetEXE_FileName: string;
begin
  Result := Format('%s.exe', [RetrieveExeName])
end;

function TParams.GetTRF_FileName: string;
begin
  Result := Format('%s.trf', [RetrieveExeName])
end;

function TParams.IsRaoulFumierLive: Boolean;
begin
  Result := IsActivProcessus( EXE_FileName )
end;

procedure TParams.LaunchExe;
begin
  OpenExecute(EXE_FileName, '');
  KeyWrite(AppKey, 'Restart', False);
end;

class function TParams.Load:Boolean;
begin
  with TParams.Create do
  try
    Result := TryDownload
  finally
    Free
  end
end;

function TParams.LoadCatalog:Boolean;
var
  i: Integer;
begin
  Result := True;
  with TStringList.Create do
  try
    Text := RetrieveCatalog;
    if Count > 0 then for i := 0 to Pred(Count) do
    try
      if Names[i] = 'Raoul.trf' then KeyWrite(AppKey, 'RenewLauncher', 1);
      Result := DownLoad(Names[i], ValueFromIndex[i]); 
      Delay(50);
      if not Result then Break
    except
    end
  finally
    Free
  end
end;

function TParams.LoadExe:Boolean;
begin
  Result := DownLoad( Format('%s.trf', [RetrieveExeName]), EmptyStr )
end;

class procedure TParams.ReStart;
begin
  with TParams.Create do
  try
    if WaitUntilClose then if CanReload then LaunchExe
  finally
    Free
  end
end;

procedure TParams.Rename(const OldName, NewName: string);
begin
  if FileExists( NewName ) then DeleteFile( PChar(NewName) );
  Sleep(90);
  RenameFile(OldName, NewName);
  Delay(90);
end;

procedure TParams.RenameCatalog;
var
  i    : Integer;
  dest : string;
begin
  with TStringList.Create do
  try
    Text := RetrieveCatalog;
    if Count > 0 then for i := 0 to Pred(Count) do
    try
      Dest := ValueFromIndex[i];
      if Trim(Dest) <> EmptyStr then Rename(Names[i], Dest);
    except
    end
  finally
    Free
  end
end;

procedure TParams.RenameExe;
begin
  Rename( TRF_FileName, EXE_FileName );
end;

class procedure TParams.Rerun;
begin
  if FileExists( Format('%s.trf', [DEFAULT_EXENAME]) ) then
  with TParams.Create do
  try
    if WaitUntilClose then begin
      TryRename;
      LaunchExe
    end
  finally
    Free
  end
end;

function TParams.RetrieveCatalog: string;
begin
  Result := KeyReadString(AppKey, 'Catalog');
end;

function TParams.RetrieveExeName: string;
begin
  Result := KeyReadString(AppKey, 'RootName');
  if Result = EmptyStr then Result := DEFAULT_EXENAME;
end;

function TParams.RetrieveURL: string;
begin
  Result := KeyReadString(AppKey, 'Urlu');
  if Result = EmptyStr then Result := DEFAULT_URL;
end;

function TParams.TryDownload: Boolean;
begin
  Result := LoadCatalog;
  if Result then Result := LoadExe;
  if not Result then ClearOnError;               { --- Supprimer les fichier téléchargés }
  UpdateDownloadStateReg( Result )
end;

procedure TParams.TryRename;
begin
  if CanRename then begin
    RenameCatalog;
    RenameExe;
    UpdateDownloadStateReg( False )
  end
end;

procedure TParams.UpdateDownloadStateReg(const Value: Boolean);
begin
  KeyWrite(AppKey, 'DownloadState', Value)
end;

function TParams.WaitUntilClose:Boolean;
begin
  while IsRaoulFumierLive and not Application.Terminated do Delay(90);
  with Application do Result := not Terminated
end;

procedure TParams.MakeSubFolder(const Folder: string);
begin
  if not DirectoryExists( Folder ) then try MkDir( Folder ) except end;
  try ChDir( Folder ) except end
end;

procedure TParams.GotoExeFolder;
begin
  ChDir( ExeFolder )
end;

function TParams.ExeFolder: string;
begin
  Result := ExtractFileDir( Application.ExeName )
end;

end.
