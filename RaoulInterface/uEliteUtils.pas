unit uEliteUtils;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, uRegistry, StrCopyUtils;

const
  ELITE_CLASS          = 'FrontierDevelopmentsAppWinClass';
  SHELL_ACCES          = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';  //Registry key
  SAVE_GAMES_KEY       = '{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}';                            //Registry key
  BINDING_OPT_KEY      = 'Local AppData';
  LOCAL_SAVE           = 'SaveBindigs';
  TEMP_CUSTOM_FILE     = 'Temp.Custom.3.0.binds';
  CUSTOM_FILE          = 'Custom.3.0.binds';
  START_PRESET         = 'StartPreset.start';
  DISPLAY_SETTINGS     = 'DisplaySettings.xml';

var
  SavedGamesW    : string = 'Frontier Developments\Elite Dangerous';
  BindingsFolder : string = 'Frontier Developments\Elite Dangerous\Options\Bindings';
  GraphicsFolder : string = 'Frontier Developments\Elite Dangerous\Options\Graphics';

function  IsEliteRunningUTLS: Boolean;
procedure EliteForeGround;
function  GetFrontierSaveGames: string;
function  GetEliteBindingsFolder: string;
function  GetEliteGraphicsFolder: string;

function  GameLogFilesRetrieve: string;
function  GetLogPseudo: string;

procedure EliteSetFocus;

implementation

function IsEliteRunningUTLS: Boolean;
begin
  Result := FindWindow( PChar(ELITE_CLASS), nil ) <> 0;
end;

procedure EliteForeGround;
begin
  SetForegroundWindow( FindWindow( PChar(ELITE_CLASS), nil ) );
end;

procedure EliteSetFocus;
begin
  if IsEliteRunningUTLS then EliteForeGround
end;

function GetFrontierSaveGames: string;
begin
  KeyRead(SHELL_ACCES, SAVE_GAMES_KEY, Result);
  Result := Format('%s\%s', [Result, SavedGamesW])
end;

function GetEliteBindingsFolder: string;
begin
  KeyRead(SHELL_ACCES, BINDING_OPT_KEY, Result);
  Result := Format('%s\%s', [Result, BindingsFolder])
end;

function GetEliteGraphicsFolder: string;
begin
  KeyRead(SHELL_ACCES, BINDING_OPT_KEY, Result);
  Result := Format('%s\%s', [Result, GraphicsFolder])
end;

function GameLogFilesRetrieve:string;
var
  SearchResult : TSearchRec;
begin
  ChDir(GetFrontierSaveGames);
  with TStringList.Create do
  try
    Sorted := True;
    if FindFirst('*.log', faAnyFile, SearchResult) = 0 then begin
      repeat
        case IndexStr( SearchResult.Name, ['.', '..']) of
          -1 : Add( SearchResult.Name )
        end;
      until FindNext( SearchResult ) <> 0;
      Result := Text
    end
  finally
    with Application do ChDir( ExtractFileDir( ExeName ) );
    Free
  end
end;


const
  COMMANDER = '"Commander"';

function FilePseudoCheck(const AFileName: string):Boolean;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    Result := AnsiPos(COMMANDER, Text) > 0
  finally
    Free
  end
end;

function ExtractPseudo(const AFileName: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(AFileName);
    Result := GetAfterStr(Text, COMMANDER);
    Result := GetAfterStr(Result, '"Name":"');
    Result := GetBeforStr(Result, '"')
  finally
    Free
  end
end;

function GetLogPseudo: string;
var
  i : Integer;

  function FullFileName(const Src: string): string; begin
    Result := Format('%s\%s', [GetFrontierSaveGames, Src])
  end;

begin
  Result := EmptyStr;
  with TStringList.Create do
  try
    Text := GameLogFilesRetrieve;
    { --- Ne pas prendre le dernier fichier car il est ouvert }
    for i := Pred( Count - 1 ) downto 0 do
      if FilePseudoCheck( FullFileName(Strings[i]) ) then begin
        Result := ExtractPseudo( FullFileName(Strings[i]) );
        Break
      end
  finally
    Free
  end
end; {GetLogPseudo}

end.
