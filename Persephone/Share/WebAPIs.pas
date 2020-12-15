unit WebAPIs;

interface

uses
  SysUtils, Classes, Forms, Dialogs, WinINet;

procedure DownloadHTTP(const AUrl : string; FileName: string); overload;
function  UrlFileName(const AUrl: string; FileName, Folder: string):string;
procedure SendPostData;
function  InternetCheck: Boolean;

implementation

uses
  idHttp, IdMultipartFormData;

function  UrlFileName(const AUrl: string; FileName, Folder: string):string;
var
  UpdateURL : string;
begin
  UpdateURL := AUrl; 
  Result := Format('%s/%s/%s', [UpdateURL, Folder, FileName]);
end;

procedure DownloadHTTP(const AUrl : string; out DestStream: TStream); overload;
begin
  with TIdHTTP.Create(nil) do
  try
    Request.Connection := 'keep-alive';
    Request.UserAgent  := 'UserAgent';
    try
      Get(AUrl, DestStream);
    except
      on E: Exception do ShowMessage(E.Message);
    end;
  finally
    Free
  end;
end;

procedure DownloadHTTP(const AUrl : string; FileName: string);
var
  Stream : TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    DownloadHTTP(AUrl, TStream( Stream ));
  finally
    Stream.Free
  end;
end;


{-------------------------------------------------------------------------------
                      A revoir : ne marche pas !!!
...............................................................................}

function GetFileText:string;
begin
  with TStringList.Create do
  try
    LoadFromFile('H:\2020 Developpement\Persephone\Recorder\bin\gr_insult.grxml');
    Result := Text;
  finally
    Free
  end
end;

procedure SendPostData;
var
  Stream: TStringStream;
  Params: TIdMultipartFormDataStream;
  HTTP  : TIdHTTP;
begin
  HTTP := TIdHTTP.Create(nil);
  with HTTP do begin
   Request.Connection := 'keep-alive';
   Request.UserAgent  := 'UserAgent';
  end;
  try
    Stream := TStringStream.Create('');
    try
      Params := TIdMultipartFormDataStream.Create;
      try
        Params.AddFile('File1', 'H:\2020 Developpement\Persephone\Recorder\bin\gr_insult.grxml', 'application/octet-stream');
        try
          HTTP.Post('http://www.maxidonkey.com/Recorder/Grammar/gr_insult.grxml', Params, Stream);
        except
          on E: Exception do
            ShowMessage('Error encountered during POST: ' + E.Message);
        end;
        ShowMessage(Stream.DataString);
      finally
        Params.Free;
      end;
    finally
      Stream.Free;
      HTTP.Free;
    end;
  except
  end;
end;

function InternetCheck: Boolean;
var
  origin : cardinal;
begin
  result := InternetGetConnectedState(@origin,0);

     //connections origins by origin value
     //NO INTERNET CONNECTION              = 0;
     //INTERNET_CONNECTION_MODEM           = 1;
     //INTERNET_CONNECTION_LAN             = 2;
     //INTERNET_CONNECTION_PROXY           = 4;
     //INTERNET_CONNECTION_MODEM_BUSY      = 8;
end;

end.
