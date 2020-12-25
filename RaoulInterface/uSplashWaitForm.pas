unit uSplashWaitForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSplashWaitForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class procedure Splash(const ACaption: string);
    class procedure SplashHide;
  end;

var
  SplashWaitForm: TSplashWaitForm = nil;

implementation

{$R *.dfm}

uses
  main;

procedure TSplashWaitForm.FormShow(Sender: TObject);
begin
  SetForegroundWindow( SplashWaitForm.Handle );
end;

procedure TSplashWaitForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := CaFree
end;

class procedure TSplashWaitForm.Splash(const ACaption: string);
begin
  if not Assigned(SplashWaitForm) then begin
    SplashWaitForm := TSplashWaitForm.Create(Application.MainForm);
    with SplashWaitForm do begin
      Label1.Caption := ACaption;
      Show
    end;
    Application.ProcessMessages;
  end
end;

class procedure TSplashWaitForm.SplashHide;
begin
  if Assigned(SplashWaitForm) then begin
    SplashWaitForm.Close;
    SplashWaitForm := nil
  end
end;

end.
