unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, uDosUtils, uRegistry, uUpdating, ExtCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Process;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamCount = 0 then Application.Terminate;
  Timer1.Enabled := True;
end;



procedure TForm1.Process;
begin
  Sleep(1500);
  case IndexStr(ParamStr(1), ['-d', '-u', '-r']) of
    0 : TParams.Load;
    1 : TParams.Rerun;
    2 : TParams.ReStart;
  end;
  Application.Terminate
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Process
end;

end.
