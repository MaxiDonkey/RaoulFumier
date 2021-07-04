program GazeMode;

{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {TDirectorClass},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TTDirectorClass, TDirectorClass);
  Application.Run;
end.
