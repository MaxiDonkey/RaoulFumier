program RaoulFumier;

uses
  Forms,
  Windows,
  Dialogs,
  uDosUtils,
  main in 'main.pas' {MainForm},
  uRaoulDisplay in 'uRaoulDisplay.pas',
  DisplayTextForm in 'DisplayTextForm.pas' {TalkativeBox},
  ScreenDlg in 'ScreenDlg.pas' {ScreenDialog},
  uRaoulRecorder in 'uRaoulRecorder.pas',
  uEmbendedDlg in 'uEmbendedDlg.pas' {EmbendedDlg},
  uEmbendedAncestor in 'uEmbendedAncestor.pas' {AncestorEmbendedForm},
  uEmbendedSensibility in 'uEmbendedSensibility.pas' {EmbendedSensibility},
  uPickerForm in 'uPickerForm.pas' {PickerGridForm},
  uRaoulDB in 'uRaoulDB.pas',
  uRaoulUpdater in 'uRaoulUpdater.pas',
  uNewRecorder in 'uNewRecorder.pas',
  EliteBindingsTools in 'EliteBindingsTools.pas';

{$R *.res}


var
  Mutex: THandle;

begin
  Mutex := CreateMutex(nil, True, 'RaoulFumier');
  if (Mutex = 0) or (GetLastError = ERROR_ALREADY_EXISTS) then
    MsgDlg('Application is active', mtWarning, [mbOk], 0)
  else begin
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTalkativeBox, TalkativeBox);
  Application.CreateForm(TScreenDialog, ScreenDialog);
  Application.CreateForm(TAncestorEmbendedForm, AncestorEmbendedForm);
  Application.CreateForm(TEmbendedDlg, EmbendedDlg);
  Application.CreateForm(TEmbendedSensibility, EmbendedSensibility);
  Application.CreateForm(TPickerGridForm, PickerGridForm);
  Application.Run;
  end
end.
