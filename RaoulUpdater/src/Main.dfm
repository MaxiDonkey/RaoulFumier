object Form1: TForm1
  Left = 0
  Top = 0
  Width = 220
  Height = 105
  Caption = 'Form1'
  Color = 3421236
  TransparentColorValue = 3421236
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clGray
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMinimized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 18
  object Timer1: TTimer
    Enabled = False
    Interval = 90
    OnTimer = Timer1Timer
    Left = 8
    Top = 16
  end
end
