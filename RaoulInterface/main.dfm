object MainForm: TMainForm
  Left = 0
  Top = 0
  AlphaBlendValue = 0
  BorderStyle = bsNone
  Caption = 'MainForm'
  ClientHeight = 851
  ClientWidth = 1565
  Color = 3421236
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RaoulKeys: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Key'
    TabOrder = 0
  end
  object DelayedStart: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = DelayedStartTimer
    Left = 24
    Top = 56
  end
  object DistantData: TDistantData
    Server = '94.76.216.225\SQLSAPI'
    Catalog = 'RaoulFumier'
    OnPrepare = DistantDataPrepare
    Left = 24
    Top = 88
  end
  object ActionList1: TActionList
    Left = 72
    Top = 56
  end
  object BootDelayed: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = BootDelayedTimer
    Left = 24
    Top = 120
  end
  object BootFinalize: TTimer
    Enabled = False
    Interval = 4000
    OnTimer = BootFinalizeTimer
    Left = 24
    Top = 152
  end
end
