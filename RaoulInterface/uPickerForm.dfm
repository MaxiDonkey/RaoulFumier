object PickerGridForm: TPickerGridForm
  Left = 0
  Top = 0
  Width = 651
  Height = 338
  AlphaBlend = True
  Caption = 'PickerGridForm'
  Color = 3421236
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GridPicker: TGridPicker
    MinAlphaBlendValue = 40
    MaxAlphaBlendValue = 150
    OnShowPickList = GridPickerShowPickList
    OnDeleteConfirm = GridPickerDeleteConfirm
    Left = 8
    Top = 8
  end
  object DelayedStart: TTimer
    Enabled = False
    Interval = 100
    OnTimer = DelayedStartTimer
    Left = 8
    Top = 48
  end
end
