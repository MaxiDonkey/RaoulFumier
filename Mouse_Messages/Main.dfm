object TDirectorClass: TTDirectorClass
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Director'
  ClientHeight = 490
  ClientWidth = 432
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 18
  object PageControl1: TPageControl
    Left = 0
    Top = 152
    Width = 432
    Height = 266
    ActivePage = TabSheet2
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitTop = 105
    ExplicitHeight = 259
    object TabSheet1: TTabSheet
      Caption = 'Settings'
      TabVisible = False
      ExplicitHeight = 280
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 107
        Height = 18
        Caption = 'Walk Dead Zone'
      end
      object Label2: TLabel
        Left = 16
        Top = 56
        Width = 60
        Height = 18
        Caption = 'Sensibility'
      end
      object Label3: TLabel
        Left = 16
        Top = 96
        Width = 35
        Height = 18
        Caption = 'Noise'
      end
      object TrackBar1: TTrackBar
        Left = 184
        Top = 16
        Width = 185
        Height = 25
        Max = 12
        PageSize = 6
        TabOrder = 0
        OnChange = TrackBar1Change
      end
      object TrackBar2: TTrackBar
        Left = 184
        Top = 56
        Width = 185
        Height = 25
        Max = 4
        Min = 1
        Position = 2
        TabOrder = 1
        OnChange = TrackBar2Change
      end
      object TrackBar3: TTrackBar
        Left = 184
        Top = 87
        Width = 185
        Height = 25
        Max = 12
        Position = 2
        TabOrder = 2
        OnChange = TrackBar3Change
      end
      object cbCombat: TCheckBox
        Left = 16
        Top = 137
        Width = 193
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Ultra speed'
        TabOrder = 3
        OnClick = cbCombatClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'EyeStick calibration'
      ImageIndex = 1
      ExplicitHeight = 226
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 424
        Height = 233
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label9: TLabel
          Left = 8
          Top = 24
          Width = 250
          Height = 18
          AutoSize = False
          Caption = 'Correction to the left......................'
        end
        object Label11: TLabel
          Left = 235
          Top = 53
          Width = 18
          Height = 18
          Caption = 'Dx'
        end
        object Label12: TLabel
          Left = 240
          Top = 165
          Width = 18
          Height = 18
          Caption = 'Dy'
        end
        object Label13: TLabel
          Left = 8
          Top = 136
          Width = 250
          Height = 18
          AutoSize = False
          Caption = 'Correction to the top......................'
        end
        object ToggleSwitch1: TToggleSwitch
          Left = 264
          Top = 24
          Width = 119
          Height = 20
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
        object UpDown1: TUpDown
          Left = 337
          Top = 50
          Width = 42
          Height = 26
          Associate = Edit1
          DoubleBuffered = True
          Max = 1200
          Orientation = udHorizontal
          ParentDoubleBuffered = False
          TabOrder = 1
          Thousands = False
        end
        object Edit1: TEdit
          Left = 264
          Top = 50
          Width = 73
          Height = 26
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
          Text = '0'
        end
        object Edit2: TEdit
          Left = 264
          Top = 162
          Width = 73
          Height = 26
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 3
          Text = '0'
        end
        object UpDown2: TUpDown
          Left = 337
          Top = 162
          Width = 42
          Height = 26
          Associate = Edit2
          DoubleBuffered = True
          Max = 1200
          Orientation = udHorizontal
          ParentDoubleBuffered = False
          TabOrder = 4
          Thousands = False
        end
        object ToggleSwitch2: TToggleSwitch
          Left = 264
          Top = 136
          Width = 119
          Height = 20
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 5
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Mouse calibration'
      ImageIndex = 2
      ExplicitHeight = 280
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 424
        Height = 233
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label7: TLabel
          Left = 8
          Top = 24
          Width = 250
          Height = 18
          AutoSize = False
          Caption = 'Correction to the left......................'
        end
        object Label8: TLabel
          Left = 235
          Top = 53
          Width = 18
          Height = 18
          Caption = 'Dx'
        end
        object Label10: TLabel
          Left = 240
          Top = 165
          Width = 18
          Height = 18
          Caption = 'Dy'
        end
        object Label14: TLabel
          Left = 8
          Top = 136
          Width = 250
          Height = 18
          AutoSize = False
          Caption = 'Correction to the top......................'
        end
        object ToggleSwitch3: TToggleSwitch
          Left = 264
          Top = 24
          Width = 119
          Height = 20
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 0
        end
        object UpDown3: TUpDown
          Left = 337
          Top = 50
          Width = 42
          Height = 26
          Associate = Edit3
          DoubleBuffered = True
          Max = 1200
          Orientation = udHorizontal
          ParentDoubleBuffered = False
          TabOrder = 1
          Thousands = False
        end
        object Edit3: TEdit
          Left = 264
          Top = 50
          Width = 73
          Height = 26
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 2
          Text = '0'
        end
        object Edit4: TEdit
          Left = 264
          Top = 162
          Width = 73
          Height = 26
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 3
          Text = '0'
        end
        object UpDown4: TUpDown
          Left = 337
          Top = 162
          Width = 42
          Height = 26
          Associate = Edit4
          DoubleBuffered = True
          Max = 1200
          Orientation = udHorizontal
          ParentDoubleBuffered = False
          TabOrder = 4
          Thousands = False
        end
        object ToggleSwitch4: TToggleSwitch
          Left = 264
          Top = 136
          Width = 119
          Height = 20
          DoubleBuffered = True
          ParentDoubleBuffered = False
          TabOrder = 5
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 432
    Height = 152
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object StateLine: TLabel
      Left = 0
      Top = 0
      Width = 432
      Height = 32
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'StateLine'
      Color = 2565927
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = False
      Layout = tlCenter
    end
    object Label6: TLabel
      Left = 37
      Top = 107
      Width = 65
      Height = 18
      Caption = 'Resolution'
    end
    object BtnMenu: TButton
      Left = 295
      Top = 56
      Width = 88
      Height = 25
      Caption = 'MENU'
      TabOrder = 0
      OnClick = BtnMenuClick
    end
    object BtnPause: TButton
      Left = 209
      Top = 56
      Width = 88
      Height = 25
      Caption = 'PAUSE'
      TabOrder = 1
      OnClick = BtnPauseClick
    end
    object btnStop: TButton
      Left = 37
      Top = 56
      Width = 88
      Height = 25
      Caption = 'STOP'
      TabOrder = 2
      OnClick = btnStopClick
    end
    object BtnJoy: TButton
      Left = 123
      Top = 56
      Width = 88
      Height = 25
      Caption = 'EYESTICK'
      TabOrder = 3
      OnClick = BtnJoyClick
    end
    object ComboBox1: TComboBox
      Left = 120
      Top = 104
      Width = 263
      Height = 25
      Style = csOwnerDrawFixed
      ItemHeight = 19
      ItemIndex = 0
      TabOrder = 4
      Text = 'None'
      Items.Strings = (
        'None')
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 418
    Width = 432
    Height = 72
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 364
    object Label4: TLabel
      Left = 20
      Top = 24
      Width = 105
      Height = 36
      Caption = 'EyeXMouse.exe Launcher'
      WordWrap = True
    end
    object Label5: TLabel
      Left = 20
      Top = 88
      Width = 89
      Height = 36
      Caption = 'Saved configurations'
      Visible = False
      WordWrap = True
    end
    object BtnGo: TButton
      Left = 151
      Top = 16
      Width = 222
      Height = 47
      Caption = 'START EyeXMouse'
      TabOrder = 0
      OnClick = BtnGoClick
    end
    object Button1: TButton
      Left = 151
      Top = 80
      Width = 47
      Height = 47
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      Visible = False
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 195
      Top = 80
      Width = 47
      Height = 47
      Caption = '2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Visible = False
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 237
      Top = 80
      Width = 47
      Height = 47
      Caption = '3'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      Visible = False
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 281
      Top = 80
      Width = 47
      Height = 47
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      Visible = False
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 326
      Top = 80
      Width = 47
      Height = 47
      Caption = '5'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      Visible = False
      OnClick = Button5Click
    end
  end
  object EliteStateSurveyor: TTimer
    Enabled = False
    Interval = 500
    OnTimer = EliteStateSurveyorTimer
    Left = 392
    Top = 56
  end
end
