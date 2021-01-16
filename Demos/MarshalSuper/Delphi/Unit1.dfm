object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 436
  ClientWidth = 515
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    515
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object BMarshal: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Marshal'
    TabOrder = 0
    OnClick = BMarshalClick
  end
  object BUnMarshal: TButton
    Left = 8
    Top = 207
    Width = 75
    Height = 25
    Caption = 'UnMarshal'
    TabOrder = 2
    OnClick = BUnMarshalClick
  end
  object MMarshal: TMemo
    Left = 8
    Top = 39
    Width = 499
    Height = 162
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '{'
      '  "SET"     :"[csDesigning,csInline]",'
      '  "DATE_2"  :"2020-12-16T00:36:34.651Z",'
      '  "DATE_1"  :"2020-12-17T00:36:34.651Z",'
      '  "ENUM"    :"--soCurrent", // sample bug'
      '  "BOOL"    :"+"'
      '}')
    ParentFont = False
    TabOrder = 1
  end
  object MUnMarshal: TMemo
    Left = 8
    Top = 238
    Width = 499
    Height = 190
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object BMarshalUnMarshal: TButton
    Left = 136
    Top = 207
    Width = 161
    Height = 25
    Caption = 'Marshal+UnMarshal'
    TabOrder = 4
    OnClick = BMarshalUnMarshalClick
  end
end
