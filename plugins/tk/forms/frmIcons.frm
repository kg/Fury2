VERSION 5.00
Object = "{396F7AC0-A0DD-11D3-93EC-00C0DFE7442A}#1.0#0"; "vbalIml6.ocx"
Begin VB.Form frmIcons 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Icon Container Form"
   ClientHeight    =   540
   ClientLeft      =   45
   ClientTop       =   300
   ClientWidth     =   1620
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   540
   ScaleWidth      =   1620
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Visible         =   0   'False
   Begin vbalIml6.vbalImageList ilIcons 
      Left            =   0
      Top             =   0
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   32
      Size            =   10332
      Images          =   "frmIcons.frx":0000
      Version         =   131072
      KeyCount        =   9
      Keys            =   "FOLDERÿFOLDER_OPENÿBLOCKINGÿAREASÿTILESÿSPRITESÿPROPERTIESÿTILESETÿOBJECTS"
   End
   Begin vbalIml6.vbalImageList ilContextMenus 
      Left            =   1080
      Top             =   0
      _ExtentX        =   953
      _ExtentY        =   953
      ColourDepth     =   24
      Size            =   22960
      Images          =   "frmIcons.frx":287C
      Version         =   131072
      KeyCount        =   20
      Keys            =   "NEWÿOPENÿSAVEÿSAVE ASÿCUTÿCOPYÿPASTEÿPROPERTIESÿADDÿREMOVEÿUNDOÿREDOÿLISTÿPROJECTÿDELETEÿCHECKÿHELPÿSELECT NONEÿSELECT ALLÿPLUGIN"
   End
   Begin vbalIml6.vbalImageList ilEntityIcons 
      Left            =   540
      Top             =   0
      _ExtentX        =   953
      _ExtentY        =   953
      IconSizeX       =   20
      IconSizeY       =   20
      ColourDepth     =   32
      Size            =   10440
      Images          =   "frmIcons.frx":824C
      Version         =   131072
      KeyCount        =   6
      Keys            =   "HIDDENÿVISIBLEÿLIGHT OFFÿLIGHT ONÿLAYER HIDDENÿLAYER VISIBLE"
   End
End
Attribute VB_Name = "frmIcons"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

