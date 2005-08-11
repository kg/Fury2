VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IScriptLanguage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Public Property Get Name() As String
End Property

Public Property Get LineTerminator() As String
End Property

Public Property Get ThisKeyword() As String
End Property

Public Property Get MethodOperator() As String
End Property

Public Function GenerateSubCall(Name As String, Args As Variant) As String
End Function

Public Function GenerateFunctionCall(Name As String, Args As Variant) As String
End Function

Public Function GenerateClass(Name As String, Code As String) As String
End Function

Public Function GenerateInstantiation(Class As String) As String
End Function

Public Function GenerateClassVariable(Name As String, Optional IsPublic As Boolean = True) As String
End Function

Public Function GenerateClassMethod(Name As String, Args As Variant, Code As String) As String
End Function

Public Function GenerateFunction(Name As String, Args As Variant, Code As String) As String
End Function

Public Function GenerateVariable(Name As String, Optional Value As String) As String
End Function

Public Function GenerateAssignment(Target As String, Source As String, Optional Object As Boolean = False) As String
End Function

Public Function GenerateComment(Text As String) As String
End Function

Public Function GenerateStringLiteral(Text As String) As String
End Function

Public Function ReadStringLiteral(Text As String) As String
End Function

Public Function ReadComment(Text As String) As String
End Function

Public Function ReadFunctionName(Text As String) As String
End Function

Public Function ReadFunctionArgs(Text As String) As String
End Function

Public Function ReadFunctionCode(Text As String) As String
End Function

Public Function SplitLines(Text As String) As Variant
End Function

Public Function ImplementsMethod(Code As String, Method As String) As Boolean
End Function

Public Function ParseCode(Code As String, State As ScriptEngineState) As String
End Function
