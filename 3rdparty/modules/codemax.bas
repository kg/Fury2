Attribute VB_Name = "mdlCodeMax"
'///////////////////////////////////////////////////////////////////////////////
'//
'//  CodeMax Code Editor Control
'//
'//  Copyright © 2000  WinMain Software
'//
'//  This header file declares common values used in various CodeMax object
'//  methods and properties.  For a complete description of all declarations
'//  below, please refer to the CodeMax documentation.
'//
'///////////////////////////////////////////////////////////////////////////////

'///////////////////////////////////////////////////////////
'//
'// General Limitations
'//
'//
Const cmMaxFindreplText As Integer = 100         ' maximum size (TCHARs) of text to find or replace
Const cmFindReplaceMRUMax As Integer = 10        ' maximum MRU size in find and find/replace dialogs
Const cmFindReplaceMRUBuffSize As Integer = 1010 ' required buffer size for pszMRUList argument CMSetFindReplaceMRUList() and CMGetFindReplaceMRUList()
Const cmMaxMacros As Integer = 10                ' maximum number of keystroke macros supported by CodeMax
Const cmMaxCmdString As Integer = 50             ' maximum size of command string returned in pszBuff param of CMGetCommandString() if bDescription is FALSE
Const cmMaxCmdDescription As Integer = 100       ' maximum size of command string returned in pszBuff param of CMGetCommandString() if bDescription is TRUE
Const cmMaxLanguageName As Integer = 30          ' maximum size of a language name set with CMRegisterLanguage
Const cmMaxTabsize As Integer = 100              ' maximum tab size (characters)
Const cmMinTabsize As Integer = 2                ' minimum tab size (characters)
Const cmLeftMarginWidth As Integer = 24          ' left margin width (pixels)

'/////////////////////////////////////////////////////////////
'//
'// hotkey modifier values used by CodeMax.HotKey.  These
'// values can be OR'd together to form keystroke combinations
'//
'//
Const HOTKEYF_SHIFT As Integer = 1
Const HOTKEYF_CONTROL As Integer = 2
Const HOTKEYF_ALT As Integer = 4
Const HOTKEYF_EXT As Integer = 8

'/////////////////////////////////////////////////////////////
'//
'// Edit commands passed to:
'//
'// CodeMax.Globals.GetHotKeysForCmd()
'// CodeMax.Globals.RegisterHotKey()
'// CodeMax.Globals.UnregisterHotKey()
'//

Const cmCmdFirst As Integer = 100                    ' The first command id (not an actual command)
Const cmCmdWordUpperCase As Integer = 100            ' Makes the current word uppercase
Const cmCmdWordTranspose As Integer = 101            ' Swaps the current and previous words
Const cmCmdWordRightExtend As Integer = 102          ' Extends the selection forward to the start of the next word
Const cmCmdWordRight As Integer = 103                ' Moves forward to the start of the next word
Const cmCmdWordEndRight As Integer = 104             ' Moves forward to the end of the next word
Const cmCmdWordEndRightExtend As Integer = 105       ' Extends the selection forward to the start of the next word
Const cmCmdWordLowerCase As Integer = 106            ' Makes the current word lowercase
Const cmCmdWordLeftExtend As Integer = 107           ' Extends the selection backward to the start of the previous word
Const cmCmdWordLeft As Integer = 108                 ' Moves backward to the start of the previous word
Const cmCmdWordEndLeft As Integer = 109              ' Moves backward to the end of the previous word
Const cmCmdWordEndLeftExtend As Integer = 110        ' Extends the selection backward to the end of the previous word
Const cmCmdWordDeleteToStart As Integer = 111        ' Deletes a word to the left
Const cmCmdWordDeleteToEnd As Integer = 112          ' Deletes a word to the right
Const cmCmdWordCapitalize As Integer = 113           ' Makes the first character uppercase
Const cmCmdWindowStart As Integer = 114              ' Moves to the top of the text window
Const cmCmdWindowScrollUp As Integer = 115           ' Scrolls the file contents up one line
Const cmCmdWindowScrollToTop As Integer = 116        ' Scrolls the line to the top of the window
Const cmCmdWindowScrollToCenter As Integer = 117     ' Scrolls the line to the center of the window
Const cmCmdWindowScrollToBottom As Integer = 118     ' Scrolls the line to the bottom of the window
Const cmCmdWindowScrollRight As Integer = 119        ' Scrolls the window to the right
Const cmCmdWindowScrollLeft As Integer = 120         ' Scrolls the window to the left
Const cmCmdWindowScrollDown As Integer = 121         ' Scrolls the file contents down one line
Const cmCmdWindowRightEdge As Integer = 122          ' Moves to the right edge of the text window
Const cmCmdWindowLeftEdge As Integer = 123           ' Moves to the left edge of the text window
Const cmCmdWindowEnd As Integer = 124                ' Moves to the bottom of the text window
Const cmCmdUpperCaseSelection As Integer = 125       ' Makes the selection all uppercase
Const cmCmdUntabifySelection As Integer = 126        ' Replaces tabs with spaces in the selection
Const cmCmdUnindentSelection As Integer = 127        ' Indents the selected text left one tab stop
Const cmCmdUndoChanges As Integer = 128              ' Undoes the last action, ignoring movement commands
Const cmCmdUndo As Integer = 129                     ' Undoes the last action
Const cmCmdTabifySelection As Integer = 130          ' Replaces spaces with tabs in the selection
Const cmCmdSentenceRight As Integer = 131            ' Moves to the beginning of the next sentence
Const cmCmdSentenceLeft As Integer = 132             ' Moves to the beginning of the previous sentence
Const cmCmdSentenceCut As Integer = 133              ' Deletes the remainder of the sentence
Const cmCmdSelectSwapAnchor As Integer = 134         ' Swaps the anchor and the cursor in a selection
Const cmCmdSelectPara As Integer = 135               ' Selects the current paragraph
Const cmCmdSelectLine As Integer = 136               ' Selects lines of text
Const cmCmdSelectAll As Integer = 137                ' Selects the entire document
Const cmCmdRedoChanges As Integer = 138              ' Redoes the last action, ignoring movement commands
Const cmCmdRedo As Integer = 139                     ' Redoes the previously undone action
Const cmCmdPaste As Integer = 140                    ' Inserts the Clipboard contents at the insertion point
Const cmCmdParaUp As Integer = 141                   ' Moves to the beginning of the previous paragraph
Const cmCmdParaDown As Integer = 142                 ' Moves to the beginning of the next paragraph
Const cmCmdPageUpExtend As Integer = 143             ' Extends the selection up one page
Const cmCmdPageUp As Integer = 144                   ' Moves the cursor up one page
Const cmCmdPageDownExtend As Integer = 145           ' Extends the selection down one page
Const cmCmdPageDown As Integer = 146                 ' Moves the cursor down one page
Const cmCmdLowerCaseSelection As Integer = 147       ' Makes the selection all lowercase
Const cmCmdLineUpExtend As Integer = 148             ' Extends the selection up one line
Const cmCmdLineUp As Integer = 149                   ' Moves the cursor up one line
Const cmCmdLineTranspose As Integer = 150            ' Swaps current and previous lines
Const cmCmdLineStart As Integer = 151                ' Moves to the start of the current line
Const cmCmdLineOpenBelow As Integer = 152            ' Opens a new line below the cursor
Const cmCmdLineOpenAbove As Integer = 153            ' Opens a new line above the cursor
Const cmCmdLineEndExtend As Integer = 154            ' Extends the selection to the end of the current line
Const cmCmdLineEnd As Integer = 155                  ' Moves the cursor to the end of the current line
Const cmCmdLineDownExtend As Integer = 156           ' Extends the selection down one line
Const cmCmdLineDown As Integer = 157                 ' Moves the cursor down one line
Const cmCmdLineDeleteToStart As Integer = 158        ' Deletes to the beginning of the current line
Const cmCmdLineDeleteToEnd As Integer = 159          ' Deletes to the end of the current line
Const cmCmdLineDelete As Integer = 160               ' Deletes the selected line
Const cmCmdLineCut As Integer = 161                  ' Deletes the selected lines and places the text on the clipboard
Const cmCmdIndentToPrev As Integer = 162             ' Indents to the position of the next text on the previous line
Const cmCmdIndentSelection As Integer = 163          ' Indents the selected text right one tab stop
Const cmCmdHomeExtend As Integer = 164               ' Extends the selection to either the start of the current line or the start of the text on that line
Const cmCmdHome As Integer = 165                     ' Moves to either the start of the current line or the start of the text on that line
Const cmCmdGoToMatchBrace As Integer = 166           ' Finds the matching brace
Const cmCmdGoToIndentation As Integer = 167          ' Moves to the end of the indentation
Const cmCmdGoToLine As Integer = 168                 ' Moves to a user-specified line
Const cmCmdFindReplace As Integer = 169              ' Displays the find & replace dialog box
Const cmCmdReplace As Integer = 170                  ' Replaces the first occurrence of the find text after the current position with the replace text and finds the next occurrence of the find text
Const cmCmdReplaceAllInBuffer As Integer = 171       ' Replaces the find text with the replace text in the entire buffer
Const cmCmdReplaceAllInSelection As Integer = 172    ' Replaces the find text with the replace text in the selection
Const cmCmdFindPrevWord As Integer = 173             ' Finds the previous occurrence of the selected text
Const cmCmdFindPrev As Integer = 174                 ' Finds the previous occurrence of the specified text
Const cmCmdFindNextWord As Integer = 175             ' Finds the next occurrence of the selected text
Const cmCmdFindNext As Integer = 176                 ' Finds the next occurrence of the specified text
Const cmCmdFindMarkAll As Integer = 177              ' Finds the specified text and sets a bookmark at the found locations
Const cmCmdFind As Integer = 178                     ' Finds the specified text
Const cmCmdSetFindKeyWord As Integer = 179           ' Sets the text to search for in subsequent find commands
Const cmCmdSetReplaceKeyWord As Integer = 180        ' Sets the text to substitute for the find text in subsequent find & replace commands
Const cmCmdToggleWholeWord As Integer = 181          ' Toggles intelligent case preservation when replacing text
Const cmCmdTogglePreserveCase As Integer = 182       ' Toggles whole word searching on and off
Const cmCmdToggleCaseSensitive As Integer = 183      ' Toggles case sensitive searching on and off
Const cmCmdEnd As Integer = 184                      ' Moves to the end of the current line, bottom of the text window, or end of the file
Const cmCmdToggleWhitespaceDisplay As Integer = 185  ' Shows or hides whitespace indicators
Const cmCmdToggleOvertype As Integer = 186           ' Toggles between inserting and replacing text
Const cmCmdSetRepeatCount As Integer = 187           ' Sets the repeat count for the next command
Const cmCmdDocumentStartExtend As Integer = 188      ' Extends the selection to the beginning of the file
Const cmCmdDocumentStart As Integer = 189            ' Moves to the beginning of the file
Const cmCmdDocumentEndExtend As Integer = 190        ' Extends the selection to the end of the file
Const cmCmdDocumentEnd As Integer = 191              ' Moves to the end of the file
Const cmCmdDeleteHorizontalSpace As Integer = 192    ' Deletes the spaces and tabs around the cursor
Const cmCmdDeleteBlankLines As Integer = 193         ' Deletes the blank lines adjacent to the cursor
Const cmCmdDeleteBack As Integer = 194               ' Deletes the selection or, if there is no selection, the character to the left of the cursor
Const cmCmdDelete As Integer = 195                   ' Deletes the selection
Const cmCmdCutSelection As Integer = 196             ' Cuts the selection and puts it on the Clipboard
Const cmCmdCut As Integer = 197                      ' Cuts the selection and puts it on the Clipboard
Const cmCmdCopy As Integer = 198                     ' Copies the selection to the Clipboard
Const cmCmdCharTranspose As Integer = 199            ' Swap characters around the insertion point
Const cmCmdCharRightExtend As Integer = 200          ' Extends the selection one character to the right
Const cmCmdCharRight As Integer = 201                ' Moves the cursor one character to the right
Const cmCmdCharLeftExtend As Integer = 202           ' Extends the selection one character to the left
Const cmCmdCharLeft As Integer = 203                 ' Moves the cursor one character to the left
Const cmCmdBookmarkToggle As Integer = 204           ' Toggles a bookmark for the current line on and off
Const cmCmdBookmarkPrev As Integer = 205             ' Moves to the line containing the previous bookmark
Const cmCmdBookmarkNext As Integer = 206             ' Moves to the line containing the next bookmark
Const cmCmdBookmarkClearAll As Integer = 207         ' Clears all bookmarks in the window
Const cmCmdBookmarkJumpToFirst As Integer = 208      ' Moves to the first line containing a bookmark
Const cmCmdBookmarkJumpToLast As Integer = 209       ' Moves to the last line containing a bookmark
Const cmCmdAppendNextCut As Integer = 210            ' Adds the next cut text to end of the Clipboard
Const cmCmdInsertChar As Integer = 211               ' Inserts a character at the current location
Const cmCmdNewLine As Integer = 212                  ' Inserts a new-line character at the current location
Const cmCmdRecordMacro As Integer = 213              ' Begins/ends keystroke macro-recording
Const cmCmdPlayMacro1 As Integer = 214               ' Plays keystroke macro 1
Const cmCmdPlayMacro2 As Integer = 215               ' Plays keystroke macro 2
Const cmCmdPlayMacro3 As Integer = 216               ' Plays keystroke macro 3
Const cmCmdPlayMacro4 As Integer = 217               ' Plays keystroke macro 4
Const cmCmdPlayMacro5 As Integer = 218               ' Plays keystroke macro 5
Const cmCmdPlayMacro6 As Integer = 219               ' Plays keystroke macro 6
Const cmCmdPlayMacro7 As Integer = 220               ' Plays keystroke macro 7
Const cmCmdPlayMacro8 As Integer = 221               ' Plays keystroke macro 8
Const cmCmdPlayMacro9 As Integer = 222               ' Plays keystroke macro 9
Const cmCmdPlayMacro10 As Integer = 223              ' Plays keystroke macro 10
Const cmCmdProperties As Integer = 224               ' Displays the properties dialog
Const cmCmdToggleRegExp As Integer = 228             ' Toggles regular expression searching on and off
Const cmCmdClearSelection As Integer = 229           ' Empties the selection
Const cmCmdRegExpOn As Integer = 230                 ' Turns on regular expression searching
Const cmCmdRegExpOff As Integer = 231                ' Turns off regular expression searching
Const cmCmdWholeWordOn As Integer = 232              ' Turns on whole word searching
Const cmCmdWholeWordOff As Integer = 233             ' Turns off whole word searching
Const cmCmdPreserveCaseOn As Integer = 234           ' Turns on case preservation when replacing text
Const cmCmdPreserveCaseOff As Integer = 235          ' Turns off case preservation when replacing text
Const cmCmdCaseSensitiveOn As Integer = 236          ' Turns on case sensitive searching
Const cmCmdCaseSensitiveOff As Integer = 237         ' Turns off case sensitive searching
Const cmCmdWhitespaceDisplayOn As Integer = 238      ' Turns on whitespace display
Const cmCmdWhitespaceDisplayOff As Integer = 239     ' Turns off whitespace display
Const cmCmdOvertypeOn As Integer = 240               ' Turns on overtype mode
Const cmCmdOvertypeOff As Integer = 241              ' Turns off overtype mode
Const cmCmdLast As Integer = 241                     ' The last command id (not an actual command)

'////////////////////////////////////////////////////////////////////////////////////////
'//
'// command failure codes returned via CmdFailure notification:
'//

Const cmErrFailure As Integer = 1                                        ' general failure
Const cmErrInput As Integer = 2                                          ' bad input
Const cmErrSelection As Integer = 3                                      ' bad selection
Const cmErrNotFound As Integer = 4                                       ' data not found
Const cmErrEmptyBuf As Integer = 5                                       ' buffer is empty
Const cmErrReadOnly As Integer = 6                                       ' buffer is read-only

'////////////////////////////////////////////////////////////////////////////////////////
'//
'// Stock languages that can be passed to:
'//
'// CodeMax.Control.SetLanguage(),
'// CodeMax.Globals.RegisterLanguage()
'// CodeMax.Globals.UnregisterLanguage()
'//
Const cmLangCPP    As String = "C/C++"
Const cmLangPascal As String = "Pascal"
Const cmLangBasic  As String = "Basic"
Const cmLangSQL    As String = "SQL"
Const cmLangJava   As String = "Java"
Const cmLangHTM    As String = "HTML"
Const cmLangXML    As String = "XML"

'/////////////////////////////////////////////////////////////
'//
'// Color settings for:
'//
'// CodeMax.Control.GetColors()
'// CodeMax.Control.SetColors()
'//
Const cmClrWindow As Integer = 0                         ' window background color
Const cmClrLeftMargin As Integer = 1         ' left margin background color
Const cmClrBookmark As Integer = 2                       ' bookmark foreground color
Const cmClrBookmarkBk As Integer = 3             ' bookmark background color
Const cmClrText As Integer = 4                           ' plain text foreground color
Const cmClrTextBk As Integer = 5                         ' plain text background color
Const cmClrNumber As Integer = 6                         ' numeric literal foreground color
Const cmClrNumberBk As Integer = 7                       ' numeric literal background color
Const cmClrKeyword As Integer = 8                        ' keyword foreground color
Const cmClrKeywordBk As Integer = 9                      ' keyword background color
Const cmClrOperator As Integer = 10                      ' operator foreground color
Const cmClrOperatorBk As Integer = 11            ' operator background color
Const cmClrScopeKeyword As Integer = 12          ' scope keyword foreground color
Const cmClrScopeKeywordBk As Integer = 13        ' scope keyword background color
Const cmClrComment As Integer = 14                       ' comment foreground color
Const cmClrCommentBk As Integer = 15             ' comment background color
Const cmClrString As Integer = 16                        ' string foreground color
Const cmClrStringBk As Integer = 17                      ' string background color
Const cmClrTagText As Integer = 18                       ' plain tag text foreground color
Const cmClrTagTextBk As Integer = 19             ' plain tag text background color
Const cmClrTagEntity As Integer = 20             ' tag entity foreground color
Const cmClrTagEntityBk As Integer = 21           ' tag entity background color
Const cmClrTagElementName As Integer = 22        ' tag element name foreground color
Const cmClrTagElementNameBk As Integer = 23      ' tag element name background color
Const cmClrTagAttributeName As Integer = 24      ' tag attribute name foreground color
Const cmClrTagAttributeNameBk As Integer = 25 ' tag attribute name background color
Const cmClrLineNumber As Integer = 26            ' line number foreground color
Const cmClrLineNumberBk As Integer = 27          ' line number background color
Const cmClrHDividerLines As Integer = 28         ' line number separate line color
Const cmClrVDividerLines As Integer = 29         ' left margin separate line color
Const cmClrHighlightedLine As Integer = 30       ' highlighted line color

'/////////////////////////////////////////////////////////////
'//
'// language properties supporting font styles.  Used in:
'//
'// CodeMax.Control.GetFontStyle()
'// CodeMax.Control.SetFontStyle()
'//
Const cmStyText As Integer = 0                           ' plain text font style
Const cmStyNumber As Integer = 1                         ' numeric literal font style
Const cmStyKeyword As Integer = 2                        ' keyword font style
Const cmStyOperator As Integer = 3                       ' operator font style
Const cmStyScopeKeyword As Integer = 4           ' scope keyword font style
Const cmStyComment As Integer = 5                        ' comment font style
Const cmStyString As Integer = 6                         ' string font style
Const cmStyTagText As Integer = 7                        ' plain tag text font style
Const cmStyTagEntity As Integer = 8                      ' tag entity font style
Const cmStyTagElementName As Integer = 9         ' tag element name font style
Const cmStyTagAttributeName As Integer = 10      ' attribute name font style
Const cmStyLineNumber As Integer = 11            ' line number font style

'/////////////////////////////////////////////////////////////
'//
'// font style settings ffor:
'//
'// CodeMax.Control.GetFontStyle()
'// CodeMax.Control.SetFontStyle()
'//
Const cmFontNormal As Integer = 0                        ' normal font
Const cmFontBold As Integer = 1                          ' bold font
Const cmFontItalic As Integer = 2                        ' italic font
Const cmFontBoldItalic As Integer = 3            ' bold + italic font
Const cmFontUnderline As Integer = 4             ' normal weight, underline

'/////////////////////////////////////////////////////////////
'//
'// AutoIndent options in CodeMax.Control.AutoIndentMode
'//
'//
Const cmIndentOff As Integer = 0         ' auto-indent off -- new line begins at column 0
Const cmIndentScope As Integer = 1       ' new line begins at correct language scope indentation level
Const cmIndentPrevLine As Integer = 2    ' new line has identical indentation of previous line

'/////////////////////////////////////////////////////////////
'//
'// CodeMax.Control.HitTest() return codes
'//
'//

Const cmNowhere As Integer = 0       ' Not over the CodeMax control
Const cmHSplitter As Integer = 1     ' Over the horizontal splitter bar
Const cmVSplitter As Integer = 2     ' Over the vertical splitter bar
Const cmHVSplitter As Integer = 3    ' Over the intersection of the horizontal and vertical splitter bar
Const cmEditSpace As Integer = 4     ' Over the buffer contents (code)
Const cmHScrollBar As Integer = 5    ' Over the horizontal scrollbar
Const cmVScrollBar As Integer = 6    ' Over the vertical scrollbar
Const cmSizeBox As Integer = 7       ' Over the sizebox visible when both scrollbars are visible
Const cmLeftMargin As Integer = 8    ' Over the left margin area

'/////////////////////////////////////////////////////////////
'//
'// line numbering style values for LineNumberStyle property
'//
'//
Const cmDecimal As Integer = 10          ' base 10 numbering
Const cmHexadecimal As Integer = 16      ' base 16 numbering
Const cmBinary As Integer = 2            ' base 2 numbering
Const cmOctal As Integer = 8             ' base 8 numbering

'/////////////////////////////////////////////////////////////
'//
'// print flags used with Print()  Can be Or'd together
'//
'//
Const cmPrnPromptDlg As Integer = &H0            ' display the print common dialog
Const cmPrnDefaultPrn As Integer = &H1           ' use default printer (no print dialog displayed)
Const cmPrnHDC As Integer = &H2                          ' use HDC provided
Const cmPrnRichFonts As Integer = &H4            ' use bold, italics, underline, etc. when appropriate
Const cmPrnColor As Integer = &H8                        ' print in color
Const cmPrnPageNums As Integer = &H10            ' print 'page # of #' at the bottom of the page
Const cmPrnDateTime As Integer = &H20            ' print date and time at top of the page
Const cmPrnBorderThin As Integer = &H40          ' surround text with a thin border
Const cmPrnBorderThick As Integer = &H80         ' surround text with a thick border
Const cmPrnBorderDouble As Integer = &H100       ' surround text with two thin borders
Const cmPrnBorderDouble As Integer = &H100       ' surround text with two thin borders
Const cmPrnSelection = &H200                 ' print the selection rather than entire edit contents

'/////////////////////////////////////////////////////////////
'//
'// style flags used with SetLineStyle()  Can be Or'd together
'//
'//
Const cmNotifyDel As Integer = &H2               ' parent window should receive LineDeleted notification

'/////////////////////////////////////////////////////////////
'//
'// Button flags used with mouse notifications.
'//
'//
Const cmBtnLeft As Integer = &H1                 ' left mouse button
Const cmBtnRight As Integer = &H2                ' right mouse button
Const cmBtnMiddle As Integer = &H4               ' middle mouse button
