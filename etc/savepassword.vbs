If WScript.Arguments.Count > 1 Then
         Set fso = CreateObject("Scripting.FileSystemObject")
        For Each arg in Wscript.Arguments
                If fso.FileExists(arg) Then
                        Set objWord = CreateObject("Word.Application")
                        objWord.Caption = "Password Protecting Documents"
                        objWord.Visible = False
                        objWord.Documents.Open(fso.GetAbsolutePathName(arg))
                        ' Now put in the numbered headings.
                        objWord.ActiveDocument.Styles("Heading 1").ParagraphFormat.OutlineLevel = 1
                        objWord.ListGalleries(3).ListTemplates(2).ListLevels(1).NumberFormat = "%1."
                        objWord.ListGalleries(3).ListTemplates(2).ListLevels(1).LinkedStyle = "Heading 1"
                        objWord.ActiveDocument.Styles("Heading 1").LinkToListTemplate objWord.ListGalleries(3).ListTemplates(2), 1
                        ' wdGoToPage = 1 wdGoToNext = 2
                        ' Populate the Fields:
                        ' For Each table, expand to contents.
                        For Counter = 1 To objWord.ActiveDocument.Tables.Count
                                ' Ok
                                objWord.ActiveDocument.Tables(Counter).AutoFitBehavior 1
                                objWord.ActiveDocument.Tables(Counter).Cell(1,1).Select
                                objWord.Selection.ParagraphFormat.KeepWithNext = True
                                objWord.ActiveDocument.Tables(Counter).Select
                                objWord.Selection.Fields.Update
                        Next
                        objWord.Selection.WholeStory
                        objWord.Selection.Fields.Update

                        objWord.ActiveDocument.WritePassword = Wscript.Arguments(0)
                        objWord.ActiveDocument.SaveAs Left(fso.GetAbsolutePathName(arg),Len(fso.GetAbsolutePathName(arg))-4) & "-ro.doc",TRUE,0
                        objWord.ActiveDocument.Close
                        objWord.Quit
                        fso.GetFile(fso.GetAbsolutePathName(arg)).Delete
                        fso.GetFile(Left(fso.GetAbsolutePathName(arg),Len(fso.GetAbsolutePathName(arg))-4) & "-ro.doc").Move(Left(fso.GetAbsolutePathName(arg),Len(fso.GetAbsolutePathName(arg))-4) & ".doc")
                Else
                End If
        next
End If
