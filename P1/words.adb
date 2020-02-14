with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Exceptions;
with Word_Lists;
with Ada.Strings.Maps;
with Ada.Characters.Handling;

procedure words is
	package ATIO renames Ada.Text_IO;
	package ASU renames Ada.Strings.Unbounded;
	package ACL renames Ada.Command_Line;
	package AIOE renames Ada.IO_Exceptions;
	package AE renames Ada.Exceptions;
	Package WL renames Word_Lists;
	package ASM renames Ada.Strings.Maps;
	package ACH renames Ada.Characters.Handling;

	Usage_Error: Exception;
	Format_Error: Exception;

	procedure Arguments_Control(File_Name: out ASU.Unbounded_String; Correct_Option: out Boolean) is 
	begin
		if ACL.Argument_Count /= 1 and ACL.Argument_Count /= 2 then
			raise Usage_Error;			
		end if;

		if ACL.Argument_Count = 1 then
			if ACL.Argument(1) = "-i" then  
				raise Usage_Error;
			end if;
			File_Name:= ASU.To_Unbounded_String(ACL.Argument(1));
		else 
			if ACL.Argument(1) /= "-i" then 
				raise Usage_Error;
			else 
				Correct_Option := True;
				File_Name:= ASU.To_Unbounded_String(ACL.Argument(2));
			end if;
		end if;
	end Arguments_Control;


	procedure Print_Menu (Option: out Integer) is 
	begin
		ATIO.New_Line;
		ATIO.Put_Line("Options");
		ATIO.Put_Line("1 Add word");
		ATIO.Put_Line("2 Delete word");
		ATIO.Put_Line("3 Search word");
		ATIO.Put_Line("4 Show all words");
		ATIO.Put_Line("5 Add Quit");
		ATIO.New_Line;
		ATIO.Put("Your Option? ");
		Option:= integer'value(ATIO.Get_Line);
		ATIO.New_Line;
	end Print_Menu;

	procedure Interactive_Menu(Correct_Option: in Boolean; List: in out WL.Word_List_Type) is 
		Word:ASU.Unbounded_String;
		Finish: Boolean;
		Option: Integer;
		Count: Integer; 
	begin
		if Correct_Option then 
			Finish := False;
			while not Finish loop
			Print_Menu(Option);
			case Option is 
				when 1 => 
					ATIO.Put("word? ");
					Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
					Word:= ASU.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
					WL.Add_Word(List, Word);
					ATIO.Put_Line("Word |" & ASU.To_String(Word) & "| added" ); 
				when 2 =>
					ATIO.Put("word? ");
					Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
					Word:= ASu.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
					begin
					WL.Delete_Word(List,Word);
					ATIO.Put_Line("|" & ASU.To_String(Word) & "| Deleted");
					exception
						when WL.Word_List_Error =>
							ATIO.Put_Line("|" & ASU.To_String(Word) & "| not found");
					end;
				when 3 =>
					ATIO.Put("word? ");
					Word:= ASU.To_Unbounded_String(ATIO.Get_Line);
					Word:= ASu.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
					WL.Search_Word(List,Word,count);
					ATIO.Put_Line("|" & ASU.To_String(Word) & "| -" & Integer'Image(count));
				when 4 => 
					WL.Print_All(List);
				when 5 =>
					WL.Max_Word(List, Word, Count);
					ATIO.Put_Line("The most frequent word: |" & ASU.To_String(Word) & "| -" & Integer'image(Count));
					Finish := True;
				when others => 
					ATIO.Put_Line("Invalid Option");
			end case;
			end loop;
		end if;
	end interactive_Menu;

	procedure Split_Line (Line: in out ASU.Unbounded_String; Word: out ASU.Unbounded_String) is
		Space_Position: integer;
		begin
			Space_Position:= ASU.Index(Line, ASM.To_Set(" ,;.-:'""''"));
			word := ASU.Head(Line, Space_Position-1);
			Word:= ASu.To_Unbounded_String(ACH.To_Lower(ASU.To_String(Word)));
			ASU.Tail(Line, ASU.Length(Line)-Space_Position);
		exception
			when others =>
				raise Format_Error;
	end Split_Line;

	File_Name : ASU.Unbounded_String;
	File : ATIO.File_Type;
	Finish : Boolean;
	Line : ASU.Unbounded_String;
	Correct_Option: Boolean;
	End_Of_Line: Boolean;
	Word: ASU.Unbounded_String;
	Count: Integer;
	List: WL.Word_List_Type;
	File_Not_Found: Boolean:= False;
begin	
	Arguments_Control(File_Name,Correct_Option);

	begin
		ATIO.Open(File, ATIO.In_File, ASU.To_String(File_Name)); 
		Finish:= False;
		while not Finish loop
			begin 
				Line := ASU.To_Unbounded_String(ATIO.Get_Line(File));

				End_Of_Line:= False;
				While not End_Of_Line loop
					begin
						Split_Line(Line, Word);
						if ASU.Length(word) /= 0 then 
							WL.Add_Word(List,Word);
						end if;
					Exception
						when Format_Error =>
							End_Of_Line:= True;
							if ASU.Length(Line) /= 0 then
								Word := Line;
								WL.Add_Word(List,Word);
							end if;
					end;
				end loop;
			exception
				when AIOE.End_Error =>
					Finish := True;
					ATIO.Close(File);
			end;
		end loop;
	Exception
		when AIOE.Name_Error =>
			File_Not_Found := True;
			ATIO.Put_Line(ASU.To_String(File_Name) & "file not found");
	end;

	case ACL.Argument_Count is 
		when 1 => 
			if not File_Not_Found then 
				WL.Max_Word(List,Word,Count);
				ATIO.Put_Line("The most frequent word: |" & ASU.To_String(Word) & "| - " & Integer'Image(Count));
			end if;
		when 2 =>
			if ACL.Argument(1) /= "-i" then 
				raise Usage_Error;
			else 
				begin
				Interactive_Menu(Correct_Option,List);
				exception
					when WL.Word_List_Error =>
						ATIO.Put_Line("No words.");
				end;
			end if;
		when others =>
			null;
	end case;

	WL.Delete_list(List);

Exception

	when Usage_Error =>
		ATIO.Put_Line("usage: words [-i] <filename>");

end words;
