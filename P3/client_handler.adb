with Chat_Messages;

package body Client_Handler is 
	package CM renames Chat_Messages;
	
	procedure Chat_Handler(From: in LLU.End_Point_Type;
							 To: in LLU.End_Point_Type;
							 P_Buffer: access LLU.Buffer_Type) is
	MT: CM.Message_Type;
	Nick: ASU.Unbounded_String;
	Comment: ASU.Unbounded_String;
	begin
		MT:= CM.Message_Type'Input(P_Buffer);
		case MT is 
			When CM.Server =>
				Nick:= ASU.Unbounded_String'Input(P_Buffer);
				Comment:= ASU.Unbounded_String'Input(P_Buffer);
				
				ATIO.New_Line;
				ATIO.Put_Line(ASU.To_String(Nick) & (":") & ASU.To_String(Comment) );
				ATIO.Put(">>");
			when Others =>
				null;
		end case;
	end Chat_Handler;
	
end Client_Handler;
