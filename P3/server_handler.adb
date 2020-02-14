with Ada.Text_IO;
with Gnat.Calendar.Time_IO;
with Chat_Messages;

package body Server_Handler is
	package ATIO renames Ada.Text_IO;
	package CM renames Chat_Messages;
	
	use type Ada.Calendar.Time;
	use type ASU.Unbounded_String;
	use type LLU.End_Point_Type;
	
	Active_Clients: ACP.Map;
	Inactive_Clients: ICP.Map;
	
	Procedure Send_To_All(Actives_Client: ACP.Map; P_Buffer: Access LLU.Buffer_Type) is
		Cursor: ACP.Cursor:= ACP.First(Active_Clients);
		Active_Client: ACP.Element_Type;
	begin 
		while ACP.Has_Element(Cursor) loop
			Active_Client:= ACP.Element(Cursor);
			LLU.Send(Active_Client.Value.Client_EP_Handler, P_Buffer);
			ACP.Next(Cursor);
		end loop;
	end Send_To_All;
	
	procedure Send_To_All_Less_One(Actives_Client: ACP.Map; Nick: in ASU.Unbounded_String;
								   P_Buffer: Access LLU.Buffer_Type) is 
		Cursor: ACP.Cursor:= ACP.First(Active_Clients);
		Active_Client: ACP.Element_Type;
	begin
		while ACP.Has_Element(Cursor) loop 
			Active_Client:= ACP.Element(Cursor);
			if Active_Client.Key /= Nick then
				LLU.Send(Active_Client.Value.Client_EP_Handler, P_Buffer);
			end if;
			ACP.Next(Cursor);
		end loop;
	end Send_To_All_Less_One;
	
	Function Split_EP(EP: in LLU.End_Point_Type) return ASU.Unbounded_String is 
		EP_Image: ASU.Unbounded_String;
		Port: Integer;
		IP: ASU.Unbounded_String;
		Position: Integer;
	begin
		EP_Image:= ASU.To_Unbounded_String(LLU.Image(EP));
		Position := ASU.Index(EP_Image,":");
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position:= ASU.Index(EP_Image, ",");
		IP:= ASU.Head(EP_Image, Position - 1);
		ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1));

		Position:= ASU.Index(EP_Image, ":");
		Port:= Integer'Value(ASU.To_String(ASU.Tail(EP_Image, ASU.Length(EP_Image) - (Position + 1))));

		return ASU.To_Unbounded_String("(" & ASU.To_String(IP) & ":" & Integer'Image(Port) & ")");	
	end Split_EP;

	function Time_Image(T: Ada.Calendar.Time) return String is
	begin
		return Gnat.Calendar.Time_IO.Image(T, "%d-%b-%y %T.%i");
	end Time_Image;
	
	procedure Active_Collection is
		Cursor: ACP.Cursor:= ACP.First(Active_Clients);
		Active_Client: ACP.Element_Type;
	begin
		ATIO.Put_Line("ACTIVE CLIENTS");
		ATIO.Put_Line("==============");
		while ACP.Has_Element(Cursor) loop
			Active_Client := ACP.Element(Cursor);
			
			ATIO.Put_Line(ASU.To_String(Active_Client.Key) & " " & ASU.To_String(Split_EP(Active_Client.Value.Client_EP_Handler)) & ": " &
						  Time_Image(Active_Client.Value.Last_Connection));
			
			ACP.Next(Cursor);
		end loop;
		ATIO.New_Line;
	end Active_Collection;
	
	procedure Inactive_Collection is
		Cursor: ICP.Cursor:= ICP.First(Inactive_Clients);
		Inactive_Client: ICP.Element_Type;
	begin
		ATIO.Put_Line("OLD CLIENTS");
		ATIO.Put_Line("==============");
		while ICP.Has_Element(Cursor) loop
			Inactive_Client := ICP.Element(Cursor);
			
			ATIO.Put_Line(ASU.To_String(Inactive_Client.Key) & ": " & Time_Image(Inactive_Client.Value));
				ICP.Next(Cursor);
		end loop;
		ATIO.New_Line;
	end Inactive_Collection;
	
	Function Delete_Old_Client(Active_Clients: ACP.Map) return ASU.Unbounded_String is  
		Cursor: ACP.Cursor:= ACP.First(Active_Clients);
		Old_Client:ACP.Element_Type;
		New_Client:ACP.Element_Type;
	begin 
		Old_Client:= ACP.Element(Cursor);	
		ACP.Next(Cursor);
		while ACP.Has_Element(Cursor) loop
			New_Client:= ACP.Element(Cursor);
			if New_Client.Value.Last_Connection < Old_Client.Value.Last_Connection then 
				Old_client:= New_Client;
			end if ;
			ACP.Next(Cursor);
		end loop;
		Return Old_Client.Key;
	end Delete_Old_Client;
	
	Procedure Server_Message(Nick: in ASU.Unbounded_String;
							 Comment: in ASU.Unbounded_String; P_Buffer: access LLU.Buffer_Type) is
	begin
		LLU.Reset(P_Buffer.all);
		CM.Message_Type'Output(P_Buffer, CM.Server);
		ASU.Unbounded_String'Output(P_Buffer, Nick);
		ASU.Unbounded_String'Output(P_Buffer, Comment);	
	end Server_Message;
	
	procedure Welcome_Message(Acogido: in Boolean; P_buffer: access LLU.Buffer_Type) is 
	begin 
		LLU.Reset(P_Buffer.all);
		CM.Message_Type'Output(P_Buffer, CM.Welcome);
		Boolean'Output(P_Buffer, Acogido);
	end Welcome_Message;
	
	procedure Init_Mode(Client_EP_Receive: out LLU.End_Point_Type; Client_EP_Handler: out LLU.End_Point_Type;
						Nick:  out ASU.Unbounded_String; P_Buffer: access LLU.Buffer_Type) is 
		CLient_Value: Active_Client_Value_Type;
		Success: Boolean;
		Delete_Client: ASU.Unbounded_String;
		Comment: ASU.Unbounded_String;
	begin
		Client_EP_Receive := LLU.End_Point_Type'Input(P_Buffer);
		Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
		Nick := ASU.Unbounded_String'Input(P_Buffer);
		
		ATIO.Put("INIT received from " & ASU.To_String(Nick) & ": ");
		
		ACP.Get(Active_Clients, Nick, Client_Value, Success);
		if Success then
			ATIO.Put_Line("IGNORED, nick already used");
			
			Welcome_Message(False, P_Buffer);
			LLU.Send(Client_EP_Receive, P_Buffer);
		else
			ATIO.Put_Line("ACCEPTED");
			
			Client_Value.Client_EP_Handler := Client_EP_Handler;
			Client_Value.Last_Connection := Ada.Calendar.Clock;

			begin
				ACP.Put(Active_Clients, Nick, Client_Value);
			exception
				when ACP.Full_Map =>
					Delete_Client:= Delete_Old_Client(Active_Clients);
					Comment:= Delete_Client & ASU.To_Unbounded_String(" banned for being idle too long");
					Server_Message(ASU.To_Unbounded_String("Server"), Comment, P_Buffer);
					Send_To_all(Active_Clients, P_Buffer);
					
					ACP.Delete(Active_Clients, Delete_Client, Success);
					if Success then
						begin
							ICP.Put(Inactive_Clients, Delete_Client, Ada.Calendar.Clock);
						exception
							when ICP.Full_Map =>
								null;
						end;
						ACP.Put(Active_Clients, Nick, Client_Value);
					end if;
			end;
			
			Welcome_Message(True, P_Buffer);
			LLU.Send(Client_EP_Receive, P_Buffer);
			
			Comment:= Nick & Asu.To_Unbounded_String(" joins the chat");
			Server_Message(ASU.To_Unbounded_String("Server"), Comment, P_Buffer);
			Send_To_All_Less_One(Active_Clients, Nick, P_Buffer);
		end if;
	end Init_Mode;
	
	procedure Writer_Mode(Client_EP_Handler: out LLU.End_Point_Type; Nick: out ASU.Unbounded_String;
						  Comment: out ASU.Unbounded_String; P_Buffer: access LLU.Buffer_Type) is
		Client_Value: Active_Client_Value_Type;
		Success: Boolean; 
	begin
		ATIO.Put("WRITER received from ");

		Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
		Nick := ASU.Unbounded_String'Input(P_Buffer);
		Comment := ASU.Unbounded_String'Input(P_Buffer);

		ACP.Get(Active_Clients, Nick, Client_Value, Success);
		if Success then
			if Client_EP_Handler = Client_Value.Client_EP_Handler then
				ATIO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Comment));
				
				Client_Value.Last_Connection := Ada.Calendar.Clock;
				ACP.Put(Active_Clients, Nick, Client_Value);
				Server_Message(Nick, Comment, P_Buffer);
				Send_To_All_Less_One(Active_Clients, Nick, P_Buffer);
			else
				ATIO.Put_Line("unknown client. IGNORED");
			end if;
		else
			ATIO.Put_Line("unknown client. IGNORED");
		end if;
	end Writer_Mode;
	
	procedure Logout_Mode(Client_EP_Handler: out LLU.End_Point_Type; Nick: out ASU.Unbounded_String;
						  P_Buffer: access LLU.Buffer_Type)is
		Client_Value: Active_Client_Value_Type;
		Success: Boolean; 
		Comment: ASU.Unbounded_String;
	begin
		ATIO.Put("LOGOUT received from ");

		Client_EP_Handler := LLU.End_Point_Type'Input(P_Buffer);
		Nick := ASU.Unbounded_String'Input(P_Buffer);

		ACP.Get(Active_Clients, Nick, Client_Value, Success);
		if Success then
			if Client_EP_Handler = Client_Value.Client_EP_Handler then
				ATIO.Put_Line(ASU.To_String(Nick));
				
				ACP.Delete(Active_Clients, Nick, Success);
				if Success then
				begin
					ICP.Put(Inactive_Clients, Nick, Ada.Calendar.Clock);
				
					Comment:= Nick & ASU.To_Unbounded_String(" leaves the chat");
					Server_Message(ASU.To_Unbounded_String("Server"),Comment, P_Buffer);
					Send_To_All(Active_Clients, P_Buffer);	
				exception
					when ICP.Full_Map =>
						null;
					end;
				end if;
			else
				ATIO.Put_Line("unknown client. IGNORED");
			end if;
		else
			ATIO.Put_Line("unknown client. IGNORED");
		end if;

	
	end Logout_Mode;
	procedure Chat_Handler(From: in LLU.End_Point_Type; To: in LLU.End_Point_Type; 
						   P_Buffer: access LLU.Buffer_Type) is
		MT: CM.Message_Type;
		Client_EP_Receive: LLU.End_Point_Type;
		Client_EP_Handler: LLU.End_Point_Type;
		Nick: ASU.Unbounded_String;
		Comment: ASU.Unbounded_String;	
	begin
		MT := CM.Message_Type'Input(P_Buffer);
		case MT is
			when CM.Init =>
				Init_Mode(Client_EP_Receive, Client_EP_Handler, Nick, P_Buffer);
				
			when CM.Writer =>
				Writer_Mode(Client_EP_Handler, Nick, Comment, P_Buffer);
				
			when CM.Logout =>
				Logout_Mode(Client_EP_Handler, Nick, P_Buffer);
				
			when others =>
				Null;
		end case;
	end Chat_Handler;
end Server_Handler;
