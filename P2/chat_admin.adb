with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Chat_Messages;

procedure Chat_Admin is 

	package ASU renames Ada.Strings.Unbounded;
	package ATIO renames Ada.Text_IO;
	package ACL renames Ada.Command_Line;
	package LLU renames Lower_Layer_UDP;
	package CM renames Chat_Messages;
	use Type CM.Message_Type;
	use type ASU.Unbounded_String;	
	

	Usage_Error: Exception;

	procedure Arguments_Control (Hostname: out ASU.Unbounded_String; Port: out Integer; Password: out ASU.Unbounded_String; Correct_Option: out Boolean)is 

	begin
		if ACL.Argument_Count /= 3 then 
			raise Usage_Error;
		else 
			Hostname:= ASU.To_Unbounded_String(ACL.Argument(1));
			Port:= Integer'Value(ACL.Argument(2));
			Password:= ASU.To_Unbounded_String(ACL.Argument(3)); 
			Correct_Option:= True;
		end if;
	end Arguments_Control;

	procedure Collection_Request_Message(Buffer: Access LLU.Buffer_Type; Admin_EP: in LLU.End_Point_Type; Password: in ASU.Unbounded_String) is
	begin
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Collection_Request);
		LLU.End_Point_Type'Output(Buffer, Admin_EP);
		ASU.Unbounded_String'Output(Buffer, Password);

	end Collection_Request_Message;

	procedure Ban_Message (Buffer: Access LLU.Buffer_Type; Password: in ASU.Unbounded_String; Nick: in ASU.Unbounded_String) is
	begin
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Ban);
		ASU.Unbounded_String'Output(Buffer, Password);
		ASU.Unbounded_String'Output(Buffer, Nick);
	end Ban_Message;

	procedure Shutdown_Message (Buffer: Access LLU.Buffer_Type; Password: ASU.Unbounded_String) is
		
	begin 
		LLU.Reset(Buffer.all);

		CM.Message_Type'Output(Buffer, CM.Shutdown);
		ASU.Unbounded_String'Output(Buffer, Password);
		
	end Shutdown_Message;

	procedure Print_Menu (Option: out Integer) is

	begin
		ATIO.Put_Line("Options");
		ATIO.Put_Line("1 Show writers collection");
		ATIO.Put_Line("2 Ban writer");
		ATIO.Put_Line("3 Shutdown server"); 
		ATIO.Put_Line("4 Quit");
		ATIO.Put("Your option? ");
		Option := Integer'Value(ATIO.Get_Line);
		ATIO.New_Line;
	end Print_Menu;

	Procedure Interactive_Menu(Server_EP: out LLU.End_Point_Type; Admin_EP: out LLU.End_Point_Type; Correct_Option: in Boolean;
								Hostname: in ASU.Unbounded_String; Port: in Integer; Password: in ASU.Unbounded_String) is 
		Finish: Boolean;
		Buffer: Aliased LLU.Buffer_Type(1024);
		Expired: Boolean;
		Nickname : ASU.Unbounded_String;
		Option: Integer;
		Data: ASU.Unbounded_String;
		MT: CM.Message_Type;
				
		begin
			if Correct_Option then 
				Server_EP:= LLU.Build(LLU.To_IP(ASU.To_String(Hostname)), Port);
				LLU.Bind_Any(Admin_EP);
				Finish:= False;
				while not Finish loop
					Print_Menu(Option);
					case Option is 
						when 1 =>
							Collection_Request_Message(Buffer'Access, Admin_EP, Password);
							LLU.Send(Server_EP,Buffer'Access);
 
							LLU.Receive(Admin_EP,Buffer'Access,5.0, Expired);
							if not Expired then 
								MT:= CM.Message_Type'Input(Buffer'Access);
								if MT = CM.Collection_Data then 
									Data := ASU.Unbounded_String'Input(Buffer'Access);
									ATIO.Put_Line(ASU.To_String(Data));
								end if;
							else 
								ATIO.Put_Line("Password incorrect");
								Finish := True;
							end if;
						when 2 =>
							ATIO.Put("Nick to ban? ");
							Nickname:= ASU.To_Unbounded_String(ATIO.Get_Line);
							Ban_Message(Buffer'Access, Password, Nickname);
							LLU.Send(Server_EP, Buffer'Access);
						when 3 => 
							ATIO.Put_Line("Server shutdown sent");
							ATIO.New_Line;
							Shutdown_Message(Buffer'Access, Password);
							LLU.Send(Server_EP, Buffer'Access);
						when 4 =>
							Finish:=True;
						when others => 
							ATIO.Put_Line("Option incorrect");
					end case;
				end Loop;
			end if;
	end Interactive_Menu;

	Hostname: ASU.Unbounded_String;
	Port: Integer;
	Password: ASU.Unbounded_String;
	Correct_Option: Boolean;
	Server_EP: LLU.End_Point_Type;
	Admin_EP: LLU.End_Point_Type;
begin
	Arguments_Control(Hostname, Port, Password, Correct_Option);
	Interactive_Menu(Server_EP,Admin_EP, Correct_Option, Hostname, Port, Password);
	LLU.Finalize;
Exception
	when Usage_Error =>
		ATIO.Put_Line("Usage: <Hostname> <Port> <Password>");
end Chat_Admin;
